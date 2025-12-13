{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_arrayhelpers;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.ide_codetools_parser, simba.ide_codetools_insight;

function GetArrayHelpers(ci: TCodeinsight; Decl: TDeclaration): TDeclarationArray;

implementation

uses
  simba.initializations;

type
  EArrayHelperType = (ahtNone, ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic);
  EArrayHelperTypeSet = set of EArrayHelperType;

var
  ArrayHelperParsers: TCodeParserList;
  ArrayHelperTemplates: array[EArrayHelperType] of TStringArray;

type
  TArrayHelperParser = class(TCodeParser)
  public
    ArrayHelperType: EArrayHelperType;
    ArrayName: String;
    ArrayElementType: String;
    ArrayDef: String;

    constructor Create(HelperType: EArrayHelperType; Name, ElementType, Def: String); reintroduce;
  end;

constructor TArrayHelperParser.Create(HelperType: EArrayHelperType; Name, ElementType, Def: String);
var
  Script: String;
  I: Integer;
begin
  inherited Create();

  ArrayName := Name;
  ArrayElementType := ElementType;
  ArrayDef := Def;
  ArrayHelperType := HelperType;

  Script := '';
  for I := 0 to High(ArrayHelperTemplates[HelperType]) do
    Script := Script + ArrayHelperTemplates[HelperType][I] + ' external;' + LineEnding;
  Script := Script.Replace('<ArrayName>', ArrayName);
  Script := Script.Replace('<ArrayElementType>', ArrayElementType);
  Script := Script.Replace('<ArrayDef>', ArrayDef);

  SetScript(Script, 'ArrayHelpers');
end;

function GetArrayHelpers(ci: TCodeinsight; Decl: TDeclaration): TDeclarationArray;

  function GetHelperType(Decl: TDeclaration): EArrayHelperType;
  var
    VarType: TDeclaration;
  begin
    Result := ahtNone;
    if (Decl is TDeclaration_TypeStaticArray) then
      Exit(ahtStatic);

    if (Decl is TDeclaration_TypeArray) then
    begin
      VarType := ci.ResolveVarType(TDeclaration_TypeArray(Decl).VarType);
      if (VarType is TDeclaration_TypeArray) then
        Exit(ahtDynMulti);

      if VarType.IsName('Int8') or VarType.IsName('Int16') or VarType.IsName('Int32') or VarType.IsName('Int64') or
         VarType.IsName('UInt8') or VarType.IsName('UInt16') or VarType.IsName('UInt32') or VarType.IsName('UInt64') or
         VarType.IsName('Integer') or VarType.IsName('Single') or VarType.IsName('Double') then
        Exit(ahtDynOrdinal);

      Exit(ahtDynSingle);
    end;
  end;

  function Get(HelperType: EArrayHelperType; ArrayName, ArrayElementType, ArrayDef: String): TDeclarationArray;
  var
    I: Integer;
    Parser: TCodeParser;
  begin
    for I := 0 to ArrayHelperParsers.Count - 1 do
      if (TArrayHelperParser(ArrayHelperParsers[I]).ArrayName = ArrayName) and
         (TArrayHelperParser(ArrayHelperParsers[I]).ArrayElementType = ArrayElementType) and
         (TArrayHelperParser(ArrayHelperParsers[I]).ArrayDef = ArrayDef) and
         (TArrayHelperParser(ArrayHelperParsers[I]).ArrayHelperType = HelperType) then
        Exit(ArrayHelperParsers[I].Items.ToArray);

    Parser := TArrayHelperParser.Create(HelperType, ArrayName, ArrayElementType, ArrayDef);
    Parser.Run();

    ArrayHelperParsers.Add(Parser);

    Result := Parser.Items.ToArray;
  end;

var
  HelperType: EArrayHelperType;
  ElementType: String;
begin
  Result := [];

  ElementType := Decl.Items.GetTextOfClass(TDeclaration_VarType);
  if (ElementType <> '') then
  begin
    HelperType := GetHelperType(Decl);
    if (HelperType <> ahtNone) then
      Result := Get(HelperType, IfThen(Decl.Name <> '', Decl.Name, 'array'), ElementType, IfThen(Decl.Name <> '', Decl.Name, 'array of ' + ElementType))
  end;
end;

procedure DoCreate;

  procedure Build(Decl: String; Types: EArrayHelperTypeSet);
  var
    Typ: EArrayHelperType;
  begin
    for Typ in Types do
      ArrayHelperTemplates[Typ] += [Decl];
  end;

begin
  ArrayHelperParsers := TCodeParserList.Create(True);

  Build('procedure <ArrayName>.SetLength(NewLength: Integer);', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('property <ArrayName>.Low: Integer;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('property <ArrayName>.High: Integer;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('property <ArrayName>.Length: Integer;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('property <ArrayName>.First: <ArrayElementType>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('property <ArrayName>.Last: <ArrayElementType>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer);', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.Copy: <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.CopyRange(StartIndex, EndIndex: Integer): <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.Random: <ArrayElementType>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('property <ArrayName>.IsEmpty: Boolean;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('procedure <ArrayName>.Clear;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('property <ArrayName>.Pop: <ArrayElementType>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('function <ArrayName>.Delete(Index: Integer): <ArrayElementType>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('procedure <ArrayName>.DeleteRange(StartIndex, EndIndex: Integer);', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('function <ArrayName>.Remove(Value: <ArrayElementType>): <ArrayElementType>;', [ahtDynSingle, ahtDynOrdinal]);
  Build('procedure <ArrayName>.Insert(Item: <ArrayElementType>; Index: Integer);', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('function <ArrayName>.Equals(Other: <ArrayDef>): Boolean;', [ahtDynSingle, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.Unique: <ArrayDef>;', [ahtDynSingle, ahtDynOrdinal]);
  Build('function <ArrayName>.Contains(Value: <ArrayElementType>): Boolean;', [ahtDynSingle, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.IndexOf(Value: <ArrayElementType>): Integer;', [ahtDynSingle, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.IndicesOf(Value: <ArrayElementType>): TIntegerArray;', [ahtDynSingle, ahtDynOrdinal, ahtStatic]);
  Build('procedure <ArrayName>.Sort;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayElementType>): Integer);', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean);', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('function <ArrayName>.Sorted: <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayElementType>): Integer): <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal]);
  Build('procedure <ArrayName>.Reverse;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.Reversed: <ArrayDef>;', [ahtDynSingle, ahtDynMulti, ahtDynOrdinal, ahtStatic]);
  Build('function <ArrayName>.Difference(Other: <ArrayDef>): <ArrayDef>;', [ahtDynSingle, ahtDynOrdinal]);
  Build('function <ArrayName>.SymDifference(Other: <ArrayDef>): <ArrayDef>;', [ahtDynSingle, ahtDynOrdinal]);
  Build('function <ArrayName>.Intersection(Other: <ArrayDef>): <ArrayDef>;', [ahtDynSingle, ahtDynOrdinal]);
  Build('function <ArrayName>.Median: Double;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Mode: <ArrayElementType>;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Min: <ArrayElementType>;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Max: <ArrayElementType>;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Sum: <ArrayElementType>;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Stdev: Double;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Variance: Double;', [ahtDynOrdinal]);
  Build('function <ArrayName>.Stdev: Double;', [ahtDynOrdinal]);
end;

procedure DoDestroy;
begin
  FreeAndNil(ArrayHelperParsers);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'ArrayHelperParsers', 5);
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'ArrayHelperParsers', -5);

end.



