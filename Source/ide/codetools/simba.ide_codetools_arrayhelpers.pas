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
  simba.base, simba.ide_codetools_parser;

const
  HELPERS_DYNARRAY_FLAT: TStringArray = (
    'property <ArrayName>.IsEmpty: Boolean;',
    'property <ArrayName>.Length: Integer;',
    'property <ArrayName>.Low: Integer;',
    'property <ArrayName>.High: Integer;',
    'property <ArrayName>.First: <ArrayElementType>;',
    'property <ArrayName>.Last: <ArrayElementType>;',
    'property <ArrayName>.Pop: <ArrayElementType>;',
    'function <ArrayName>.Min: <ArrayElementType>;',
    'function <ArrayName>.Max: <ArrayElementType>;',
    'function <ArrayName>.Sum: <ArrayElementType>;',
    'function <ArrayName>.Mode: <ArrayElementType>;',
    'function <ArrayName>.Median: Double;',
    'function <ArrayName>.Mean: Double;',
    'function <ArrayName>.Variance: Double;',
    'function <ArrayName>.Stdev: Double;',
    'function <ArrayName>.Contains(Value: <ArrayElementType>): Boolean;',
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer);',
    'function <ArrayName>.Unique: <ArrayDef>;',
    'function <ArrayName>.IndexOf(Value: <ArrayElementType>): Integer;',
    'function <ArrayName>.IndicesOf(Value: <ArrayElementType>): TIntegerArray;',
    'procedure <ArrayName>.Sort;',
    'procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayElementType>): Integer);',
    'procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean);',
    'function <ArrayName>.Sorted: <ArrayDef>;',
    'function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayElementType>): Integer): <ArrayDef>;',
    'function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <ArrayDef>;',
    'function <ArrayName>.Copy: <ArrayDef>;',
    'function <ArrayName>.CopyRange(StartIndex, EndIndex: Integer): <ArrayDef>;',
    'function <ArrayName>.Random: <ArrayElementType>;',
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayDef>;',
    'function <ArrayName>.Remove(Value: <ArrayElementType>): <ArrayElementType>;',
    'function <ArrayName>.Delete(Index: Integer): <ArrayElementType>;',
    'procedure <ArrayName>.DeleteRange(StartIndex, EndIndex: Integer);',
    'procedure <ArrayName>.Insert(Item: <ArrayElementType>; Index: Integer); overload;',
    'procedure <ArrayName>.Insert(Item: <ArrayDef>; Index: Integer); overload;',
    'procedure <ArrayName>.SetLength(NewLength: Integer);',
    'procedure <ArrayName>.Reverse;',
    'function <ArrayName>.Reversed: <ArrayDef>;',
    'procedure <ArrayName>.Clear;',
    'function <ArrayName>.Equals(Other: <ArrayDef>): Boolean;',
    'function <ArrayName>.Intersection(Other: <ArrayDef>): <ArrayDef>;',
    'function <ArrayName>.Difference(Other: <ArrayDef>): <ArrayDef>;',
    'function <ArrayName>.SymDifference(Other: <ArrayDef>): <ArrayDef>;'
  );

  HELPERS_DYNARRAY_MULTIDIM: TStringArray = (
    'procedure <ArrayName>.SetLength(NewLength: Integer);',
    'property <ArrayName>.Low: Integer;',
    'property <ArrayName>.High: Integer;',
    'property <ArrayName>.Length: Integer;',
    'property <ArrayName>.First: <ArrayElementType>;',
    'property <ArrayName>.Last: <ArrayElementType>;',
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer);',
    'function <ArrayName>.Copy: <ArrayDef>;',
    'function <ArrayName>.CopyRange(StartIndex, EndIndex: Integer): <ArrayDef>;',
    'function <ArrayName>.Random: <ArrayElementType>;',
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayDef>;',
    'property <ArrayName>.IsEmpty: Boolean',
    'procedure <ArrayName>.Clear;',
    'property <ArrayName>.Pop: <ArrayElementType>;',
    'function <ArrayName>.Delete(Index: Integer): <ArrayElementType>;',
    'procedure <ArrayName>.DeleteRange(StartIndex, EndIndex: Integer);',
    'procedure <ArrayName>.Insert(Item: <ArrayElementType>; Index: Integer);',
    'procedure <ArrayName>.Reverse;',
    'function <ArrayName>.Reversed: <ArrayDef>;',
    'function <ArrayName>.Equals(Other: <ArrayDef>): Boolean;'
  );

  function GetArrayHelpers(Decl: TDeclaration_TypeArray; IsMultiDim: Boolean): TDeclarationArray;

implementation

uses
  simba.initializations;

var
  ArrayHelperParsers: TCodeParserList;

type
  TArrayHelperParser = class(TCodeParser)
  public
    ArrayName: String;
    ArrayElementType: String;
    ArrayDef: String;

    constructor Create(Helpers: TStringArray; Name, ElementType, Def: String); reintroduce;
  end;

constructor TArrayHelperParser.Create(Helpers: TStringArray; Name, ElementType, Def: String);
var
  Script: String;
  I: Integer;
begin
  inherited Create();

  ArrayName := Name;
  ArrayElementType := ElementType;
  ArrayDef := Def;

  Script := '';
  for I := 0 to High(Helpers) do
    Script := Script + Helpers[I] + ' external;' + LineEnding;
  Script := Script.Replace('<ArrayName>', ArrayName);
  Script := Script.Replace('<ArrayElementType>', ArrayElementType);
  Script := Script.Replace('<ArrayDef>', ArrayDef);

  SetScript(Script, 'ArrayHelpers');
end;

function GetArrayHelpers(Decl: TDeclaration_TypeArray; IsMultiDim: Boolean): TDeclarationArray;

  function Get(Helpers: TStringArray; ArrayName, ArrayElementType, ArrayDef: String): TCodeParser;
  var
    I: Integer;
  begin
    for I := 0 to ArrayHelperParsers.Count - 1 do
      if (TArrayHelperParser(ArrayHelperParsers[I]).ArrayName = ArrayName) and
         (TArrayHelperParser(ArrayHelperParsers[I]).ArrayElementType = ArrayElementType) and
         (TArrayHelperParser(ArrayHelperParsers[I]).ArrayDef = ArrayDef) then
        Exit(ArrayHelperParsers[I]);

    Result := TArrayHelperParser.Create(Helpers, ArrayName, ArrayElementType, ArrayDef);
    Result.Run();

    ArrayHelperParsers.Add(Result);
  end;

var
  Parser: TCodeParser;
  ElementType: String;
  Methods: TStringArray;
begin
  Parser := nil;

  ElementType := Decl.Items.GetTextOfClass(TDeclaration_VarType);
  if (ElementType <> '') then
  begin
    if IsMultiDim then
      Methods := HELPERS_DYNARRAY_MULTIDIM
    else
      Methods := HELPERS_DYNARRAY_FLAT;

    Parser := Get(Methods, IfThen(Decl.Name <> '', Decl.Name, 'array'), ElementType, IfThen(Decl.Name <> '', Decl.Name, 'array of ' + ElementType));
  end;

  if (Parser <> nil) then
    Result := Parser.Items.ToArray
  else
    Result := [];
end;

procedure DoCreate;
begin
  ArrayHelperParsers := TCodeParserList.Create(True);
end;

procedure DoDestroy;
begin
  FreeAndNil(ArrayHelperParsers);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'ArrayHelperParsers', 5);
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'ArrayHelperParsers', -5);

end.



