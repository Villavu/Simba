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
  HELPERS_DYNARRAY: TStringArray = (
    'property <ArrayName>.Length: Integer;',
    'property <ArrayName>.Low: Integer;',
    'property <ArrayName>.High: Integer;',
    'property <ArrayName>.First: <ArrayElementType>;',
    'property <ArrayName>.Last: <ArrayElementType>;',
    'property <ArrayName>.Pop: <ArrayElementType>;',
    'property <ArrayName>.Min: <ArrayElementType>;',
    'property <ArrayName>.Max: <ArrayElementType>;',
    'property <ArrayName>.Sum: <ArrayElementType>;',
    'property <ArrayName>.Mode: <ArrayElementType>;',
    'property <ArrayName>.Median: Double;',
    'property <ArrayName>.Mean: Double;',
    'property <ArrayName>.Variance: Double;',
    'property <ArrayName>.Stdev: Double;',
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
    'function <ArrayName>.Reversed: <ArrayDef>;',
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayDef>;',
    'function <ArrayName>.Remove(Value: <ArrayElementType>): <ArrayElementType>;',
    'function <ArrayName>.DeleteIndex(Index: Integer; Count: Integer): <ArrayElementType>;',
    'procedure <ArrayName>.DeleteRange(StartIndex, EndIndex: Integer);',
    'procedure <ArrayName>.Insert(Item: <ArrayElementType>; Index: Integer);',
    'procedure <ArrayName>.SetLength(NewLength: Integer);',
    'procedure <ArrayName>.Reverse;',
    'procedure <ArrayName>.Clear;',
    'function <ArrayName>.Equals(Other: <ArrayDef>): Boolean;',
    'function <ArrayName>.Intersection(Other: <ArrayDef>): <ArrayDef>;',
    'function <ArrayName>.Difference(Other: <ArrayDef>): <ArrayDef>;',
    'function <ArrayName>.SymDifference(Other: <ArrayDef>): <ArrayDef>;'
  );

  function GetArrayHelpers(Decl: TDeclaration): TDeclarationArray;

implementation

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

function GetArrayHelpers(Decl: TDeclaration): TDeclarationArray;

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
begin
  Parser := nil;

  if (Decl is TDeclaration_TypeArray) then
  begin
    ElementType := Decl.Items.GetTextOfClass(TDeclaration_VarType);
    if (ElementType <> '') then
      Parser := Get(HELPERS_DYNARRAY, IfThen(Decl.Name <> '', Decl.Name, 'array'), ElementType, IfThen(Decl.Name <> '', Decl.Name, 'array of ' + ElementType));
  end;

  if (Parser <> nil) then
    Result := Parser.Items.ToArray
  else
    Result := [];
end;

initialization
  ArrayHelperParsers := TCodeParserList.Create(True);

finalization
  FreeAndNil(ArrayHelperParsers);

end.



