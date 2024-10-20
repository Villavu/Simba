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
  HELPERS_STRING =
    'procedure <ArrayName>.SetLength(NewLength: Integer); external;' + LineEnding +
    'function <ArrayName>.Length: Integer; external;' + LineEnding +
    'function <ArrayName>.Copy: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): <ArrayName>; external;' + LineEnding +
    'procedure <ArrayName>.Delete(StartIndex: Integer; Count: Integer = High(Integer)); external;' + LineEnding +
    'function <ArrayName>.First: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Last: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Pop: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Pop(Index: Integer): <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;' + LineEnding +
    'procedure <ArrayName>.Reverse; external;' + LineEnding +
    'function <ArrayName>.Reversed: <ArrayName>; external;' + LineEnding +
    'procedure <ArrayName>.Clear; external;';

  HELPERS_DYNARRAY =
    'function <ArrayName>.Low: Integer; external;' + LineEnding +
    'function <ArrayName>.High: Integer; external;' + LineEnding +
    'function <ArrayName>.Contains(Value: <ArrayVarType>): Boolean; external;' + LineEnding +
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer); external;' + LineEnding +
    'function <ArrayName>.Unique: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.IndexOf(Value: <ArrayVarType>): Integer; external;' + LineEnding +
    'function <ArrayName>.IndicesOf(Value: <ArrayVarType>): TIntegerArray; external;' + LineEnding +
    'procedure <ArrayName>.Sort; external;' + LineEnding +
    'procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayVarType>): Integer); external;' + LineEnding +
    'procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;' + LineEnding +
    'function <ArrayName>.Sorted: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayVarType>): Integer): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Length: Integer; external;' + LineEnding +
    'function <ArrayName>.Copy: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.First: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Last: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Reversed: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Min: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Max: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Sum: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Mode: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Median: Double; external;' + LineEnding +
    'function <ArrayName>.Mean: Double; external;' + LineEnding +
    'function <ArrayName>.Variance: Double; external;' + LineEnding +
    'function <ArrayName>.Stdev: Double; external;' + LineEnding +
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Remove(Value: <ArrayVarType>): Boolean; external;' + LineEnding +
    'function <ArrayName>.RemoveAll(Value: <ArrayVarType>): Integer; external;' + LineEnding +
    'procedure <ArrayName>.Delete(Index: Integer; Count: Integer = High(Integer)); external;' + LineEnding +
    'procedure <ArrayName>.Insert(Item: <ArrayVarType>; Index: Integer); external;' + LineEnding +
    'procedure <ArrayName>.SetLength(NewLength: Integer); external;' + LineEnding +
    'function <ArrayName>.Pop: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Pop(Index: Integer): <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;' + LineEnding +
    'procedure <ArrayName>.Reverse; external;' + LineEnding +
    'procedure <ArrayName>.Clear; external;' + LineEnding +
    'procedure <ArrayName>.Append(Value: <ArrayVarType>); external;' + LineEnding +
    'procedure <ArrayName>.Extend(Value: <ArrayName>); external;';

  // var x: array of SomeType;
  HELPERS_DYNARRAY_UNTYPED =
    'function <ArrayName>.Low: Integer; external;' + LineEnding +
    'function <ArrayName>.High: Integer; external;' + LineEnding +
    'function <ArrayName>.Contains(Value: <ArrayVarType>): Boolean; external;' + LineEnding +
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer); external;' + LineEnding +
    'function <ArrayName>.Unique: array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.IndexOf(Value: <ArrayVarType>): Integer; external;' + LineEnding +
    'function <ArrayName>.IndicesOf(Value: <ArrayVarType>): TIntegerArray; external;' + LineEnding +
    'procedure <ArrayName>.Sort; external;' + LineEnding +
    'procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayVarType>): Integer); external;' + LineEnding +
    'procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;' + LineEnding +
    'function <ArrayName>.Sorted: array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayVarType>): Integer): array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Length: Integer; external;' + LineEnding +
    'function <ArrayName>.Copy: array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.First: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Last: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Reversed: array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Min: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Max: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Sum: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Mode: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Median: Double; external;' + LineEnding +
    'function <ArrayName>.Mean: Double; external;' + LineEnding +
    'function <ArrayName>.Variance: Double; external;' + LineEnding +
    'function <ArrayName>.Stdev: Double; external;' + LineEnding +
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): array of <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Remove(Value: <ArrayVarType>): Boolean; external;' + LineEnding +
    'function <ArrayName>.RemoveAll(Value: <ArrayVarType>): Integer; external;' + LineEnding +
    'procedure <ArrayName>.Delete(Index: Integer; Count: Integer = High(Integer)); external;' + LineEnding +
    'procedure <ArrayName>.Insert(Item: <ArrayVarType>; Index: Integer); external;' + LineEnding +
    'procedure <ArrayName>.SetLength(NewLength: Integer); external;' + LineEnding +
    'function <ArrayName>.Pop: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Pop(Index: Integer): <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;' + LineEnding +
    'procedure <ArrayName>.Reverse; external;' + LineEnding +
    'procedure <ArrayName>.Clear; external;' + LineEnding +
    'procedure <ArrayName>.Append(Value: <ArrayVarType>); external;' + LineEnding +
    'procedure <ArrayName>.Extend(Value: array of <ArrayVarType>); external;';

  HELPERS_STATICARRAY =
    'function <ArrayName>.Low: Integer; external;' + LineEnding +
    'function <ArrayName>.High: Integer; external;' + LineEnding +
    'function <ArrayName>.Contains(Value: <ArrayVarType>): Boolean; external;' + LineEnding +
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer); external;' + LineEnding +
    'function <ArrayName>.Unique: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.IndexOf(Value: <ArrayVarType>): Integer; external;' + LineEnding +
    'function <ArrayName>.IndicesOf(Value: <ArrayVarType>): TIntegerArray; external;' + LineEnding +
    'procedure <ArrayName>.Sort; external;' + LineEnding +
    'procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayVarType>): Integer); external;' + LineEnding +
    'procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;' + LineEnding +
    'function <ArrayName>.Sorted: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayVarType>): Integer): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Length: Integer; external;' + LineEnding +
    'function <ArrayName>.Copy: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.First: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Last: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Reversed: <ArrayName>; external;' + LineEnding +
    'function <ArrayName>.Min: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Max: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Sum: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Mode: <ArrayVarType>; external;' + LineEnding +
    'function <ArrayName>.Median: Double; external;' + LineEnding +
    'function <ArrayName>.Mean: Double; external;' + LineEnding +
    'function <ArrayName>.Variance: Double; external;' + LineEnding +
    'function <ArrayName>.Stdev: Double; external;' + LineEnding +
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayName>; external;';

function GetArrayHelpers(Decl: TDeclaration): TDeclarationArray;

implementation

var
  ArrayHelperParsers: TCodeParserList;

function GetArrayHelpers(Decl: TDeclaration): TDeclarationArray;

  function Run(Helpers, ArrayName, ArrayVarType: String): TCodeParser;
  var
    I: Integer;
    FileName: String;
  begin
    FileName := '!ArrayHelper::' + ArrayName + '::' + ArrayVarType;
    for I := 0 to ArrayHelperParsers.Count - 1 do
      if (ArrayHelperParsers[I].Lexer.FileName = FileName) then
        Exit(ArrayHelperParsers[I]);

    Helpers := Helpers.Replace('<ArrayName>', ArrayName);
    Helpers := Helpers.Replace('<ArrayVarType>', ArrayVarType);

    Result := TCodeParser.Create();
    Result.SetScript(Helpers, FileName);
    Result.Run();

    ArrayHelperParsers.Add(Result);
  end;

var
  Parser: TCodeParser;
  ArrName, ArrType: String;
begin
  Parser := nil;

  if (Decl is TDeclaration_TypeArray) then
  begin
    ArrName := Decl.Name;
    if (ArrName = '') then
      ArrName := 'array';
    ArrType := Decl.Items.GetTextOfClass(TDeclaration_VarType);

    if (ArrType <> '') then
    begin
      if (Decl is TDeclaration_TypeStaticArray) then
        Parser := Run(HELPERS_STATICARRAY, ArrName, ArrType)
      else
      if (ArrName = 'array') then
        Parser := Run(HELPERS_DYNARRAY_UNTYPED, ArrName, ArrType)
      else
        Parser := Run(HELPERS_DYNARRAY, ArrName, ArrType);
    end;
  end
  else if (Decl is TDeclaration_TypeAlias) then
  begin
    case UpperCase(Decl.Name) of
      'STRING':        Parser := Run(HELPERS_STRING, 'String', 'Char');
      'ANSISTRING':    Parser := Run(HELPERS_STRING, 'AnsiString', 'Char');
      'WIDESTRING':    Parser := Run(HELPERS_STRING, 'WideString', 'WideChar');
      'UNICODESTRING': Parser := Run(HELPERS_STRING, 'UnicodeString', 'UnicodeChar');
    end;
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



