{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.codetools_arrayhelpers;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.codeparser;

function ParseArrayHelpers(Arr: TciArrayType): TCodeParser;
function ParseStringHelpers(StrType, CharType: String): TCodeParser;

implementation

const
  STRING_HELPERS: TStringArray = (
    'procedure <Array>.SetLength(NewLength: Integer); external;',
    'function <Array>.Length: Integer; external;',
    'function <Array>.Copy: <Array>; external;',
    'function <Array>.Copy(StartIndex; Count: Integer = High(Integer)): <Array>; external;',
    'function <Array>.First: <ArrayType>; external;',
    'function <Array>.Last: <ArrayType>; external;',
    'function <Array>.Pop: <ArrayType>; external;',
    'function <Array>.Pop(Index: Integer): <ArrayType>; external;',
    'function <Array>.RandomValue: <ArrayType>; external;',
    'procedure <Array>.Reverse; external;',
    'function <Array>.Reversed: <Array>; external;',
    'procedure <Array>.Clear; external;',
    'procedure <Array>.Append(Value: <ArrayType>); external;',
    'procedure <Array>.Extend(Value: <ArrayType>); external;'
  );

  STATIC_HELPERS: TStringArray = (
    'function <Array>.Low: Integer; external;',
    'function <Array>.High: Integer; external;',
    'function <Array>.Contains(Value: <ArrayType>): Boolean; external;',
    'procedure <Array>.Swap(FromIndex, ToIndex: Integer); external;',
    'function <Array>.Unique: <Array>; external;',
    'function <Array>.IndexOf(Value: <ArrayType>): Integer; external;',
    'function <Array>.IndicesOf(Value: <ArrayType>): TIntegerArray; external;',
    'procedure <Array>.Sort; external;',
    'procedure <Array>.Sort(CompareFunc: function(constref L, R: <ArrayType>): Integer); external;',
    'procedure <Array>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;',
    'function <Array>.Sorted: <Array>; external;',
    'function <Array>.Sorted(CompareFunc: function(constref L, R: <ArrayType>): Integer): <Array>; external;',
    'function <Array>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <Array>; external;',
    'function <Array>.Length: Integer; external;',
    'function <Array>.Copy: <Array>; external;',
    'function <Array>.Copy(StartIndex; Count: Integer = High(Integer)): <Array>; external;',
    'function <Array>.First: <ArrayType>; external;',
    'function <Array>.Last: <ArrayType>; external;',
    'function <Array>.RandomValue: <ArrayType>; external;',
    'function <Array>.Reversed: <Array>; external;',
    'function <Array>.Min: <ArrayType>; external;',
    'function <Array>.Max: <ArrayType>; external;',
    'function <Array>.Sum: <ArrayType>; external;',
    'function <Array>.Mode: <ArrayType>; external;',
    'function <Array>.Median: Double; external;',
    'function <Array>.Mean: Double; external;',
    'function <Array>.Variance: Double; external;',
    'function <Array>.Stddev: Double; external;',
    'function <Array>.Slice(Start, Stop, Step: Integer): <Array>; external;'
  );

  DYNAMIC_HELPERS: TStringArray = (
    'function <Array>.Low: Integer; external;',
    'function <Array>.High: Integer; external;',
    'function <Array>.Contains(Value: <ArrayType>): Boolean; external;',
    'procedure <Array>.Swap(FromIndex, ToIndex: Integer); external;',
    'function <Array>.Unique: <Array>; external;',
    'function <Array>.IndexOf(Value: <ArrayType>): Integer; external;',
    'function <Array>.IndicesOf(Value: <ArrayType>): TIntegerArray; external;',
    'procedure <Array>.Sort; external;',
    'procedure <Array>.Sort(CompareFunc: function(constref L, R: <ArrayType>): Integer); external;',
    'procedure <Array>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;',
    'function <Array>.Sorted: <Array>; external;',
    'function <Array>.Sorted(CompareFunc: function(constref L, R: <ArrayType>): Integer): <Array>; external;',
    'function <Array>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <Array>; external;',
    'function <Array>.Length: Integer; external;',
    'function <Array>.Copy: <Array>; external;',
    'function <Array>.Copy(StartIndex; Count: Integer = High(Integer)): <Array>; external;',
    'function <Array>.First: <ArrayType>; external;',
    'function <Array>.Last: <ArrayType>; external;',
    'function <Array>.RandomValue: <ArrayType>; external;',
    'function <Array>.Reversed: <Array>; external;',
    'function <Array>.Min: <ArrayType>; external;',
    'function <Array>.Max: <ArrayType>; external;',
    'function <Array>.Sum: <ArrayType>; external;',
    'function <Array>.Mode: <ArrayType>; external;',
    'function <Array>.Median: Double; external;',
    'function <Array>.Mean: Double; external;',
    'function <Array>.Variance: Double; external;',
    'function <Array>.Stddev: Double; external;',
    'function <Array>.Slice(Start, Stop, Step: Integer): <Array>; external;',
    'function <Array>.Remove(Value: <ArrayType>): Boolean; external;',
    'function <Array>.RemoveAll(Value: <ArrayType>): Integer; external;',
    'procedure <Array>.Delete(Index, Count: Integer); external;',
    'procedure <Array>.Delete(Index: Integer; Count: Integer = High(Integer)); external;',
    'procedure <Array>.Insert(Item: <ArrayType>; Index: Integer); external;',
    'procedure <Array>.SetLength(NewLength: Integer); external;',
    'function <Array>.Pop: <ArrayType>; external;',
    'function <Array>.Pop(Index: Integer): <ArrayType>; external;',
    'function <Array>.RandomValue: <ArrayType>; external;',
    'procedure <Array>.Reverse; external;',
    'procedure <Array>.Clear; external;',
    'procedure <Array>.Append(Value: <ArrayType>); external;',
    'procedure <Array>.Extend(Value: <ArrayType>); external;'
   );

function ParseArrayHelpers(Arr: TciArrayType): TCodeParser;
var
  Script: String;
begin
  Result := TCodeParser.Create();

  if (Arr.GetType() <> nil) then
  begin
    if Arr.IsStatic() then
      Script := String.Join('', STATIC_HELPERS)
    else
      Script := String.Join('', DYNAMIC_HELPERS);

    if (Arr.Name <> '') then
      Script := Script.Replace('<Array>', Arr.Name)
    else
      Script := Script.Replace('<Array>', 'Array');

    Script := Script.Replace('<ArrayType>', Arr.GetType().CleanText);

    Result.Run(Script, '!ArrayHelper');
  end;
end;

function ParseStringHelpers(StrType, CharType: String): TCodeParser;
var
  Script: String;
begin
  Script := String.Join('', STRING_HELPERS);
  Script := Script.Replace('<Array>', StrType);
  Script := Script.Replace('<ArrayType>', CharType);

  Result := TCodeParser.Create();
  Result.Run(Script, '!StringHelper');
end;

end.

