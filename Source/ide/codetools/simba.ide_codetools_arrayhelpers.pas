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
  simba.base, simba.ide_codetools_base, simba.ide_codetools_parser, simba.ide_initialization;

function GetArrayHelpers(Decl: TDeclaration): TCodeParser;

implementation

uses
  simba.containers, simba.vartype_string;

type
  TArrayHelperGenerator = class(TObject)
  protected
  type
    TList = specialize TSimbaObjectList<TCodeParser>;
  protected
    FStringHelpers: String;
    FDynHelpers: String;
    FStaticHelpers: String;

    FHelpers: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(Decl: TDeclaration): TCodeParser;
  end;

var
  ArrayHelperGenerator: TArrayHelperGenerator;

function GetArrayHelpers(Decl: TDeclaration): TCodeParser;
begin
  Result := ArrayHelperGenerator.Get(Decl);
end;

constructor TArrayHelperGenerator.Create;
begin
  inherited Create();

  FHelpers := TList.Create(True);

  FStringHelpers := LineEnding.Join([
    'procedure <ArrayName>.SetLength(NewLength: Integer); external;',
    'function <ArrayName>.Length: Integer; external;',
    'function <ArrayName>.Copy: <ArrayName>; external;',
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): <ArrayName>; external;',
    'procedure <ArrayName>.Delete(StartIndex: Integer; Count: Integer = High(Integer)); external;',
    'function <ArrayName>.First: <ArrayVarType>; external;',
    'function <ArrayName>.Last: <ArrayVarType>; external;',
    'function <ArrayName>.Pop: <ArrayVarType>; external;',
    'function <ArrayName>.Pop(Index: Integer): <ArrayVarType>; external;',
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;',
    'procedure <ArrayName>.Reverse; external;',
    'function <ArrayName>.Reversed: <ArrayName>; external;',
    'procedure <ArrayName>.Clear; external;'
  ]);

  FDynHelpers := LineEnding.Join([
    'function <ArrayName>.Low: Integer; external;',
    'function <ArrayName>.High: Integer; external;',
    'function <ArrayName>.Contains(Value: <ArrayVarType>): Boolean; external;',
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer); external;',
    'function <ArrayName>.Unique: <ArrayName>; external;',
    'function <ArrayName>.IndexOf(Value: <ArrayVarType>): Integer; external;',
    'function <ArrayName>.IndicesOf(Value: <ArrayVarType>): TIntegerArray; external;',
    'procedure <ArrayName>.Sort; external;',
    'procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayVarType>): Integer); external;',
    'procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;',
    'function <ArrayName>.Sorted: <ArrayName>; external;',
    'function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayVarType>): Integer): <ArrayName>; external;',
    'function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <ArrayName>; external;',
    'function <ArrayName>.Length: Integer; external;',
    'function <ArrayName>.Copy: <ArrayName>; external;',
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): <ArrayName>; external;',
    'function <ArrayName>.First: <ArrayVarType>; external;',
    'function <ArrayName>.Last: <ArrayVarType>; external;',
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;',
    'function <ArrayName>.Reversed: <ArrayName>; external;',
    'function <ArrayName>.Min: <ArrayVarType>; external;',
    'function <ArrayName>.Max: <ArrayVarType>; external;',
    'function <ArrayName>.Sum: <ArrayVarType>; external;',
    'function <ArrayName>.Mode: <ArrayVarType>; external;',
    'function <ArrayName>.Median: Double; external;',
    'function <ArrayName>.Mean: Double; external;',
    'function <ArrayName>.Variance: Double; external;',
    'function <ArrayName>.Stdev: Double; external;',
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayName>; external;',
    'function <ArrayName>.Remove(Value: <ArrayVarType>): Boolean; external;',
    'function <ArrayName>.RemoveAll(Value: <ArrayVarType>): Integer; external;',
    'procedure <ArrayName>.Delete(Index: Integer; Count: Integer = High(Integer)); external;',
    'procedure <ArrayName>.Insert(Item: <ArrayVarType>; Index: Integer); external;',
    'procedure <ArrayName>.SetLength(NewLength: Integer); external;',
    'function <ArrayName>.Pop: <ArrayVarType>; external;',
    'function <ArrayName>.Pop(Index: Integer): <ArrayVarType>; external;',
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;',
    'procedure <ArrayName>.Reverse; external;',
    'procedure <ArrayName>.Clear; external;',
    'procedure <ArrayName>.Append(Value: <ArrayVarType>); external;',
    'procedure <ArrayName>.Extend(Value: <ArrayName>); external;'
  ]);

  FStaticHelpers := LineEnding.Join([
    'function <ArrayName>.Low: Integer; external;',
    'function <ArrayName>.High: Integer; external;',
    'function <ArrayName>.Contains(Value: <ArrayVarType>): Boolean; external;',
    'procedure <ArrayName>.Swap(FromIndex, ToIndex: Integer); external;',
    'function <ArrayName>.Unique: <ArrayName>; external;',
    'function <ArrayName>.IndexOf(Value: <ArrayVarType>): Integer; external;',
    'function <ArrayName>.IndicesOf(Value: <ArrayVarType>): TIntegerArray; external;',
    'procedure <ArrayName>.Sort; external;',
    'procedure <ArrayName>.Sort(CompareFunc: function(constref L, R: <ArrayVarType>): Integer); external;',
    'procedure <ArrayName>.Sort(Weights: TIntegerArray; LowToHigh: Boolean); external;',
    'function <ArrayName>.Sorted: <ArrayName>; external;',
    'function <ArrayName>.Sorted(CompareFunc: function(constref L, R: <ArrayVarType>): Integer): <ArrayName>; external;',
    'function <ArrayName>.Sorted(Weights: TIntegerArray; LowToHigh: Boolean): <ArrayName>; external;',
    'function <ArrayName>.Length: Integer; external;',
    'function <ArrayName>.Copy: <ArrayName>; external;',
    'function <ArrayName>.Copy(StartIndex: Integer; Count: Integer = High(Integer)): <ArrayName>; external;',
    'function <ArrayName>.First: <ArrayVarType>; external;',
    'function <ArrayName>.Last: <ArrayVarType>; external;',
    'function <ArrayName>.RandomValue: <ArrayVarType>; external;',
    'function <ArrayName>.Reversed: <ArrayName>; external;',
    'function <ArrayName>.Min: <ArrayVarType>; external;',
    'function <ArrayName>.Max: <ArrayVarType>; external;',
    'function <ArrayName>.Sum: <ArrayVarType>; external;',
    'function <ArrayName>.Mode: <ArrayVarType>; external;',
    'function <ArrayName>.Median: Double; external;',
    'function <ArrayName>.Mean: Double; external;',
    'function <ArrayName>.Variance: Double; external;',
    'function <ArrayName>.Stdev: Double; external;',
    'function <ArrayName>.Slice(Start, Stop, Step: Integer): <ArrayName>; external;'
  ]);
end;

destructor TArrayHelperGenerator.Destroy;
begin
  if (FHelpers <> nil) then
    FreeAndNil(FHelpers);

  inherited Destroy;
end;

function TArrayHelperGenerator.Get(Decl: TDeclaration): TCodeParser;
var
  FileName: String;

  function Run(Helpers, ArrayName, ArrayVarType: String): TCodeParser;
  var
    I: Integer;
  begin
    if (ArrayVarType = '') then
      Exit(nil);

    FileName := '!ArrayHelper::' + ArrayName + '::' + ArrayVarType;
    for I := 0 to FHelpers.Count - 1 do
      if (FHelpers[I].Lexer.FileName = FileName) then
      begin
        Result := FHelpers[I];
        Exit;
      end;

    Helpers := Helpers.Replace('<ArrayName>', ArrayName);
    Helpers := Helpers.Replace('<ArrayVarType>', ArrayVarType);

    Result := TCodeParser.Create();
    Result.SetScript(Helpers, FileName);
    Result.Run();

    FHelpers.Add(Result);
  end;

var
  ArrName, ArrType: String;
begin
  Result := nil;

  if (Decl is TDeclaration_TypeAlias) then
  begin
    case Decl.Name.ToUpper() of
      'STRING':        Result := Run(FStringHelpers, 'String', 'Char');
      'ANSISTRING':    Result := Run(FStringHelpers, 'AnsiString', 'Char');
      'WIDESTRING':    Result := Run(FStringHelpers, 'WideString', 'WideChar');
      'UNICODESTRING': Result := Run(FStringHelpers, 'UnicodeString', 'UnicodeChar');
    end;
  end
  else
  if (Decl is TDeclaration_TypeArray) then
  begin
    ArrName := Decl.Name;
    if (ArrName = '') then
      ArrName := 'Array';
    ArrType := Decl.Items.GetTextOfClass(TDeclaration_VarType);
    if (ArrType = '') then
      Exit;

    if (Decl is TDeclaration_TypeStaticArray) then
      Result := Run(FStaticHelpers, ArrName, ArrType)
    else
      Result := Run(FDynHelpers, ArrName, ArrType);
  end;
end;

procedure CreateArrayHelperGenerator;
begin
  ArrayHelperGenerator := TArrayHelperGenerator.Create();
end;

initialization
  SimbaIDEInitialization_AddBeforeCreate(@CreateArrayHelperGenerator, 'Codetools ArrayHelper Generator');

finalization
  if (ArrayHelperGenerator <> nil) then
    FreeAndNil(ArrayHelperGenerator);

end.
