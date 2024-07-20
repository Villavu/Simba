{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_generics;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.ide_codetools_parser;

const
  MAP_METHODS =
    'function <MapName>.Exists(Key: <KeyType>): Boolean; external;'                    + LineEnding +
    'property <MapName>.Value(Key: <KeyType>): <ValueType>; external;'                 + LineEnding +
    'property <MapName>.Value(Key: <KeyType>; Value: <ValueType>); external;'          + LineEnding +
    'property <MapName>.Count: Integer; external;'                                     + LineEnding +
    'procedure <MapName>.Clear; external;'                                             + LineEnding +
    'function <MapName>.IndexOf(Key: <KeyType>): Integer; external;'                   + LineEnding +
    'procedure <MapName>.Delete(Key: <KeyType>); external;'                            + LineEnding +
    'procedure <MapName>.Delete(Index: Integer); external;'                            + LineEnding +
    'function <MapName>.ToString: String; external;'                                   + LineEnding +
    'property <MapName>.ValueFromIndex(Index: Integer): <ValueType>; external;'        + LineEnding +
    'property <MapName>.ValueFromIndex(Index: Integer; Value: <ValueType>); external;' + LineEnding +
    'property <MapName>.KeyFromIndex(Index: Integer): <KeyType>; external;'            + LineEnding +
    'property <MapName>.KeyFromIndex(Index: Integer; Key: <KeyType>); external;'       + LineEnding +
    'property <MapName>.Values: array of <ValueType>; external;'                       + LineEnding +
    'property <MapName>.Keys: array of <KeyType>; external;'                           + LineEnding;

  STRINGMAP_METHODS = MAP_METHODS +
    'procedure <MapName>.Load(FileName: String; Sep: String; StrToValue: function(Str: String): <ValueType>); external;'   + LineEnding +
    'procedure <MapName>.Save(FileName: String; Sep: String; ValueToStr: function(Value: <ValueType>): String); external;' + LineEnding +
    'property <MapName>.CaseSens: Boolean; external;'                                                                      + LineEnding +
    'property <MapName>.CaseSens(Value: Boolean); external;'                                                               + LineEnding +
    'property <MapName>.InvalidVal: <ValueType>; external;'                                                                + LineEnding +
    'property <MapName>.InvalidVal(Value: <ValueType>); external;'                                                         + LineEnding;

  HEAP_METHODS =
    'procedure <HeapName>.Push(Value: <ValueType>; Index: SizeInt; LoHi: Boolean); external;'             + LineEnding +
    'procedure <HeapName>.Pop(LoHi: Boolean); external;'                                                  + LineEnding +
    'function <HeapName>.Peek(LoHi: Boolean): record Value: <ValueType>; Index: SizeInt; end; external;';

function GetGeneric(Decl: TDeclaration): TDeclarationArray;

implementation

var
  GenericParsers: TCodeParserList;

function GetGeneric(Decl: TDeclaration): TDeclarationArray;

  function RunStrMap(Name, Value: String): TCodeParser;
  var
    I: Integer;
    Methods, FileName: String;
  begin
    FileName := '!GenericStringMap::' + Name + '::' + Value;
    for I := 0 to GenericParsers.Count - 1 do
      if (GenericParsers[I].Lexer.FileName = FileName) then
        Exit(GenericParsers[I]);

    Methods := MAP_METHODS;
    Methods := Methods.Replace('<MapName>', Name);
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods);
    Result.Run();

    GenericParsers.Add(Result);
  end;

  function RunMap(Name, Key, Value: String): TCodeParser;
  var
    I: Integer;
    Methods, FileName: String;
  begin
    FileName := '!GenericMap::' + Name + '::' + Key + ', ' + Value;
    for I := 0 to GenericParsers.Count - 1 do
      if (GenericParsers[I].Lexer.FileName = FileName) then
        Exit(GenericParsers[I]);

    Methods := STRINGMAP_METHODS;
    Methods := Methods.Replace('<MapName>', Name);
    Methods := Methods.Replace('<KeyType>', Key);
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods);
    Result.Run();

    GenericParsers.Add(Result);
  end;

  function RunHeap(Name, Value: String): TCodeParser;
  var
    I: Integer;
    Methods, FileName: String;
  begin
    FileName := '!GenericHeap::' + Name + '::' + Value;
    for I := 0 to GenericParsers.Count - 1 do
      if (GenericParsers[I].Lexer.FileName = FileName) then
        Exit(GenericParsers[I]);

    Methods := HEAP_METHODS;
    Methods := Methods.Replace('<HeapName>', Name);
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods);
    Result.Run();

    GenericParsers.Add(Result);
  end;

var
  Parser: TCodeParser;
  Params: TDeclarationArray;
  Name, Kind: String;
begin
  Parser := nil;

  if (Decl is TDeclaration_TypeFakeGeneric) then
  begin
    Kind := Decl.Items.GetTextOfClass(TDeclaration_Identifier);
    Name := Decl.Name;
    if (Name = '') then
      Name := Kind;

    case LowerCase(Kind) of
      'stringmap':
        begin
          Params := Decl.Items.GetByClass(TDeclaration_Parameter, True, True);
          if Length(Params) = 1 then
            Parser := RunMap(Name, 'String', Params[0].Name);
        end;

      'map':
        begin
          Params := Decl.Items.GetByClass(TDeclaration_Parameter, True, True);
          if Length(Params) = 2 then
            Parser := RunMap(Name, Params[0].Name, Params[1].Name);
        end;

      'heap':
        begin
          Params := Decl.Items.GetByClass(TDeclaration_Parameter, True, True);
          if Length(Params) = 1 then
            Parser := RunHeap(Name, Params[0].Name);
        end;
    end;
  end;

  if (Parser <> nil) then
    Result := Parser.Items.ToArray
  else
    Result := [];
end;

initialization
  GenericParsers := TCodeParserList.Create(True);

finalization
  FreeAndNil(GenericParsers);

end.

