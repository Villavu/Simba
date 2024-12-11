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
    'procedure <MapName>.DeleteIndex(Index: Integer); external;'                       + LineEnding +
    'function <MapName>.ToString: String; external;'                                   + LineEnding +
    'property <MapName>.ValueFromIndex(Index: Integer): <ValueType>; external;'        + LineEnding +
    'property <MapName>.ValueFromIndex(Index: Integer; Value: <ValueType>); external;' + LineEnding +
    'property <MapName>.KeyFromIndex(Index: Integer): <KeyType>; external;'            + LineEnding +
    'property <MapName>.KeyFromIndex(Index: Integer; Key: <KeyType>); external;'       + LineEnding +
    'property <MapName>.Values: array of <ValueType>; external;'                       + LineEnding +
    'property <MapName>.Keys: array of <KeyType>; external;'                           + LineEnding +
    'property <MapName>.InvalidVal: <ValueType>; external;'                            + LineEnding +
    'property <MapName>.InvalidVal(Value: <ValueType>); external;'                     + LineEnding;

  STRINGMAP_METHODS = MAP_METHODS +
    'procedure <MapName>.Load(FileName: String; Sep: String; StrToValue: function(Str: String): <ValueType>); external;'   + LineEnding +
    'procedure <MapName>.Save(FileName: String; Sep: String; ValueToStr: function(Value: <ValueType>): String); external;' + LineEnding +
    'property <MapName>.CaseSens: Boolean; external;'                                                                      + LineEnding +
    'property <MapName>.CaseSens(Value: Boolean); external;'                                                               + LineEnding;

  HEAP_METHODS =
    'procedure <HeapName>.Push(Value: <ValueType>; Index: Integer); external;'                      + LineEnding +
    'property <HeapName>.Pop: record Value: <ValueType>; Index: Integer; end; external;'            + LineEnding +
    'property <HeapName>.Peek: record Value: <ValueType>; Index: Integer; end; external;'           + LineEnding +
    'property <HeapName>.Items: array of record Value: <ValueType>; Index: Integer; end; external;' + LineEnding +
    'property <HeapName>.Count: Integer; external;'                                                 + LineEnding +
    'procedure <HeapName>.Clear; external;'                                                         + LineEnding +
    'function <HeapName>.ToString: String; external;'                                               + LineEnding;

  ARRAYBUFFER_METHODS =
    'property <ArrayBufferName>.Count: Integer; external;'                                 + LineEnding +
    'property <ArrayBufferName>.Items: array of <ValueType>; external;'                    + LineEnding +
    'property <ArrayBufferName>.First: <ValueType>; external;'                             + LineEnding +
    'property <ArrayBufferName>.Last: <ValueType>; external;'                              + LineEnding +
    'property <ArrayBufferName>.Pop: <ValueType>; external;'                               + LineEnding +
    'property <ArrayBufferName>.ToArray: array of <ValueType>; external;'                  + LineEnding +
    'procedure <ArrayBufferName>.Add(Value: <ValueType>); overload; external;'             + LineEnding +
    'procedure <ArrayBufferName>.Add(Values: array of <ValueType>); overload; external;'   + LineEnding +
    'procedure <ArrayBufferName>.Clear; external;'                                         + LineEnding +
    'function <ArrayBufferName>.ToString: String; external;'                               + LineEnding;

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

    Methods := STRINGMAP_METHODS;
    Methods := Methods.Replace('<MapName>', IfThen(Name <> '', Name, 'TStringMap'));
    Methods := Methods.Replace('<KeyType>', 'String');
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods, FileName);
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

    Methods := MAP_METHODS;
    Methods := Methods.Replace('<MapName>', IfThen(Name <> '', Name, 'TMap'));
    Methods := Methods.Replace('<KeyType>', Key);
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods, FileName);
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
    Methods := Methods.Replace('<HeapName>', IfThen(Name <> '', Name, 'THeap'));
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods, FileName);
    Result.Run();

    GenericParsers.Add(Result);
  end;

  function RunArrayBuffer(Name, Value: String): TCodeParser;
  var
    I: Integer;
    Methods, FileName: String;
  begin
    FileName := '!GenericArrayBuffer::' + Name + '::' + Value;
    for I := 0 to GenericParsers.Count - 1 do
      if (GenericParsers[I].Lexer.FileName = FileName) then
        Exit(GenericParsers[I]);

    Methods := ARRAYBUFFER_METHODS;
    Methods := Methods.Replace('<ArrayBufferName>', IfThen(Name <> '', Name, 'TArrayBuffer'));
    Methods := Methods.Replace('<ValueType>', Value);

    Result := TCodeParser.Create();
    Result.SetScript(Methods, FileName);
    Result.Run();

    GenericParsers.Add(Result);
  end;

var
  Parser: TCodeParser;
  Typ: TDeclaration;
  Params: TDeclarationArray;
begin
  Parser := nil;

  if (Decl is TDeclaration_TypeGeneric) then
  begin
    Typ := TDeclaration_TypeGeneric(Decl).Typ;
    Params := TDeclaration_TypeGeneric(Decl).Params;

    if (Typ <> nil) and (Length(Params) > 0) then
    begin
      case UpperCase(Typ.Text) of
        'TSTRINGMAP':
          if (Length(Params) = 1) then
            Parser := RunStrMap(Decl.Name, Params[0].Text);

        'TMAP':
          if (Length(Params) = 2) then
            Parser := RunMap(Decl.Name, Params[0].Text, Params[1].Text);

        'THEAP':
          if (Length(Params) = 1) then
            Parser := RunHeap(Decl.Name, Params[0].Text);

        'TARRAYBUFFER':
          if (Length(Params) = 1) then
           Parser := RunArrayBuffer(Decl.Name, Params[0].Text);
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

