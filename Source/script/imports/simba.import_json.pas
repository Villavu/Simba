unit simba.import_json;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script, simba.script_objectutil;

procedure ImportJson(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.json;

type
  PJSONItemType = ^EJSONItemType;
  PJSONFormatOptions = ^EJSONFormatOptions;

(*
JSON
====
JSON parsing.
*)

(*
TJSONParser.Construct
---------------------
```
function TJSONArray.Construct: TJSONArray; static;
function TJSONObject.Construct: TJSONObject; static;
function TJSONParser.Construct: TJSONParser; static;
```
Constructors for JSON. Use the `new` keyword for these.

```
var
  parser: TJSONParser;
begin
  parser := new TJSONParser();
  parser.Parse('{"someNumber": 123, someString: "hello"}');

  WriteLn parser.Typ;
  WriteLn parser.Count;
  WriteLn parser.Item[0].Typ;
  WriteLn parser.Item[0].AsInt;
  WriteLn parser.Item['someString'].Typ;
  WriteLn parser.Item['someString'].AsString;
end;
```
*)
procedure _LapeJSONArray_Construct(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := NewJSONArray();
end;

procedure _LapeJSONObject_Construct(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := NewJSONObject();
end;

procedure _LapeJSONParser_Construct(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := NewJSONObject();
end;

procedure _LapeJSONItem_Destroy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if IsLapeObjectManage(Params^[0]) and Assigned(PLapeObjectJSON(Params^[0])^^) then
    PLapeObjectJSON(Params^[0])^^.Free();
end;

(*
TJSONParser.Parse
-----------------
```
procedure TJSONParser.Parse(Str: String);
```
Parse a json string.
*)
procedure _LapeJSONParser_Parse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.Parse(PString(Params^[1])^);
end;

(*
TJSONParser.Load
----------------
```
procedure TJSONParser.Load(FileName: String);
```
Parse json from a file.
*)
procedure _LapeJSONParser_Load(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.Load(PString(Params^[1])^);
end;

(*
TJSONParser.Save
----------------
```
procedure TJSONParser.Save(FileName: String; Options: EJSONFormatOptions = []; Indent: Integer = 2);
```
Formats the json parser to a string and saves to file.
*)
procedure _LapeJSONParser_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.Save(PString(Params^[1])^, PJSONFormatOptions(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TJSONItem.Typ
-------------
```
property TJSONItem.Typ: EJSONType
```
Returns json type of the item.

This can be any of:
 - `EJSONType.UNKNOWN`
 - `EJSONType.INT`
 - `EJSONType.FLOAT`
 - `EJSONType.STR`
 - `EJSONType.BOOL`
 - `EJSONType.NULL`
 - `EJSONType.ARR`
 - `EJSONType.OBJ`
*)
procedure _LapeJSONItem_ItemType_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PJSONItemType(Result)^ := PLapeObjectJSON(Params^[0])^^.Typ;
end;

procedure _LapeJSONItem_AsInt_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PLapeObjectJSON(Params^[0])^^.AsInt;
end;

procedure _LapeJSONItem_AsInt_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AsInt := PInt64(Params^[1])^;
end;

procedure _LapeJSONItem_AsFloat_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PLapeObjectJSON(Params^[0])^^.AsFloat;
end;

procedure _LapeJSONItem_AsFloat_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AsFloat := PDouble(Params^[1])^;
end;

procedure _LapeJSONItem_AsString_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectJSON(Params^[0])^^.AsString;
end;

procedure _LapeJSONItem_AsString_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AsString := PString(Params^[1])^;
end;

procedure _LapeJSONItem_AsUnicodeString_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUnicodeString(Result)^ := PLapeObjectJSON(Params^[0])^^.AsUnicodeString;
end;

procedure _LapeJSONItem_AsUnicodeString_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AsUnicodeString := PUnicodeString(Params^[1])^;
end;

procedure _LapeJSONItem_AsBool_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.AsBool;
end;

procedure _LapeJSONItem_AsBool_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AsBool := PBoolean(Params^[1])^;
end;

procedure _LapeJSONItem_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectJSON(Params^[0])^^.Count;
end;

procedure _LapeJSONItem_Key_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectJSON(Params^[0])^^.Key[PInteger(Params^[1])^];
end;

procedure _LapeJSONItem_Key_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.Key[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeJSONItem_Item_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := PLapeObjectJSON(Params^[0])^^.ItemsByKey[PString(Params^[1])^];
  SetLapeObjectManage(Result, False);
end;

procedure _LapeJSONItem_ItemFromIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := PLapeObjectJSON(Params^[0])^^.ItemsByIndex[PInteger(Params^[1])^];
  SetLapeObjectManage(Result, False);
end;

procedure _LapeJSONItem_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  SetLapeObjectManage(Params^[2], False);
  PLapeObjectJSON(Params^[0])^^.Add(PString(Params^[1])^, PLapeObjectJSON(Params^[2])^^);
end;

procedure _LapeJSONItem_AddInt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AddInt(PString(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeJSONItem_AddFloat(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AddFloat(PString(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeJSONItem_AddString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AddString(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeJSONItem_AddUnicodeString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AddUnicodeString(PString(Params^[1])^, PUnicodeString(Params^[2])^);
end;

procedure _LapeJSONItem_AddBool(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.AddBool(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeJSONItem_AddArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SetLapeObjectManage(Params^[2], False);
  PLapeObjectJSON(Params^[0])^^.AddArray(PString(Params^[1])^, PLapeObjectJSON(Params^[2])^^);
end;

procedure _LapeJSONItem_AddObject(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SetLapeObjectManage(Params^[2], False);
  PLapeObjectJSON(Params^[0])^^.AddObject(PString(Params^[1])^, PLapeObjectJSON(Params^[2])^^);
end;

procedure _LapeJSONItem_Has1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.Has(PString(Params^[1])^);
end;

procedure _LapeJSONItem_Has2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.Has(PString(Params^[1])^, PJSONItemType(Params^[2])^);
end;

procedure _LapeJSONItem_GetInt(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetInt(PString(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeJSONItem_GetString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetString(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeJSONItem_GetUnicodeString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetUnicodeString(PString(Params^[1])^, PUnicodeString(Params^[2])^);
end;

procedure _LapeJSONItem_GetBool(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetBool(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeJSONItem_GetArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
var
  Item: TSimbaJSONItem;
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetArray(PString(Params^[1])^, Item);
  PLapeObject(Params^[2])^ := LapeObjectAlloc(Item, False);
end;

procedure _LapeJSONItem_GetObject(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
var
  Item: TSimbaJSONItem;
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetObject(PString(Params^[1])^, Item);
  PLapeObject(Params^[2])^ := LapeObjectAlloc(Item, False);
end;

procedure _LapeJSONItem_GetFloat(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectJSON(Params^[0])^^.GetFloat(PString(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapeJSONItem_FindPath(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := PLapeObjectJSON(Params^[0])^^.FindPath(PString(Params^[1])^);
  SetLapeObjectManage(Result, False);
end;

procedure _LapeJSONItem_Clone(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := PLapeObjectJSON(Params^[0])^^.Clone();
end;

procedure _LapeJSONItem_Delete(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.Delete(PLapeObjectJSON(Params^[1])^^);
end;

procedure _LapeJSONItem_Extract(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Result)^^ := PLapeObjectJSON(Params^[0])^^.Extract(PLapeObjectJSON(Params^[1])^^);
  SetLapeObjectManage(Result, True);
end;

procedure _LapeJSONItem_Clear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectJSON(Params^[0])^^.Clear();
end;

(*
TJSONItem.Format
----------------
```
function TJSONItem.Format(Options: EJSONFormatOptions = []; Indent: Integer = 2): String;
```
Formats the json item into a JSON string.

Options can be a set of:
  - `EJSONFormatOption.SINGLE_LINE_ARR`: Array without CR/LF : all on one line
  - `EJSONFormatOption.SINGLE_LINE_OBJ`: Object without CR/LF : all on one line
  - `EJSONFormatOption.NO_QUOTE_MEMBERS`: Do not quote object member names.
  - `EJSONFormatOption.USE_TABS`: Use tab characters instead of spaces.
  - `EJSONFormatOption.NO_WHITESPACE`: Do not use whitespace at all
*)
procedure _LapeJSONItem_Format(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PLapeObjectJSON(Params^[0])^^.Format(PJSONFormatOptions(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportJSON(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'JSON';

    LapeObjectImport(Script.Compiler, 'TJSONItem');

    addGlobalType('type TJSONItem', 'TJSONArray');
    addGlobalType('type TJSONItem', 'TJSONObject');
    addGlobalType('type TJSONItem', 'TJSONParser');
    addGlobalType('enum(SINGLE_LINE_ARR, SINGLE_LINE_OBJ, NO_QUOTE_MEMBERS, USE_TABS, NO_WHITESPACE)', 'EJSONFormatOption');
    addGlobalType('enum(UNKNOWN, INT, FLOAT, STR, BOOL, NULL, ARR, OBJ)', 'EJSONType');
    addGlobalType('set of EJSONFormatOption', 'EJSONFormatOptions');
    addGlobalFunc('function TJSONArray.Construct: TJSONArray; static;', @_LapeJSONArray_Construct);
    addGlobalFunc('function TJSONObject.Construct: TJSONObject; static;', @_LapeJSONObject_Construct);
    addGlobalFunc('function TJSONParser.Construct: TJSONParser; static;', @_LapeJSONParser_Construct);
    addGlobalFunc('procedure TJSONItem.Destroy;', @_LapeJSONItem_Destroy);

    addGlobalFunc('procedure TJSONParser.Load(FileName: String);', @_LapeJSONParser_Load);
    addGlobalFunc('procedure TJSONParser.Parse(Str: String);', @_LapeJSONParser_Parse);
    addGlobalFunc('procedure TJSONParser.Save(FileName: String; Options: EJSONFormatOptions = []; Indent: Integer = 2);', @_LapeJSONParser_Save);

    addProperty('TJSONItem', 'Typ', 'EJSONType', @_LapeJSONItem_ItemType_Read);
    addProperty('TJSONItem', 'AsInt', 'Int64', @_LapeJSONItem_AsInt_Read, @_LapeJSONItem_AsInt_Write);
    addProperty('TJSONItem', 'AsString', 'String', @_LapeJSONItem_AsString_Read, @_LapeJSONItem_AsString_Write);
    addProperty('TJSONItem', 'AsUnicodeString', 'UnicodeString', @_LapeJSONItem_AsUnicodeString_Read, @_LapeJSONItem_AsUnicodeString_Write);
    addProperty('TJSONItem', 'AsBool', 'Boolean', @_LapeJSONItem_AsBool_Read, @_LapeJSONItem_AsBool_Write);
    addProperty('TJSONItem', 'AsFloat', 'Double', @_LapeJSONItem_AsFloat_Read, @_LapeJSONItem_AsFloat_Write);
    addProperty('TJSONItem', 'Count', 'Integer', @_LapeJSONItem_Count_Read);
    addPropertyIndexed('TJSONItem', 'Key', 'Index: Integer', 'String', @_LapeJSONItem_Key_Read, @_LapeJSONItem_Key_Write);
    addPropertyIndexed('TJSONItem', 'Item', 'Key: String', 'TJSONItem', @_LapeJSONItem_Item_Read);
    addPropertyIndexed('TJSONItem', 'Item', 'Index: Integer', 'TJSONItem', @_LapeJSONItem_ItemFromIndex_Read);

    addGlobalFunc('function TJSONItem.Format(Options: EJSONFormatOptions = []; Indent: Integer = 2): String;', @_LapeJSONItem_Format);

    addGlobalFunc('procedure TJSONItem.Add(Key: String; Item: TJSONItem)', @_LapeJSONItem_Add);
    addGlobalFunc('procedure TJSONItem.AddInt(Key: String; Value: Int64)', @_LapeJSONItem_AddInt);
    addGlobalFunc('procedure TJSONItem.AddString(Key: String; Value: String)', @_LapeJSONItem_AddString);
    addGlobalFunc('procedure TJSONItem.AddUnicodeString(Key: String; Value: UnicodeString)', @_LapeJSONItem_AddUnicodeString);
    addGlobalFunc('procedure TJSONItem.AddBool(Key: String; Value: Boolean)', @_LapeJSONItem_AddBool);
    addGlobalFunc('procedure TJSONItem.AddFloat(Key: String; Value: Double)', @_LapeJSONItem_AddFloat);
    addGlobalFunc('procedure TJSONItem.AddArray(Key: String; Value: TJSONItem)', @_LapeJSONItem_AddArray);
    addGlobalFunc('procedure TJSONItem.AddObject(Key: String; Value: TJSONItem)', @_LapeJSONItem_AddObject);
    addGlobalFunc('function TJSONItem.Has(Key: String): Boolean; overload', @_LapeJSONItem_Has1);
    addGlobalFunc('function TJSONItem.Has(Key: String; Typ: EJSONType): Boolean; overload', @_LapeJSONItem_Has2);

    addGlobalFunc('function TJSONItem.GetInt(Key: String; out Value: Int64): Boolean', @_LapeJSONItem_GetInt);
    addGlobalFunc('function TJSONItem.GetFloat(Key: String; out Value: Double): Boolean', @_LapeJSONItem_GetFloat);
    addGlobalFunc('function TJSONItem.GetString(Key: String; out Value: String): Boolean', @_LapeJSONItem_GetString);
    addGlobalFunc('function TJSONItem.GetUnicodeString(Key: String; out Value: String): Boolean', @_LapeJSONItem_GetUnicodeString);
    addGlobalFunc('function TJSONItem.GetBool(Key: String; out Value: Boolean): Boolean', @_LapeJSONItem_GetBool);
    addGlobalFunc('function TJSONItem.GetArray(Key: String; out Value: TJSONItem): Boolean', @_LapeJSONItem_GetArray);
    addGlobalFunc('function TJSONItem.GetObject(Key: String; out Value: TJSONItem): Boolean', @_LapeJSONItem_GetObject);

    addGlobalFunc('function TJSONItem.FindPath(Path: String): TJSONItem', @_LapeJSONItem_FindPath);
    addGlobalFunc('function TJSONItem.Clone: TJSONItem', @_LapeJSONItem_Clone);
    addGlobalFunc('procedure TJSONItem.Delete(Item: TJSONItem)', @_LapeJSONItem_Delete);
    addGlobalFunc('function TJSONItem.Extract(Item: TJSONItem): TJSONItem', @_LapeJSONItem_Extract);
    addGlobalFunc('procedure TJSONItem.Clear', @_LapeJSONItem_Clear);

    addDelayedCode('function ToString(constref Item: TJSONItem): String;   override; begin Result := Item.Format(); end;');
    addDelayedCode('function ToString(constref Item: TJSONArray): String;  override; begin Result := Item.Format(); end;');
    addDelayedCode('function ToString(constref Item: TJSONObject): String; override; begin Result := Item.Format(); end;');
    addDelayedCode('function ToString(constref Item: TJSONParser): String; override; begin Result := Item.Format(); end;');

    DumpSection := '';
  end;
end;

end.
