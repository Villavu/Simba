unit simba.import_class_json;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportJson(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes,
  simba.json;

procedure _LapeJSONElement_Keys(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaJSONElement(Params^[0])^.Keys;
end;

procedure _LapeJSONElement_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaJSONElement(Params^[0])^.Count;
end;

procedure _LapeJSONElement_GetItem(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONElement(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeJSONElement_AddValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Params^[0])^.AddValue(PString(Params^[1])^, PVariant(Params^[2])^);
end;

procedure _LapeJSONElement_AddArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONElement(Params^[0])^.AddArray(PString(Params^[1])^);
end;

procedure _LapeJSONElement_AddObject(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONElement(Params^[0])^.AddObject(PString(Params^[1])^);
end;

procedure _LapeJSONElement_AddElement(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Params^[0])^.AddElement(PString(Params^[1])^, PSimbaJSONElement(Params^[2])^);
end;

procedure _LapeJSONElement_ValueType(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  ESimbaJSONValueType(Result^) := PSimbaJSONElement(Params^[0])^.ValueType;
end;

procedure _LapeJSONElement_GetValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := PSimbaJSONElement(Params^[0])^.Value;
end;

procedure _LapeJSONElement_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Params^[0])^.Value := PVariant(Params^[1])^;
end;

procedure _LapeJSONElement_AsString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaJSONElement(Params^[0])^.AsString;
end;

procedure _LapeJSONElement_Clone(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONElement(Params^[0])^.Clone();
end;

procedure _LapeJSONElement_IsValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONElement(Params^[0])^.IsValue;
end;

procedure _LapeJSONElement_IsArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONElement(Params^[0])^.IsArray;
end;

procedure _LapeJSONElement_IsObject(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONElement(Params^[0])^.IsObject;
end;

procedure _LapeJSONElement_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeJSONElement_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeJSONElement_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Params^[0])^.Clear();
end;

procedure _LapeJSONElement_Find(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONElement(Params^[0])^.Find(PString(Params^[1])^, PSimbaJSONElement(Params^[2])^);
end;

procedure _LapeJSONParser_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Result)^ := TSimbaJSONParser.Create(PString(Params^[0])^);
end;

procedure _LapeJSONParser_CreateFromFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Result)^ := TSimbaJSONParser.Create(PString(Params^[0])^);
end;

procedure _LapeJSONParser_SaveToFile(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONParser(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeJSONParser_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Params^[0])^.Clear();
end;

procedure _LapeJSONParser_AddValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Params^[0])^.AddValue(PString(Params^[1])^, PVariant(Params^[2])^);
end;

procedure _LapeJSONParser_AddArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONParser(Params^[0])^.AddArray(PString(Params^[1])^);
end;

procedure _LapeJSONParser_AddObject(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONParser(Params^[0])^.AddObject(PString(Params^[1])^);
end;

procedure _LapeJSONParser_AddElement(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Params^[0])^.AddElement(PString(Params^[1])^, PSimbaJSONElement(Params^[2])^);
end;

procedure _LapeJSONParser_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaJSONParser(Params^[0])^.Count;
end;

procedure _LapeJSONParser_GetItem(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONElement(Result)^ := PSimbaJSONParser(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeJSONParser_AsString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaJSONParser(Params^[0])^.AsString;
end;

procedure _LapeJSONParser_Find(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONParser(Params^[0])^.Find(PString(Params^[1])^, PSimbaJSONElement(Params^[2])^);
end;

procedure _LapeJSONParser_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeJSONParser_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaJSONParser(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeJSONParser_FindPath(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaJSONParser(Params^[0])^.FindPath(PString(Params^[1])^, PSimbaJSONElement(Params^[2])^);
end;

procedure _LapeJSONParser_Keys(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PSimbaJSONParser(Params^[0])^.Keys;
end;

procedure ImportJSON(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'TJSONParser';

    addGlobalType('enum(UNKNOWN, NULL, INT, FLOAT, STR, BOOL)', 'EJSONValueType');
    addGlobalType('record {%CODETOOLS OFF}InternalData: Pointer;{%CODETOOLS ON} end', 'TJSONElement');
    if (getGlobalType('TJSONElement').Size <> SizeOf(TSimbaJSONParser)) then
      raise Exception.Create('SizeOf(TJSONElement) is wrong!');

    addGlobalFunc('function TJSONElement.Keys: TStringArray', @_LapeJSONElement_Keys);
    addGlobalFunc('function TJSONElement.Count: Integer', @_LapeJSONElement_Count);
    addGlobalFunc('function TJSONElement.GetItem(Index: Integer): TJSONElement', @_LapeJSONElement_GetItem);
    addGlobalFunc('procedure TJSONElement.AddValue(Key: String; Value: Variant)', @_LapeJSONElement_AddValue);
    addGlobalFunc('function TJSONElement.AddArray(Key: String): TJSONElement', @_LapeJSONElement_AddArray);
    addGlobalFunc('function TJSONElement.AddObject(Key: String): TJSONElement', @_LapeJSONElement_AddObject);
    addGlobalFunc('procedure TJSONElement.AddElement(Key: String; Element: TJSONElement)', @_LapeJSONElement_AddElement);
    addGlobalFunc('function TJSONElement.ValueType: EJSONValueType', @_LapeJSONElement_ValueType);
    addGlobalFunc('function TJSONElement.GetValue: Variant', @_LapeJSONElement_GetValue);
    addGlobalFunc('procedure TJSONElement.SetValue(NewValue: Variant);', @_LapeJSONElement_SetValue);
    addGlobalFunc('function TJSONElement.AsString: String', @_LapeJSONElement_SetValue);
    addGlobalFunc('function TJSONElement.Clone: TJSONElement', @_LapeJSONElement_Clone);
    addGlobalFunc('function TJSONElement.IsValue: Boolean', @_LapeJSONElement_IsValue);
    addGlobalFunc('function TJSONElement.IsArray: Boolean', @_LapeJSONElement_IsArray);
    addGlobalFunc('function TJSONElement.IsObject: Boolean', @_LapeJSONElement_IsObject);
    addGlobalFunc('procedure TJSONElement.Clear', @_LapeJSONElement_Clear);
    addGlobalFunc('procedure TJSONElement.Delete(Key: String); overload', @_LapeJSONElement_Delete1);
    addGlobalFunc('procedure TJSONElement.Delete(Index: Integer); overload', @_LapeJSONElement_Delete2);
    addGlobalFunc('function TJSONElement.Find(Key: String; out Element: TJSONElement): Boolean;', @_LapeJSONElement_Find);

    addClass('TJSONParser');

    addGlobalFunc('function TJSONParser.Create(Str: String = ""): TJSONParser; static;', @_LapeJSONParser_Create);
    addGlobalFunc('function TJSONParser.CreateFromFile(FileName: String): TJSONParser; static', @_LapeJSONParser_CreateFromFile);
    addGlobalFunc('function TJSONParser.SaveToFile(FileName: String): Boolean', @_LapeJSONParser_SaveToFile);
    addGlobalFunc('function TJSONParser.Keys: TStringArray', @_LapeJSONParser_Keys);
    addGlobalFunc('procedure TJSONParser.Clear;', @_LapeJSONParser_Clear);
    addGlobalFunc('procedure TJSONParser.AddValue(Key: String; Value: Variant);', @_LapeJSONParser_AddValue);
    addGlobalFunc('function TJSONParser.AddArray(Key: String): TJSONElement;', @_LapeJSONParser_AddArray);
    addGlobalFunc('function TJSONParser.AddObject(Key: String): TJSONElement;', @_LapeJSONParser_AddObject);
    addGlobalFunc('procedure TJSONParser.AddElement(Key: String; Element: TJSONElement)', @_LapeJSONParser_AddElement);
    addGlobalFunc('function TJSONParser.Count: Integer', @_LapeJSONParser_Count);
    addGlobalFunc('function TJSONParser.GetItem(Index: Integer): TJSONElement', @_LapeJSONParser_GetItem);
    addGlobalFunc('function TJSONParser.AsString: String', @_LapeJSONParser_AsString);
    addGlobalFunc('function TJSONParser.Find(Key: String; out Element: TJSONElement): Boolean;', @_LapeJSONParser_Find);
    addGlobalFunc('procedure TJSONParser.Delete(Key: String); overload;', @_LapeJSONParser_Delete1);
    addGlobalFunc('procedure TJSONParser.Delete(Index: Integer); overload;', @_LapeJSONParser_Delete2);
    addGlobalFunc('function TJSONParser.FindPath(Path: String; out Element: TJSONElement): Boolean;', @_LapeJSONParser_FindPath);

    ImportingSection := '';
  end;
end;

end.

