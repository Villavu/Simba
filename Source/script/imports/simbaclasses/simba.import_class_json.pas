unit simba.import_class_json;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.jsonparser;

type
  PStringList = ^TStringList;

procedure _LapeJSONArray_create(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create();
end;

procedure _LapeJSONArray_createExExEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create(PString(Params^[1])^);
end;

procedure _LapeJSONArray_get(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PZAbstractObject(Result)^ := PJSONArray(Params^[0])^.get(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_getBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.getBoolean(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_getDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONArray(Params^[0])^.getDouble(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_getInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.getInt(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_getJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.getJSONArray(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_getJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.getJSONObject(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_getString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.getString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_isNull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.isNull(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_join(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.join(PString(Params^[1])^);
end;

procedure _LapeJSONArray_length(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.length();
end;

procedure _LapeJSONArray_opt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PZAbstractObject(Result)^ := PJSONArray(Params^[0])^.opt(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.optBoolean(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optBooleanEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.optBoolean(Pinteger(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure _LapeJSONArray_optDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONArray(Params^[0])^.optDouble(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optDoubleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONArray(Params^[0])^.optDouble(Pinteger(Params^[1])^, Pdouble(Params^[2])^);
end;

procedure _LapeJSONArray_optInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.optInt(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optIntEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONArray(Params^[0])^.optInt(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONArray_optJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.optJSONArray(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.optJSONObject(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.optString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.optString(Pinteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeJSONArray_put(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pboolean(Params^[1])^);
end;

procedure _LapeJSONArray_putEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pdouble(Params^[1])^);
end;

procedure _LapeJSONArray_putExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_putExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(PZAbstractObject(Params^[1])^);
end;

procedure _LapeJSONArray_putExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(PString(Params^[1])^);
end;

procedure _LapeJSONArray_putExExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure _LapeJSONArray_putExExExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, Pdouble(Params^[2])^);
end;

procedure _LapeJSONArray_putExExExExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONArray_putExExExExExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONArray_putExExExExExExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeJSONArray_toJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.toJSONObject(PJSONArray(Params^[1])^);
end;

procedure _LapeJSONArray_toString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.toString();
end;

procedure _LapeJSONArray_toStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.toString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_toStringExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONArray(Params^[0])^.toString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONArray_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create();
end;

procedure _LapeJSONArray_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Params^[0])^.Free();
end;

procedure _LapeJSONObject_create(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create();
end;

procedure _LapeJSONObject_createExExExEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create(PString(Params^[1])^);
end;

procedure _LapeJSONObject_clean(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^.clean();
end;

procedure _LapeJSONObject_clone(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.clone();
end;

procedure _LapeJSONObject_accumulate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.accumulate(PString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONObject_get(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.get(PString(Params^[1])^);
end;

procedure _LapeJSONObject_getBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.getBoolean(PString(Params^[1])^);
end;

procedure _LapeJSONObject_getDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.getDouble(PString(Params^[1])^);
end;

procedure _LapeJSONObject_getInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.getInt(PString(Params^[1])^);
end;

procedure _LapeJSONObject_getJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.getJSONArray(PString(Params^[1])^);
end;

procedure _LapeJSONObject_getJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.getJSONObject(PString(Params^[1])^);
end;

procedure _LapeJSONObject_getString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONObject(Params^[0])^.getString(PString(Params^[1])^);
end;

procedure _LapeJSONObject_has(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.has(PString(Params^[1])^);
end;

procedure _LapeJSONObject_isNull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.isNull(PString(Params^[1])^);
end;

procedure _LapeJSONObject_keys(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Result)^ := PJSONObject(Params^[0])^.keys();
end;

procedure _LapeJSONObject_length(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.length();
end;

procedure _LapeJSONObject_names(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.names();
end;

procedure _LapeJSONObject_opt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PZabstractObject(Result)^ := PJSONObject(Params^[0])^.opt(PString(Params^[1])^);
end;

procedure _LapeJSONObject_optBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.optBoolean(PString(Params^[1])^);
end;

procedure _LapeJSONObject_optBooleanEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.optBoolean(PString(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure _LapeJSONObject_optDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.optDouble(PString(Params^[1])^);
end;

procedure _LapeJSONObject_optDoubleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.optDouble(PString(Params^[1])^, Pdouble(Params^[2])^);
end;

procedure _LapeJSONObject_optInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.optInt(PString(Params^[1])^);
end;

procedure _LapeJSONObject_optIntEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.optInt(PString(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONObject_optString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONObject(Params^[0])^.optString(PString(Params^[1])^);
end;

procedure _LapeJSONObject_optStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONObject(Params^[0])^.optString(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeJSONObject_optJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.optJSONArray(PString(Params^[1])^);
end;

procedure _LapeJSONObject_optJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.optJSONObject(PString(Params^[1])^);
end;

procedure _LapeJSONObject_put(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PString(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure _LapeJSONObject_putEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PString(Params^[1])^, Pdouble(Params^[2])^);
end;

procedure _LapeJSONObject_putExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PString(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONObject_putExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeJSONObject_putExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONObject_putOpt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.putOpt(PString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONObject_remove(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.remove(PString(Params^[1])^);
end;

procedure _LapeJSONObject_assignTo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^.assignTo(PJSONObject(Params^[1])^);
end;

procedure _LapeJSONObject_toJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.toJSONArray(PJSONArray(Params^[1])^);
end;

procedure _LapeJSONObject_toString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString (Result)^ := PJSONObject(Params^[0])^.toString();
end;

procedure _LapeJSONObject_toStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONObject(Params^[0])^.toString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONObject_toStringExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PJSONObject(Params^[0])^.toString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONObject_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create();
end;

procedure _LapeJSONObject_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^.Free();
end;

procedure ImportJSON(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TJSONObject', 'TObject');
    addClass('TJSONArray', 'TObject');
    addGlobalFunc('procedure TJSONArray.Init(); overload', @_LapeJSONArray_create);
    addGlobalFunc('procedure TJSONArray.Init(s : string); overload', @_LapeJSONArray_createExExEx);
    addGlobalFunc('function TJSONArray.get(index : integer): Pointer;', @_LapeJSONArray_get);
    addGlobalFunc('function TJSONArray.getBoolean(index : integer): boolean;', @_LapeJSONArray_getBoolean);
    addGlobalFunc('function TJSONArray.getDouble(index : integer): double;', @_LapeJSONArray_getDouble);
    addGlobalFunc('function TJSONArray.getInt(index : integer): integer;', @_LapeJSONArray_getInt);
    addGlobalFunc('function TJSONArray.getJSONArray(index : integer): TJSONArray;', @_LapeJSONArray_getJSONArray);
    addGlobalFunc('function TJSONArray.getJSONObject(index : integer): TJsonObject;', @_LapeJSONArray_getJSONObject);
    addGlobalFunc('function TJSONArray.getString(index : integer): string;', @_LapeJSONArray_getString);
    addGlobalFunc('function TJSONArray.isNull(index : integer): boolean;', @_LapeJSONArray_isNull);
    addGlobalFunc('function TJSONArray.join(separator : string): string;', @_LapeJSONArray_join);
    addGlobalFunc('function TJSONArray.length: integer;', @_LapeJSONArray_length);
    addGlobalFunc('function TJSONArray.opt(index : integer): Pointer;', @_LapeJSONArray_opt);
    addGlobalFunc('function TJSONArray.optBoolean(index : integer): boolean;', @_LapeJSONArray_optBoolean);
    addGlobalFunc('function TJSONArray.optBoolean(index : integer; defaultValue : boolean): boolean; overload', @_LapeJSONArray_optBooleanEx);
    addGlobalFunc('function TJSONArray.optDouble(index : integer): double;', @_LapeJSONArray_optDouble);
    addGlobalFunc('function TJSONArray.optDouble(index : integer; defaultValue :double ): double; overload', @_LapeJSONArray_optDoubleEx);
    addGlobalFunc('function TJSONArray.optInt(index : integer): integer;', @_LapeJSONArray_optInt);
    addGlobalFunc('function TJSONArray.optInt(index : integer; defaultValue : integer): integer; overload', @_LapeJSONArray_optIntEx);
    addGlobalFunc('function TJSONArray.optJSONArray(index : integer): TJSONArray;', @_LapeJSONArray_optJSONArray);
    addGlobalFunc('function TJSONArray.optJSONObject(index : integer): TJSONObject;', @_LapeJSONArray_optJSONObject);
    addGlobalFunc('function TJSONArray.optString(index : integer): string;', @_LapeJSONArray_optString);
    addGlobalFunc('function TJSONArray.optString(index : integer; defaultValue : string): string; overload', @_LapeJSONArray_optStringEx);
    addGlobalFunc('function TJSONArray.put(value : boolean): TJSONArray; overload', @_LapeJSONArray_put);
    addGlobalFunc('function TJSONArray.put(value : double ): TJSONArray; overload', @_LapeJSONArray_putEx);
    addGlobalFunc('function TJSONArray.put(value : integer): TJSONArray; overload', @_LapeJSONArray_putExEx);
    addGlobalFunc('function TJSONArray.put(value : pointer): TJSONArray; overload', @_LapeJSONArray_putExExEx);
    addGlobalFunc('function TJSONArray.put(value: string): TJSONArray; overload', @_LapeJSONArray_putExExExEx);
    addGlobalFunc('function TJSONArray.put(index : integer ; value : boolean): TJSONArray; overload', @_LapeJSONArray_putExExExExEx);
    addGlobalFunc('function TJSONArray.put(index : integer ; value : double): TJSONArray; overload', @_LapeJSONArray_putExExExExExEx);
    addGlobalFunc('function TJSONArray.put(index : integer ; value : integer): TJSONArray; overload', @_LapeJSONArray_putExExExExExExEx);
    addGlobalFunc('function TJSONArray.put(index : integer ; value : pointer): TJSONArray; overload', @_LapeJSONArray_putExExExExExExExEx);
    addGlobalFunc('function TJSONArray.put(index: integer; value: string): TJSONArray; overload', @_LapeJSONArray_putExExExExExExExExEx);
    addGlobalFunc('function TJSONArray.toJSONObject(names  :TJSONArray ): TJsonObject ; overload', @_LapeJSONArray_toJSONObject);
    addGlobalFunc('function TJSONArray.toString: string; overload; override', @_LapeJSONArray_toString);
    addGlobalFunc('function TJSONArray.toString(indentFactor : integer): string; overload', @_LapeJSONArray_toStringEx);
    addGlobalFunc('function TJSONArray.toString(indentFactor, indent : integer): string; overload', @_LapeJSONArray_toStringExEx);

    addGlobalFunc('procedure TJSONObject.Init(); overload', @_LapeJSONObject_create);
    addGlobalFunc('procedure TJSONObject.Init(s : string); overload', @_LapeJSONObject_createExExExEx);
    addGlobalFunc('procedure TJSONObject.clean;', @_LapeJSONObject_clean);
    addGlobalFunc('function TJSONObject.accumulate(key : string; value : pointer): TJSONObject;', @_LapeJSONObject_accumulate);
    addGlobalFunc('function TJSONObject.get(key : string): pointer;', @_LapeJSONObject_get);
    addGlobalFunc('function TJSONObject.getBoolean(key : string): boolean;', @_LapeJSONObject_getBoolean);
    addGlobalFunc('function TJSONObject.getDouble(key : string): double;', @_LapeJSONObject_getDouble);
    addGlobalFunc('function TJSONObject.getInt(key : string): integer;', @_LapeJSONObject_getInt);
    addGlobalFunc('function TJSONObject.getJSONArray(key : string): TJSONArray;', @_LapeJSONObject_getJSONArray);
    addGlobalFunc('function TJSONObject.getJSONObject(key : string): TJSONObject;', @_LapeJSONObject_getJSONObject);
    addGlobalFunc('function TJSONObject.getString(key : string): string;', @_LapeJSONObject_getString);
    addGlobalFunc('function TJSONObject.has(key : string): boolean;', @_LapeJSONObject_has);
    addGlobalFunc('function TJSONObject.isNull(key : string): boolean;', @_LapeJSONObject_isNull);
    addGlobalFunc('function TJSONObject.keys: TStringList;', @_LapeJSONObject_keys);
    addGlobalFunc('function TJSONObject.length: integer;', @_LapeJSONObject_length);
    addGlobalFunc('function TJSONObject.names: TJSONArray;', @_LapeJSONObject_names);
    addGlobalFunc('function TJSONObject.opt(key : string): pointer;', @_LapeJSONObject_opt);
    addGlobalFunc('function TJSONObject.optBoolean(key : string): boolean;', @_LapeJSONObject_optBoolean);
    addGlobalFunc('function TJSONObject.optBoolean(key : string; defaultValue : boolean): boolean; overload', @_LapeJSONObject_optBooleanEx);
    addGlobalFunc('function TJSONObject.optDouble(key : string): double;', @_LapeJSONObject_optDouble);
    addGlobalFunc('function TJSONObject.optDouble(key : string; defaultValue : double): double; overload', @_LapeJSONObject_optDoubleEx);
    addGlobalFunc('function TJSONObject.optInt(key : string): integer;', @_LapeJSONObject_optInt);
    addGlobalFunc('function TJSONObject.optInt(key : string; defaultValue : integer): integer; overload', @_LapeJSONObject_optIntEx);
    addGlobalFunc('function TJSONObject.optString(key : string): string;', @_LapeJSONObject_optString);
    addGlobalFunc('function TJSONObject.optString(key : string; defaultValue : string): string; overload', @_LapeJSONObject_optStringEx);
    addGlobalFunc('function TJSONObject.optJSONArray(key : string): TJSONArray;', @_LapeJSONObject_optJSONArray);
    addGlobalFunc('function TJSONObject.optJSONObject(key : string): TJSONObject;', @_LapeJSONObject_optJSONObject);
    addGlobalFunc('function TJSONObject.put(key : string; value : boolean): TJSONObject;', @_LapeJSONObject_put);
    addGlobalFunc('function TJSONObject.put(key : string; value : double): TJSONObject; overload', @_LapeJSONObject_putEx);
    addGlobalFunc('function TJSONObject.put(key : string; value : integer): TJSONObject; overload', @_LapeJSONObject_putExEx);
    addGlobalFunc('function TJSONObject.put(key : string; value : string): TJSONObject; overload', @_LapeJSONObject_putExExEx);
    addGlobalFunc('function TJSONObject.put(key : string; value : pointer): TJSONObject; overload', @_LapeJSONObject_putExExExEx);
    addGlobalFunc('function TJSONObject.putOpt(key : string; value : pointer): TJSONObject;', @_LapeJSONObject_putOpt);
    addGlobalFunc('function TJSONObject.remove(key : string): pointer;', @_LapeJSONObject_remove);
    addGlobalFunc('procedure TJSONObject.assignTo(json: TJSONObject);', @_LapeJSONObject_assignTo);
    addGlobalFunc('function TJSONObject.toJSONArray(names : TJSONArray): TJSONArray;', @_LapeJSONObject_toJSONArray);
    addGlobalFunc('function TJSONObject.toString: string ; overload; override', @_LapeJSONObject_toString);
    addGlobalFunc('function TJSONObject.toString(indentFactor : integer): string; overload', @_LapeJSONObject_toStringEx);
    addGlobalFunc('function TJSONObject.toString(indentFactor, indent : integer): string; overload', @_LapeJSONObject_toStringExEx);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportJSON);

end.

