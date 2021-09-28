procedure _LapeJSONArray_create(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create();
end;

procedure _LapeJSONArray_createExExEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Params^[0])^ := TJSONArray.Create(PlpString(Params^[1])^);
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
  PlpString(Result)^ := PJSONArray(Params^[0])^.getString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_isNull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONArray(Params^[0])^.isNull(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_join(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.join(PlpString(Params^[1])^);
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
  PlpString(Result)^ := PJSONArray(Params^[0])^.optString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_optStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.optString(Pinteger(Params^[1])^, PlpString(Params^[2])^);
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
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(PlpString(Params^[1])^);
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
  PJSONArray(Result)^ := PJSONArray(Params^[0])^.put(Pinteger(Params^[1])^, PlpString(Params^[2])^);
end;

procedure _LapeJSONArray_toJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONArray(Params^[0])^.toJSONObject(PJSONArray(Params^[1])^);
end;

procedure _LapeJSONArray_toString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLpstring(Result)^ := PJSONArray(Params^[0])^.toString();
end;

procedure _LapeJSONArray_toStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.toString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONArray_toStringExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONArray(Params^[0])^.toString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
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
  PJSONObject(Params^[0])^ := TJSONObject.Create(PlpString(Params^[1])^);
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
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.accumulate(PlpString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONObject_get(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.get(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_getBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.getBoolean(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_getDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.getDouble(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_getInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.getInt(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_getJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.getJSONArray(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_getJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.getJSONObject(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_getString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.getString(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_has(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.has(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_isNull(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.isNull(PlpString(Params^[1])^);
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
  PZabstractObject(Result)^ := PJSONObject(Params^[0])^.opt(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_optBoolean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.optBoolean(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_optBooleanEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PJSONObject(Params^[0])^.optBoolean(PlpString(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure _LapeJSONObject_optDouble(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.optDouble(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_optDoubleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pdouble(Result)^ := PJSONObject(Params^[0])^.optDouble(PlpString(Params^[1])^, Pdouble(Params^[2])^);
end;

procedure _LapeJSONObject_optInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.optInt(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_optIntEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PJSONObject(Params^[0])^.optInt(PlpString(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONObject_optString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.optString(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_optStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.optString(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure _LapeJSONObject_optJSONArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONArray(Result)^ := PJSONObject(Params^[0])^.optJSONArray(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_optJSONObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.optJSONObject(PlpString(Params^[1])^);
end;

procedure _LapeJSONObject_put(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, Pboolean(Params^[2])^);
end;

procedure _LapeJSONObject_putEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, Pdouble(Params^[2])^);
end;

procedure _LapeJSONObject_putExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONObject_putExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure _LapeJSONObject_putExExExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.put(PlpString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONObject_putOpt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Result)^ := PJSONObject(Params^[0])^.putOpt(PlpString(Params^[1])^, PZAbstractObject(Params^[2])^);
end;

procedure _LapeJSONObject_remove(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PZAbstractObject(Result)^ := PJSONObject(Params^[0])^.remove(PlpString(Params^[1])^);
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
  PlPstring (Result)^ := PJSONObject(Params^[0])^.toString();
end;

procedure _LapeJSONObject_toStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.toString(Pinteger(Params^[1])^);
end;

procedure _LapeJSONObject_toStringExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PJSONObject(Params^[0])^.toString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeJSONObject_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^ := TJSONObject.Create();
end;

procedure _LapeJSONObject_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PJSONObject(Params^[0])^.Free();
end;

