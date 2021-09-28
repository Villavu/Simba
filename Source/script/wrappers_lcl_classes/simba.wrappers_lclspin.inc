type
  PCustomFloatSpinEdit = ^TCustomFloatSpinEdit;
  PCustomSpinEdit = ^TCustomSpinEdit;
  PFloatSpinEdit = ^TFloatSpinEdit;
  PSpinEdit = ^TSpinEdit;

procedure _LapeCustomFloatSpinEdit_GetLimitedValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.GetLimitedValue(PDouble(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_ValueToStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueToStr(PDouble(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_StrToValue(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.StrToValue(PlpString(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_DecimalPlaces_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces;
end;

procedure _LapeCustomFloatSpinEdit_DecimalPlaces_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces := PInteger(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeCustomFloatSpinEdit_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Increment := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeCustomFloatSpinEdit_MinValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.MinValue := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeCustomFloatSpinEdit_MaxValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.MaxValue := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Value;
end;

procedure _LapeCustomFloatSpinEdit_Value_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Value := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_ValueEmpty_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueEmpty;
end;

procedure _LapeCustomFloatSpinEdit_ValueEmpty_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.ValueEmpty := PBoolean(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^ := TCustomFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomFloatSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomFloatSpinEdit(Params^[0])^.Free();
end;

procedure _LapeFloatSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFloatSpinEdit(Params^[0])^ := TFloatSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeFloatSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFloatSpinEdit(Params^[0])^.Free();
end;

procedure _LapeCustomSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Value;
end;

procedure _LapeCustomSpinEdit_Value_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Value := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeCustomSpinEdit_MinValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.MinValue := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeCustomSpinEdit_MaxValue_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.MaxValue := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeCustomSpinEdit_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Increment := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^ := TCustomSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpinEdit(Params^[0])^.Free();
end;

procedure _LapeSpinEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSpinEdit(Params^[0])^ := TSpinEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeSpinEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSpinEdit(Params^[0])^.Free();
end;

