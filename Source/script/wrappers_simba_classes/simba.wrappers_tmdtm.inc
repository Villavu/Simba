procedure _LapeMDTM_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMDTM(Params^[0])^.Name;
end;

procedure _LapeMDTM_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Name := PlpString(Params^[1])^;
end;

procedure _LapeMDTM_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.Index;
end;

procedure _LapeMDTM_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Index := Pinteger(Params^[1])^;
end;

procedure _LapeMDTM_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMDTM(Params^[0])^.ToString();
end;

procedure _LapeMDTM_SaveToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

procedure _LapeMDTM_LoadFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.LoadFromString(PlpString(Params^[1])^);
end;

procedure _LapeMDTM_Normalize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Normalize();
end;

procedure _LapeMDTM_Valid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.Valid();
end;

procedure _LapeMDTM_DeletePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.DeletePoint(Pinteger(Params^[1])^);
end;

procedure _LapeMDTM_SwapPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.SwapPoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMDTM_MovePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.MovePoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMDTM_AddPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.AddPoint(PMDTMPoint(Params^[1])^);
end;

procedure _LapeMDTM_PPoints_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPMDTMPoint(Result)^ := PMDTM(Params^[0])^.PPoints;
end;

procedure _LapeMDTM_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.Count;
end;

procedure _LapeMDTM_Count_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Count := Pinteger(Params^[1])^;
end;

procedure _LapeMDTM_Points_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMPointArray(Result)^ := PMDTM(Params^[0])^.Points;
end;

procedure _LapeMDTM_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^ := TMDTM.Create();
end;

procedure _LapeMDTM_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Free();
end;

