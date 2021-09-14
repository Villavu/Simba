procedure _LapeMFont_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMFont(Params^[0])^.Name;
end;

procedure _LapeMFont_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^.Name := PlpString(Params^[1])^;
end;

procedure _LapeMFont_Data_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POcrData(Result)^ := PMFont(Params^[0])^.Data;
end;

procedure _LapeMFont_Data_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^.Data := POcrData(Params^[1])^;
end;

procedure _LapeMFont_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^ := TMFont.Create();
end;

procedure _LapeMFont_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Result)^ := PMFont(Params^[0])^.Copy();
end;

procedure _LapeMFont_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFont(Params^[0])^.Free();
end;

