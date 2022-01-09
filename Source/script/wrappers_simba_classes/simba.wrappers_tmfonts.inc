procedure _LapeMFonts_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^ := TMFonts.Create(PObject(Params^[1])^);
end;

procedure _LapeMFonts_GetFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POcrData(Result)^ := PMFonts(Params^[0])^.GetFontData(PString(Params^[1])^);
end;

procedure _LapeMFonts_FreeFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFonts(Params^[0])^.FreeFont(PString(Params^[1])^);
end;

procedure _LapeMFonts_LoadFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadFont(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMFonts_LoadSystemFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFonts(Params^[0])^.LoadSystemFont(PFont(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMFonts_Copy(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Result)^ := PMFonts(Params^[0])^.Copy(PObject(Params^[1])^);
end;

procedure _LapeMFonts_Count(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMFonts(Params^[0])^.Count();
end;

procedure _LapeMFonts_Path_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMFonts(Params^[0])^.Path;
end;

procedure _LapeMFonts_Path_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^.Path := PString(Params^[1])^;
end;

procedure _LapeMFonts_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFonts(Params^[0])^.Free();
end;

