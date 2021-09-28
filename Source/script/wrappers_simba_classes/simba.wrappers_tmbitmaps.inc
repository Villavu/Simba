procedure _LapeMBitmaps_GetBMP(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMufasaBitmap(Result)^ := PMBitmaps(Params^[0])^.GetBMP(Pinteger(Params^[1])^);
end;

procedure _LapeMBitmaps_CreateBMP(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMP(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeMBitmaps_ExistsBMP(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMBitmaps(Params^[0])^.ExistsBMP(Pinteger(Params^[1])^);
end;

procedure _LapeMBitmaps_AddBMP(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.AddBMP(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeMBitmaps_CopyBMP(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.CopyBMP(Pinteger(Params^[1])^);
end;

procedure _LapeMBitmaps_CreateMirroredBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMBitmaps(Params^[0])^.CreateMirroredBitmap(PInteger(Params^[1])^, PBmpMirrorStyle(Params^[2])^);
end;

procedure _LapeMBitmaps_CreateBMPFromFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMPFromFile(PlpString(Params^[1])^);
end;

procedure _LapeMBitmaps_CreateBMPFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMPFromString(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PlpString(Params^[3])^);
end;

procedure _LapeMBitmaps_CreateBMPFromStringEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMBitmaps(Params^[0])^.CreateBMPFromString(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, PlpString(Params^[4])^);
end;

procedure _LapeMBitmaps_RemoveBMP(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMBitmaps(Params^[0])^.RemoveBMP(Pinteger(Params^[1])^);
end;

procedure _LapeMBitmaps_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMBitmaps(Params^[0])^ := TMBitmaps.Create(PObject(Params^[1])^);
end;

procedure _LapeMBitmaps_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMBitmaps(Params^[0])^.Free();
end;

