procedure _LapeTarget_GetTargetDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.GetTargetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_GetTargetPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.GetTargetPosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_GetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PTarget(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_CopyData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PTarget(Params^[0])^.CopyData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_ReturnData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRetData(Result)^ := PTarget(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_FreeReturnData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  //PTarget(Params^[0])^.FreeReturnData();
end;

procedure _LapeTarget_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ActivateClient();
end;

procedure _LapeTarget_TargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.TargetValid();
end;

procedure _LapeTarget_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.MouseSetClientArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.MouseResetClientArea();
end;

procedure _LapeTarget_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.ImageSetClientArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ImageResetClientArea();
end;

procedure _LapeTarget_GetMousePosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.GetMousePosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_MoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.MoveMouse(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_ScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ScrollMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeTarget_HoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.HoldMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeTarget_ReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ReleaseMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeTarget_IsMouseButtonHeld(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.IsMouseButtonHeld(PClickType(Params^[1])^);
end;

procedure _LapeTarget_SendString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.SendString(PlpString(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeTarget_HoldKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.HoldKey(PInteger(Params^[1])^);
end;

procedure _LapeTarget_ReleaseKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ReleaseKey(PInteger(Params^[1])^);
end;

procedure _LapeTarget_IsKeyHeld(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.IsKeyHeld(PInteger(Params^[1])^);
end;

procedure _LapeTarget_GetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PTarget(Params^[0])^.GetKeyCode(PChar(Params^[1])^);
end;

procedure _LapeTarget_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^ := TTarget.Create();
end;

procedure _LapeTarget_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.Free();
end;

