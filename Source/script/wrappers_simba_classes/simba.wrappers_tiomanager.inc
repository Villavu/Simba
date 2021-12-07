procedure _LapeIOManager_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^ := TIOManager.Create();
end;

procedure _LapeIOManager_InitEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^ := TIOManager.Create(PlpString(Params^[1])^);
end;

procedure _LapeIOManager_SetTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PPRGB32(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapeIOManager_SetTargetEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeIOManager_SetTargetExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure _LapeIOManager_TargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.TargetValid();
end;

procedure _LapeIOManager_GetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PIOManager(Params^[0])^.GetColor(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeIOManager_ReturnMatrix(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerMatrix(Result)^ := PIOManager(Params^[0])^.ReturnMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_ReturnData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRetData(Result)^ := PIOManager(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_CopyData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PIOManager(Params^[0])^.CopyData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_FreeReturnData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.FreeReturnData();
end;

procedure _LapeIOManager_GetDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_GetPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetPosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ActivateClient();
end;

procedure _LapeIOManager_IsFrozen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.IsFrozen();
end;

procedure _LapeIOManager_SetFrozen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetFrozen(Pboolean(Params^[1])^);
end;

procedure _LapeIOManager_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.MouseSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeIOManager_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.MouseResetClientArea();
end;

procedure _LapeIOManager_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.ImageSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeIOManager_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ImageResetClientArea();
end;

procedure _LapeIOManager_GetMousePos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetMousePos(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_MoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.MoveMouse(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_ScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ScrollMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure _LapeIOManager_HoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.HoldMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeIOManager_ReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ReleaseMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeIOManager_ClickMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ClickMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeIOManager_IsMouseButtonDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.IsMouseButtonDown(PClickType(Params^[1])^);
end;

procedure _LapeIOManager_KeyUp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.KeyUp(PWord(Params^[1])^);
end;

procedure _LapeIOManager_KeyDown(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.KeyDown(PWord(Params^[1])^);
end;

procedure _LapeIOManager_PressKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.PressKey(PWord(Params^[1])^);
end;

procedure _LapeIOManager_SendText(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SendText(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

procedure _LapeIOManager_isKeyDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.isKeyDown(PWord(Params^[1])^);
end;

procedure _LapeIOManager_GetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.GetKeyCode(Pchar(Params^[1])^);
end;

procedure _LapeIOManager_GetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.GetImageTarget();
end;

procedure _LapeIOManager_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.GetKeyMouseTarget();
end;

procedure _LapeIOManager_ExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := PIOManager(Params^[0])^.ExportImageTarget();
end;

procedure _LapeIOManager_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := PIOManager(Params^[0])^.ExportKeyMouseTarget();
end;

procedure _LapeIOManager_GetImageTargetEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetImageTarget(Pinteger(Params^[1])^);
end;

procedure _LapeIOManager_GetKeyMouseTargetEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetKeyMouseTarget(Pinteger(Params^[1])^);
end;

procedure _LapeIOManager_SetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetImageTarget(Pinteger(Params^[1])^);
end;

procedure _LapeIOManager_SetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetKeyMouseTarget(Pinteger(Params^[1])^);
end;

procedure _LapeIOManager_FreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.FreeTarget(Pinteger(Params^[1])^);
end;

procedure _LapeIOManager_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.Free();
end;

procedure _LapeIOManager_SetTargetHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PPtrUInt(Params^[1])^);
end;

procedure _LapeIOManager_SetImageTargetEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.SetImageTargetEx(PWindowHandle(Params^[1])^);
end;

procedure _LapeIOManager_SetKeyMouseTargetEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.SetKeyMouseTargetEx(PWindowHandle(Params^[1])^);
end;

