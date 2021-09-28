procedure _LapeSimbaImageBox_Background_LoadFromMufasaBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Background(Params^[0])^.LoadFromMufasaBitmap(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Background_LoadFromPointer(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Background(Params^[0])^.LoadFromPointer(PPRGB32(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaImageBox_Overlay_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.Canvas;
end;

procedure _LapeSimbaImageBox_Overlay_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Overlay(Params^[0])^.Clear();
end;

procedure _LapeSimbaImageBox_Overlay_DebugTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Overlay(Params^[0])^.DebugTPA(PPointArray(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Overlay_DebugColorCTS0(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugColorCTS0(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaImageBox_Overlay_DebugColorCTS1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugColorCTS1(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaImageBox_Overlay_DebugColorCTS2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugColorCTS2(PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure _LapeSimbaImageBox_Overlay_DebugDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugDTM(PMDTM(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Zoom_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PSimbaImageBox(Params^[0])^.Zoom;
end;

procedure _LapeSimbaImageBox_Zoom_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Zoom := PDouble(Params^[1])^;
end;

procedure _LapeSimbaImageBox_StatusBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Result)^ := PSimbaImageBox(Params^[0])^.StatusBar;
end;

procedure _LapeSimbaImageBox_StatusPanel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Result)^ := PSimbaImageBox(Params^[0])^.StatusPanel;
end;

procedure _LapeSimbaImageBox_OnPaintArea_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_PaintArea(Result)^ := PSimbaImageBox(Params^[0])^.OnPaintArea;
end;

procedure _LapeSimbaImageBox_OnPaintArea_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnPaintArea := PSimbaImageBox_PaintArea(Params^[1])^;
end;

procedure _LapeSimbaImageBox_MoveTo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.MoveTo(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeSimbaImageBox_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.IsVisible(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeSimbaImageBox_Background_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Background(Result)^ := PSimbaImageBox(Params^[0])^.Background;
end;

procedure _LapeSimbaImageBox_Overlay_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Overlay(Result)^ := PSimbaImageBox(Params^[0])^.Overlay;
end;

procedure _LapeSimbaImageBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^ := TSimbaImageBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Free();
end;

procedure _LapeSimbaImageBox_BackgroundChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.BackgroundChanged(PBoolean(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Cursor_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCursor(Result)^ := PSimbaImageBox(Params^[0])^.Cursor;
end;

procedure _LapeSimbaImageBox_Cursor_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseMoveEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseMove;
end;

procedure _LapeSimbaImageBox_OnMouseMove_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseDown;
end;

procedure _LapeSimbaImageBox_OnMouseDown_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseUp;
end;

procedure _LapeSimbaImageBox_OnMouseUp_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseEnter;
end;

procedure _LapeSimbaImageBox_OnMouseEnter_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseLeave;
end;

procedure _LapeSimbaImageBox_OnMouseLeave_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSimbaImageBox_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnDblClick;
end;

procedure _LapeSimbaImageBox_OnDblClick_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

