unit simba.import_class_imagebox;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, controls, extctrls, comctrls, graphics, lptypes, ffi,
  simba.script_compiler, simba.mufasatypes, simba.imagebox, simba.bitmap, simba.dtm, simba.iomanager;

type
  PComponent = ^TComponent;
  PCursor = ^TCursor;
  PNotifyEvent = ^TNotifyEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PMouseEvent = ^TMouseEvent;
  PStatusBar = ^TStatusBar;
  PStatusPanel = ^TStatusPanel;

procedure _LapeSimbaImageBox_Zoom_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSingle(Result)^ := PSimbaImageBox(Params^[0])^.Zoom;
end;

procedure _LapeSimbaImageBox_Zoom_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Zoom := PSingle(Params^[1])^;
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

procedure _LapeSimbaImageBox_DebugColor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.DebugColor(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PExtended(Params^[4])^, PExtended(Params^[5])^);
end;

procedure _LapeSimbaImageBox_DebugDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.DebugDTM(PMDTM(Params^[1])^);
end;

procedure _LapeSimbaImageBox_DebugTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.DebugTPA(PPointArray(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Clear();
end;

procedure _LapeSimbaImageBox_Paint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Paint();
end;

procedure _LapeSimbaImageBox_SetBackground_Data(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.SetBackground(PPRGB32(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeSimbaImageBox_SetBackground_FileName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.SetBackground(PString(Params^[1])^);
end;

procedure _LapeSimbaImageBox_SetBackground_Bitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.SetBackground(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeSimbaImageBox_SetBackground_IOManagerArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.SetBackground(PIOManager(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

procedure _LapeSimbaImageBox_SetBackground_IOManager(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.SetBackground(PIOManager(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^ := TSimbaImageBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeSimbaImageBox_InitNoOverlay(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^ := TSimbaImageBox.CreateNoOverlay(PComponent(Params^[1])^);
end;

procedure _LapeSimbaImageBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Free();
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

procedure ImportSimbaImageBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TSimbaImageBox', 'TCustomControl');
    addGlobalType('procedure(Sender: TObject; ACanvas: TCanvas; R: TRect) of object', 'TSimbaImageBox_PaintArea', FFI_DEFAULT_ABI);
    addClassVar('TSimbaImageBox', 'OnPaintArea', 'TSimbaImageBox_PaintArea', @_LapeSimbaImageBox_OnPaintArea_Read, @_LapeSimbaImageBox_OnPaintArea_Write);
    addClassVar('TSimbaImageBox', 'Zoom', 'Single', @_LapeSimbaImageBox_Zoom_Read, @_LapeSimbaImageBox_Zoom_Write);
    addClassVar('TSimbaImageBox', 'StatusBar', 'TStatusBar', @_LapeSimbaImageBox_StatusBar_Read);
    addClassVar('TSimbaImageBox', 'StatusPanel', 'TStatusPanel', @_LapeSimbaImageBox_StatusPanel_Read);
    addClassVar('TSimbaImageBox', 'OnMouseMove', 'TMouseMoveEvent', @_LapeSimbaImageBox_OnMouseMove_Read, @_LapeSimbaImageBox_OnMouseMove_Write);
    addClassVar('TSimbaImageBox', 'OnMouseDown', 'TMouseEvent', @_LapeSimbaImageBox_OnMouseDown_Read, @_LapeSimbaImageBox_OnMouseDown_Write);
    addClassVar('TSimbaImageBox', 'OnMouseUp', 'TMouseEvent', @_LapeSimbaImageBox_OnMouseUp_Read, @_LapeSimbaImageBox_OnMouseUp_Write);
    addClassVar('TSimbaImageBox', 'OnMouseLeave', 'TNotifyEvent', @_LapeSimbaImageBox_OnMouseLeave_Read, @_LapeSimbaImageBox_OnMouseLeave_Write);
    addClassVar('TSimbaImageBox', 'OnMouseEnter', 'TNotifyEvent', @_LapeSimbaImageBox_OnMouseEnter_Read, @_LapeSimbaImageBox_OnMouseEnter_Write);
    addClassVar('TSimbaImageBox', 'OnDblClick', 'TNotifyEvent', @_LapeSimbaImageBox_OnDblClick_Read, @_LapeSimbaImageBox_OnDblClick_Write);
    //addClassVar('TSimbaImageBox', 'Cursor', 'TCursor', @_LapeSimbaImageBox_Cursor_Read, @_LapeSimbaImageBox_Cursor_Write);
    addGlobalFunc('procedure TSimbaImageBox.MoveTo(X, Y: Int32);', @_LapeSimbaImageBox_MoveTo);
    addGlobalFunc('function TSimbaImageBox.IsVisible(X, Y: Int32): Boolean; overload;', @_LapeSimbaImageBox_IsVisible);

    addGlobalFunc('procedure TSimbaImageBox.DebugColor(CTS: Integer; Col, Tol: Integer; HueMod: Extended = 0.2; SatMod: Extended = 0.2);', @_LapeSimbaImageBox_DebugColor);
    addGlobalFunc('procedure TSimbaImageBox.DebugDTM(DTM: TMDTM);', @_LapeSimbaImageBox_DebugDTM);
    addGlobalFunc('procedure TSimbaImageBox.DebugTPA(TPA: TPointArray);', @_LapeSimbaImageBox_DebugTPA);
    addGlobalFunc('procedure TSimbaImageBox.Clear; overload;', @_LapeSimbaImageBox_Clear);
    addGlobalFunc('procedure TSimbaImageBox.Paint; overload;', @_LapeSimbaImageBox_Paint);

    addGlobalFunc('procedure TSimbaImageBox.SetBackground(Data: PRGB32; AWidth, AHeight: Integer); overload;', @_LapeSimbaImageBox_SetBackground_Data);
    addGlobalFunc('procedure TSimbaImageBox.SetBackground(FileName: String); overload;', @_LapeSimbaImageBox_SetBackground_FileName);
    addGlobalFunc('procedure TSimbaImageBox.SetBackground(Bitmap: TMufasaBitmap); overload;', @_LapeSimbaImageBox_SetBackground_Bitmap);
    addGlobalFunc('procedure TSimbaImageBox.SetBackground(IOManager: TIOManager; X1, Y1, X2, Y2: Integer); overload;', @_LapeSimbaImageBox_SetBackground_IOManagerArea);
    addGlobalFunc('procedure TSimbaImageBox.SetBackground(IOManager: TIOManager); overload;', @_LapeSimbaImageBox_SetBackground_IOManager);

    addGlobalFunc('procedure TSimbaImageBox.Init(Owner: TComponent); override', @_LapeSimbaImageBox_Init);
    addGlobalFunc('procedure TSimbaImageBox.InitNoOverlay(Owner: TComponent); overload; override', @_LapeSimbaImageBox_InitNoOverlay);
    //addGlobalFunc('procedure TSimbaImageBox.Free;', @_LapeSimbaImageBox_Free);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportSimbaImageBox);

end.

