unit simba.script_import_simbaimagebox;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_SimbaImageBox(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, comctrls, graphics, extctrls,
  simba.imagebox, simba.dtm, simba.client, simba.bitmap;

type
  PSimbaImageBox = ^TSimbaImageBox;
  PSimbaImageBox_Overlay = ^TSimbaImageBox_Overlay;
  PSimbaImageBox_Background = ^TSimbaImageBox_Background;
  PSimbaImageBox_PaintArea = ^TSimbaImageBox_PaintArea;

  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PStatusBar = ^TStatusBar;
  PStatusPanel = ^TStatusPanel;
  PComponent = ^TComponent;
  PCursor = ^TCursor;
  PNotifyEvent = ^TNotifyEvent;

procedure TSimbaImageBox_Background_LoadFromMufasaBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Background(Params^[0])^.LoadFromMufasaBitmap(PMufasaBitmap(Params^[1])^);
end;

procedure TSimbaImageBox_Background_LoadFromPointer(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Background(Params^[0])^.LoadFromPointer(PPRGB32(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[2])^);
end;

procedure Lape_Import_TSimbaImageBox_Background(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TSimbaImageBox_Background', 'TBitmap');
    addGlobalFunc('procedure TSimbaImageBox_Background.LoadFromMufasaBitmap(Bitmap: TMufasaBitmap); constref;', @TSimbaImageBox_Background_LoadFromMufasaBitmap);
    addGlobalFunc('procedure TSimbaImageBox_Background.LoadFromPointer(Data: PRGB32; AWidth, AHeight: Int32); constref; ', @TSimbaImageBox_Background_LoadFromPointer);
  end;
end;

procedure TSimbaImageBox_Overlay_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.Canvas;
end;

procedure TSimbaImageBox_Overlay_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Overlay(Params^[0])^.Clear();
end;

procedure TSimbaImageBox_Overlay_DebugTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Overlay(Params^[0])^.DebugTPA(PPointArray(Params^[1])^);
end;

procedure TSimbaImageBox_Overlay_DebugColorCTS0(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugColorCTS0(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure TSimbaImageBox_Overlay_DebugColorCTS1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugColorCTS1(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure TSimbaImageBox_Overlay_DebugColorCTS2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugColorCTS2(PInteger(Params^[1])^, PInteger(Params^[2])^, PExtended(Params^[3])^, PExtended(Params^[4])^);
end;

procedure TSimbaImageBox_Overlay_DebugDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaImageBox_Overlay(Params^[0])^.DebugDTM(PMDTM(Params^[1])^);
end;

procedure Lape_Import_TSimbaImageBox_Overlay(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TSimbaImageBox_Overlay', 'TObject');
    addClassVar('TSimbaImageBox_Overlay', 'Canvas', 'TCanvas', @TSimbaImageBox_Overlay_Canvas_Read);
    addGlobalFunc('procedure TSimbaImageBox_Overlay.Clear(); constref;', @TSimbaImageBox_Overlay_Clear);
    addGlobalFunc('procedure TSimbaImageBox_Overlay.DebugTPA(TPA: TPointArray); constref;', @TSimbaImageBox_Overlay_DebugTPA);
    addGlobalFunc('function TSimbaImageBox_Overlay.DebugColorCTS0(Color, Tolerance: Int32): Int32; constref;', @TSimbaImageBox_Overlay_DebugColorCTS0);
    addGlobalFunc('function TSimbaImageBox_Overlay.DebugColorCTS1(Color, Tolerance: Int32): Int32; constref;', @TSimbaImageBox_Overlay_DebugColorCTS1);
    addGlobalFunc('function TSimbaImageBox_Overlay.DebugColorCTS2(Color, Tolerance: Int32; Hue, Sat: Extended): Int32; constref;', @TSimbaImageBox_Overlay_DebugColorCTS2);
    addGlobalFunc('function TSimbaImageBox_Overlay.DebugDTM(DTM: TMDTM): Int32; constref;', @TSimbaImageBox_Overlay_DebugDTM);
  end;
end;

procedure TSimbaImageBox_Zoom_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PSimbaImageBox(Params^[0])^.Zoom;
end;

procedure TSimbaImageBox_Zoom_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Zoom := PDouble(Params^[1])^;
end;

procedure TSimbaImageBox_StatusBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusBar(Result)^ := PSimbaImageBox(Params^[0])^.StatusBar;
end;

procedure TSimbaImageBox_StatusPanel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStatusPanel(Result)^ := PSimbaImageBox(Params^[0])^.StatusPanel;
end;

procedure TSimbaImageBox_OnPaintArea_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_PaintArea(Result)^ := PSimbaImageBox(Params^[0])^.OnPaintArea;
end;

procedure TSimbaImageBox_OnPaintArea_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnPaintArea := PSimbaImageBox_PaintArea(Params^[1])^;
end;

procedure TSimbaImageBox_MoveTo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.MoveTo(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure TSimbaImageBox_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PSimbaImageBox(Params^[0])^.IsVisible(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure TSimbaImageBox_Background_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Background(Result)^ := PSimbaImageBox(Params^[0])^.Background;
end;

procedure TSimbaImageBox_Overlay_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox_Overlay(Result)^ := PSimbaImageBox(Params^[0])^.Overlay;
end;

procedure TSimbaImageBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^ := TSimbaImageBox.Create(PComponent(Params^[1])^);
end;

procedure TSimbaImageBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Free();
end;

procedure TSimbaImageBox_BackgroundChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.BackgroundChanged(PBoolean(Params^[1])^);
end;

procedure TSimbaImageBox_Cursor_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCursor(Result)^ := PSimbaImageBox(Params^[0])^.Cursor;
end;

procedure TSimbaImageBox_Cursor_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

procedure TSimbaImageBox_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseMoveEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseMove;
end;

procedure TSimbaImageBox_OnMouseMove_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure TSimbaImageBox_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseDown;
end;

procedure TSimbaImageBox_OnMouseDown_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure TSimbaImageBox_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMouseEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseUp;
end;

procedure TSimbaImageBox_OnMouseUp_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure TSimbaImageBox_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseEnter;
end;

procedure TSimbaImageBox_OnMouseEnter_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure TSimbaImageBox_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnMouseLeave;
end;

procedure TSimbaImageBox_OnMouseLeave_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure TSimbaImageBox_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PSimbaImageBox(Params^[0])^.OnDblClick;
end;

procedure TSimbaImageBox_OnDblClick_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

procedure Lape_Import_SimbaImageBox(Compiler: TSimbaScript_Compiler);
begin
  Lape_Import_TSimbaImageBox_Background(Compiler);
  Lape_Import_TSimbaImageBox_Overlay(Compiler);

  with Compiler do
  begin
    addClass('TSimbaImageBox', 'TCustomControl');

    addGlobalType('procedure(Sender: TObject; ACanvas: TCanvas; R: TRect) of object', 'TSimbaImageBox_PaintArea', FFI_DEFAULT_ABI);

    addClassVar('TSimbaImageBox', 'OnPaintArea', 'TSimbaImageBox_PaintArea', @TSimbaImageBox_OnPaintArea_Read, @TSimbaImageBox_OnPaintArea_Write);
    addClassVar('TSimbaImageBox', 'Zoom', 'Double', @TSimbaImageBox_Zoom_Read, @TSimbaImageBox_Zoom_Write);
    addClassVar('TSimbaImageBox', 'StatusBar', 'TStatusBar', @TSimbaImageBox_StatusBar_Read);
    addClassVar('TSimbaImageBox', 'StatusPanel', 'TStatusPanel', @TSimbaImageBox_StatusPanel_Read);
    addClassVar('TSimbaImageBox', 'Background', 'TSimbaImageBox_Background', @TSimbaImageBox_Background_Read);
    addClassVar('TSimbaImageBox', 'Overlay', 'TSimbaImageBox_Overlay', @TSimbaImageBox_Overlay_Read);
    addClassVar('TSimbaImageBox', 'OnMouseMove', 'TMouseMoveEvent', @TSimbaImageBox_OnMouseMove_Read, @TSimbaImageBox_OnMouseMove_Write);
    addClassVar('TSimbaImageBox', 'OnMouseDown', 'TMouseEvent', @TSimbaImageBox_OnMouseDown_Read, @TSimbaImageBox_OnMouseDown_Write);
    addClassVar('TSimbaImageBox', 'OnMouseUp', 'TMouseEvent', @TSimbaImageBox_OnMouseUp_Read, @TSimbaImageBox_OnMouseUp_Write);
    addClassVar('TSimbaImageBox', 'OnMouseLeave', 'TNotifyEvent', @TSimbaImageBox_OnMouseLeave_Read, @TSimbaImageBox_OnMouseLeave_Write);
    addClassVar('TSimbaImageBox', 'OnMouseEnter', 'TNotifyEvent', @TSimbaImageBox_OnMouseEnter_Read, @TSimbaImageBox_OnMouseEnter_Write);
    addClassVar('TSimbaImageBox', 'OnDblClick', 'TNotifyEvent', @TSimbaImageBox_OnDblClick_Read, @TSimbaImageBox_OnDblClick_Write);
    //addClassVar('TSimbaImageBox', 'Cursor', 'TCursor', @TSimbaImageBox_Cursor_Read, @TSimbaImageBox_Cursor_Write);

    addGlobalFunc('procedure TSimbaImageBox.BackgroundChanged(UpdateOverlay: Boolean = True); constref;', @TSimbaImageBox_BackgroundChanged);
    addGlobalFunc('procedure TSimbaImageBox.MoveTo(X, Y: Int32); constref;', @TSimbaImageBox_MoveTo);
    addGlobalFunc('function TSimbaImageBox.IsVisible(X, Y: Int32): Boolean; overload; constref;', @TSimbaImageBox_IsVisible);
    addGlobalFunc('procedure TSimbaImageBox.Init(Owner: TComponent); override;', @TSimbaImageBox_Init);
    //addGlobalFunc('procedure TSimbaImageBox.Free(); constref;', @TSimbaImageBox_Free);
  end;
end;

end.

