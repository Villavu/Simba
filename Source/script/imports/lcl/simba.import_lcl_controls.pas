unit simba.import_lcl_controls;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, controls, extctrls, comctrls, graphics, forms, lptypes, ffi,
  simba.script_compiler;

type
  PAlign = ^TAlign;
  PControl = ^TControl;
  PControlScrollBar = ^TControlScrollBar;
  PCursor = ^TCursor;
  PCustomControl = ^TCustomControl;
  PGraphicControl = ^TGraphicControl;
  PKeyEvent = ^TKeyEvent;
  PKeyPressEvent = ^TKeyPressEvent;
  PScrollBarKind = ^TScrollBarKind;
  PScrollingWinControl = ^TScrollingWinControl;
  PWinControl = ^TWinControl;
  PRect = ^TRect;
  PPoint = ^TPoint;
  PComponent = ^TComponent;
  PNotifyEvent = ^TNotifyEvent;
  PHandle = ^THandle;
  PBrush = ^TBrush;
  PCanvas = ^TCanvas;
  PBitmap = ^TBitmap;
  PFont = ^TFont;
  PPersistent = ^TPersistent;

procedure _LapeControl_AdjustSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.AdjustSize();
end;

procedure _LapeControl_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_SetInitialBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetInitialBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_SetBoundsKeepBase(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SetBoundsKeepBase(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_GetPreferredSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.GetPreferredSize(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

procedure _LapeControl_GetDefaultWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultWidth();
end;

procedure _LapeControl_GetDefaultHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControl(Params^[0])^.GetDefaultHeight();
end;

procedure _LapeControl_BaseBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BaseBounds;
end;

procedure _LapeControl_ReadBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.ReadBounds;
end;

procedure _LapeControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^ := TControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeControl_EditingDone(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.EditingDone();
end;

procedure _LapeControl_ExecuteDefaultAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ExecuteDefaultAction();
end;

procedure _LapeControl_ExecuteCancelAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ExecuteCancelAction();
end;

procedure _LapeControl_BringToFront(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BringToFront();
end;

procedure _LapeControl_HasParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.HasParent();
end;

procedure _LapeControl_GetParentComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PControl(Params^[0])^.GetParentComponent();
end;

procedure _LapeControl_IsParentOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsParentOf(PControl(Params^[1])^);
end;

procedure _LapeControl_GetTopParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PControl(Params^[0])^.GetTopParent();
end;

procedure _LapeControl_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsVisible();
end;

procedure _LapeControl_IsControlVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControlVisible();
end;

procedure _LapeControl_IsEnabled(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsEnabled();
end;

procedure _LapeControl_IsParentColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentColor();
end;

procedure _LapeControl_IsParentFont(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsParentFont();
end;

procedure _LapeControl_FormIsUpdating(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.FormIsUpdating();
end;

procedure _LapeControl_IsProcessingPaintMsg(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PControl(Params^[0])^.IsProcessingPaintMsg();
end;

procedure _LapeControl_Hide(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Hide();
end;

procedure _LapeControl_Refresh(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Refresh();
end;

procedure _LapeControl_Repaint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Repaint();
end;

procedure _LapeControl_Invalidate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Invalidate();
end;

procedure _LapeControl_SendToBack(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.SendToBack();
end;

procedure _LapeControl_ScreenToClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToClient(PPoint(Params^[1])^);
end;

procedure _LapeControl_ClientToScreen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientToScreen(PPoint(Params^[1])^);
end;

procedure _LapeControl_ScreenToControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToControl(PPoint(Params^[1])^);
end;

procedure _LapeControl_ControlToScreen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlToScreen(PPoint(Params^[1])^);
end;

procedure _LapeControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.GetChildrenRect(Pboolean(Params^[1])^);
end;

procedure _LapeControl_Show(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Show();
end;

procedure _LapeControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Update();
end;

procedure _LapeControl_AutoSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSize;
end;

procedure _LapeControl_AutoSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.AutoSize := PBoolean(Params^[1])^;
end;

procedure _LapeControl_BoundsRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRect;
end;

procedure _LapeControl_BoundsRect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BoundsRect := PRect(Params^[1])^;
end;

procedure _LapeControl_BoundsRectForNewParent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRectForNewParent;
end;

procedure _LapeControl_BoundsRectForNewParent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.BoundsRectForNewParent := PRect(Params^[1])^;
end;

procedure _LapeControl_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PControl(Params^[0])^.Caption;
end;

procedure _LapeControl_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Caption := PString(Params^[1])^;
end;

procedure _LapeControl_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientHeight;
end;

procedure _LapeControl_ClientHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

procedure _LapeControl_ClientOrigin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientOrigin;
end;

procedure _LapeControl_ClientRect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PControl(Params^[0])^.ClientRect;
end;

procedure _LapeControl_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientWidth;
end;

procedure _LapeControl_ClientWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

procedure _LapeControl_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Color;
end;

procedure _LapeControl_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapeControl_ControlOrigin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlOrigin;
end;

procedure _LapeControl_Enabled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Enabled;
end;

procedure _LapeControl_Enabled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Font_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFont(Result)^ := PControl(Params^[0])^.Font;
end;

procedure _LapeControl_Font_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Font := PFont(Params^[1])^;
end;

procedure _LapeControl_IsControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsControl;
end;

procedure _LapeControl_IsControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.IsControl := PBoolean(Params^[1])^;
end;

procedure _LapeControl_MouseEntered_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.MouseInClient;
end;

procedure _LapeControl_OnChangeBounds_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnChangeBounds;
end;

procedure _LapeControl_OnChangeBounds_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnChangeBounds := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnClick;
end;

procedure _LapeControl_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_OnResize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnResize;
end;

procedure _LapeControl_OnResize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_Align_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PAlign(Result)^ := PControl(Params^[0])^.Align;
end;

procedure _LapeControl_Align_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

procedure _LapeControl_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Visible;
end;

procedure _LapeControl_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeControl_IsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.IsRightToLeft();
end;

procedure _LapeControl_Left_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Left;
end;

procedure _LapeControl_Left_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Left := PInteger(Params^[1])^;
end;

procedure _LapeControl_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Height;
end;

procedure _LapeControl_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeControl_Top_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Top;
end;

procedure _LapeControl_Top_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Top := PInteger(Params^[1])^;
end;

procedure _LapeControl_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControl(Params^[0])^.Width;
end;

procedure _LapeControl_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeControl_ShowHint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ShowHint := True;
end;

procedure _LapeControl_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PControl(Params^[0])^.Parent;
end;

procedure _LapeControl_Parent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Parent := PWinControl(Params^[1])^;
end;

procedure _LapeControl_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PControl(Params^[0])^.Hint;
end;

procedure _LapeControl_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Hint := PString(Params^[1])^;
end;

procedure _LapeControl_ShowHint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShowHint;
end;

procedure _LapeControl_ShowHint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Cursor_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCursor(Result)^ := PControl(Params^[0])^.Cursor;
end;

procedure _LapeControl_Cursor_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

procedure _LapeControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Params^[0])^.Free();
end;

procedure _LapeWinControl_BorderWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BorderWidth;
end;

procedure _LapeWinControl_BorderWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.BorderWidth := Pinteger(Params^[1])^;
end;

procedure _LapeWinControl_BoundsLockCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.BoundsLockCount;
end;

procedure _LapeWinControl_Brush_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBrush(Result)^ := PWinControl(Params^[0])^.Brush;
end;

procedure _LapeWinControl_CachedClientHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientHeight;
end;

procedure _LapeWinControl_CachedClientWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.CachedClientWidth;
end;

procedure _LapeWinControl_ControlCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.ControlCount;
end;

procedure _LapeWinControl_DoubleBuffered_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.DoubleBuffered;
end;

procedure _LapeWinControl_DoubleBuffered_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DoubleBuffered := PBoolean(Params^[1])^;
end;

procedure _LapeWinControl_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.Handle;
end;

procedure _LapeWinControl_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapeWinControl_IsResizing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.IsResizing;
end;

procedure _LapeWinControl_TabOrder_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.TabOrder;
end;

procedure _LapeWinControl_TabOrder_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.TabOrder := PInteger(Params^[1])^;
end;

procedure _LapeWinControl_TabStop_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.TabStop;
end;

procedure _LapeWinControl_TabStop_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.TabStop := PBoolean(Params^[1])^;
end;

procedure _LapeWinControl_OnEnter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnEnter;
end;

procedure _LapeWinControl_OnEnter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnExit_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnExit;
end;

procedure _LapeWinControl_OnExit_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyPress_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

procedure _LapeWinControl_ParentWindow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.ParentWindow;
end;

procedure _LapeWinControl_ParentWindow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ParentWindow := PHandle(Params^[1])^;
end;

procedure _LapeWinControl_Showing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Showing;
end;

procedure _LapeWinControl_AutoSizeDelayed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayed();
end;

procedure _LapeWinControl_AutoSizeDelayedReport(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedReport();
end;

procedure _LapeWinControl_AutoSizeDelayedHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.AutoSizeDelayedHandle();
end;

procedure _LapeWinControl_BeginUpdateBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.BeginUpdateBounds();
end;

procedure _LapeWinControl_EndUpdateBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.EndUpdateBounds();
end;

procedure _LapeWinControl_LockRealizeBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.LockRealizeBounds();
end;

procedure _LapeWinControl_UnlockRealizeBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.UnlockRealizeBounds();
end;

procedure _LapeWinControl_ControlAtPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeWinControl_ControlAtPosEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.ControlAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeWinControl_ContainsControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.ContainsControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeWinControl_GetChildsRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PWinControl(Params^[0])^.GetChildrenRect(Pboolean(Params^[1])^);
end;

procedure _LapeWinControl_DisableAlign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.DisableAlign();
end;

procedure _LapeWinControl_EnableAlign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.EnableAlign();
end;

procedure _LapeWinControl_ReAlign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ReAlign();
end;

procedure _LapeWinControl_ScrollBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeWinControl_WriteLayoutDebugReport(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.WriteLayoutDebugReport(PString(Params^[1])^);
end;

procedure _LapeWinControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^ := TWinControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeWinControl_CreateParented(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^ := TWinControl.CreateParented(Phandle(Params^[1])^);
end;

procedure _LapeWinControl_CanFocus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.CanFocus();
end;

procedure _LapeWinControl_GetControlIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PWinControl(Params^[0])^.GetControlIndex(PControl(Params^[1])^);
end;

procedure _LapeWinControl_SetControlIndex(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetControlIndex(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeWinControl_Focused(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Focused();
end;

procedure _LapeWinControl_PerformTab(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.PerformTab(Pboolean(Params^[1])^);
end;

procedure _LapeWinControl_FindChildControl(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControl(Result)^ := PWinControl(Params^[0])^.FindChildControl(PString(Params^[1])^);
end;

procedure _LapeWinControl_SelectNext(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SelectNext(PWinControl(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
end;

procedure _LapeWinControl_GetTextLen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.GetTextLen();
end;

procedure _LapeWinControl_Invalidate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Invalidate();
end;

procedure _LapeWinControl_AddControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.AddControl();
end;

procedure _LapeWinControl_InsertControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_InsertControlEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeWinControl_RemoveControl(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.RemoveControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_Repaint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Repaint();
end;

procedure _LapeWinControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Update();
end;

procedure _LapeWinControl_SetFocus(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetFocus();
end;

procedure _LapeWinControl_FlipChildren(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.FlipChildren(PBoolean(Params^[1])^);
end;

procedure _LapeWinControl_ScaleBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.ScaleBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeWinControl_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.HandleAllocated();
end;

procedure _LapeWinControl_ParentHandlesAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PWinControl(Params^[0])^.ParentHandlesAllocated();
end;

procedure _LapeWinControl_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.HandleNeeded();
end;

procedure _LapeWinControl_BrushCreated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.BrushCreated();
end;

procedure _LapeWinControl_PaintTo(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.PaintTo(PCanvas(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeWinControl_SetShape(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.SetShape(PBitmap(Params^[1])^);
end;

procedure _LapeWinControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWinControl(Params^[0])^.Free();
end;

procedure _LapeCustomControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^ := TCustomControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomControl(Params^[0])^.Canvas;
end;

procedure _LapeCustomControl_Canvas_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

procedure _LapeCustomControl_OnPaint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomControl(Params^[0])^.OnPaint;
end;

procedure _LapeCustomControl_OnPaint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomControl(Params^[0])^.Free();
end;

procedure _LapeControlScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^ := TControlScrollBar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

procedure _LapeControlScrollBar_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeControlScrollBar_IsScrollBarVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.IsScrollBarVisible();
end;

procedure _LapeControlScrollBar_ScrollPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.ScrollPos();
end;

procedure _LapeControlScrollBar_GetOtherScrollBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PControlScrollBar(Params^[0])^.GetOtherScrollBar();
end;

procedure _LapeControlScrollBar_Size_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.Size;
end;

procedure _LapeControlScrollBar_ClientSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSize();
end;

procedure _LapeControlScrollBar_ClientSizeWithBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithBar();
end;

procedure _LapeControlScrollBar_ClientSizeWithoutBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithoutBar();
end;

procedure _LapeControlScrollBar_Increment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Increment;
end;

procedure _LapeControlScrollBar_Increment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Increment := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Page_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Page;
end;

procedure _LapeControlScrollBar_Page_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Page := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Smooth;
end;

procedure _LapeControlScrollBar_Smooth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Smooth := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Position;
end;

procedure _LapeControlScrollBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Range_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Range;
end;

procedure _LapeControlScrollBar_Range_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Range := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Tracking_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Tracking;
end;

procedure _LapeControlScrollBar_Tracking_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Tracking := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Visible_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Visible;
end;

procedure _LapeControlScrollBar_Visible_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Params^[0])^.Free();
end;

procedure _LapeScrollingWinControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^ := TScrollingWinControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeScrollingWinControl_UpdateScrollbars(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.UpdateScrollbars();
end;

procedure _LapeScrollingWinControl_ScrollBy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeScrollingWinControl_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.HorzScrollBar;
end;

procedure _LapeScrollingWinControl_HorzScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.HorzScrollBar := PControlScrollBar(Params^[1])^;
end;

procedure _LapeScrollingWinControl_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.VertScrollBar;
end;

procedure _LapeScrollingWinControl_VertScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.VertScrollBar := PControlScrollBar(Params^[1])^;
end;

procedure _LapeScrollingWinControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollingWinControl(Params^[0])^.Free();
end;

procedure _LapeGraphicControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PGraphicControl(Params^[0])^.Canvas;
end;

procedure _LapeGraphicControl_Update(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGraphicControl(Params^[0])^.Update();
end;

procedure ImportLCLControls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2)', 'TShiftStateEnum');
    addGlobalType('set of TShiftStateEnum', 'TShiftState');
    addGlobalType('procedure(Sender: TObject; var Key: Int16; Shift: TShiftState) of object', 'TKeyEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var Key: Char) of object', 'TKeyPressEvent', FFI_DEFAULT_ABI);
    addGlobalType('(mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2)', 'TMouseButton');
    addGlobalType('procedure(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer) of object', 'TMouseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object', 'TMouseMoveEvent', FFI_DEFAULT_ABI);
    addGlobalType('(sbHorizontal, sbVertical)', 'TScrollBarKind');
    addGlobalType('(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom)', 'TAlign');
    addGlobalType('Integer', 'TCursor');
    addGlobalVar(crDefault, 'crDefault').isConstant := True;
    addGlobalVar(crNone, 'crNone').isConstant := True;
    addGlobalVar(crSize, 'crSize').isConstant := True;
    addGlobalVar(crCross, 'crCross').isConstant := True;
    addGlobalVar(crHandPoint, 'crHandPoint').isConstant := True;
    addGlobalVar(crIBeam, 'crIBeam').isConstant := True;

    addClass('TControl', 'TComponent');
    addClass('TWinControl', 'TControl');
    addGlobalFunc('procedure TControl.AdjustSize;', @_LapeControl_AdjustSize);
    addGlobalFunc('procedure TControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer);', @_LapeControl_SetBounds);
    addGlobalFunc('procedure TControl.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);', @_LapeControl_SetInitialBounds);
    addGlobalFunc('procedure TControl.SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer);', @_LapeControl_SetBoundsKeepBase);
    addGlobalFunc('procedure TControl.GetPreferredSize(var PreferredWidth, PreferredHeight: integer;Raw: boolean;WithThemeSpace: boolean);', @_LapeControl_GetPreferredSize);
    addGlobalFunc('function TControl.GetDefaultWidth: integer;', @_LapeControl_GetDefaultWidth);
    addGlobalFunc('function TControl.GetDefaultHeight: integer;', @_LapeControl_GetDefaultHeight);
    addClassVar('TControl', 'BaseBounds', 'TRect', @_LapeControl_BaseBounds_Read);
    addClassVar('TControl', 'ReadBounds', 'TRect', @_LapeControl_ReadBounds_Read);
    addGlobalFunc('procedure TControl.Init(TheOwner: TComponent); override', @_LapeControl_Init);
    addGlobalFunc('procedure TControl.EditingDone;', @_LapeControl_EditingDone);
    addGlobalFunc('procedure TControl.ExecuteDefaultAction;', @_LapeControl_ExecuteDefaultAction);
    addGlobalFunc('procedure TControl.ExecuteCancelAction;', @_LapeControl_ExecuteCancelAction);
    addGlobalFunc('procedure TControl.BringToFront;', @_LapeControl_BringToFront);
    addGlobalFunc('function TControl.HasParent: Boolean;', @_LapeControl_HasParent);
    addGlobalFunc('function TControl.GetParentComponent: TComponent;', @_LapeControl_GetParentComponent);
    addGlobalFunc('function TControl.IsParentOf(AControl: TControl): boolean;', @_LapeControl_IsParentOf);
    addGlobalFunc('function TControl.GetTopParent: TControl;', @_LapeControl_GetTopParent);
    addGlobalFunc('function TControl.IsVisible: Boolean;', @_LapeControl_IsVisible);
    addGlobalFunc('function TControl.IsControlVisible: Boolean;', @_LapeControl_IsControlVisible);
    addGlobalFunc('function TControl.IsEnabled: Boolean;', @_LapeControl_IsEnabled);
    addGlobalFunc('function TControl.IsParentColor: Boolean;', @_LapeControl_IsParentColor);
    addGlobalFunc('function TControl.IsParentFont: Boolean;', @_LapeControl_IsParentFont);
    addGlobalFunc('function TControl.FormIsUpdating: boolean;', @_LapeControl_FormIsUpdating);
    addGlobalFunc('function TControl.IsProcessingPaintMsg: boolean;', @_LapeControl_IsProcessingPaintMsg);
    addGlobalFunc('procedure TControl.Hide;', @_LapeControl_Hide);
    addGlobalFunc('procedure TControl.Refresh;', @_LapeControl_Refresh);
    addGlobalFunc('procedure TControl.Repaint;', @_LapeControl_Repaint);
    addGlobalFunc('procedure TControl.Invalidate;', @_LapeControl_Invalidate);
    addGlobalFunc('procedure TControl.SendToBack;', @_LapeControl_SendToBack);
    addGlobalFunc('function TControl.ScreenToClient(const APoint: TPoint): TPoint;', @_LapeControl_ScreenToClient);
    addGlobalFunc('function TControl.ClientToScreen(const APoint: TPoint): TPoint;', @_LapeControl_ClientToScreen);
    addGlobalFunc('function TControl.ScreenToControl(const APoint: TPoint): TPoint;', @_LapeControl_ScreenToControl);
    addGlobalFunc('function TControl.ControlToScreen(const APoint: TPoint): TPoint;', @_LapeControl_ControlToScreen);
    addGlobalFunc('function TControl.GetChildsRect(Scrolled: boolean): TRect;', @_LapeControl_GetChildsRect);
    addGlobalFunc('procedure TControl.ShowHint;', @_LapeControl_ShowHint);
    addGlobalFunc('function TControl.IsRightToLeft: Boolean;', @_LapeControl_IsRightToLeft);
    addGlobalFunc('procedure TControl.Show;', @_LapeControl_Show);
    addGlobalFunc('procedure TControl.Update;', @_LapeControl_Update);
    addClassVar('TControl', 'Cursor', 'TCursor', @_LapeControl_Cursor_Read, @_LapeControl_Cursor_Write);
    addClassVar('TControl', 'Align', 'TAlign', @_LapeControl_Align_Read, @_LapeControl_Align_Write);
    addClassVar('TControl', 'AutoSize', 'Boolean', @_LapeControl_AutoSize_Read, @_LapeControl_AutoSize_Write);
    addClassVar('TControl', 'BoundsRect', 'TRect', @_LapeControl_BoundsRect_Read, @_LapeControl_BoundsRect_Write);
    addClassVar('TControl', 'BoundsRectForNewParent', 'TRect', @_LapeControl_BoundsRectForNewParent_Read, @_LapeControl_BoundsRectForNewParent_Write);
    addClassVar('TControl', 'Caption', 'String', @_LapeControl_Caption_Read, @_LapeControl_Caption_Write);
    addClassVar('TControl', 'ClientHeight', 'Integer', @_LapeControl_ClientHeight_Read, @_LapeControl_ClientHeight_Write);
    addClassVar('TControl', 'ClientOrigin', 'TPoint', @_LapeControl_ClientOrigin_Read);
    addClassVar('TControl', 'ClientRect', 'TRect', @_LapeControl_ClientRect_Read);
    addClassVar('TControl', 'ClientWidth', 'Integer', @_LapeControl_ClientWidth_Read, @_LapeControl_ClientWidth_Write);
    addClassVar('TControl', 'Color', 'Integer', @_LapeControl_Color_Read, @_LapeControl_Color_Write);
    addClassVar('TControl', 'ControlOrigin', 'TPoint', @_LapeControl_ControlOrigin_Read);
    addClassVar('TControl', 'Enabled', 'Boolean', @_LapeControl_Enabled_Read, @_LapeControl_Enabled_Write);
    addClassVar('TControl', 'Font', 'TFont', @_LapeControl_Font_Read, @_LapeControl_Font_Write);
    addClassVar('TControl', 'IsControl', 'Boolean', @_LapeControl_IsControl_Read, @_LapeControl_IsControl_Write);
    addClassVar('TControl', 'MouseEntered', 'Boolean', @_LapeControl_MouseEntered_Read);
    addClassVar('TControl', 'OnChangeBounds', 'TNotifyEvent', @_LapeControl_OnChangeBounds_Read, @_LapeControl_OnChangeBounds_Write);
    addClassVar('TControl', 'OnClick', 'TNotifyEvent', @_LapeControl_OnClick_Read, @_LapeControl_OnClick_Write);
    addClassVar('TControl', 'OnResize', 'TNotifyEvent', @_LapeControl_OnResize_Read, @_LapeControl_OnResize_Write);
    addClassVar('TControl', 'Visible', 'Boolean', @_LapeControl_Visible_Read, @_LapeControl_Visible_Write);
    addClassVar('TControl', 'ShowHint', 'Boolean', @_LapeControl_ShowHint_Read, @_LapeControl_ShowHint_Write);
    addClassVar('TControl', 'Left', 'Integer', @_LapeControl_Left_Read, @_LapeControl_Left_Write);
    addClassVar('TControl', 'Height', 'Integer', @_LapeControl_Height_Read, @_LapeControl_Height_Write);
    addClassVar('TControl', 'Top', 'Integer', @_LapeControl_Top_Read, @_LapeControl_Top_Write);
    addClassVar('TControl', 'Width', 'Integer', @_LapeControl_Width_Read, @_LapeControl_Width_Write);
    addClassVar('TControl', 'Hint', 'String', @_LapeControl_Hint_Read, @_LapeControl_Hint_Write);
    addClassVar('TControl', 'Parent', 'TWinControl', @_LapeControl_Parent_Read, @_LapeControl_Parent_Write);

    addClassVar('TWinControl', 'BorderWidth', 'integer', @_LapeWinControl_BorderWidth_Read, @_LapeWinControl_BorderWidth_Write);
    addClassVar('TWinControl', 'BoundsLockCount', 'integer', @_LapeWinControl_BoundsLockCount_Read);
    addClassVar('TWinControl', 'Brush', 'TBrush', @_LapeWinControl_Brush_Read);
    addClassVar('TWinControl', 'CachedClientHeight', 'integer', @_LapeWinControl_CachedClientHeight_Read);
    addClassVar('TWinControl', 'CachedClientWidth', 'integer', @_LapeWinControl_CachedClientWidth_Read);
    addClassVar('TWinControl', 'ControlCount', 'Integer', @_LapeWinControl_ControlCount_Read);
    addClassVar('TWinControl', 'DoubleBuffered', 'Boolean', @_LapeWinControl_DoubleBuffered_Read, @_LapeWinControl_DoubleBuffered_Write);
    addClassVar('TWinControl', 'Handle', 'THandle', @_LapeWinControl_Handle_Read, @_LapeWinControl_Handle_Write);
    addClassVar('TWinControl', 'IsResizing', 'Boolean', @_LapeWinControl_IsResizing_Read);
    addClassVar('TWinControl', 'TabOrder', 'Integer', @_LapeWinControl_TabOrder_Read, @_LapeWinControl_TabOrder_Write);
    addClassVar('TWinControl', 'TabStop', 'Boolean', @_LapeWinControl_TabStop_Read, @_LapeWinControl_TabStop_Write);
    addClassVar('TWinControl', 'OnEnter', 'TNotifyEvent', @_LapeWinControl_OnEnter_Read, @_LapeWinControl_OnEnter_Write);
    addClassVar('TWinControl', 'OnExit', 'TNotifyEvent', @_LapeWinControl_OnExit_Read, @_LapeWinControl_OnExit_Write);
    addClassVar('TWinControl', 'OnKeyDown', 'TKeyEvent', nil, @_LapeWinControl_OnKeyDown_Write);
    addClassVar('TWinControl', 'OnKeyPress', 'TKeyPressEvent', nil, @_LapeWinControl_OnKeyPress_Write);
    addClassVar('TWinControl', 'OnKeyUp', 'TKeyEvent', nil, @_LapeWinControl_OnKeyUp_Write);
    addClassVar('TWinControl', 'ParentWindow', 'THandle', @_LapeWinControl_ParentWindow_Read, @_LapeWinControl_ParentWindow_Write);
    addClassVar('TWinControl', 'Showing', 'Boolean', @_LapeWinControl_Showing_Read);
    addGlobalFunc('procedure TWinControl.BeginUpdateBounds;', @_LapeWinControl_BeginUpdateBounds);
    addGlobalFunc('procedure TWinControl.EndUpdateBounds;', @_LapeWinControl_EndUpdateBounds);
    addGlobalFunc('procedure TWinControl.LockRealizeBounds;', @_LapeWinControl_LockRealizeBounds);
    addGlobalFunc('procedure TWinControl.UnlockRealizeBounds;', @_LapeWinControl_UnlockRealizeBounds);
    addGlobalFunc('function TWinControl.ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;', @_LapeWinControl_ControlAtPos);
    addGlobalFunc('function TWinControl.ControlAtPos(const Pos: TPoint;AllowDisabled, AllowWinControls: Boolean): TControl; overload', @_LapeWinControl_ControlAtPosEx);
    addGlobalFunc('function TWinControl.ContainsControl(Control: TControl): Boolean;', @_LapeWinControl_ContainsControl);
    addGlobalFunc('procedure TWinControl.DisableAlign;', @_LapeWinControl_DisableAlign);
    addGlobalFunc('procedure TWinControl.EnableAlign;', @_LapeWinControl_EnableAlign);
    addGlobalFunc('procedure TWinControl.ReAlign;', @_LapeWinControl_ReAlign);
    addGlobalFunc('procedure TWinControl.ScrollBy(DeltaX, DeltaY: Integer);', @_LapeWinControl_ScrollBy);
    addGlobalFunc('procedure TWinControl.Init(TheOwner: TComponent); override', @_LapeWinControl_Init);
    addGlobalFunc('procedure TWinControl.CreateParented(AParentWindow: Thandle);', @_LapeWinControl_CreateParented);
    addGlobalFunc('function TWinControl.CanFocus: Boolean;', @_LapeWinControl_CanFocus);
    addGlobalFunc('function TWinControl.GetControlIndex(AControl: TControl): integer;', @_LapeWinControl_GetControlIndex);
    addGlobalFunc('procedure TWinControl.SetControlIndex(AControl: TControl; NewIndex: integer);', @_LapeWinControl_SetControlIndex);
    addGlobalFunc('function TWinControl.Focused: Boolean;', @_LapeWinControl_Focused);
    addGlobalFunc('function TWinControl.PerformTab(ForwardTab: boolean): boolean;', @_LapeWinControl_PerformTab);
    addGlobalFunc('function TWinControl.FindChildControl(const ControlName: String): TControl;', @_LapeWinControl_FindChildControl);
    addGlobalFunc('procedure TWinControl.SelectNext(CurControl: TWinControl;GoForward, CheckTabStop: Boolean);', @_LapeWinControl_SelectNext);
    addGlobalFunc('procedure TWinControl.AddControl;', @_LapeWinControl_AddControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl);', @_LapeWinControl_InsertControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl; Index: integer); overload', @_LapeWinControl_InsertControlEx);
    addGlobalFunc('procedure TWinControl.RemoveControl(AControl: TControl);', @_LapeWinControl_RemoveControl);
    addGlobalFunc('procedure TWinControl.SetFocus;', @_LapeWinControl_SetFocus);
    addGlobalFunc('procedure TWinControl.FlipChildren(AllLevels: Boolean);', @_LapeWinControl_FlipChildren);
    addGlobalFunc('procedure TWinControl.ScaleBy(Multiplier, Divider: Integer);', @_LapeWinControl_ScaleBy);
    addGlobalFunc('function TWinControl.HandleAllocated: Boolean;', @_LapeWinControl_HandleAllocated);
    addGlobalFunc('procedure TWinControl.HandleNeeded;', @_LapeWinControl_HandleNeeded);
    addGlobalFunc('function TWinControl.BrushCreated: Boolean;', @_LapeWinControl_BrushCreated);
    addGlobalFunc('procedure TWinControl.PaintTo(ACanvas: TCanvas; X, Y: Integer);', @_LapeWinControl_PaintTo);
    addGlobalFunc('procedure TWinControl.SetShape(AShape: TBitmap);', @_LapeWinControl_SetShape);

    addClass('TCustomControl', 'TWinControl');
    addGlobalFunc('procedure TCustomControl.Init(AOwner: TComponent); override', @_LapeCustomControl_Init);
    addClassVar('TCustomControl', 'Canvas', 'TCanvas', @_LapeCustomControl_Canvas_Read, @_LapeCustomControl_Canvas_Write);
    addClassVar('TCustomControl', 'OnPaint', 'TNotifyEvent', @_LapeCustomControl_OnPaint_Read, @_LapeCustomControl_OnPaint_Write);

    addClass('TControlScrollBar', 'TPersistent');
    addGlobalFunc('procedure TControlScrollBar.Init(AControl: TWinControl; AKind: TScrollBarKind)', @_LapeControlScrollBar_Init);
    addGlobalFunc('function TControlScrollBar.IsScrollBarVisible: Boolean;', @_LapeControlScrollBar_IsScrollBarVisible);
    addGlobalFunc('function TControlScrollBar.ScrollPos: Integer;', @_LapeControlScrollBar_ScrollPos);
    addGlobalFunc('function TControlScrollBar.GetOtherScrollBar: TControlScrollBar;', @_LapeControlScrollBar_GetOtherScrollBar);
    addClassVar('TControlScrollBar', 'Size', 'integer', @_LapeControlScrollBar_Size_Read);
    addGlobalFunc('function TControlScrollBar.ClientSize: integer;', @_LapeControlScrollBar_ClientSize);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithBar: integer;', @_LapeControlScrollBar_ClientSizeWithBar);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithoutBar: integer;', @_LapeControlScrollBar_ClientSizeWithoutBar);
    addClassVar('TControlScrollBar', 'Increment', 'Integer', @_LapeControlScrollBar_Increment_Read, @_LapeControlScrollBar_Increment_Write);
    addClassVar('TControlScrollBar', 'Page', 'Integer', @_LapeControlScrollBar_Page_Read, @_LapeControlScrollBar_Page_Write);
    addClassVar('TControlScrollBar', 'Smooth', 'Boolean', @_LapeControlScrollBar_Smooth_Read, @_LapeControlScrollBar_Smooth_Write);
    addClassVar('TControlScrollBar', 'Position', 'Integer', @_LapeControlScrollBar_Position_Read, @_LapeControlScrollBar_Position_Write);
    addClassVar('TControlScrollBar', 'Range', 'Integer', @_LapeControlScrollBar_Range_Read, @_LapeControlScrollBar_Range_Write);
    addClassVar('TControlScrollBar', 'Tracking', 'Boolean', @_LapeControlScrollBar_Tracking_Read, @_LapeControlScrollBar_Tracking_Write);
    addClassVar('TControlScrollBar', 'Visible', 'Boolean', @_LapeControlScrollBar_Visible_Read, @_LapeControlScrollBar_Visible_Write);

    addClass('TScrollingWinControl', 'TCustomControl');
    addGlobalFunc('procedure TScrollingWinControl.Init(AOwner: TComponent); override', @_LapeScrollingWinControl_Init);
    addGlobalFunc('procedure TScrollingWinControl.UpdateScrollbars;', @_LapeScrollingWinControl_UpdateScrollbars);
    addClassVar('TScrollingWinControl', 'HorzScrollBar', 'TControlScrollBar', @_LapeScrollingWinControl_HorzScrollBar_Read, @_LapeScrollingWinControl_HorzScrollBar_Write);
    addClassVar('TScrollingWinControl', 'VertScrollBar', 'TControlScrollBar', @_LapeScrollingWinControl_VertScrollBar_Read, @_LapeScrollingWinControl_VertScrollBar_Write);

    addClass('TGraphicControl', 'TControl');
    addClassVar('TGraphicControl', 'Canvas', 'TCanvas', @_LapeGraphicControl_Canvas_Read);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportLCLControls);

end.

