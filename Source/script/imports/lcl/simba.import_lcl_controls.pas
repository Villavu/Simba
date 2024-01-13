unit simba.import_lcl_controls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportLCLControls(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, extctrls, comctrls, graphics, forms, lptypes, ffi;

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
  PControlBorderSpacing = ^TControlBorderSpacing;
  PAnchorSide = ^TAnchorSide;
  PAnchorSideReference = ^TAnchorSideReference;
  PAnchorKind = ^TAnchorKind;
  PAnchors = ^TAnchors;

procedure _LapeAnchorSide_Side_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorSideReference(Result)^ := PAnchorSide(Params^[0])^.Side;
end;

procedure _LapeAnchorSide_Side_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorSide(Params^[0])^.Side := PAnchorSideReference(Params^[1])^;
end;

procedure _LapeAnchorSide_Kind_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorKind(Result)^ := PAnchorSide(Params^[0])^.Kind;
end;

procedure _LapeAnchorSide_Control_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := PAnchorSide(Params^[0])^.Control;
end;

procedure _LapeControlBorderSpacing_Left_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlBorderSpacing(Params^[0])^.Left;
end;

procedure _LapeControlBorderSpacing_Left_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Params^[0])^.Left := PInteger(Params^[1])^;
end;

procedure _LapeControlBorderSpacing_Top_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlBorderSpacing(Params^[0])^.Top;
end;

procedure _LapeControlBorderSpacing_Top_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Params^[0])^.Top := PInteger(Params^[1])^;
end;

procedure _LapeControlBorderSpacing_Right_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlBorderSpacing(Params^[0])^.Right;
end;

procedure _LapeControlBorderSpacing_Right_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Params^[0])^.Right := PInteger(Params^[1])^;
end;

procedure _LapeControlBorderSpacing_Bottom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlBorderSpacing(Params^[0])^.Bottom;
end;

procedure _LapeControlBorderSpacing_Bottom_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Params^[0])^.Bottom := PInteger(Params^[1])^;
end;

procedure _LapeControlBorderSpacing_Around_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlBorderSpacing(Params^[0])^.Around;
end;

procedure _LapeControlBorderSpacing_Around_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Params^[0])^.Around := PInteger(Params^[1])^;
end;

procedure _LapeControlBorderSpacing_InnerBorder_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlBorderSpacing(Params^[0])^.InnerBorder;
end;

procedure _LapeControlBorderSpacing_InnerBorder_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Params^[0])^.InnerBorder := PInteger(Params^[1])^;
end;

procedure _LapeControl_SetBounds(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := TControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeControl_BringToFront(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.BringToFront();
end;

procedure _LapeControl_Hide(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Hide();
end;

procedure _LapeControl_Refresh(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Refresh();
end;

procedure _LapeControl_Repaint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Repaint();
end;

procedure _LapeControl_Invalidate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Invalidate();
end;

procedure _LapeControl_SendToBack(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.SendToBack();
end;

procedure _LapeControl_ScreenToClient(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToClient(PPoint(Params^[1])^);
end;

procedure _LapeControl_ClientToScreen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PControl(Params^[0])^.ClientToScreen(PPoint(Params^[1])^);
end;

procedure _LapeControl_ScreenToControl(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PControl(Params^[0])^.ScreenToControl(PPoint(Params^[1])^);
end;

procedure _LapeControl_ControlToScreen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PControl(Params^[0])^.ControlToScreen(PPoint(Params^[1])^);
end;

procedure _LapeControl_Show(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Show();
end;

procedure _LapeControl_Update(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Update();
end;

procedure _LapeControl_AutoSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControl(Params^[0])^.AutoSize;
end;

procedure _LapeControl_AutoSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.AutoSize := PBoolean(Params^[1])^;
end;

procedure _LapeControl_BoundsRect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PControl(Params^[0])^.BoundsRect;
end;

procedure _LapeControl_BoundsRect_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.BoundsRect := PRect(Params^[1])^;
end;

procedure _LapeControl_Caption_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PControl(Params^[0])^.Caption;
end;

procedure _LapeControl_Caption_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Caption := PString(Params^[1])^;
end;

procedure _LapeControl_ClientHeight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientHeight;
end;

procedure _LapeControl_ClientHeight_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.ClientHeight := PInteger(Params^[1])^;
end;

procedure _LapeControl_ClientRect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PControl(Params^[0])^.ClientRect;
end;

procedure _LapeControl_ClientWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.ClientWidth;
end;

procedure _LapeControl_ClientWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.ClientWidth := PInteger(Params^[1])^;
end;

procedure _LapeControl_Color_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.Color;
end;

procedure _LapeControl_Color_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Color := PInteger(Params^[1])^;
end;

procedure _LapeControl_Enabled_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Enabled;
end;

procedure _LapeControl_Enabled_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Font_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFont(Result)^ := PControl(Params^[0])^.Font;
end;

procedure _LapeControl_Font_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Font := PFont(Params^[1])^;
end;

procedure _LapeControl_MouseInClient_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControl(Params^[0])^.MouseInClient;
end;

procedure _LapeControl_OnChangeBounds_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnChangeBounds;
end;

procedure _LapeControl_OnChangeBounds_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.OnChangeBounds := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_OnClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnClick;
end;

procedure _LapeControl_OnClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_OnResize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PControl(Params^[0])^.OnResize;
end;

procedure _LapeControl_OnResize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.OnResize := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControl_Align_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAlign(Result)^ := PControl(Params^[0])^.Align;
end;

procedure _LapeControl_Align_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Align := PAlign(Params^[1])^;
end;

procedure _LapeControl_Visible_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControl(Params^[0])^.Visible;
end;

procedure _LapeControl_Visible_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Left_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.Left;
end;

procedure _LapeControl_Left_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Left := PInteger(Params^[1])^;
end;

procedure _LapeControl_Height_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.Height;
end;

procedure _LapeControl_Height_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Height := PInteger(Params^[1])^;
end;

procedure _LapeControl_Top_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.Top;
end;

procedure _LapeControl_Top_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Top := PInteger(Params^[1])^;
end;

procedure _LapeControl_Width_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControl(Params^[0])^.Width;
end;

procedure _LapeControl_Width_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Width := PInteger(Params^[1])^;
end;

procedure _LapeControl_ShowHint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.ShowHint := True;
end;

procedure _LapeControl_Parent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Result)^ := PControl(Params^[0])^.Parent;
end;

procedure _LapeControl_Parent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Parent := PWinControl(Params^[1])^;
end;

procedure _LapeControl_Hint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PControl(Params^[0])^.Hint;
end;

procedure _LapeControl_Hint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Hint := PString(Params^[1])^;
end;

procedure _LapeControl_Anchors_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchors(Result)^ := PControl(Params^[0])^.Anchors;
end;

procedure _LapeControl_Anchors_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Anchors := PAnchors(Params^[1])^;
end;

procedure _LapeControl_AnchorSideLeft_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorSide(Result)^ := PControl(Params^[0])^.AnchorSideLeft;
end;

procedure _LapeControl_AnchorSideTop_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorSide(Result)^ := PControl(Params^[0])^.AnchorSideTop;
end;

procedure _LapeControl_AnchorSideRight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorSide(Result)^ := PControl(Params^[0])^.AnchorSideRight;
end;

procedure _LapeControl_AnchorSideBottom_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAnchorSide(Result)^ := PControl(Params^[0])^.AnchorSideBottom;
end;


procedure _LapeControl_BorderSpacing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControlBorderSpacing(Result)^ := PControl(Params^[0])^.BorderSpacing;
end;

procedure _LapeControl_ShowHint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControl(Params^[0])^.ShowHint;
end;

procedure _LapeControl_ShowHint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.ShowHint := PBoolean(Params^[1])^;
end;

procedure _LapeControl_Cursor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCursor(Result)^ := PControl(Params^[0])^.Cursor;
end;

procedure _LapeControl_Cursor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Cursor := PCursor(Params^[1])^;
end;

procedure _LapeWinControl_Brush_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBrush(Result)^ := PWinControl(Params^[0])^.Brush;
end;

procedure _LapeWinControl_ControlCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.ControlCount;
end;

procedure _LapeWinControl_Control_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := PWinControl(Params^[0])^.Controls[PInteger(Params^[1])^];
end;

procedure _LapeWinControl_DoubleBuffered_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.DoubleBuffered;
end;

procedure _LapeWinControl_DoubleBuffered_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.DoubleBuffered := PBoolean(Params^[1])^;
end;

procedure _LapeWinControl_Handle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.Handle;
end;

procedure _LapeWinControl_Handle_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapeWinControl_TabOrder_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PWinControl(Params^[0])^.TabOrder;
end;

procedure _LapeWinControl_TabOrder_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.TabOrder := PInteger(Params^[1])^;
end;

procedure _LapeWinControl_TabStop_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.TabStop;
end;

procedure _LapeWinControl_TabStop_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.TabStop := PBoolean(Params^[1])^;
end;

procedure _LapeWinControl_OnEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnEnter;
end;

procedure _LapeWinControl_OnEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.OnEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnExit_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PWinControl(Params^[0])^.OnExit;
end;

procedure _LapeWinControl_OnExit_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.OnExit := PNotifyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.OnKeyDown := PKeyEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyPress_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.OnKeyPress := PKeyPressEvent(Params^[1])^;
end;

procedure _LapeWinControl_OnKeyUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.OnKeyUp := PKeyEvent(Params^[1])^;
end;

procedure _LapeWinControl_ParentWindow_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PWinControl(Params^[0])^.ParentWindow;
end;

procedure _LapeWinControl_ParentWindow_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.ParentWindow := PHandle(Params^[1])^;
end;

procedure _LapeWinControl_Showing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Showing;
end;

procedure _LapeWinControl_SetBounds(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeWinControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Result)^ := TWinControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeWinControl_CanFocus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.CanFocus();
end;

procedure _LapeWinControl_Focused(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.Focused();
end;

procedure _LapeWinControl_FindChildControl(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Result)^ := PWinControl(Params^[0])^.FindChildControl(PString(Params^[1])^);
end;

procedure _LapeWinControl_Invalidate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.Invalidate();
end;

procedure _LapeWinControl_InsertControl(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_InsertControlEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.InsertControl(PControl(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeWinControl_RemoveControl(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.RemoveControl(PControl(Params^[1])^);
end;

procedure _LapeWinControl_Repaint(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.Repaint();
end;

procedure _LapeWinControl_Update(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.Update();
end;

procedure _LapeWinControl_SetFocus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.SetFocus();
end;

procedure _LapeWinControl_ScaleBy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.ScaleBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeWinControl_HandleAllocated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.HandleAllocated();
end;

procedure _LapeWinControl_HandleNeeded(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.HandleNeeded();
end;

procedure _LapeWinControl_PaintTo(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.PaintTo(PCanvas(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeWinControl_SetShape(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^.SetShape(PBitmap(Params^[1])^);
end;

procedure _LapeCustomControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomControl(Result)^ := TCustomControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PCustomControl(Params^[0])^.Canvas;
end;

procedure _LapeCustomControl_Canvas_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomControl(Params^[0])^.Canvas := PCanvas(Params^[1])^;
end;

procedure _LapeCustomControl_OnPaint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomControl(Params^[0])^.OnPaint;
end;

procedure _LapeCustomControl_OnPaint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomControl(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeControlScrollBar_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Result)^ := TControlScrollBar.Create(PWinControl(Params^[0])^, PScrollBarKind(Params^[1])^);
end;

procedure _LapeControlScrollBar_IsScrollBarVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.IsScrollBarVisible();
end;

procedure _LapeControlScrollBar_ScrollPos(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.ScrollPos();
end;

procedure _LapeControlScrollBar_GetOtherScrollBar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Result)^ := PControlScrollBar(Params^[0])^.GetOtherScrollBar();
end;

procedure _LapeControlScrollBar_Size_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.Size;
end;

procedure _LapeControlScrollBar_ClientSize(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSize();
end;

procedure _LapeControlScrollBar_ClientSizeWithBar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithBar();
end;

procedure _LapeControlScrollBar_ClientSizeWithoutBar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PControlScrollBar(Params^[0])^.ClientSizeWithoutBar();
end;

procedure _LapeControlScrollBar_Increment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Increment;
end;

procedure _LapeControlScrollBar_Increment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Increment := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Page_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Page;
end;

procedure _LapeControlScrollBar_Page_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Page := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Smooth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Smooth;
end;

procedure _LapeControlScrollBar_Smooth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Smooth := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Position;
end;

procedure _LapeControlScrollBar_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Range_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PControlScrollBar(Params^[0])^.Range;
end;

procedure _LapeControlScrollBar_Range_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Range := PInteger(Params^[1])^;
end;

procedure _LapeControlScrollBar_Tracking_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Tracking;
end;

procedure _LapeControlScrollBar_Tracking_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Tracking := PBoolean(Params^[1])^;
end;

procedure _LapeControlScrollBar_Visible_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PControlScrollBar(Params^[0])^.Visible;
end;

procedure _LapeControlScrollBar_Visible_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

procedure _LapeScrollingWinControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollingWinControl(Result)^ := TScrollingWinControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeScrollingWinControl_UpdateScrollbars(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollingWinControl(Params^[0])^.UpdateScrollbars();
end;

procedure _LapeScrollingWinControl_ScrollBy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollingWinControl(Params^[0])^.ScrollBy(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeScrollingWinControl_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.HorzScrollBar;
end;

procedure _LapeScrollingWinControl_HorzScrollBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollingWinControl(Params^[0])^.HorzScrollBar := PControlScrollBar(Params^[1])^;
end;

procedure _LapeScrollingWinControl_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Result)^ := PScrollingWinControl(Params^[0])^.VertScrollBar;
end;

procedure _LapeScrollingWinControl_VertScrollBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollingWinControl(Params^[0])^.VertScrollBar := PControlScrollBar(Params^[1])^;
end;

procedure _LapeGraphicControl_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PGraphicControl(Params^[0])^.Canvas;
end;

procedure _LapeGraphicControl_Update(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGraphicControl(Params^[0])^.Update();
end;

procedure ImportLCLControls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2)', 'TLazShiftStateEnum');
    addGlobalType('set of TLazShiftStateEnum', 'TLazShiftState');
    addGlobalType('(mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2)', 'TLazMouseButton');

    addGlobalType('procedure(Sender: TObject; var Key: Int16; Shift: TLazShiftState) of object', 'TLazKeyEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var Key: Char) of object', 'TLazKeyPressEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Button: TLazMouseButton; Shift: TLazShiftState; X, Y: Integer) of object', 'TLazMouseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TLazShiftState; X, Y: Integer) of object', 'TLazMouseMoveEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TLazShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object', 'TLazMouseWheelEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TLazShiftState; MousePos: TPoint; var Handled: Boolean) of object', 'TLazMouseWheelUpDownEvent', FFI_DEFAULT_ABI);

    addGlobalType('(sbHorizontal, sbVertical)', 'TLazScrollBarKind');
    addGlobalType('(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom)', 'TLazAlign');
    addGlobalType('(akTop, akLeft, akRight, akBottom)', 'TLazAnchorKind');
    addGlobalType('set of TLazAnchorKind', 'TLazAnchors');
    addGlobalType('(asrTop, asrBottom, asrCenter)', 'TLazAnchorSideReference');
    addGlobalType('Integer', 'TLazCursor');
    addGlobalVar(crDefault, 'crDefault').isConstant := True;
    addGlobalVar(crNone, 'crNone').isConstant := True;
    addGlobalVar(crSize, 'crSize').isConstant := True;
    addGlobalVar(crCross, 'crCross').isConstant := True;
    addGlobalVar(crHandPoint, 'crHandPoint').isConstant := True;
    addGlobalVar(crIBeam, 'crIBeam').isConstant := True;

    addClass('TLazControlBorderSpacing');
    addClassVar('TLazControlBorderSpacing', 'Left', 'Integer', @_LapeControlBorderSpacing_Left_Read, @_LapeControlBorderSpacing_Left_Write);
    addClassVar('TLazControlBorderSpacing', 'Right', 'Integer', @_LapeControlBorderSpacing_Right_Read, @_LapeControlBorderSpacing_Right_Write);
    addClassVar('TLazControlBorderSpacing', 'Bottom', 'Integer', @_LapeControlBorderSpacing_Bottom_Read, @_LapeControlBorderSpacing_Bottom_Write);
    addClassVar('TLazControlBorderSpacing', 'Around', 'Integer', @_LapeControlBorderSpacing_Around_Read, @_LapeControlBorderSpacing_Around_Write);
    addClassVar('TLazControlBorderSpacing', 'InnerBorder', 'Integer', @_LapeControlBorderSpacing_InnerBorder_Read, @_LapeControlBorderSpacing_InnerBorder_Write);

    addClass('TLazControl', 'TLazComponent');
    addClass('TLazWinControl', 'TLazControl');

    addClass('TLazAnchorSide');
    addClassVar('TLazAnchorSide', 'Side', 'TLazAnchorSideReference', @_LapeAnchorSide_Side_Read, @_LapeAnchorSide_Side_Write);
    addClassVar('TLazAnchorSide', 'Kind', 'TLazAnchorKind', @_LapeAnchorSide_Kind_Read);
    addClassVar('TLazAnchorSide', 'Control', 'TLazControl', @_LapeAnchorSide_Control_Read);

    addClassVar('TLazControl', 'Anchors', 'TLazAnchors',  @_LapeControl_Anchors_Read, @_LapeControl_Anchors_Write);
    addClassVar('TLazControl', 'AnchorSideLeft', 'TLazAnchorSide',  @_LapeControl_AnchorSideLeft_Read);
    addClassVar('TLazControl', 'AnchorSideTop', 'TLazAnchorSide',  @_LapeControl_AnchorSideTop_Read);
    addClassVar('TLazControl', 'AnchorSideRight', 'TLazAnchorSide',  @_LapeControl_AnchorSideRight_Read);
    addClassVar('TLazControl', 'AnchorSideBottom', 'TLazAnchorSide',  @_LapeControl_AnchorSideBottom_Read);

    addGlobalFunc('procedure TLazControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);', @_LapeControl_SetBounds);
    addClassConstructor('TLazControl', '(AOwner: TLazComponent)', @_LapeControl_Create);
    addGlobalFunc('procedure TLazControl.BringToFront;', @_LapeControl_BringToFront);
    addGlobalFunc('procedure TLazControl.Hide;', @_LapeControl_Hide);
    addGlobalFunc('procedure TLazControl.Refresh;', @_LapeControl_Refresh);
    addGlobalFunc('procedure TLazControl.Repaint;', @_LapeControl_Repaint);
    addGlobalFunc('procedure TLazControl.Invalidate;', @_LapeControl_Invalidate);
    addGlobalFunc('procedure TLazControl.SendToBack;', @_LapeControl_SendToBack);
    addGlobalFunc('function TLazControl.ScreenToClient(const APoint: TPoint): TPoint;', @_LapeControl_ScreenToClient);
    addGlobalFunc('function TLazControl.ClientToScreen(const APoint: TPoint): TPoint;', @_LapeControl_ClientToScreen);
    addGlobalFunc('function TLazControl.ScreenToControl(const APoint: TPoint): TPoint;', @_LapeControl_ScreenToControl);
    addGlobalFunc('function TLazControl.ControlToScreen(const APoint: TPoint): TPoint;', @_LapeControl_ControlToScreen);
    addGlobalFunc('procedure TLazControl.ShowHint;', @_LapeControl_ShowHint);
    addGlobalFunc('procedure TLazControl.Show;', @_LapeControl_Show);
    addGlobalFunc('procedure TLazControl.Update;', @_LapeControl_Update);

    addClassVar('TLazControl', 'Cursor', 'TLazCursor', @_LapeControl_Cursor_Read, @_LapeControl_Cursor_Write);
    addClassVar('TLazControl', 'Align', 'TLazAlign', @_LapeControl_Align_Read, @_LapeControl_Align_Write);
    addClassVar('TLazControl', 'AutoSize', 'Boolean', @_LapeControl_AutoSize_Read, @_LapeControl_AutoSize_Write);
    addClassVar('TLazControl', 'BoundsRect', 'TLazRect', @_LapeControl_BoundsRect_Read, @_LapeControl_BoundsRect_Write);
    addClassVar('TLazControl', 'Caption', 'String', @_LapeControl_Caption_Read, @_LapeControl_Caption_Write);
    addClassVar('TLazControl', 'ClientHeight', 'Integer', @_LapeControl_ClientHeight_Read, @_LapeControl_ClientHeight_Write);
    addClassVar('TLazControl', 'ClientRect', 'TLazRect', @_LapeControl_ClientRect_Read);
    addClassVar('TLazControl', 'ClientWidth', 'Integer', @_LapeControl_ClientWidth_Read, @_LapeControl_ClientWidth_Write);
    addClassVar('TLazControl', 'Color', 'Integer', @_LapeControl_Color_Read, @_LapeControl_Color_Write);
    addClassVar('TLazControl', 'Enabled', 'Boolean', @_LapeControl_Enabled_Read, @_LapeControl_Enabled_Write);
    addClassVar('TLazControl', 'Font', 'TLazFont', @_LapeControl_Font_Read, @_LapeControl_Font_Write);
    addClassVar('TLazControl', 'OnChangeBounds', 'TLazNotifyEvent', @_LapeControl_OnChangeBounds_Read, @_LapeControl_OnChangeBounds_Write);
    addClassVar('TLazControl', 'OnClick', 'TLazNotifyEvent', @_LapeControl_OnClick_Read, @_LapeControl_OnClick_Write);
    addClassVar('TLazControl', 'OnResize', 'TLazNotifyEvent', @_LapeControl_OnResize_Read, @_LapeControl_OnResize_Write);
    addClassVar('TLazControl', 'Visible', 'Boolean', @_LapeControl_Visible_Read, @_LapeControl_Visible_Write);
    addClassVar('TLazControl', 'ShowHint', 'Boolean', @_LapeControl_ShowHint_Read, @_LapeControl_ShowHint_Write);
    addClassVar('TLazControl', 'Left', 'Integer', @_LapeControl_Left_Read, @_LapeControl_Left_Write);
    addClassVar('TLazControl', 'Height', 'Integer', @_LapeControl_Height_Read, @_LapeControl_Height_Write);
    addClassVar('TLazControl', 'Top', 'Integer', @_LapeControl_Top_Read, @_LapeControl_Top_Write);
    addClassVar('TLazControl', 'Width', 'Integer', @_LapeControl_Width_Read, @_LapeControl_Width_Write);
    addClassVar('TLazControl', 'Hint', 'String', @_LapeControl_Hint_Read, @_LapeControl_Hint_Write);
    addClassVar('TLazControl', 'Parent', 'TLazWinControl', @_LapeControl_Parent_Read, @_LapeControl_Parent_Write);
    addClassVar('TLazControl', 'BorderSpacing', 'TLazControlBorderSpacing', @_LapeControl_BorderSpacing_Read);

    addClassVar('TLazWinControl', 'Brush', 'TLazBrush', @_LapeWinControl_Brush_Read);
    addClassVar('TLazWinControl', 'ControlCount', 'Integer', @_LapeWinControl_ControlCount_Read);
    addClassVar('TLazWinControl', 'Controls', 'TLazControl', @_LapeWinControl_Control_Read, nil, True);
    addClassVar('TLazWinControl', 'DoubleBuffered', 'Boolean', @_LapeWinControl_DoubleBuffered_Read, @_LapeWinControl_DoubleBuffered_Write);
    addClassVar('TLazWinControl', 'Handle', 'TLazHandle', @_LapeWinControl_Handle_Read, @_LapeWinControl_Handle_Write);
    addClassVar('TLazWinControl', 'TabOrder', 'Integer', @_LapeWinControl_TabOrder_Read, @_LapeWinControl_TabOrder_Write);
    addClassVar('TLazWinControl', 'TabStop', 'Boolean', @_LapeWinControl_TabStop_Read, @_LapeWinControl_TabStop_Write);
    addClassVar('TLazWinControl', 'OnEnter', 'TLazNotifyEvent', @_LapeWinControl_OnEnter_Read, @_LapeWinControl_OnEnter_Write);
    addClassVar('TLazWinControl', 'OnExit', 'TLazNotifyEvent', @_LapeWinControl_OnExit_Read, @_LapeWinControl_OnExit_Write);
    addClassVar('TLazWinControl', 'OnKeyDown', 'TLazKeyEvent', nil, @_LapeWinControl_OnKeyDown_Write);
    addClassVar('TLazWinControl', 'OnKeyPress', 'TLazKeyPressEvent', nil, @_LapeWinControl_OnKeyPress_Write);
    addClassVar('TLazWinControl', 'OnKeyUp', 'TLazKeyEvent', nil, @_LapeWinControl_OnKeyUp_Write);
    addClassVar('TLazWinControl', 'ParentWindow', 'TLazHandle', @_LapeWinControl_ParentWindow_Read, @_LapeWinControl_ParentWindow_Write);
    addClassVar('TLazWinControl', 'Showing', 'Boolean', @_LapeWinControl_Showing_Read);
    addClassConstructor('TLazWinControl', '(TheOwner: TLazComponent)', @_LapeWinControl_Create);
    addGlobalFunc('function TLazWinControl.CanFocus: Boolean;', @_LapeWinControl_CanFocus);
    addGlobalFunc('function TLazWinControl.Focused: Boolean;', @_LapeWinControl_Focused);
    addGlobalFunc('function TLazWinControl.FindChildControl(const ControlName: String): TLazControl;', @_LapeWinControl_FindChildControl);
    addGlobalFunc('procedure TLazWinControl.InsertControl(AControl: TLazControl);', @_LapeWinControl_InsertControl);
    addGlobalFunc('procedure TLazWinControl.InsertControl(AControl: TLazControl; Index: Integer); overload', @_LapeWinControl_InsertControlEx);
    addGlobalFunc('procedure TLazWinControl.RemoveControl(AControl: TLazControl);', @_LapeWinControl_RemoveControl);
    addGlobalFunc('procedure TLazWinControl.SetFocus;', @_LapeWinControl_SetFocus);
    addGlobalFunc('procedure TLazWinControl.ScaleBy(Multiplier, Divider: Integer);', @_LapeWinControl_ScaleBy);
    addGlobalFunc('function TLazWinControl.HandleAllocated: Boolean;', @_LapeWinControl_HandleAllocated);
    addGlobalFunc('procedure TLazWinControl.HandleNeeded;', @_LapeWinControl_HandleNeeded);
    addGlobalFunc('procedure TLazWinControl.PaintTo(ACanvas: TLazCanvas; X, Y: Integer);', @_LapeWinControl_PaintTo);
    addGlobalFunc('procedure TLazWinControl.SetShape(AShape: TLazBitmap);', @_LapeWinControl_SetShape);

    addClass('TLazCustomControl', 'TLazWinControl');
    addClassConstructor('TLazCustomControl', '(AOwner: TLazComponent)', @_LapeCustomControl_Create);
    addClassVar('TLazCustomControl', 'Canvas', 'TLazCanvas', @_LapeCustomControl_Canvas_Read, @_LapeCustomControl_Canvas_Write);
    addClassVar('TLazCustomControl', 'OnPaint', 'TLazNotifyEvent', @_LapeCustomControl_OnPaint_Read, @_LapeCustomControl_OnPaint_Write);

    addClass('TLazControlScrollBar');
    addClassConstructor('TLazControlScrollBar' ,'(AControl: TLazWinControl; AKind: TLazScrollBarKind)', @_LapeControlScrollBar_Create);
    addGlobalFunc('function TLazControlScrollBar.IsScrollBarVisible: Boolean;', @_LapeControlScrollBar_IsScrollBarVisible);
    addGlobalFunc('function TLazControlScrollBar.ScrollPos: Integer;', @_LapeControlScrollBar_ScrollPos);
    addGlobalFunc('function TLazControlScrollBar.GetOtherScrollBar: TLazControlScrollBar;', @_LapeControlScrollBar_GetOtherScrollBar);
    addClassVar('TLazControlScrollBar', 'Size', 'Integer', @_LapeControlScrollBar_Size_Read);
    addGlobalFunc('function TLazControlScrollBar.ClientSize: Integer;', @_LapeControlScrollBar_ClientSize);
    addGlobalFunc('function TLazControlScrollBar.ClientSizeWithBar: Integer;', @_LapeControlScrollBar_ClientSizeWithBar);
    addGlobalFunc('function TLazControlScrollBar.ClientSizeWithoutBar: Integer;', @_LapeControlScrollBar_ClientSizeWithoutBar);
    addClassVar('TLazControlScrollBar', 'Increment', 'Integer', @_LapeControlScrollBar_Increment_Read, @_LapeControlScrollBar_Increment_Write);
    addClassVar('TLazControlScrollBar', 'Page', 'Integer', @_LapeControlScrollBar_Page_Read, @_LapeControlScrollBar_Page_Write);
    addClassVar('TLazControlScrollBar', 'Smooth', 'Boolean', @_LapeControlScrollBar_Smooth_Read, @_LapeControlScrollBar_Smooth_Write);
    addClassVar('TLazControlScrollBar', 'Position', 'Integer', @_LapeControlScrollBar_Position_Read, @_LapeControlScrollBar_Position_Write);
    addClassVar('TLazControlScrollBar', 'Range', 'Integer', @_LapeControlScrollBar_Range_Read, @_LapeControlScrollBar_Range_Write);
    addClassVar('TLazControlScrollBar', 'Tracking', 'Boolean', @_LapeControlScrollBar_Tracking_Read, @_LapeControlScrollBar_Tracking_Write);
    addClassVar('TLazControlScrollBar', 'Visible', 'Boolean', @_LapeControlScrollBar_Visible_Read, @_LapeControlScrollBar_Visible_Write);

    addClass('TLazScrollingWinControl', 'TLazCustomControl');
    addClassConstructor('TLazScrollingWinControl', '(AOwner: TLazComponent)', @_LapeScrollingWinControl_Create);
    addGlobalFunc('procedure TLazScrollingWinControl.UpdateScrollbars;', @_LapeScrollingWinControl_UpdateScrollbars);
    addClassVar('TLazScrollingWinControl', 'HorzScrollBar', 'TLazControlScrollBar', @_LapeScrollingWinControl_HorzScrollBar_Read, @_LapeScrollingWinControl_HorzScrollBar_Write);
    addClassVar('TLazScrollingWinControl', 'VertScrollBar', 'TLazControlScrollBar', @_LapeScrollingWinControl_VertScrollBar_Read, @_LapeScrollingWinControl_VertScrollBar_Write);

    addClass('TLazGraphicControl', 'TLazControl');
    addClassVar('TLazGraphicControl', 'Canvas', 'TLazCanvas', @_LapeGraphicControl_Canvas_Read);
  end;
end;

end.

