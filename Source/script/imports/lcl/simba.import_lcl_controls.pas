unit simba.import_lcl_controls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

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

procedure _LapeControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^ := TControl.Create(PComponent(Params^[1])^);
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

procedure _LapeWinControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^ := TWinControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeWinControl_CreateParented(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PWinControl(Params^[0])^ := TWinControl.CreateParented(Phandle(Params^[1])^);
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

procedure _LapeCustomControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomControl(Params^[0])^ := TCustomControl.Create(PComponent(Params^[1])^);
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

procedure _LapeControlScrollBar_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControlScrollBar(Params^[0])^ := TControlScrollBar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
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

procedure _LapeScrollingWinControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollingWinControl(Params^[0])^ := TScrollingWinControl.Create(PComponent(Params^[1])^);
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
    addGlobalType('(ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssMeta, ssSuper, ssHyper, ssAltGr, ssCaps, ssNum, ssScroll, ssTriple, ssQuad, ssExtra1, ssExtra2)', 'TShiftStateEnum');
    addGlobalType('set of TShiftStateEnum', 'TShiftState');
    addGlobalType('(mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2)', 'TMouseButton');

    addGlobalType('procedure(Sender: TObject; var Key: Int16; Shift: TShiftState) of object', 'TKeyEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var Key: Char) of object', 'TKeyPressEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object', 'TMouseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer) of object', 'TMouseMoveEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object', 'TMouseWheelEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of object', 'TMouseWheelUpDownEvent', FFI_DEFAULT_ABI);

    addGlobalType('(sbHorizontal, sbVertical)', 'TScrollBarKind');
    addGlobalType('(alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom)', 'TAlign');
    addGlobalType('(akTop, akLeft, akRight, akBottom)', 'TAnchorKind');
    addGlobalType('set of TAnchorKind', 'TAnchors');
    addGlobalType('(asrTop, asrBottom, asrCenter)', 'TAnchorSideReference');
    addGlobalType('Integer', 'TCursor');
    addGlobalVar(crDefault, 'crDefault').isConstant := True;
    addGlobalVar(crNone, 'crNone').isConstant := True;
    addGlobalVar(crSize, 'crSize').isConstant := True;
    addGlobalVar(crCross, 'crCross').isConstant := True;
    addGlobalVar(crHandPoint, 'crHandPoint').isConstant := True;
    addGlobalVar(crIBeam, 'crIBeam').isConstant := True;

    addClass('TControlBorderSpacing');
    addClassVar('TControlBorderSpacing', 'Left', 'Integer', @_LapeControlBorderSpacing_Left_Read, @_LapeControlBorderSpacing_Left_Write);
    addClassVar('TControlBorderSpacing', 'Right', 'Integer', @_LapeControlBorderSpacing_Right_Read, @_LapeControlBorderSpacing_Right_Write);
    addClassVar('TControlBorderSpacing', 'Bottom', 'Integer', @_LapeControlBorderSpacing_Bottom_Read, @_LapeControlBorderSpacing_Bottom_Write);
    addClassVar('TControlBorderSpacing', 'Around', 'Integer', @_LapeControlBorderSpacing_Around_Read, @_LapeControlBorderSpacing_Around_Write);
    addClassVar('TControlBorderSpacing', 'InnerBorder', 'Integer', @_LapeControlBorderSpacing_InnerBorder_Read, @_LapeControlBorderSpacing_InnerBorder_Write);

    addClass('TControl', 'TComponent');
    addClass('TWinControl', 'TControl');

    addClass('TAnchorSide');
    addClassVar('TAnchorSide', 'Side', 'TAnchorSideReference', @_LapeAnchorSide_Side_Read, @_LapeAnchorSide_Side_Write);
    addClassVar('TAnchorSide', 'Kind', 'TAnchorKind', @_LapeAnchorSide_Kind_Read);
    addClassVar('TAnchorSide', 'Control', 'TControl', @_LapeAnchorSide_Control_Read);

    addClassVar('TControl', 'Anchors', 'TAnchors',  @_LapeControl_Anchors_Read, @_LapeControl_Anchors_Write);
    addClassVar('TControl', 'AnchorSideLeft', 'TAnchorSide',  @_LapeControl_AnchorSideLeft_Read);
    addClassVar('TControl', 'AnchorSideTop', 'TAnchorSide',  @_LapeControl_AnchorSideTop_Read);
    addClassVar('TControl', 'AnchorSideRight', 'TAnchorSide',  @_LapeControl_AnchorSideRight_Read);
    addClassVar('TControl', 'AnchorSideBottom', 'TAnchorSide',  @_LapeControl_AnchorSideBottom_Read);

    addGlobalFunc('procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);', @_LapeControl_SetBounds);
    addGlobalFunc('procedure TControl.Init(AOwner: TComponent); override', @_LapeControl_Init);
    addGlobalFunc('procedure TControl.BringToFront;', @_LapeControl_BringToFront);
    addGlobalFunc('procedure TControl.Hide;', @_LapeControl_Hide);
    addGlobalFunc('procedure TControl.Refresh;', @_LapeControl_Refresh);
    addGlobalFunc('procedure TControl.Repaint;', @_LapeControl_Repaint);
    addGlobalFunc('procedure TControl.Invalidate;', @_LapeControl_Invalidate);
    addGlobalFunc('procedure TControl.SendToBack;', @_LapeControl_SendToBack);
    addGlobalFunc('function TControl.ScreenToClient(const APoint: TPoint): TPoint;', @_LapeControl_ScreenToClient);
    addGlobalFunc('function TControl.ClientToScreen(const APoint: TPoint): TPoint;', @_LapeControl_ClientToScreen);
    addGlobalFunc('function TControl.ScreenToControl(const APoint: TPoint): TPoint;', @_LapeControl_ScreenToControl);
    addGlobalFunc('function TControl.ControlToScreen(const APoint: TPoint): TPoint;', @_LapeControl_ControlToScreen);
    addGlobalFunc('procedure TControl.ShowHint;', @_LapeControl_ShowHint);
    addGlobalFunc('procedure TControl.Show;', @_LapeControl_Show);
    addGlobalFunc('procedure TControl.Update;', @_LapeControl_Update);

    addClassVar('TControl', 'Cursor', 'TCursor', @_LapeControl_Cursor_Read, @_LapeControl_Cursor_Write);
    addClassVar('TControl', 'Align', 'TAlign', @_LapeControl_Align_Read, @_LapeControl_Align_Write);
    addClassVar('TControl', 'AutoSize', 'Boolean', @_LapeControl_AutoSize_Read, @_LapeControl_AutoSize_Write);
    addClassVar('TControl', 'BoundsRect', 'TRect', @_LapeControl_BoundsRect_Read, @_LapeControl_BoundsRect_Write);
    addClassVar('TControl', 'Caption', 'String', @_LapeControl_Caption_Read, @_LapeControl_Caption_Write);
    addClassVar('TControl', 'ClientHeight', 'Integer', @_LapeControl_ClientHeight_Read, @_LapeControl_ClientHeight_Write);
    addClassVar('TControl', 'ClientRect', 'TRect', @_LapeControl_ClientRect_Read);
    addClassVar('TControl', 'ClientWidth', 'Integer', @_LapeControl_ClientWidth_Read, @_LapeControl_ClientWidth_Write);
    addClassVar('TControl', 'Color', 'Integer', @_LapeControl_Color_Read, @_LapeControl_Color_Write);
    addClassVar('TControl', 'Enabled', 'Boolean', @_LapeControl_Enabled_Read, @_LapeControl_Enabled_Write);
    addClassVar('TControl', 'Font', 'TFont', @_LapeControl_Font_Read, @_LapeControl_Font_Write);
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
    addClassVar('TControl', 'BorderSpacing', 'TControlBorderSpacing', @_LapeControl_BorderSpacing_Read);

    addClassVar('TWinControl', 'Brush', 'TBrush', @_LapeWinControl_Brush_Read);
    addClassVar('TWinControl', 'ControlCount', 'Integer', @_LapeWinControl_ControlCount_Read);
    addClassVar('TWinControl', 'Controls', 'TControl', @_LapeWinControl_Control_Read, nil, True);
    addClassVar('TWinControl', 'DoubleBuffered', 'Boolean', @_LapeWinControl_DoubleBuffered_Read, @_LapeWinControl_DoubleBuffered_Write);
    addClassVar('TWinControl', 'Handle', 'THandle', @_LapeWinControl_Handle_Read, @_LapeWinControl_Handle_Write);
    addClassVar('TWinControl', 'TabOrder', 'Integer', @_LapeWinControl_TabOrder_Read, @_LapeWinControl_TabOrder_Write);
    addClassVar('TWinControl', 'TabStop', 'Boolean', @_LapeWinControl_TabStop_Read, @_LapeWinControl_TabStop_Write);
    addClassVar('TWinControl', 'OnEnter', 'TNotifyEvent', @_LapeWinControl_OnEnter_Read, @_LapeWinControl_OnEnter_Write);
    addClassVar('TWinControl', 'OnExit', 'TNotifyEvent', @_LapeWinControl_OnExit_Read, @_LapeWinControl_OnExit_Write);
    addClassVar('TWinControl', 'OnKeyDown', 'TKeyEvent', nil, @_LapeWinControl_OnKeyDown_Write);
    addClassVar('TWinControl', 'OnKeyPress', 'TKeyPressEvent', nil, @_LapeWinControl_OnKeyPress_Write);
    addClassVar('TWinControl', 'OnKeyUp', 'TKeyEvent', nil, @_LapeWinControl_OnKeyUp_Write);
    addClassVar('TWinControl', 'ParentWindow', 'THandle', @_LapeWinControl_ParentWindow_Read, @_LapeWinControl_ParentWindow_Write);
    addClassVar('TWinControl', 'Showing', 'Boolean', @_LapeWinControl_Showing_Read);
    addGlobalFunc('procedure TWinControl.Init(TheOwner: TComponent); override', @_LapeWinControl_Init);
    addGlobalFunc('procedure TWinControl.CreateParented(AParentWindow: Thandle);', @_LapeWinControl_CreateParented);
    addGlobalFunc('function TWinControl.CanFocus: Boolean;', @_LapeWinControl_CanFocus);
    addGlobalFunc('function TWinControl.Focused: Boolean;', @_LapeWinControl_Focused);
    addGlobalFunc('function TWinControl.FindChildControl(const ControlName: String): TControl;', @_LapeWinControl_FindChildControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl);', @_LapeWinControl_InsertControl);
    addGlobalFunc('procedure TWinControl.InsertControl(AControl: TControl; Index: Integer); overload', @_LapeWinControl_InsertControlEx);
    addGlobalFunc('procedure TWinControl.RemoveControl(AControl: TControl);', @_LapeWinControl_RemoveControl);
    addGlobalFunc('procedure TWinControl.SetFocus;', @_LapeWinControl_SetFocus);
    addGlobalFunc('procedure TWinControl.ScaleBy(Multiplier, Divider: Integer);', @_LapeWinControl_ScaleBy);
    addGlobalFunc('function TWinControl.HandleAllocated: Boolean;', @_LapeWinControl_HandleAllocated);
    addGlobalFunc('procedure TWinControl.HandleNeeded;', @_LapeWinControl_HandleNeeded);
    addGlobalFunc('procedure TWinControl.PaintTo(ACanvas: TCanvas; X, Y: Integer);', @_LapeWinControl_PaintTo);
    addGlobalFunc('procedure TWinControl.SetShape(AShape: TBitmap);', @_LapeWinControl_SetShape);

    addClass('TCustomControl', 'TWinControl');
    addGlobalFunc('procedure TCustomControl.Init(AOwner: TComponent); override', @_LapeCustomControl_Init);
    addClassVar('TCustomControl', 'Canvas', 'TCanvas', @_LapeCustomControl_Canvas_Read, @_LapeCustomControl_Canvas_Write);
    addClassVar('TCustomControl', 'OnPaint', 'TNotifyEvent', @_LapeCustomControl_OnPaint_Read, @_LapeCustomControl_OnPaint_Write);

    addClass('TControlScrollBar');
    addGlobalFunc('procedure TControlScrollBar.Init(AControl: TWinControl; AKind: TScrollBarKind)', @_LapeControlScrollBar_Init);
    addGlobalFunc('function TControlScrollBar.IsScrollBarVisible: Boolean;', @_LapeControlScrollBar_IsScrollBarVisible);
    addGlobalFunc('function TControlScrollBar.ScrollPos: Integer;', @_LapeControlScrollBar_ScrollPos);
    addGlobalFunc('function TControlScrollBar.GetOtherScrollBar: TControlScrollBar;', @_LapeControlScrollBar_GetOtherScrollBar);
    addClassVar('TControlScrollBar', 'Size', 'Integer', @_LapeControlScrollBar_Size_Read);
    addGlobalFunc('function TControlScrollBar.ClientSize: Integer;', @_LapeControlScrollBar_ClientSize);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithBar: Integer;', @_LapeControlScrollBar_ClientSizeWithBar);
    addGlobalFunc('function TControlScrollBar.ClientSizeWithoutBar: Integer;', @_LapeControlScrollBar_ClientSizeWithoutBar);
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

end.

