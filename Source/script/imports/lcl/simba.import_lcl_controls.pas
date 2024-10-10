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
  PColor(Result)^ := PControl(Params^[0])^.Color;
end;

procedure _LapeControl_Color_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Color := PColor(Params^[1])^;
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

{$scopedenums on}
type  ELazCursor = (Default, None, Arrow, Cross, IBeam, Size, SizeNESW, SizeNS, SizeNWSE, SizeWE, SizeNW, SizeN, SizeNE, SizeW, SizeE, SizeSW, SizeS, SizeSE, UpArrow, HourGlass, Drag, NoDrop, HSplit, VSplit, MultiDrag, AppStart, Help, HandPoint);
const ELazCurorToTCursor: array[ELazCursor] of TCursor = (crDefault, crNone, crArrow, crCross, crIBeam, crSize, crSizeNESW, crSizeNS, crSizeNWSE, crSizeWE, crSizeNW, crSizeN, crSizeNE, crSizeW, crSizeE, crSizeSW, crSizeS, crSizeSE, crUpArrow, crHourGlass, crDrag, crNoDrop, crHSplit, crVSplit, crMultiDrag, crAppStart, crHelp, crHandPoint);
{$scopedenums off}

procedure _LapeControl_Cursor_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function CursorToEnum(val: TCursor): ELazCursor;
  var
    e: ELazCursor;
  begin
    for e in ELazCursor do
      if (ELazCurorToTCursor[e] = Val) then
        Exit(e);
    Result := ELazCursor.Default;
  end;

begin
  ELazCursor(Result^) := CursorToEnum(PControl(Params^[0])^.Cursor);
end;

procedure _LapeControl_Cursor_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.Cursor := ELazCurorToTCursor[ELazCursor(Params^[1]^)];
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

procedure _LapeWinControl_CanSetFocus(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PWinControl(Params^[0])^.CanSetFocus();
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
    addGlobalType('set of enum(Shift, Alt, Ctrl, Left, Right, Middle, Double, Meta, Super, Hyper, AltGr, Caps, Num, Scroll, Triple, Quad, Extra1, Extra2)', 'ELazShiftStates');
    addGlobalType('enum(Left, Right, Middle, Extra1, Extra2)', 'ELazMouseButton');
    addGlobalType('enum(Default, None, Arrow, Cross, IBeam, Size, SizeNESW, SizeNS, SizeNWSE, SizeWE, SizeNW, SizeN, SizeNE, SizeW, SizeE, SizeSW, SizeS, SizeSE, UpArrow, HourGlass, Drag, NoDrop, HSplit, VSplit, MultiDrag, AppStart, Help, HandPoint)', 'ELazCursor');
    addGlobalType('enum(Horizontal, Vertical)', 'ELazScrollBarKind');
    addGlobalType('enum(None, Top, Bottom, Left, Right, Client, Custom)', 'ELazAlign');
    addGlobalType('enum(Top, Left, Right, Bottom)', 'ELazAnchorKind');
    addGlobalType('set of ELazAnchorKind', 'ELazAnchors');

    addGlobalType('enum(asrTop, asrBottom, asrCenter)', 'ELazAnchorSideReference');
    addGlobalType('procedure(Sender: TObject; var Key: Int16; Shift: ELazShiftStates) of object', 'TLazKeyEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; var Key: Char) of object', 'TLazKeyPressEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Button: ELazMouseButton; Shift: ELazShiftStates; X, Y: Integer) of object', 'TLazMouseEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: ELazShiftStates; X, Y: Integer) of object', 'TLazMouseMoveEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: ELazShiftStates; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object', 'TLazMouseWheelEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Sender: TObject; Shift: ELazShiftStates; MousePos: TPoint; var Handled: Boolean) of object', 'TLazMouseWheelUpDownEvent', FFI_DEFAULT_ABI);

    addClass('TLazControlBorderSpacing');
    addProperty('TLazControlBorderSpacing', 'Left', 'Integer', @_LapeControlBorderSpacing_Left_Read, @_LapeControlBorderSpacing_Left_Write);
    addProperty('TLazControlBorderSpacing', 'Right', 'Integer', @_LapeControlBorderSpacing_Right_Read, @_LapeControlBorderSpacing_Right_Write);
    addProperty('TLazControlBorderSpacing', 'Bottom', 'Integer', @_LapeControlBorderSpacing_Bottom_Read, @_LapeControlBorderSpacing_Bottom_Write);
    addProperty('TLazControlBorderSpacing', 'Around', 'Integer', @_LapeControlBorderSpacing_Around_Read, @_LapeControlBorderSpacing_Around_Write);
    addProperty('TLazControlBorderSpacing', 'InnerBorder', 'Integer', @_LapeControlBorderSpacing_InnerBorder_Read, @_LapeControlBorderSpacing_InnerBorder_Write);

    addClass('TLazControl', 'TLazComponent');
    addClass('TLazWinControl', 'TLazControl');

    addClass('TLazAnchorSide');
    addProperty('TLazAnchorSide', 'Side', 'ELazAnchorSideReference', @_LapeAnchorSide_Side_Read, @_LapeAnchorSide_Side_Write);
    addProperty('TLazAnchorSide', 'Kind', 'ELazAnchorKind', @_LapeAnchorSide_Kind_Read);
    addProperty('TLazAnchorSide', 'Control', 'TLazControl', @_LapeAnchorSide_Control_Read);

    addProperty('TLazControl', 'Anchors', 'ELazAnchors',  @_LapeControl_Anchors_Read, @_LapeControl_Anchors_Write);
    addProperty('TLazControl', 'AnchorSideLeft', 'TLazAnchorSide',  @_LapeControl_AnchorSideLeft_Read);
    addProperty('TLazControl', 'AnchorSideTop', 'TLazAnchorSide',  @_LapeControl_AnchorSideTop_Read);
    addProperty('TLazControl', 'AnchorSideRight', 'TLazAnchorSide',  @_LapeControl_AnchorSideRight_Read);
    addProperty('TLazControl', 'AnchorSideBottom', 'TLazAnchorSide',  @_LapeControl_AnchorSideBottom_Read);

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
    addGlobalFunc('procedure TLazControl.Show;', @_LapeControl_Show);
    addGlobalFunc('procedure TLazControl.Update;', @_LapeControl_Update);

    addProperty('TLazControl', 'Cursor', 'ELazCursor', @_LapeControl_Cursor_Read, @_LapeControl_Cursor_Write);
    addProperty('TLazControl', 'Align', 'ELazAlign', @_LapeControl_Align_Read, @_LapeControl_Align_Write);
    addProperty('TLazControl', 'AutoSize', 'Boolean', @_LapeControl_AutoSize_Read, @_LapeControl_AutoSize_Write);
    addProperty('TLazControl', 'BoundsRect', 'TLazRect', @_LapeControl_BoundsRect_Read, @_LapeControl_BoundsRect_Write);
    addProperty('TLazControl', 'Caption', 'String', @_LapeControl_Caption_Read, @_LapeControl_Caption_Write);
    addProperty('TLazControl', 'ClientHeight', 'Integer', @_LapeControl_ClientHeight_Read, @_LapeControl_ClientHeight_Write);
    addProperty('TLazControl', 'ClientRect', 'TLazRect', @_LapeControl_ClientRect_Read);
    addProperty('TLazControl', 'ClientWidth', 'Integer', @_LapeControl_ClientWidth_Read, @_LapeControl_ClientWidth_Write);
    addProperty('TLazControl', 'Color', 'TColor', @_LapeControl_Color_Read, @_LapeControl_Color_Write);
    addProperty('TLazControl', 'Enabled', 'Boolean', @_LapeControl_Enabled_Read, @_LapeControl_Enabled_Write);
    addProperty('TLazControl', 'Font', 'TLazFont', @_LapeControl_Font_Read, @_LapeControl_Font_Write);
    addProperty('TLazControl', 'OnChangeBounds', 'TLazNotifyEvent', @_LapeControl_OnChangeBounds_Read, @_LapeControl_OnChangeBounds_Write);
    addProperty('TLazControl', 'OnClick', 'TLazNotifyEvent', @_LapeControl_OnClick_Read, @_LapeControl_OnClick_Write);
    addProperty('TLazControl', 'OnResize', 'TLazNotifyEvent', @_LapeControl_OnResize_Read, @_LapeControl_OnResize_Write);
    addProperty('TLazControl', 'Visible', 'Boolean', @_LapeControl_Visible_Read, @_LapeControl_Visible_Write);
    addProperty('TLazControl', 'ShowHint', 'Boolean', @_LapeControl_ShowHint_Read, @_LapeControl_ShowHint_Write);
    addProperty('TLazControl', 'Left', 'Integer', @_LapeControl_Left_Read, @_LapeControl_Left_Write);
    addProperty('TLazControl', 'Height', 'Integer', @_LapeControl_Height_Read, @_LapeControl_Height_Write);
    addProperty('TLazControl', 'Top', 'Integer', @_LapeControl_Top_Read, @_LapeControl_Top_Write);
    addProperty('TLazControl', 'Width', 'Integer', @_LapeControl_Width_Read, @_LapeControl_Width_Write);
    addProperty('TLazControl', 'Hint', 'String', @_LapeControl_Hint_Read, @_LapeControl_Hint_Write);
    addProperty('TLazControl', 'Parent', 'TLazWinControl', @_LapeControl_Parent_Read, @_LapeControl_Parent_Write);
    addProperty('TLazControl', 'BorderSpacing', 'TLazControlBorderSpacing', @_LapeControl_BorderSpacing_Read);

    addProperty('TLazWinControl', 'Brush', 'TLazBrush', @_LapeWinControl_Brush_Read);
    addProperty('TLazWinControl', 'ControlCount', 'Integer', @_LapeWinControl_ControlCount_Read);
    addPropertyIndexed('TLazWinControl', 'Control', 'Index: Integer', 'TLazControl', @_LapeWinControl_Control_Read);
    addProperty('TLazWinControl', 'DoubleBuffered', 'Boolean', @_LapeWinControl_DoubleBuffered_Read, @_LapeWinControl_DoubleBuffered_Write);
    addProperty('TLazWinControl', 'Handle', 'TLazHandle', @_LapeWinControl_Handle_Read, @_LapeWinControl_Handle_Write);
    addProperty('TLazWinControl', 'OnEnter', 'TLazNotifyEvent', @_LapeWinControl_OnEnter_Read, @_LapeWinControl_OnEnter_Write);
    addProperty('TLazWinControl', 'OnExit', 'TLazNotifyEvent', @_LapeWinControl_OnExit_Read, @_LapeWinControl_OnExit_Write);
    addProperty('TLazWinControl', 'OnKeyDown', 'TLazKeyEvent', nil, @_LapeWinControl_OnKeyDown_Write);
    addProperty('TLazWinControl', 'OnKeyPress', 'TLazKeyPressEvent', nil, @_LapeWinControl_OnKeyPress_Write);
    addProperty('TLazWinControl', 'OnKeyUp', 'TLazKeyEvent', nil, @_LapeWinControl_OnKeyUp_Write);
    addProperty('TLazWinControl', 'ParentWindow', 'TLazHandle', @_LapeWinControl_ParentWindow_Read, @_LapeWinControl_ParentWindow_Write);
    addProperty('TLazWinControl', 'Showing', 'Boolean', @_LapeWinControl_Showing_Read);
    addClassConstructor('TLazWinControl', '(TheOwner: TLazComponent)', @_LapeWinControl_Create);
    addGlobalFunc('function TLazWinControl.CanSetFocus: Boolean;', @_LapeWinControl_CanSetFocus);
    addGlobalFunc('function TLazWinControl.Focused: Boolean;', @_LapeWinControl_Focused);
    addGlobalFunc('function TLazWinControl.FindChildControl(const ControlName: String): TLazControl;', @_LapeWinControl_FindChildControl);
    addGlobalFunc('procedure TLazWinControl.SetFocus;', @_LapeWinControl_SetFocus);
    addGlobalFunc('procedure TLazWinControl.ScaleBy(Multiplier, Divider: Integer);', @_LapeWinControl_ScaleBy);
    addGlobalFunc('function TLazWinControl.HandleAllocated: Boolean;', @_LapeWinControl_HandleAllocated);
    addGlobalFunc('procedure TLazWinControl.HandleNeeded;', @_LapeWinControl_HandleNeeded);
    addGlobalFunc('procedure TLazWinControl.PaintTo(ACanvas: TLazCanvas; X, Y: Integer);', @_LapeWinControl_PaintTo);
    addGlobalFunc('procedure TLazWinControl.SetShape(AShape: TLazBitmap);', @_LapeWinControl_SetShape);

    addClass('TLazCustomControl', 'TLazWinControl');
    addClassConstructor('TLazCustomControl', '(AOwner: TLazComponent)', @_LapeCustomControl_Create);
    addProperty('TLazCustomControl', 'Canvas', 'TLazCanvas', @_LapeCustomControl_Canvas_Read, @_LapeCustomControl_Canvas_Write);
    addProperty('TLazCustomControl', 'OnPaint', 'TLazNotifyEvent', @_LapeCustomControl_OnPaint_Read, @_LapeCustomControl_OnPaint_Write);

    addClass('TLazControlScrollBar');
    addClassConstructor('TLazControlScrollBar' ,'(AControl: TLazWinControl; AKind: ELazScrollBarKind)', @_LapeControlScrollBar_Create);
    addGlobalFunc('function TLazControlScrollBar.GetOtherScrollBar: TLazControlScrollBar;', @_LapeControlScrollBar_GetOtherScrollBar);
    addProperty('TLazControlScrollBar', 'Size', 'Integer', @_LapeControlScrollBar_Size_Read);
    addGlobalFunc('function TLazControlScrollBar.ClientSize: Integer;', @_LapeControlScrollBar_ClientSize);
    addGlobalFunc('function TLazControlScrollBar.ClientSizeWithBar: Integer;', @_LapeControlScrollBar_ClientSizeWithBar);
    addGlobalFunc('function TLazControlScrollBar.ClientSizeWithoutBar: Integer;', @_LapeControlScrollBar_ClientSizeWithoutBar);
    addProperty('TLazControlScrollBar', 'Increment', 'Integer', @_LapeControlScrollBar_Increment_Read, @_LapeControlScrollBar_Increment_Write);
    addProperty('TLazControlScrollBar', 'Page', 'Integer', @_LapeControlScrollBar_Page_Read, @_LapeControlScrollBar_Page_Write);
    addProperty('TLazControlScrollBar', 'Smooth', 'Boolean', @_LapeControlScrollBar_Smooth_Read, @_LapeControlScrollBar_Smooth_Write);
    addProperty('TLazControlScrollBar', 'Position', 'Integer', @_LapeControlScrollBar_Position_Read, @_LapeControlScrollBar_Position_Write);
    addProperty('TLazControlScrollBar', 'Range', 'Integer', @_LapeControlScrollBar_Range_Read, @_LapeControlScrollBar_Range_Write);
    addProperty('TLazControlScrollBar', 'Tracking', 'Boolean', @_LapeControlScrollBar_Tracking_Read, @_LapeControlScrollBar_Tracking_Write);
    addProperty('TLazControlScrollBar', 'Visible', 'Boolean', @_LapeControlScrollBar_Visible_Read, @_LapeControlScrollBar_Visible_Write);

    addClass('TLazScrollingWinControl', 'TLazCustomControl');
    addClassConstructor('TLazScrollingWinControl', '(AOwner: TLazComponent)', @_LapeScrollingWinControl_Create);
    addGlobalFunc('procedure TLazScrollingWinControl.UpdateScrollbars;', @_LapeScrollingWinControl_UpdateScrollbars);
    addProperty('TLazScrollingWinControl', 'HorzScrollBar', 'TLazControlScrollBar', @_LapeScrollingWinControl_HorzScrollBar_Read, @_LapeScrollingWinControl_HorzScrollBar_Write);
    addProperty('TLazScrollingWinControl', 'VertScrollBar', 'TLazControlScrollBar', @_LapeScrollingWinControl_VertScrollBar_Read, @_LapeScrollingWinControl_VertScrollBar_Write);

    addClass('TLazGraphicControl', 'TLazControl');
    addProperty('TLazGraphicControl', 'Canvas', 'TLazCanvas', @_LapeGraphicControl_Canvas_Read);
  end;
end;

end.

