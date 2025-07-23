unit simba.import_lcl_extctrls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportLCLExtCtrls(Script: TSimbaScript);

implementation

uses
  controls, extctrls, graphics, customtimer, lptypes, ffi, math;

type
  PAlignment = ^TAlignment;
  PNotifyEvent = ^TNotifyEvent;
  PBevelWidth = ^TBevelWidth;
  PCustomPanel = ^TCustomPanel;
  PImage = ^TImage;
  PPanel = ^TPanel;
  PPanelBevel = ^TPanelBevel;
  PTimer = ^TTimer;
  PComponent = ^TComponent;
  PRect = ^TRect;
  PPicture = ^TPicture;
  PMouseEvent = ^TMouseEvent;
  PMouseMoveEvent = ^TMouseMoveEvent;

procedure _LapeTimer_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Result)^ := TTimer.Create(PComponent(Params^[0])^);
end;

procedure _LapeTimer_Enabled_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTimer(Params^[0])^.Enabled;
end;

procedure _LapeTimer_Enabled_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^.Enabled := PBoolean(Params^[1])^;
end;

procedure _LapeTimer_Interval_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCardinal(Result)^ := PTimer(Params^[0])^.Interval;
end;

procedure _LapeTimer_Interval_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^.Interval := PCardinal(Params^[1])^;
end;

procedure _LapeTimer_OnTimer_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTimer(Params^[0])^.OnTimer;
end;

procedure _LapeTimer_OnTimer_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^.OnTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeTimer_OnStartTimer_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTimer(Params^[0])^.OnStartTimer;
end;

procedure _LapeTimer_OnStartTimer_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^.OnStartTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeTimer_OnStopTimer_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTimer(Params^[0])^.OnStopTimer;
end;

procedure _LapeTimer_OnStopTimer_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimer(Params^[0])^.OnStopTimer := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Result)^ := TImage.Create(PComponent(Params^[0])^);
end;

procedure _LapeImage_DestRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PImage(Params^[0])^.DestRect();
end;

procedure _LapeImage_Center_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PImage(Params^[0])^.Center;
end;

procedure _LapeImage_Center_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.Center := PBoolean(Params^[1])^;
end;

procedure _LapeImage_Picture_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPicture(Result)^ := PImage(Params^[0])^.Picture;
end;

procedure _LapeImage_Picture_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.Picture := PPicture(Params^[1])^;
end;

procedure _LapeImage_Stretch_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PImage(Params^[0])^.Stretch;
end;

procedure _LapeImage_Stretch_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.Stretch := PBoolean(Params^[1])^;
end;

procedure _LapeImage_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PImage(Params^[0])^.Transparent;
end;

procedure _LapeImage_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeImage_Proportional_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PImage(Params^[0])^.Proportional;
end;

procedure _LapeImage_Proportional_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.Proportional := PBoolean(Params^[1])^;
end;

procedure _LapeImage_OnPictureChanged_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnPictureChanged;
end;

procedure _LapeImage_OnPictureChanged_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnPictureChanged := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PImage(Params^[0])^.OnMouseDown;
end;

procedure _LapeImage_OnMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PImage(Params^[0])^.OnMouseUp;
end;

procedure _LapeImage_OnMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseEnter;
end;

procedure _LapeImage_OnMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnMouseLeave;
end;

procedure _LapeImage_OnMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeImage_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseMoveEvent(Result)^ := PImage(Params^[0])^.OnMouseMove;
end;

procedure _LapeImage_OnMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeImage_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PImage(Params^[0])^.OnDblClick;
end;

procedure _LapeImage_OnDblClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImage(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomPanel_Alignment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAlignment(Result)^ := PCustomPanel(Params^[0])^.Alignment;
end;

procedure _LapeCustomPanel_Alignment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelInner_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelInner;
end;

procedure _LapeCustomPanel_BevelInner_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.BevelInner := PPanelBevel(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelOuter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanelBevel(Result)^ := PCustomPanel(Params^[0])^.BevelOuter;
end;

procedure _LapeCustomPanel_BevelOuter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.BevelOuter := PPanelBevel(Params^[1])^;
end;

procedure _LapeCustomPanel_BevelWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBevelWidth(Result)^ := PCustomPanel(Params^[0])^.BevelWidth;
end;

procedure _LapeCustomPanel_BevelWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Params^[0])^.BevelWidth := PBevelWidth(Params^[1])^;
end;

procedure _LapeCustomPanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomPanel(Result)^ := TCustomPanel.Create(PComponent(Params^[0])^);
end;

procedure _LapePanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Result)^ := TPanel.Create(PComponent(Params^[0])^);
end;

procedure _LapePanel_WordWrap_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

procedure _LapePanel_WordWrap_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PPanel(Params^[0])^.WordWrap;
end;

procedure _LapePanel_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PPanel(Params^[0])^.OnMouseDown;
end;

procedure _LapePanel_OnMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapePanel_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PPanel(Params^[0])^.OnMouseUp;
end;

procedure _LapePanel_OnMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapePanel_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPanel(Params^[0])^.OnMouseEnter;
end;

procedure _LapePanel_OnMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapePanel_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPanel(Params^[0])^.OnMouseLeave;
end;

procedure _LapePanel_OnMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapePanel_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseMoveEvent(Result)^ := PPanel(Params^[0])^.OnMouseMove;
end;

procedure _LapePanel_OnMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapePanel_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPanel(Params^[0])^.OnDblClick;
end;

procedure _LapePanel_OnDblClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

type
  PDragablePanel = ^TDragablePanel;
  TDragablePanel = class(TPanel)
  protected type
    {$scopedenums on}
    EDragEdge = (
      NONE,
      LEFT, RIGHT, TOP, BOTTOM,
      TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT
    );
    {$scopedenums off}
  protected
    FDownEdge: EDragEdge;
    FDownX: Integer;
    FDownY: Integer;
    FDown: Boolean;

    FMouseEdgeSize: Integer;
    FAllowMoving: Boolean;
    FAllowResizing: Boolean;

    function GetDragEdge(X, Y: Integer): EDragEdge;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(TheOwner: TComponent); override;

    property MouseEdgeSize: Integer read FMouseEdgeSize write FMouseEdgeSize;
    property AllowMoving: Boolean read FAllowMoving write FAllowMoving;
    property AllowResizing: Boolean read FAllowResizing write FAllowResizing;
  end;

function TDragablePanel.GetDragEdge(X, Y: Integer): EDragEdge;

  function DistToCorner(CornerX, CornerY: Integer): Integer;
  begin
    Result := Round(Hypot(CornerX - X, CornerY - Y));
  end;

begin
  Result := EDragEdge.NONE;

  with ClientRect do
  begin
    if (DistToCorner(Left,  Top) < FMouseEdgeSize+2)    then Exit(EDragEdge.TOP_LEFT);
    if (DistToCorner(Right, Top) < FMouseEdgeSize+2)    then Exit(EDragEdge.TOP_RIGHT);
    if (DistToCorner(Left,  Bottom) < FMouseEdgeSize+2) then Exit(EDragEdge.BOTTOM_LEFT);
    if (DistToCorner(Right, Bottom) < FMouseEdgeSize+2) then Exit(EDragEdge.BOTTOM_RIGHT);

    if (X < FMouseEdgeSize)          then Exit(EDragEdge.LEFT);
    if (Y < FMouseEdgeSize)          then Exit(EDragEdge.TOP);
    if (X > Width - FMouseEdgeSize)  then Exit(EDragEdge.RIGHT);
    if (Y > Height - FMouseEdgeSize) then Exit(EDragEdge.BOTTOM);
  end;
end;

procedure TDragablePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  MouseXY: TPoint;
  NewRect: TRect;
begin
  inherited MouseMove(Shift, X, Y);

  if (not FDown) then
  begin
    case GetDragEdge(X, Y) of
      EDragEdge.LEFT:   Cursor := crSizeE;
      EDragEdge.TOP:    Cursor := crSizeN;
      EDragEdge.RIGHT:  Cursor := crSizeW;
      EDragEdge.BOTTOM: Cursor := crSizeS;

      EDragEdge.TOP_LEFT:     Cursor := crSizeNW;
      EDragEdge.TOP_RIGHT:    Cursor := crSizeNE;
      EDragEdge.BOTTOM_LEFT:  Cursor := crSizeSW;
      EDragEdge.BOTTOM_RIGHT: Cursor := crSizeSE;
      else
        Cursor := crSize;
    end;
    Exit;
  end;

  if (FDownEdge = EDragEdge.NONE) and (not AllowMoving)    then Exit;
  if (FDownEdge <> EDragEdge.NONE) and (not AllowResizing) then Exit;

  MouseXY := Parent.ScreenToClient(Mouse.CursorPos);

  NewRect := BoundsRect;
  case FDownEdge of
    EDragEdge.LEFT:   NewRect.Left   := MouseXY.X;
    EDragEdge.RIGHT:  NewRect.Right  := MouseXY.X;
    EDragEdge.TOP:    NewRect.Top    := MouseXY.Y;
    EDragEdge.BOTTOM: NewRect.Bottom := MouseXY.Y;

    EDragEdge.TOP_LEFT:     begin NewRect.Left  := MouseXY.X; NewRect.Top := MouseXY.Y;    end;
    EDragEdge.TOP_RIGHT:    begin NewRect.Right := MouseXY.X; NewRect.Top := MouseXY.Y;    end;
    EDragEdge.BOTTOM_LEFT:  begin NewRect.Left  := MouseXY.X; NewRect.Bottom := MouseXY.Y; end;
    EDragEdge.BOTTOM_RIGHT: begin NewRect.Right := MouseXY.X; NewRect.Bottom := MouseXY.Y; end;
    else
      NewRect.SetLocation(MouseXY.X - FDownX, MouseXY.Y - FDownY);
  end;
  BoundsRect := NewRect;
end;

procedure TDragablePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  FDownEdge := GetDragEdge(X, Y);
  FDownX := X;
  FDownY := Y;
  FDown := True;
end;

procedure TDragablePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  FDown := False;
end;

constructor TDragablePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FMouseEdgeSize := 5;
  FAllowMoving := True;
  FAllowResizing := True;

  BevelOuter := bvNone;
  Constraints.MinWidth := 10;
  Constraints.MinHeight := 10;
end;

procedure _LapeDragablePanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDragablePanel(Result)^ := TDragablePanel.Create(PComponent(Params^[0])^);
end;

procedure _LapeDragablePanel_MouseEdgeSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDragablePanel(Params^[0])^.MouseEdgeSize := PInteger(Params^[1])^;
end;

procedure _LapeDragablePanel_MouseEdgeSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PDragablePanel(Params^[0])^.MouseEdgeSize;
end;

procedure _LapeDragablePanel_AllowMoving_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDragablePanel(Params^[0])^.AllowMoving := PBoolean(Params^[1])^;
end;

procedure _LapeDragablePanel_AllowMoving_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDragablePanel(Params^[0])^.AllowMoving;
end;

procedure _LapeDragablePanel_AllowResizing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDragablePanel(Params^[0])^.AllowResizing := PBoolean(Params^[1])^;
end;

procedure _LapeDragablePanel_AllowResizing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDragablePanel(Params^[0])^.AllowResizing;
end;

procedure ImportLCLExtCtrls(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addGlobalType('enum(None, Lowered, Raised, Space)', 'ELazPanelBevel');

    addClass('TLazTimer', 'TLazComponent', TTimer);
    addClassConstructor('TLazTimer', '(Owner: TLazComponent)', @_LapeTimer_Create);
    addProperty('TLazTimer', 'Enabled', 'Boolean', @_LapeTimer_Enabled_Read, @_LapeTimer_Enabled_Write);
    addProperty('TLazTimer', 'Interval', 'UInt32', @_LapeTimer_Interval_Read, @_LapeTimer_Interval_Write);
    addProperty('TLazTimer', 'OnTimer', 'TLazNotifyEvent', @_LapeTimer_OnTimer_Read, @_LapeTimer_OnTimer_Write);
    addProperty('TLazTimer', 'OnStartTimer', 'TLazNotifyEvent', @_LapeTimer_OnStartTimer_Read, @_LapeTimer_OnStartTimer_Write);
    addProperty('TLazTimer', 'OnStopTimer', 'TLazNotifyEvent', @_LapeTimer_OnStopTimer_Read, @_LapeTimer_OnStopTimer_Write);

    addClass('TLazImage', 'TLazGraphicControl', TImage);
    addClassConstructor('TLazImage', '(Owner: TLazComponent)', @_LapeImage_Create);
    addGlobalFunc('function TLazImage.DestRect: TLazRect;', @_LapeImage_DestRect);
    addProperty('TLazImage', 'Center', 'Boolean', @_LapeImage_Center_Read, @_LapeImage_Center_Write);
    addProperty('TLazImage', 'Picture', 'TLazPicture', @_LapeImage_Picture_Read, @_LapeImage_Picture_Write);
    addProperty('TLazImage', 'Stretch', 'Boolean', @_LapeImage_Stretch_Read, @_LapeImage_Stretch_Write);
    addProperty('TLazImage', 'Transparent', 'Boolean', @_LapeImage_Transparent_Read, @_LapeImage_Transparent_Write);
    addProperty('TLazImage', 'Proportional', 'Boolean', @_LapeImage_Proportional_Read, @_LapeImage_Proportional_Write);
    addProperty('TLazImage', 'OnPictureChanged', 'TLazNotifyEvent', @_LapeImage_OnPictureChanged_Read, @_LapeImage_OnPictureChanged_Write);
    addProperty('TLazImage', 'OnMouseDown', 'TLazMouseEvent', @_LapeImage_OnMouseDown_Read, @_LapeImage_OnMouseDown_Write);
    addProperty('TLazImage', 'OnMouseUp', 'TLazMouseEvent', @_LapeImage_OnMouseUp_Read, @_LapeImage_OnMouseUp_Write);
    addProperty('TLazImage', 'OnMouseEnter', 'TLazNotifyEvent', @_LapeImage_OnMouseEnter_Read, @_LapeImage_OnMouseEnter_Write);
    addProperty('TLazImage', 'OnMouseLeave', 'TLazNotifyEvent', @_LapeImage_OnMouseLeave_Read, @_LapeImage_OnMouseLeave_Write);
    addProperty('TLazImage', 'OnMouseMove', 'TLazMouseMoveEvent', @_LapeImage_OnMouseMove_Read, @_LapeImage_OnMouseMove_Write);
    addProperty('TLazImage', 'OnDblClick', 'TLazNotifyEvent', @_LapeImage_OnDblClick_Read, @_LapeImage_OnDblClick_Write);

    addClass('TLazCustomPanel', 'TLazCustomControl', TCustomPanel);
    addProperty('TLazCustomPanel', 'Alignment', 'ELazAlignment', @_LapeCustomPanel_Alignment_Read, @_LapeCustomPanel_Alignment_Write);
    addProperty('TLazCustomPanel', 'BevelInner', 'ELazPanelBevel', @_LapeCustomPanel_BevelInner_Read, @_LapeCustomPanel_BevelInner_Write);
    addProperty('TLazCustomPanel', 'BevelOuter', 'ELazPanelBevel', @_LapeCustomPanel_BevelOuter_Read, @_LapeCustomPanel_BevelOuter_Write);
    addProperty('TLazCustomPanel', 'BevelWidth', 'Integer', @_LapeCustomPanel_BevelWidth_Read, @_LapeCustomPanel_BevelWidth_Write);
    addClassConstructor('TLazCustomPanel', '(Owner: TLazComponent)', @_LapeCustomPanel_Create);

    addClass('TLazPanel', 'TLazCustomPanel', TPanel);
    addClassConstructor('TLazPanel', '(Owner: TLazComponent)', @_LapePanel_Create);

    addProperty('TLazPanel', 'WordWrap', 'Boolean', @_LapePanel_WordWrap_Read, @_LapePanel_WordWrap_Write);
    addProperty('TLazPanel', 'OnMouseDown', 'TLazMouseEvent', @_LapePanel_OnMouseDown_Read, @_LapePanel_OnMouseDown_Write);
    addProperty('TLazPanel', 'OnMouseUp', 'TLazMouseEvent', @_LapePanel_OnMouseUp_Read, @_LapePanel_OnMouseUp_Write);
    addProperty('TLazPanel', 'OnMouseEnter', 'TLazNotifyEvent', @_LapePanel_OnMouseEnter_Read, @_LapePanel_OnMouseEnter_Write);
    addProperty('TLazPanel', 'OnMouseLeave', 'TLazNotifyEvent', @_LapePanel_OnMouseLeave_Read, @_LapePanel_OnMouseLeave_Write);
    addProperty('TLazPanel', 'OnMouseMove', 'TLazMouseMoveEvent', @_LapePanel_OnMouseMove_Read, @_LapePanel_OnMouseMove_Write);
    addProperty('TLazPanel', 'OnDblClick', 'TLazNotifyEvent', @_LapePanel_OnDblClick_Read, @_LapePanel_OnDblClick_Write);

    addClass('TLazDragablePanel', 'TLazPanel', TDragablePanel);
    addClassConstructor('TLazDragablePanel', '(Owner: TLazComponent)', @_LapeDragablePanel_Create);
    addProperty('TLazDragablePanel', 'MouseEdgeSize', 'Integer', @_LapeDragablePanel_MouseEdgeSize_Read, @_LapeDragablePanel_MouseEdgeSize_Write);
    addProperty('TLazDragablePanel', 'AllowMoving', 'Boolean', @_LapeDragablePanel_AllowMoving_Read, @_LapeDragablePanel_AllowMoving_Write);
    addProperty('TLazDragablePanel', 'AllowResizing', 'Boolean', @_LapeDragablePanel_AllowResizing_Read, @_LapeDragablePanel_AllowResizing_Write);
  end;
end;


end.

