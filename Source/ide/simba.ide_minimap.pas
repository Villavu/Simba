{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_minimap;

{$i simba.inc}

interface

uses
  Types, Classes, SysUtils, Controls, ExtCtrls, Graphics,
  SynEdit, SynEditTypes, SynEditMiscClasses, LazSynEditText,
  simba.component_synedit,
  simba.settings;

const
  DefaultFloatGap        = 3;
  MinimapSummonTolerance = 10;

type
  TMinimapControl = class(TPanel)
  private
    FMiniSynEdit: TSimbaSynEdit;
    FSourceSynEdit: TSimbaSynEdit;
    FSyncingFold: Boolean;
    FEnabled: Boolean;
    FDocked: Boolean;
    FFloatWidth: Integer;
    FFloatGap: Integer;
    FDragging: Boolean;
    FGrabOffset: Integer;

    procedure ConfigMiniEdit;
    procedure AttachEditor;
    procedure DetachEditor;
    procedure SetSummonHooks(AEnable: Boolean);
    procedure SetDocked(AValue: Boolean);
    procedure ApplyLayout;
    procedure ShowFloating;
    function MouseInFloatArea: Boolean;
    function MouseNearScrollbar: Boolean;
    procedure PollFloat;
    procedure DetachFromPoller;
    procedure UpdatePoller;
    procedure SyncViewWindow;
    procedure DragScrollTo(Y: Integer);
    function MiniLineHeight: Integer;

    procedure DoSettingChange_Enabled(Setting: TSimbaSetting);
    procedure DoSettingChange_Docked(Setting: TSimbaSetting);
    procedure DoSettingChange_FontSize(Setting: TSimbaSetting);
    procedure DoSettingChange_Width(Setting: TSimbaSetting);

    // Draw the current visible area on the minimap
    procedure HandleLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
    // Zero SynEdit's hardcoded 1px left padding so markup spans the full width
    procedure HandleMiniBeforePaint({%H-}Sender: TObject; {%H-}EventType: TSynPaintEvent; const {%H-}rcClip: TRect);
    procedure HandleStatusChange(Sender: TObject; {%H-}Changes: TSynStatusChanges);
    // Mirror fold state exactly
    procedure HandleSourceFoldChanged(Sender: TSynEditStrings; {%H-}aIndex, {%H-}aCount: Integer);
    // Forward scroll wheel to editor
    procedure HandleMiniMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
    // Drag scrolling
    procedure HandleMiniMouseDown(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure HandleMiniMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    procedure HandleMiniMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    // Vert scrollbar: Float on mouse enter if style=floating
    procedure HandleScrollbarEnter({%H-}Sender: TObject);
    // Editor mouse-move: float when the cursor comes within tolerance of the scrollbar
    procedure HandleSourceMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent; AEditor: TSimbaSynEdit); reintroduce;
    destructor Destroy; override;

    // Docked: always visible on the right.
    // Not docked: floating while scrollbar/minimap is hovered.
    property Docked: Boolean read FDocked write SetDocked default True;
    property FloatWidth: Integer read FFloatWidth write FFloatWidth;
    property FloatGap: Integer read FFloatGap write FFloatGap default DefaultFloatGap;
  end;

implementation

uses
  ATScrollBar,
  SynEditFoldedView,
  SynEditMarkupFoldColoring,
  simba.component_theme,
  simba.initializations;

type
  TMinimapFloatPoller = class
  private
    FTimer: TTimer;
    FList: TList;

    procedure DoTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Attach(AMinimap: TMinimapControl);
    procedure Detach(AMinimap: TMinimapControl);
  end;

var
  FloatPoller: TMinimapFloatPoller = nil;

constructor TMinimapFloatPoller.Create;
begin
  inherited Create();

  FList := TList.Create();

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := @DoTimer;
end;

destructor TMinimapFloatPoller.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FList);

  inherited Destroy;
end;

procedure TMinimapFloatPoller.DoTimer(Sender: TObject);
var
  I: Integer;
begin
  for I := FList.Count - 1 downto 0 do
    TMinimapControl(FList[I]).PollFloat();
  if (FList.Count = 0) then
    FTimer.Enabled := False;
end;

procedure TMinimapFloatPoller.Attach(AMinimap: TMinimapControl);
begin
  if (FList.IndexOf(AMinimap) < 0) then
    FList.Add(AMinimap);
  FTimer.Enabled := True;
end;

procedure TMinimapFloatPoller.Detach(AMinimap: TMinimapControl);
begin
  FList.Remove(AMinimap);
  if (FList.Count = 0) then
    FTimer.Enabled := False;
end;

procedure TMinimapControl.HandleLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
var
  TopLine, BottomLine: Integer;
begin
  TopLine := FSourceSynEdit.TopLine;

  // ScreenXYToTextXY is fold-aware
  BottomLine := FSourceSynEdit.ScreenXYToTextXY(Point(1, FSourceSynEdit.LinesInWindow - 1)).Y;
  if (Line >= TopLine) and (Line <= BottomLine) then
  begin
    Markup.Background := SimbaComponentTheme.ColorLine;
    Markup.BackAlpha := 80;
    Markup.Foreground := clNone;

    Special := True;
  end;
end;

procedure TMinimapControl.HandleMiniBeforePaint(Sender: TObject; EventType: TSynPaintEvent; const rcClip: TRect);
begin
  // With gutter hidden SynEdit hardcodes PaintArea.Padding[bsLeft] := 1
  // we dont want that else the visible rect area has a 1px offset.
  if (FMiniSynEdit.TextArea.Padding[bsLeft] <> 0) then
    FMiniSynEdit.TextArea.Padding[bsLeft] := 0;
end;

procedure TMinimapControl.HandleStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  SyncViewWindow();
end;

procedure TMinimapControl.HandleSourceFoldChanged(Sender: TSynEditStrings; aIndex, aCount: Integer);
begin
  if FSyncingFold or (not Assigned(FSourceSynEdit)) then
    Exit;

  FSyncingFold := True;
  try
    FMiniSynEdit.FoldView.UnfoldAll();
    FMiniSynEdit.FoldState := FSourceSynEdit.FoldState;
  finally
    FSyncingFold := False;
  end;
end;

function TMinimapControl.MiniLineHeight: Integer;
begin
  Result := FMiniSynEdit.LineHeight;
  if (Result < 1) then
    Result := 1;
end;

procedure TMinimapControl.DragScrollTo(Y: Integer);
var
  ThumbH, TrackH: Integer;
  MinTop, MaxTop: Int64;
  Frac: Double;
begin
  ThumbH := FSourceSynEdit.LinesInWindow * MiniLineHeight;
  TrackH := FMiniSynEdit.ClientHeight - ThumbH;

  MinTop := FSourceSynEdit.ScrollbarVert.Min;
  MaxTop := FSourceSynEdit.ScrollbarVert.Max - FSourceSynEdit.ScrollbarVert.PageSize;
  if (MaxTop < MinTop) then
    MaxTop := MinTop;

  if (TrackH > 0) then
    Frac := (Y - FGrabOffset) / TrackH
  else
    Frac := 0;
  if (Frac < 0) then Frac := 0;
  if (Frac > 1) then Frac := 1;

  FSourceSynEdit.TopView := MinTop + Round(Frac * (MaxTop - MinTop));
end;

procedure TMinimapControl.HandleMiniMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FSourceSynEdit.ScrollbarVert.WheelScroll(WheelDelta);
  Handled := True;
end;

procedure TMinimapControl.HandleMiniMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LineHeight, ThumbTop, ThumbH: Integer;
begin
  if (Button <> mbLeft) then
    Exit;
  FDragging := True;

  LineHeight := MiniLineHeight;
  ThumbTop := (FSourceSynEdit.TopView - FMiniSynEdit.TopView) * LineHeight;
  ThumbH   := FSourceSynEdit.LinesInWindow * LineHeight;

  if (Y >= ThumbTop) and (Y < ThumbTop + ThumbH) then
    FGrabOffset := Y - ThumbTop // keep that point under the cursor
  else
    FGrabOffset := ThumbH div 2; // jump so the thumb centres on the click

  DragScrollTo(Y);
end;

procedure TMinimapControl.HandleMiniMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
    DragScrollTo(Y);
end;

procedure TMinimapControl.HandleMiniMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := False;
end;

procedure TMinimapControl.SyncViewWindow;
var
  SrcTop, SrcLines, MiniLines, MinTop, MaxTop, Slack: Int64;
  Frac: Double;
begin
  SrcTop    := FSourceSynEdit.TopView;
  SrcLines  := FSourceSynEdit.LinesInWindow;
  MiniLines := FMiniSynEdit.LinesInWindow;
  MinTop    := FSourceSynEdit.ScrollbarVert.Min;
  MaxTop    := FSourceSynEdit.ScrollbarVert.Max - FSourceSynEdit.ScrollbarVert.PageSize;

  Slack := MiniLines - SrcLines;
  if (Slack > 0) and (MaxTop > MinTop) then
    Frac := (SrcTop - MinTop) / (MaxTop - MinTop)
  else
    Frac := 0;

  FMiniSynEdit.TopView := Round(SrcTop - Frac * Slack);
  FMiniSynEdit.Invalidate;
end;

procedure TMinimapControl.DoSettingChange_Enabled(Setting: TSimbaSetting);
begin
  FEnabled := Setting.Value;
  if not FEnabled then
    Visible := False;
end;

procedure TMinimapControl.DoSettingChange_Docked(Setting: TSimbaSetting);
var
  NewDocked: Boolean;
begin
  NewDocked := Setting.Value;
  if (FDocked = NewDocked) then
    ApplyLayout()
  else
    Docked := NewDocked;
end;

procedure TMinimapControl.DoSettingChange_FontSize(Setting: TSimbaSetting);
begin
  FMiniSynEdit.Font.Size := Setting.Value;
  SyncViewWindow();
end;

procedure TMinimapControl.DoSettingChange_Width(Setting: TSimbaSetting);
begin
  FFloatWidth := Setting.Value;
  Width := FFloatWidth;

  if (not FDocked) and Visible then
    ShowFloating();
end;

procedure TMinimapControl.SetDocked(AValue: Boolean);
begin
  if FDocked = AValue then Exit;
  FDocked := AValue;
  ApplyLayout;
end;

procedure TMinimapControl.ApplyLayout;
begin
  if FDocked then
  begin
    SetSummonHooks(False);
    FMiniSynEdit.BorderSpacing.Left := 0;
    Align := alRight;
    Visible := True;
  end else
  begin
    Align := alNone;
    Visible := False;
    FMiniSynEdit.BorderSpacing.Left := FFloatGap;
    SetSummonHooks(True);
  end;

  UpdatePoller();
end;

procedure TMinimapControl.ShowFloating;
begin
  SetBounds(
    FSourceSynEdit.Left + FSourceSynEdit.Width - FFloatWidth,
    FSourceSynEdit.Top,
    FFloatWidth,
    FSourceSynEdit.Height
  );
  Visible := True;
  BringToFront();
  SyncViewWindow();
end;

function TMinimapControl.MouseInFloatArea: Boolean;
var
  P: TPoint;

  function InControl(C: TControl): Boolean;
  var
    R: TRect;
  begin
    Result := Assigned(C) and C.Visible;
    if Result then
    begin
      R.TopLeft := C.ControlToScreen(Point(0, 0));
      R.BottomRight := C.ControlToScreen(Point(C.Width, C.Height));
      Result := PtInRect(R, P);
    end;
  end;

begin
  P := Mouse.CursorPos;
  Result := InControl(Self) or MouseNearScrollbar;
end;

function TMinimapControl.MouseNearScrollbar: Boolean;
var
  Bar: TControl;
  R: TRect;
begin
  Result := False;
  if not Assigned(FSourceSynEdit) then
    Exit;
  Bar := FSourceSynEdit.ScrollbarVert;
  if not (Assigned(Bar) and Bar.Visible) then
    Exit;

  R.TopLeft     := Bar.ControlToScreen(Point(-Scale96ToScreen(MinimapSummonTolerance), 0));
  R.BottomRight := Bar.ControlToScreen(Point(Bar.Width, Bar.Height));
  Result := PtInRect(R, Mouse.CursorPos);
end;

procedure TMinimapControl.HandleScrollbarEnter(Sender: TObject);
begin
  if (not FDocked) and FEnabled and (not Visible) then
    ShowFloating();
end;

procedure TMinimapControl.HandleSourceMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (not FDocked) and FEnabled and (not Visible) and MouseNearScrollbar() then
    ShowFloating();
end;

procedure TMinimapControl.PollFloat;
begin
  if FDragging then
    Exit;
  if not MouseInFloatArea then
    Visible := False;
end;

procedure TMinimapControl.DetachFromPoller;
begin
  if Assigned(FloatPoller) then
    FloatPoller.Detach(Self);
end;

procedure TMinimapControl.UpdatePoller;
begin
  if Visible and (not FDocked) and Assigned(FSourceSynEdit) then
    FloatPoller.Attach(Self)
  else
    DetachFromPoller();
end;

procedure TMinimapControl.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  UpdatePoller();
end;

procedure TMinimapControl.SetSummonHooks(AEnable: Boolean);
var
  Bar: TControl;
begin
  if not Assigned(FSourceSynEdit) then
    Exit;

  if AEnable then
    FSourceSynEdit.OnMouseMove := @HandleSourceMouseMove
  else
    FSourceSynEdit.OnMouseMove := nil;

  Bar := FSourceSynEdit.ScrollbarVert;
  if (Bar is TATScrollbar) then
  begin
    if AEnable then
      TATScrollbar(Bar).OnMouseEnter := @HandleScrollbarEnter
    else
      TATScrollbar(Bar).OnMouseEnter := nil;
  end;
end;

procedure TMinimapControl.ConfigMiniEdit;
begin
  With FMiniSynEdit do
  begin
    Align := alClient;
    ParentColor := False;
    ParentFont := False;
    ShowVertScroll := False;
    ShowHorzScroll := False;
    Gutter.Visible := False;
    RightGutter.Visible := False;
    ReadOnly := True;
    Keystrokes.Clear;
    Options := [eoNoCaret, eoNoSelection];
    Options2 := [];

    OnMouseWheel := @HandleMiniMouseWheel;
    OnMouseDown := @HandleMiniMouseDown;
    OnMouseMove := @HandleMiniMouseMove;
    OnMouseUp := @HandleMiniMouseUp;
    OnSpecialLineMarkup := @HandleLineMarkup;
    RegisterPaintEventHandler(@HandleMiniBeforePaint, [peBeforePaint]);

    MarkupByClass[TSynEditMarkupFoldColors].Enabled := False;
  end;
end;

procedure TMinimapControl.AttachEditor;
begin
  With FMiniSynEdit do
  begin
    Font := FSourceSynEdit.Font;
    ShareTextBufferFrom(FSourceSynEdit);
    Highlighter := FSourceSynEdit.Highlighter;
    RightEdge := FSourceSynEdit.RightEdge;
    RightEdgeColor := FSourceSynEdit.RightEdgeColor;
    Color := SimbaComponentTheme.ColorFrame;
  end;

  FSourceSynEdit.RegisterStatusChangedHandler(@HandleStatusChange, [scTopLine, scLinesInWindow]);
  FSourceSynEdit.FoldView.AddChangeHandler(senrLineMappingChanged, @HandleSourceFoldChanged);
  FMiniSynEdit.FoldState := FSourceSynEdit.FoldState;

  SyncViewWindow;
end;

procedure TMinimapControl.DetachEditor;
begin
  if not Assigned(FSourceSynEdit) then
    Exit;

  SetSummonHooks(False);
  FSourceSynEdit.FoldView.RemoveChangeHandler(senrLineMappingChanged, @HandleSourceFoldChanged);
  FSourceSynEdit.UnRegisterStatusChangedHandler(@HandleStatusChange);
  FMiniSynEdit.UnShareTextBuffer;
  FSourceSynEdit := nil;
end;

constructor TMinimapControl.Create(AOwner: TComponent; AEditor: TSimbaSynEdit);
begin
  inherited Create(AOwner);

  BevelInner := bvNone;
  BevelOuter := bvNone;
  Color := SimbaComponentTheme.ColorFrame;
  FDocked := True;
  FFloatGap := DefaultFloatGap;
  FEnabled := True;

  FMiniSynEdit := TSimbaSynEdit.Create(Self, nil);
  FMiniSynEdit.Parent := Self;
  ConfigMiniEdit();

  FSourceSynEdit := AEditor;
  AttachEditor();

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.Minimap.FontSize, @DoSettingChange_FontSize, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.Minimap.Width, @DoSettingChange_Width, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.Minimap.Docked, @DoSettingChange_Docked, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.Minimap.Enabled, @DoSettingChange_Enabled, True);
end;

destructor TMinimapControl.Destroy;
begin
  DetachFromPoller();
  DetachEditor();

  inherited Destroy();
end;

procedure DoCreate;
begin
  FloatPoller := TMinimapFloatPoller.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(FloatPoller);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'MinimapFloatPoller');

finalization
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'MinimapFloatPoller');

end.
