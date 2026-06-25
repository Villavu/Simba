{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_synedit;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, Graphics,
  LazSynEditText,
  SynEdit,
  SynEditTypes,
  SynEditFoldedView,
  SynEditTextBuffer,
  SynEditHighlighter,
  {%H-}SynEditWrappedView,
  simba.component_scrollbar,
  simba.component_syneditstyler;

type
  TSimbaSynEdit = class(TSynEdit)
  protected
    FFoldView: TSynEditFoldedView;
    FStyler: TSimbaSynEditStyler;
    FScrollbarVert: TSimbaScrollBar;
    FScrollbarHorz: TSimbaScrollBar;
    FShowVertScroll: Boolean;
    FShowHorzScroll: Boolean;
    FAllowVertScroll: Boolean;
    FAllowHorzScroll: Boolean;

    procedure DoVertScrollBarChange(Sender: TObject);
    procedure DoHorzScrollBarChange(Sender: TObject);
    procedure SetShowVertScroll(Value: Boolean);
    procedure SetShowHorzScroll(Value: Boolean);
    procedure SetAllowVertScroll(Value: Boolean);
    procedure SetAllowHorzScroll(Value: Boolean);

    // Override to scroll horizontally when shift + mouse wheel
    // or to do nothing if scrolling is disabled.
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    procedure UpdateBars;
    procedure UpdateScrollbarReserve;
    procedure LayoutScrollbars;
    procedure StatusChanged(AChanges: TSynStatusChanges); override;
    procedure DoLineChanges(Sender: TSynEditStrings; aIndex, aCount: Integer);
    procedure SetParent(NewParent: TWinControl); override;
    procedure Resize; override;
    procedure SetVisible(Value: Boolean); override;

    function GetFontAntialising: Boolean;
    function GetFontName: String;
    procedure SetFontAntialising(Value: Boolean);
    procedure SetFontName(AValue: String);
  public
    constructor Create(AOwner: TComponent; HighlighterClass: TSynCustomHighlighterClass); virtual; reintroduce;

    property FoldView: TSynEditFoldedView read FFoldView;
    property TopView;

    property Styler: TSimbaSynEditStyler read FStyler;
    property FontName: String read GetFontName write SetFontName;
    property FontAntialising: Boolean read GetFontAntialising write SetFontAntialising;

    property ScrollbarVert: TSimbaScrollBar read FScrollbarVert;
    property ScrollbarHorz: TSimbaScrollBar read FScrollbarHorz;
    // Scrollbar visible?
    property ShowVertScroll: Boolean read FShowVertScroll write SetShowVertScroll;
    property ShowHorzScroll: Boolean read FShowHorzScroll write SetShowHorzScroll;
    // Allow scrolling (this includes outside of scrollbars)
    // Will be pinned to the left or top depending on scrollbar.
    property AllowVertScroll: Boolean read FAllowVertScroll write SetAllowVertScroll;
    property AllowHorzScroll: Boolean read FAllowHorzScroll write SetAllowHorzScroll;
  end;

  // Hide gutters and such so the synedit acts more like the "memo" component.
  TSimbaMemo = class(TSimbaSynEdit)
  public
    constructor Create(AOwner: TComponent; LineWrapping: Boolean); virtual; reintroduce;
  end;

implementation

uses
  SynEditMarkupSelection,
  simba.component_theme,
  simba.misc;

function TSimbaSynEdit.GetFontAntialising: Boolean;
begin
  Result := (Font.Quality = fqCleartypeNatural);
end;

procedure TSimbaSynEdit.SetFontAntialising(Value: Boolean);
begin
  case Value of
    True:  Font.Quality := fqCleartypeNatural;
    False: Font.Quality := fqNonAntialiased;
  end;
end;

function TSimbaSynEdit.GetFontName: String;
begin
  Result := Font.Name;
end;

procedure TSimbaSynEdit.SetFontName(AValue: String);
begin
  if IsFontFixed(AValue) then
    Font.Name := AValue;
end;

procedure TSimbaSynEdit.SetShowVertScroll(Value: Boolean);
begin
  if (FShowVertScroll = Value) then
    Exit;
  FShowVertScroll := Value;

  if (FScrollbarVert <> nil) then
  begin
    FScrollbarVert.Visible := FShowVertScroll and Visible;
    UpdateScrollbarReserve();
  end;
end;

procedure TSimbaSynEdit.SetShowHorzScroll(Value: Boolean);
begin
  if (FShowHorzScroll = Value) then
    Exit;
  FShowHorzScroll := Value;

  if (FScrollbarHorz <> nil) then
  begin
    FScrollbarHorz.Visible := FShowHorzScroll and Visible;
    UpdateScrollbarReserve();
  end;
end;

procedure TSimbaSynEdit.SetAllowVertScroll(Value: Boolean);
begin
  if (FAllowVertScroll = Value) then
    Exit;
  FAllowVertScroll := Value;

  if (not FAllowVertScroll) then
    TopView := 1;
  UpdateBars();
end;

procedure TSimbaSynEdit.SetAllowHorzScroll(Value: Boolean);
begin
  if (FAllowHorzScroll = Value) then
    Exit;
  FAllowHorzScroll := Value;

  if (not FAllowHorzScroll) then
    LeftChar := 1;
  UpdateBars();
end;

procedure TSimbaSynEdit.DoVertScrollBarChange(Sender: TObject);
begin
  TopView := FScrollbarVert.Position;
end;

procedure TSimbaSynEdit.DoHorzScrollBarChange(Sender: TObject);
begin
  LeftChar := FScrollbarHorz.Position;
end;

function TSimbaSynEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if FAllowHorzScroll and (ssShift in Shift) then
  begin
    if (WheelDelta > 0) then
      FScrollbarHorz.Position := FScrollbarHorz.Position - 5
    else
      FScrollbarHorz.Position := FScrollbarHorz.Position + 5;

    Result := True;
  end
  else if (not FAllowVertScroll) and (not (ssShift in Shift)) then
    Result := True // vertical scrolling disabled - swallow the wheel
  else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TSimbaSynEdit.UpdateBars;
begin
  if FScrollbarVert=nil then Exit;
  if FScrollbarHorz=nil then Exit;

  if FAllowVertScroll then
  begin
    FScrollbarVert.Min := 1;
    FScrollbarVert.Max := TextView.ViewedCount + 1;
    if (eoScrollPastEof in Options) then
      FScrollbarVert.Max := FScrollbarVert.Max + (LinesInWindow - 1);
    FScrollbarVert.PageSize := LinesInWindow;
    FScrollbarVert.Position := TopView;
  end;

  if FAllowHorzScroll then
  begin
    FScrollbarHorz.Min := 1;
    FScrollbarHorz.Max := TextView.LengthOfLongestLine + 1;
    if (eoScrollPastEol in Options) and (FScrollbarHorz.Max < MaxLeftChar + 1) then
      FScrollbarHorz.Max := MaxLeftChar + 1;
    FScrollbarHorz.PageSize := CharsInWindow;
    FScrollbarHorz.Position := LeftChar;
  end;
end;

procedure TSimbaSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  inherited StatusChanged(AChanges);

  if (not FAllowHorzScroll) and (scLeftChar in AChanges) and (LeftChar <> 1) then
    LeftChar := 1;
  if (not FAllowVertScroll) and (scTopLine in AChanges) and (TopView <> 1) then
    TopView := 1;

  if (AChanges * [scLeftChar, scTopLine, scLinesInWindow, scCharsInWindow] <> []) then
    UpdateBars();
end;

procedure TSimbaSynEdit.DoLineChanges(Sender: TSynEditStrings; aIndex, aCount: Integer);
begin
  UpdateBars();
end;

procedure TSimbaSynEdit.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  FScrollbarVert.Parent := NewParent;
  FScrollbarVert.Anchors := [akLeft, akTop];
  FScrollbarHorz.Parent := NewParent;
  FScrollbarHorz.Anchors := [akLeft, akTop];

  UpdateScrollbarReserve();
end;

procedure TSimbaSynEdit.UpdateScrollbarReserve;
begin
  if (FScrollbarVert = nil) or (FScrollbarHorz = nil) then
    Exit;

  if FScrollbarVert.Visible then
    BorderSpacing.Right := FScrollbarVert.Width
  else
    BorderSpacing.Right := 0;

  if FScrollbarHorz.Visible then
    BorderSpacing.Bottom := FScrollbarHorz.Height
  else
    BorderSpacing.Bottom := 0;

  LayoutScrollbars();
end;

procedure TSimbaSynEdit.LayoutScrollbars;
var
  VertW, HorzH: Integer;
begin
  if (FScrollbarVert = nil) or (FScrollbarHorz = nil) or (Parent = nil) then
    Exit;

  VertW := FScrollbarVert.Width;
  HorzH := FScrollbarHorz.Height;
  if not FScrollbarVert.Visible then VertW := 0;
  if not FScrollbarHorz.Visible then HorzH := 0;

  if FScrollbarVert.Visible then
    FScrollbarVert.SetBounds(Left + Width, Top, VertW, Height);

  if FScrollbarHorz.Visible then
  begin
    if FScrollbarVert.Visible then
      FScrollbarHorz.IndentCorner := 100
    else
      FScrollbarHorz.IndentCorner := 0;
    FScrollbarHorz.SetBounds(Left, Top + Height, Width + VertW, HorzH);
  end;
end;

procedure TSimbaSynEdit.Resize;
begin
  inherited Resize();

  LayoutScrollbars();
end;

procedure TSimbaSynEdit.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  FScrollbarHorz.Visible := Value and FShowHorzScroll;
  FScrollbarVert.Visible := Value and FShowVertScroll;

  UpdateScrollbarReserve();
end;

constructor TSimbaSynEdit.Create(AOwner: TComponent; HighlighterClass: TSynCustomHighlighterClass);
begin
  inherited Create(AOwner);

  FShowVertScroll  := True;
  FShowHorzScroll  := True;
  FAllowVertScroll := True;
  FAllowHorzScroll := True;

  FScrollbarVert := TSimbaScrollBar.Create(Self);
  FScrollbarVert.Kind := sbVertical;
  FScrollbarVert.OnChange := @DoVertScrollBarChange;

  FScrollbarHorz := TSimbaScrollBar.Create(Self);
  FScrollbarHorz.Kind := sbHorizontal;
  FScrollbarHorz.OnChange := @DoHorzScrollBarChange;

  FStyler := TSimbaSynEditStyler.Create(Self);

  FFoldView := TSynEditFoldedView(FoldedTextBuffer);
  FFoldView.AddChangeHandler(senrLineMappingChanged, @DoLineChanges);
  TextView.AddChangeHandler(senrLineCount, @DoLineChanges);

  ScrollBars := ssNone;
  BorderStyle := bsNone;
  if (HighlighterClass <> nil) then
    Highlighter := HighlighterClass.Create(Self);

  Font.Color := SimbaComponentTheme.ColorFont;
  Font.Size := SynDefaultFontSize;
  Font.Name := SynDefaultFontName;
  Font.Quality := fqCleartypeNatural;
end;

constructor TSimbaMemo.Create(AOwner: TComponent; LineWrapping: Boolean);
var
  I: Integer;
begin
  inherited Create(AOwner, nil);

  Options := Options + [eoHideRightMargin];
  Gutter.Visible := False;
  RightGutter.Visible := False;

  if LineWrapping then
  begin
    AllowHorzScroll := False;
    ShowHorzScroll := False;
    Options := Options - [eoScrollPastEol];
    TLazSynEditLineWrapPlugin.Create(Self);
  end;

  for I := 0 to MarkupCount - 1 do
    if (Markup[I] <> nil) then
      Markup[I].Enabled := Markup[I] is TSynEditMarkupSelection;
end;

end.

