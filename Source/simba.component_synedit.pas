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
  simba.component_theme,
  simba.component_scrollbar,
  simba.component_syneditstyler;

type
  TSimbaSynEdit = class(TSynEdit)
  protected
    FStyler: TSimbaSynEditStyler;
    FScrollbarVert: TSimbaScrollBar;
    FScrollbarHorz: TSimbaScrollBar;

    procedure DoVertScrollBarChange(Sender: TObject);
    procedure DoHorzScrollBarChange(Sender: TObject);

    // Override to scroll horizontally when shift + mouse wheel
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    procedure UpdateBars;
    procedure StatusChanged(AChanges: TSynStatusChanges); override;
    procedure DoLineChanges(Sender: TSynEditStrings; aIndex, aCount: Integer);
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetVisible(Value: Boolean); override;

    function GetFontAntialising: Boolean;
    function GetFontName: String;
    procedure SetFontAntialising(Value: Boolean);
    procedure SetFontName(AValue: String);
  public
    constructor Create(AOwner: TComponent; HighlighterClass: TSynCustomHighlighterClass); virtual; reintroduce;

    property Styler: TSimbaSynEditStyler read FStyler;
    property FontName: String read GetFontName write SetFontName;
    property FontAntialising: Boolean read GetFontAntialising write SetFontAntialising;
  end;

  // Hide gutters and such so the synedit acts more like the "memo" component.
  TSimbaMemo = class(TSimbaSynEdit)
  public
    constructor Create(AOwner: TComponent; LineWrapping: Boolean); virtual; reintroduce;
  end;

implementation

uses
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

procedure TSimbaSynEdit.DoVertScrollBarChange(Sender: TObject);
begin
  TopView := FScrollbarVert.Position;
end;

procedure TSimbaSynEdit.DoHorzScrollBarChange(Sender: TObject);
begin
  LeftChar := FScrollbarHorz.Position;
end;

function TSimbaSynEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const
  SCROLL_AMOUNT = 5;
begin
  if (ssShift in Shift) then
  begin
    if (WheelDelta > 0) then
      FScrollbarHorz.Position := FScrollbarHorz.Position - SCROLL_AMOUNT
    else
      FScrollbarHorz.Position := FScrollbarHorz.Position + SCROLL_AMOUNT;

    Result := True;
  end else
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TSimbaSynEdit.UpdateBars;
begin
  if FScrollbarVert=nil then Exit;
  if FScrollbarHorz=nil then Exit;

  FScrollbarVert.Min := 1;
  FScrollbarVert.Max := TextView.ViewedCount + 1;
  if (eoScrollPastEof in Options) then
    FScrollbarVert.Max := FScrollbarVert.Max + (LinesInWindow - 1);
  FScrollbarVert.PageSize := LinesInWindow;
  FScrollbarVert.Position := TopView;

  FScrollbarHorz.Min := 1;
  FScrollbarHorz.Max := TextView.LengthOfLongestLine + 1;
  if (eoScrollPastEol in Options) and (FScrollbarHorz.Max < MaxLeftChar + 1) then
    FScrollbarHorz.Max := MaxLeftChar + 1;
  FScrollbarHorz.PageSize := CharsInWindow;
  FScrollbarHorz.Position := LeftChar;
end;

procedure TSimbaSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  inherited StatusChanged(AChanges);

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

  FScrollbarHorz.Parent := NewParent;
  FScrollbarHorz.Align := alBottom;
  FScrollbarHorz.IndentCorner := 100;

  FScrollbarVert.Parent := NewParent;
  FScrollbarVert.Align := alRight;
end;

procedure TSimbaSynEdit.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  FScrollbarHorz.Visible := Value;
  FScrollbarVert.Visible := Value;
end;

constructor TSimbaSynEdit.Create(AOwner: TComponent; HighlighterClass: TSynCustomHighlighterClass);
begin
  inherited Create(AOwner);

  FScrollbarVert := TSimbaScrollBar.Create(Self);
  FScrollbarVert.Kind := sbVertical;
  FScrollbarVert.OnChange := @DoVertScrollBarChange;
  FScrollbarVert.ForwardScrollControl := Self;

  FScrollbarHorz := TSimbaScrollBar.Create(Self);
  FScrollbarHorz.Kind := sbHorizontal;
  FScrollbarHorz.OnChange := @DoHorzScrollBarChange;

  with FoldedTextBuffer as TSynEditFoldedView do
    AddChangeHandler(senrLineMappingChanged, @DoLineChanges);
  TextView.AddChangeHandler(senrLineCount, @DoLineChanges);

  ScrollBars := ssNone;
  BorderStyle := bsNone;
  if (HighlighterClass <> nil) then
    Highlighter := HighlighterClass.Create(Self);

  Font.Color := SimbaComponentTheme.ColorFont;
  Font.Size := SynDefaultFontSize;
  Font.Name := SynDefaultFontName;
  Font.Quality := fqCleartypeNatural;
  FontAntialising := True;

  FStyler := TSimbaSynEditStyler.Create(Self);
end;

constructor TSimbaMemo.Create(AOwner: TComponent; LineWrapping: Boolean);
begin
  inherited Create(AOwner, nil);

  Gutter.Visible := False;
  RightGutter.Visible := False;
  Options := Options + [eoHideRightMargin];

  if LineWrapping then
  begin
    FScrollbarHorz.Visible := False;
    Options := Options - [eoScrollPastEol];
    TLazSynEditLineWrapPlugin.Create(Self);
  end;
end;

end.

