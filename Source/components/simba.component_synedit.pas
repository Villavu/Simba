unit simba.component_synedit;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, StdCtrls, Graphics,
  SynEdit, SynEditTypes, SynEditFoldedView, SynEditTextBuffer, SynEditMarkupSelection,
  LazSynEditText,
  ATScrollBar;

type
  TSimbaSynEdit = class(TSynEdit)
  protected
    FScrollbarVert: TATScrollbar;
    FScrollbarHorz: TATScrollbar;

    procedure DoVertScrollBarChange(Sender: TObject);
    procedure DoHorzScrollBarChange(Sender: TObject);

    procedure UpdateBars;
    procedure StatusChanged(AChanges: TSynStatusChanges); override;
    procedure DoLineChanges(Sender: TSynEditStrings; aIndex, aCount: Integer);
    procedure SetParent(NewParent: TWinControl); override;

    function GetFontAntialising: Boolean;
    procedure SetFontAntialising(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;

    property FontAntialising: Boolean read GetFontAntialising write SetFontAntialising;
  end;

implementation

uses
  simba.mufasatypes, simba.theme;

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

procedure TSimbaSynEdit.DoVertScrollBarChange(Sender: TObject);
begin
  TopLine := FScrollbarVert.Position;
end;

procedure TSimbaSynEdit.DoHorzScrollBarChange(Sender: TObject);
begin
  LeftChar := FScrollbarHorz.Position;
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

  FScrollbarVert.Update();
  FScrollbarHorz.Update();
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

constructor TSimbaSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScrollbarVert := TATScrollbar.Create(Self);
  FScrollbarVert.Kind := sbVertical;
  FScrollbarVert.OnChange := @DoVertScrollBarChange;

  FScrollbarHorz := TATScrollbar.Create(Self);
  FScrollbarHorz.Kind := sbHorizontal;
  FScrollbarHorz.OnChange := @DoHorzScrollBarChange;

  with FoldedTextBuffer as TSynEditFoldedView do
    AddChangeHandler(senrLineMappingChanged, @DoLineChanges);
  TextView.AddChangeHandler(senrLineCount, @DoLineChanges);

  ScrollBars := ssNone;
  BorderStyle := bsNone;

  TSynEditMarkupSelection(MarkupByClass[TSynEditMarkupSelection]).MarkupInfoSeletion.Background := SimbaTheme.ColorActive;

  Color := SimbaTheme.ColorBackground;

  Font.Color := SimbaTheme.ColorFont;
  Font.Size := SynDefaultFontSize;
  Font.Name := SynDefaultFontName;

  FontAntialising := True;
end;

end.

