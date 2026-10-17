{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  OCR for pixel fonts: no antialiasing, so every pixel of a character must match.
  Originally SimpleOCR by Slacky: https://github.com/ollydev/SimpleOCR
}
unit simba.pixelocr;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.image,
  simba.pixelocr_font;

type
  TPixelOCRMatch = record
    Text: String;
    Hits: Integer;
    Bounds: TBox;
  end;
  TPixelOCRMatchArray = array of TPixelOCRMatch;

  PPixelOCR = ^TPixelOCR;

  // The glyph at X, Y with the most hits, if more than Hits, which is set to them. Sender has the tolerances.
  TGlyphMatcher = function(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph;

  TPixelOCR = record
    Tolerance: Single;
    ShadowTolerance: Integer;
    Whitelist: set of Char;
    MaxWalk: Integer; // stop after this many columns with no character. 0 = no limit
    MaxLen: Integer;  // stop at this many characters. 0 = no limit
    Matches: TPixelOCRMatchArray;
  private
    function RecognizeRow(Image: TSimbaImage; Font: PPixelFont; Matcher: TGlyphMatcher; X, Y, MaxX: Integer): TPixelOCRMatch;
    function RecognizeRows(Image: TSimbaImage; Font: PPixelFont; Bounds: TBox): TPixelOCRMatchArray;
  public
    function Locate(Image: TSimbaImage; Font: PPixelFont; Text: String): Single;

    function Recognize(Image: TSimbaImage; Font: PPixelFont; P: TPoint): String; overload;
    function Recognize(Image: TSimbaImage; Font: PPixelFont; Bounds: TBox): String; overload;
    function RecognizeLines(Image: TSimbaImage; Font: PPixelFont; Bounds: TBox): TStringArray;
  end;

implementation

uses
  simba.vartype_box,
  simba.vartype_pointarray,
  simba.colormath,
  simba.pixelocr_backend;

// is >= 50% of the text alpha-num
function IsAlphaNumSym(const Text: String): Boolean; inline;
var
  I, Count: Integer;
begin
  Count := 0;
  for I := 1 to Length(Text) do
    if (Text[I] in ['a'..'z', 'A'..'Z', '0'..'9', '%', '&', '#', '$', '[', ']', '{', '}', '@', '!', '?']) then
      Inc(Count);

  Result := (Count > 0) and (Count >= Length(Text) div 2);
end;

// Columns of row Y where all a glyph looks at is one colour: no glyph can be there
function GetPlainColumns(Image: TSimbaImage; Font: PPixelFont; Y: Integer): TBooleanArray;

  function IsPlain(Image: TSimbaImage; Col, Y1, Y2: Integer; const Color: TColorBGRA): Boolean; inline;
  var
    Pixel, Bottom: PColorBGRA;
  begin
    Pixel := Image.PixelPtr[Col, Y1];
    Bottom := Image.PixelPtr[Col, Y2];
    while (Pixel <= Bottom) and Pixel^.EqualsIgnoreAlpha(Color) do
      Inc(Pixel, Image.Width);

    Result := (Pixel > Bottom);
  end;

var
  Reach: TBox;
  Col, Run: Integer;
begin
  SetLength(Result, Image.Width);

  Reach := Font^.Reach;
  Reach.Y1 := Max(0, Y + Reach.Y1);
  Reach.Y2 := Min(Image.Height - 1, Y + Reach.Y2);

  // a column off the image counts as one colour
  Run := 0;
  for Col := Reach.X1 to Image.Width - 1 + Reach.X2 do
  begin
    if (Col < 0) or (Col >= Image.Width) or IsPlain(Image, Col, Reach.Y1, Reach.Y2, Image.PixelPtr[Max(0, Col - 1), Reach.Y1]^) then
      Inc(Run)
    else
      Run := 0;

    if (Run >= Reach.Width) then
      Result[Col - Reach.X2] := True;
  end;
end;

procedure AddGlyph(var Match: TPixelOCRMatch; Font: PPixelFont; Glyph: PPixelFontGlyph; X, Y, Space, Hits: Integer);
begin
  if (Match.Text = '') then
    Match.Bounds := Glyph^.BackgroundBounds.Offset(TPoint.Create(X, Y))
  else
  begin
    Match.Bounds := Match.Bounds.Combine(Glyph^.BackgroundBounds.Offset(TPoint.Create(X, Y)));
    if (Space >= Font^.SpaceWidth) then
      Match.Text += ' ';
  end;

  Match.Text += Glyph^.Value;
  Match.Hits += Hits;
end;

function TPixelOCR.RecognizeRow(Image: TSimbaImage; Font: PPixelFont; Matcher: TGlyphMatcher; X, Y, MaxX: Integer): TPixelOCRMatch;
var
  Plain: TBooleanArray;

  function FindGlyph(AtX: Integer; var Hits: Integer): PPixelFontGlyph;
  begin
    if Plain[AtX] then
      Result := nil
    else
      Result := Matcher(@Self, Image, Font, AtX, Y, Hits);
  end;

var
  Space, Ahead, Hits: Integer;
  Glyph, BestGlyph: PPixelFontGlyph;
begin
  Result := Default(TPixelOCRMatch);
  Space := 0;

  if (Image.Width = 0) or (Image.Height = 0) or (Y < -Font^.Reach.Y2) or (Y >= Image.Height) then
    Exit;

  X := Max(X, 0);
  MaxX := Min(MaxX, Image.Width - 1);
  Plain := GetPlainColumns(Image, Font, Y);

  while (X <= MaxX) and ((MaxWalk = 0) or (Space < MaxWalk)) and ((MaxLen = 0) or (Length(Result.Text) < MaxLen)) do
  begin
    Hits := 0;
    BestGlyph := FindGlyph(X, Hits);
    if (BestGlyph = nil) then
    begin
      Inc(Space);
      Inc(X);
      Continue;
    end;

    // a glyph with more hits starting inside this one replaces it
    Ahead := 1;
    while (Ahead < BestGlyph^.BackgroundBounds.Width) and (X + Ahead <= MaxX) do
    begin
      Glyph := FindGlyph(X + Ahead, Hits);
      if (Glyph = nil) then
        Inc(Ahead)
      else
      begin
        BestGlyph := Glyph;
        X += Ahead;
        Space += Ahead;
        Ahead := 1;
      end;
    end;

    if (Whitelist = []) or (BestGlyph^.Value in Whitelist) then
      AddGlyph(Result, Font, BestGlyph, X, Y, Space, Hits);

    Space := 0;
    X += BestGlyph^.BackgroundBounds.Width;
  end;
end;

function TPixelOCR.RecognizeRows(Image: TSimbaImage; Font: PPixelFont; Bounds: TBox): TPixelOCRMatchArray;
var
  Matcher: TGlyphMatcher;
  I: Integer;
begin
  Result := [];
  Matcher := GetGlyphMatcher(Image, Font);

  if (Bounds.X1 = -1) and (Bounds.Y1 = -1) and (Bounds.X2 = -1) and (Bounds.Y2 = -1) then
    Bounds := TBox.Create(0, 0, Image.Width - 1, Image.Height - 1);
  Bounds.Y1 := Max(Bounds.Y1, 0);
  Bounds.Y2 := Min(Bounds.Y2, Image.Height - 1);
  if (Bounds.Y1 > Bounds.Y2) then
    Exit;

  SetLength(Result, Bounds.Y2 - Bounds.Y1 + Font^.Reach.Y2 + 1);
  for I := 0 to High(Result) do
  begin
    Result[I] := RecognizeRow(Image, Font, Matcher, Bounds.X1, Bounds.Y1 - Font^.Reach.Y2 + I, Bounds.X2);
    if (Result[I].Bounds.Y1 < Bounds.Y1) or (Result[I].Bounds.Y1 > Bounds.Y2) then
      Result[I] := Default(TPixelOCRMatch);
  end;
end;

function TPixelOCR.Locate(Image: TSimbaImage; Font: PPixelFont; Text: String): Single;
var
  Points, Background: TPointArray;
  At: TPoint;
begin
  Result := 0;
  Matches := [];

  Points := Font^.TextToTPA(Text, Background);
  if (Length(Points) = 0) or (Length(Background) = 0) then
    Exit;

  if Image.isBinary() then
    Result := TBackendBinary.Locate(@Self, Image, Points, Background, At)
  else
    Result := TBackendColor.Locate(@Self, Image, Points, Background, At);

  if (Result > 0) then
  begin
    Matches := [Default(TPixelOCRMatch)];
    Matches[0].Text := Text;
    Matches[0].Hits := Round(Result * 100);
    Matches[0].Bounds := Points.Bounds.Offset(At);
  end;
end;

function TPixelOCR.Recognize(Image: TSimbaImage; Font: PPixelFont; P: TPoint): String;
begin
  Matches := [RecognizeRow(Image, Font, GetGlyphMatcher(Image, Font), P.X, P.Y, Image.Width - 1)];
  Result := Matches[0].Text;
end;

function TPixelOCR.Recognize(Image: TSimbaImage; Font: PPixelFont; Bounds: TBox): String;
var
  Rows: TPixelOCRMatchArray;
  I: Integer;
begin
  Rows := RecognizeRows(Image, Font, Bounds);

  Matches := [Default(TPixelOCRMatch)];
  for I := 0 to High(Rows) do
    if (Rows[I].Hits > Matches[0].Hits) then
      Matches[0] := Rows[I];

  Result := Matches[0].Text;
end;

function TPixelOCR.RecognizeLines(Image: TSimbaImage; Font: PPixelFont; Bounds: TBox): TStringArray;
var
  Temp: TPixelOCR;
  Rows: TPixelOCRMatchArray;
  Window, I, J: Integer;
  Best: Boolean;
begin
  Result := [];
  Matches := [];

  // a line is read whole
  Temp := Self;
  Temp.MaxWalk := 0;
  Temp.MaxLen := 0;
  Rows := Temp.RecognizeRows(Image, Font, Bounds);

  for I := 0 to High(Rows) do
    if not IsAlphaNumSym(Rows[I].Text) then
      Rows[I] := Default(TPixelOCRMatch);

  // Picks the best-matched row in each window of half the font's reach
  Window := Font^.Reach.Height div 2;
  for I := 0 to High(Rows) do
  begin
    // of rows that read the same, the first
    Best := (Rows[I].Hits > 0);
    for J := Max(0, I - Window) to Min(High(Rows), I + Window) do
      if (Rows[J].Hits > Rows[I].Hits) or ((Rows[J].Hits = Rows[I].Hits) and (J < I)) then
        Best := False;

    if Best then
    begin
      Result += [Rows[I].Text];
      Matches += [Rows[I]];
    end;
  end;
end;

end.
