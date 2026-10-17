{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The two ways pixelocr reads an image: in colour, or black and white (binary)
}
unit simba.pixelocr_backend;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.image,
  simba.pixelocr,
  simba.pixelocr_font;

type
  // Reads an image in colour: text is one colour, to a tolerance, and shadows are dark
  TBackendColor = record
    class function Match(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph; static;
    class function MatchShadow(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph; static;
    // the best place for the text: 0 for none, 1 where none of the pixels round it are its colour
    class function Locate(Sender: PPixelOCR; Image: TSimbaImage; Points, Background: TPointArray; out At: TPoint): Single; static;
  end;

  // Reads a black and white image: text is white and shadows are black, exactly
  TBackendBinary = record
    class function Match(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph; static;
    class function MatchShadow(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph; static;
    // the best place for the text: 0 for none, 1 where none of the pixels round it are its colour
    class function Locate(Sender: PPixelOCR; Image: TSimbaImage; Points, Background: TPointArray; out At: TPoint): Single; static;
  end;

// Raises for a font with no glyphs
function GetGlyphMatcher(Image: TSimbaImage; Font: PPixelFont): TGlyphMatcher;

implementation

uses
  simba.vartype_box,
  simba.vartype_pointarray,
  simba.colormath_distance;

function IsDark(Color: TColorBGRA; Tol: Integer): Boolean; inline;
begin
  Result := (Color.R <= Tol) and (Color.G <= Tol) and (Color.B <= Tol + 5); // a little more blue allowed
end;

function IsWhite(Color: TColorBGRA): Boolean; inline;
begin
  Result := (Color.AsInteger and $00FFFFFF) = $00FFFFFF;
end;

function IsBlack(Color: TColorBGRA): Boolean; inline;
begin
  Result := (Color.AsInteger and $00FFFFFF) = 0;
end;

function PixelOffsets(Points: TPointArray; Width: Integer): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Points));
  for I := 0 to High(Points) do
    Result[I] := Points[I].Y * Width + Points[I].X;
end;

class function TBackendColor.Match(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph;

  function CanCompare(Image: TSimbaImage; Glyph: PPixelFontGlyph; X, Y, Best: Integer): Boolean; inline;
  begin
    Result := (Glyph^.BestMatch > Best) and
              (Int32(X + Glyph^.BackgroundBounds.X1 - 1) >= 0) and (Int32(X + Glyph^.BackgroundBounds.X2 + 1) < Image.Width) and
              (Int32(Y + Glyph^.BackgroundBounds.Y1 - 1) >= 0) and (Int32(Y + Glyph^.BackgroundBounds.Y2 + 1) < Image.Height);
  end;

  function Compare(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; Tol: Single): Integer;
  var
    I, Misses, Threshold: Integer;
    First: TColorBGRA;
    P: TPoint;
  begin
    First := Origin[Glyph^.Points[0].Y * Image.Width + Glyph^.Points[0].X];
    for I := 1 to High(Glyph^.Points) do
      if not SimilarRGB(Origin[Glyph^.Points[I].Y * Image.Width + Glyph^.Points[I].X], First, Tol) then
        Exit(-1);

    Misses := 0;
    Threshold := (Length(Glyph^.Background) + 1) div 2;
    for I := 0 to High(Glyph^.Background) do
    begin
      P := Glyph^.Background[I];
      if SimilarRGB(Origin[P.Y * Image.Width + P.X], First, Tol) then
      begin
        Inc(Misses);
        if (Misses >= Threshold) then
          Exit(-1);
      end;
    end;

    Result := Glyph^.BestMatch - Misses;
  end;

var
  Best, GlyphHits: Integer;
  Tol: Single;
  Glyph, Last: PPixelFontGlyph;
  Origin: PColorBGRA;
begin
  Result := nil;

  Tol := Sender^.Tolerance;
  Best := Hits;
  Origin := Image.PixelPtr[X, Y];
  Glyph := @Font^.Glyphs[0];
  Last := @Font^.Glyphs[High(Font^.Glyphs)];
  while (Glyph <= Last) do
  begin
    if CanCompare(Image, Glyph, X, Y, Best) then
    begin
      GlyphHits := Compare(Image, Origin, Glyph, Tol);
      if (GlyphHits > Best) then
      begin
        Result := Glyph;
        Best := GlyphHits;
      end;
    end;

    Inc(Glyph);
  end;

  Hits := Best;
end;

class function TBackendColor.MatchShadow(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph;

  function CanCompareGroup(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; X, Y, ShadowTol: Integer): Boolean; inline;
  begin
    Result := (Glyph^.Shadow <> nil) and
              (UInt32(X + Glyph^.Shadow[0].X) < UInt32(Image.Width)) and (UInt32(Y + Glyph^.Shadow[0].Y) < UInt32(Image.Height)) and
              IsDark(Origin[Glyph^.Shadow[0].Y * Image.Width + Glyph^.Shadow[0].X], ShadowTol);
  end;

  function CanCompare(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; X, Y, ShadowTol, Best: Integer): Boolean; inline;
  begin
    Result := (Glyph^.BestMatch > Best) and
              (Int32(X + Glyph^.BackgroundBounds.X1) >= 0) and (Int32(X + Glyph^.BackgroundBounds.X2) < Image.Width) and
              (Int32(Y + Glyph^.BackgroundBounds.Y1) >= 0) and (Int32(Y + Glyph^.BackgroundBounds.Y2) < Image.Height) and
              (not IsDark(Origin[Glyph^.Points[0].Y * Image.Width + Glyph^.Points[0].X], ShadowTol * 2));
  end;

  function Compare(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; Tol: Single; ShadowTol: Integer): Integer;
  var
    I: Integer;
    First: TColorBGRA;
  begin
    for I := 0 to High(Glyph^.Shadow) do
      if not IsDark(Origin[Glyph^.Shadow[I].Y * Image.Width + Glyph^.Shadow[I].X], ShadowTol) then
        Exit(-1);

    First := Origin[Glyph^.Points[0].Y * Image.Width + Glyph^.Points[0].X];
    for I := 1 to High(Glyph^.Points) do
      if not SimilarRGB(Origin[Glyph^.Points[I].Y * Image.Width + Glyph^.Points[I].X], First, Tol) then
        Exit(-1);

    Result := Glyph^.BestMatch;
  end;

var
  Best, GlyphHits, ShadowTol: Integer;
  Tol: Single;
  Glyph, GroupLast, Last: PPixelFontGlyph;
  Origin: PColorBGRA;
begin
  Result := nil;

  Tol := Sender^.Tolerance;
  ShadowTol := Sender^.ShadowTolerance;
  Best := Hits;
  Origin := Image.PixelPtr[X, Y];
  Glyph := @Font^.Glyphs[0];
  Last := @Font^.Glyphs[High(Font^.Glyphs)];
  while (Glyph <= Last) do
  begin
    GroupLast := Glyph + Glyph^.Group;
    if CanCompareGroup(Image, Origin, Glyph, X, Y, ShadowTol) then
      while (Glyph <= GroupLast) do
      begin
        if CanCompare(Image, Origin, Glyph, X, Y, ShadowTol, Best) then
        begin
          GlyphHits := Compare(Image, Origin, Glyph, Tol, ShadowTol);
          if (GlyphHits > Best) then
          begin
            Result := Glyph;
            Best := GlyphHits;
          end;
        end;

        Inc(Glyph);
      end;

    Glyph := GroupLast + 1;
  end;

  Hits := Best;
end;

class function TBackendColor.Locate(Sender: PPixelOCR; Image: TSimbaImage; Points, Background: TPointArray; out At: TPoint): Single;
var
  Offsets, BackgroundOffsets: TIntegerArray;
  Tol: Single;

  function BadAt(Origin: PColorBGRA; Limit: Integer): Integer;
  var
    I: Integer;
    First: TColorBGRA;
  begin
    First := Origin[Offsets[0]];
    for I := 1 to High(Offsets) do
      if not SimilarRGB(Origin[Offsets[I]], First, Tol) then
        Exit(Limit);

    Result := 0;
    for I := 0 to High(BackgroundOffsets) do
      if SimilarRGB(Origin[BackgroundOffsets[I]], First, Tol) then
      begin
        Inc(Result);
        if (Result >= Limit) then
          Exit;
      end;
  end;

var
  X, Y, Bad, BestBad: Integer;
  TextBox: TBox;
begin
  Result := 0;

  Tol := Sender^.Tolerance;
  Offsets := PixelOffsets(Points, Image.Width);
  BackgroundOffsets := PixelOffsets(Background, Image.Width);
  TextBox := Points.Bounds.Combine(Background.Bounds);

  BestBad := Length(Background);
  for Y := 0 to Image.Height - TextBox.Y2 - 1 do
    for X := 0 to Image.Width - TextBox.X2 - 1 do
    begin
      Bad := BadAt(Image.PixelPtr[X, Y], BestBad);
      if (Bad < BestBad) then
      begin
        BestBad := Bad;
        Result := 1 - (Bad / Length(Background));
        At := TPoint.Create(X, Y);
        if (Bad = 0) then
          Exit;
      end;
    end;
end;

class function TBackendBinary.Match(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph;

  function CanCompareGroup(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; X, Y: Integer): Boolean; inline;
  begin
    Result := (Glyph^.Points <> nil) and
              (UInt32(X + Glyph^.Points[0].X) < UInt32(Image.Width)) and (UInt32(Y + Glyph^.Points[0].Y) < UInt32(Image.Height)) and
              IsWhite(Origin[Glyph^.Points[0].Y * Image.Width + Glyph^.Points[0].X]);
  end;

  function CanCompare(Image: TSimbaImage; Glyph: PPixelFontGlyph; X, Y, Best: Integer): Boolean; inline;
  begin
    Result := (Glyph^.BestMatch > Best) and
              (Int32(X + Glyph^.BackgroundBounds.X1 - 1) >= 0) and (Int32(X + Glyph^.BackgroundBounds.X2 + 1) < Image.Width) and
              (Int32(Y + Glyph^.BackgroundBounds.Y1 - 1) >= 0) and (Int32(Y + Glyph^.BackgroundBounds.Y2 + 1) < Image.Height);
  end;

  function Compare(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph): Integer;
  var
    I, Misses, Threshold: Integer;
    P: TPoint;
  begin
    for I := 0 to High(Glyph^.Points) do
      if not IsWhite(Origin[Glyph^.Points[I].Y * Image.Width + Glyph^.Points[I].X]) then
        Exit(-1);

    Misses := 0;
    Threshold := (Length(Glyph^.Background) + 1) div 2;
    for I := 0 to High(Glyph^.Background) do
    begin
      P := Glyph^.Background[I];
      if IsWhite(Origin[P.Y * Image.Width + P.X]) then
      begin
        Inc(Misses);
        if (Misses >= Threshold) then
          Exit(-1);
      end;
    end;

    Result := Glyph^.BestMatch - Misses;
  end;

var
  Best, GlyphHits: Integer;
  Glyph, GroupLast, Last: PPixelFontGlyph;
  Origin: PColorBGRA;
begin
  Result := nil;

  Best := Hits;
  Origin := Image.PixelPtr[X, Y];
  Glyph := @Font^.Glyphs[0];
  Last := @Font^.Glyphs[High(Font^.Glyphs)];
  while (Glyph <= Last) do
  begin
    GroupLast := Glyph + Glyph^.Group;
    if CanCompareGroup(Image, Origin, Glyph, X, Y) then
      while (Glyph <= GroupLast) do
      begin
        if CanCompare(Image, Glyph, X, Y, Best) then
        begin
          GlyphHits := Compare(Image, Origin, Glyph);
          if (GlyphHits > Best) then
          begin
            Result := Glyph;
            Best := GlyphHits;
          end;
        end;

        Inc(Glyph);
      end;

    Glyph := GroupLast + 1;
  end;

  Hits := Best;
end;

class function TBackendBinary.MatchShadow(Sender: PPixelOCR; Image: TSimbaImage; Font: PPixelFont; X, Y: Integer; var Hits: Integer): PPixelFontGlyph;

  function CanCompareGroup(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; X, Y: Integer): Boolean; inline;
  begin
    Result := (Glyph^.Shadow <> nil) and
              (UInt32(X + Glyph^.Shadow[0].X) < UInt32(Image.Width)) and (UInt32(Y + Glyph^.Shadow[0].Y) < UInt32(Image.Height)) and
              IsBlack(Origin[Glyph^.Shadow[0].Y * Image.Width + Glyph^.Shadow[0].X]);
  end;

  function CanCompare(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph; X, Y, Best: Integer): Boolean; inline;
  begin
    Result := (Glyph^.BestMatch > Best) and
              (Int32(X + Glyph^.BackgroundBounds.X1) >= 0) and (Int32(X + Glyph^.BackgroundBounds.X2) < Image.Width) and
              (Int32(Y + Glyph^.BackgroundBounds.Y1) >= 0) and (Int32(Y + Glyph^.BackgroundBounds.Y2) < Image.Height) and
              IsWhite(Origin[Glyph^.Points[0].Y * Image.Width + Glyph^.Points[0].X]);
  end;

  function Compare(Image: TSimbaImage; Origin: PColorBGRA; Glyph: PPixelFontGlyph): Integer;
  var
    I: Integer;
  begin
    for I := 0 to High(Glyph^.Points) do
      if not IsWhite(Origin[Glyph^.Points[I].Y * Image.Width + Glyph^.Points[I].X]) then
        Exit(-1);

    for I := 0 to High(Glyph^.Shadow) do
      if not IsBlack(Origin[Glyph^.Shadow[I].Y * Image.Width + Glyph^.Shadow[I].X]) then
        Exit(-1);

    Result := Glyph^.BestMatch;
  end;

var
  Best, GlyphHits: Integer;
  Glyph, GroupLast, Last: PPixelFontGlyph;
  Origin: PColorBGRA;
begin
  Result := nil;

  Best := Hits;
  Origin := Image.PixelPtr[X, Y];
  Glyph := @Font^.Glyphs[0];
  Last := @Font^.Glyphs[High(Font^.Glyphs)];
  while (Glyph <= Last) do
  begin
    GroupLast := Glyph + Glyph^.Group;
    if CanCompareGroup(Image, Origin, Glyph, X, Y) then
      while (Glyph <= GroupLast) do
      begin
        if CanCompare(Image, Origin, Glyph, X, Y, Best) then
        begin
          GlyphHits := Compare(Image, Origin, Glyph);
          if (GlyphHits > Best) then
          begin
            Result := Glyph;
            Best := GlyphHits;
          end;
        end;

        Inc(Glyph);
      end;

    Glyph := GroupLast + 1;
  end;

  Hits := Best;
end;

class function TBackendBinary.Locate(Sender: PPixelOCR; Image: TSimbaImage; Points, Background: TPointArray; out At: TPoint): Single;
var
  Offsets, BackgroundOffsets: TIntegerArray;

  function BadAt(Origin: PColorBGRA; Limit: Integer): Integer;
  var
    I: Integer;
  begin
    for I := 0 to High(Offsets) do
      if not IsWhite(Origin[Offsets[I]]) then
        Exit(Limit);

    Result := 0;
    for I := 0 to High(BackgroundOffsets) do
      if IsWhite(Origin[BackgroundOffsets[I]]) then
      begin
        Inc(Result);
        if (Result >= Limit) then
          Exit;
      end;
  end;

var
  X, Y, Bad, BestBad: Integer;
  TextBox: TBox;
  Origin: PColorBGRA;
begin
  Result := 0;

  Offsets := PixelOffsets(Points, Image.Width);
  BackgroundOffsets := PixelOffsets(Background, Image.Width);
  TextBox := Points.Bounds.Combine(Background.Bounds);

  BestBad := Length(Background);
  for Y := 0 to Image.Height - TextBox.Y2 - 1 do
    for X := 0 to Image.Width - TextBox.X2 - 1 do
    begin
      // most places fail on the first point
      Origin := Image.PixelPtr[X, Y];
      if not IsWhite(Origin[Offsets[0]]) then
        Continue;

      Bad := BadAt(Origin, BestBad);
      if (Bad < BestBad) then
      begin
        BestBad := Bad;
        Result := 1 - (Bad / Length(Background));
        At := TPoint.Create(X, Y);
        if (Bad = 0) then
          Exit;
      end;
    end;
end;

function GetGlyphMatcher(Image: TSimbaImage; Font: PPixelFont): TGlyphMatcher;
begin
  if (Length(Font^.Glyphs) = 0) then
    SimbaException('Font is empty');

  if Image.isBinary() then
  begin
    if Font^.IsShadow then
      Result := @TBackendBinary.MatchShadow
    else
      Result := @TBackendBinary.Match;
  end else
  begin
    if Font^.IsShadow then
      Result := @TBackendColor.MatchShadow
    else
      Result := @TBackendColor.Match;
  end;
end;

end.
