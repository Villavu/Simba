{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The font pixelocr reads with.
}
unit simba.pixelocr_font;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.image,
  simba.canvas;

type
  TPixelFontGlyph = record
    Value: Char;

    Width: Integer; // image width
    Left: Integer;  // the first column of the image that has a point

    Points: TPointArray;      // X is from Left
    Shadow: TPointArray;
    Background: TPointArray;  // with no shadow: the other pixels of BackgroundBounds grown by 1
    BackgroundBounds: TBox;   // bounds of points and shadow

    BestMatch: Integer;
    Group: Integer; // how many glyphs after this one look at the same pixel first, so a read tests it once
  end;
  PPixelFontGlyph = ^TPixelFontGlyph;

  TPixelFont = record
    Glyphs: array of TPixelFontGlyph; // in the order a read tries them
    GlyphSimilarities: array of TByteArray;

    SpaceWidth: Integer;
    Reach: TBox; // every pixel any glyph looks at, from where it is placed
    IsShadow: Boolean;

    class function Load(Dir: String; ASpaceWidth: Integer): TPixelFont; overload; static;
    class function Load(Images: TSimbaImageArray; ASpaceWidth: Integer): TPixelFont; overload; static;
    class function Load(ZipFile, ZipDir: String; ASpaceWidth: Integer): TPixelFont; overload; static;

    function TextToTPA(Text: String; out Background: TPointArray): TPointArray; overload;
    function TextToTPA(Text: String; Offset: TPoint; Scale: Integer = 1): TPointArray; overload;
    function GetSimilarGlyphs(c: Char; MinMatch: Single): TCharArray;

    procedure DrawText(Canvas: TSimbaCanvas; Text: String; Position: TPoint; Scale: Integer = 1);
  end;
  PPixelFont = ^TPixelFont;

implementation

uses
  simba.vartype_string,
  simba.vartype_box,
  simba.vartype_pointarray,
  simba.fs,
  simba.zip;

function FontFileChar(const FileName: String): Integer;
begin
  Result := 0;
  if TSimbaPath.PathHasExt(FileName, ['.bmp', '.png']) then
    Result := StrToIntDef(TSimbaPath.PathExtractNameWithoutExt(FileName), 0);
  if (Result < 32) or (Result > 126) then
    Result := 0;
end;

class function TPixelFont.Load(Dir: String; ASpaceWidth: Integer): TPixelFont;
var
  Images: TSimbaImageArray;
  Files: TStringArray;
  I, Value: Integer;
begin
  if not DirectoryExists(Dir) then
    SimbaException('TPixelOCR.LoadFont: Directory "%s" does not exist', [Dir]);

  Files := TSimbaDir.DirList(Dir);

  SetLength(Images, 256);
  try
    for I := 0 to High(Files) do
    begin
      Value := FontFileChar(Files[I]);
      if (Value > 0) and (Images[Value] = nil) then
        Images[Value] := TSimbaImage.Create(Files[I]);
    end;

    Result := TPixelFont.Load(Images, ASpaceWidth);
  finally
    for I := 0 to High(Images) do
      Images[I].Free();
  end;
end;

function GlyphSimilarity(const A, B: TPixelFontGlyph): Single;
var
  Hit: Integer;
begin
  Hit := Length(A.Points.Intersection(B.Points));

  if (Hit > 0) then
    Result := Hit / (Length(A.Points) + Length(B.Points) - Hit)
  else
    Result := 0;
end;

function GlyphOrder(const Glyph: TPixelFontGlyph): Integer;
begin
  if (Glyph.Shadow <> nil) then
    Result := Glyph.Shadow[0].Y * $10000 + Glyph.Shadow[0].X
  else if (Glyph.Points <> nil) then
    Result := Glyph.Points[0].Y * $10000 + Glyph.Points[0].X
  else
    Result := High(Integer);
end;

class function TPixelFont.Load(Images: TSimbaImageArray; ASpaceWidth: Integer): TPixelFont;
var
  I, J, Count: Integer;
  Glyph: TPixelFontGlyph;
begin
  Result := Default(TPixelFont);
  Result.SpaceWidth := ASpaceWidth;

  for I := 0 to Min(High(Images), Ord(High(Char))) do
  begin
    if (Images[I] = nil) then
      Continue;

    Glyph := Default(TPixelFontGlyph);
    Glyph.Value := Char(I);
    Glyph.Width := Images[I].Width;
    if (Glyph.Value > #32) then
    begin
      Glyph.Points := Images[I].Finder.FindColor($FFFFFF, 0, TBox.Create(-1,-1,-1,-1));
      if (Length(Glyph.Points) = 0) then
        Continue;
      Glyph.Shadow := Images[I].Finder.FindColor($0000FF, 0, TBox.Create(-1,-1,-1,-1));

      Glyph.Left := Glyph.Points.Bounds.X1;
      Glyph.Points := Glyph.Points.Offset(-Glyph.Left, 0);
      Glyph.Shadow := Glyph.Shadow.Offset(-Glyph.Left, 0);
      Glyph.BackgroundBounds := TPointArray(Glyph.Points + Glyph.Shadow).Bounds;

      if (Glyph.Shadow <> nil) then
        Result.IsShadow := True
      else
        Glyph.Background := Glyph.Points.Invert(Glyph.BackgroundBounds.Expand(1));
      Glyph.BestMatch := Length(Glyph.Points) + Length(Glyph.Shadow) + Length(Glyph.Background);

      Result.Reach := Result.Reach.Combine(Glyph.BackgroundBounds.Expand(1));
    end;

    Result.Glyphs += [Glyph];
  end;
  Count := Length(Result.Glyphs);
  if (Count = 0) then
    SimbaException('TPixelOCR.LoadFont: Font is empty');

  for I := 0 to Count - 1 do
    if Result.IsShadow and (Result.Glyphs[I].Points <> nil) and (Result.Glyphs[I].Shadow = nil) then
      SimbaException('TPixelOCR.LoadFont: Character "%s" has no shadow, other characters of the font do', [Result.Glyphs[I].Value]);

  // into search order, ties staying in character order
  for I := 1 to Count - 1 do
  begin
    Glyph := Result.Glyphs[I];
    J := I - 1;
    while (J >= 0) and (GlyphOrder(Result.Glyphs[J]) > GlyphOrder(Glyph)) do
    begin
      Result.Glyphs[J + 1] := Result.Glyphs[J];
      Dec(J);
    end;
    Result.Glyphs[J + 1] := Glyph;
  end;

  for I := Count - 2 downto 0 do
    if (Result.Glyphs[I].Points <> nil) and (GlyphOrder(Result.Glyphs[I]) = GlyphOrder(Result.Glyphs[I + 1])) then
      Result.Glyphs[I].Group := Result.Glyphs[I + 1].Group + 1;

  SetLength(Result.GlyphSimilarities, Count, Count);
  for I := 0 to Count - 1 do
    for J := I + 1 to Count - 1 do
    begin
      Result.GlyphSimilarities[I][J] := Round(GlyphSimilarity(Result.Glyphs[I], Result.Glyphs[J]) * 255);
      Result.GlyphSimilarities[J][I] := Result.GlyphSimilarities[I][J];
    end;
end;

class function TPixelFont.Load(ZipFile, ZipDir: String; ASpaceWidth: Integer): TPixelFont;
var
  Images: TSimbaImageArray;
  Entries, Wanted: TStringArray;
  Streams: specialize TArray<TMemoryStream>;
  I, Value: Integer;
begin
  if not FileExists(ZipFile) then
    SimbaException('TPixelOCR.LoadFont: Zip "%s" does not exist', [ZipFile]);

  ZipDir := ZipDir.Replace('\', '/');
  if (ZipDir <> '') and (not ZipDir.EndsWith('/')) then
    ZipDir := ZipDir + '/';

  // all the font's files in one go: opening the zip per character is slow
  Entries := ZipReadEntries(ZipFile);
  for I := 0 to High(Entries) do
    if SameText(ExtractFilePath(Entries[I].Replace('\', '/')), ZipDir) and (FontFileChar(Entries[I]) > 0) then
      Wanted += [Entries[I]];
  Streams := ZipExtractEntriesToStreams(ZipFile, Wanted);

  SetLength(Images, 256);
  try
    for I := 0 to High(Wanted) do
    begin
      Value := FontFileChar(Wanted[I]);
      if (Images[Value] = nil) then
      begin
        Images[Value] := TSimbaImage.Create();
        Images[Value].FromStream(Streams[I], Wanted[I]);
      end;
    end;

    Result := TPixelFont.Load(Images, ASpaceWidth);
  finally
    for I := 0 to High(Images) do
      Images[I].Free();
    for I := 0 to High(Streams) do
      Streams[I].Free();
  end;
end;

function IndexOfGlyph(const Font: TPixelFont; c: Char): Integer;
var
  I: Integer;
begin
  for I := 0 to High(Font.Glyphs) do
    if (Font.Glyphs[I].Value = c) then
      Exit(I);

  SimbaException('Character "%s" does not exist in the font', [c]);
  Result := -1;
end;

function PlaceText(const Font: TPixelFont; const Text: String; out Shadow: TPointArray; out Width: Integer): TPointArray;
var
  I: Integer;
  Glyph: PPixelFontGlyph;
begin
  Result := [];
  Shadow := [];

  Width := 0;
  for I := 1 to Length(Text) do
  begin
    Glyph := @Font.Glyphs[IndexOfGlyph(Font, Text[I])];
    Result += Glyph^.Points.Offset(Width + Glyph^.Left, 0); // back to where it was in its image
    Shadow += Glyph^.Shadow.Offset(Width + Glyph^.Left, 0);

    Width += Glyph^.Width;
  end;
end;

function ScalePoints(const Points: TPointArray; Offset: TPoint; Scale: Integer): TPointArray;
var
  I, X, Y: Integer;
begin
  Scale := Max(1, Scale);

  SetLength(Result, Length(Points) * Scale * Scale);
  for I := 0 to High(Points) do
    for Y := 0 to Scale - 1 do
      for X := 0 to Scale - 1 do
        Result[(I * Scale + Y) * Scale + X] := TPoint.Create(Offset.X + Points[I].X * Scale + X, Offset.Y + Points[I].Y * Scale + Y);
end;

function TPixelFont.TextToTPA(Text: String; out Background: TPointArray): TPointArray;
var
  Shadow: TPointArray;
  Width: Integer;
  Box: TBox;
begin
  Background := [];

  Result := PlaceText(Self, Text, Shadow, Width);
  if (Result = nil) then
    Exit;

  // the top row of the text is row 0
  Box := TPointArray(Result + Shadow).Bounds;
  Result := Result.Offset(0, -Box.Y1);
  Background := Result.Invert(TBox.Create(0, 0, Width - 1, Box.Y2 - Box.Y1));
end;

function TPixelFont.TextToTPA(Text: String; Offset: TPoint; Scale: Integer): TPointArray;
var
  _: TPointArray;
begin
  Result := ScalePoints(TextToTPA(Text, _), Offset, Scale);
end;

function TPixelFont.GetSimilarGlyphs(c: Char; MinMatch: Single): TCharArray;
var
  I, J, Score: Integer;
begin
  Result := [];

  I := IndexOfGlyph(Self, c);

  for Score := 255 downto 1 do
    if (Score >= MinMatch * 255) then
      for J := 0 to High(Glyphs) do
        if (J <> I) and (GlyphSimilarities[I][J] = Score) then
          Result += [Glyphs[J].Value];
end;

procedure TPixelFont.DrawText(Canvas: TSimbaCanvas; Text: String; Position: TPoint; Scale: Integer);
var
  Points, Shadow: TPointArray;
  Width: Integer;
  State: TSimbaCanvasState;
begin
  Points := PlaceText(Self, Text, Shadow, Width);

  // a pixel at a time, whatever the canvas's thickness and antialiasing
  State := Canvas.State;
  Canvas.DrawThickness := 1;
  Canvas.DrawAntialiasing := False;

  Canvas.DrawColor := $000000; // black
  Canvas.DrawTPA(ScalePoints(Shadow, Position, Scale));
  Canvas.DrawColor := State.DrawColor;
  Canvas.DrawTPA(ScalePoints(Points, Position, Scale));

  Canvas.State := State;
end;

end.
