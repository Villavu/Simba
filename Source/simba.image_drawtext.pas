{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Drawing text onto a raw BGRA pixel buffer using freetype fontsets.
   - Each glyph is rasterized once and cached so future draws only blends coverage.
   - Text is drawn opaque: Color's alpha should be 255, glyph edges are blended by their coverage.
}
unit simba.image_drawtext;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  {$scopedenums on}
  ECanvasTextAlign = (
    LEFT,   // lines start at the box's left edge (the default)
    CENTER, // centred horizontally unless LEFT or RIGHT is set, vertically unless TOP or BOTTOM is
    RIGHT,  // lines end at the box's right edge
    TOP,    // the text starts at the top of the box (the default)
    BOTTOM  // the text ends at the bottom of the box
  );
  ECanvasTextAligns = set of ECanvasTextAlign;

  ECanvasFontStyle = (
    ANTIALIASED,
    BOLD,
    ITALIC,
    UNDERLINE
  );
  ECanvasFontStyles = set of ECanvasFontStyle;
  {$scopedenums off}

// Loads every .ttf in Dir (not its subdirectories). False if none could be read.
function SimbaImage_LoadFontsInDir(Dir: String): Boolean;
// Every font name loaded
function SimbaImage_FontNames: TStringArray;
// return the text width/height in pixels
function SimbaImage_MeasureText(FontName: String;
                             FontSize: Single;
                             FontStyles: ECanvasFontStyles;
                             Text: String): TPoint;

// draw at (x,y)
procedure SimbaImage_DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                              Color: TColorBGRA;
                              FontName: String;
                              FontSize: Single;
                              FontStyles: ECanvasFontStyles;
                              Text: String;
                              Position: TPoint); overload;

// draw in box, with alignments
procedure SimbaImage_DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                              Color: TColorBGRA;
                              FontName: String;
                              FontSize: Single;
                              FontStyles: ECanvasFontStyles;
                              Text: String;
                              Box: TBox;
                              Alignments: ECanvasTextAligns); overload;

implementation

uses
  syncobjs,
  Math, EasyLazFreeType, LazUTF8,
  simba.image_utils, simba.vartype_string, simba.image_fonts;

type
  TSimbaFreeTypeFont = class
  protected
  const
    SUBPIXEL_POSITIONS = 4; // glyphs are cached at this many horizontal sub-pixel offsets
  protected
  type
    // cached rasterized glyph (coverage)
    TGlyphBitmap = record
      Rasterized: Boolean;
      Coverage: TByteArray;
      Width, Height: Integer;
      OffsetX, OffsetY: Integer;

      // FreeType's span callback while this glyph is rasterized: copies one span into Coverage
      procedure CaptureSpan(X, Y, TX: Integer; Data: Pointer);
    end;
    PGlyphBitmap = ^TGlyphBitmap;

    // Codepoint to index
    TCharInfo = record
      Known: Boolean;
      GlyphIndex: Integer; // -1 when the font has nothing to draw for it
      Advance: Single;
    end;
    PCharInfo = ^TCharInfo;
  protected
    FName: String;
    FSize: Single;
    FStyles: ECanvasFontStyles;
    FHandle: TFreeTypeFont;
    FUnhinted: TFreeTypeFont; // opened the first time a glyph fails to load hinted
    FAscent: Single;
    FLineFullHeight: Single;
    FUnderlineTop: Single;       // pixels below the baseline
    FUnderlineThickness: Single; // pixels, at least one

    FGlyphs: array of TGlyphBitmap; // by glyph index * SUBPIXEL_POSITIONS + sub-pixel
    FChars: array of TCharInfo;     // by codepoint
    FRow: array of TColorBGRA;      // BlendGlyph's scratch row

    function LoadGlyph(GlyphIndex: Integer): TFreeTypeGlyph;
    function CachedGlyph(GlyphIndex, SubPixel: Integer): PGlyphBitmap;
    function CachedChar(Codepoint: Integer): PCharInfo;

    function LineWidth(const Text: String): Single;
    procedure BlitGlyph(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Bitmap: TGlyphBitmap; X, Y: Integer; const Color: TColorBGRA);
    procedure BlendGlyph(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Bitmap: TGlyphBitmap; X, Y: Integer; const Color: TColorBGRA);
    procedure DrawUnderline(Data: PColorBGRA; PixelsPerRow, Height: Integer; X, Width: Single; Baseline: Integer; const Color: TColorBGRA);
    procedure DrawLine(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Text: String; X: Single; Baseline: Integer; const Color: TColorBGRA);
  public
    constructor Create(AName: String; ASize: Single; AStyles: ECanvasFontStyles); reintroduce;
    destructor Destroy; override;

    function MeasureText(const Text: String): TPoint;
    procedure DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Text: String; X, Y: Integer; const Color: TColorBGRA);
    procedure DrawTextInBox(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Text: String; const Box: TBox; Alignments: ECanvasTextAligns; const Color: TColorBGRA);
  end;

constructor TSimbaFreeTypeFont.Create(AName: String; ASize: Single; AStyles: ECanvasFontStyles);
begin
  inherited Create();

  FName := AName;
  FSize := ASize;
  FStyles := AStyles;

  FHandle := SimbaFonts_OpenFont(AName, ASize, ECanvasFontStyle.BOLD in AStyles, ECanvasFontStyle.ITALIC in AStyles, ECanvasFontStyle.ANTIALIASED in AStyles);

  FAscent := FHandle.Ascent;
  FLineFullHeight := FHandle.LineFullHeight;
  FUnderlineTop := FAscent * 0.12;
  FUnderlineThickness := Max(FAscent * 0.08, 1);
end;

destructor TSimbaFreeTypeFont.Destroy;
begin
  FHandle.Free();
  FUnhinted.Free();

  inherited Destroy();
end;

procedure TSimbaFreeTypeFont.TGlyphBitmap.CaptureSpan(X, Y, TX: Integer; Data: Pointer);
begin
  if (Y < OffsetY) or (Y >= OffsetY + Height) or (X < OffsetX) or (X + TX > OffsetX + Width) then
    Exit;

  Move(Data^, Coverage[(Y - OffsetY) * Width + (X - OffsetX)], TX);
end;

// The hinted glyph, or the unhinted one when hinting fails to load it.
// nil if neither loads.
function TSimbaFreeTypeFont.LoadGlyph(GlyphIndex: Integer): TFreeTypeGlyph;
begin
  Result := FHandle.Glyph[GlyphIndex];
  if (Result = nil) or Result.Loaded then
    Exit;

  if (FUnhinted = nil) then
  begin
    FUnhinted := SimbaFonts_OpenFont(FName, FSize, ECanvasFontStyle.BOLD in FStyles, ECanvasFontStyle.ITALIC in FStyles, ECanvasFontStyle.ANTIALIASED in FStyles);
    FUnhinted.Hinted := False;
  end;

  Result := FUnhinted.Glyph[GlyphIndex];
  if (Result <> nil) and (not Result.Loaded) then
    Result := nil;
end;

function TSimbaFreeTypeFont.CachedGlyph(GlyphIndex, SubPixel: Integer): PGlyphBitmap;
var
  Slot: Integer;
  Glyph: TFreeTypeGlyph;
  Bounds: TRect;
begin
  Slot := GlyphIndex * SUBPIXEL_POSITIONS + SubPixel;
  if (Slot >= Length(FGlyphs)) then
    SetLength(FGlyphs, Slot + 1);

  Result := @FGlyphs[Slot];
  if Result^.Rasterized then
    Exit;
  Result^.Rasterized := True;

  Glyph := LoadGlyph(GlyphIndex);
  if (Glyph = nil) then
    Exit;

  Bounds := Glyph.BoundsWithOffset[SubPixel / SUBPIXEL_POSITIONS, 0];
  if (Bounds.Right <= Bounds.Left) or (Bounds.Bottom <= Bounds.Top) then
    Exit;

  Result^.OffsetX := Bounds.Left;
  Result^.OffsetY := Bounds.Top;
  Result^.Width := Bounds.Right - Bounds.Left;
  Result^.Height := Bounds.Bottom - Bounds.Top;
  SetLength(Result^.Coverage, Result^.Width * Result^.Height);

  Glyph.RenderDirectly(
    SubPixel / SUBPIXEL_POSITIONS,
    0,
    Bounds,
    @Result^.CaptureSpan,
    FHandle.Quality,
    False
  );
end;

function TSimbaFreeTypeFont.CachedChar(Codepoint: Integer): PCharInfo;
var
  Glyph: TFreeTypeGlyph;
begin
  if (Codepoint >= Length(FChars)) then
    SetLength(FChars, Codepoint + 1);

  Result := @FChars[Codepoint];
  if (not Result^.Known) then
  begin
    Result^.Known := True;
    Result^.GlyphIndex := FHandle.CharIndex[Codepoint];

    Glyph := LoadGlyph(Result^.GlyphIndex);
    if (Glyph = nil) then // nothing to draw, and nothing to advance by
    begin
      Result^.GlyphIndex := -1;
      Result^.Advance := 0;
    end else
      Result^.Advance := Glyph.Advance;
  end;
end;

function TSimbaFreeTypeFont.LineWidth(const Text: String): Single;
var
  Str: PChar;
  Remaining, DecodedLen, CharCode: Integer;
begin
  Result := 0;

  Str := PChar(Text);
  Remaining := Length(Text);
  while (Remaining > 0) do
  begin
    if (Str^ < #128) then
    begin
      CharCode := Ord(Str^);
      Inc(Str);
      Dec(Remaining);
    end else
    begin
      CharCode := UTF8CodepointToUnicode(Str, DecodedLen);
      Inc(Str, DecodedLen);
      Dec(Remaining, DecodedLen);
    end;

    if (CharCode < Length(FChars)) and FChars[CharCode].Known then
      Result := Result + FChars[CharCode].Advance
    else
      Result := Result + CachedChar(CharCode)^.Advance;
  end;
end;

procedure TSimbaFreeTypeFont.BlitGlyph(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Bitmap: TGlyphBitmap; X, Y: Integer; const Color: TColorBGRA);
var
  X1, Y1, X2, Y2, Row: Integer;
  Src, SrcEnd: PByte;
  Dest: PColorBGRA;
  Pixel: TColorBGRA;
begin
  X1 := Max(X, 0);
  Y1 := Max(Y, 0);
  X2 := Min(X + Bitmap.Width - 1, PixelsPerRow - 1);
  Y2 := Min(Y + Bitmap.Height - 1, Height - 1);
  if (X1 > X2) or (Y1 > Y2) then
    Exit;

  Pixel := Color;
  for Row := Y1 to Y2 do
  begin
    Src := @Bitmap.Coverage[(Row - Y) * Bitmap.Width + (X1 - X)];
    SrcEnd := Src + (X2 - X1 + 1);
    Dest := @Data[Int64(Row * PixelsPerRow + X1)];

    while (Src < SrcEnd) do
    begin
      // fully covered pixels are written, partly covered ones blended by their coverage
      if (Src^ = ALPHA_OPAQUE) then
        Dest^ := Color
      else
      if (Src^ > 0) then
      begin
        Pixel.A := Src^;
        BlendPixel(Dest, @Pixel);
      end;

      Inc(Src);
      Inc(Dest);
    end;
  end;
end;

procedure TSimbaFreeTypeFont.BlendGlyph(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Bitmap: TGlyphBitmap; X, Y: Integer; const Color: TColorBGRA);
var
  X1, Y1, X2, Y2, Row, Col, Count: Integer;
  Src: PByte;
  Dest: PColorBGRA;
begin
  X1 := Max(X, 0);
  Y1 := Max(Y, 0);
  X2 := Min(X + Bitmap.Width - 1, PixelsPerRow - 1);
  Y2 := Min(Y + Bitmap.Height - 1, Height - 1);
  if (X1 > X2) or (Y1 > Y2) then
    Exit;

  Count := X2 - X1 + 1;
  if (Count > Length(FRow)) then
    SetLength(FRow, Count);
  FillData(@FRow[0], Count, Color); // only the alphas change from row to row

  for Row := Y1 to Y2 do
  begin
    Src := @Bitmap.Coverage[(Row - Y) * Bitmap.Width + (X1 - X)];
    Dest := @FRow[0];

    // each pixel's coverage is its alpha
    Col := Count;
    while (Col > 0) do
    begin
      Dest^.A := Src^;

      Inc(Src);
      Inc(Dest);
      Dec(Col);
    end;

    BlendData(@Data[Int64(Row * PixelsPerRow + X1)], @FRow[0], Count);
  end;
end;

procedure TSimbaFreeTypeFont.DrawUnderline(Data: PColorBGRA; PixelsPerRow, Height: Integer; X, Width: Single; Baseline: Integer; const Color: TColorBGRA);
var
  Top: Single;
  X1, Y1, X2, Y2, Row, Count: Integer;
begin
  Top := Baseline + FUnderlineTop;

  X1 := Max(Round(X), 0);
  Y1 := Max(Round(Top), 0);
  X2 := Min(Round(X + Width), PixelsPerRow) - 1;
  Y2 := Min(Round(Top + FUnderlineThickness), Height) - 1;
  if (X1 > X2) or (Y1 > Y2) then
    Exit;

  Count := X2 - X1 + 1;
  for Row := Y1 to Y2 do
    FillData(@Data[Int64(Row * PixelsPerRow + X1)], Count, Color);
end;

procedure TSimbaFreeTypeFont.DrawLine(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Text: String; X: Single; Baseline: Integer; const Color: TColorBGRA);
var
  Str: PChar;
  Remaining, DecodedLen, CharCode, PenX, Slot: Integer;
  Info: PCharInfo;
  Bitmap: PGlyphBitmap;
begin
  // first, so the glyphs draw over it
  if (ECanvasFontStyle.UNDERLINE in FStyles) then
    DrawUnderline(Data, PixelsPerRow, Height, X, LineWidth(Text), Baseline, Color);

  Str := PChar(Text);
  Remaining := Length(Text);
  while (Remaining > 0) do
  begin
    if (Str^ < #128) then
    begin
      CharCode := Ord(Str^);
      Inc(Str);
      Dec(Remaining);
    end else
    begin
      CharCode := UTF8CodepointToUnicode(Str, DecodedLen);
      Inc(Str, DecodedLen);
      Dec(Remaining, DecodedLen);
    end;

    if (CharCode < Length(FChars)) and FChars[CharCode].Known then
      Info := @FChars[CharCode]
    else
      Info := CachedChar(CharCode);

    if (Info^.GlyphIndex >= 0) then
    begin
      PenX := Trunc(X);
      if (X < PenX) then
        Dec(PenX);
      Slot := Info^.GlyphIndex * SUBPIXEL_POSITIONS + Min(Trunc((X - PenX) * SUBPIXEL_POSITIONS), SUBPIXEL_POSITIONS - 1);
      X := X + Info^.Advance;

      if (Slot < Length(FGlyphs)) and FGlyphs[Slot].Rasterized then
        Bitmap := @FGlyphs[Slot]
      else
        Bitmap := CachedGlyph(Slot div SUBPIXEL_POSITIONS, Slot mod SUBPIXEL_POSITIONS);

      if (Bitmap^.Width > 0) then
      begin
        if (ECanvasFontStyle.ANTIALIASED in FStyles) then
          BlendGlyph(Data, PixelsPerRow, Height, Bitmap^, PenX + Bitmap^.OffsetX, Baseline + Bitmap^.OffsetY, Color)
        else
          BlitGlyph(Data, PixelsPerRow, Height, Bitmap^, PenX + Bitmap^.OffsetX, Baseline + Bitmap^.OffsetY, Color);
      end;
    end;
  end;
end;

function TSimbaFreeTypeFont.MeasureText(const Text: String): TPoint;
var
  Lines: TStringArray;
  I: Integer;
begin
  Lines := Text.SplitLines();

  Result.X := 0;
  for I := 0 to High(Lines) do
    Result.X := Max(Result.X, Round(LineWidth(Lines[I])));
  Result.Y := Length(Lines) * Round(FLineFullHeight);
end;

procedure TSimbaFreeTypeFont.DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Text: String; X, Y: Integer; const Color: TColorBGRA);
var
  Lines: TStringArray;
  I, Baseline: Integer;
begin
  Baseline := Round(Y + FAscent);

  if (Pos(#10, Text) = 0) then // one line: nothing to split
    DrawLine(Data, PixelsPerRow, Height, Text, X, Baseline, Color)
  else
  begin
    Lines := Text.SplitLines();
    for I := 0 to High(Lines) do
    begin
      DrawLine(Data, PixelsPerRow, Height, Lines[I], X, Baseline, Color);
      Baseline := Baseline + Round(FLineFullHeight);
    end;
  end;
end;

procedure TSimbaFreeTypeFont.DrawTextInBox(Data: PColorBGRA; PixelsPerRow, Height: Integer; const Text: String; const Box: TBox; Alignments: ECanvasTextAligns; const Color: TColorBGRA);
var
  Paragraphs: TStringArray;
  Lines: TStringArray;
  Remaining: String;
  MaxWidth, Top, X: Single;
  I, Baseline: Integer;
begin
  MaxWidth := Box.X2 - Box.X1;
  if (Text = '') or (MaxWidth <= 0) then
    Exit;

  // word wrap each paragraph into lines
  Paragraphs := Text.SplitLines();
  for I := 0 to High(Paragraphs) do
  begin
    Remaining := Paragraphs[I];
    repeat
      SetLength(Lines, Length(Lines) + 1);
      Lines[High(Lines)] := Remaining;
      FHandle.SplitText(Lines[High(Lines)], MaxWidth, Remaining);
    until (Remaining = '');
  end;

  if (ECanvasTextAlign.BOTTOM in Alignments) then
    Top := Box.Y2 - FLineFullHeight * (Length(Lines) - 0.5)
  else
  if (ECanvasTextAlign.CENTER in Alignments) and not (ECanvasTextAlign.TOP in Alignments) then
    Top := (Box.Y1 + Box.Y2) / 2 - FLineFullHeight * (Length(Lines) / 2 - 0.5)
  else
    Top := Box.Y1 + FLineFullHeight * 0.5;
  Baseline := Round(Top + (FAscent - FLineFullHeight * 0.5));

  for I := 0 to High(Lines) do
  begin
    if (ECanvasTextAlign.RIGHT in Alignments) then
      X := Box.X2 + Round(-LineWidth(Lines[I]))
    else
    if (ECanvasTextAlign.CENTER in Alignments) and not (ECanvasTextAlign.LEFT in Alignments) then
      X := (Box.X1 + Box.X2) / 2 + Round(-LineWidth(Lines[I]) / 2)
    else
      X := Box.X1;

    DrawLine(Data, PixelsPerRow, Height, Lines[I], X, Baseline, Color);
    Baseline := Baseline + Round(FLineFullHeight);
  end;
end;

type
  TSimbaTextDrawer = class
  protected
    FLock: TCriticalSection;
    FFonts: array of TSimbaFreeTypeFont;

    function GetFont(FontName: String; FontSize: Single; FontStyles: ECanvasFontStyles): TSimbaFreeTypeFont;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadFontsInDir(Dir: String): Boolean;
    function FontNames: TStringArray;

    function MeasureText(FontName: String;
                         FontSize: Single;
                         FontStyles: ECanvasFontStyles;
                         Text: String): TPoint;

    procedure DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                       Color: TColorBGRA;
                       FontName: String; FontSize: Single;
                       FontStyles: ECanvasFontStyles;
                       Text: String; Position: TPoint); overload;

    procedure DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                       Color: TColorBGRA;
                       FontName: String; FontSize: Single;
                       FontStyles: ECanvasFontStyles;
                       Text: String; Box: TBox; Alignments: ECanvasTextAligns); overload;
  end;

var
  SimbaTextDrawer: TSimbaTextDrawer = nil;

constructor TSimbaTextDrawer.Create;
begin
  inherited Create();

  FLock := TCriticalSection.Create();
end;

destructor TSimbaTextDrawer.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FFonts) do
    FFonts[I].Free();
  FreeAndNil(FLock);

  inherited Destroy();
end;

function TSimbaTextDrawer.GetFont(FontName: String; FontSize: Single; FontStyles: ECanvasFontStyles): TSimbaFreeTypeFont;
var
  I: Integer;
begin
  for I := 0 to High(FFonts) do
    if (FFonts[I].FName = FontName) and
       (FFonts[I].FSize = FontSize) and
       (FFonts[I].FStyles = FontStyles) then
      Exit(FFonts[I]);

  Result := TSimbaFreeTypeFont.Create(FontName, FontSize, FontStyles);
  FFonts += [Result];
end;

function TSimbaTextDrawer.LoadFontsInDir(Dir: String): Boolean;
begin
  FLock.Enter();
  try
    Result := SimbaFonts_LoadFontsInDir(Dir);
  finally
    FLock.Leave();
  end;
end;

function TSimbaTextDrawer.FontNames: TStringArray;
begin
  FLock.Enter();
  try
    Result := SimbaFonts_FontNames();
  finally
    FLock.Leave();
  end;
end;

function TSimbaTextDrawer.MeasureText(FontName: String; FontSize: Single; FontStyles: ECanvasFontStyles; Text: String): TPoint;
begin
  FLock.Enter();
  try
    Result := GetFont(FontName, FontSize, FontStyles).MeasureText(Text);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaTextDrawer.DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                                    Color: TColorBGRA;
                                    FontName: String; FontSize: Single;
                                    FontStyles: ECanvasFontStyles;
                                    Text: String; Position: TPoint);
begin
  Color.A := ALPHA_OPAQUE; // dont support drawing text with a non opaque alpha

  FLock.Enter();
  try
    with GetFont(FontName, FontSize, FontStyles) do
      DrawText(Data, PixelsPerRow, Height, Text, Position.X, Position.Y, Color);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaTextDrawer.DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                                    Color: TColorBGRA;
                                    FontName: String; FontSize: Single;
                                    FontStyles: ECanvasFontStyles;
                                    Text: String; Box: TBox;
                                    Alignments: ECanvasTextAligns);
begin
  Color.A := ALPHA_OPAQUE; // dont support drawing text with a non opaque alpha

  FLock.Enter();
  try
    with GetFont(FontName, FontSize, FontStyles) do
      DrawTextInBox(Data, PixelsPerRow, Height, Text, Box, Alignments, Color);
  finally
    FLock.Leave();
  end;
end;

function SimbaImage_LoadFontsInDir(Dir: String): Boolean;
begin
  Result := SimbaTextDrawer.LoadFontsInDir(Dir);
end;

function SimbaImage_FontNames: TStringArray;
begin
  Result := SimbaTextDrawer.FontNames();
end;

function SimbaImage_MeasureText(FontName: String; FontSize: Single; FontStyles: ECanvasFontStyles; Text: String): TPoint;
begin
  Result := SimbaTextDrawer.MeasureText(FontName, FontSize, FontStyles, Text);
end;

procedure SimbaImage_DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                              Color: TColorBGRA;
                              FontName: String; FontSize: Single;
                              FontStyles: ECanvasFontStyles;
                              Text: String; Position: TPoint);
begin
  SimbaTextDrawer.DrawText(Data, PixelsPerRow, Height, Color, FontName, FontSize, FontStyles, Text, Position);
end;

procedure SimbaImage_DrawText(Data: PColorBGRA; PixelsPerRow, Height: Integer;
                              Color: TColorBGRA;
                              FontName: String; FontSize: Single;
                              FontStyles: ECanvasFontStyles;
                              Text: String; Box: TBox; Alignments: ECanvasTextAligns);
begin
  SimbaTextDrawer.DrawText(Data, PixelsPerRow, Height, Color, FontName, FontSize, FontStyles, Text, Box, Alignments);
end;

initialization
  SimbaTextDrawer := TSimbaTextDrawer.Create();

finalization
  FreeAndNil(SimbaTextDrawer);

end.
