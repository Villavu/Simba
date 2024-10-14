{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_textdrawer;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, FPImage, LazFreeTypeFPImageDrawer, EasyLazFreeType,
  simba.base, simba.threading;

type
  TSimbaFreeTypeFontLoader = class
  protected
  type
    TFontCacheEntry = record
      Font: TFreeTypeFont;
      Name: String;
    end;
    TFontCache = array of TFontCacheEntry;
  protected
    FFonts: TFontCache;
    FFontNames: TStringArray;
    FLock: TEnterableLock;
    FSystemFontsLoaded: Boolean;

    procedure LoadSystemFonts;

    function GetFontNames: TStringArray;
  public
    function LoadFonts(Dir: String): Boolean;
    function GetFont(AName: String; ASize: Single; AAntialised, ABold, AItalic: Boolean): TFreeTypeFont;

    property FontNames: TStringArray read GetFontNames;
  end;

  {$scopedenums on}
  EImageTextAlign = set of (LEFT, CENTER, RIGHT, JUSTIFY, TOP, VERTICAL_CENTER, BASE_LINE, BOTTOM);
  {$scopedenums off}

  TSimbaTextDrawerBase = class(TFPImageFreeTypeDrawer)
  protected
    FFont: TFreeTypeFont;
    FFontName: String;
    FFontAntialised: Boolean;
    FSize: Single;
    FBold: Boolean;
    FItalic: Boolean;

    procedure MoveToPixel(X, Y: Integer); override;
    function GetCurrentColor: TFPColor; override;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); override;
    procedure MoveRight; override;
    function GetClipRect: TRect; override;

    procedure BeginDrawing; virtual;
    procedure EndDrawing; virtual;
  public
    constructor Create; reintroduce;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign; Color: TColor); overload;

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    property Font: String read FFontName write FFontName;
    property Size: Single read FSize write FSize;
    property Antialiased: Boolean read FFontAntialised write FFontAntialised;
    property Bold: Boolean read FBold write FBold;
    property Italic: Boolean read FItalic write FItalic;
  end;

  TSimbaTextDrawer = class(TSimbaTextDrawerBase)
  protected
    FClipRect: TRect;
    FWidth: Integer;
    FHeight: Integer;
    FData: PColorBGRA;
    FCurrentX, FCurrentY: Integer;
    FCurrentColor: PColorBGRA;
    FSimbaImage: TObject;

    FLock: TEnterableLock;
    FDrawn: Boolean;
    FDrawnBox: TBox;

    procedure MoveToPixel(X, Y: Integer); override;
    function GetCurrentColor: TFPColor; override;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); override;
    procedure MoveRight; override;
    function GetClipRect: TRect; override;

    procedure BeginDrawing; override;
    procedure EndDrawing; override;
  public
    property Drawn: Boolean read FDrawn;
    property DrawnBox: TBox read FDrawnBox;

    constructor Create(SimbaImage: TObject); reintroduce;
  end;

var
  SimbaFreeTypeFontLoader: TSimbaFreeTypeFontLoader = nil;

implementation

uses
  Forms, FileUtil, LazFileUtils, LazFreeTypeFontCollection,
  simba.image, simba.image_utils, simba.vartype_box;

function TSimbaFreeTypeFontLoader.GetFontNames: TStringArray;
begin
  LoadSystemFonts();

  Result := FFontNames;
end;

procedure TSimbaFreeTypeFontLoader.LoadSystemFonts;
var
  SearchPaths, FontFiles: TStringList;
  I: Integer;
begin
  if FSystemFontsLoaded then
    Exit;
  FSystemFontsLoaded := True;

  SearchPaths := nil;
  FontFiles := nil;

  try
    SearchPaths := TStringList.Create();
    SearchPaths.Add(Application.Location);

    {$IFDEF WINDOWS}
    SearchPaths.Add(SHGetFolderPathUTF8(20));
    {$ENDIF}

    {$IFDEF LINUX}
    SearchPaths.Add('/usr/share/fonts/');
    SearchPaths.Add('/usr/local/share/fonts');
    SearchPaths.Add(GetUserDir() + '.fonts/');
    SearchPaths.Add(GetUserDir() + '.local/share/fonts/');
    {$ENDIF}

    {$IFDEF DARWIN}
    SearchPaths.Add('/Library/Fonts/');
    SearchPaths.Add('/System/Library/Fonts/');
    {$ENDIF}

    FontFiles := TStringList.Create();
    FontFiles.Duplicates := dupIgnore;
    FontFiles.Sorted := True;

    for I := 0 to SearchPaths.Count - 1 do
      FindAllFiles(FontFiles, ExpandFileName(SearchPaths[I]), '*.ttf');

    FontCollection.BeginUpdate();
    try
      for I := 0 to FontFiles.Count - 1 do
      try
        FontCollection.AddFile(FontFiles[I]);
      except
        // ignore exceptions due to font read errors
      end;
    finally
      FontCollection.EndUpdate();
    end;

    FFontNames := [];
    with FontCollection.FamilyEnumerator do
      while MoveNext() do
        FFontNames += [Current.FamilyName];
  finally
    if Assigned(SearchPaths) then
      SearchPaths.Free();
    if Assigned(FontFiles) then
      FontFiles.Free();
  end;
end;

function TSimbaFreeTypeFontLoader.LoadFonts(Dir: String): Boolean;
var
  FontFiles: TStringList;
  I: Integer;
begin
  Result := False;

  FLock.Enter();
  try
    FontFiles := FindAllFiles(ExpandFileName(Dir), '*.ttf', False);

    FontCollection.BeginUpdate();
    try
      for I := 0 to FontFiles.Count - 1 do
      try
        FontCollection.AddFile(FontFiles[I]);

        Result := True;
      except
        // ignore exceptions due to font read errors
      end;
    finally
      FontCollection.EndUpdate();
    end;

    FFontNames := [];
    with FontCollection.FamilyEnumerator do
      while MoveNext() do
        FFontNames += [Current.FamilyName];
  finally
    if Assigned(FontFiles) then
      FontFiles.Free();

    FLock.Leave();
  end;
end;

function TSimbaFreeTypeFontLoader.GetFont(AName: String; ASize: Single; AAntialised, ABold, AItalic: Boolean): TFreeTypeFont;
var
  I: Integer;
  Entry: TFontCacheEntry;
  Style: TFreeTypeStyles;
  StyleAsStr: String;
  Quality: TGlyphRenderQuality;
  FamilyItem: TCustomFamilyCollectionItem;
  FontItem: TCustomFontCollectionItem;
begin
  Result := nil;

  FLock.Enter();
  try
    LoadSystemFonts();

    if AAntialised then
      Quality := grqHighQuality
    else
      Quality := grqMonochrome;

    Style := [];
    if ABold   then Style += [ftsBold];
    if AItalic then Style += [ftsItalic];

    for I := 0 to High(FFonts) do
    begin
      Entry := FFonts[I];

      if (Result = nil) and
         (Entry.Name = AName) and
         (Entry.Font.SizeInPixels = ASize) and
         (Entry.Font.Style = Style) and
         (Entry.Font.ClearType = AAntialised) and
         (Entry.Font.Quality = Quality) then
        begin
          Result := Entry.Font;
          Exit;
        end;
    end;

    StyleAsStr := '';
    if ABold   then StyleAsStr += ' Bold';
    if AItalic then StyleAsStr += ' Italic';

    FamilyItem := FontCollection.Family[AName];
    if Assigned(FamilyItem) then
    begin
      FontItem := FamilyItem.GetFont(StyleAsStr);

      if Assigned(FontItem) then
      begin
        Result := FontItem.CreateFont();

        Entry.Name := AName;
        Entry.Font := Result;
        Entry.Font.Style := Style;
        Entry.Font.SizeInPixels := ASize;
        Entry.Font.KerningEnabled := False;
        Entry.Font.Quality := Quality;
        Entry.Font.ClearType := AAntialised;

        FFonts += [Entry];
      end;
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaTextDrawerBase.MoveToPixel(X, Y: Integer);
begin
  SimbaException('MoveToPixel');
end;

function TSimbaTextDrawerBase.GetCurrentColor: TFPColor;
begin
  SimbaException('GetCurrentColor');
end;

procedure TSimbaTextDrawerBase.SetCurrentColorAndMoveRight(const AColor: TFPColor);
begin
  SimbaException('SetCurrentColorAndMoveRight');
end;

procedure TSimbaTextDrawerBase.MoveRight;
begin
  SimbaException('MoveRight');
end;

function TSimbaTextDrawerBase.GetClipRect: TRect;
begin
  SimbaException('GetClipRect');
end;

procedure TSimbaTextDrawerBase.BeginDrawing;
begin
  FFont := SimbaFreeTypeFontLoader.GetFont(FFontName, FSize, FFontAntialised, FBold, FItalic);
  if (FFont = nil) then
    SimbaException('Font "%s" not found', [FFontName]);
end;

procedure TSimbaTextDrawerBase.EndDrawing;
begin

end;

constructor TSimbaTextDrawerBase.Create;
begin
  FSize := 20;
  FFontAntialised := False;
  //FFontName := GetDefaultFontName();
end;

procedure TSimbaTextDrawerBase.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  BeginDrawing();
  try
    inherited DrawText(Text, FFont, Position.X, Position.Y + FFont.SizeInPoints, TColorToFPColor(Color));
  finally
    EndDrawing();
  end;
end;

procedure TSimbaTextDrawerBase.DrawText(Text: String; Box: TBox; Alignments: EImageTextAlign; Color: TColor);
var
  FreeTypeAlignments: TFreeTypeAlignments absolute Alignments;
begin
  BeginDrawing();
  try
    inherited DrawTextRect(Text, FFont, Box.X1, Box.Y1, Box.X2, Box.Y2, TColorToFPColor(Color), FreeTypeAlignments);
  finally
    EndDrawing();
  end;
end;

function TSimbaTextDrawerBase.TextWidth(Text: String): Integer;
begin
  BeginDrawing();

  try
    Result := Round(FFont.TextWidth(Text));
  finally
    EndDrawing();
  end;
end;

function TSimbaTextDrawerBase.TextHeight(Text: String): Integer;
begin
  BeginDrawing();

  try
    Result := Round(FFont.TextHeight(Text));
  finally
    EndDrawing();
  end;
end;

function TSimbaTextDrawerBase.TextSize(Text: String): TPoint;
begin
  BeginDrawing();

  try
    Result.X := Round(FFont.TextWidth(Text));
    Result.Y := Round(FFont.TextHeight(Text));
  finally
    EndDrawing();
  end;
end;

procedure TSimbaTextDrawer.MoveToPixel(X, Y: Integer);
begin
  FCurrentColor := @FData[Y * FWidth + X];
  FCurrentX := X;
  FCurrentY := Y;
end;

function TSimbaTextDrawer.GetCurrentColor: TFPColor;
begin
  with FCurrentColor^ do
  begin
    Result.Red   := R + (R shr 8); // TFPColor fields are 16 bits. So duplicate our 8 bit data
    Result.Green := G + (G shr 8);
    Result.Blue  := B + (B shr 8);
    Result.Alpha := A + (A shr 8);
  end;
end;

procedure TSimbaTextDrawer.SetCurrentColorAndMoveRight(const AColor: TFPColor);
var
  BGRA: TColorBGRA;
begin
  BGRA.R := AColor.Red div 257;
  BGRA.G := AColor.Green div 257;
  BGRA.B := AColor.Blue div 257;
  BGRA.A := AColor.Alpha shr 8;

  BlendPixel(FCurrentColor, BGRA);

  if FDrawn then
  begin
    if (FCurrentX > FDrawnBox.X2) then FDrawnBox.X2 := FCurrentX else
    if (FCurrentX < FDrawnBox.X1) then FDrawnBox.X1 := FCurrentX;
    if (FCurrentY > FDrawnBox.Y2) then FDrawnBox.Y2 := FCurrentY else
    if (FCurrentY < FDrawnBox.Y1) then FDrawnBox.Y1 := FCurrentY;
  end else
  begin
    FDrawn := True;
    FDrawnBox := TBox.Create(FCurrentX, FCurrentY, FCurrentX, FCurrentY);
  end;

  Inc(FCurrentColor);
  Inc(FCurrentX);
end;

procedure TSimbaTextDrawer.MoveRight;
begin
  Inc(FCurrentColor);
  Inc(FCurrentX);
end;

function TSimbaTextDrawer.GetClipRect: TRect;
begin
  Result := FClipRect;
end;

procedure TSimbaTextDrawer.BeginDrawing;
begin
  FLock.Enter();

  with TSimbaImage(FSimbaImage) do
  begin
    Self.FData := Data;
    Self.FWidth := Width;
    Self.FHeight := Height;
  end;

  FClipRect.Right := FWidth;
  FClipRect.Bottom := FHeight;

  FDrawn := False;

  FFont := SimbaFreeTypeFontLoader.GetFont(FFontName, FSize, FFontAntialised, FBold, FItalic);
  if (FFont = nil) then
    SimbaException('Font "%s" not found', [FFontName]);
end;

procedure TSimbaTextDrawer.EndDrawing;
begin
  FLock.Leave();
end;

constructor TSimbaTextDrawer.Create(SimbaImage: TObject);
begin
  inherited Create();

  FSimbaImage := SimbaImage;
end;

initialization
  SimbaFreeTypeFontLoader := TSimbaFreeTypeFontLoader.Create();

finalization
  if Assigned(SimbaFreeTypeFontLoader) then
    FreeAndNil(SimbaFreeTypeFontLoader);

end.

