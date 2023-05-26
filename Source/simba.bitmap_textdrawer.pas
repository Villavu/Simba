{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.bitmap_textdrawer;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics, FPImage, LazFreeTypeFPImageDrawer, EasyLazFreeType,
  simba.mufasatypes, simba.simplelock;

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
    FLock: TSimpleEnterableLock;
    FSystemFontsLoaded: Boolean;

    procedure LoadSystemFonts;
  public
    function LoadFonts(Dir: String): Boolean;
    function GetFont(AName: String; ASize: Single; AAntialised, ABold, AItalic: Boolean): TFreeTypeFont;

    property FontNames: TStringArray read FFontNames;
  end;

  TSimbaTextDrawer = class(TFPImageFreeTypeDrawer)
  protected
    FWidth: Integer;
    FHeight: Integer;
    FData: PColorBGRA;
    FCurrentColor: PColorBGRA;
    FBitmap: TObject;
    FFonts: TStringArray;
    FFont: TFreeTypeFont;
    FFontName: String;
    FFontAntialised: Boolean;
    FSize: Single;
    FClipRect: TRect;
    FBold: Boolean;
    FItalic: Boolean;
    FLock: TSimpleEnterableLock;

    procedure MoveToPixel(X, Y: Integer); override;
    function GetCurrentColor: TFPColor; override;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); override;
    procedure MoveRight; override;
    function GetClipRect: TRect; override;
    procedure BeginDrawing;
    procedure EndDrawing;
  public
    property FontName: String read FFontName write FFontName;
    property Size: Single read FSize write FSize;
    property Antialiased: Boolean read FFontAntialised write FFontAntialised;
    property Bold: Boolean read FBold write FBold;
    property Italic: Boolean read FItalic write FItalic;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor); overload;

    function TextWidth(Text: String): Integer;
    function TextHeight(Text: String): Integer;
    function TextSize(Text: String): TPoint;

    constructor Create(Bitmap: TObject); reintroduce;
  end;

var
  SimbaFreeTypeFontLoader: TSimbaFreeTypeFontLoader = nil;

implementation

uses
  Forms, FileUtil, LazFileUtils, LazFreeTypeFontCollection,
  simba.bitmap;

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

procedure TSimbaTextDrawer.MoveToPixel(X, Y: Integer);
begin
  FCurrentColor := @FData[Y * FWidth + X];
end;

function TSimbaTextDrawer.GetCurrentColor: TFPColor;
begin
  with FCurrentColor^ do
  begin
    Result.Red   := (R shl 8) or R;     // TFPColor fields are 16 bits. So duplicate our 8 bit data
    Result.Green := (G shl 8) or G;
    Result.Blue  := (B shl 8) or B;
    Result.Alpha := (255 shl 8) or 255;
  end;
end;

procedure TSimbaTextDrawer.SetCurrentColorAndMoveRight(const AColor: TFPColor);
begin
  FCurrentColor^ := TColorBGRA(((AColor.Blue shr 8) and $FF) or (AColor.Green and $FF00) or ((AColor.Red shl 8) and $FF0000) or (AColor.Alpha and $FF000000));

  Inc(FCurrentColor);
end;

procedure TSimbaTextDrawer.MoveRight;
begin
  Inc(FCurrentColor);
end;

function TSimbaTextDrawer.GetClipRect: TRect;
begin
  Result := FClipRect;
end;

procedure TSimbaTextDrawer.BeginDrawing;
begin
  FLock.Enter();

  with TMufasaBitmap(FBitmap) do
  begin
    Self.FData := Data;
    Self.FWidth := Width;
    Self.FHeight := Height;
  end;

  FClipRect.Right := FWidth;
  FClipRect.Bottom := FHeight;

  FFont := SimbaFreeTypeFontLoader.GetFont(FFontName, FSize, FFontAntialised, FBold, FItalic);
  if (FFont = nil) then
    SimbaException('Font "%s" not found', [FFontName]);
end;

procedure TSimbaTextDrawer.EndDrawing;
begin
  FLock.Leave();
end;

constructor TSimbaTextDrawer.Create(Bitmap: TObject);
begin
  FBitmap := Bitmap;
  FSize := 18;
  FFontAntialised := True;
end;

procedure TSimbaTextDrawer.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  BeginDrawing();
  try
    inherited DrawText(Text, FFont, Position.X, Position.Y + FFont.SizeInPoints, TColorToFPColor(Color));
  finally
    EndDrawing();
  end;
end;

procedure TSimbaTextDrawer.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor);
begin
  BeginDrawing();

  try
    if Center then
      inherited DrawTextRect(Text, FFont, Box.X1, Box.Y1, Box.X2, Box.Y2, TColorToFPColor(Color), [ftaCenter, ftaVerticalCenter])
    else
      inherited DrawTextRect(Text, FFont, Box.X1, Box.Y1, Box.X2, Box.Y2, TColorToFPColor(Color), [ftaLeft, ftaTop]);
  finally
    EndDrawing();
  end;
end;

function TSimbaTextDrawer.TextWidth(Text: String): Integer;
begin
  BeginDrawing();

  try
    Result := Round(FFont.TextWidth(Text));
  finally
    EndDrawing();
  end;
end;

function TSimbaTextDrawer.TextHeight(Text: String): Integer;
begin
  BeginDrawing();

  try
    Result := Round(FFont.TextHeight(Text));
  finally
    EndDrawing();
  end;
end;

function TSimbaTextDrawer.TextSize(Text: String): TPoint;
begin
  BeginDrawing();

  try
    Result.X := Round(FFont.TextWidth(Text));
    Result.Y := Round(FFont.TextHeight(Text));
  finally
    EndDrawing();
  end;
end;

initialization
  SimbaFreeTypeFontLoader := TSimbaFreeTypeFontLoader.Create();

finalization
  if Assigned(SimbaFreeTypeFontLoader) then
    FreeAndNil(SimbaFreeTypeFontLoader);

end.

