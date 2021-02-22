{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

  Text drawing for TMufasaBitmap. TrueType fonts are loaded from the system.
}
unit simba.textdrawer;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils, lazfreetypefpimagedrawer, easylazfreetype, fpimage, graphics,
  simba.mufasatypes;

type
  TSimbaTextDrawer = class(TFPImageFreeTypeDrawer)
  protected
    FWidth: Int32;
    FHeight: Int32;
    FData: PRGB32;
    FCurrentColor: PRGB32;
    FBitmap: TObject;
    FFonts: TStringArray;
    FFont: TFreeTypeFont;
    FFontName: String;
    FFontAntialised: Boolean;
    FFontSize: Single;
    FClipRect: TRect;

    procedure MoveToPixel(X, Y: Integer); override;
    function GetCurrentColor: TFPColor; override;
    procedure SetCurrentColorAndMoveRight(const AColor: TFPColor); override;
    procedure MoveRight; override;
    function GetClipRect: TRect; override;
    function GetFonts: TStringArray;
    procedure PrepareDrawing;
  public
    property Fonts: TStringArray read GetFonts;
    property FontName: String read FFontName write FFontName;
    property FontSize: Single read FFontSize write FFontSize;
    property Antialiased: Boolean read FFontAntialised write FFontAntialised;

    procedure DrawText(Text: String; Position: TPoint; Color: TColor); overload;
    procedure DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor); overload;

    function TextWidth(Text: String): Int32;
    function TextHeight(Text: String): Int32;
    function TextSize(Text: String): TPoint;

    constructor Create(Bitmap: TObject); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  fileutil, lazfileutils, syncobjs, lazfreetypefontcollection,
  simba.bitmap;

var
  FontsLoaded: Boolean = False;
  FontNames: TStringArray;
  FontsCriticalSection: TCriticalSection;

procedure LoadFonts;
var
  SearchPaths, FontsFiles: TStringList;
  I: Int32;
begin
  FontsCriticalSection.Enter();

  try
    if FontsLoaded then
      Exit;
    FontsLoaded := True;

    SearchPaths := TStringList.Create();

    {$IFDEF WINDOWS}
    SearchPaths.Add(SHGetFolderPathUTF8(20));
    {$ENDIF}
    {$IFDEF LINUX}
    SearchPaths.Add('/usr/share/cups/fonts/');
    SearchPaths.Add('/usr/share/fonts/truetype/');
    SearchPaths.Add('/usr/local/lib/X11/fonts/');
    SearchPaths.Add(GetUserDir() + '.fonts/');
    {$ENDIF}
    {$IFDEF DARWIN}
    SearchPaths.Add('/Library/Fonts/');
    SearchPaths.Add('/System/Library/Fonts/');
    SearchPaths.Add('/Network/Library/Fonts/');
    SearchPaths.Add('~/Library/Fonts/');
    {$ENDIF}

    FontsFiles := TStringList.Create();
    FontsFiles.Sorted := True;

    for I := 0 to SearchPaths.Count - 1 do
      FindAllFiles(FontsFiles, ExpandFileName(SearchPaths[I]), '*.ttf', True);

    FontCollection.BeginUpdate();

    try
      for I := 0 to FontsFiles.Count - 1 do
      try
        FontCollection.AddFile(FontsFiles[I]);
      except
        // ignore exceptions due to font read errors
      end;
    finally
      FontCollection.EndUpdate();
    end;

    with FontCollection.FamilyEnumerator do
      while MoveNext do
        FontNames += [Current.FamilyName];

    SearchPaths.Free();
    FontsFiles.Free();
  finally
    FontsCriticalSection.Leave();
  end;
end;

function LoadFont(Name: String; AStyle: TFreeTypeStyles = []): TFreeTypeFont;
var
  FamilyItem: TCustomFamilyCollectionItem;
  FontItem: TCustomFontCollectionItem;
  Style: String = '';
begin
  Result := nil;

  LoadFonts();

  FamilyItem := FontCollection.Family[Name];
  if (FamilyItem <> nil) then
  begin
    if (ftsBold in AStyle) then
      Style := 'Bold';
    if (ftsItalic in AStyle) then
      Style := Style + ' Italic';

    FontItem := FamilyItem.GetFont(Style);
    if (FontItem <> nil) then
    begin
      Result := FontItem.CreateFont;
      Result.Style := AStyle;
    end;
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
  FCurrentColor^ := TRGB32(((AColor.Blue shr 8) and $FF) or (AColor.Green and $FF00) or ((AColor.Red shl 8) and $FF0000) or (AColor.Alpha and $FF000000));

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

function TSimbaTextDrawer.GetFonts: TStringArray;
begin
  PrepareDrawing();

  Result := FontNames;
end;

procedure TSimbaTextDrawer.PrepareDrawing;
begin
  with TMufasaBitmap(FBitmap) do
  begin
    Self.FData := Data;
    Self.FWidth := Width;
    Self.FHeight := Height;
  end;

  FClipRect.Right := FWidth;
  FClipRect.Bottom := FHeight;

  if (FFont <> nil) and (FFontName <> FFont.Family) then
  begin
    FreeAndNil(FFont);

    FFont := LoadFont(FFontName);
    if (FFont <> nil) then
      FFontName := FFont.Family;
  end;

  if (FFont = nil) then
  begin
    FFont := LoadFont(FFontName);
    if (FFont = nil) then
      raise Exception.Create('No fonts found');

    FFontName := FFont.Family;
  end;

  case FFontAntialised of
    True:
      begin
        FFont.Quality := grqHighQuality;
        FFont.ClearType := True;
      end;

    False:
      begin
        FFont.Quality := grqMonochrome;
        FFont.ClearType := False;
      end;
  end;

  FFont.SizeInPixels := FFontSize;
end;

constructor TSimbaTextDrawer.Create(Bitmap: TObject);
begin
  FBitmap := Bitmap;

  FFontSize := 22.5;
  FFontAntialised := True;
end;

destructor TSimbaTextDrawer.Destroy;
begin
  if (FFont <> nil) then
    FreeAndNil(FFont);

  inherited Destroy();
end;

procedure TSimbaTextDrawer.DrawText(Text: String; Position: TPoint; Color: TColor);
begin
  PrepareDrawing();

  inherited DrawText(Text, FFont, Position.X, Position.Y + FFont.SizeInPoints, TColorToFPColor(Color));
end;

procedure TSimbaTextDrawer.DrawText(Text: String; Box: TBox; Center: Boolean; Color: TColor);
begin
  PrepareDrawing();

  if Center then
    inherited DrawTextRect(Text, FFont, Box.X1, Box.Y1, Box.X2, Box.Y2, TColorToFPColor(Color), [ftaCenter, ftaVerticalCenter])
  else
    inherited DrawTextRect(Text, FFont, Box.X1, Box.Y1, Box.X2, Box.Y2, TColorToFPColor(Color), [ftaLeft, ftaTop]);
end;

function TSimbaTextDrawer.TextWidth(Text: String): Int32;
begin
  PrepareDrawing();

  Result := Round(FFont.TextWidth(Text));
end;

function TSimbaTextDrawer.TextHeight(Text: String): Int32;
begin
  PrepareDrawing();

  Result := Round(FFont.TextHeight(Text));
end;

function TSimbaTextDrawer.TextSize(Text: String): TPoint;
begin
  PrepareDrawing();

  Result.X := Round(FFont.TextWidth(Text));
  Result.Y := Round(FFont.TextHeight(Text));
end;

initialization
  FontsCriticalSection := TCriticalSection.Create();

finalization
  if (FontsCriticalSection <> nil) then
    FontsCriticalSection.Free();

end.

