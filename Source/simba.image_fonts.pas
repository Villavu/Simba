{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_fonts;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  EasyLazFreeType,
  simba.base;

// Adds every .ttf in Dir (not its subdirectories).
function SimbaFonts_LoadFontsInDir(Dir: String): Boolean;
// Every font family loaded.
function SimbaFonts_FontNames: TStringArray;
// Opens the family's font closest to the style. Raises if there is no such family.
function SimbaFonts_OpenFont(FontName: String; FontSize: Single; Bold, Italic, Antialiased: Boolean): TFreeTypeFont;

implementation

uses
  Forms,
  FileUtil,
  LazFileUtils,
  LazFreeTypeFontCollection;

var
  LoadedDefaultFonts: Boolean = False;

// Adds the files to the font collection, skipping any already in it and any that fail to load.
function AddFontFiles(Files: TStrings): Boolean;
var
  I: Integer;
  FileName: String;
  Loaded: IFreeTypeFontEnumerator;
  Known: Boolean;
begin
  Result := False;

  FontCollection.BeginUpdate();
  try
    for I := 0 to Files.Count - 1 do
    try
      FileName := ExpandFileName(Files[I]);

      Known := False;
      Loaded := FontCollection.FontFileEnumerator;
      while (not Known) and Loaded.MoveNext() do
        Known := SameFileName(Loaded.Current.Filename, FileName);

      if not Known then
        FontCollection.AddFile(FileName);

      Result := True;
    except
      // ignore fonts that fail to load
    end;
  finally
    FontCollection.EndUpdate();
  end;
end;

procedure LoadDefaultFonts;
var
  Files: TStringList;
begin
  if LoadedDefaultFonts then
    Exit;
  LoadedDefaultFonts := True;

  Files := TStringList.Create();
  try
    Files.Sorted := True;
    Files.Duplicates := dupIgnore;

    FindAllFiles(Files, Application.Location, '*.ttf');
    {$IFDEF WINDOWS}
    FindAllFiles(Files, SHGetFolderPathUTF8(20), '*.ttf'); // CSIDL_FONTS
    {$ENDIF}
    {$IFDEF LINUX}
    FindAllFiles(Files, '/usr/share/fonts/', '*.ttf');
    FindAllFiles(Files, '/usr/local/share/fonts/', '*.ttf');
    FindAllFiles(Files, GetUserDir() + '.fonts/', '*.ttf');
    FindAllFiles(Files, GetUserDir() + '.local/share/fonts/', '*.ttf');
    {$ENDIF}
    {$IFDEF DARWIN}
    FindAllFiles(Files, '/Library/Fonts/', '*.ttf');
    FindAllFiles(Files, '/System/Library/Fonts/', '*.ttf');
    FindAllFiles(Files, GetUserDir() + 'Library/Fonts/', '*.ttf');
    {$ENDIF}

    AddFontFiles(Files);
  finally
    Files.Free();
  end;
end;

function SimbaFonts_LoadFontsInDir(Dir: String): Boolean;
var
  Files: TStringList;
begin
  Files := FindAllFiles(ExpandFileName(Dir), '*.ttf', False);
  try
    Result := AddFontFiles(Files);
  finally
    Files.Free();
  end;
end;

function SimbaFonts_FontNames: TStringArray;
begin
  LoadDefaultFonts();

  Result := [];
  with FontCollection.FamilyEnumerator do
    while MoveNext() do
      Result += [Current.FamilyName];
end;

function SimbaFonts_OpenFont(FontName: String; FontSize: Single; Bold, Italic, Antialiased: Boolean): TFreeTypeFont;
var
  Style: TFreeTypeStyles;
  StyleNames: String;
  Family: TCustomFamilyCollectionItem;
  Item: TCustomFontCollectionItem;
begin
  LoadDefaultFonts();

  Style := [];
  StyleNames := '';
  if Bold then
  begin
    Style += [ftsBold];
    StyleNames += ' Bold';
  end;
  if Italic then
  begin
    Style += [ftsItalic];
    StyleNames += ' Italic';
  end;

  Family := FontCollection.Family[FontName];
  if (Family = nil) then
    SimbaException('Font "%s" not found', [FontName]);
  Item := Family.GetFont(StyleNames);
  if (Item = nil) then
    SimbaException('Font "%s" has no%s style', [FontName, StyleNames]);

  Result := Item.CreateFont();
  try
    Result.Style := Style;
    Result.SizeInPixels := FontSize;
    Result.Hinted := True;
    Result.KerningEnabled := False;
    Result.ClearType := False;
    Result.Orientation := 0;
    Result.UnderlineDecoration := False;
    Result.StrikeoutDecoration := False;
    if Antialiased then
      Result.Quality := grqHighQuality
    else
      Result.Quality := grqMonochrome;
  except
    Result.Free();
    raise;
  end;
end;

end.
