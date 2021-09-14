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

    Fonts class for the Mufasa Macro Library
}

unit simba.fontloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics, simba.bitmap,
  simba.ocrutil,lclintf; // contains the actual `loading'

type
  PMFont = ^TMFont;
  TMFont = class(TObject)
  public
    Name: String;
    Data: TOcrData;

    function Copy: TMFont;

    constructor Create;
    destructor Destroy; override;
  end;

  PMFonts = ^TMFonts;
  TMFonts = class(TObject)
  private
    FFonts: TList;
    FPath: String;
    FClient : TObject;

    procedure SetPath(const aPath: String);
    function GetPath: String;
  public
    procedure Add(Font: TMFont);

    function GetFont(const Name: String): TMFont; overload;
    function GetFont(const Font: TFont): TMFont; overload;
    function GetFontData(const Name: String): TOcrData; overload;
    function GetFontData(const Font: TFont): TOcrData; overload;

    function FreeFont(const Name: String): Boolean;
    function LoadFont(Name: String; Shadow: Boolean): boolean;
    function LoadSystemFont(const SysFont: TFont; const FontName: String): Boolean;
    function IsFontLoaded(const Name: String): boolean;
    function Copy(Owner : TObject): TMFonts; overload;
    function Copy(const Name: String): TMFont; overload;
    function Count: integer;
    property Path: string read GetPath write SetPath;

    constructor Create(Owner: TObject);
    destructor Destroy; override;
  end;

implementation

uses
  simba.mufasatypes, forms;

constructor TMFont.Create;
begin
  inherited;

  Name:='';
end;

destructor TMFont.Destroy;
begin
  Name:='';

  inherited;
end;

function TMFont.Copy: TMFont;
var
  i, l, ll:integer;
begin
  Result := TMFont.Create;
  Result.Name := Self.Name;
  Move(Self.Data.ascii[0], Result.Data.ascii[0], length(Self.Data.ascii) * SizeOf(TocrGlyphMetric));
  l := Length(Self.Data.Pos);
  SetLength(Result.Data.pos, l);
  for i := 0 to l - 1 do
  begin
    ll := length(Self.Data.Pos[i]);
    setlength(Result.Data.Pos[i], ll);
    Move(Self.Data.Pos[i][0], Result.Data.Pos[i][0], ll*SizeOf(Integer));
  end;

  SetLength(Result.Data.pos_adj,  length(Self.Data.pos_adj));
  Move(Self.Data.pos_adj[0], Result.Data.pos_adj[0], length(Self.Data.pos_adj) * SizeOf(real));

  l := Length(Self.Data.neg);
  SetLength(Result.Data.neg, l);
  for i := 0 to l - 1 do
  begin
    ll := length(Self.Data.neg[i]);
    setlength(Result.Data.neg[i], ll);
    Move(Self.Data.neg[i][0], Result.Data.neg[i][0], ll*SizeOf(Integer));
  end;

  SetLength(Result.Data.neg_adj,  length(Self.Data.neg_adj));
  Move(Self.Data.neg_adj[0], Result.Data.neg_adj[0], length(Self.Data.neg_adj) * SizeOf(real));

  SetLength(Result.Data.map,  length(Self.Data.map));
  Move(Self.Data.map[0], Result.Data.map[0], length(Self.Data.map) * SizeOf(char));

  Result.Data.Width := Self.Data.Width;
  Result.Data.Height := Self.Data.Height;
  Result.Data.inputs := Self.Data.inputs;
  Result.Data.outputs := Self.Data.outputs;
  Result.Data.max_height:= Self.Data.max_height;
  Result.Data.max_width:= Self.Data.max_width;
end;

function TMFonts.GetFont(const Font: TFont): TMFont;
var
  Name: String;
begin
  Name := Format('%s:%d:%d', [Font.Name, Font.Size, Int32(Font.Style)]);

  if not IsFontLoaded(Name) then
    LoadSystemFont(Font, Name);

  Result := GetFont(Name);
end;

constructor TMFonts.Create(Owner : TObject);

begin
  inherited Create;
  FFonts := TList.Create;
  FClient := Owner;
end;

destructor TMFonts.Destroy;
var
  i:integer;
begin
  for i := 0 to FFonts.Count - 1 do
    TMFont(FFonts.Items[i]).Free();
  FFonts.Free();

  inherited;
end;

procedure TMFonts.Add(Font: TMFont);
begin
  FFonts.Add(Font);
end;

procedure TMFonts.SetPath(const aPath: String);
begin
  FPath := aPath;
end;

function TMFonts.GetPath: String;
begin
  Exit(FPath);
end;

function TMFonts.GetFont(const Name: String): TMFont;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to FFonts.Count - 1 do
    if SameText(TMFont(FFonts[i]).Name, Name) then
    begin
      Result := TMFont(FFonts[i]);
      Exit;
    end;

  if LoadFont(Name, False) then
    Result := TMFont(FFonts.Last);
end;

function TMFonts.GetFontData(const Name: String): TOcrData;
begin
  Result := Default(TocrData);

  if (GetFont(Name) <> nil) then
    Result := GetFont(Name).Data;
end;

function TMFonts.GetFontData(const Font: TFont): TOcrData;
begin
  Result := Default(TocrData);

  if (GetFont(Font) <> nil) then
    Result := GetFont(Font).Data;
end;

function TMFonts.FreeFont(const Name: String): Boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to FFonts.Count - 1 do
    if SameText(TMFont(FFonts[i]).Name, Name) then
    begin
      TMFont(FFonts[i]).Free();
      FFonts.Delete(i);

      Result := True;
      Break;
    end;
end;

function TMFonts.LoadFont(Name: String; Shadow: Boolean): boolean;
var
  Font: TMFont;
begin
  Result := True;

  // Allow user to pass a filename too
  if DirectoryExists(Name) then
  begin
    Font := TMFont.Create();
    Font.Name := ExtractFileName(Name);
    Font.Data := InitOCR(LoadGlyphMasks(IncludeTrailingPathDelimiter(SetDirSeparators(Name)), Shadow));

    FFonts.Add(Font);
  end else
  begin
    if (not DirectoryExists(FPath + Name)) then
      raise Exception.Create('Font "' + Name + '" does not exist in path "' + FPath + '"');

    Font := TMFont.Create();
    Font.Name := Name;
    if Shadow then
      Font.Name := Font.Name + '_s';
    Font.Data := InitOCR(LoadGlyphMasks(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPath) + Name), Shadow));

    FFonts.Add(Font);
  end;
end;

function TMFonts.LoadSystemFont(const SysFont: TFont; const FontName: String): Boolean;
var
  Masks: TOCRGlyphMaskArray;
  i, W, H: Int32;
  BMP: TBitmap;
  F: TMFont;
  Mufasa: TMufasaBitmap;
begin
  if IsFontLoaded(FontName) then
    Exit(True);
  if (Screen.Fonts.IndexOf(SysFont.Name) = -1) then
    Exit(False);

  Mufasa := TMufasaBitmap.Create();
  BMP := TBitmap.Create();

  try
    with BMP.Canvas do
    begin
      Font := SysFont;
      Font.Color := clWhite;
      Font.Quality := fqNonAntialiased;
      Brush.Color := clBlack;
      Pen.Style := psClear;

      for i := 1 to 255 do
      begin
        GetTextSize(UnicodeString(Chr(i)), W, H);

        if (W > 0) and (H > 0) then
        begin
          SetLength(Masks, Length(Masks) + 1);

          BMP.SetSize(W, H);
          TextOut(0, 0, Chr(i));
          Mufasa.LoadFromTBitmap(BMP);

          {$IFDEF LINUX} // :| GTK2 always renders anti aliased.
          Mufasa.ThresholdAdaptive(0, 255, False, TM_MinMax, -100);
          Masks[High(Masks)] := LoadGlyphMask(Mufasa, True, Chr(i));
          {$ELSE}
          Masks[High(Masks)] := LoadGlyphMask(Mufasa, False, Chr(i));
          {$ENDIF}
        end;
      end;
    end;

    F := TMFont.Create();
    F.Name := FontName;
    F.Data := InitOCR(Masks);

    FFonts.Add(F);
  finally
    BMP.Free();
    Mufasa.Free();
  end;

  Exit(True);
end;

function TMFonts.IsFontLoaded(const Name: String): boolean;
var
  i: integer;
begin
  result := false;

  for i := 0 to (FFonts.Count - 1) do
    if (lowercase(Name) = lowercase(TMFont(FFonts.Items[i]).Name)) then
      exit(true);
end;

function TMFonts.Copy(Owner : TObject): TMFonts;

var
  i:integer;
begin
  Result := TMFonts.Create(Owner);
  Result.Path := FPath;
  for i := 0 to Self.FFonts.Count -1 do
    Result.Add(TMFont(Self.FFonts.Items[i]).Copy());
end;

function TMFonts.Copy(const Name: String): TMFont;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to FFonts.Count - 1 do
    if SameText(Name, TMFont(FFonts.Items[i]).Name) then
      Exit(TMFont(FFonts.Items[i]).Copy());
end;

function TMFonts.Count: integer;
begin
  Result := FFonts.Count;
end;

end.

