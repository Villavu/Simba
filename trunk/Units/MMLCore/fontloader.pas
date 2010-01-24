{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

unit fontloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ocrutil; // contains the actual `loading'

{
  We will not give any access to actual indices.
}

type
    TMFont = class(TObject)
            constructor Create;
            destructor Destroy; override;

            function Copy: TMFont;
        public
            Name: String;
            Data: TOcrData;
    end;


type

    { TMFonts }

    TMFonts = class(TObject)
    private
        function GetFontIndex(Name: String): Integer;
        function GetFontByIndex(Index : integer): TMfont;
    private
        Fonts: TList;
        Path: String;
    public
      constructor Create;
      destructor Destroy; override;

      function GetFont(Name: String): TOcrData;
      function FreeFont(Name: String): boolean;
      function LoadFont(Name: String; Shadow: Boolean): boolean;
      procedure SetPath(aPath: String);
      function GetPath: String;
      function Copy: TMFonts;
      function Count : integer;
      property Font[Index : integer]: TMfont read GetFontByIndex; default;
    end;

implementation

uses
  files, MufasaTypes;


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
end;

function TMFonts.GetFontByIndex(Index : integer): TMfont;
begin
  result := TMfont(Fonts.Items[index]);
end;

constructor TMFonts.Create;

begin
  inherited;

  Fonts := TList.Create;
end;

destructor TMFonts.Destroy;
var
  i:integer;
begin
  for i := 0 to Fonts.Count - 1 do
    TMFont(Fonts.Items[i]).Free;
  Fonts.Free;

  inherited;
end;

procedure TMFonts.SetPath(aPath: String);
begin
  Path := aPath;
end;

function TMFonts.GetPath: String;
begin
  Exit(Path);
end;

function TMFonts.GetFontIndex(Name: String): Integer;
var
  i: integer;
begin
  for i := 0 to Fonts.Count - 1 do
  begin
    if Name = TMFont(Fonts.Items[i]).Name then
      Exit(i);
  end;
  raise Exception.Create('Font [' + Name + '] not found.');
  Exit(-1);
end;

function TMFonts.GetFont(Name: String): TOcrData;
var
  i: integer;
begin
  i := GetFontIndex(Name);
  Exit(TMFont(Fonts.Items[i]).Data);
end;

function TMFonts.FreeFont(Name: String): boolean;
var
  i: integer;
begin
  i := GetFontIndex(Name);
  TMFont(Fonts.Items[i]).Free;
  Fonts.Delete(i);
end;

function TMFonts.LoadFont(Name: String; Shadow: Boolean): boolean;
var
  f: TMFont;
  ocrdata: TOcrData;

begin
  if not DirectoryExists(Path + Name) then
  begin
    raise Exception.Create('LoadFont: Directory ' + Path + Name + ' does not exists.');
    Exit(False);
  end;

  ocrdata := InitOCR(Path + Name + DS, Shadow);

  f:=TMFont.Create;
  f.Name := Name;
  if Shadow then
    F.Name := F.Name + '_s';
  f.Data := ocrdata;
  Fonts.Add(f);
  {$IFDEF FONTDEBUG}
  writeln('Loaded Font ' + f.Name);
  {$ENDIF}
end;

function TMFonts.Copy: TMFonts;

var
  i:integer;
begin
  Result := TMFonts.Create;
  Result.Path := Self.GetPath();
  for i := 0 to Self.Fonts.Count -1 do
    Result.Fonts.Add(TMFont(Self.Fonts.Items[i]).Copy());
end;

function TMFonts.Count: integer;
begin
  result := Fonts.Count;
end;

end.

