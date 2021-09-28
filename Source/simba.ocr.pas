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

    OCR class for the Mufasa Macro Library
}

unit simba.ocr;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, graphics,
  simba.mufasatypes, simba.bitmap, math, simba.ocrutil, simba.fontloader, simba.type_matrix;

type
  PMOCR = ^TMOCR;
  TMOCR = class(TObject)
  private
    Client: TObject;
    FFonts: TMFonts;

    function GetFontPath: String;
    procedure SetFontPath(Value: String);
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;

    function GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing, color, tol, len: integer; font: string): string;overload;
    function GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing, color, tol: integer; font: string): string;overload;
    function GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string;
    function TextToFontMatrix(Text, Font: String): TIntegerMatrix;
    function TextToFontTPA(Text: String; Data: TOCRData; out W, H: Int32): TPointArray; overload;
    function TextToFontTPA(Text, Font: String; out W, H: Int32): TPointArray; overload;
    function TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray; overload;
    function TextToFontBitmap(Text, font: String): TMufasaBitmap;
    function TextToMask(Text, font: String): TMask;

    property Fonts: TMFonts read FFonts;
    property FontPath: String read GetFontPath write SetFontPath;
  end;

implementation

uses
  simba.colormath, simba.client, simba.tpa, simba.iomanager;

constructor TMOCR.Create(Owner: TObject);
begin
  inherited Create;

  Self.Client := Owner;
  Self.FFonts := TMFonts.Create(Owner);
end;

destructor TMOCR.Destroy;
begin
  Self.FFonts.Free;

  inherited Destroy;
end;

function TMOCR.GetFontPath: String;
begin
  Result := FFonts.Path;
end;

procedure TMOCR.SetFontPath(Value: String);
begin
  FFonts.Path := Value;
end;

function TMOCR.GetTextATPA(const ATPA : T2DPointArray;const maxvspacing : integer; font: string): string;
var
  b, lb: TBox;
  i, j: Integer;
  lbset: boolean;
  n: TNormArray;
  fD: TocrData;
  TPA: TPointArray;

begin
  Result := '';
  fD := FFonts.GetFontData(font);

  lbset := false;
  SetLength(Result, 0);
  SetLength(n, (fd.width + 1) * (fd.height + 1));
  for i := 0 to high(ATPA) do
  begin
    for j := 0 to high(n) do
      n[j] := 0;
    TPA := ATPA[i];
    b := GetTPABounds(TPA);
    if not lbset then
    begin
      lb:=b;
      lbset:=true;
    end else
    begin
     { if b.x1 - lb.x2 < minvspacing then
      begin
        writeln('GetTextAt: not enough spacing between chars...');
        lb := b;
        continue;
      end;   }
      if b.x1 - lb.x2 > maxvspacing then
        result:=result+' ';

      lb:=b;
    end;

    for j := 0 to high(tpa) do
      tpa[j] := tpa[j] - point(b.x1,b.y1);

    {
      FIXME: We never check it j actually fits in n's bounds...
      This *WILL* error when wrong spaces etc are passed.
      Added a temp resolution.
    }
    for j := 0 to high(tpa) do
    begin
      if (tpa[j].x) + ((tpa[j].y) * fD.width) <= high(n) then
        n[(tpa[j].x) + ((tpa[j].y) * fD.width)] := 1
      else
        WriteLn('The automatically split characters are too wide. Try decreasing minspacing');
    end;
    result := result + GuessGlyph(n, fD);
  end;
end;

function TMOCR.GetTextAt(xs, ys, xe,ye, minvspacing, maxvspacing, hspacing,
                               color, tol: integer; font: string): string;
var
  TPA : TPointArray;
  STPA : T2DPointArray;
begin;
  SetLength(TPA, 0);
  TClient(Client).MFinder.FindColorsTolerance(TPA, color, xs,ys,xe,ye,tol);

  { Split the text points into something usable. }
  { +1 because splittpa will not split well if we use 0 space ;) }
  STPA := SplitTPAEx(TPA, minvspacing+1, hspacing+1);

  SortATPAFrom(STPA, Point(0, ys));
  SortATPAFromFirstPoint(STPA, Point(0, ys));
  result := gettextatpa(STPA,maxvspacing,font);
end;

function TMOCR.GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing,
                               color, tol, len: integer; font: string): string;
var
  w,h : integer;
  fD: TocrData;

begin
  Result := '';
  fD := FFonts.GetFontData(font);
  TClient(Client).IOManager.GetDimensions(w, h);
 { writeln('Dimensions: (' + inttostr(w) + ', ' + inttostr(h) + ')');    }

  { Get the text points }
  if (atY + fD.height -1) >= h then
    raise exception.createFMT('You are trying to get text that is out of is origin y-coordinate: %d',[aty]);
  result := GetTextAt(atX, atY,min(atX + fD.max_width * len, w - 1),
            atY + fD.max_height - 1, minvspacing,maxvspacing,hspacing,color,tol,font);
  if length(result) > len then
    setlength(result,len);

end;

function TMOCR.TextToFontMatrix(Text, Font: String): TIntegerMatrix;
var
  Character, X, Y, i: Int32;
  Glyph: TocrGlyphMetric;
  Data: TocrData;
  Bounds: TBox;
begin
  Data := FFonts.GetFontData(Font);

  Bounds.X1 := 0;
  Bounds.Y1 := $FFFFFF;
  Bounds.X2 := 0;
  Bounds.Y2 := -$FFFFFF;

  for i := 1 to Length(Text) do
  begin
    Character := Ord(Text[i]);
    if (not (Character in [0..255])) then
      Continue;
    Glyph := Data.ascii[Character];
    if not Glyph.inited then
      Continue;

    if (Character <> 32) then
    begin
      if (Glyph.YOff < Bounds.Y1) then
        Bounds.Y1 := Glyph.YOff;
      if (Glyph.Bottom > Bounds.Y2) then
        Bounds.Y2 := Glyph.Bottom;
    end;

    Bounds.X2 := Bounds.X2 + Glyph.Width;
  end;

  SetLength(Result, Bounds.Height, Bounds.Width - 1);

  Bounds.X1 := 0;
  Bounds.X2 := 0;

  for i := 1 to Length(Text) do
  begin
    Character := Ord(Text[i]);
    if (not (Character in [0..255])) then
      Continue;
    Glyph := Data.ascii[Character];
    if not Glyph.inited then
      Continue;

    if (Character = 32) then
    begin
      for Y := Bounds.Y1 to Bounds.Y2 do
        for X := 0 to Glyph.Width - 1 do
          Result[Y + Glyph.YOff - Bounds.Y1][X + Bounds.X2 + Glyph.XOff] := $00FF00;
    end else
    begin
      for Y := 0 to Data.Height - 1 do
        for X := 0 to Data.Width - 1 do
        begin
          if Data.Pos[Glyph.Index][X + Y * Data.Width] = 1 then
            Result[Y + Glyph.YOff - Bounds.Y1][X + Bounds.X2 + Glyph.XOff] := $0000FF;
        end;
    end;

    Bounds.X2 := Bounds.X2 + Glyph.Width;
  end;
end;

function TMOCR.TextToFontTPA(Text: String; Data: TOCRData; out W, H: Int32): TPointArray;
var
  c, i, x, y, off: Integer;
  d: TocrGlyphMetric;
  Character: integer;
  Count, Size: Int32;
begin
  c := 0;
  off := 0;

  Count := 0;
  Size := 128;
  SetLength(Result, Size);

  for i := 1 to Length(text) do
  begin
    Character := Ord(Text[i]);
    if (not (Character in [0..255])) then
      Continue;
    d := Data.ascii[Character];
    if not d.inited then
      Continue;

    for y := 0 to Data.height - 1 do
      for x := 0 to Data.width - 1 do
      begin
        if Data.pos[d.index][x + y * Data.width] = 1 then
        begin
          Result[Count] := Point(x + off + d.xoff, y + d.yoff);
          Inc(Count);

          if (Count = Size) then
          begin
            Size := Size * 2;
            SetLength(Result, Size);
          end;
        end;
      end;

    off := off + d.width;
  end;

  SetLength(Result, Count);

  W := off;
  H := d.height;
end;

function TMOCR.TextToFontTPA(Text, Font: String; out W, H: Int32): TPointArray;
var
  Data: TOCRData;
begin
  Data := FFonts.GetFontData(Font);
  Result := TextToFontTPA(Text, Data, W, H);
end;

function TMOCR.TextToFontTPA(Text: String; Font: TFont; out W, H: Int32): TPointArray;
var
  Data: TOCRData;
begin
  Data := FFonts.GetFontData(Font);
  Result := TextToFontTPA(Text, Data, W, H);
end;

function TMOCR.TextToFontBitmap(Text, font: String): TMufasaBitmap;
var
   TPA: TPointArray;
   w,h: integer;
   bmp: TMufasaBitmap;
begin
  TPA := TextToFontTPA(text, font, w, h);
  bmp := TMufasaBitmap.Create;
  bmp.SetSize(w, h);
  bmp.DrawTPA(TPA, clWhite);
  result := bmp;
end;

function TMOCR.TextToMask(Text, font: String): TMask;
var
   TPA: TPointArray;
   w,h: integer;
   i,x,y : integer;
   dx,dy : integer;
   c : integer;
   Pixels : array of array of boolean; //White = true
begin
  TPA := TextToFontTPA(text, font, w, h);
  Result.w := w;
  Result.h := h;
  Result.WhiteHi:= High(TPA);//High(WhitePixels)
  Result.BlackHi:= w*h - Length(TPA) - 1;//High(BlackPixels) = Length(blackPixels) - 1 = (TotalLength - LenWhitePixels) - 1
  SetLength(Pixels,w,h);
  SetLength(result.White,Result.WhiteHi + 1);
  SetLength(result.Black,Result.BlackHi + 1);
  for i := Result.WhiteHi downto 0 do
  begin
    Result.White[i] := TPA[i];
    Pixels[TPA[i].x][TPA[i].y] := true;
  end;
  c := 0;
  dx := w-1;
  dy := h-1;
  for y := 0 to dY do
    for x := 0 to dX do
    if not Pixels[x][y] then
    begin
      result.Black[c].x :=x;
      result.black[c].y := y;
      inc(c);
    end;
end;

end.
