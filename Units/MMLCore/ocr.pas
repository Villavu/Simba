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

    OCR class for the Mufasa Macro Library
}

unit ocr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes,MufasaBase, bitmaps, math, ocrutil, fontloader,
  {Begin To-Remove units. Replace ReadBmp with TMufasaBitmap stuff later.}
  graphtype, intfgraphics,graphics;
  {End To-Remove unit}


type
    { TMOCR }

  TMOCR = class(TObject)
  private
    Client: TObject;
    FFonts: TMFonts;
    {$IFDEF OCRDEBUG}
    debugbmp: TMufasaBitmap;
    {$ENDIF}
    function  GetFonts:TMFonts;
    procedure SetFonts(const NewFonts: TMFonts);
  public
    constructor Create(Owner: TObject);
    destructor Destroy; override;
    function InitTOCR(const path: string): boolean;
    function getTextPointsIn(sx, sy, w, h: Integer; shadow: boolean;
           var _chars, _shadows: T2DPointArray): Boolean;
    function GetUpTextAtEx(atX, atY: integer; shadow: boolean): string;
    function GetUpTextAt(atX, atY: integer; shadow: boolean): string;

    procedure FilterUpTextByColour(bmp: TMufasaBitmap; w,h: integer);
    procedure FilterUpTextByCharacteristics(bmp: TMufasaBitmap; w,h: integer);
    procedure FilterShadowBitmap(bmp: TMufasaBitmap);
    procedure FilterCharsBitmap(bmp: TMufasaBitmap);

    function GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing,
                    color, tol, len: integer; font: string): string;overload;
    function GetTextAt(xs, ys, xe, ye, minvspacing, maxvspacing, hspacing,
      color, tol: integer; font: string): string;overload;
    function GetTextATPA(const ATPA: T2DPointArray; const maxvspacing: integer; font: string): string;
    function TextToFontTPA(Text, font: String; out w, h: integer): TPointArray;
    function TextToFontBitmap(Text, font: String): TMufasaBitmap;
    function TextToMask(Text, font: String): TMask;
    property Fonts : TMFonts read GetFonts write SetFonts;
    {$IFDEF OCRDEBUG}
    procedure DebugToBmp(bmp: TMufasaBitmap; hmod,h: integer);
    {$ENDIF}
  end;

  {$IFDEF OCRDEBUG}
    {$IFDEF LINUX}
      const OCRDebugPath = '/tmp/';
    {$ELSE}
      const OCRDebugPath = '';
    {$ENDIF}
  {$ENDIF}
implementation

uses
  colour_conv, client,  files, tpa, mufasatypesutil;

const
    { Very rough limits for R, G, B }
    ocr_Limit_High = 190;
    ocr_Limit_Med = 130;
    ocr_Limit_Low = 65;


    { `base' Colours of the Uptext }

    { White }
    ocr_White = 16777215;

    { Level < Your Level }
    ocr_Green = 65280;

    { Level > Your Level }
    ocr_Red = 255;

    { Interact or Level = Your Level }
    ocr_Yellow = 65535;

    { Object }
    ocr_Blue = 16776960;

    { Item }
    ocr_ItemC = 16744447;

    { Shadow }
    ocr_Purple = 8388736;


{ Constructor }
constructor TMOCR.Create(Owner: TObject);
begin
  inherited Create;
  Self.Client := Owner;
  Self.FFonts := TMFonts.Create(Owner);
end;

{ Destructor }
destructor TMOCR.Destroy;
begin
  Self.FFonts.Free;
  inherited Destroy;
end;

{
  InitTOCR loads all fonts in path
  We don't do this in the constructor because we may not yet have the path.
}
function TMOCR.InitTOCR(const path: string): boolean;
var
   dirs: array of string;
   i: longint;
begin
  // We're going to load all fonts now
  FFonts.Path := path;
  dirs := GetDirectories(path);
  Result := false;
  for i := 0 to high(dirs) do
  begin
    if FFonts.LoadFont(dirs[i], false) then
      result := true;
  end;
  If DirectoryExists(path + 'UpChars') then
    FFonts.LoadFont('UpChars', true); // shadow
end;

{ Get the current pointer to our list of Fonts }
function TMOCR.GetFonts:TMFonts;
begin
  Exit(Self.FFonts);
end;

{ Set new Fonts. We set it to a Copy of NewFonts }
procedure TMOCR.SetFonts(const NewFonts: TMFonts);
begin
  Self.FFonts := NewFonts.Copy(Self.Client);
end;

{
  Filter UpText by a very rough colour comparison / range check.
  We first convert the colour to RGB, and if it falls into the following
  defined ranges, it may be part of the uptext. Also get the possible
  shadows.

  We have large ranges because we rather have extra (fake) pixels than less
  uptext pixels... This because we can filter most of the noise out easily.

  Non optimised. We can make it use direct data instead of fastgetpixel and
  fastsetpixel, but speed isn't really an issue. The entire algorithm is still
  fast enough.
}

procedure TMOCR.FilterUpTextByColour(bmp: TMufasaBitmap; w,h: integer);
var
   x, y,r, g, b: Integer;
begin
  // We're going to filter the bitmap solely on colours first.
  // If we found one, we set it to it's `normal' colour.
  for y := 0 to bmp.Height - 1 do
    for x := 0 to bmp.Width - 1 do
    begin
      colortorgb(bmp.fastgetpixel(x,y),r,g,b);
      // the abs(g-b) < 15 seems to help heaps when taking out crap points
      if (r > ocr_Limit_High) and (g > ocr_Limit_High) and (b > ocr_Limit_High)
         // 50 or 55. 55 seems to be better.
         and (abs(r-g) + abs(r-b) + abs(g-b) < 55) then
         // TODO: make 55 a var, and make it so that it can be set
      begin
        bmp.fastsetpixel(x,y,ocr_White);
        continue;
      end;
      if (r < ocr_Limit_Low) and (g > ocr_Limit_High) and (b > ocr_Limit_High) then
      begin
        bmp.fastsetpixel(x,y,ocr_Blue);
        continue;
      end;
      if (r < ocr_Limit_Low) and (g > ocr_Limit_High) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Green);
        continue;
      end;

      // false results with fire
      if(r > ocr_Limit_High) and (g > 100) and (g < ocr_Limit_High) and (b > 40) and (b < 127) then
      begin
        bmp.fastsetpixel(x,y,ocr_ItemC);
        continue;
      end;
      if(r > ocr_Limit_High) and (g > ocr_Limit_High) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Yellow);
        continue;
      end;
      // better use g < 40 than ocr_Limit_Low imo
      if (r > ocr_Limit_High) and (g < ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Red);
        continue;
      end;
      if (r > ocr_Limit_High) and (g > ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.fastsetpixel(x,y,ocr_Red);
        continue;
      end;
      if (r > ocr_Limit_Med) and (r < (ocr_Limit_High + 10)) and (g > ocr_Limit_Low - 10) and
          (b < 20) then
          begin
            bmp.fastsetpixel(x,y,ocr_Green);
            continue;
          end;
      //shadow
      if (r < ocr_Limit_Low) and (g < ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.FastSetPixel(x,y, ocr_Purple);
        continue;
      end;

      bmp.fastsetpixel(x,y,0);
    end;


    // make outline black for shadow characteristics filter
    // first and last horiz line = 0
    for x := 0 to bmp.width -1 do
      bmp.fastsetpixel(x,0,0);
    for x := 0 to bmp.width -1 do
      bmp.fastsetpixel(x,bmp.height-1,0);
    // same for vertical lines
    for y := 0 to bmp.Height -1 do
      bmp.fastsetpixel(0, y, 0);
    for y := 0 to bmp.Height -1 do
      bmp.fastsetpixel(bmp.Width-1, y, 0);
end;


{
  This filter assumes the previous colour filter has been applied first.
  I like to call this the `characteristics' filter because we really only filter
  on characteristics.

  For the uptext, a few things apply...
  First of all:

  *** Remove False Shadow ***
      if shadow[x,y] then not shadow[x-1,y-1]
  If there is a shadow at x,y then there should not be a shadow at x-1, y-1; if
  there is one, then shadow[x, y] is not a shadow.

    (One could also say, if shadow[x,y] and shadow[x+1,y+1] then shadow[x+1,y+1]
     is no shadow; because it essentially means the same. However, a smart mind
     will soon see that this algorithm will be a *lot* more efficient if we
     start at the right bottom, instead of the left top. Which means we should
     work with x-1 and y-1, rather than x+1,y+1
     Yeah.... My comments are vague.
    )

  *** UpText chars identity 1 and 2 ***
      if UpTextChar[x,y] then (UpTextChar[x+1,y+1] or shadow[x+1,y+1])
    If this is not true, then UpTextChar[x,y] cannot be part of uptext - it
    has no shadow, and it doesn't have a `friend' (at x+1,y+1) either.
    We don't need to do this from the right bottom to left top.

}
procedure TMOCR.FilterUpTextByCharacteristics(bmp: TMufasaBitmap; w,h: integer);
var
   x,y: Integer;
begin
  // Filter 2
  // This performs a `simple' filter.
  // What we are doing here is simple checking that if Colour[x,y] is part
  // of the uptext, then so must Colour[x+1,y+1], or Colour[x+1,y+1] is a shadow.
  // if it is neither, we can safely remove it.
 for y := 0 to bmp.Height - 2 do
   for x := 0 to bmp.Width - 2 do
   begin
     if bmp.fastgetpixel(x,y) = clPurple then
       continue;
     if bmp.fastgetpixel(x,y) = clBlack then
       continue;
     if (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x+1,y+1)) and (bmp.fastgetpixel(x+1,y+1) <> clpurple) then
       bmp.fastsetpixel(x,y,{clAqua}0);
   end;

  // Remove false shadow
 for y := bmp.Height - 1 downto 1 do
   for x := bmp.Width - 1 downto 1 do
   begin
     if bmp.fastgetpixel(x,y) <> clPurple then
       continue;
     if bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x-1,y-1) then
     begin
       bmp.fastsetpixel(x,y,clSilver);
       continue;
     end;
     if bmp.fastgetpixel(x-1,y-1) = 0 then
       bmp.fastsetpixel(x,y,clSilver);
   end;

  // Now we do another filter, with uptext chars identity 1 and 2.
 for y := bmp.Height - 2 downto 0 do
   for x := bmp.Width - 2 downto 0 do
   begin
     if bmp.fastgetpixel(x,y) = clPurple then
       continue;
     if bmp.fastgetpixel(x,y) = clBlack then
       continue;

     // identity 1
     if (bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x+1,y+1) ) then
       continue;

     // identity 2
     if bmp.fastgetpixel(x+1,y+1) <> clPurple then
     begin
       bmp.fastsetpixel(x,y,clOlive);
       continue;
     end;

     // If we make it to here, it means the pixel is part of the uptext.
   end;
end;

{$IFDEF OCRDEBUG}
{ Write to our debugbmp }
procedure TMOCR.DebugToBmp(bmp: TMufasaBitmap; hmod, h: integer);
var
   x,y: integer;
begin
 for y := 0 to bmp.height - 1 do
   for x := 0 to bmp.width - 1 do
     debugbmp.fastsetpixel(x,y + hmod *h,bmp.fastgetpixel(x,y));
end;
{$ENDIF}

{
  Return the shadows of the points in charpoint on bitmap shadowsbmp.

  Pseudo:
    if shadow[charpoint[i].x+1, charpoint[i].y+1] then addtoResult;
}
function getshadows(shadowsbmp:TMufasaBitmap; charpoint: tpointarray): tpointarray;
var
   i,c:integer;
begin
  setlength(result,length(charpoint));
  c:=0;
  for i := 0 to high(charpoint) do
  begin
    if shadowsbmp.fastgetpixel(charpoint[i].x+1,charpoint[i].y+1) = clPurple then
    begin
      result[c]:=point(charpoint[i].x+1, charpoint[i].y+1);
      inc(c);
    end;
  end;
  setlength(result,c);
end;

{ Remove anything but the shadows on the bitmap (Shadow = clPurple, remember?) }
procedure TMOCR.FilterShadowBitmap(bmp: TMufasaBitmap);
var
   x,y:integer;
begin
  for y := 0 to bmp.Height - 1 do
    for x := 0 to bmp.Width - 1 do
    begin
      if bmp.fastgetpixel(x,y) <> clPurple then
      begin
        bmp.FastSetPixel(x,y,0);
        continue;
      end;
    end;
end;

{
  Remove all but uptext colours clWhite,clGreen, etc.
  See constants above.

  This assumes that the bitmap only consists of colour 0, and the other
  constants founds above the functionss
}
procedure TMOCR.FilterCharsBitmap(bmp: TMufasaBitmap);
var
   x,y: integer;
begin
  begin
    for y := 0 to bmp.Height - 1 do
      for x := 0 to bmp.Width - 1 do
      begin
        if bmp.fastgetpixel(x,y) = clPurple then
        begin
          bmp.FastSetPixel(x,y,0);
          continue;
        end;
        if bmp.fastgetpixel(x,y) = clOlive then
        begin
          bmp.FastSetPixel(x,y,0);
          continue;
        end;
        if bmp.fastgetpixel(x,y) = clSilver then
        begin
          bmp.FastSetPixel(x,y,0);
          continue;
        end;
      end;
  end;
end;


{
  This uses the two filters, and performs a split on the bitmap.
  A split per character, that is. So we can more easily identify it.

  TODO:
  *
    Remove more noise after we have split, it should be possible to identify
    noise; weird positions or boxes compared to the rest, etc.
  *
    Split each colours seperately, and combine only later, after removing noise.

}
function TMOCR.getTextPointsIn(sx, sy, w, h: Integer; shadow: boolean;
                               var _chars, _shadows: T2DPointArray): Boolean;
var
   bmp, shadowsbmp, charsbmp: TMufasaBitmap;
   x,y: integer;
   {$IFDEF OCRDEBUG}
   dx,dy: integer;
   {$ENDIF}
   shadows: T2DPointArray;
   helpershadow: TPointArray;
   chars: TPointArray;
   charscount: integer;
   chars_2d, chars_2d_b, finalchars: T2DPointArray;
   pc: integer;
   bb: Tbox;

begin
  bmp := TMufasaBitmap.Create;
  { Increase to create a black horizonal line at the top and at the bottom }
  { This so the crappy algo can do it's work correctly. }
  bmp.SetSize(w + 2, h + 2);

  // Copy the client to out working bitmap.
  bmp.CopyClientToBitmap(TClient(Client).IOManager, False, 1{0},1, sx, sy, sx + w - 1, sy + h - 1);

  {$IFDEF OCRSAVEBITMAP}
  bmp.SaveToFile(OCRDebugPath + 'ocrinit.bmp');
  {$ENDIF}

  {$IFDEF OCRDEBUG}
    debugbmp := TMufasaBitmap.Create;
    debugbmp.SetSize(w + 2, (h + 2) * 7);
  {$ENDIF}
  {$IFDEF OCRDEBUG}
    DebugToBmp(bmp,0,h);
  {$ENDIF}

  // Filter 1
  FilterUpTextByColour(bmp,w,h);
  {$IFDEF OCRSAVEBITMAP}
  bmp.SaveToFile(OCRDebugPath + 'ocrcol.bmp');
  {$ENDIF}

  {$IFDEF OCRDEBUG}
    DebugToBmp(bmp,1,h);
  {$ENDIF}

  // Filter 2
  FilterUpTextByCharacteristics(bmp,w,h);

  {$IFDEF OCRSAVEBITMAP}
  bmp.SaveToFile(OCRDebugPath + 'ocrdebug.bmp');
  {$ENDIF}
  {$IFDEF OCRDEBUG}
    DebugToBmp(bmp,2,h);
  {$ENDIF}

  // create a bitmap with only the shadows on it
  shadowsbmp := bmp.copy;
  FilterShadowBitmap(shadowsbmp);
  {$IFDEF OCRDEBUG}
  DebugToBmp(shadowsbmp,3,h);
  {$ENDIF}

  // create a bitmap with only the chars on it
  charsbmp := bmp.copy;
  FilterCharsBitmap(charsbmp);
  {$IFDEF OCRDEBUG}
    DebugToBmp(charsbmp,4,h);
  {$ENDIF}

  // this gets the chars from the bitmap.

  // TODO:
  // We should make a different TPA
  // for each colour, rather than put them all in one. Noise can be a of a
  // differnet colour.
  setlength(chars, charsbmp.height * charsbmp.width);
  charscount:=0;
  for y := 0 to charsbmp.height - 1 do
    for x := 0 to charsbmp.width - 1 do
    begin
      if charsbmp.fastgetpixel(x,y) > 0 then
      begin
        chars[charscount]:=point(x,y);
        inc(charscount);
      end;
    end;
  setlength(chars,charscount);

  // split chars
  chars_2d := SplitTPAEx(chars,1,charsbmp.height);

  { FIXME: This only sorts the points in every TPA }
  SortATPAFrom(chars_2d, point(0,0));
  for x := 0 to high(chars_2d) do
  begin
    pc := random(clWhite);
    for y := 0 to high(chars_2d[x]) do
      charsbmp.FastSetPixel(chars_2d[x][y].x, chars_2d[x][y].y, pc);
  end;
  {$IFDEF OCRDEBUG}
    DebugToBmp(charsbmp,5,h);
  {$ENDIF}

  for y := 0 to high(chars_2d) do
  begin
    bb:=gettpabounds(chars_2d[y]);
    if (bb.x2 - bb.x1 > 10) or (length(chars_2d[y]) > 70) then
    begin // more than one char
      {$IFDEF OCRDEBUG}
      if length(chars_2d[y]) > 70 then
        mDebugLn('more than one char at y: ' + inttostr(y));
      if (bb.x2 - bb.x1 > 10) then
        mDebugLn('too wide at y: ' + inttostr(y));
      {$ENDIF}
      helpershadow:=getshadows(shadowsbmp,chars_2d[y]);
      chars_2d_b := splittpaex(helpershadow,2,shadowsbmp.height);
      //writeln('chars_2d_b length: ' + inttostr(length(chars_2d_b)));
      shadowsbmp.DrawATPA(chars_2d_b);
      for x := 0 to high(chars_2d_b) do
      begin
        setlength(shadows,length(shadows)+1);
        shadows[high(shadows)] :=  ConvTPAArr(chars_2d_b[x]);
      end;
    end else
    if length(chars_2d[y]) < 70 then
    begin
      setlength(shadows,length(shadows)+1);
      shadows[high(shadows)] := getshadows(shadowsbmp, chars_2d[y]);
    end;
  end;

  // sort, split messes with the order of chars
  SortATPAFromFirstPoint(chars_2d, point(0,0));
  for y := 0 to high(chars_2d) do
  begin
    if length(chars_2d[y]) > 70 then
      continue;
    setlength(finalchars,length(finalchars)+1);
    finalchars[high(finalchars)] := chars_2d[y];
  end;

  SortATPAFromFirstPoint(shadows, point(0,0));
  for x := 0 to high(shadows) do
  begin
    pc:=0;
    pc := random(clWhite);
    //pc := rgbtocolor(integer(round((x+1)*255/length(shadows))), round((x+1)*255/length(shadows)), round((x+1)*255/length(shadows)));
    for y := 0 to high(shadows[x]) do
      shadowsbmp.FastSetPixel(shadows[x][y].x, shadows[x][y].y, pc);
  end;
  {$IFDEF OCRDEBUG}
    DebugToBmp(shadowsbmp,6,h);
  {$ENDIF}

  _chars := finalchars;
  _shadows := shadows;

  bmp.Free;
  charsbmp.Free;
  shadowsbmp.Free;
  Result := true;
end;

{
  GetUpTextAtEx combines/uses the functions above.

  It will identify each character, and also keep track of the previous
  chars' final `x' bounds. If the difference between the .x2 of the previous
  character and the .x1 of the current character is bigger than 5, then there
  was a space between them. (Add ' ' to result)
}

function TMOCR.GetUpTextAtEx(atX, atY: integer; shadow: boolean): string;
var
   n:Tnormarray;
   ww, hh,i,j,nl: integer;
   font: TocrData;
   chars, shadows, thachars: T2DPointArray;
   t:Tpointarray;
   b,lb:tbox;
   lbset: boolean;

begin
  result:='';
  ww := 400;
  hh := 20;

  getTextPointsIn(atX, atY, ww, hh, shadow, chars, shadows);

  // Get font data for analysis.

  if shadow then
  begin
    font := FFonts.GetFont('UpChars_s');
    thachars := shadows;
  end
  else
  begin
    font := FFonts.GetFont('UpChars');
    thachars := chars;
  end;

  lbset:=false;
  setlength(n, (font.width+1) * (font.height+1));
  nl := high(n);
  for j := 0 to high(thachars) do
  begin
    for i := 0 to nl do
      n[i] := 0;

    t:= thachars[j];
    b:=gettpabounds(t);
    if not lbset then
    begin
      lb:=b;
      lbset:=true;
    end else
    begin
      // spacing
      if b.x1 - lb.x2 > 5 then
        result:=result+' ';
      lb:=b;
    end;


    for i := 0 to high(t) do
      t[i] := t[i] - point(b.x1,b.y1);

    {
      FIXME: If the TPA is too large, we can still go beyond n's bounds.
      We should check the bounds in GetTextPointsIn
    }
    for i := 0 to high(thachars[j]) do
    begin
      if (thachars[j][i].x) + ((thachars[j][i].y) * font.width) <= nl then
        n[(thachars[j][i].x) + ((thachars[j][i].y) * font.width)] := 1;
    end;
    result := result + GuessGlyph(n, font);
  end;
end;

function TMOCR.GetUpTextAt(atX, atY: integer; shadow: boolean): string;

begin
  if shadow then
    result := GetUpTextAtEx(atX, atY, true)
  else
    result := GetUpTextAtEx(atX, atY, false);
end;

function TMOCR.GetTextATPA(const ATPA : T2DPointArray;const maxvspacing : integer; font: string): string;
var
  b, lb: TBox;
  i, j, w, h: Integer;
  lbset: boolean;
  n: TNormArray;
  fD: TocrData;
  TPA: TPointArray;

begin
  Result := '';
  fD := FFonts.GetFont(font);

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
        mDebugLn('The automatically split characters are too wide. Try decreasing minspacing');
    end;
    result := result + GuessGlyph(n, fD);
  end;
end;

function TMOCR.GetTextAt(xs, ys, xe,ye, minvspacing, maxvspacing, hspacing,
                               color, tol: integer; font: string): string;
var
  TPA : TPointArray;
  STPA : T2DPointArray;
  B : TBox;
begin;
  SetLength(TPA, 0);
  TClient(Client).MFinder.FindColorsTolerance(TPA, color, xs,ys,xe,ye,tol);
  b := GetTPABounds(TPA);

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
  fD := FFonts.GetFont(font);
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

function TMOCR.TextToFontTPA(Text, font: String; out w, h: integer): TPointArray;

var
   fontD: TOcrData;
   c, i, x, y, off: Integer;
   d: TocrGlyphMetric;
   an: integer;

begin
  fontD := FFonts.GetFont(font);
  c := 0;
  off := 0;
  setlength(result, 0);

  for i := 1 to length(text)  do
  begin
    an := Ord(text[i]);
    if not InRange(an, 0, 255) then
    begin
      mDebugLn('WARNING: Invalid character passed to TextToFontTPA');
      continue;
    end;
    d := fontD.ascii[an];
    {writeln(format('xoff, yoff: %d, %d', [d.xoff, d.yoff]));
    writeln(format('bmp w,h: %d, %d', [d.width, d.height]));
    writeln(format('font w,h: %d, %d', [fontD.width, fontD.height])); }
    setlength(result, c+d.width*d.height);
    for y := 0 to fontD.height - 1 do
      for x := 0 to fontD.width - 1 do
      begin
        if fontD.pos[fontD.ascii[an].index][x + y * fontD.width] = 1 then
       // if fontD.pos[an][x + y * fontD.width] = 1 then
        begin
          result[c] := Point(x + off +d.xoff, y+d.yoff);
          inc(c);
        end;
      end;
    setlength(result, c);
    off := off + d.width;
  end;
  w := off;
  h := d.height;
 { writeln('C: ' + inttostr(c));      }
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

