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
  Classes, SysUtils, MufasaTypes, bitmaps, math, ocrutil,
  {Begin To-Remove units. Replace ReadBmp with TMufasaBitmap stuff later.}
  graphtype, intfgraphics,graphics;
  {End To-Remove unit}

  type
      TMOCR = class(TObject)
             constructor Create(Owner: TObject);
             destructor Destroy; override;
             function InitTOCR(path: string; shadow: Boolean): boolean;
             function GetFontIndex(FontName: string): integer;
             function GetFont(FontName: string): TocrData;

             function getTextPointsIn(sx, sy, w, h: Integer; shadow: boolean;
                      var _chars, _shadows: T2DPointArray): Boolean;
             function GetUpTextAtEx(atX, atY: integer; shadow: boolean): string;
             function GetUpTextAt(atX, atY: integer; shadow: boolean): string;

             procedure FilterUpTextByColour(bmp: TMufasaBitmap; w,h: integer);
             procedure FilterUpTextByCharacteristics(bmp: TMufasaBitmap; w,h: integer);
             procedure FilterShadowBitmap(bmp: TMufasaBitmap);
             procedure FilterCharsBitmap(bmp: TMufasaBitmap);
             {$IFDEF OCRDEBUG}
             procedure DebugToBmp(bmp: TMufasaBitmap; hmod,h: integer);
             {$ENDIF}
      private
             Client: TObject;
             OCRData: TocrDataArray;
             OCRNames: Array Of String;
             OCRPath: string;
      {$IFDEF OCRDEBUG}
      public
             debugbmp: TMufasaBitmap;
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
    ocr_Limit_High = 190;
    ocr_Limit_Med = 130;
    ocr_Limit_Low = 65;

    ocr_White = 16777215;
    ocr_Green = 65280;
    ocr_Red = 255;
    ocr_Yellow = 65535;
    ocr_Blue = 16776960;
    ocr_ItemC = 16744447;

    ocr_Purple = 8388736;

{
**************************************************************************************************
**************************************************************************************************
**************************************************************************************************
**************************************************************************************************
**************************************************************************************************
When splitting the shadows, having the `original' points of the chars would be very helpful when splitting!

Short description:
Merge shadow and char; (Same colour; simple)
Split with spacing of 1.
Remove character points; leaves only shadow. Voila, perfect splitting.
Splitting could go wrong for shadows of characters with two much spacing; like 'h'.

In some cases, this may `bind' characters to each other.
In this case, we need to remove the shadow and split again. After this split
we can put the shadows back in, and see to what group they belong.

We can also just split the chars, and then use their shadow.
**************************************************************************************************
**************************************************************************************************
**************************************************************************************************
**************************************************************************************************
**************************************************************************************************
}




{
   Non optimised. ;-)
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

  // Now we do another filter like
 for y := bmp.Height - 2 downto 0 do
   for x := bmp.Width - 2 downto 0 do
   begin
     if bmp.fastgetpixel(x,y) = clPurple then
       continue;
     if bmp.fastgetpixel(x,y) = clBlack then
       continue;
     if (bmp.fastgetpixel(x,y) = bmp.fastgetpixel(x+1,y+1) ) then
       continue;

     if bmp.fastgetpixel(x+1,y+1) <> clPurple then
     begin
       bmp.fastsetpixel(x,y,clOlive);
       continue;
     end;
   end;
end;

{$IFDEF OCRDEBUG}
procedure TMOCR.DebugToBmp(bmp: TMufasaBitmap; hmod, h: integer);
var
   x,y: integer;
begin
 for y := 0 to bmp.height - 1 do
   for x := 0 to bmp.width - 1 do
     debugbmp.fastsetpixel(x,y + hmod *h,bmp.fastgetpixel(x,y));
end;
{$ENDIF}

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

function TMOCR.getTextPointsIn(sx, sy, w, h: Integer; shadow: boolean;
                               var _chars, _shadows: T2DPointArray): Boolean;
var
   bmp, shadowsbmp, charsbmp: TMufasaBitmap;
   x,y: integer;
   r,g,b: integer;
   n: TNormArray;
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
  bmp.CopyClientToBitmap(TClient(Client).MWindow, False, 1{0},1, sx, sy, sx + w - 1, sy + h - 1);

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

  chars_2d := SplitTPAEx(chars,1,charsbmp.height);
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
        writeln('more than one char at y: ' + inttostr(y));
      if (bb.x2 - bb.x1 > 10) then
        writeln('too wide at y: ' + inttostr(y));
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
end;

constructor TMOCR.Create(Owner: TObject);

var
   files: TStringArray;

begin
  inherited Create;
  Self.Client := Owner;

  SetLength(OCRData, 0);
  SetLength(OCRNames, 0);
end;

destructor TMOCR.Destroy;

begin

  SetLength(OCRData, 0);
  SetLength(OCRNames, 0);
  inherited Destroy;
end;

function TMOCR.InitTOCR(path: string; shadow: boolean): boolean;
var
   dirs: array of string;
   i: longint;
   dir: string;
begin
  { This must be dynamic }
  writeln(path);

  dirs := GetDirectories(path);


  SetLength(OCRData, length(dirs) * 2);
  SetLength(OCRNames, length(dirs) * 2);

  for i := 0 to high(dirs) do
  begin
    OCRData[i] := ocrutil.InitOCR(path + dirs[i] + DS, false);
    OCRNames[i] := dirs[i];
    OCRData[i+length(dirs)] := ocrutil.InitOCR(path + dirs[i] + DS, true);
    OCRNames[i+length(dirs)] := dirs[i] + '_s';
    {writeln('Loaded Font ' + OCRNames[i]);
    writeln('Loaded Font ' + OCRNames[i+1]);}
  end;
  Result := (length(OCRData) > 0);
  OCRPath := path;
end;

function TMOCR.GetFontIndex(FontName: string): integer;
var
   i: integer;
begin
  if length(OCRNames) <> length(OCRData) then
    raise Exception.Create('Internal OCR error. Len(OCRData) <> Len(OCRNames)');
  for i := 0 to high(OCRNames) do
    if FontName = OCRNames[i] then
    begin
      Exit(i);
    end;
  raise Exception.Create('Font ' + FontName + ' is not loaded.');
end;

function TMOCR.GetFont(FontName: string): TocrData;
var
   i: integer;
begin
  if length(OCRNames) <> length(OCRData) then
    raise Exception.Create('Internal OCR error. Len(OCRData) <> Len(OCRNames)');
  for i := 0 to high(OCRNames) do
    if FontName = OCRNames[i] then
    begin
      Exit(OCRData[i]);
    end;
  raise Exception.Create('Font ' + FontName + ' is not loaded.');
end;

function TMOCR.GetUpTextAtEx(atX, atY: integer; shadow: boolean): string;
var
   n:Tnormarray;
   ww, hh,i,j: integer;
   font: TocrData;
   chars, shadows, thachars: T2DPointArray;
   t:Tpointarray;
   b,lb:tbox;
   lbset: boolean;

begin
  ww := 400;
  hh := 20;
  getTextPointsIn(atX, atY, ww, hh, shadow, chars, shadows);

  // only shadow!
  //shadow:=true;
  if shadow then
  begin
    font := GetFont('UpChars_s');
    thachars := shadows;
    {$IFDEF OCRDEBUG}
    writeln('using shadows');
    {$ENDIF}
  end
  else
  begin
    font := GetFont('UpChars');
    thachars := chars;
    {$IFDEF OCRDEBUG}
    writeln('not using shadows');
    {$ENDIF}
  end;

  lbset:=false;
  //writeln(format('FFont Width/Height: (%d, %d)', [font.width,font.height]));
  setlength(n, (font.width+1) * (font.height+1));
  for j := 0 to high(thachars) do
  begin
    for i := 0 to high(n) do
      n[i] := 0;

    t:= thachars[j];
    b:=gettpabounds(t);
    if not lbset then
    begin
      lb:=b;
      lbset:=true;
    end else
    begin
      if b.x1 - lb.x2 > 5 then
        result:=result+' ';
      lb:=b;
    end;
    for i := 0 to high(t) do
      t[i] := t[i] - point(b.x1,b.y1);

    for i := 0 to high(thachars[j]) do
    begin
      n[(thachars[j][i].x) + ((thachars[j][i].y) * font.width)] := 1;
    end;
    result := result + GuessGlyph(n, font);
    //writeln('--'+GuessGlyph(n, font));
  end;


  //Result := ocrDetect(n, ww-1, hh-1, font);
  //Result:='To do';
  //Result:='';
end;

function TMOCR.GetUpTextAt(atX, atY: integer; shadow: boolean): string;

begin
  if shadow then
    result := GetUpTextAtEx(atX, atY, true)
  else
    result := GetUpTextAtEx(atX, atY, false);
end;

end.

