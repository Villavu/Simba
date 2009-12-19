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
             function InitTOCR(path: string): boolean;
             function getTextPointsIn(sx, sy, w, h: Integer): TNormArray;
             function GetUpTextAt(atX, atY: integer): string;
      private
             Client: TObject;
             OCRData: TocrDataArray;
             OCRPath: string;

      end;

implementation

uses
  colour_conv, client,  files;

const
    ocr_Limit_High = 190;
    ocr_Limit_Low = 65;

    ocr_White = 16777215;
    ocr_Green = 65280;
    ocr_Red = 255;
    ocr_Yellow = 65535;
    ocr_Blue = 16776960;
    ocr_ItemC = 16744447;

    ocr_Purple = 8388736;


{
   Non optimised. ;-)
}

function TMOCR.getTextPointsIn(sx, sy, w, h: Integer): TNormArray;
var
   bmp: TMufasaBitmap;
   x,y: integer;
   r,g,b: integer;
   n: TNormArray;

begin
  bmp := TMufasaBitmap.Create;
  bmp.SetSize(w, h + 2);
  bmp.CopyClientToBitmap(TClient(Client).MWindow, False, 0,1, sx, sy, sx + w - 1, sy + h - 1);
  //bmp.CopyClientToBitmap(TClient(Client).MWindow, True, sx, sy, sx + w - 1, sy + h - 1);

  bmp.SaveToFile('/tmp/ocrinit.bmp');
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

      if (r < ocr_Limit_Low) and (g < ocr_Limit_Low) and (b < ocr_Limit_Low) then
      begin
        bmp.FastSetPixel(x,y, ocr_Purple);
        continue;
      end;

      bmp.fastsetpixel(x,y,0);
    end;

    // increase height by 1, so our algo works better.
    {bmp.SetSize(Bmp.Width, Bmp.Height+1);    }

    // first and last horiz line = 0
    for x := 0 to bmp.width -1 do
      bmp.fastsetpixel(x,0,0);
    for x := 0 to bmp.width -1 do
      bmp.fastsetpixel(x,bmp.height-1,0);

    bmp.SaveToFile('/tmp/ocrcol.bmp');

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

   { Optional - remove false shadow }
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

   { remove bad points }
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

    { may remove some pixels from chars. }
  {  for y := bmp.Height - 2 downto 1 do
       for x := bmp.Width - 2 downto 1 do
       begin
         if (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x+1,y)) and
            (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x-1,y)) and
            (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x,y+1)) and
            (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x,y-1)) then
            bmp.fastsetpixel(x,y, clOlive);
       end;   }
   { remove debug ;) }

   bmp.SaveToFile('/tmp/ocrdebug.bmp');

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

     setlength(n, bmp.Height * bmp.Width);

     for y := 0 to bmp.Height - 1 do
       for x := 0 to bmp.Width - 1 do
       begin
         if bmp.fastgetpixel(x,y) > 0 then
           n[x + y * bmp.width] := 1
         else
           n[x + y * bmp.width] := 0;
       end;

     result := n;
     bmp.SaveToFile('/tmp/ocrfinal.bmp');
     bmp.Free;
       { Dangerous removes all pixels that had no pixels on x-1 or x+1}
     {  for y := 0 to bmp.Height - 2 do
         for x := 1 to bmp.Width - 2 do
         begin
           if bmp.fastgetpixel(x,y) = clBlack then continue;
           if bmp.fastgetpixel(x,y) = clPurple then continue;
           if bmp.fastgetpixel(x,y) = clOlive then continue;
           if bmp.fastgetpixel(x,y) = clSilver then continue;
           if bmp.fastgetpixel(x,y) = clLime then continue;
           if (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x+1,y) )  and
              (bmp.fastgetpixel(x,y) <> bmp.fastgetpixel(x-1,y) ) then
              bmp.fastsetpixel(x,y,clFuchsia);
         end;                                }
end;

constructor TMOCR.Create(Owner: TObject);

var
   files: TStringArray;

begin
  inherited Create;
  Self.Client := Owner;

  SetLength(OCRData, 0);

  //files := GetFiles('/home/merlijn/Programs/mufasa/ben/upchars', 'bmp');

end;

destructor TMOCR.Destroy;

begin

  SetLength(OCRData, 0);
  inherited Destroy;
end;

function TMOCR.InitTOCR(path: string): boolean;
begin
  { This must be dynamic }

  SetLength(OCRData, 2);
  result := true;
  OCRPath := path + DS;
  if DirectoryExists(path + DS + 'UpChars' + DS) then
    OCRData[0] := ocrutil.InitOCR(path + DS + 'UpChars' + DS)
  else
    result := false;
  if DirectoryExists(path + DS + 'StatChars' + DS) then
    OCRData[1] := ocrutil.InitOCR(path + DS + 'StatChars' + DS)
  else
    result := false;
end;

function TMOCR.GetUpTextAt(atX, atY: integer): string;

var
   n:Tnormarray;
   ww, hh: integer;

begin
  ww := 400;
  hh := 20;

  n := getTextPointsIn(atX, atY, ww, hh);
  Result := ocrDetect(n, ww, hh, OCRData[0]);
  //writeln(result);
end;
    {
function TMOCR.GetUpTextAt(atX, atY: integer): string;

var
   t: tpointarray;
   b: TMufasaBitmap;
   i,len,ww,hh: integer;
   p:Prgb32;
   n:Tnormarray;

begin
 // t:=MakeTPAString('01:44 < mixster> Wizzup: Lies! How can my laptop handle that as well as playing music if even ownage machines suffer from it?');
  t:=MakeTPAString('Wizzup Lies How can my laptop handle that as well as playing music if even ownage machines suffer from it');
  writeln(inttostr(length(t)));
  b := TMufasaBitmap.Create;
  b.SetSize(1000,1000);
  for i := 0 to high(t) do
    b.FastSetPixel(t[i].x, t[i].y, clwhite);
  b.SaveToFile('/tmp/hoi.bmp');

  ww := b.width;
  hh := b.height;

  p := b.FData;
  len := ww * hh;
  setlength(n, ww * hh);

  for i := 0 to len - 1 do
    begin
      if((p^.R = 255) and (p^.B = 255) and (p^.G = 255))  //white
      or((p^.R = 255) and (p^.B < 2) and (p^.G < 2))      //red
      or((p^.R = 0) and (p^.B = 255) and (p^.G = 255))    //cyan
      or((p^.R = 255) and (p^.B = 0) and (p^.G = 255))    //yellow
      or((p^.R = 0) and (p^.B = 0) and (p^.G = 255)) then //green
        n[i] := 1
      else
        n[i] := 0;
      Inc(P);
    end;

  Result := ocrDetect(n, b.Width, b.Height, OCRData[1]);
  writeln(result);


end;               }
           {
function TMOCR.GetUpTextAt(atX, atY: integer): string;

var
   bmp: TMufasaBitmap;
   n: TNormArray;
   p: prgb32;
   w,h,ww,hh,len,i: integer;

begin
  result := '';
  TClient(Client).MWindow.GetDimensions(w, h);

  ww := 450;
  hh := 20;

  if ww + atX > w then
    ww := w - atX;
  if hh + atY > h then
    hh := h - atY;
  {writeln('ww and hh: ' + inttostr(ww) + ', ' + inttostr(hh));}
  bmp := TMufasaBitmap.Create;

  bmp.SetSize(ww, hh);
  bmp.CopyClientToBitmap(TClient(Client).MWindow, False, atX, atY, atX + ww - 1,
    atY + hh - 1);

  //bmp.SaveToFile('.' + DS + 'output.bmp');

  bmp.Posterize(127);
  //bmp.SaveToFile('.' + DS + 'posterize.bmp');
  bmp.Contrast(127);
  //bmp.SaveToFile('.' + DS + 'posterizecontrast.bmp');
  {bmp.Brightness(-20);
  bmp.SaveToFile('.' + DS + 'posterizecontrastbrightness.bmp');  }

  {writeln('bmp.w / bmp.h: ' + inttostr(bmp.Width) + ', ' + inttostr(bmp.height));
  writeln('wwhh: ' + inttostr(ww * hh));
  writeln('widhei: ' + inttostr(bmp.width * bmp.height));}

  bmp.SaveToFile('.' + DS + 'final.bmp');
  p := bmp.FData;

  len := ww * hh;
  setlength(n, ww * hh);
  for i := 0 to len - 1 do
    begin
      if((p^.R = 255) and (p^.B = 255) and (p^.G = 255))  //white
      or((p^.R = 255) and (p^.B < 2) and (p^.G < 2))      //red
      or((p^.R = 0) and (p^.B = 255) and (p^.G = 255))    //cyan
      or((p^.R = 255) and (p^.B = 0) and (p^.G = 255))    //yellow
      or((p^.R = 0) and (p^.B = 0) and (p^.G = 255)) then //green
        n[i] := 1
      else
        n[i] := 0;
      Inc(P);
    end;



 { n := ExtractText(bmp.FData, bmp.Width, bmp.Height); }
  {writeln('n: ' + inttostr(length(n))); }
  Result := ocrDetect(n, bmp.Width, bmp.Height, OCRData[0]);


  bmp.Free;
end;
                              }
end.

