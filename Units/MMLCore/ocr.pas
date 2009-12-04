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

             function GetUpTextAt(atX, atY: integer): string;
      private
             Client: TObject;
             OCRData: TocrDataArray;
             OCRPath: string;

      end;

implementation

uses
  client;

constructor TMOCR.Create(Owner: TObject);

var
   files: TStringArray;

begin
  inherited Create;
  Self.Client := Owner;

  SetLength(OCRData, 0);

  //files := GetFiles('/home/merlijn/Programs/mufasa/UpText/upchars', 'bmp');

end;

destructor TMOCR.Destroy;

begin

  SetLength(OCRData, 0);
  inherited Destroy;
end;

function TMOCR.InitTOCR(path: string): boolean;
begin
  { This must be dynamic }

  SetLength(OCRData, 1);
  result := true;
  OCRPath := path + DS;
  if DirectoryExists(path + DS + 'UpChars' + DS) then
    OCRData[0] := ocrutil.InitOCR(path + DS + 'UpChars' + DS)
  else
    result := false;

end;

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

end.

