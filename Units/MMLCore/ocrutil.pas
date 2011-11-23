{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

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

    OCR Util class for the Mufasa Macro Library
}
unit ocrutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes,bitmaps;

type
  TNormArray = array of integer; { Can we please call this TIntegerArray?}
  TocrGlyphMask = record
    ascii: char; { ASCII value - TODO: If we want unicode or something it
                   shouldn't be a char}
    width,height: integer; { Width, Height }
    l,r,t,b: integer; {Left, Right, Top, Bottom }
    mask: TNormArray; { 1D TPA. '0' is not part of char, 1 is part of char }
  end;

  TocrGlyphMaskArray = array of TocrGlyphMask;

  TocrGlyphMetric = record
    xoff,yoff: integer; { xoff and yoff (only on the left side, afaik
                          correspond to l and t (see InitOCR) }
    width,height: integer;
    index: integer; //stores the internal TocrData index for this char
    { Index is never used }

    inited : boolean; //This char has been loaded.
  end;

  TocrData = record
    ascii: array[0..255] of TocrGlyphMetric; {Why is this also named ascii? :(}
    pos: array of array of integer;
    pos_adj: array of real; { pixels / pixels that are 1 }
    neg: array of array of integer;
    neg_adj: array of real; { pixels / pixels that are 0 }
    map: array of char; { Ascii value - maps back to the char }
    width,height, max_width, max_height: integer;
    inputs,outputs: integer;
    {
      Inputs = bitfield
      Outputs = amount of masks
    }
  end;

  TocrDataArray = array of TocrData;

  { TODO: We can remove this and Extract Text as well }
  tLab = record
    L,a,b: real;
  end;

  procedure findBounds(glyphs: TocrGlyphMaskArray; out width,height,maxwidth,maxheight: integer);
  function LoadGlyphMask(const bmp : TMufasaBitmap; shadow: boolean; const ascii : char): TocrGlyphMask;
  function LoadGlyphMasks(const path: string; shadow: boolean): TocrGlyphMaskArray;
  function InitOCR(const Masks : TocrGlyphMaskArray): TocrData;
  function GuessGlyph(glyph: TNormArray; ocrdata: TocrData): char;
  function PointsToNorm(points: TpointArray; out w,h: integer): TNormArray;
  function ImageToNorm(src: TRGB32Array; w,h: integer): TNormArray;
  function ocrDetect(txt: TNormArray; w,h: integer; var ocrdata: TocrData): string;
  function ExtractText(colors: PRGB32;{colors: tRGBArray;} w,h: integer): TNormArray;

implementation
uses
  math,
  {Begin To-Remove units. Replace ReadBmp with TMufasaBitmap stuff later.}
  graphtype, intfgraphics,graphics;
  {End To-Remove unit}

{initalizes the remaining fields from a TocrGlyphMask and finds the global bounds}

{
  Function initialises the following fields:
      l,r,t,b (Left, Right, Top, Bottom)
      returns maxwidth and maxheight of the font.


  TODO: out width,height seem to be dead variables? Fields seem to be used, but
  I have idea what for yet. Isn't it exactly the same as maxwidth and maxheight?

  TODO: It does not seem to normalize the characters.

}
procedure findBounds(glyphs: TocrGlyphMaskArray; out width,height,maxwidth,maxheight: integer);
var
  i,x,y,c,w,h: integer;
  minx,miny,maxx,maxy : integer;
  l,r,t,b: integer;
  dat: TNormArray;
begin
  width:= 0;
  height:= 0;
  MaxWidth := 0;
  MaxHeight := 0;
  minx := 9000;
  miny := 9000;
  maxx := 0;
  maxy := 0;
  for c:= 0 to length(glyphs) - 1 do
  begin
    dat:= glyphs[c].mask;
    w:= glyphs[c].width;
    h:= glyphs[c].height;
    l:= w; {left}
    r:= 0; {right}
    t:= h; {top}
    b:= 0; {bottom}
    for i:= 0 to w*h-1 do
    begin
      if dat[i] = 1 then
      begin
        { Get position in x, y }
        x:= i mod w;
        y:= i div w;
        if x > maxx then maxx := x;
        if x < minx then minx := x;
        if x > r then r:= x;
        if x < l then l:= x;
        if y > maxy then maxy := y;
        if y < miny then miny := y;
        if y > b then b:= y;
        if y < t then t:= y;
      end;
    end;
    if l = w then l:= 0;
    if t = h then t:= 0;
    glyphs[c].r:= r;
    glyphs[c].l:= l;
    glyphs[c].b:= b;
    glyphs[c].t:= t;
    if (r - l + 1) > width then width:= r - l + 1;
    if (b - t + 1) > height then height:= b - t + 1;
  end;
  maxwidth:= (maxx-minx) + 1;
  maxheight := (maxy - miny) + 1;
end;

{Use whatever you want if you don't like this}
function GetFiles(Path, Ext: string): TstringArray;
var
  SearchRec : TSearchRec;
  c : integer;
begin
  c := 0;
  if FindFirst(Path + '*.' + ext, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      inc(c);
      SetLength(Result,c);
      Result[c-1] := SearchRec.Name;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

{
  Load one bitmap and return the accompanying TOCRGlyphMask.

  TODO: We should probably just calculate the shadow rather than use the red pixels.
}
function LoadGlyphMask(const bmp: TMufasaBitmap; shadow: boolean; const ascii : char): TocrGlyphMask;
var
  size,j: integer;
  color: TRGB32;
  shadow_i: byte;
begin
  if shadow then
    shadow_i := 0
  else
    shadow_i := 255;

  size := bmp.Width * bmp.Height;
  SetLength(result.mask, size);

  for j := 0 to size-1 do
  begin
    color := bmp.FData[j];
    {
    The character colour is white (255,255,255) if it is a character, red if it
    is a shadow.
    r should always be 255, g and b either 0 or 255, depending wether it is a
    colour.
    }
    if (color.r = 255) and (color.g = shadow_i) and (color.b = shadow_i) then
      result.mask[j]:= 1
    else
      result.mask[j]:= 0; { Not part of shadow or character }
  end;

  result.width:= bmp.width;
  result.height:= bmp.height;
  result.ascii:= ascii;
end;

{
  This Loads the actual data from the .bmp, but does not init all fields and
  doesn't normalize the characters.
}
function LoadGlyphMasks(const path: string; shadow: boolean): TocrGlyphMaskArray;
var
  strs: array of string;
  bmp : TMufasaBitmap;
  len,i: integer;
begin
  strs:= GetFiles(path,'bmp');
  len:= length(strs);
  SetLength(result,len);
  bmp := TMufasaBitmap.Create;
  for i:= 0 to len-1 do
  begin
    bmp.LoadFromFile(path + strs[i]);
    SetLength(strs[i],Length(strs[i])-4);
    Result[i] := LoadGlyphMask(bmp,shadow,chr(strtoint(strs[i])));
  end;
  Bmp.free;
end;

{Fully initalizes a TocrData structure, this is LoadFont or whatever, call it first}
{
  InitOCR initialises each character. 
}
function InitOCR(const masks : TocrGlyphMaskArray): TocrData;
var
  t,b,l,r,w,h,mw: integer;
  x,y: integer;
  maxw,maxh : integer;
  c,i,len,size: integer;
  pos: integer;
  ascii: char;
begin
  w:= 0;
  h:= 0;

  { Find the maxw and maxh, initialise the l,r,b,h fields }
  findBounds(masks,w,h,maxw,maxh);

  len:= Length(masks);

  result.width:= w;
  result.height:= h;
  result.max_width := maxw;
  result.max_height := maxh;

  { What are w and h even? They are reset the loop in findBounds every time }
  { Perhaps you want max_width and max_height? }
  size:= w * h;

  SetLength(result.pos,len,size);
  SetLength(result.pos_adj,len);
  SetLength(result.neg,len,size);
  SetLength(result.neg_adj,len);

  { Maps back to value of char }
  SetLength(result.map,len);

  for i := 0 to 255 do
    Result.ascii[i].inited:= false;

  {
    This loop seems to normalize the values (store them in pos and neg).
  }
  for i:= 0 to len - 1 do
  begin
    ascii:= masks[i].ascii;

    pos:= 0;
    l:= masks[i].l;
    r:= masks[i].r;
    b:= masks[i].b;
    t:= masks[i].t;
    mw:= masks[i].width;
    for y:= t to b do
    begin
      for x:= l to r do
      begin
        c:= (x-l) + (y-t)*w;
        if masks[i].mask[x+y*mw] <> 0 then
        begin
          result.pos[i][c]:= 1;
          inc(pos);
        end else
          result.pos[i][c] := 0;
      end;
    end;

    { Inverted of pos, 1 if pos is 0, 0 if neg was 1 }
    for c:= 0 to size-1 do
      result.neg[i][c]:= 1 - result.pos[i][c];

    {
        local var pos = the amount of pixels with the value 1.
        So pos_adj and neg_adj is the percentage of pixels that are 1 and 0,
        respectively.
    }
    if pos = 0 then result.neg_adj[i]:= 1 else result.neg_adj[i]:= 1 / pos;
    if pos = 0 then result.pos_adj[i]:= 0 else result.pos_adj[i]:= 1 / pos;

    { Map back to ascii value. }
    result.map[i]:= ascii;
    result.ascii[ord(ascii)].index:= i; { Reference back to masks, I think.
                                          TODO: NEVER USED }
    result.ascii[ord(ascii)].xoff:= masks[i].l;
    result.ascii[ord(ascii)].yoff:= masks[i].t;

    { These are not changed by the loop }
    result.ascii[ord(ascii)].width:= masks[i].width;
    result.ascii[ord(ascii)].height:= masks[i].height;

    { Done }
    result.ascii[ord(ascii)].inited:= true;
  end;

  { What is size ?? w * h, but those are just maxwidth and maxheight? }
  result.inputs:= size;

  { Amount of masks }
  result.outputs:= len;
end;

{guesses a glyph stored in glyph (which is an 1-0 image of the size specified by width and height in ocrdata}
function GuessGlyph(glyph: TNormArray; ocrdata: TocrData): char;
var
  i,c,inputs,outputs,val: integer;
  pos_weights: array of real;
  neg_weights: array of real;
  max, weight: real;
begin
  SetLength(pos_weights,ocrdata.outputs);
  SetLength(neg_weights,ocrdata.outputs);
  inputs:= ocrdata.inputs - 1;
  outputs:= ocrdata.outputs - 1;

  {
    This loop calculates the ``weights'' for each mask/character compared to the
    glyph passed.

    pos_weights contains (after the loop) the amount of correct fields that
    should be 1. 
    neg_weights contains (after the loop) the amount of correct fields that
    should be 0.
  }
  for i:= 0 to inputs do { every entry of the bitfield }
  begin
    val:= glyph[i]; { Value of the glyph at that point - 1 or 0. }
    for c:= 0 to outputs do { amount of masks }
    begin
      pos_weights[c]:= pos_weights[c] + ocrdata.pos[c][i] * val;
      neg_weights[c]:= neg_weights[c] + ocrdata.neg[c][i] * val;
    end
  end;

  max:= 0;
  for i:= 0 to outputs do
  begin
    {
      Weight = the amount of correct POS matches * the percentage of `1' pixels
      in the orig char, minus the amount of negative weights * the percentage of
      `0' pixels in the orig char.

      The first part (POS part) is high if there are a lot of good matches (if
      they share a lot of `1's at the same spot, the second is LOW (you want
      this) if the amount of matches of `0's matches the orig char.
    }
    weight:= pos_weights[i] * ocrdata.pos_adj[i] - neg_weights[i] * ocrdata.neg_adj[i];
    if (weight > max) then
    begin
      max:= weight;
      result:= ocrdata.map[i];
    end;
  end;
end;

{converts a TPA into a 1-0 image of the smallest possible size}
function PointsToNorm(points: TpointArray; out w,h: integer): TNormArray;
var
  l,r,t,b: integer;
  i,len,size: integer;
  norm: TNormArray;
begin
  len:= length(points);
  l:= points[0].x;
  r:= points[0].x;
  t:= points[0].y;
  b:= points[0].y;
  for i:= 1 to len-1 do
  begin
    if points[i].x < l then l:= points[i].x;
    if points[i].x > r then r:= points[i].x;
    if points[i].y < t then t:= points[i].y;
    if points[i].y > b then b:= points[i].y;
  end;
  w:= r - l + 1;
  h:= b - t + 1;
  size:= w * h;
  SetLength(norm,size);
  for i:= 0 to len-1 do
    norm[(points[i].x - l) + (points[i].y - t) * w]:= 1;
  result:= norm;
end;

function ImageToNorm(src: TRGB32Array; w,h: integer): TNormArray;
var
  norm: TNormArray;
  i: integer;
begin
  SetLength(norm,w*h);
  for i:= 0 to w*h-1 do
    if (src[i].r = 255) and (src[i].g = 255) and (src[i].b = 255) then
      norm[i]:= 1 else  norm[i]:= 0;
  result:= norm;
end;

{takes a mask of only one line of text, a TocrData, and returns the string in it}
function ocrDetect(txt: TNormArray; w,h: integer; var ocrdata: TocrData): string;
var
  l,r,t,b,x,y,xx,yy: integer;
  upper,left,last,spaces: integer;
  glyph: TNormArray;
  empty: boolean;
  ascii: char;
begin
  result:= '';
  l:= -1;
  r:= -1;
  upper:= -9001; //large negative
  left:= -9001; //large negative
  x:= 0;
  while x < w do
  begin
    empty:= true;
    for y:= 0 to h-1 do
    begin
      if txt[x+y*w] = 1 then
      begin
        empty:= false;
        break;
      end;
    end;
    if (l = -1) and (not empty) then
    begin
      l:= x
    end else if (l <> -1) then
    begin
      if empty then
        r:= x - 1
      else if x = w-1 then
        r:= x;
    end;
    if (r <> -1) and (l <> -1) then
    begin
      t:= -1;
      b:= -1;
      SetLength(glyph,0);
      SetLength(glyph,ocrdata.width*ocrdata.height);
      for yy:= 0 to h-1 do
      begin
        for xx:= l to r do
          if txt[xx+yy*w] = 1 then begin t:= yy; break; end;
        if t <> -1 then break;
      end;
      for yy:= h-1 downto 0 do
      begin
        for xx:= l to r do
          if txt[xx+yy*w] = 1 then begin b:= yy; break; end;
        if b <> -1 then break;
      end;
      if b - t + 1 > ocrdata.height then b:= b - (b-t+1-ocrdata.height);
      if r - l + 1 > ocrdata.width then r:= r - (r-l+1-ocrdata.width);
      for yy:= t to b do
        for xx:= l to r do
          glyph[(xx-l) + (yy-t)*ocrdata.width]:= txt[xx+yy*w];

      ascii:= GuessGlyph(glyph,ocrdata);
      if (upper = -9001) or (left = -9001) then
      begin
        upper:= t - ocrdata.ascii[ord(ascii)].yoff;
        left:= l - ocrdata.ascii[ord(ascii)].xoff + ocrdata.ascii[ord(ascii)].width;
        x:= left;
      end else
      begin
        last:= left;
        left:= l - ocrdata.ascii[ord(ascii)].xoff;
        if last <> left then
        begin
          for spaces:= 1 to (left - last) div ocrdata.ascii[32].width do
              result:= result + ' ';
        end;
        left:= left + ocrdata.ascii[ord(ascii)].width;
        x:= left;
      end;
      result:= result + ascii;
      l:= -1;
      r:= -1;
    end;
    inc(x);
  end;
end;

function AvgColors(color1:TRGB32; weight1: integer; color2: TRGB32; weight2: integer): TRGB32;
begin
  result.r:= (color1.r * weight1 + color2.r * weight2) div (weight1 + weight2);
  result.g:= (color1.g * weight1 + color2.g * weight2) div (weight1 + weight2);
  result.b:= (color1.b * weight1 + color2.b * weight2) div (weight1 + weight2);
end;

procedure RGBtoXYZ(color: TRGB32; out X, Y, Z: real); inline;
var
  nr,ng,nb: real;
begin
  nr:= color.r / 255.0;
  ng:= color.g / 255.0;
  nb:= color.b / 255.0;
  if nr <= 0.04045 then nr:= nr / 12.92 else nr:= power((nr + 0.055)/1.055,2.4);
  if ng <= 0.04045 then ng:= ng / 12.92 else ng:= power((ng + 0.055)/1.055,2.4);
  if nb <= 0.04045 then nr:= nb / 12.92 else nb:= power((nb + 0.055)/1.055,2.4);
  X:= 0.4124*nr + 0.3576*ng + 0.1805*nb;
  Y:= 0.2126*nr + 0.7152*ng + 0.0722*nb;
  Z:= 0.0193*nr + 0.1192*ng + 0.9505*nb;
end;

function labmod(i: real): real; inline;
begin
  if i > power(0.206896552,3) then
    result:= power(i,0.333333333)
  else
    result:= 7.787037037*i + 0.137931034;
end;

function ColortoLab(c: TRGB32): tLab; inline;
var
  X,Y,Z,sum,Xn,Yn,Zn: real;
begin
  RGBtoXYZ(c,X,Y,Z);
  sum:= X + Y + Z;
  if(sum = 0) then
  begin
    result.l := 0.0;
    result.a := 0.0;
    result.b := 0.0;
  end;
  Xn:= X / sum;
  Yn:= Y / sum;
  Zn:= Z / sum;
  result.L:= 116.0*labmod(y/yn) - 16.0;
  result.a:= 500.0*(labmod(x/xn)-labmod(y/yn));
  result.b:= 500.0*(labmod(y/yn)-labmod(z/zn));
end;

function colorDistSqr(a,b:TRGB32): integer; inline;
begin
  result:= (a.r-b.r)*(a.r-b.r)+(a.b-b.b)*(a.b-b.b)+(a.g-b.g)*(a.g-b.g);
end;

function ExtractText(colors: PRGB32;{colors: tRGBArray;} w,h: integer): TNormArray;
const
  GradientMax = 2.0;
  white:  TRGB32= ( b: $FF; g: $FF; r: $FF; a: $00 );
  cyan:   TRGB32= ( b: $FF; g: $FF; r: $00; a: $00 );
  yellow: TRGB32= ( b: $00; g: $EF; r: $FF; a: $00 );
  red:    TRGB32= ( b: $00; g: $00; r: $FF; a: $00 );
  green:  TRGB32= ( b: $00; g: $FF; r: $00; a: $00 );
var
  up, left: boolean;
  len,numblobs,thisblob,lastblob,i,j,used: integer;
  blobbed,blobcount,stack: array of integer;
  labs: array of tLab;
  a,b: tLab;
  blobcolor: TRGB32Array;
  newcolors: array of integer;
  c: TRGB32;
  norm: TNormArray;
begin
  len:= w*h;
  SetLength(blobbed,len);
  SetLength(blobcount,len);
  SetLength(blobcolor,len);
  SetLength(stack,len);
  SetLength(labs,len);
  for i:= 0 to len-1 do
    labs[i]:= ColorToLab( TRGB32(colors[i]));
  numblobs:= 0;
  for i:= 0 to len-1 do
  begin
    a:= labs[i];
    if i >= w then
    begin
      b:= labs[i-w];
      up:= abs(a.L-b.L)+abs(a.a-b.a)+abs(a.b-b.b) <= GradientMax;
    end else
      up:= false;
    if i mod w <> 0 then
    begin
      b:= labs[i-1];
      left:= abs(a.L-b.L)+abs(a.a-b.a)+abs(a.b-b.b) <= GradientMax;
    end else
      left:= false;
    if left and up then
    begin
      thisblob:= blobbed[i-w];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],tRGB32(colors[i]),1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
      lastblob:= blobbed[i-1];
      if lastblob <> thisblob then
      begin
        used:= 1;
        stack[0]:= i-1;
        while used > 0 do
        begin
          used:= used - 1;
          j:= stack[used];
          if blobbed[j] = lastblob then
          begin
            blobbed[j]:= thisblob;
            if j >= w then if blobbed[j-w] = lastblob then begin stack[used]:= j-w; used:= used + 1; end;
            if j mod w <> 0 then if blobbed[j-1] = lastblob then begin stack[used]:= j-1; used:= used + 1; end;
            if (j+1) mod w <> 0 then if blobbed[j+1] = lastblob then  begin stack[used]:= j+1; used:= used + 1; end;
            if j < w*h-w then if blobbed[j+w] = lastblob then  begin stack[used]:= j+w; used:= used + 1; end;
          end;
        end;
        blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],blobcolor[lastblob],blobcount[lastblob]);
        blobcount[thisblob]:= blobcount[thisblob] + blobcount[lastblob];
        blobcount[lastblob]:= 0;
      end;
    end else if left then
    begin
      thisblob:= blobbed[i-1];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],tRGB32(colors[i]),1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
    end else if up then
    begin
      thisblob:= blobbed[i-w];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],tRGB32(colors[i]),1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
    end else
    begin
      blobbed[i]:= numblobs;
      blobcount[numblobs]:= 1;
      blobcolor[numblobs]:= tRGB32(colors[i]);
      numblobs:= numblobs + 1;
   end;
  end;
  SetLength(blobcount,numblobs);
  SetLength(blobcolor,numblobs);
  //Done blobfinding, extract char masks
  SetLength(newcolors,numblobs);
  for i:= 0 to numblobs-1 do
  begin
    j:= blobcount[i];
    if j > 0 then
    begin
      c:= blobcolor[i];
      if (j > 50) or (j < 2) then
        newcolors[i]:= 0
      else if colorDistSqr(white,c) <= 10000 then
        newcolors[i]:= 1
      else if colorDistSqr(cyan,c) <= 10000 then
        newcolors[i]:= 1
      else if colorDistSqr(yellow,c) <= 10000 then
        newcolors[i]:= 1
      else if colorDistSqr(red,c) <= 10000 then
        newcolors[i]:= 1
      else if colorDistSqr(green,c) <= 10000 then
        newcolors[i]:= 1
      else
        newcolors[i]:= 0;
    end;
  end;
  SetLength(norm,len);
  for i:= 0 to len-1 do
    norm[i]:= newcolors[blobbed[i]];
  result:= norm;
end;

end.

