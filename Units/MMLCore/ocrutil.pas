unit ocrutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes;

type
    TNormArray = array of integer;
    TocrGlyphMask = record
        ascii: char;
        width,height: integer;
        l,r,t,b: integer;
        mask: TNormArray;
    end;
    TocrGlyphMaskArray = array of TocrGlyphMask;
    TocrGlyphMetric = record
        xoff,yoff: integer;
        width,height: integer;
    end;
    TocrData = record
        ascii: array[0..255] of TocrGlyphMetric;
        pos: array of array of integer;
        pos_adj: array of real;
        neg: array of array of integer;
        neg_adj: array of real;
        map: array of char;
        width,height: integer;
        inputs,outputs: integer;
    end;

    TocrDataArray = array of TocrData;

   { We already have PRGB32 -> To be removed later }
   type
    tRGB = packed record
       B, G, R, A: Byte;
   end;
   tRGBArray = array of tRGB;

   {Begin To be removed}
   Tbmp = record
       data: array of tRGB;
       width,height: integer;
   end;
   {End   To be removed}


    tLab = record
        L,a,b: real;
    end;

    procedure findBounds(glyphs: TocrGlyphMaskArray; out width,height: integer);
    function LoadGlyphMasks(path: string): TocrGlyphMaskArray;
    function InitOCR(path: string): TocrData;
    function GuessGlyph(glyph: TNormArray; ocrdata: TocrData): char;
    function PointsToNorm(points: TpointArray; out w,h: integer): TNormArray;
    function ImageToNorm(src: tRGBArray; w,h: integer): TNormArray;
    function ocrDetect(txt: TNormArray; w,h: integer; var ocrdata: TocrData): string;
    function ExtractText(colors: PRGB32;{colors: tRGBArray;} w,h: integer): TNormArray;
    function MakeTPAString(str: string): TpointArray;

implementation
uses
  bitmaps, math,
  {Begin To-Remove units. Replace ReadBmp with TMufasaBitmap stuff later.}
  graphtype, intfgraphics,graphics;
  {End To-Remove unit}

function ReadBMP(path: string): Tbmp;
var
  LazIntf : TLazIntfImage;
  RawImageDesc : TRawImageDescription;
  data: array of tRGB;
begin
  if FileExists(path) then
  begin;
    LazIntf := TLazIntfImage.Create(0,0);
    RawImageDesc.Init_BPP32_B8G8R8_BIO_TTB(LazIntf.Width,LazIntf.Height);
    LazIntf.DataDescription := RawImageDesc;
    LazIntf.LoadFromFile(path);
    Result.width := LazIntf.Width;
    Result.height := LazIntf.Height;
    SetLength(data,LazIntf.Width*LazIntf.Height);
    Move(LazIntf.PixelData[0],data[0],LazIntf.Width*LazIntf.Height*sizeOf(tRGB));
    Result.data:= data;
    LazIntf.Free;
  end;
end;




{initalizes the remaining fields from a TocrGlyphMask and finds the global bounds}
procedure findBounds(glyphs: TocrGlyphMaskArray; out width,height: integer);
var
    i,x,y,c,w,h: integer;
    l,r,t,b: integer;
    dat: TNormArray;
begin
    width:= 0;
    height:= 0;
    for c:= 0 to length(glyphs) - 1 do
    begin
        dat:= glyphs[c].mask;
        w:= glyphs[c].width;
        h:= glyphs[c].height;
        l:= w;
        r:= 0;
        t:= h;
        b:= 0;
        for i:= 0 to w*h-1 do
        begin
            if dat[i] = 1 then
            begin
                x:= i mod w;
                y:= i div w;
                if x > r then r:= x;
                if x < l then l:= x;
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

{This Loads the actual data from the .bmp, but does not init all fields}
function LoadGlyphMasks(path: string): TocrGlyphMaskArray;
var
    strs: array of string;
    bmp: array of Tbmp; {-> TMufasaBitmap, and why use an array? }
    len,size,i,j: integer;
    color: tRGB;
begin
    strs:= GetFiles(path,'bmp');
    len:= length(strs);
    SetLength(result,len);
    SetLength(bmp,len);
    for i:= 0 to len-1 do
    begin
        bmp[i]:= ReadBmp(path + strs[i]);
        size:= bmp[i].width*bmp[i].height;
        SetLength(result[i].mask,size);
        for j:= 0 to size-1 do
        begin
            color:= bmp[i].data[j];
            if (color.r = 255) and (color.g = 255) and (color.b = 255) then
            //if (color.r = 255) and (color.g = 0) and (color.b = 0) then
                result[i].mask[j]:= 1
            else
                result[i].mask[j]:= 0;
        end;
        result[i].width:= bmp[i].width;
        result[i].height:= bmp[i].height;
        SetLength(strs[i],Length(strs[i])-4);
        result[i].ascii:= chr(strtoint(strs[i]));
    end;
end;

{Fully initalizes a TocrData structure, this is LoadFont or whatever, call it first}
function InitOCR(path: string): TocrData;
var
    masks: TocrGlyphMaskArray;
    t,b,l,r,w,h,mw: integer;
    x,y: integer;
    c,i,len,size: integer;
    pos: integer;
    ascii: char;
begin
    masks:= LoadGlyphMasks(path);
    w:= 0;
    h:= 0;
    findBounds(masks,w,h);
    len:= Length(masks);
    result.width:= w;
    result.height:= h;
    size:= w * h;
    SetLength(result.pos,len,size);
    SetLength(result.pos_adj,len);
    SetLength(result.neg,len,size);
    SetLength(result.neg_adj,len);
    SetLength(result.map,len);
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
                end;
            end;
        end;
        for c:= 0 to size-1 do
            result.neg[i][c]:= 1 - result.pos[i][c];
        if pos = 0 then result.neg_adj[i]:= 1 else result.neg_adj[i]:= 1 / pos;
        if pos = 0 then result.pos_adj[i]:= 0 else result.pos_adj[i]:= 1 / pos;
        result.map[i]:= ascii;
        result.ascii[ord(ascii)].xoff:= masks[i].l;
        result.ascii[ord(ascii)].yoff:= masks[i].t;
        result.ascii[ord(ascii)].width:= masks[i].width;
        result.ascii[ord(ascii)].height:= masks[i].height;
    end;
    result.inputs:= size;
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
    for i:= 0 to inputs do
    begin
        val:= glyph[i];
        for c:= 0 to outputs do
        begin
            pos_weights[c]:= pos_weights[c] + ocrdata.pos[c][i] * val;
            neg_weights[c]:= neg_weights[c] + ocrdata.neg[c][i] * val;
        end
    end;
    max:= 0;
    for i:= 0 to outputs do
    begin
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

function ImageToNorm(src: tRGBArray; w,h: integer): TNormArray;
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

function AvgColors(color1:tRGB; weight1: integer; color2: tRGB; weight2: integer): tRGB;
begin
    result.r:= (color1.r * weight1 + color2.r * weight2) div (weight1 + weight2);
    result.g:= (color1.g * weight1 + color2.g * weight2) div (weight1 + weight2);
    result.b:= (color1.b * weight1 + color2.b * weight2) div (weight1 + weight2);
end;

procedure RGBtoXYZ(color: tRGB; out X, Y, Z: real); inline;
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

function ColortoLab(c: tRGB): tLab; inline;
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

function colorDistSqr(a,b:tRGB): integer; inline;
begin
    result:= (a.r-b.r)*(a.r-b.r)+(a.b-b.b)*(a.b-b.b)+(a.g-b.g)*(a.g-b.g);
end;

function ExtractText(colors: PRGB32;{colors: tRGBArray;} w,h: integer): TNormArray;
const
  GradientMax = 2.0;
  white:  tRGB = ( b: $FF; g: $FF; r: $FF; a: $00 );
  cyan:   tRGB = ( b: $FF; g: $FF; r: $00; a: $00 );
  yellow: tRGB = ( b: $00; g: $EF; r: $FF; a: $00 );
  red:    tRGB = ( b: $00; g: $00; r: $FF; a: $00 );
  green:  tRGB = ( b: $00; g: $FF; r: $00; a: $00 );
var
  up, left: boolean;
  len,numblobs,thisblob,lastblob,i,j,used: integer;
  blobbed,blobcount,stack: array of integer;
  labs: array of tLab;
  a,b: tLab;
  blobcolor: tRGBArray;
  newcolors: array of integer;
  c: tRGB;
  norm: TNormArray;
begin
  len:= w*h;
  SetLength(blobbed,len);
  SetLength(blobcount,len);
  SetLength(blobcolor,len);
  SetLength(stack,len);
  SetLength(labs,len);
  for i:= 0 to len-1 do
    labs[i]:= ColorToLab(tRGB(colors[i]));
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
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],tRGB(colors[i]),1);
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
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],tRGB(colors[i]),1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
    end else if up then
    begin
      thisblob:= blobbed[i-w];
      blobbed[i]:= thisblob;
      blobcolor[thisblob]:= AvgColors(blobcolor[thisblob],blobcount[thisblob],tRGB(colors[i]),1);
      blobcount[thisblob]:= blobcount[thisblob] + 1;
    end else
    begin
      blobbed[i]:= numblobs;
      blobcount[numblobs]:= 1;
      blobcolor[numblobs]:= tRGB(colors[i]);
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

function MakeTPAString(str: string): TpointArray;
var
    i,j,c,off: integer;
    bmp: array of Tbmp;
begin
    raise Exception.Create('MakeTPAString sucks ass, don''t use it.');
    c:= 0;
    off:= 0;
    SetLength(bmp,length(str));
    for i:= 0 to length(str)-1 do
    begin
        bmp[i]:= ReadBmp('/home/merlijn/Programs/mufasa/Fonts/StatChars/' + inttostr(ord(str[i+1])) + '.bmp');
        SetLength(result,c+bmp[i].width*bmp[i].height);
        for j:= 0 to bmp[i].width*bmp[i].height - 1 do
        begin
            if bmp[i].data[j].g = 255 then
            begin
                result[c].x:= j mod bmp[i].width + off;
                result[c].y:= j div bmp[i].width;
                inc(c);
            end;
        end;
        off:= off + bmp[i].width;
        SetLength(result,c);
    end;
end;

end.

