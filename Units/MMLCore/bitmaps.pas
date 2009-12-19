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

    Bitmaps class for the Mufasa Macro Library
}

unit bitmaps;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FPImage,IntfGraphics,graphtype,MufasaTypes,graphics;

type

  { TMufasaBitmap }
  TMufasaBitmap = class(TObject)
  private
    w,h : integer;
    TransparentColor : TRGB32;
    TransparentSet : boolean;
  public
    OnDestroy : procedure(Bitmap : TMufasaBitmap) of object;
    FData : PRGB32;
    Index : integer;
    BmpName : string; //Optional?
    procedure SetSize(AWidth,AHeight : integer);
    procedure StretchResize(AWidth,AHeight : integer);
    property Width : Integer read w;
    property Height : Integer read h;
    procedure ValidatePoint(x,y : integer);
    function SaveToFile(const FileName : string) :boolean;
    procedure LoadFromFile(const FileName : string);
    procedure FastSetPixel(x,y : integer; Color : TColor);
    procedure FastSetPixels(TPA : TPointArray; Colors : TIntegerArray);
    procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);overload;
    procedure DrawATPA(ATPA : T2DPointArray);overload;
    procedure DrawTPA(TPA : TPointArray; Color : TColor);
    function FastGetPixel(x,y : integer) : TColor;
    function FastGetPixels(TPA : TPointArray) : TIntegerArray;
    Procedure SetTransparentColor(Col : TColor);
    Function GetTransparentColor : TColor;
    property TransparentColorSet : boolean read TransparentSet;
    procedure FastDrawClear(Color : TColor);
    procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);
    procedure FastReplaceColor(OldColor, NewColor: TColor);
    procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Integer);overload;
    procedure CopyClientToBitmap(MWindow : TObject;Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);overload;
    procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
    procedure Desaturate;overload;
    procedure Desaturate(TargetBitmap : TMufasaBitmap); overload;
    procedure GreyScale(TargetBitmap : TMufasaBitmap);overload;
    procedure GreyScale;
    procedure Brightness(br: integer);overload;
    procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer); overload;
    procedure Contrast(co: Extended);overload;
    procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);overload;
    procedure Invert;
    procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);overload;
    procedure Posterize(Po : integer);overload;
    function CreateTMask : TMask;
    constructor Create;
    destructor Destroy;override;
  end;

  TMufasaBmpArray = Array of TMufasaBitmap;
  { TMBitmaps }
  TMBitmaps = class(TObject)
  protected
    Client : TObject;
    FreeSpots : Array of integer;
    BmpArray : TMufasaBmpArray;
    BmpsCurr,BmpsHigh,FreeSpotsHigh,FreeSpotsLen : integer;
  public
    function GetBMP(Index : integer) : TMufasaBitmap;
    property Bmp[Index : integer]: TMufasaBitmap read GetBMP;
    function CreateBMP(w, h: integer): Integer;
    function CopyBMP( Bitmap : integer) : Integer;
    function CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer;
    function CreateBMPFromFile(const Path : string) : integer;
    function CreateBMPFromString(width,height : integer; Data : string) : integer;overload;
    function CreateBMPFromString(BmpName : string; width,height : integer; Data : string) : integer;overload;
    procedure FreeBMP( Number : integer);
    constructor Create(Owner : TObject);
    destructor Destroy;override;
  end;


implementation

uses
  Windowutil,paszlib,DCPbase64,math,
  colour_conv,window,mufasatypesutil;

function Min(a,b:integer) : integer;
begin
  if a < b then
    result := a
  else
    result := b;
end;

{ TMBitmaps }


function TMBitmaps.GetBMP(Index: integer): TMufasaBitmap;
begin
  Result := nil;
  if (Index >= 0) and (Index <= BmpsCurr) then
    if BmpArray[Index] <> nil then
      Result := BmpArray[Index];
  if Result = nil then
    raise Exception.CreateFmt('The bitmap[%d] does not exist',[Index]);
end;

function TMBitmaps.CreateBMP(w,h : integer): Integer;
begin
  if BmpsCurr < BmpsHigh then
  begin;
    inc(BmpsCurr);
    Result := BmpsCurr;
  end else if (FreeSpotsHigh > -1) then
  begin;
    Result := FreeSpots[FreeSpotsHigh];
    dec(FreeSpotsHigh);
  end else
  begin;
    SetLength(BmpArray, BmpsHigh + 6);
    BmpsHigh := BmpsHigh + 5;
    inc(BmpsCurr);
    Result := BmpsCurr;
  end;
  BmpArray[Result] := TMufasaBitmap.Create;
  BmpArray[Result].SetSize(w,h);
  BmpArray[Result].Index:= Result;
end;

function TMBitmaps.CopyBMP(Bitmap: integer): Integer;
var
  InputBMP : TMufasaBitmap;
  OutputBMP : TMUfasaBitmap;
begin
  InputBMP := GetBMP(Bitmap);
  Result := CreateBMP(InputBmp.w,InputBMP.h);
  OutputBMP := GetBMP(Result);
  Move(InputBMP.FData[0],OutPutBMP.FData[0],InputBMP.w * InputBMP.h * SizeOf(TRGB32));
end;

function TMBitmaps.CreateMirroredBitmap(bitmap: Integer;
  MirrorStyle: TBmpMirrorStyle): Integer;
var
  w,h : integer;
  y,x : integer;
  Source,Dest : PRGB32;
begin
  Source := Bmp[Bitmap].FData;
  w := BmpArray[Bitmap].Width;
  h := BmpArray[Bitmap].Height;
  if MirrorStyle = MirrorLine then
    Result := CreateBMP(h,w)
  else
    Result := CreateBMP(w,h);
  Dest := BmpArray[Result].FData;
  case MirrorStyle of
    MirrorWidth :  for y := (h-1) downto 0 do
                     for x := (w-1) downto 0 do
                       Dest[y*w+x] := Source[y*w+w-1-x];
    MirrorHeight : for y := (h-1) downto 0 do
                    Move(Source[y*w],Dest[(h-1 - y) * w],w*SizeOf(TRGB32));
    MirrorLine :  for y := (h-1) downto 0 do
                     for x := (w-1) downto 0 do
                       Dest[x*h+y] := Source[y*w+x];

  end;
//Can be optmized, this is just proof of concept
end;

function TMBitmaps.CreateBMPFromFile(const Path: string): integer;
begin
  Result := CreateBMP(0,0);
  BmpArray[result].LoadFromFile(Path);
end;

function HexToInt(HexNum: string): LongInt;inline;
begin
   Result:=StrToInt('$' + HexNum);
end;

function TMBitmaps.CreateBMPFromString(width, height: integer; Data: string): integer;
var
  I,II: LongWord;
  DestLen : LongWord;
  Dest,Source : string;
  DestPoint, Point : PByte;


begin
  Result := CreateBMP(width,height);
  if (Data <> '') and (Length(Data) <> 6) then
  begin;
    Point := Pointer(BmpArray[Result].FData);
    if Data[1] = 'b' then
    begin;
      Source := Base64DecodeStr(Copy(Data,2,Length(Data) - 1));
      Destlen := Width * Height * 3;
      Setlength(Dest,DestLen);
      if uncompress(PChar(Dest),Destlen,pchar(Source), Length(Source)) = Z_OK then
      begin;
        DestPoint := @Dest[1];
        i := 0;
        ii := 2;
        Dec(DestLen);
        if DestLen > 2 then
        begin;
          while (ii < DestLen) do
          Begin;
            Point[i]:= DestPoint[ii+2];
            Point[i+1]:= DestPoint[ii+1];
            Point[i+2]:= DestPoint[ii];
            ii := ii + 3;
            i := i + 4;
          end;
          Point[i] := DestPoint[1];
          Point[i+1] := DestPoint[0];
          Point[i+2] := DestPoint[ii];
        end else if (Width = 1) and (Height =1 ) then
        begin;
          Point[0] := DestPoint[1];
          Point[1] := DestPoint[0];
          Point[2] := DestPoint[2];
        end;
      end;
    end else if Data[1] = 'z' then
    begin;
      Destlen := Width * Height * 3 *2;
      Setlength(Dest,DestLen);
      ii := (Length(Data) - 1) div 2;
      SetLength(Source,ii);
      for i := 1 to ii do
        Source[i] := Chr(HexToInt(Data[i * 2] + Data[i * 2+1]));
      if uncompress(PChar(Dest),Destlen,pchar(Source), ii) = Z_OK then
      begin;
        ii := 1;
        i := 0;
        while (II < DestLen) do
        begin;
          Point[i+2]:= HexToInt(Dest[ii] + Dest[ii + 1]);
          Point[i+1]:= HexToInt(Dest[ii+2] + Dest[ii + 3]);
          Point[i]:= HexToInt(Dest[ii+4] + Dest[ii + 5]);
          ii := ii + 6;
          i := i + 4;
        end;
      end;
    end else if LongWord(Length(Data)) = LongWord((Width * Height * 3 * 2)) then
    begin;
      ii := 1;
      i := 0;
      Destlen := Width * Height * 3 * 2;
      while (II < DestLen) do
      begin;
        Point[i+2]:= HexToInt(Data[ii] + Data[ii + 1]);
        Point[i+1]:= HexToInt(Data[ii+2] + Data[ii + 3]);
        Point[i]:= HexToInt(Data[ii+4] + Data[ii + 5]);
        ii := ii + 6;
        i := i + 4;
      end;
    end;
  end else
  begin;
    if Length(data) = 6 then
      BmpArray[Result].FastDrawClear(HexToInt(Data));
//    else
//      FastDrawClear(Result,clBlack);
  end;
end;

function TMBitmaps.CreateBMPFromString(BmpName: string; width, height: integer;
  Data: string): integer;
begin
  Result := Self.CreateBMPFromString(width,height,data);
  Bmp[Result].BmpName:= BmpName;

end;

procedure TMBitmaps.FreeBMP(Number: integer);
var
  ToDestroy : TMufasaBitmap;
begin
  ToDestroy := GetBMP(Number);
  if Number = BmpsCurr then
    Dec(BmpsCurr)
  else
  begin;
    inc(FreeSpotsHigh);
    if FreeSpotsHigh = FreeSpotsLen then
    begin;
      inc(FreeSpotsLen);
      SetLength(FreeSpots, FreeSpotsLen);
    end;
    FreeSpots[FreeSpotsHigh] := Number;
  end;
  //Just for testing purposes
  if ToDestroy.BmpName = '' then
    Writeln(Format('BMP[%d] has been freed.',[number]))
  else
    Writeln(Format('BMP[%s] has been freed.',[ToDestroy.BmpName]));
  FreeAndNil(ToDestroy);
end;

function TMufasaBitmap.SaveToFile(const FileName: string): boolean;
var
  rawImage : TRawImage;
  Bmp : TLazIntfImage;
begin
  ArrDataToRawImage(FData,Point(w,h),RawImage);
  result := true;
//  Bmp := Graphics.TBitmap.Create;
  try
    Bmp := TLazIntfImage.Create(RawImage,false);
    Bmp.SaveToFile(FileName);
    Bmp.Free;
  except
    result := false;
  end;
end;

procedure TMufasaBitmap.LoadFromFile(const FileName: string);
var
  LazIntf : TLazIntfImage;
  RawImageDesc : TRawImageDescription;
begin
  if FileExists(FileName) then
  begin;
    LazIntf := TLazIntfImage.Create(0,0);
    RawImageDesc.Init_BPP32_B8G8R8_BIO_TTB(LazIntf.Width,LazIntf.Height);
    LazIntf.DataDescription := RawImageDesc;
    LazIntf.LoadFromFile(FileName);
    if Assigned(FData) then
      Freemem(FData);
    Self.W := LazIntf.Width;
    Self.H := LazIntf.Height;
    FData := GetMem(Self.W*Self.H*SizeOf(TRGB32));
    Move(LazIntf.PixelData[0],FData[0],w*h*sizeOf(TRGB32));
    LazIntf.Free;
  end;
end;

function RGBToBGR(Color : TColor) : TRGB32; inline;
begin;
  Result.R := Color and $ff;
  Result.G := Color shr 8 and $ff;
  Result.B := Color shr 16 and $ff;
end;

function BGRToRGB(BGR : TRGB32) : TColor;inline;
begin;
  Result := BGR.R or BGR.g shl 8 or BGR.b shl 16;
end;

procedure TMufasaBitmap.FastSetPixel(x, y: integer; Color: TColor);
begin
  ValidatePoint(x,y);
  FData[y*w+x] := RGBToBGR(Color);
end;

procedure TMufasaBitmap.FastSetPixels(TPA: TPointArray; Colors: TIntegerArray);
var
  i,len : integer;
begin
  len := High(TPA);
  if Len <> High(colors) then
    Raise Exception.CreateFMT('TPA/Colors Length differ',[]);
  for i := 0 to len do
  begin;
    ValidatePoint(TPA[i].x,TPA[i].y);
    FData[TPA[i].y * w + TPA[i].x] := RGBToBGR(Colors[i]);
  end;
end;

procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray; Colors: TIntegerArray);
var
  lenTPA,lenATPA : integer;
  i,ii : integer;
  Color : TRGB32;
begin
  lenATPA := High(ATPA);
  if LenATPA <> High(colors) then
    Raise Exception.CreateFMT('TPA/Colors Length differ -> %d : %d',[LenATPA + 1,High(Colors) + 1]);
  for i := 0 to lenATPA do
  begin;
    lenTPA := High(ATPA[i]);
    Color := RGBToBGR(Colors[i]);
    for ii := 0 to lenTPA do
    begin;
      ValidatePoint(ATPA[i][ii].x,ATPA[i][ii].y);
      FData[ATPA[i][ii].y * w + ATPA[i][ii].x] := Color;
    end;
  end;
end;


procedure TMufasaBitmap.DrawATPA(ATPA: T2DPointArray);
var
  Colors : TIntegerArray;
  i,len : integer;
begin
  len := high(ATPA);
  SetLength(colors,len+1);
  for i := 0 to len do
    Colors[i] := Random(clwhite);
  DrawATPA(ATPA,Colors);
end;

procedure TMufasaBitmap.DrawTPA(TPA: TPointArray; Color: TColor);
begin
  DrawATPA(ConvArr([TPA]),ConvArr([Color]));
end;



function TMufasaBitmap.FastGetPixel(x, y: integer): TColor;
begin
  ValidatePoint(x,y);
  Result := BGRToRGB(FData[y*w+x]);
end;

function TMufasaBitmap.FastGetPixels(TPA: TPointArray): TIntegerArray;
var
  i,len : integer;
begin
  len := high(TPA);
  SetLength(result,len+1);
  for i := 0 to len do
  begin;
    ValidatePoint(TPA[i].x,TPA[i].y);
    Result[i] := BGRToRGB(FData[TPA[i].y*w + TPA[i].x]);
  end;
end;

procedure TMufasaBitmap.SetTransparentColor(Col: TColor);
begin
  TransparentColor:= RGBToBGR(Col);
  TransparentSet:= True;
end;

function TMufasaBitmap.GetTransparentColor: TColor;
begin
  if TransparentSet then
    Result := BGRToRGB(TransparentColor)
  else
    raise Exception.CreateFmt('Transparent color for Bitmap[%d] isn''t set',[index]);
end;

procedure TMufasaBitmap.FastDrawClear(Color: TColor);
var
  i : integer;
  Rec : TRGB32;
begin
  Rec := RGBToBGR(Color);
  if h > 0 then
  begin;
    for i := (w-1) downto 0 do
      FData[i] := Rec;
    for i := (h-1) downto 1 do
      Move(FData[0],FData[i*w],w*SizeOf(TRGB32));
  end;
end;

procedure TMufasaBitmap.FastDrawTransparent(x, y: Integer;
  TargetBitmap: TMufasaBitmap);
var
  MinW,MinH,TargetW,TargetH : Integer;
  loopx,loopy : integer;
begin
  ValidatePoint(x,y);
  TargetW := TargetBitmap.Width;
  TargetH := TargetBitmap.height;
  MinW := Min(w-1,TargetW-x-1);
  MinH := Min(h-1,TargetH-y-1);
  if TransparentSet then
  begin;
    for loopy := 0 to MinH do
      for loopx := 0 to MinW do
        if LongWord(FData[loopy * w + loopx]) <> LongWord(TransparentColor) then
          TargetBitmap.FData[(loopy + y) * TargetW + loopx + x] := FData[Loopy * w + loopx];

  end
  else
    for loopy := 0 to MinH do
      Move(FData[loopy*w],TargetBitmap.FData[(loopy+y) * TargetW + x],(MinW+1) * SizeOf(TRGB32));

end;

procedure TMufasaBitmap.FastReplaceColor(OldColor, NewColor: TColor);
var
  OldCol,NewCol : TRGB32;
  i : integer;
begin
  OldCol := RGBToBGR(OldColor);
  NewCol := RGBToBGR(NewColor);
  for i := w*h-1 downto 0 do
    if LongWord(FData[i]) = LongWord(OldCol) then
      FData[i] := NewCol;
end;

procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TObject;Resize : boolean; xs, ys, xe, ye: Integer);
var
  y : integer;
  wi,hi : integer;
  PtrRet : TRetData;
begin
  if Resize then
    Self.SetSize(xe-xs+1,ye-ys+1);
 { writeln('self: ' + inttostr(self.w) + ', ' + inttostr(self.h));
  writeln('end - start + 1: ' + inttostr(xe-xs + 1) + ', ' + inttostr(ye-ys + 1));}
  wi := Min(xe-xs + 1,Self.w);
  hi := Min(ye-ys + 1,Self.h);
 { if wi <> xe - xs + 1 then
    writeln('WAT x');
  if hi <> ye - ys + 1 then
    writeln('WAT y');  }
  PtrRet := TMWindow(MWindow).ReturnData(xs,ys,wi,hi);

  for y := 0 to (hi-1) do
    Move(PtrRet.Ptr[y * (wi + PtrRet.IncPtrWith)], FData[y * self.w],wi * SizeOf(TRGB32));
  TMWindow(MWindow).FreeReturnData;
end;

procedure TMufasaBitmap.CopyClientToBitmap(MWindow: TObject; Resize: boolean;
  x, y: integer; xs, ys, xe, ye: Integer);
var
  yy : integer;
  wi,hi : integer;
  PtrRet : TRetData;
begin
  if Resize then
    Self.SetSize(xe-xs+1 + x,ye-ys+1 + y);
  wi := Min(xe-xs + 1 + x,Self.w);
  hi := Min(ye-ys + 1 + y,Self.h);
  PtrRet := TMWindow(MWindow).ReturnData(xs,ys,wi - x,hi - y);

  for yy := 0 to (hi-1 - y) do
    Move(PtrRet.Ptr[yy * (wi - x + PtrRet.IncPtrWith)], FData[(yy + y) * self.w + x],wi * SizeOf(TRGB32));
  TMWindow(MWindow).FreeReturnData;
end;


function RotatePointEdited(p: TPoint; angle, mx, my: Extended): TPoint;

begin
  Result.X := Ceil(mx + cos(angle) * (p.x - mx) - sin(angle) * (p.y - my));
  Result.Y := Ceil(my + sin(angle) * (p.x - mx) + cos(angle) * (p.y- my));
end;

//Scar rotates unit circle-wise.. Oh, scar doesnt update the bounds, so kinda crops ur image.
procedure TMufasaBitmap.RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );
var
  NewW,NewH : integer;
  CosAngle,SinAngle : extended;
  MinX,MinY,MaxX,MaxY : integer;
  i : integer;
  x,y : integer;
  OldX,OldY : integer;
  MiddlePoint : TPoint;
  NewCorners : array[1..4] of TPoint; //(xs,ye);(xe,ye);(xe,ys);(xs,ys)
begin
  MiddlePoint := Point((w-1) div 2,(h-1) div 2);
  CosAngle := Cos(Angle);
  SinAngle := Sin(Angle);
  MinX := MaxInt;
  MinY := MaxInt;
  MaxX := 0;
  MaxY := 0;
  NewCorners[1]:= RotatePointEdited(Point(0,h-1),angle,middlepoint.x,middlepoint.y);
  NewCorners[2]:= RotatePointEdited(Point(w-1,h-1),angle,middlepoint.x,middlepoint.y);
  NewCorners[3]:= RotatePointEdited(Point(w-1,0),angle,middlepoint.x,middlepoint.y);
  NewCorners[4]:= RotatePointEdited(Point(0,0),angle,middlepoint.x,middlepoint.y);
  for i := 1 to 4 do
  begin;
    if NewCorners[i].x > MaxX then
      MaxX := NewCorners[i].x;
    if NewCorners[i].Y > MaxY then
      MaxY := NewCorners[i].y;
    if NewCorners[i].x < MinX then
      MinX := NewCorners[i].x;
    if NewCorners[i].y < MinY then
      MinY := NewCorners[i].y;
  end;
  Writeln(Format('Min: (%d,%d) Max : (%d,%d)',[MinX,MinY,MaxX,MaxY]));
  NewW := MaxX - MinX+1;
  NewH := MaxY - MinY+1;
  Writeln(format('New bounds: %d,%d',[NewW,NewH]));
  TargetBitmap.SetSize(NewW,NewH);
  for y := NewH - 1 downto 0 do
    for x := NewW - 1 downto 0 do
    begin;
      Oldx := Round(MiddlePoint.x + CosAngle * (x + MinX-MiddlePoint.x) - SinAngle * (y + MinY - MiddlePoint.y));
      Oldy := Round(MiddlePoint.y + SinAngle * (x + MinX-MiddlePoint.x) + CosAngle * (y + MinY-MiddlePoint.y));
      if not ((Oldx <0) or (Oldx >= w) or (Oldy < 0) or (Oldy >= h)) then
        TargetBitmap.FData[ y * NewW + x] := Self.FData[OldY * W + OldX];
    end;
end;

procedure TMufasaBitmap.Desaturate;
var
  I : integer;
  He,Se,Le : extended;
  Ptr : PRGB32;
begin
  Ptr := FData;
  for i := (h*w-1) downto 0 do
  begin;
    RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,Ptr^.R,Ptr^.G,Ptr^.B);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Desaturate(TargetBitmap: TMufasaBitmap);
var
  I : integer;
  He,Se,Le : extended;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    RGBToHSL(PtrOld^.R,PtrOld^.G,PtrOld^.B,He,Se,Le);
    HSLtoRGB(He,0.0,Le,PtrNew^.R,PtrNew^.G,PtrNew^.B);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale(TargetBitmap: TMufasaBitmap);
var
  I : integer;
  Lum : byte;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Lum := Round(PtrOld^.r * 0.3 + PtrOld^.g * 0.59 + PtrOld^.b * 0.11);
    PtrNew^.r := Lum;
    PtrNew^.g := Lum;
    PtrNew^.b := Lum;
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.GreyScale;
var
  I : integer;
  Lum : Byte;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Lum := Round(Ptr^.r * 0.3 + Ptr^.g * 0.59 + Ptr^.b * 0.11);
    Ptr^.r := Lum;
    Ptr^.g := Lum;
    Ptr^.b := Lum;
    inc(ptr);
  end;
end;

function BrigthnessAdjust(Col:  byte; br : integer): byte;inline;
var
  temp : integer;
begin;
  Temp := Col + Br;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  result := temp;
end;
procedure TMufasaBitmap.Brightness(br: integer);
var
  I : integer;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Ptr^.r := BrigthnessAdjust(Ptr^.r,br);
    Ptr^.g := BrigthnessAdjust(Ptr^.g,br);
    Ptr^.b := BrigthnessAdjust(Ptr^.b,br);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Brightness(TargetBitmap: TMufasaBitmap; br: integer);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := BrigthnessAdjust(PtrOld^.r,br);
    PtrNew^.g := BrigthnessAdjust(PtrOld^.g,br);
    PtrNew^.b := BrigthnessAdjust(PtrOld^.b,br);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

const
  Grey = 128;
function ContrastAdjust(Col:  byte; co : extended): byte;inline;
var
  temp : integer;
begin;
  Temp := floor((col - Grey) * co) + grey;
  if temp < 0 then
    temp := 0
  else if temp > 255 then
    temp := 255;
  result := temp;
end;

procedure TMufasaBitmap.Contrast(co: Extended);
var
  I : integer;
  Ptr: PRGB32;
begin
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
    Ptr^.r := ContrastAdjust(Ptr^.r,co);
    Ptr^.g := ContrastAdjust(Ptr^.g,co);
    Ptr^.b := ContrastAdjust(Ptr^.b,co);
    inc(ptr);
  end;
end;

procedure TMufasaBitmap.Contrast(TargetBitmap: TMufasaBitmap; co: Extended);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := ContrastAdjust(PtrOld^.r,co);
    PtrNew^.g := ContrastAdjust(PtrOld^.g,co);
    PtrNew^.b := ContrastAdjust(PtrOld^.b,co);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Invert;
var
  i : integer;
begin
  for i := (h*w-1) downto 0 do
  begin;
    Self.FData[i].r := not Self.FData[i].r;
    Self.FData[i].g := not Self.FData[i].g;
    Self.Fdata[i].b := not Self.FData[i].b;
  end;
end;

procedure TMufasaBitmap.Posterize(TargetBitmap: TMufasaBitmap; Po: integer);
var
  I : integer;
  PtrOld,PtrNew : PRGB32;
begin
  if not InRange(Po,1,255) then
    Raise exception.CreateFmt('Posterize Po(%d) out of range[1,255]',[Po]);
  TargetBitmap.SetSize(w,h);
  PtrOld := Self.FData;
  PtrNew := TargetBitmap.FData;
  for i := (h*w-1) downto 0 do
  begin;
    PtrNew^.r := min(Round(PtrOld^.r / po) * Po, 255);
    PtrNew^.g := min(Round(PtrOld^.g / po) * Po, 255);
    PtrNew^.b := min(Round(PtrOld^.b / po) * Po, 255);
    inc(ptrOld);
    inc(PtrNew);
  end;
end;

procedure TMufasaBitmap.Posterize(Po: integer);
var
  I : integer;
  Ptr: PRGB32;
  {a:integer; }
begin
  if not InRange(Po,1,255) then
    Raise exception.CreateFmt('Posterize Po(%d) out of range[1,255]',[Po]);
  Ptr := Self.FData;
  for i := (h*w-1) downto 0 do
  begin;
   { a := round(ptr^.r / po);
    a := a * po;
    ptr^.r := min(a,255);
    a := round(ptr^.g / po);
    a := a * po;
    ptr^.g := min(a,255);
    a := round(ptr^.b / po);
    a := a * po;
    ptr^.b := min(a,255);      }
    ptr^.r := min(Round(ptr^.r / po) * Po, 255);
    ptr^.g := min(Round(ptr^.g / po) * Po, 255);
    ptr^.b := min(Round(ptr^.b / po) * Po, 255);
    inc(ptr);
  end;
end;

function TMufasaBitmap.CreateTMask: TMask;
var
  x,y : integer;
  dX,dY : integer;
begin
  Result.BlackHi:= -1;
  Result.WhiteHi:= -1;
  Result.W := Self.Width;
  Result.H := Self.Height;
  SetLength(result.Black,w*h);
  SetLength(result.White,w*h);
  dX := w-1;
  dY := h-1;
  //Search it like | | | | | instead of horizontal -> for X loop first.
  for x := 0 to dX do
    for y := 0 to dY do
    //Check for non-white/black pixels? Not for now atleast.
      if FData[y*w+x].r = 255 then
      begin;
        inc(Result.WhiteHi);
        Result.White[Result.WhiteHi].x := x;
        Result.White[Result.WhiteHi].y := y;
      end else
      begin;
        inc(Result.BlackHi);
        Result.Black[Result.BlackHi].x := x;
        Result.Black[Result.BlackHi].y := y;
      end;
  SetLength(result.Black,Result.BlackHi+1);
  SetLength(result.White,Result.WhiteHi+1);
end;



constructor TMBitmaps.Create(Owner: TObject);
begin
  inherited Create;
  SetLength(BmpArray,50);
  SetLength(FreeSpots, 50);
  FreeSpotsLen := 50;
  BmpsHigh := 49;
  BmpsCurr := -1;
  FreeSpotsHigh := -1;
  Self.Client := Owner;
end;

destructor TMBitmaps.Destroy;
var
  I : integer;
begin
  for i := 0 to BmpsCurr do
    if BmpArray[i] <> nil then
    begin;
      if BmpArray[i].BmpName = '' then
        Writeln(Format('BMP[%d] has not been freed in the script, freeing it now.',[i]))
      else
        Writeln(Format('BMP[%s] has not been freed in the script, freeing it now.',[BmpArray[i].BmpName]));
      FreeAndNil(BmpArray[i]);
    end;
  SetLength(BmpArray,0);
  SetLength(FreeSpots,0);
  inherited Destroy;
end;


{ TMufasaBitmap }
procedure TMufasaBitmap.SetSize(Awidth, Aheight: integer);
var
  NewData : PRGB32;
  i,minw,minh : integer;
begin
  if (AWidth <> w) or (AHeight <> h) then
  begin;
    if AWidth*AHeight <> 0 then
    begin;
      NewData := GetMem(AWidth * AHeight * SizeOf(TRGB32));
      FillDWord(NewData[0],AWidth*AHeight,0);
    end
    else
      NewData := nil;
    if Assigned(FData) and Assigned(NewData) and (w*H <> 0) then
    begin;
      minw := Min(AWidth,w);
      minh := Min(AHeight,h);
      for i := 0 to minh - 1 do
        Move(FData[i*w],Newdata[i*AWidth],minw * SizeOf(TRGB32));
    end;
    if Assigned(FData) then
      FreeMem(FData);
    FData := NewData;
    w := AWidth;
    h := AHeight;
  end;
end;

procedure TMufasaBitmap.StretchResize(AWidth, AHeight: integer);
var
  NewData : PRGB32;
  x,y : integer;
begin
  if (AWidth <> w) or (AHeight <> h) then
  begin;
    if AWidth*AHeight <> 0 then
    begin;
      NewData := GetMem(AWidth * AHeight * SizeOf(TRGB32));
      FillDWord(NewData[0],AWidth*AHeight,0);
    end
    else
      NewData := nil;
    if Assigned(FData) and Assigned(NewData) and (w*H <> 0) then
    begin;
      for y := 0 to AHeight - 1 do
        for x := 0 to AWidth -1 do
          NewData[y*AWidth + x] := FData[((y * h)div aheight) * W+ (x * W) div awidth];
    end;
    if Assigned(FData) then
      FreeMem(FData);
    FData := NewData;
    w := AWidth;
    h := AHeight;
  end;
end;

procedure TMufasaBitmap.ValidatePoint(x, y: integer);
begin
  if (x <0) or (x >= w) or (y < 0) or (y >= h) then
    raise Exception.CreateFmt('You are accessing an invalid point, (%d,%d) at bitmap[%d]',[x,y,index]);
end;

constructor TMufasaBitmap.Create;
begin
  inherited Create;
  BmpName:= '';
  FData:= nil;
  TransparentSet:= False;
  w := 0;
  h := 0;
end;

destructor TMufasaBitmap.Destroy;
begin
  if Assigned(OnDestroy) then
    OnDestroy(Self);
  if Assigned(FData) then
    Freemem(FData);
  inherited Destroy;
end;

end.

