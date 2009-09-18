unit bitmaps;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FPImgCanv,FPImage,IntfGraphics,graphtype,MufasaTypes,window,graphics;

type

  { TMufasaBitmap }

  TMufasaBitmap = class(TObject)
  private
    w,h : integer;
    TransparentColor : TRGB32;
    TransparentSet : boolean;
  public
    FData : PRGB32;
    Index : integer;
    procedure SetSize(AWidth,AHeight : integer);
    property Width : Integer read w;
    property Height : Integer read h;
    procedure ValidatePoint(x,y : integer);
    function SaveToFile(const FileName : string) :boolean;
    procedure LoadFromFile(const FileName : string);
    procedure FastSetPixel(x,y : integer; Color : TColor);
    procedure FastSetPixels(TPA : TPointArray; Colors : TIntegerArray);
    function FastGetPixel(x,y : integer) : TColor;
    function FastGetPixels(TPA : TPointArray) : TIntegerArray;
    Procedure SetTransparentColor(Col : TColor);
    Function GetTransparentColor : TColor;
    procedure FastDrawClear(Color : TColor);
    procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);
    procedure FastReplaceColor(OldColor, NewColor: TColor);
    procedure CopyClientToBitmap(MWindow : TMWindow; xs, ys, xe, ye: Integer);
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
    function CreateMirroredBitmap(bitmap: Integer; MirrorStyle : TBmpMirrorStyle): Integer;
    function CreateBMPFromFile(const Path : string) : integer;
    function CreateBMPFromString(width,height : integer; Data : string) : integer;
    procedure FreeBMP( Number : integer);
    constructor Create(Owner : TObject);
    destructor Destroy;override;
  end;


implementation

uses
  Windowutil,paszlib,DCPbase64;

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
  I,II,x,y: LongWord;
  DestLen : LongWord;
  Dest,Source : string;
  DestPoint, Point : PByte;
  LazIntf  : TLazIntfImage;

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
    end else if LongWord(Length(Data)) = (Width * Height * 3 * 2) then
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
  FreeAndNil(ToDestroy);
end;

function TMufasaBitmap.SaveToFile(const FileName: string): boolean;
var
  rawImage : TRawImage;
  Bmp : TLazIntfImage;
begin
  ArrDataToRawImage(FData,Point(w,h),RawImage);
//  Bmp := Graphics.TBitmap.Create;
  Bmp := TLazIntfImage.Create(RawImage,false);
  Bmp.SaveToFile(FileName);
  Bmp.Free;
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

procedure TMufasaBitmap.CopyClientToBitmap(MWindow : TMWindow; xs, ys, xe, ye: Integer);
var
  wi,hi,y : integer;
  PtrRet : TRetData;
  Rows : integer;
begin
  Self.ValidatePoint(xs,ys);
  Self.ValidatePoint(xe,ye);
  wi := xe-xs + 1;
  hi := ye-ys + 1;
  PtrRet := MWindow.ReturnData(xs,ys,wi,hi);
  for y := 0 to (hi-1) do
    Move(PtrRet.Ptr[y * (wi + PtrRet.IncPtrWith)], FData[y * self.w],wi * SizeOf(TRGB32));
  MWindow.FreeReturnData;
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
      FreeAndNil(BmpArray[i]);
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

procedure TMufasaBitmap.ValidatePoint(x, y: integer);
begin
  if (x <0) or (x >= w) or (y < 0) or (y >= h) then
    raise Exception.CreateFmt('You are accesing an invalid point, (%d,%d) at bitmap[%d]',[x,y,index]);
end;

constructor TMufasaBitmap.Create;
begin
  inherited Create;
  FData:= nil;
  TransparentSet:= False;
  w := 0;
  h := 0;
end;

destructor TMufasaBitmap.Destroy;
begin
  if Assigned(FData) then
    Freemem(FData);
  inherited Destroy;
end;

end.

