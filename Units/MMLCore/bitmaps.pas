unit bitmaps;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, FPImgCanv,FPImage,IntfGraphics,graphtype,MufasaTypes ,graphics;

type

  { TMufasaBitmap }

  TMufasaBitmap = class(TObject)
  private
    w,h : integer;
  public
    FData : PRGB32;
    procedure SetSize(AWidth,AHeight : integer);
    property Width : Integer read w;
    property Height : Integer read h;
    function SaveToFile(const FileName : string) :boolean;
    procedure LoadFromFile(const FileName : string);
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
    function CreateMirroredBitmap(bitmap: Integer): Integer;
    function CreateBMPFromFile(const Path : string) : integer;
    function CreateBMPFromString(width,height : integer; Data : string) : integer;
    procedure FreeBMP( Number : integer);
    constructor Create(Owner : TObject);
    destructor Destroy;override;
  end;


implementation

uses
  Windowutil,paszlib,DCPbase64;
{ TMBitmaps }


function TMBitmaps.GetBMP(Index: integer): TMufasaBitmap;
begin
  if (Index >= 0) and (Index <= BmpsCurr) then
    if BmpArray[Index] <> nil then
      Result := BmpArray[Index];
end;

function TMBitmaps.CreateBMP(w,h : integer): Integer;
begin;
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
end;

function TMBitmaps.CreateMirroredBitmap(bitmap: Integer): Integer;
var
  w,h : integer;
  y,x : integer;
  Source,Dest : PRGB32;
begin;
  Source := Bmp[Bitmap].FData;
  w := BmpArray[Bitmap].Width;
  h := BmpArray[Bitmap].Height;
  Result := CreateBMP(w,h);
  Dest := BmpArray[Result].FData;
  for y := (h-1) downto 0 do
    for x := (w-1) downto 0 do
      Dest[y*w+x] := Source[y*w+w-1-x];
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

begin;
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
{    if Length(data) = 6 then
      FastDrawClear(Result,HexToInt(Data))
    else
      Bmps[Result].Canvas.Rectangle(0,0,Width-1,Height-1);}
  end;
end;

procedure TMBitmaps.FreeBMP(Number: integer);
begin;
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
  BmpArray[Number].Free;
end;

function TMufasaBitmap.SaveToFile(const FileName: string): boolean;
var
  rawImage : TRawImage;
  Bmp : TLazIntfImage;
begin
  ArrDataToRawImage(FData,Point(w,h),RawImage);
//  Bmp := Graphics.TBitmap.Create;
  Bmp := TLazIntfImage.Create(RawImage,true);
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
begin

  inherited Destroy;
end;


{ TMufasaBitmap }
function Min(a,b:integer) : integer;
begin;
  if a < b then
    result := a
  else
    result := b;
end;

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

constructor TMufasaBitmap.Create;
begin
  inherited Create;
  FData:= nil;
  w := 0;
  h := 0;
end;

destructor TMufasaBitmap.Destroy;
begin
  if Assigned(FData) then
    Freemem(FData,w*h*SizeOf(TRGB32));
  inherited Destroy;
end;

end.

