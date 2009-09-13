function CreateBitmap(w,h : integer):integer;
begin
  result := CurrThread.Client.MBitmaps.CreateBMP(w,h);
end;

procedure FreeBitmap(Number : integer);
begin
  CurrThread.Client.MBitmaps.FreeBMP(Number);
end;

procedure SaveBitmap(Bmp : integer; path : string);
begin;
  CurrThread.Client.MBitmaps.Bmp[Bmp].SaveToFile(Path);
end;

function BitmapFromString(Width,height : integer; Data : string) : integer;
begin;
  Result := CurrThread.Client.MBitmaps.CreateBMPFromString(Width,Height,Data);
end;

function LoadBitmap(Path : String) : integer;
begin;
  Result := CurrThread.Client.MBitmaps.CreateBMPFromFile(Path);
end;

procedure SetBitmapSize(Bmp,NewW,NewH : integer);
begin;
  if (NewW>=0) and (NewH >=0) then
    CurrThread.Client.MBitmaps.Bmp[Bmp].SetSize(NewW,NewH);
end;

procedure GetBitmapSize(Bmp : integer; var BmpW,BmpH : integer);
begin;
  With CurrThread.Client.MBitmaps.Bmp[bmp] do
  begin;
    BmpW := width;
    BmpH := Height;
  end;
end;

function CreateMirroredBitmap(Bmp : integer) : integer;
begin;
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp, MirrorWidth);
end;

function CreateMirroredBitmapEx(Bmp : integer; MirrorStyle : TBmpMirrorStyle) : integer;
begin;
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp,MirrorStyle);
end;

function FastGetPixel(bmp,x,y : integer) : LongWord;
begin;
  Result := CurrThread.Client.MBitmaps.Bmp[Bmp].FastGetPixel(x,y);
end;

function FastGetPixels(bmp : integer; TPA : TPointArray) : TIntegerArray;
begin;
  result := CurrThread.Client.MBitmaps.Bmp[Bmp].FastGetPixels(TPA);
end;

procedure FastSetPixel(Bmp,x,y : integer; Color : TColor);
begin
  CurrThread.Client.MBitmaps.Bmp[bmp].FastSetPixel(x,y,color);
end;

procedure FastSetPixels(Bmp : integer; TPA : TPointArray; Colors : TIntegerArray);
begin;
  CurrThread.Client.MBitmaps.Bmp[Bmp].FastSetPixels(TPA,Colors);
end;

procedure FastDrawClear(bmp : integer; Color : TColor);
begin;
  CurrThread.Client.MBitmaps.Bmp[bmp].FastDrawClear(Color);
end;

procedure FastDrawTransparent(x, y: Integer; SourceBitmap, TargetBitmap: Integer);
begin;
  CurrThread.Client.MBitmaps.Bmp[SourceBitmap].FastDrawTransparent(x,y,CurrThread.Client.MBitmaps.Bmp[TargetBitmap]);
end;

procedure SetTransparentColor(Bmp : integer; Color : TColor);
begin
  CurrThread.Client.MBitmaps.Bmp[Bmp].SetTransparentColor(Color);
end;

function GetTransparentColor(Bmp : integer) : TColor;
begin;
  Result := CurrThread.Client.MBitmaps.Bmp[bmp].GetTransparentColor;
end;

procedure FastReplaceColor(bmp: Integer; OldColor, NewColor: TColor);
begin
  CurrThread.Client.MBitmaps.Bmp[Bmp].FastReplaceColor(OldColor,NewColor);
end;
