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
  Result := CurrThread.Client.MBitmaps.CreateMirroredBitmap(Bmp);
end;
