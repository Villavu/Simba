function rs_GetUpText: String;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;

function BitmapFromTextFont(text, font: String): integer;
var
  bmp: TMufasaBitmap;
begin
  writeln('BitmapFromText: ' + text + ' ' + font);
  bmp := CurrThread.Client.MOCR.TextToFontBitmap(text, font);
  Result := CurrThread.Client.MBitmaps.AddBMP(bmp);
end;

function TPAFromTextFont(text, font: String): TPointArray;

var
  w,h : integer;
begin
  Result := CurrThread.Client.MOCR.TextToFontTPA(text, font, w, h);
end;
