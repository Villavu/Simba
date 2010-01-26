function rs_GetUpText: String; ps_decl;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;

function BitmapFromText(text, font: String): integer; ps_decl;
var
  bmp: TMufasaBitmap;
begin
  writeln('BitmapFromText: ' + text + ' ' + font);
  bmp := CurrThread.Client.MOCR.TextToFontBitmap(text, font);
  Result := CurrThread.Client.MBitmaps.AddBMP(bmp);
end;

function MaskFromText(text, font: String): TMask; ps_decl;
begin
  Result := CurrThread.Client.MOCR.TextToMask(text,font);
end;

function TPAFromText(text, font: String;out w,h : integer): TPointArray; ps_decl;
begin
  Result := CurrThread.Client.MOCR.TextToFontTPA(text, font, w, h);
end;
