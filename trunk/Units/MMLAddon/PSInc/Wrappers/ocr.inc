function rs_GetUpText: String; extdecl;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;

function rs_GetUpTextAt(x, y : integer): string; extdecl;
begin
  result := CurrThread.Client.MOCR.GetUpTextAtEx(x,y,true);
end;

function BitmapFromText(text, font: String): integer; extdecl;
var
  bmp: TMufasaBitmap;
begin
  writeln('BitmapFromText: ' + text + ' ' + font);
  bmp := CurrThread.Client.MOCR.TextToFontBitmap(text, font);
  Result := CurrThread.Client.MBitmaps.AddBMP(bmp);
end;

function MaskFromText(text, font: String): TMask; extdecl;
begin
  Result := CurrThread.Client.MOCR.TextToMask(text,font);
end;

function TPAFromText(text, font: String;out w,h : integer): TPointArray; extdecl;
begin
  Result := CurrThread.Client.MOCR.TextToFontTPA(text, font, w, h);
end;

function GetTextAt(atX, atY, minvspacing, maxvspacing, hspacing,
                   color, tol, len: integer; font: string): string; extdecl;
begin
  result := CurrThread.Client.MOCR.GetTextAt(atx,aty,minvspacing,maxvspacing,hspacing,color,tol,len,font);
end;
