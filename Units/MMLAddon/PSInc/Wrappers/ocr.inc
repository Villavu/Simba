function ps_rs_GetUpText: String; extdecl;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;

function ps_rs_GetUpTextAt(x, y : integer): string; extdecl;
begin
  result := CurrThread.Client.MOCR.GetUpTextAtEx(x,y,true);
end;

function ps_BitmapFromText(const text, font: String): integer; extdecl;
var
  bmp: TMufasaBitmap;
begin
  bmp := CurrThread.Client.MOCR.TextToFontBitmap(text, font);
  Result := CurrThread.Client.MBitmaps.AddBMP(bmp);
end;

function ps_MaskFromText(const text, font: String): TMask; extdecl;
begin
  Result := CurrThread.Client.MOCR.TextToMask(text,font);
end;

procedure ps_TPAFromTextWrap(const text, font: String;out w,h : integer;out TPA : TPointArray); extdecl;
begin
  TPA := CurrThread.Client.MOCR.TextToFontTPA(text, font, w, h);
end;

function ps_TPAFromText(const text, font: String;out w,h : integer): TPointArray; extdecl;
begin
  Result := CurrThread.Client.MOCR.TextToFontTPA(text, font, w, h);
end;

function ps_GetTextAt(const atX, atY, minvspacing, maxvspacing, hspacing,
                   color, tol, len: integer;const font: string): string; extdecl;
begin
  result := CurrThread.Client.MOCR.GetTextAt(atx,aty,minvspacing,maxvspacing,hspacing,color,tol,len,font);
end;

function ps_LoadSystemFont(const SysFont : TFont; const FontName : string) : boolean;extdecl;
begin
  result := CurrThread.Client.MOCR.Fonts.LoadSystemFont(SysFont,FontName);
end;
