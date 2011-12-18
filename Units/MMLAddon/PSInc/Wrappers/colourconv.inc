procedure ps_ColorToRGB(Color: integer; var r, g, b: Integer);extdecl;
var
  RR,GG,BB : byte;
begin
  colour_conv.ColorToRGB(color,rr,gg,bb);
  r := rr;
  g := gg;
  b := bb;
end;

function ps_RGBtoColor(r, g, b: Integer): TColor;extdecl;
begin
  result := RGBtoColor(r,g,b);
end;

procedure ps_ColorToHSL(Color: Integer; var h, s, l: Extended); extdecl;
begin
  ColorToHSL(color,h,s,l);
end;

function ps_HSLToColor(H, S, L: Extended): TColor; extdecl;
begin
  result := HSLToColor(h,s,l);
end;

procedure ps_ColorToXYZ(Color: Integer; var x, y, z: Extended); extdecl;
begin
  ColorToXYZ(Color,x,y,z)
end;

function ps_XYZToColor(X, Y, Z: Extended): TColor; extdecl;
begin
  result := XYZToColor(x,y,z);
end;

procedure ps_RGBToHSL(R, G, B: Integer; var h, s, l: Extended);extdecl;
begin
  RGBToHSL(r,g,b,h,s,l);
end;

procedure ps_HSLtoRGB(H, S, L: extended; var R, G ,B: Integer);extdecl;
var
  RR,GG,BB : byte;
begin
  HSLtoRGB(h,s,l,rr,gg,bb);
  r := rr;
  g := gg;
  b := bb;
end;

procedure ps_RGBToXYZ(R, G, B: Integer;var x, y ,z: Extended);extdecl;
begin
  RGBToXYZ(r,g,b,x,y,z);
end;

procedure ps_XYZToRGB(X, Y, Z: Extended; var R, G, B: Integer);extdecl;
var
  RR,GG,BB : byte;
begin;
  XYZToRGB(x,y,z,rr,gg,bb);
  r := rr;
  g := gg;
  b := bb;
end;

procedure ps_XYZToHSL(X, Y, Z: Extended; out H, S, L: Extended);
begin
  XYZToHSL(X, Y, Z, H, S, L);
end;

procedure ps_HSLToXYZ(H, S, L: Extended; out X, Y, Z: Extended);
begin
  HSLToXYZ(H, S, L, X, Y, Z);
end;

procedure ps_XYZtoCIELab(X, Y, Z: Extended; out L, a, b: Extended);
begin
  XYZtoCIELab(X, Y, Z, L, a, b);
end;

procedure ps_CIELabtoXYZ(L, a, b: Extended; out X, Y, Z: Extended);
begin
  CIELabToXYZ(L, a, b, X, Y, Z);
end;

procedure ps_CIELabToRGB(L, a, b: Extended; out rr, gg, bb: integer);
begin
  CIELabToRGB(L, a, b, rr, gg, bb);
end;

procedure ps_RGBToCIELab(rr, gg, bb: integer; out L, a, b: Extended);
begin
  RGBToCIELab(rr, gg, bb, L, a, b);
end;

function ps_CIELabToColor(L, a, b: Extended): TColor;
begin
  Result := CIELabToColor(L, a, b);
end;

procedure ps_ColorToCIELab(Color: integer; out L, a, b: Extended);
begin
  ColorToCIELab(Color, L, a, b);
end;

procedure ps_CIELabToHSL(L, a, b: Extended; out HH, SS, LL: Extended);
begin
  CIELabToHSL(L, a, b, HH, SS, LL);
end;

procedure ps_HSLToCIELab(HH, SS, LL: Extended; out L, a, b: Extended);
begin
  HSLToCIELab(HH, SS, LL, L, a, b);
end;
