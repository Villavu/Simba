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
