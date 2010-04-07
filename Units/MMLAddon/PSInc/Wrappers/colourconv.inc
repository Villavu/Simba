procedure ps_ColorToRGB(Color: integer; var r, g, b: Byte);extdecl;
begin
  colour_conv.ColorToRGB(color,r,g,b);
end;

function ps_RGBtoColor(r, g, b: Byte): TColor;extdecl;
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

procedure ps_RGBToHSL(R, G, B: Byte; var h, s, l: Extended);extdecl;
begin
  RGBToHSL(r,g,b,h,s,l);
end;

procedure ps_HSLtoRGB(H, S, L: extended; var R, G ,B: Byte);extdecl;
begin
  HSLtoRGB(h,s,l,r,g,b);
end;

procedure ps_RGBToXYZ(R, G, B: Byte;var x, y ,z: Extended);extdecl;
begin
  RGBToXYZ(r,g,b,x,y,z);
end;
procedure ps_XYZToRGB(X, Y, Z: Extended; var R, G, B: Byte);extdecl;
begin
  XYZToRGB(x,y,z,r,g,b);
end;
