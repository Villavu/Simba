{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_drawmatrix;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.vartype_matrix;

// A normalized (0..1) matrix's colors into Dest, with ColorMapType
procedure SimbaImage_DrawMatrix(const Matrix: TSingleMatrix; const ColorMapType: Integer; Dest: PColorBGRA); overload;
// An integer matrix (each cell a packed RGB colour) into Dest
procedure SimbaImage_DrawMatrix(const Matrix: TIntegerMatrix; Dest: PColorBGRA); overload;

const
  // blue->red heatmap gradient (ColorMapType = 0)
  HeatmapTable: array[0..255] of UInt32 = (
    $FF4C4CB2, $FF4C4EB3, $FF4C4FB3, $FF4C50B3, $FF4B52B4, $FF4B53B4, $FF4B55B4, $FF4A56B5, $FF4A58B5, $FF4A59B5, $FF4A5AB6, $FF495CB6, $FF495DB6, $FF495FB6, $FF4861B7, $FF4862B7,
    $FF4864B7, $FF4765B8, $FF4767B8, $FF4769B8, $FF466AB8, $FF466CB9, $FF466EB9, $FF466FB9, $FF4571BA, $FF4573BA, $FF4575BA, $FF4476BB, $FF4478BB, $FF447ABB, $FF447CBC, $FF437EBC,
    $FF4380BC, $FF4382BC, $FF4284BD, $FF4286BD, $FF4287BD, $FF4189BE, $FF418BBE, $FF418EBE, $FF4090BE, $FF4092BF, $FF4094BF, $FF4096BF, $FF3F98C0, $FF3F9AC0, $FF3F9CC0, $FF3E9EC1,
    $FF3EA1C1, $FF3EA3C1, $FF3EA5C2, $FF3DA7C2, $FF3DAAC2, $FF3DACC2, $FF3CAEC3, $FF3CB0C3, $FF3CB3C3, $FF3BB5C4, $FF3BB8C4, $FF3BBAC4, $FF3BBCC4, $FF3ABFC5, $FF3AC1C5, $FF3AC4C5,
    $FF39C6C5, $FF39C6C3, $FF39C6C1, $FF38C7BF, $FF38C7BD, $FF38C7BB, $FF38C8B9, $FF37C8B7, $FF37C8B5, $FF37C8B3, $FF36C9B1, $FF36C9AF, $FF36C9AD, $FF35CAAB, $FF35CAA9, $FF35CAA6,
    $FF34CBA4, $FF34CBA2, $FF34CBA0, $FF34CB9E, $FF33CC9B, $FF33CC99, $FF33CC97, $FF32CD94, $FF32CD92, $FF32CD90, $FF32CE8D, $FF31CE8B, $FF31CE88, $FF31CE86, $FF30CF84, $FF30CF81,
    $FF30CF7F, $FF2FD07C, $FF2FD07A, $FF2FD077, $FF2FD074, $FF2ED172, $FF2ED16F, $FF2ED16D, $FF2DD26A, $FF2DD267, $FF2DD265, $FF2CD362, $FF2CD35F, $FF2CD35C, $FF2CD45A, $FF2BD457,
    $FF2BD454, $FF2BD451, $FF2AD54E, $FF2AD54C, $FF2AD549, $FF29D646, $FF29D643, $FF29D640, $FF29D63D, $FF28D73A, $FF28D737, $FF28D734, $FF27D831, $FF27D82E, $FF27D82B, $FF26D928,
    $FF28D926, $FF2AD926, $FF2DDA26, $FF2FDA25, $FF32DA25, $FF34DA25, $FF37DB24, $FF3ADB24, $FF3CDB24, $FF3FDC23, $FF42DC23, $FF44DC23, $FF47DD22, $FF4ADD22, $FF4CDD22, $FF4FDD22,
    $FF52DE21, $FF55DE21, $FF58DE21, $FF5BDF20, $FF5DDF20, $FF60DF20, $FF63E020, $FF66E01F, $FF69E01F, $FF6CE01F, $FF6FE11E, $FF72E11E, $FF75E11E, $FF78E21D, $FF7BE21D, $FF7EE21D,
    $FF81E31C, $FF85E31C, $FF88E31C, $FF8BE31C, $FF8EE41B, $FF91E41B, $FF94E41B, $FF98E51A, $FF9BE51A, $FF9EE51A, $FFA1E61A, $FFA5E619, $FFA8E619, $FFABE619, $FFAFE718, $FFB2E718,
    $FFB6E718, $FFB9E817, $FFBDE817, $FFC0E817, $FFC3E916, $FFC7E916, $FFCAE916, $FFCEE916, $FFD2EA15, $FFD5EA15, $FFD9EA15, $FFDCEB14, $FFE0EB14, $FFE4EB14, $FFE7EC14, $FFEBEC13,
    $FFECEA13, $FFECE613, $FFEDE312, $FFEDE012, $FFEDDD12, $FFEEDA11, $FFEED711, $FFEED311, $FFEED011, $FFEFCD10, $FFEFC910, $FFEFC610, $FFF0C30F, $FFF0BF0F, $FFF0BC0F, $FFF1B90E,
    $FFF1B50E, $FFF1B20E, $FFF2AE0E, $FFF2AB0D, $FFF2A70D, $FFF2A40D, $FFF3A00C, $FFF39D0C, $FFF3990C, $FFF4960B, $FFF4920B, $FFF48F0B, $FFF58B0A, $FFF5870A, $FFF5840A, $FFF5800A,
    $FFF67C09, $FFF67909, $FFF67509, $FFF77108, $FFF76D08, $FFF76908, $FFF86608, $FFF86207, $FFF85E07, $FFF85A07, $FFF95606, $FFF95206, $FFF94E06, $FFFA4A05, $FFFA4605, $FFFA4205,
    $FFFB3E04, $FFFB3A04, $FFFB3604, $FFFB3204, $FFFC2E03, $FFFC2A03, $FFFC2603, $FFFD2202, $FFFD1E02, $FFFD1902, $FFFE1502, $FFFE1101, $FFFE0D01, $FFFE0901, $FFFF0400, $FFFF0000
  );

implementation

procedure HSLtoRGB(H,S,L: Single; out R,G,B: Byte);

  function Hue2RGB(M1, M2: Single; Hue: Single): Byte; inline;
  begin
    if (Hue < 0) then Hue += 1;
    if (Hue > 1) then Hue -= 1;

    if (6 * Hue < 1) then
      Result := Round(255 * (M1 + (M2 - M1) * 6 * Hue))
    else if (2 * Hue < 1) then
      Result := Round(255 * M2)
    else if (3 * Hue < 2) then
      Result := Round(255 * (M1 + (M2 - M1) * ((2.0 / 3.0) - Hue) * 6))
    else
      Result := Round(255 * M1);
  end;

const
  INV_360: Single = 1.0 / 360.0;
  INV_100: Single = 1.0 / 100.0;
var
  M1, M2: Single;
begin
  if (S > 0) then
  begin
    H := H * INV_360;
    S := S * INV_100;
    L := L * INV_100;
    if (L < 0.5) then
      M2 := L * (1 + S)
    else
      M2 := (L + S) - (S * L);
    M1 := 2 * L - M2;

    R := Hue2RGB(M1, M2, H + 1.0 / 3.0);
    G := Hue2RGB(M1, M2, H);
    B := Hue2RGB(M1, M2, H - 1.0 / 3.0);
  end else
  begin
    R := Round(L * 2.55);
    G := R;
    B := R;
  end;
end;

procedure SimbaImage_DrawMatrix(const Matrix: TSingleMatrix; const ColorMapType: Integer; Dest: PColorBGRA);
var
  X, Y, Width, Height: Integer;
  Value: Single;
begin
  Matrix.GetSizeMinusOne(Width, Height);

  for Y := 0 to Height do
    for X := 0 to Width do
    begin
      Value := Matrix[Y, X];

      case ColorMapType of
        // cold blue to red
        0: HSLtoRGB((1 - Value) * 240, 40 + Value * 60, 50, Dest^.R, Dest^.G, Dest^.B);
        // black -> blue -> red
        1: HSLtoRGB((1 - Value) * 240, 100, Value * 50, Dest^.R, Dest^.G, Dest^.B);
        // white -> blue -> red
        2: HSLtoRGB((1 - Value) * 240, 100, 100 - Value * 50, Dest^.R, Dest^.G, Dest^.B);
        // light (to white)
        3: HSLtoRGB(0, 0, (1 - Value) * 100, Dest^.R, Dest^.G, Dest^.B);
        // light (to white)
        4: HSLtoRGB(0, 0, Value * 100, Dest^.R, Dest^.G, Dest^.B);
        // diverging blue -> white -> red (for signed data around a midpoint)
        5: if (Value < 0.5) then HSLtoRGB(240, 100, 50  + Value * 100, Dest^.R, Dest^.G, Dest^.B)
           else                  HSLtoRGB(0,   100, 150 - Value * 100, Dest^.R, Dest^.G, Dest^.B);
        // traffic: green -> yellow -> red
        6: HSLtoRGB((1 - Value) * 120, 100, 50, Dest^.R, Dest^.G, Dest^.B);
        // rainbow: full hue sweep
        7: HSLtoRGB(Value * 300, 100, 50, Dest^.R, Dest^.G, Dest^.B);
        // Any other id is taken as a hue in degrees (0..360): the map ramps
        // black -> that hue at full saturation (Value 0.5) -> white as Value goes 0 -> 1.
        else
           HSLtoRGB(ColorMapType, 100, Value * 100, Dest^.R, Dest^.G, Dest^.B);
      end;

      Dest^.A := ALPHA_OPAQUE;
      Inc(Dest);
    end;
end;

procedure SimbaImage_DrawMatrix(const Matrix: TIntegerMatrix; Dest: PColorBGRA);
var
  X, Y, Width, Height: Integer;
begin
  Matrix.GetSize(Width, Height);

  Dec(Width);
  Dec(Height);
  for Y := 0 to Height do
    for X := 0 to Width do
    begin
      Dest^.A := ALPHA_OPAQUE;
      Dest^.R := (Matrix[Y, X] and R_MASK) shr R_BIT;
      Dest^.G := (Matrix[Y, X] and G_MASK) shr G_BIT;
      Dest^.B := (Matrix[Y, X] and B_MASK) shr B_BIT;

      Inc(Dest);
    end;
end;

end.

