{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
}
unit simba.imagebox_drawers;

{$DEFINE SIMBA_MAX_OPTIMIZATION}
{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.colormath;

const
  HEATMAP_TABLE: array[0..837] of TColor = (
    $B24C4C, $B34D4C, $B34E4C, $B34F4C, $B3504C, $B3514C, $B4514C, $B4514B, $B4524B, $B4534B, $B4544B, $B4554B, $B4564B, $B4564A, $B5564A, $B5574A, $B5584A, $B5594A, $B55A4A, $B65A4A, $B65A49, $B65B49, $B65C49, $B65D49, $B65E49, $B65F49, $B66049, $B66048, $B76048, $B76148, $B76248, $B76348, $B76448, $B76548, $B86548, $B86547, $B86647, $B86747, $B86847, $B86947, $B86A47, $B86A46, $B96A46, $B96B46, $B96C46, $B96D46, $B96E46, $B96F46, $B97046, $BA7046, $BA7045, $BA7145, $BA7245, $BA7345, $BA7445, $BA7545, $BA7645, $BA7644, $BB7644, $BB7744, $BB7844, $BB7944, $BB7A44, $BB7B44, $BB7C44, $BC7C44, $BC7C43, $BC7D43, $BC7E43, $BC7F43, $BC8043, $BC8143, $BC8243, $BC8242, $BD8242, $BD8342, $BD8442, $BD8542, $BD8642, $BD8742, $BD8842, $BD8942, $BE8942, $BE8941, $BE8A41, $BE8B41, $BE8C41,
    $BE8D41, $BE8E41, $BE8F41, $BE9041, $BE9040, $BF9040, $BF9140, $BF9240, $BF9340, $BF9440, $BF9540, $BF9640, $BF9740, $C09740, $C0973F, $C0983F, $C0993F, $C09A3F, $C09B3F, $C09C3F, $C09D3F, $C09E3F, $C19E3E, $C19F3E, $C1A03E, $C1A13E, $C1A23E, $C1A33E, $C1A43E, $C1A53E, $C2A53E, $C2A53D, $C2A63D, $C2A73D, $C2A83D, $C2A93D, $C2AA3D, $C2AB3D, $C2AC3D, $C2AD3D, $C3AD3C, $C3AE3C, $C3AF3C, $C3B03C, $C3B13C, $C3B23C, $C3B33C, $C3B43C, $C4B43C, $C4B43B, $C4B53B, $C4B63B, $C4B73B, $C4B83B, $C4B93B, $C4BA3B, $C4BB3B, $C4BC3B, $C5BC3A, $C5BD3A, $C5BE3A, $C5BF3A, $C5C03A, $C5C13A, $C5C23A, $C5C33A, $C5C43A, $C5C53A, $C6C53A, $C6C539, $C6C639, $C5C639, $C4C639, $C3C639, $C2C639, $C1C639, $C0C639, $C0C738, $BFC738, $BEC738, $BDC738, $BCC738, $BBC738, $BAC738, $B9C738, $B9C838, $B9C837,
    $B8C837, $B7C837, $B6C837, $B5C837, $B4C837, $B3C837, $B3C936, $B2C936, $B1C936, $B0C936, $AFC936, $AEC936, $ADC936, $ACC936, $ACCA36, $ACCA35, $ABCA35, $AACA35, $A9CA35, $A8CA35, $A7CA35, $A6CA35, $A5CA35, $A4CA35, $A4CB34, $A3CB34, $A2CB34, $A1CB34, $A0CB34, $9FCB34, $9ECB34, $9DCB34, $9DCC34, $9DCC33, $9CCC33, $9BCC33, $9ACC33, $99CC33, $98CC33, $97CC33, $96CC33, $95CC33, $95CD32, $94CD32, $93CD32, $92CD32, $91CD32, $90CD32, $8FCD32, $8ECD32, $8DCD32, $8DCE32, $8DCE31, $8CCE31, $8BCE31, $8ACE31, $89CE31, $88CE31, $87CE31, $86CE31, $85CE31, $85CF30, $84CF30, $83CF30, $82CF30, $81CF30, $80CF30, $7FCF30, $7ECF30, $7DCF30, $7DD030, $7DD02F, $7CD02F, $7BD02F, $7AD02F, $79D02F, $78D02F, $77D02F, $76D02F, $75D02F, $74D02F, $74D12E, $73D12E, $72D12E, $71D12E, $70D12E, $6FD12E,
    $6ED12E, $6DD12E, $6CD12E, $6CD22E, $6CD22D, $6BD22D, $6AD22D, $69D22D, $68D22D, $67D22D, $66D22D, $65D22D, $64D22D, $63D22D, $63D32C, $62D32C, $61D32C, $60D32C, $5FD32C, $5ED32C, $5DD32C, $5CD32C, $5BD32C, $5AD32C, $5AD42C, $5AD42B, $59D42B, $58D42B, $57D42B, $56D42B, $55D42B, $54D42B, $53D42B, $52D42B, $51D42B, $50D42B, $50D52A, $4FD52A, $4ED52A, $4DD52A, $4CD52A, $4BD52A, $4AD52A, $49D52A, $48D52A, $47D52A, $47D62A, $47D629, $46D629, $45D629, $44D629, $43D629, $42D629, $41D629, $40D629, $3FD629, $3ED629, $3DD629, $3DD728, $3CD728, $3BD728, $3AD728, $39D728, $38D728, $37D728, $36D728, $35D728, $34D728, $33D728, $33D828, $33D827, $32D827, $31D827, $30D827, $2FD827, $2ED827, $2DD827, $2CD827, $2BD827, $2AD827, $29D827, $29D926, $28D926, $27D926, $26D926, $26D927, $26D928,
    $26D929, $26D92A, $26D92B, $26D92C, $26D92D, $26DA2D, $25DA2D, $25DA2E, $25DA2F, $25DA30, $25DA31, $25DA32, $25DA33, $25DA34, $25DA35, $24DB35, $24DB36, $24DB37, $24DB38, $24DB39, $24DB3A, $24DB3B, $24DB3C, $24DB3D, $24DB3E, $24DC3E, $23DC3E, $23DC3F, $23DC40, $23DC41, $23DC42, $23DC43, $23DC44, $23DC45, $23DC46, $23DC47, $22DD47, $22DD48, $22DD49, $22DD4A, $22DD4B, $22DD4C, $22DD4D, $22DD4E, $22DD4F, $22DD50, $22DE50, $21DE50, $21DE51, $21DE52, $21DE53, $21DE54, $21DE55, $21DE56, $21DE57, $21DE58, $21DE59, $21DE5A, $20DF5A, $20DF5B, $20DF5C, $20DF5D, $20DF5E, $20DF5F, $20DF60, $20DF61, $20DF62, $20DF63, $20E063, $1FE063, $1FE064, $1FE065, $1FE066, $1FE067, $1FE068, $1FE069, $1FE06A, $1FE06B, $1FE06C, $1FE06D, $1EE16D, $1EE16E, $1EE16F, $1EE170, $1EE171, $1EE172, $1EE173,
    $1EE174, $1EE175, $1EE176, $1EE177, $1EE277, $1DE277, $1DE278, $1DE279, $1DE27A, $1DE27B, $1DE27C, $1DE27D, $1DE27E, $1DE27F, $1DE280, $1DE281, $1CE381, $1CE382, $1CE383, $1CE384, $1CE385, $1CE386, $1CE387, $1CE388, $1CE389, $1CE38A, $1CE38B, $1CE38C, $1CE48C, $1BE48C, $1BE48D, $1BE48E, $1BE48F, $1BE490, $1BE491, $1BE492, $1BE493, $1BE494, $1BE495, $1BE496, $1BE497, $1AE597, $1AE598, $1AE599, $1AE59A, $1AE59B, $1AE59C, $1AE59D, $1AE59E, $1AE59F, $1AE5A0, $1AE5A1, $1AE6A1, $1AE6A2, $19E6A2, $19E6A3, $19E6A4, $19E6A5, $19E6A6, $19E6A7, $19E6A8, $19E6A9, $19E6AA, $19E6AB, $19E6AC, $19E6AD, $18E7AD, $18E7AE, $18E7AF, $18E7B0, $18E7B1, $18E7B2, $18E7B3, $18E7B4, $18E7B5, $18E7B6, $18E7B7, $18E7B8, $18E8B8, $17E8B8, $17E8B9, $17E8BA, $17E8BB, $17E8BC, $17E8BD, $17E8BE, $17E8BF,
    $17E8C0, $17E8C1, $17E8C2, $17E8C3, $16E9C3, $16E9C4, $16E9C5, $16E9C6, $16E9C7, $16E9C8, $16E9C9, $16E9CA, $16E9CB, $16E9CC, $16E9CD, $16E9CE, $16E9CF, $16EACF, $15EACF, $15EAD0, $15EAD1, $15EAD2, $15EAD3, $15EAD4, $15EAD5, $15EAD6, $15EAD7, $15EAD8, $15EAD9, $15EADA, $15EADB, $14EBDB, $14EBDC, $14EBDD, $14EBDE, $14EBDF, $14EBE0, $14EBE1, $14EBE2, $14EBE3, $14EBE4, $14EBE5, $14EBE6, $14EBE7, $14ECE7, $13ECE7, $13ECE8, $13ECE9, $13ECEA, $13ECEB, $13ECEC, $13EBEC, $13EAEC, $13E9EC, $13E8EC, $13E7EC, $13E6EC, $13E5EC, $12E5ED, $12E4ED, $12E3ED, $12E2ED, $12E1ED, $12E0ED, $12DFED, $12DEED, $12DDED, $12DCED, $12DBED, $12DBEE, $11DBEE, $11DAEE, $11D9EE, $11D8EE, $11D7EE, $11D6EE, $11D5EE, $11D4EE, $11D3EE, $11D2EE, $11D1EE, $11D0EE, $10D0EF, $10CFEF, $10CEEF, $10CDEF, $10CCEF,
    $10CBEF, $10CAEF, $10C9EF, $10C8EF, $10C7EF, $10C6EF, $10C5EF, $10C5F0, $0FC5F0, $0FC4F0, $0FC3F0, $0FC2F0, $0FC1F0, $0FC0F0, $0FBFF0, $0FBEF0, $0FBDF0, $0FBCF0, $0FBBF0, $0FBAF0, $0EBAF1, $0EB9F1, $0EB8F1, $0EB7F1, $0EB6F1, $0EB5F1, $0EB4F1, $0EB3F1, $0EB2F1, $0EB1F1, $0EB0F1, $0EAFF1, $0EAEF1, $0EAEF2, $0DAEF2, $0DADF2, $0DACF2, $0DABF2, $0DAAF2, $0DA9F2, $0DA8F2, $0DA7F2, $0DA6F2, $0DA5F2, $0DA4F2, $0DA3F2, $0CA3F3, $0CA2F3, $0CA1F3, $0CA0F3, $0C9FF3, $0C9EF3, $0C9DF3, $0C9CF3, $0C9BF3, $0C9AF3, $0C99F3, $0C98F3, $0C97F3, $0C97F4, $0B97F4, $0B96F4, $0B95F4, $0B94F4, $0B93F4, $0B92F4, $0B91F4, $0B90F4, $0B8FF4, $0B8EF4, $0B8DF4, $0B8CF4, $0B8BF4, $0A8BF5, $0A8AF5, $0A89F5, $0A88F5, $0A87F5, $0A86F5, $0A85F5, $0A84F5, $0A83F5, $0A82F5, $0A81F5, $0A80F5, $0A7FF5, $0A7FF6,
    $097FF6, $097EF6, $097DF6, $097CF6, $097BF6, $097AF6, $0979F6, $0978F6, $0977F6, $0976F6, $0975F6, $0974F6, $0973F6, $0972F6, $0872F7, $0871F7, $0870F7, $086FF7, $086EF7, $086DF7, $086CF7, $086BF7, $086AF7, $0869F7, $0868F7, $0867F7, $0866F7, $0866F8, $0766F8, $0765F8, $0764F8, $0763F8, $0762F8, $0761F8, $0760F8, $075FF8, $075EF8, $075DF8, $075CF8, $075BF8, $075AF8, $0759F8, $0659F9, $0658F9, $0657F9, $0656F9, $0655F9, $0654F9, $0653F9, $0652F9, $0651F9, $0650F9, $064FF9, $064EF9, $064DF9, $064CF9, $064CFA, $054CFA, $054BFA, $054AFA, $0549FA, $0548FA, $0547FA, $0546FA, $0545FA, $0544FA, $0543FA, $0542FA, $0541FA, $0540FA, $053FFA, $053EFA, $043EFB, $043DFB, $043CFB, $043BFB, $043AFB, $0439FB, $0438FB, $0437FB, $0436FB, $0435FB, $0434FB, $0433FB, $0432FB, $0431FB, $0431FC,
    $0331FC, $0330FC, $032FFC, $032EFC, $032DFC, $032CFC, $032BFC, $032AFC, $0329FC, $0328FC, $0327FC, $0326FC, $0325FC, $0324FC, $0323FC, $0223FD, $0222FD, $0221FD, $0220FD, $021FFD, $021EFD, $021DFD, $021CFD, $021BFD, $021AFD, $0219FD, $0218FD, $0217FD, $0216FD, $0215FD, $0215FE, $0115FE, $0114FE, $0113FE, $0112FE, $0111FE, $0110FE, $010FFE, $010EFE, $010DFE, $010CFE, $010BFE, $010AFE, $0109FE, $0108FE, $0107FE, $0007FE, $0007FF, $0006FF, $0005FF, $0004FF, $0003FF, $0002FF, $0001FF, $0000FF
  );
  HEATMAP_DIV = 1.0 / High(HEATMAP_TABLE);

type
  TDrawInfo = record
    Color: TColor;
    Data: PByte;
    BytesPerLine: Integer;
    Width: Integer;
    Height: Integer;
    VisibleRect: TRect;
    Offset: TPoint;
  end;

generic procedure DoDrawPoints<_T>(TPA: TPointArray; DrawInfo: TDrawInfo);
generic procedure DoDrawLine<_T>(Start, Stop: TPoint; DrawInfo: TDrawInfo);
generic procedure DoDrawLineGap<_T>(Start, Stop: TPoint; GapSize: Integer; DrawInfo: TDrawInfo);
generic procedure DoDrawBoxEdge<_T>(Box: TBox; DrawInfo: TDrawInfo);
generic procedure DoDrawBoxFilled<_T>(Box: TBox; DrawInfo: TDrawInfo);
generic procedure DoDrawBoxFilledEx<_T>(Box: TBox; Transparency: Single; DrawInfo: TDrawInfo);
generic procedure DoDrawCircle<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
generic procedure DoDrawCircleFilled<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
generic procedure DoDrawHeatmap<_T>(Mat: TSingleMatrix; DrawInfo: TDrawInfo);
generic procedure DoDrawPolygonFilled<_T>(Poly: TPointArray; DrawInfo: TDrawInfo);

implementation

uses
  Math,
  simba.matrix_float, simba.array_point, simba.box, simba.algo_sort;

generic procedure DoDrawPolygonFilled<_T>(Poly: TPointArray; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Row(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (Y >= 0) and (Y < DrawInfo.Height) then
    begin
      X1 := EnsureRange(X1, 0, DrawInfo.Width - 1);
      X2 := EnsureRange(X2, 0, DrawInfo.Width - 1);

      if ((X2 - X1) + 1 > 0) then
      begin
        Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
        Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
        while (PtrUInt(Ptr) <= Upper) do
        begin
          PType(Ptr)^ := Color;

          Inc(Ptr, SizeOf(_T));
        end;
      end;
    end;
  end;

  {$i shapebuilder_polygonfilled.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildPolygonFilled(Poly, TRect.Create(0,0,DrawInfo.Width-1, DrawInfo.Height-1), DrawInfo.Offset);
end;

generic procedure DoDrawPoints<_T>(TPA: TPointArray; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;
  I, X, Y: Integer;
begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  for I := 0 to High(TPA) do
  begin
    X := TPA[I].X + DrawInfo.Offset.X;
    Y := TPA[I].Y + DrawInfo.Offset.Y;
    if (X >= 0) and (Y >= 0) and (X < DrawInfo.Width) and (Y < DrawInfo.Height) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;
end;

generic procedure DoDrawLine<_T>(Start, Stop: TPoint; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < DrawInfo.Width) and (Y < DrawInfo.Height) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  {$i shapebuilder_line.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildLine(Start, Stop);
end;

generic procedure DoDrawLineGap<_T>(Start, Stop: TPoint; GapSize: Integer; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < DrawInfo.Width) and (Y < DrawInfo.Height) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  {$i shapebuilder_linegap.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildLineGap(Start, Stop, GapSize);
end;

generic procedure DoDrawBoxEdge<_T>(Box: TBox; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < DrawInfo.Width) and (Y < DrawInfo.Height) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  procedure _Row(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (Y >= 0) and (Y < DrawInfo.Height) then
    begin
      X1 := EnsureRange(X1, 0, DrawInfo.Width - 1);
      X2 := EnsureRange(X2, 0, DrawInfo.Width - 1);

      if ((X2 - X1) + 1 > 0) then
      begin
        Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
        Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
        while (PtrUInt(Ptr) <= Upper) do
        begin
          PType(Ptr)^ := Color;

          Inc(Ptr, SizeOf(_T));
        end;
      end;
    end;
  end;

  {$i shapebuilder_boxedge.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildBoxEdge(Box);
end;

generic procedure DoDrawBoxFilled<_T>(Box: TBox; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Row(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (Y >= 0) and (Y < DrawInfo.Height) then
    begin
      X1 := EnsureRange(X1, 0, DrawInfo.Width - 1);
      X2 := EnsureRange(X2, 0, DrawInfo.Width - 1);

      if ((X2 - X1) + 1 > 0) then
      begin
        Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
        Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
        while (PtrUInt(Ptr) <= Upper) do
        begin
          PType(Ptr)^ := Color;

          Inc(Ptr, SizeOf(_T));
        end;
      end;
    end;
  end;

  {$i shapebuilder_boxfilled.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildBoxFilled(Box);
end;

generic procedure DoDrawBoxFilledEx<_T>(Box: TBox; Transparency: Single; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  RMod, GMod, BMod: Single;
  Size, Y: Integer;
  Ptr: PByte;
  Upper: PtrUInt;
begin
  RMod := DrawInfo.Color.R * Transparency;
  GMod := DrawInfo.Color.G * Transparency;
  BMod := DrawInfo.Color.B * Transparency;
  Transparency := 1.0 - Transparency;

  Size := Box.Width * SizeOf(_T);
  for Y := Box.Y1 to Box.Y2 do
  begin
    Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + Box.X1 * SizeOf(_T));
    Upper := PtrUInt(Ptr + Size);
    while (PtrUInt(Ptr) < Upper) do
      with PType(Ptr)^ do
      begin
        R := Byte(Round((R * Transparency) + RMod));
        G := Byte(Round((G * Transparency) + GMod));
        B := Byte(Round((B * Transparency) + BMod));

        Inc(Ptr, SizeOf(_T));
      end;
  end;
end;

generic procedure DoDrawCircle<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Pixel(const X, Y: Integer); inline;
  begin
    if (X >= 0) and (Y >= 0) and (X < DrawInfo.Width) and (Y < DrawInfo.Height) then
      PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ := Color;
  end;

  {$i shapebuilder_circle.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildCircle(Center.X, Center.Y, Radius);
end;

generic procedure DoDrawCircleFilled<_T>(Center: TPoint; Radius: Integer; DrawInfo: TDrawInfo);
type
  PType = ^_T;
var
  Color: _T;

  procedure _Row(Y: Integer; X1, X2: Integer);
  var
    Ptr: PByte;
    Upper: PtrUInt;
  begin
    if (Y >= 0) and (Y < DrawInfo.Height) then
    begin
      X1 := EnsureRange(X1, 0, DrawInfo.Width - 1);
      X2 := EnsureRange(X2, 0, DrawInfo.Width - 1);

      if ((X2 - X1) + 1 > 0) then
      begin
        Ptr := DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X1 * SizeOf(_T));
        Upper := PtrUInt(Ptr) + ((X2 - X1) * SizeOf(_T));
        while (PtrUInt(Ptr) < Upper) do
        begin
          PType(Ptr)^ := Color;

          Inc(Ptr, SizeOf(_T));
        end;
      end;
    end;
  end;

  {$i shapebuilder_circlefilled.inc}

begin
  Color := Default(_T);
  Color.R := DrawInfo.Color.R;
  Color.G := DrawInfo.Color.G;
  Color.B := DrawInfo.Color.B;

  _BuildCircleFilled(Center.X, Center.Y, Radius);
end;

generic procedure DoDrawHeatmap<_T>(Mat: TSingleMatrix; DrawInfo: TDrawInfo);
type
  PType = ^_T;

  procedure _Pixel(const X, Y: Integer; const Color: TColor); inline;
  begin
    // Should already be clipped
    // if (X >= 0) and (Y >= 0) and (X < DrawInfo.Width) and (Y < DrawInfo.Height) then
    with PType(DrawInfo.Data + (Y * DrawInfo.BytesPerLine + X * SizeOf(_T)))^ do
    begin
      R := Color shr R_BIT and $FF;
      G := Color shr G_BIT and $FF;
      B := Color shr B_BIT and $FF;
    end;
  end;

var
  X, Y: Integer;
  B: TBox;
begin
  B.X1 := DrawInfo.VisibleRect.Left;
  B.Y1 := DrawInfo.VisibleRect.Top;
  B.X2 := Min(Min(B.X1 + Mat.Width, Mat.Width) - 1, DrawInfo.VisibleRect.Right - 1);
  B.Y2 := Min(Min(B.Y1 + Mat.Height, Mat.Height) - 1, DrawInfo.VisibleRect.Bottom - 1);

  for Y := B.Y1 to B.Y2 do
    for X := B.X1 to B.X2 do
      _Pixel(X + DrawInfo.Offset.X, Y + DrawInfo.Offset.Y, HEATMAP_TABLE[Round(Mat[Y,X] / HEATMAP_DIV)]);
end;

end.

