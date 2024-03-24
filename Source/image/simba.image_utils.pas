{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.image_utils;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

// https://sashamaps.net/docs/resources/20-colors/
const
  DISTINCT_COLORS: TColorArray = (
    $4B19E6, $4BB43C, $19E1FF, $D86343, $3182F5, $B41E91, $F4D442,
    $E632F0, $45EFBF, $D4BEFA, $909946, $FFBEDC, $24639A, $C8FAFF,
    $000080, $C3FFAA, $008080, $B1D8FF, $750000, $A9A9A9
  );

  ALPHA_OPAQUE      = Byte(255);
  ALPHA_TRANSPARENT = Byte(0);

procedure BlendPixel(const Data: PColorBGRA; const DataW, DataH: Integer; const X,Y: Integer; const Color: TColorBGRA); overload; inline;
procedure BlendPixel(const Pixel: PColorBGRA; const Color: TColorBGRA); overload; inline;

function GetDistinctColor(const Index: Integer): Integer; overload;
function GetDistinctColor(const Color, Index: Integer): Integer; overload;

function GetRotatedSize(W, H: Integer; Angle: Single): TBox;

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; const Value: TColorBGRA);

implementation

uses
  simba.vartype_pointarray, simba.geometry;

procedure BlendPixel(const Pixel: PColorBGRA; const Color: TColorBGRA);
begin
  if (Pixel^.A > 0) then
    with Pixel^ do
    begin
      A := 255 - ((255 - A) * (255 - Color.A) div 255);
      R := (R * Byte(255 - Color.A) + Color.R * Color.A) div 255;
      G := (G * Byte(255 - Color.A) + Color.G * Color.A) div 255;
      B := (B * Byte(255 - Color.A) + Color.B * Color.A) div 255;
    end
  else
    Pixel^ := Color;
end;

procedure BlendPixel(const Data: PColorBGRA; const DataW, DataH: Integer; const X, Y: Integer; const Color: TColorBGRA);
var
  Pixel: PColorBGRA;
begin
  if (X >= 0) and (Y >= 0) and (X < DataW) and (Y < DataH) then
  begin
    Pixel := @Data[Y * DataW + X];
    if (Pixel^.A > 0) then
      with Pixel^ do
      begin
        A := 255 - ((255 - A) * (255 - Color.A) div 255);
        R := (R * Byte(255 - Color.A) + Color.R * Color.A) div 255;
        G := (G * Byte(255 - Color.A) + Color.G * Color.A) div 255;
        B := (B * Byte(255 - Color.A) + Color.B * Color.A) div 255;
      end
    else
      Pixel^ := Color;
  end;
end;

function GetDistinctColor(const Index: Integer): Integer;
begin
  Result := DISTINCT_COLORS[Index mod Length(DISTINCT_COLORS)];
end;

function GetDistinctColor(const Color, Index: Integer): Integer;
begin
  if (Color > -1) then
    Result := Color
  else
    Result := DISTINCT_COLORS[Index mod Length(DISTINCT_COLORS)];
end;

function GetRotatedSize(W, H: Integer; Angle: Single): TBox;
var
  B: TPointArray;
begin
  B := [
    TSimbaGeometry.RotatePoint(Point(0, H), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(W, H), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(W, 0), Angle, W div 2, H div 2),
    TSimbaGeometry.RotatePoint(Point(0, 0), Angle, W div 2, H div 2)
  ];

  Result := B.Bounds();
end;

// in FPC trunk
{$IFDEF CPUX86_64}
procedure FillXxxx_MoreThanTwoXmms; assembler; nostackframe;
{ Input:
  rcx = 'x'
  rdx = byte count
  xmm0 = pattern for unaligned writes
  xmm1 = pattern for aligned writes }
asm
    { x can start and end misaligned on the vector boundary:

      x = ~~][H1][H2][...][T2][T1]~
          [UH]                 [UT]

      UH (“unaligned head”) potentially overlaps with H1 and is already written with 'movdqu' by the caller.
      At least 1 of its bytes is exclusive to it, i.e. if x is already aligned, H1 starts at byte 16.

      H1 and so on are called “aligned heads” or just “heads”.
      T1 and so on are called “aligned tails” or just “tails”.

      UT (“unaligned tail”) is written with another 'movdqu' after the loop.
      At least 1 of its bytes is exclusive to it as well, that’s why 65 is subtracted below instead of 64. }

    lea    -65(%rcx,%rdx), %r8 { r8 = end of x - 65, to get the loop bound and to write UT later (why not write it right away though...). }
    and    $-16, %rcx { align rcx to the LEFT (so needs to be offset by an additional +16 for a while). }
    movdqa %xmm1, 16(%rcx) { Write H1. }
    mov    %r8, %rax
    and    $-16, %rax { rax = “T4” (possibly fictive) = aligned r8 = loop bound. }
    cmp    $49, %rdx { 33~49 bytes might contain 1~2 heads+tails; write as H1 and T1. }
    jle    .LOneAlignedTailWrite
    movdqa %xmm1, 32(%rcx) { Write H2. }
    cmp    $81, %rdx  { 50~81 bytes might contain 2~4 heads+tails; write as H1–2 and T2–1. }
    jle    .LTwoAlignedTailWrites
    cmp    $113, %rdx  { 82~113 bytes might contain 4~6 heads+tails; write as H1–2 and T4–1. }
    jle    .LFourAlignedTailWrites

    add    $48, %rcx
    cmp    $0x80000, %rdx
    jae    .L64xNT_Body

.balign 16
.L64x_Body:
    movdqa %xmm1, (%rcx)
    movdqa %xmm1, 16(%rcx)
    movdqa %xmm1, 32(%rcx)
    movdqa %xmm1, 48(%rcx)
    add    $64, %rcx
    cmp    %rax, %rcx
    jb     .L64x_Body

.LFourAlignedTailWrites:
    movdqa %xmm1, (%rax) { T4 }
    movdqa %xmm1, 16(%rax) { T3 }
.LTwoAlignedTailWrites:
    movdqa %xmm1, 32(%rax) { T2 }
.LOneAlignedTailWrite:
    movdqa %xmm1, 48(%rax) { T1 }
    movdqu %xmm0, 49(%r8) { UT }
    ret

.balign 16
.L64xNT_Body:
    movntdq %xmm1, (%rcx)
    movntdq %xmm1, 16(%rcx)
    movntdq %xmm1, 32(%rcx)
    movntdq %xmm1, 48(%rcx)
    add    $64, %rcx
    cmp    %rax, %rcx
    jb     .L64xNT_Body
    mfence
    jmp    .LFourAlignedTailWrites
end;

procedure FillDWord(var x;count:SizeInt;value:DWord);assembler;nostackframe;
asm
{$ifdef win64}
    mov    %r8d, %eax
{$else}
    mov    %edx, %eax
    mov    %rsi, %rdx
    mov    %rdi, %rcx
{$endif win64}

    cmp    $3, %rdx
    jle    .L3OrLess
    cmp    $8, %rdx
    jle    .L4to8

    movd   %eax, %xmm0
    pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
    movdqu %xmm0, (%rcx)

    shl    $2, %rdx { rdx = byte count }
    mov    %rcx, %r8
    shl    $3, %ecx
    rol    %cl, %eax { misalign the pattern by the misalignment of x }
    mov    %r8, %rcx
    movd   %eax, %xmm1
    pshufd $0, %xmm1, %xmm1 { xmm1 = pattern for aligned writes }
    jmp    FillXxxx_MoreThanTwoXmms

.L4to8:
{$ifndef win64} { on win64, eax = r8d already. }
    mov    %eax, %r8d
{$endif}
    shl    $32, %r8
    or     %r8, %rax
    mov    %rax, (%rcx)
    mov    %rax, 8(%rcx)
    mov    %rax, -16(%rcx,%rdx,4)
    mov    %rax, -8(%rcx,%rdx,4)
    ret

.L3OrLess:
    test   %rdx, %rdx
    jle    .LQuit
    mov    %eax, (%rcx)
    mov    %eax, -4(%rcx,%rdx,4)
    shr    $1, %edx
    mov    %eax, (%rcx,%rdx,4)
.LQuit:
end;
{$ENDIF}

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; const Value: TColorBGRA);
begin
  FillDWord(Data^, Count, DWord(Value));
end;

end.

