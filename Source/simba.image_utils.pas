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
  simba.base, simba.image;

type
  TSimbaIntegralImageF = record
  public
  type
    TData = record Value: Double; ValueSqr: Double; end;
    TDataMatrix = array of array of TData;
  public
    Data: TDataMatrix;

    class function Create(const From: TByteMatrix): TSimbaIntegralImageF; static;
    procedure Query(const Left, Top, Right, Bottom: Integer; out Value, ValueSqr: Double); overload;
    function Query(const Left, Top, Right, Bottom: Integer): Double; overload;
  end;

  TSimbaIntegralImageRGB = record
  public
  type
    TData = record R, G, B: UInt64; end;
    TDataMatrix = array of array of TData;
  public
    Data: TDataMatrix;

    class function Create(const From: TSimbaImage): TSimbaIntegralImageRGB; static;
    procedure Query(const Left, Top, Right, Bottom: Integer; out RSum, GSum, BSum: UInt64);
  end;

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

function GetDistinctColor(const Index: Integer): Integer;
function GetRotatedSize(W, H: Integer; Angle: Single): TBox;

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; const Value: TColorBGRA);

implementation

uses
  simba.vartype_ordmatrix, simba.vartype_pointarray, simba.geometry;

class function TSimbaIntegralImageF.Create(const From: TByteMatrix): TSimbaIntegralImageF;
var
  X, Y: Integer;
begin
  SetLength(Result.Data, From.Height, From.Width);

  // Compute the first row of the integral image
  for X := 0 to From.Width - 1 do
  begin
    Result.Data[0, X].Value := From[0, X];
    Result.Data[0, X].ValueSqr := Sqr(From[0, X]);
    if (X > 0) then
    begin
      Result.Data[0, X].Value += Result.Data[0, X - 1].Value;
      Result.Data[0, X].ValueSqr += Result.Data[0, X - 1].ValueSqr;
    end;
  end;

  // Compute the first column of the integral image
  for Y := 1 to From.Height - 1 do
  begin
    Result.Data[Y, 0].Value := From[Y, 0] + Result.Data[Y - 1, 0].Value;
    Result.Data[Y, 0].ValueSqr := Sqr(From[Y, 0]) + Result.Data[Y - 1, 0].ValueSqr;
  end;

  // Compute the rest of the integral image
  for Y := 1 to From.Height - 1 do
    for X := 1 to From.Width - 1 do
    begin
      Result.Data[Y, X].Value := From[Y, X] + Result.Data[Y - 1, X].Value + Result.Data[Y, X - 1].Value - Result.Data[Y - 1, X - 1].Value;
      Result.Data[Y, X].ValueSqr := Sqr(From[Y, X]) + Result.Data[Y - 1, X].ValueSqr + Result.Data[Y, X - 1].ValueSqr - Result.Data[Y - 1, X - 1].ValueSqr;
    end;
end;

procedure TSimbaIntegralImageF.Query(const Left, Top, Right, Bottom: Integer; out Value, ValueSqr: Double);
var
  A,B,C,D: TData;
begin
  A := Default(TData);
  B := Default(TData);
  C := Default(TData);
  D := Default(TData);

  if (Left - 1 >= 0) and (Top - 1 >= 0) then
    A := Data[Top - 1, Left - 1];
  if (Top - 1 >= 0) then
    B := Data[Top - 1, Right];
  if (Left - 1 >= 0) then
    C := Data[Bottom, Left - 1];
  D := Data[Bottom, Right];

  Value    := D.Value - B.Value - C.Value + A.Value;
  ValueSqr := D.ValueSqr - B.ValueSqr - C.ValueSqr + A.ValueSqr;
end;

function TSimbaIntegralImageF.Query(const Left, Top, Right, Bottom: Integer): Double;
var
  _: Double;
begin
  Query(Left, Top, Right, Bottom, Result, _);
end;

class function TSimbaIntegralImageRGB.Create(const From: TSimbaImage): TSimbaIntegralImageRGB;
var
  X, Y: Integer;
begin
  SetLength(Result.Data, From.Height, From.Width);

  // Compute the first row of the integral image
  for X := 0 to From.Width - 1 do
  begin
    Result.Data[0, X].R := From.Data[X].R;
    Result.Data[0, X].G := From.Data[X].G;
    Result.Data[0, X].B := From.Data[X].B;

    if (X > 0) then
    begin
      Result.Data[0, X].R += Result.Data[0, X - 1].R;
      Result.Data[0, X].G += Result.Data[0, X - 1].G;
      Result.Data[0, X].B += Result.Data[0, X - 1].B;
    end;
  end;

  // Compute the first column of the integral image
  for Y := 1 to From.Height - 1 do
  begin
    Result.Data[Y, 0].R := From.Data[Y * From.Width].R + Result.Data[Y - 1, 0].R;
    Result.Data[Y, 0].G := From.Data[Y * From.Width].G + Result.Data[Y - 1, 0].G;
    Result.Data[Y, 0].B := From.Data[Y * From.Width].B + Result.Data[Y - 1, 0].B;
  end;

  // Compute the rest of the integral image
  for Y := 1 to From.Height - 1 do
    for X := 1 to From.Width - 1 do
    begin
      Result.Data[Y, X].R := From.Data[Y * From.Width + X].R + Result.Data[Y - 1, X].R + Result.Data[Y, X - 1].R - Result.Data[Y - 1, X - 1].R;
      Result.Data[Y, X].G := From.Data[Y * From.Width + X].G + Result.Data[Y - 1, X].G + Result.Data[Y, X - 1].G - Result.Data[Y - 1, X - 1].G;
      Result.Data[Y, X].B := From.Data[Y * From.Width + X].B + Result.Data[Y - 1, X].B + Result.Data[Y, X - 1].B - Result.Data[Y - 1, X - 1].B;
    end;
end;

procedure TSimbaIntegralImageRGB.Query(const Left, Top, Right, Bottom: Integer; out RSum, GSum, BSum: UInt64);
var
  A,B,C,D: TData;
begin
  A := Default(TData);
  B := Default(TData);
  C := Default(TData);
  D := Default(TData);

  if (Left - 1 >= 0) and (Top - 1 >= 0) then
    A := Data[Top - 1, Left - 1];
  if (Top - 1 >= 0) then
    B := Data[Top - 1, Right];
  if (Left - 1 >= 0) then
    C := Data[Bottom, Left - 1];
  D := Data[Bottom, Right];

  RSum := D.R - B.R - C.R + A.R;
  GSum := D.G - B.G - C.G + A.G;
  BSum := D.B - B.B - C.B + A.B;
end;

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
  xmm0 = pattern for ALIGNED writes
  First and last 16 bytes are written. }
const
  NtThreshold = 4 * 1024 * 1024;
asm
  { x can start and end misaligned on the vector boundary:

    x = ~~][H1][H2][...][T2][T1]~
        [UH]                 [UT]

    UH (“unaligned head”) potentially overlaps with H1 and is already written with 'movdqu' by the caller.
    At least 1 of its bytes is exclusive to it, i.e. if x is already aligned, H1 starts at byte 16.

    H1 and so on are called “aligned heads” or just “heads”.
    T1 and so on are called “aligned tails” or just “tails”.

    UT (“unaligned tail”) is written by the caller as well.
    At least 1 of its bytes is exclusive to it as well, that’s why 65 is subtracted below instead of 64. }

  lea    -65(%rcx,%rdx), %rax
  and    $-16, %rax { rax = “T4” (possibly fictive). }
  mov    %rax, %rdx { Remember T4 to rdx. }
  and    $-16, %rcx { rcx = H1 − 16. }
  sub    %rcx, %rax { rax = aligned byte count − 48. }
  movdqa %xmm0, 16(%rcx) { Write H1. }
  cmp    $32-48, %rax
  jle    .LOneAlignedTailWrite
  movdqa %xmm0, 32(%rcx) { Write H2. }
  cmp    $64-48, %rax
  jle    .LTwoAlignedTailWrites
  sub    $48, %rax { rax = aligned byte count − 96 (32 bytes already written + 64 bytes written after loop). }
  jle    .LFourAlignedTailWrites

  add    $48, %rcx { rcx = H3. }
  cmp    $NtThreshold-64, %rax
  jae    .L64xNT_Body

.balign 16
.L64x_Body:
  movdqa %xmm0, (%rcx)
  movdqa %xmm0, 16(%rcx)
  movdqa %xmm0, 32(%rcx)
  movdqa %xmm0, 48(%rcx)
  add    $64, %rcx
  sub    $64, %rax
  ja     .L64x_Body

.LFourAlignedTailWrites:
  movdqa %xmm0, (%rdx) { T4 }
  movdqa %xmm0, 16(%rdx) { T3 }
.LTwoAlignedTailWrites:
  movdqa %xmm0, 32(%rdx) { T2 }
.LOneAlignedTailWrite:
  movdqa %xmm0, 48(%rdx) { T1 }
  ret

.balign 16
.L64xNT_Body:
  movntdq %xmm0, (%rcx)
  movntdq %xmm0, 16(%rcx)
  movntdq %xmm0, 32(%rcx)
  movntdq %xmm0, 48(%rcx)
  add    $64, %rcx
  sub    $64, %rax
  ja     .L64xNT_Body
  sfence
  jmp    .LFourAlignedTailWrites
end;

procedure FillDWord(var x; count: SizeInt; value: DWord); assembler; nostackframe;
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
  movdqu %xmm0, -16(%rcx,%rdx,4)

  shl    $2, %rdx { rdx = byte count }
  mov    %rcx, %r8
  shl    $3, %ecx
  rol    %cl, %eax { misalign the pattern by the misalignment of x }
  mov    %r8, %rcx
  movd   %eax, %xmm0
  pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for aligned writes }
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

// in FPC trunk
{$IFDEF CPUX86}
const
  FillXxxx_RepStosThreshold_NoERMS = 512 * 1024;

procedure FillXxxx_U32Pattern_RepStos_8OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 8 (preferably >= FillXxxx_RepStosThreshold_(No)ERMS, depending on fast_large_repmovstosb). }
asm
{$ifdef FPC_ENABLED_CLD}
  cld
{$endif FPC_ENABLED_CLD}
  mov    %ecx, (%eax) { Write first 4 bytes unaligned. }
  push   %ecx { pattern }
  push   %edi
  mov    %eax, %edi { Move x to edi, as expected by ‘rep stosl’. }
  xchg   %eax, %ecx { now eax = pattern (as expected by ‘rep stosl’) and ecx = x (to rotate the pattern by its misalignment) }
  shl    $3, %ecx { ecx = misalignment of x in bits. }
  rol    %cl, %eax { misalign the pattern; no-op for FillChar, but handles misaligned cases of FillWord+. }
  add    %edi, %edx { edx = x end }
  lea    -1(%edx), %ecx { ecx = x end - 1. }
  add    $4, %edi
  and    $-4, %edi { edi = 4-byte aligned pointer strictly to the right of the start. }
  and    $-4, %ecx { ecx = 4-byte aligned pointer strictly to the left of the end. }
  sub    %edi, %ecx { ecx = byte count between them. }
  shr    $2, %ecx { ecx = uint32 count, as expected by ‘rep stosl’. }
  rep stosl
  pop    %edi
  pop    %ecx
  mov    %ecx, -4(%edx) { Write last 4 bytes unaligned. }
end;

label
  FillXxxx_MoreThanTwoXMMs;

procedure FillXxxx_U32Pattern_SSE2_16OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 16 (preferably > 16). }
const
  NtThreshold = 4 * 1024 * 1024;
asm
  movd   %ecx, %xmm0
  pshufd $0, %xmm0, %xmm0 { xmm0 = pattern for unaligned writes }
  movdqu %xmm0, (%eax)
  movdqu %xmm0, -16(%eax,%edx)
  cmp    $32, %edx
  ja     .LMoreThanTwoVectors
  ret
  .byte  144 { Turn .balign 16 before .L64x_Body into a no-op. }

{ x can start and end misaligned on the vector boundary:
  x = ~~][H1][H2][...][T2][T1]~
      [UH]                 [UT]
  UH/UT stands for “unaligned head/tail”, both have 1~16 bytes. }

.LMoreThanTwoVectors:
  push   %esi
  mov    %ecx, %esi { esi = pattern }
  mov    %eax, %ecx
  shl    $3, %ecx { ecx = misalignment of x in bits }
  rol    %cl, %esi { misalign the pattern }
  movd   %esi, %xmm0
  pshufd $0, %xmm0, %xmm0
  pop    %esi

{ FillChar (to skip the misaligning above) and FillQWord jump here.
eax — x, edx — byte count > 32, xmm0 = pattern for ALIGNED writes, first and last 16 bytes written. }
FillXxxx_MoreThanTwoXMMs:
  lea    -65(%eax,%edx), %ecx
  and    $-16, %ecx { ecx = “T4” (possibly fictive) = loop bound. }
  mov    %ecx, %edx { Remember T4 to edx. }
  and    $-16, %eax { eax = H1 − 16. }
  sub    %eax, %ecx { ecx = aligned byte count − 48. }
  movdqa %xmm0, 16(%eax) { Write H1. }
  cmp    $32-48, %ecx
  jle    .LOneAlignedTailWrite
  movdqa %xmm0, 32(%eax) { Write H2. }
  cmp    $64-48, %ecx
  jle    .LTwoAlignedTailWrites
  sub    $48, %ecx { ecx = aligned byte count − 96 (32 bytes already written + 64 bytes written after loop). }
  jle    .LFourAlignedTailWrites { ecx was ≤ 96−48 }

  add    $48, %eax { eax = H3. }
  cmp    $NtThreshold-64, %ecx { Need to write aligned byte count − 32 bytes already written. ecx = aligned byte count − 96, so compare ecx + 64 to NtThreshold, or ecx to NtThreshold − 64. }
  jae    .L64xNT_Body

.balign 16 { no-op }
.L64x_Body:
  movdqa %xmm0, (%eax)
  movdqa %xmm0, 16(%eax)
  movdqa %xmm0, 32(%eax)
  movdqa %xmm0, 48(%eax)
  add    $64, %eax
  sub    $64, %ecx
  ja     .L64x_Body
.LFourAlignedTailWrites:
  movdqa %xmm0, (%edx) { T4 }
  movdqa %xmm0, 16(%edx) { T3 }
.LTwoAlignedTailWrites:
  movdqa %xmm0, 32(%edx) { T2 }
.LOneAlignedTailWrite:
  movdqa %xmm0, 48(%edx) { T1 }
  ret

.balign 16
.L64xNT_Body:
  movntdq %xmm0, (%eax)
  movntdq %xmm0, 16(%eax)
  movntdq %xmm0, 32(%eax)
  movntdq %xmm0, 48(%eax)
  add    $64, %eax
  sub    $64, %ecx
  ja     .L64xNT_Body
  sfence
  jmp    .LFourAlignedTailWrites
end;

procedure FillXxxx_U32Pattern_Plain_16OrMore; assembler; nostackframe;
{ eax — x, ecx — uint32 pattern, edx — byte count >= 12 (preferably >= 16). }
asm
  mov     %ecx, (%eax) { Write first 4 bytes. }
  lea     -9(%eax,%edx), %edx
  mov     %ecx, 5(%edx) { Write last 4 bytes. }
  and     $-4, %edx { edx = loop bound. }
  push    %esi
  mov     %ecx, %esi { esi = pattern }
  mov     %eax, %ecx
  shl     $3, %ecx { ecx = misalignment of x in bits }
  rol     %cl, %esi { misalign the pattern }
  add     $4, %eax
  and     $-4, %eax
.balign 16
.L8xLoop:
  mov     %esi, (%eax)
  mov     %esi, 4(%eax)
  add     $8, %eax
  cmp     %edx, %eax
  jb      .L8xLoop
  mov     %esi, (%edx)
  mov     %esi, 4(%edx)
  pop     %esi
end;

procedure FillDWord_4OrLess; assembler; nostackframe;
asm
  cmp     $1, %edx
  jl      .LQuit
  mov     %ecx, (%eax)
  je      .LQuit
  mov     %ecx, 4(%eax)
  mov     %ecx, -8(%eax,%edx,4)
  mov     %ecx, -4(%eax,%edx,4)
.LQuit:
end;

procedure FillDWord_Plain(var x;count:SizeInt;value:dword); assembler; nostackframe;
asm
  cmp     $4, %edx
  jle     FillDWord_4OrLess
  shl     $2, %edx
  jmp     FillXxxx_U32Pattern_Plain_16OrMore
end;

procedure FillDWord_SSE2(var x;count:SizeInt;value:dword); assembler; nostackframe;
asm
  cmp     $4, %edx
  jle     FillDWord_4OrLess
  shl     $2, %edx
  cmp     $FillXxxx_RepStosThreshold_NoERMS, %edx
  jb      FillXxxx_U32Pattern_SSE2_16OrMore
  jmp     FillXxxx_U32Pattern_RepStos_8OrMore
end;

procedure FillDWord(var x; count: SizeInt; value: DWord);
begin
  if has_sse2_support then
    FillDWord_SSE2(x, count, value)
  else
    FillDWord_Plain(x, count, value);
end;
{$ENDIF}

procedure FillData(const Data: PColorBGRA; const Count: SizeInt; const Value: TColorBGRA);
begin
  FillDWord(Data^, Count, DWord(Value));
end;

end.

