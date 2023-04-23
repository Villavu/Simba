{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ATCanvasPrimitives;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$else}
  {$define invert_pixels}
{$endif}

interface

uses
  Classes, SysUtils, Graphics,
  Types,
  Math;

procedure BitmapResize(b: TBitmap; W, H: integer);
procedure BitmapResizeBySteps(b: TBitmap; W, H: integer);

procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);

procedure CanvasLine(C: TCanvas; P1, P2: TPoint; AColor: TColor); inline;
procedure CanvasLine_DottedVertAlt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer); inline;
procedure CanvasLine_Dotted(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer);
procedure CanvasLine_WavyHorz(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
procedure CanvasLine_RoundedEdge(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
procedure CanvasLineHorz_Dashed(C: TCanvas; Color: TColor; X1, Y1, X2: integer; ADashLen, AEmptyLen: integer);

procedure CanvasPaintTriangleUp(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
procedure CanvasPaintTriangleLeft(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;

type
  TATCanvasCornerKind = (
    acckLeftTop,
    acckRightTop,
    acckLeftBottom,
    acckRightBottom
    );
  TATCanvasCornerKinds = set of TATCanvasCornerKind;

procedure CanvasPaintRoundedCorners(C: TCanvas; const R: TRect;
  Kinds: TATCanvasCornerKinds; ColorBackground, ColorBorder, ColorForeground: TColor);

procedure CanvasArrowHorz(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AArrowLen: integer;
  AToRight: boolean;
  APointerScale: integer);

procedure CanvasArrowDown(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  ALengthScale: integer;
  APointerScale: integer);

procedure CanvasArrowWrapped(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  ALengthScale: integer;
  AWidthScale: integer;
  APointerScale: integer);

procedure CanvasPilcrowChar(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AScalePercents: integer);

procedure CanvasPaintPlusMinus(C: TCanvas;
  AColorBorder, AColorBG: TColor;
  ACenter: TPoint;
  ASize, APenWidth: integer;
  APlus: boolean);

procedure CanvasPaintCircleMark(C: TCanvas;
  const R: TRect;
  AColor: TColor;
  AIndentLeft, AIndentRight: integer);

procedure CanvasPaintXMark(C: TCanvas;
  const R: TRect;
  AColor: TColor;
  AIndentLeft, AIndentRight, ALineWidth: integer);

procedure CanvasPaintRoundMark(C: TCanvas;
  const R: TRect;
  AColor: TColor;
  AIndentLeft, AIndentRight, ALineWidth: integer);

type
  TATCollapseStringMode = (
    acsmNone,
    acsmLeft,
    acsmMiddle,
    acsmRight
    );

function CanvasCollapseStringByDots(C: TCanvas;
  const Text: string;
  Mode: TATCollapseStringMode;
  Width: integer;
  DotsString: string=''): string;

function ColorBlend(c1, c2: Longint; A: Longint): Longint;
function ColorBlendHalf(c1, c2: Longint): Longint;

implementation

procedure CanvasLine(C: TCanvas; P1, P2: TPoint; AColor: TColor);
begin
  C.Pen.Color:= ColorToRGB(AColor);
  {$ifdef FPC}
  C.Line(P1, P2);
  {$else}
  C.MoveTo(P1.x, P1.y);
  C.LineTo(P2.x, P2.y);
  {$endif}
end;

procedure _CalcMarkRect(const R: TRect; AIndentLeft, AIndentRight: integer;
  out X1, Y1, X2, Y2: integer);
var
  W: integer;
begin
  W:= R.Right-R.Left-AIndentLeft-AIndentRight;
  X1:= R.Left+AIndentLeft;
  X2:= X1 + W;
  Y1:= (R.Top+R.Bottom) div 2 - W div 2;
  Y2:= Y1 + W;
end;

procedure CanvasPaintCircleMark(C: TCanvas; const R: TRect; AColor: TColor;
  AIndentLeft, AIndentRight: integer);
var
  X1, Y1, X2, Y2: integer;
  NColor: TColor;
begin
  _CalcMarkRect(R, AIndentLeft, AIndentRight, X1, Y1, X2, Y2);

  NColor:= ColorToRGB(AColor);
  C.Pen.Color:= NColor;
  C.Brush.Color:= NColor;

  C.Ellipse(Rect(X1, Y1, X2+2, Y2+2));
end;

procedure CanvasPaintXMark(C: TCanvas; const R: TRect; AColor: TColor;
  AIndentLeft, AIndentRight, ALineWidth: integer);
var
  X1, Y1, X2, Y2: integer;
  NColor: TColor;
begin
  if ALineWidth<1 then
    ALineWidth:= 1;

  _CalcMarkRect(R, AIndentLeft, AIndentRight, X1, Y1, X2, Y2);

  NColor:= ColorToRGB(AColor);
  C.Pen.Color:= NColor;
  C.Brush.Color:= NColor;

  C.Polygon([
    Point(X1, Y1+ALineWidth),
    Point(X1, Y1),
    Point(X1+ALineWidth, Y1),
    Point(X2, Y2-ALineWidth),
    Point(X2, Y2),
    Point(X2-ALineWidth, Y2)
    ]);
  C.Polygon([
    Point(X2-ALineWidth, Y1),
    Point(X2, Y1),
    Point(X2, Y1+ALineWidth),
    Point(X1+ALineWidth, Y2),
    Point(X1, Y2),
    Point(X1, Y2-ALineWidth)
    ]);
end;

procedure CanvasPaintRoundMark(C: TCanvas; const R: TRect; AColor: TColor;
  AIndentLeft, AIndentRight, ALineWidth: integer);
var
  X1, Y1, X2, Y2: integer;
  NColor: TColor;
  R2: TRect;
begin
  _CalcMarkRect(R, AIndentLeft, AIndentRight, X1, Y1, X2, Y2);

  NColor:= ColorToRGB(AColor);
  C.Brush.Color:= NColor;
  R2:= Rect(X1, Y1, X2, Y2);
  C.FillRect(R2);
end;

{$ifdef invert_pixels}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  i, j: integer;
begin
  for j:= R.Top to R.Bottom-1 do
    for i:= R.Left to R.Right-1 do
      C.Pixels[i, j]:= C.Pixels[i, j] xor (not AColor and $ffffff);
end;
{$else}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  OldAntialias: TAntialiasingMode;
  OldMode: TPenMode;
  OldStyle: TPenStyle;
  OldWidth: integer;
  {$ifdef FPC}
  OldEndCap: TPenEndCap;
  {$endif}
begin
  OldAntialias:= C.AntialiasingMode;
  OldMode:= C.Pen.Mode;
  OldStyle:= C.Pen.Style;
  OldWidth:= C.Pen.Width;

  X:= (R.Left+R.Right) div 2;
  C.Pen.Mode:= {$ifdef darwin} pmNot {$else} pmNotXor {$endif};
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  C.AntialiasingMode:= amOff;

  {$ifdef FPC}
  OldEndCap:= C.Pen.EndCap;
  C.Pen.EndCap:= pecFlat;
  {$endif}

  C.Pen.Width:= R.Width;

  C.MoveTo(X, R.Top);
  C.LineTo(X, R.Bottom);

  {$ifdef FPC}
  C.Pen.EndCap:= OldEndCap;
  {$endif}
  C.Pen.Width:= OldWidth;
  C.Pen.Style:= OldStyle;
  C.Pen.Mode:= OldMode;
  C.AntialiasingMode:= OldAntialias;
  C.Rectangle(0, 0, 0, 0); //apply pen
end;
{$endif}

procedure CanvasLine_Dotted(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer);
var
  i: integer;
  vis: boolean;
begin
  vis:= false;
  if Y1=Y2 then
  begin
    for i:= X1 to X2 do
    begin
      vis:= not vis;
      if vis then
        C.Pixels[i, Y2]:= Color;
    end;
  end
  else
  begin
    for i:= Y1 to Y2 do
    begin
      vis:= not vis;
      if vis then
        C.Pixels[X1, i]:= Color;
    end;
  end;
end;

procedure CanvasLine_DottedVertAlt(C: TCanvas; Color: TColor; X1, Y1, Y2: integer); inline;
var
  j: integer;
begin
  for j:= Y1 to Y2 do
    if Odd(j) then
      C.Pixels[X1, j]:= Color;
end;

procedure CanvasLineHorz_Dashed(C: TCanvas; Color: TColor; X1, Y1, X2: integer; ADashLen, AEmptyLen: integer);
var
  X, XTo: integer;
begin
  C.Pen.Color:= Color;
  X:= X1;
  repeat
    if X>X2 then exit;
    XTo:= Min(X2, X+ADashLen);
    C.MoveTo(X, Y1);
    C.LineTo(XTo, Y1);
    X:= XTo+AEmptyLen;
  until false;
end;

procedure CanvasPaintTriangleUp(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y + ASize),
    Point(ACoord.X + ASize*2, ACoord.Y + ASize),
    Point(ACoord.X, ACoord.Y - ASize)
    ]);
end;

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y - ASize),
    Point(ACoord.X + ASize*2, ACoord.Y - ASize),
    Point(ACoord.X, ACoord.Y + ASize)
    ]);
end;

procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer); inline;
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize, ACoord.Y - ASize*2),
    Point(ACoord.X + ASize, ACoord.Y),
    Point(ACoord.X - ASize, ACoord.Y + ASize*2)
    ]);
end;

procedure CanvasPaintTriangleLeft(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X + ASize, ACoord.Y - ASize*2),
    Point(ACoord.X - ASize, ACoord.Y),
    Point(ACoord.X + ASize, ACoord.Y + ASize*2)
    ]);
end;

procedure CanvasArrowHorz(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AArrowLen: integer;
  AToRight: boolean;
  APointerScale: integer);
const
  cIndent = 1; //offset left/rt
var
  XLeft, XRight, X1, X2, Y, Dx: integer;
begin
  XLeft:= ARect.Left+cIndent;
  XRight:= ARect.Right-cIndent;

  if AArrowLen=0 then
  begin;
    X1:= XLeft;
    X2:= XRight;
  end
  else
  begin
    X1:= XLeft;
    X2:= Min(XRight, X1+AArrowLen);
  end;

  Y:= (ARect.Top+ARect.Bottom) div 2;
  Dx:= ARect.Height * APointerScale div 100;
  C.Pen.Color:= AColorFont;

  C.MoveTo(X1, Y);
  C.LineTo(X2, Y);
  if AToRight then
  begin
    C.MoveTo(X2, Y);
    C.LineTo(X2-Dx, Y-Dx);
    C.MoveTo(X2, Y);
    C.LineTo(X2-Dx, Y+Dx);
  end
  else
  begin
    C.MoveTo(X1, Y);
    C.LineTo(X1+Dx, Y-Dx);
    C.MoveTo(X1, Y);
    C.LineTo(X1+Dx, Y+Dx);
  end;
end;

procedure CanvasArrowDown(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  ALengthScale: integer;
  APointerScale: integer);
var
  Len, X, Y1, Y2, Dx: integer;
begin
  X:= (ARect.Left+ARect.Right) div 2;
  Len:= ARect.Height * ALengthScale div 100;
  Dx:= ARect.Height * APointerScale div 100;
  C.Pen.Color:= AColorFont;

  Y1:= (ARect.Bottom+ARect.Top-Len) div 2;
  Y2:= Y1+Len;

  C.MoveTo(X, Y1);
  C.LineTo(X, Y2);
  C.MoveTo(X, Y2);
  C.LineTo(X-Dx, Y2-Dx);
  C.MoveTo(X, Y2);
  C.LineTo(X+Dx, Y2-Dx);
end;

procedure CanvasArrowWrapped(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  ALengthScale: integer;
  AWidthScale: integer;
  APointerScale: integer);
var
  Len, W, X1, X2, Y1, Y2, Dx: integer;
begin
  Len:= ARect.Height * ALengthScale div 100;
  W:= ARect.Width * AWidthScale div 100;
  Dx:= ARect.Height * APointerScale div 100;
  C.Pen.Color:= AColorFont;

  X1:= (ARect.Left+ARect.Right-W) div 2;
  X2:= X1+W;
  Y1:= (ARect.Bottom+ARect.Top-Len) div 2;
  Y2:= Y1+Len-1;

  //C.MoveTo(X1, Y1);
  //C.LineTo(X2, Y1);
  C.MoveTo(X2, Y1);
  C.LineTo(X2, Y2+1);
  C.MoveTo(X1, Y2);
  C.LineTo(X2, Y2);

  C.MoveTo(X1, Y2);
  C.LineTo(X1+Dx, Y2-Dx);
  C.MoveTo(X1, Y2);
  C.LineTo(X1+Dx, Y2+Dx);
end;


procedure CanvasPilcrowChar(C: TCanvas;
  const ARect: TRect;
  AColorFont: TColor;
  AScalePercents: integer);
var
  H, X1, X2, Y1, Y2, Xr1, Yr1, Yr2: integer;
  R2: TRect;
begin
  C.Pen.Color:= AColorFont;
  C.Brush.Color:= AColorFont;

  H:= ARect.Height * AScalePercents div 100;
  X1:= (ARect.Left+ARect.Right) div 2+1;
  X2:= ARect.Right-1;
  if X2-X1<2 then Dec(X1);
  Y1:= (ARect.Bottom+ARect.Top-H) div 2;
  Y2:= Y1+H;
  C.MoveTo(X1, Y1);
  C.LineTo(X1, Y2);
  C.MoveTo(X2, Y1);
  C.LineTo(X2, Y2);
  C.MoveTo(X1, Y1);
  C.LineTo(X2, Y1);

  Xr1:= Min(ARect.Left+2, X1-2);
  Yr1:= Y1;
  Yr2:= Yr1+(X1-Xr1)+1;
  R2:= Rect(Xr1, Yr1, X1, Yr2);
  C.FillRect(R2);
end;

procedure CanvasPaintPlusMinus(C: TCanvas; AColorBorder, AColorBG: TColor;
  ACenter: TPoint; ASize, APenWidth: integer; APlus: boolean);
var
  OldPenWidth: integer;
  {$ifdef fpc}
  OldPenCap: TPenEndCap;
  {$endif}
begin
  OldPenWidth:= C.Pen.Width;
  {$ifdef fpc}
  OldPenCap:= C.Pen.EndCap;
  {$endif}

  C.Brush.Color:= AColorBG;
  C.Pen.Color:= AColorBorder;
  C.Pen.Width:= APenWidth;
  {$ifdef fpc}
  C.Pen.EndCap:= pecFlat;
  {$endif}

  C.Rectangle(ACenter.X-ASize, ACenter.Y-ASize, ACenter.X+ASize+1, ACenter.Y+ASize+1);

  //avoid painting plus/minus by pen width 2/4/6
  if not Odd(APenWidth) then
    C.Pen.Width:= APenWidth-1;

  C.MoveTo(ACenter.X-ASize+APenWidth+1, ACenter.Y);
  C.LineTo(ACenter.X+ASize-APenWidth, ACenter.Y);
  if APlus then
  begin
    C.MoveTo(ACenter.X, ACenter.Y-ASize+APenWidth+1);
    C.LineTo(ACenter.X, ACenter.Y+ASize-APenWidth);
  end;

  C.Pen.Width:= OldPenWidth;
  {$ifdef fpc}
  C.Pen.EndCap:= OldPenCap;
  {$endif}
end;

procedure CanvasLine_WavyHorz(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
const
  cWavePeriod = 2;
  cWaveInc: array[0..cWavePeriod-1] of integer = (0, 2);
var
  Points: array of TPoint;
  PointCount, PointIndex: integer;
  X, Y, NSign: integer;
begin
  //some initial len of array, not accurate
  PointCount:= (X2-X1+1) div 2;
  if PointCount<3 then exit;
  SetLength(Points, PointCount);

  if AtDown then NSign:= -1 else NSign:= 1;
  PointIndex:= 0;

  for X:= X1 to X2 do
    if not Odd(X) then
    begin
      Y:= Y2 + NSign * cWaveInc[(X-X1) div 2 mod cWavePeriod];
      if PointIndex>High(Points) then
        SetLength(Points, Length(Points)+1);
      Points[PointIndex]:= Point(X, Y);
      Inc(PointIndex);
    end;

  C.Pen.Color:= Color;
  C.Polyline(Points);
  SetLength(Points, 0);
end;

procedure CanvasLine_RoundedEdge(C: TCanvas; Color: TColor; X1, Y1, X2, Y2: integer; AtDown: boolean);
var
  Points: array[0..3] of TPoint;
begin
  C.Pen.Color:= Color;
  if Y1=Y2 then
  begin
    //paint polyline, 4 points, horz line and 2 edges
    Points[1]:= Point(X1+2, Y1);
    Points[2]:= Point(X2-2, Y2);
    if AtDown then
    begin
      Points[0]:= Point(X1, Y1-2);
      Points[3]:= Point(X2+1, Y2-3);
    end
    else
    begin
      Points[0]:= Point(X1, Y1+2);
      Points[3]:= Point(X2+1, Y2+3);
    end;
    C.Polyline(Points);
  end
  else
  begin
    C.MoveTo(X1, Y1+2);
    C.LineTo(X2, Y2-1);
    //don't draw pixels, other lines did it
  end;
end;


function CanvasCollapseStringByDots(C: TCanvas;
  const Text: string;
  Mode: TATCollapseStringMode;
  Width: integer;
  DotsString: string=''): string;
const
  cMinLen = 3;
var
  S, STemp: UnicodeString; //UnicodeString to do steps by 1 widechar
  N, i: integer;
begin
  if (Mode=acsmNone) or
    (C.TextWidth(Text)<=Width) then
  begin
    Result:= Text;
    exit
  end;

  if DotsString='' then
    DotsString:= {$ifdef fpc}UTF8Encode{$endif}(#$2026);

  S:= Text;
  STemp:= S;

  case Mode of
    acsmLeft:
      begin
        repeat
          Delete(STemp, 1, 1);
          S:= DotsString+STemp;
        until (Length(S)<=cMinLen) or (C.TextWidth(S)<=Width);
      end;

    acsmMiddle:
      begin
        for i:= 2 to $FFFF do
        begin
          N:= (Length(STemp)+1) div 2 - i div 2;
          S:= Copy(STemp, 1, N)+DotsString+Copy(STemp, N+i, MaxInt);
          if (Length(S)<=cMinLen) or (C.TextWidth(S)<=Width) then Break;
        end;
      end;

    acsmRight:
      begin
        repeat
          SetLength(STemp, Length(STemp)-1);
          S:= STemp+DotsString;
        until (Length(S)<=cMinLen) or (C.TextWidth(S)<=Width);
      end;
  end;

  Result:= S;
end;


procedure BitmapResize(b: TBitmap; W, H: integer);
begin
  {$ifdef fpc}
  b.SetSize(W, H);
  b.FreeImage; //recommended, otherwise black bitmap on big size
  {$else}
  b.Width:= W;
  b.Height:= H;
  {$endif}
end;

procedure BitmapResizeBySteps(b: TBitmap; W, H: integer);
const
  StepW = 60;
  StepH = 40;
var
  SizeX, SizeY: integer;
begin
  SizeX:= (W div StepW + 1)*StepW;
  SizeY:= (H div StepH + 1)*StepH;
  if (SizeX>b.Width) or
    (SizeY>b.Height) then
    BitmapResize(b, SizeX, SizeY);
end;


function ColorBlend(c1, c2: Longint; A: Longint): Longint;
//blend level: 0..255
var
  r, g, b, v1, v2: byte;
begin
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= A * (v1 - v2) shr 8 + v2;
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= A * (v1 - v2) shr 8 + v2;
  Result := (b shl 16) + (g shl 8) + r;
end;

function ColorBlendHalf(c1, c2: Longint): Longint;
var
  r, g, b, v1, v2: byte;
begin
  v1:= Byte(c1);
  v2:= Byte(c2);
  r:= (v1+v2) shr 1;
  v1:= Byte(c1 shr 8);
  v2:= Byte(c2 shr 8);
  g:= (v1+v2) shr 1;
  v1:= Byte(c1 shr 16);
  v2:= Byte(c2 shr 16);
  b:= (v1+v2) shr 1;
  Result := (b shl 16) + (g shl 8) + r;
end;


procedure CanvasPaintRoundedCorners(C: TCanvas; const R: TRect;
  Kinds: TATCanvasCornerKinds; ColorBackground, ColorBorder,
  ColorForeground: TColor);
var
  ColorMixEmpty, ColorMixBg: TColor;
begin
  ColorMixEmpty:= ColorBlendHalf(ColorBorder, ColorBackground);
  ColorMixBg:= ColorBlendHalf(ColorBorder, ColorForeground);

  if acckLeftTop in Kinds then
  begin
    C.Pixels[R.Left, R.Top]:= ColorBackground;
    //
    C.Pixels[R.Left+1, R.Top]:= ColorMixEmpty;
    C.Pixels[R.Left, R.Top+1]:= ColorMixEmpty;
    //
    C.Pixels[R.Left+1, R.Top+1]:= ColorBorder;
    //
    C.Pixels[R.Left+2, R.Top+1]:= ColorMixBg;
    C.Pixels[R.Left+1, R.Top+2]:= ColorMixBg;
  end;

  if acckRightTop in Kinds then
  begin
    C.Pixels[R.Right-1, R.Top]:= ColorBackground;
    //
    C.Pixels[R.Right-2, R.Top]:= ColorMixEmpty;
    C.Pixels[R.Right-1, R.Top+1]:= ColorMixEmpty;
    //
    C.Pixels[R.Right-2, R.Top+1]:= ColorBorder;
    //
    C.Pixels[R.Right-3, R.Top+1]:= ColorMixBg;
    C.Pixels[R.Right-2, R.Top+2]:= ColorMixBg;
  end;

  if acckLeftBottom in Kinds then
  begin
    C.Pixels[R.Left, R.Bottom-1]:= ColorBackground;
    //
    C.Pixels[R.Left+1, R.Bottom-1]:= ColorMixEmpty;
    C.Pixels[R.Left, R.Bottom-2]:= ColorMixEmpty;
    //
    C.Pixels[R.Left+1, R.Bottom-2]:= ColorBorder;
    //
    C.Pixels[R.Left+2, R.Bottom-2]:= ColorMixBg;
    C.Pixels[R.Left+1, R.Bottom-3]:= ColorMixBg;
  end;

  if acckRightBottom in Kinds then
  begin
    C.Pixels[R.Right-1, R.Bottom-1]:= ColorBackground;
    //
    C.Pixels[R.Right-2, R.Bottom-1]:= ColorMixEmpty;
    C.Pixels[R.Right-1, R.Bottom-2]:= ColorMixEmpty;
    //
    C.Pixels[R.Right-2, R.Bottom-2]:= ColorBorder;
    //
    C.Pixels[R.Right-3, R.Bottom-2]:= ColorMixBg;
    C.Pixels[R.Right-2, R.Bottom-3]:= ColorMixBg;
  end;
end;


end.

