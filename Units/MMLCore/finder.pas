{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Finder class for the Mufasa Macro Library
}

unit finder;

{$mode objfpc}{$H+}
{$INLINE ON}

interface

{$define CheckAllBackground}//Undefine this to only check the first white point against the background (in masks).
uses
  colour_conv, Classes, SysUtils, bitmaps, DTM, MufasaTypes; // Types

{ TMFinder Class }

{
  Should be 100% OS independant, as all OS dependant code is in the IO Manager.
  Let's try not to use any OS-specific defines here? ;)

  TODO: Check that each procedure calling Create_CTSInfo also calls
  Free_CTSInfo().
}

type
  TCTSNoInfo = record    //No tolerance
      B, G, R, A:byte;
  end;
  PCTSNoInfo = ^TCTSNoInfo;

  TCTS0Info = record
      B, G, R, A: byte;
      Tol: Integer;
  end;
  PCTS0Info = ^TCTS0Info;

  TCTS1Info = record
      B, G, R, A: byte;
      Tol: Integer; { Squared }
  end;
  PCTS1Info = ^TCTS1Info;

  TCTS2Info = record
      H, S, L: extended;
      hueMod, satMod: extended;
      Tol: Integer;
  end;
  PCTS2Info = ^TCTS2Info;

  TCTS3Info = record
      L, A, B: extended;
      Tol: Integer; { Squared * CTS3Modifier}
  end;
  PCTS3Info = ^TCTS3Info;

  TCTSInfo = Pointer;
  TCTSInfoArray = Array of TCTSInfo;
  TCTSInfo2DArray = Array of TCTSInfoArray;
  TCTSCompareFunction = function (ctsInfo: Pointer; C2: PRGB32): boolean;

type
  TMFinder = class(TObject)
  private
    Client: TObject;
    CachedWidth, CachedHeight : integer;
    ClientTPA : TPointArray;
    hueMod, satMod: Extended;
    CTS3Modifier: Extended;
    CTS: Integer;

    Procedure UpdateCachedValues(NewWidth,NewHeight : integer);
    //Loads the Spiral into ClientTPA (Will not cause problems)
    procedure LoadSpiralPath(startX, startY, x1, y1, x2, y2: Integer);
  public
    WarnOnly : boolean;
    procedure DefaultOperations(var xs,ys,xe,ye : integer);
    function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
    function CountColor(Color, xs, ys, xe, ye: Integer): Integer;
    function SimilarColors(Color1,Color2,Tolerance : Integer) : boolean;
    // Possibly turn x, y into a TPoint var.
    function FindColor(out x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
    function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;
    function FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye,Tol: Integer): Boolean;
    function FindColorTolerance(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
    function FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean;
    function FindColorsSpiralTolerance(x, y: Integer; out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tol: Integer) : boolean;
    function FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
    function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea: Integer): Boolean;
    function FindColoredAreaTolerance(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea, tol: Integer): Boolean;
    //Mask
    function FindMaskTolerance(const mask: TMask; out x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;
    procedure CheckMask(const Mask : TMask);
    //Bitmap functions
    function FindBitmap(bitmap: TMufasaBitmap; out x, y: Integer): Boolean;
    function FindBitmapIn(bitmap: TMufasaBitmap; out x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;
    function FindBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;
    function FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;
    function FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;
    function FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x, y: Integer;
        out Points : TPointArray; xs, ys, xe, ye,tolerance: Integer; maxToFind:
            Integer = 0): Boolean;
    function FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; out accuracy: Extended): Boolean;

    function FindDTM(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
    function FindDTMs(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2 : integer; maxToFind: Integer = 0): Boolean;
    function FindDTMRotated(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended; Alternating : boolean): Boolean;
    function FindDTMsRotated(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray;Alternating : boolean; maxToFind: Integer = 0): Boolean;
    //Donno
    function GetColors(const Coords: TPointArray): TIntegerArray;

    // tol speeds
    procedure SetToleranceSpeed(nCTS: Integer);
    function GetToleranceSpeed: Integer;
    procedure SetToleranceSpeed2Modifiers(const nHue, nSat: Extended);
    procedure GetToleranceSpeed2Modifiers(out hMod, sMod: Extended);
    procedure SetToleranceSpeed3Modifier(modifier: Extended);
    function GetToleranceSpeed3Modifier: Extended;

    { }
    function Create_CTSInfo(Color, Tolerance: Integer): Pointer; overload;
    function Create_CTSInfo(R, G, B, Tolerance: Integer): Pointer; overload;
    function Create_CTSInfoArray(color, tolerance: array of integer): TCTSInfoArray;
    function Create_CTSInfo2DArray(w, h: integer; data: TPRGB32Array; Tolerance: Integer): TCTSInfo2DArray;

    constructor Create(aClient: TObject);
    destructor Destroy; override;
  end;

implementation
uses
  Client,           // For the Client casting.
  math,             // min/max
  tpa,              //TPABounds
  dtmutil;


var
  Percentage : array[0..255] of Extended;

{ Colour Same functions }
function ColorSame_ctsNo(ctsInfo: Pointer; C2: PRGB32): boolean;
var
    C1: TCTSNoInfo;
begin
  C1 := PCTSNoInfo(ctsInfo)^;
  Result := (C1.B = C2^.B)
        and (C1.G = C2^.G)
        and (C1.R = C2^.R);
end;

function ColorSame_cts0(ctsInfo: Pointer; C2: PRGB32): boolean;

var
    C1: TCTS0Info;
begin
  C1 := PCTS0Info(ctsInfo)^;
  Result := (Abs(C1.B - C2^.B) <= C1.Tol)
        and (Abs(C1.G - C2^.G) <= C1.Tol)
        and (Abs(C1.R - C2^.R) <= C1.Tol);
end;

function ColorSame_cts1(ctsInfo: Pointer; C2: PRGB32): boolean;

var
    C1: TCTS1Info;
    r,g,b: integer;
begin
  C1 := PCTS1Info(ctsInfo)^;
  b := C1.B - C2^.B;
  g := C1.G - C2^.G;
  r := C1.R - C2^.R;
  Result := (b*b + g*g + r*r) <= C1.Tol;
end;

function ColorSame_cts2(ctsInfo: Pointer; C2: PRGB32): boolean;

var
    r,g ,b: extended;
    CMin, CMax,D : extended;
    h,s,l : extended;
    i: TCTS2Info;
begin
  i := PCTS2Info(ctsInfo)^;

  R := Percentage[C2^.r];
  G := Percentage[C2^.g];
  B := Percentage[C2^.b];

  CMin := R;
  CMax := R;
  if G  < Cmin then CMin := G;
  if B  < Cmin then CMin := B;
  if G  > Cmax then CMax := G;
  if B  > Cmax then CMax := B;
  l := 0.5 * (Cmax + Cmin);
  //The L-value is already calculated, lets see if the current point meats the requirements!
  if abs(l*100 - i.L) > i.Tol then
    exit(false);
  if Cmax = Cmin then
  begin
    //S and H are both zero, lets check if it mathces the tol
    if (i.H <= (i.hueMod)) and
       (i.S <= (i.satMod)) then
      exit(true)
    else
      exit(false);
  end;
  D := Cmax - Cmin;
  if l < 0.5 then
    s := D / (Cmax + Cmin)
  else
    s := D / (2 - Cmax - Cmin);
  // We've Calculated the S, check match
  if abs(S*100 - i.S) > i.satMod then
    exit(false);
  if R = Cmax then
    h := (G - B) / D
  else
    if G = Cmax then
      h  := 2 + (B - R) / D
    else
      h := 4 +  (R - G) / D;
  h := h / 6;
  if h < 0 then
    h := h + 1;
  //Finally lets test H2
  if abs(H*100 - i.H) > i.hueMod then
    result := false
  else
    result := true;
end;

function ColorSame_cts3(ctsInfo: Pointer; C2: PRGB32): boolean;

var
    i: TCTS3Info;
    r, g, b : extended;
    x, y, z, L, A, bb: Extended;
begin
  i := PCTS3Info(ctsInfo)^;
  { RGBToXYZ(C2^.R, C2^.G, C2^.B, X, Y, Z); }
  { XYZToCIELab(X, Y, Z, L, A, B); }
  R := Percentage[C2^.r];
  G := Percentage[C2^.g];
  B := Percentage[C2^.b];
  if r > 0.04045  then
    r := Power( ( r + 0.055 ) / 1.055  , 2.4)
  else
    r := r / 12.92;
  if g > 0.04045  then
    g := Power( ( g + 0.055 ) / 1.055 , 2.4)
  else
    g := g / 12.92;
  if  b > 0.04045 then
    b := Power(  ( b + 0.055 ) / 1.055  , 2.4)
  else
    b := b / 12.92;

  y := (r * 0.2126 + g * 0.7152 + b * 0.0722)/100.000;
  if ( Y > 0.008856 ) then
    Y := Power(Y, 1.0/3.0)
  else
    Y := ( 7.787 * Y ) + ( 16.0 / 116.0 );

  x := (r * 0.4124 + g * 0.3576 + b * 0.1805)/95.047;
  if ( X > 0.008856 ) then
    X := Power(X, 1.0/3.0)
  else
    X := ( 7.787 * X ) + ( 16.0 / 116.0 );

  z := (r * 0.0193 + g * 0.1192 + b * 0.9505)/108.883;
  if ( Z > 0.008856 ) then
    Z := Power(Z, 1.0/3.0)
  else
    Z := ( 7.787 * Z ) + ( 16.0 / 116.0 );

  l := (116.0 * Y ) - 16.0;
  a := 500.0 * ( X - Y );
  bb := 200.0 * ( Y - Z );

  L := L - i.L;
  A := A - i.A;
  Bb := Bb - i.B;

  // XXX: optimise this
  // values < 1 do not increase if multiplied by themself, obviously
  L := L * 100;
  A := A * 100;
  bB := bB * 100;

  Result := (L*L + A*A + bB*Bb) < i.Tol;
end;

{ }

function Create_CTSInfo_helper(cts: integer; Color, Tol: Integer;
                        hueMod, satMod, CTS3Modifier: extended): Pointer; overload;
var
    R, G, B: Integer;
    X, Y, Z: Extended;
begin
  case cts of
      -1:
      begin
        Result :=  AllocMem(SizeOf(TCTSNoInfo));
        ColorToRGB(Color, PCTSNoInfo(Result)^.R, PCTSNoInfo(Result)^.G,
                    PCTSNoInfo(Result)^.B);
      end;
      0:
      begin
        Result := AllocMem(SizeOf(TCTS0Info));
        ColorToRGB(Color, PCTS0Info(Result)^.R, PCTS0Info(Result)^.G,
                    PCTS0Info(Result)^.B);
        PCTS0Info(Result)^.Tol := Tol;
      end;
      1:
      begin
        Result := AllocMem(SizeOf(TCTS1Info));
        ColorToRGB(Color, PCTS1Info(Result)^.R, PCTS1Info(Result)^.G,
                    PCTS1Info(Result)^.B);

        PCTS1Info(Result)^.Tol := Tol * Tol;
      end;
      2:
      begin
        Result := AllocMem(SizeOf(TCTS2Info));
        ColorToRGB(Color, R, G, B);
        RGBToHSL(R, G, B, PCTS2Info(Result)^.H, PCTS2Info(Result)^.S,
                    PCTS2Info(Result)^.L);
        PCTS2Info(Result)^.hueMod := Tol * hueMod;
        PCTS2Info(Result)^.satMod := Tol * satMod;
        PCTS2Info(Result)^.Tol := Tol;
      end;
      3:
      begin
        Result := AllocMem(SizeOf(TCTS3Info));
        ColorToRGB(Color, R, G, B);
        RGBToXYZ(R, G, B, X, Y, Z);
        XYZToCIELab(X, Y, Z, PCTS3Info(Result)^.L, PCTS3Info(Result)^.A,
                  PCTS3Info(Result)^.B);
        { XXX: TODO: Make all Tolerance extended }
        PCTS3Info(Result)^.Tol := Round(Tol*Tol*CTS3Modifier);
      end;
  end;
end;

function Create_CTSInfo_helper(cts: integer; R, G, B, Tol: Integer;
                        hueMod, satMod, CTS3Modifier: extended): Pointer; overload;

var Color: Integer;

begin
  Color := RGBToColor(R, G, B);
  Result := Create_CTSInfo_helper(cts, Color, Tol, hueMod, satMod, CTS3Modifier);
end;

procedure Free_CTSInfo(i: Pointer);
begin
  if assigned(i) then
      FreeMem(i)
  else
      raise Exception.Create('Free_CTSInfo: Invalid TCTSInfo passed');
end;


{ TODO: Not universal, mainly for DTM }
function Create_CTSInfoArray_helper(cts: integer; color, tolerance: array of integer;
    hueMod, satMod, CTS3Modifier: extended): TCTSInfoArray;

var
   i: integer;
begin
  if length(color) <> length(tolerance) then
    raise Exception.Create('Create_CTSInfoArray: Length(Color) <>'
                          +' Length(Tolerance');
  SetLength(Result, Length(color));

  for i := High(result) downto 0 do
    result[i] := Create_CTSInfo_helper(cts, color[i], tolerance[i], hueMod, satMod,
        CTS3Modifier);
end;


{ TODO: Not universal, mainly for Bitmap }
function Create_CTSInfo2DArray_helper(cts, w, h: integer; data: TPRGB32Array;
    Tolerance: Integer; hueMod, satMod, CTS3Modifier: Extended): TCTSInfo2DArray;
var
   x, y: integer;
begin
  SetLength(Result,h+1,w+1);

  for y := 0 to h do
    for x := 0 to w do
      Result[y][x] := Create_CTSInfo_helper(cts,
          data[y][x].R, data[y][x].G, data[y][x].B,
          Tolerance, hueMod, satMod, CTS3Modifier);
end;

procedure Free_CTSInfoArray(i: TCTSInfoArray);
var
   c: integer;
begin
  for c := high(i) downto 0 do
    Free_CTSInfo(i[c]);
  SetLength(i, 0);
end;

procedure Free_CTSInfo2DArray(i: TCTSInfo2DArray);
var
   x, y: integer;
begin
  for y := high(i) downto 0 do
    for x := high(i[y]) downto 0 do
      Free_CTSInfo(i[y][x]);
  SetLength(i, 0);
end;

function Get_CTSCompare(cts: Integer): TCTSCompareFunction;

begin
  case cts of
      -1: Result := @ColorSame_ctsNo;
      0: Result := @ColorSame_cts0;
      1: Result := @ColorSame_cts1;
      2: Result := @ColorSame_cts2;
      3: Result := @ColorSame_cts3;
  end;
end;


procedure TMFinder.LoadSpiralPath(startX, startY, x1, y1, x2, y2: Integer);
var
  i,c,Ring : integer;
  CurrBox : TBox;
begin
  i := 0;
  Ring := 1;
  c := 0;
  CurrBox.x1 := Startx-1;
  CurrBox.y1 := Starty-1;
  CurrBox.x2 := Startx+1;
  CurrBox.y2 := Starty+1;
  if (startx >= x1) and (startx <= x2) and (starty >= y1) and (starty <= y2) then
  begin;
    ClientTPA[c] := Point(Startx, StartY);
    Inc(c);
  end;
  repeat
    if (CurrBox.x2 >= x1) and (CurrBox.x1 <= x2) and (Currbox.y1 >= y1) and (Currbox.y1 <= y2)  then
      for i := CurrBox.x1 + 1 to CurrBox.x2 do
        if (I >= x1) and ( I <= x2) then
        begin;
          ClientTPA[c] := Point(i,CurrBox.y1);
          Inc(c);
        end;
    if (CurrBox.x2 >= x1) and (CurrBox.x2 <= x2) and (Currbox.y2 >= y1) and (Currbox.y1 <= y2)  then
      for i := CurrBox.y1 + 1 to CurrBox.y2 do
        if (I >= y1) and ( I <= y2) then
        begin;
          ClientTPA[c] := Point(Currbox.x2, I);
          Inc(c);
        end;
    if (CurrBox.x2 >= x1) and (CurrBox.x1 <= x2) and (Currbox.y2 >= y1) and (Currbox.y2 <= y2)  then
      for i := CurrBox.x2 - 1 downto CurrBox.x1 do
        if (I >= x1) and ( I <= x2) then
        begin;
          ClientTPA[c] := Point(i,CurrBox.y2);
          Inc(c);
        end;
    if (CurrBox.x1 >= x1) and (CurrBox.x1 <= x2) and (Currbox.y2 >= y1) and (Currbox.y1 <= y2)  then
      for i := CurrBox.y2 - 1 downto CurrBox.y1 do
        if (I >= y1) and ( I <= y2) then
        begin;
          ClientTPA[c] := Point(Currbox.x1, I);
          Inc(c);
        end;
    Inc(ring);
    CurrBox.x1 := Startx-ring;
    CurrBox.y1 := Starty-Ring;
    CurrBox.x2 := Startx+Ring;
    CurrBox.y2 := Starty+Ring;
  until (Currbox.x1 < x1) and (Currbox.x2 > x2) and (currbox.y1 < y1)
        and (currbox.y2 > y2);
end;

function CalculateRowPtrs(ReturnData: TRetData; RowCount: integer) : TPRGB32Array; overload;
var
  I : integer;
begin;
  SetLength(result,RowCount);
  for i := 0 to RowCount - 1 do
    result[i] := ReturnData.Ptr + ReturnData.RowLen * i;
end;

function CalculateRowPtrs(Bitmap : TMufasaBitmap) : TPRGB32Array; overload;
begin
  Result := Bitmap.RowPtrs;
end;

//SkipCoords[y][x] = False/True; True means its "transparent" and therefore not needed to be checked.
procedure CalculateBitmapSkipCoords(Bitmap : TMufasaBitmap; out SkipCoords : T2DBoolArray);
var
  x,y : integer;
  R,G,B : byte;
  Ptr : PRGB32;
begin;
  r := 0;
  g := 0;
  b := 0;
  if Bitmap.TransparentColorSet then
    ColorToRGB(Bitmap.GetTransparentColor,r,g,b);
  Ptr := Bitmap.FData;
  SetLength(SkipCoords,Bitmap.Height,Bitmap.Width);
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
    begin;
      if (Ptr^.r = r) and (Ptr^.g = g) and (Ptr^.b = b) then
        SkipCoords[y][x] := True
      else
        SkipCoords[y][x] := false;
      inc(ptr);
    end;
end;
{ Points left holds the amount of points that are "left" to be checked
   (Including the point itself.. So for example Pointsleft[0][0] would
    hold the total amount of pixels that are to be checked. }
procedure CalculateBitmapSkipCoordsEx(Bitmap : TMufasaBitmap; out SkipCoords : T2DBoolArray;out TotalPoints : integer; out PointsLeft : T2DIntArray);
var
  x,y : integer;
  R,G,B : byte;
  Ptr : PRGB32;
  TotalC : integer;
begin;
  r := 0;
  g := 0;
  b := 0;
  TotalC := 0;
  if Bitmap.TransparentColorSet then
    ColorToRGB(Bitmap.GetTransparentColor,r,g,b);
  Ptr := Bitmap.FData;
  SetLength(SkipCoords,Bitmap.Height,Bitmap.Width);
  SetLength(PointsLeft,Bitmap.Height,Bitmap.Width);
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
    begin;
      if (Ptr^.r = r) and (Ptr^.g = g) and (Ptr^.b = b) then
        SkipCoords[y][x] := True
      else
      begin;
        SkipCoords[y][x] := false;
        inc(TotalC);
      end;
      inc(ptr);
    end;
  TotalPoints:= TotalC;
  for y := 0 to Bitmap.Height - 1 do
    for x := 0 to Bitmap.Width - 1 do
    begin;
      PointsLeft[y][x] := TotalC;
      if not SkipCoords[y][x] then
        Dec(TotalC);
    end;
end;

{ Initialise the variables for TMFinder }
constructor TMFinder.Create(aClient: TObject);
var
  I : integer;

begin
  inherited Create;

  WarnOnly := False;
  Self.Client := aClient;
  Self.CTS := 1;
  Self.hueMod := 0.2;
  Self.satMod := 0.2;
  if (Percentage[255] <> 1) then
    for i := 0 to 255 do
      Percentage[i] := i / 255;

end;

destructor TMFinder.Destroy;
begin
    {   We don't really have to free stuff here.
        The array is managed, so that is automatically freed.
        The rest is either references to objects we may not destroy
    }

  inherited;
end;

procedure TMFinder.SetToleranceSpeed(nCTS: Integer);
begin
  if (nCTS < 0) or (nCTS > 3) then
    raise Exception.CreateFmt('The given CTS ([%d]) is invalid.',[nCTS]);
  Self.CTS := nCTS;
end;

function TMFinder.GetToleranceSpeed: Integer;
begin
  Result := Self.CTS;
end;

procedure TMFinder.SetToleranceSpeed2Modifiers(const nHue, nSat: Extended);
begin
  Self.hueMod := nHue;
  Self.satMod := nSat;
end;

procedure TMFinder.GetToleranceSpeed2Modifiers(out hMod, sMod: Extended);
begin
  hMod := Self.hueMod;
  sMod := Self.satMod;
end;

procedure TMFinder.SetToleranceSpeed3Modifier(modifier: Extended);
begin
  CTS3Modifier := modifier;
end;

function TMFinder.GetToleranceSpeed3Modifier: Extended;
begin
  Result := CTS3Modifier;
end;

function TMFinder.Create_CTSInfo(Color, Tolerance: Integer): Pointer; overload;
begin
  Result := Create_CTSInfo_helper(Self.cts, Color, Tolerance, Self.hueMod,
              Self.satMod, Self.CTS3Modifier);
end;

function TMFinder.Create_CTSInfo(R, G, B, Tolerance: Integer): Pointer; overload;
begin
  Result := Create_CTSInfo_helper(Self.cts, R, G, B, Tolerance, Self.hueMod,
              Self.satMod, Self.CTS3Modifier);
end;

function TMFinder.Create_CTSInfoArray(color, tolerance: array of integer): TCTSInfoArray;
begin
  Result := Create_CTSInfoArray_helper(Self.cts, color, tolerance, Self.hueMod,
      Self.satMod, Self.CTS3Modifier);
end;

function TMFinder.Create_CTSInfo2DArray(w, h: integer; data: TPRGB32Array;
    Tolerance: Integer): TCTSInfo2DArray;
begin
  Result := Create_CTSInfo2DArray_helper(Self.cts, w, h, data, tolerance, Self.hueMod,
              Self.satMod, Self.CTS3Modifier);
end;

procedure TMFinder.UpdateCachedValues(NewWidth, NewHeight: integer);
begin
  CachedWidth := NewWidth;
  CachedHeight := NewHeight;
  SetLength(ClientTPA,NewWidth * NewHeight);
end;

procedure Swap(var A,B : integer);
var
  c : integer;
begin
  c := a;
  a := b;
  b := c;
end;

procedure TMFinder.DefaultOperations(var xs, ys, xe, ye: integer);
var
  w,h : integer;
begin
  if (xs > xe) then
    if WarnOnly then
    begin
      TClient(Client).WriteLn(Format('Warning! You passed wrong values to a finder function: xs > xe (%d,%d). Swapping the values for now.',[xs,xe]));
      swap(xs,xe);
    end else
      raise Exception.CreateFMT('You passed wrong values to a finder function: xs > xe (%d,%d).',[xs,xe]);
  if ys > ye then
    if WarnOnly then
    begin
      TClient(Client).WriteLn(Format('Warning! You passed wrong values to a finder function: ys > ye (%d,%d). Swapping the values for now.',[ys,ye]));
      swap(ys,ye);
    end else
      raise Exception.CreateFMT('You passed wrong values to a finder function: ys > ye (%d,%d).',[ys,ye]);
  if xs < 0 then
    if WarnOnly then
    begin
      TClient(Client).WriteLn(Format('Warning! You passed a wrong xs to a finder function: %d. That is below 0, thus out of bounds. Setting the value to 0 for now.',[xs]));
      xs := 0;
    end else
      raise Exception.createFMT('You passed a wrong xs to a finder function: %d. That is below 0, thus out of bounds.',[xs]);
  if ys < 0 then
    if WarnOnly then
    begin
      TClient(Client).WriteLn(Format('Warning! You passed a wrong ys to a finder function: %d. That is below 0, thus out of bounds. Setting the value to 0 for now.',[ys]));
      ys := 0;
    end else
      raise Exception.createFMT('You passed a wrong ys to a finder function: %d. That is below 0, thus out of bounds.',[ys]);
  TClient(Self.Client).IOManager.GetDimensions(w,h);
  if (w <> CachedWidth) or (h <> CachedHeight) then
    UpdateCachedValues(w,h);
  if xe >= w then
    if WarnOnly then
    begin
      TClient(Client).WriteLn(Format('Warning! You passed a wrong xe to a finder function: %d. The client has a width of %d, thus the xe is out of bounds. Setting the value to %d (w-1) for now.',[xe,w,w-1]));
      xe := w-1;
    end else
      raise Exception.createFMT('You passed a wrong xe to a finder function: %d. The client has a width of %d, thus the xe is out of bounds.',[xe,w]);
  if ye >= h then
    if WarnOnly then
    begin
      TClient(Client).WriteLn(Format('Warning! You passed a wrong ye to a finder function: %d. The client has a height of %d, thus the ye is out of bounds. Setting the value to %d (h-1) for now.',[ye,h,h-1]));
      ye := h-1;
    end else
      raise Exception.createFMT('You passed a wrong ye to a finder function: %d. The client has a height of %d, thus the ye is out of bounds.',[ye,h]);
end;

function TMFinder.SimilarColors(Color1, Color2, Tolerance: Integer) : boolean;
var
   compare: TCTSCompareFunction;
   ctsinfo: TCTSInfo;
   Col2: TRGB32;

begin
  ctsinfo := Create_CTSInfo(Color1, Tolerance);
  compare := Get_CTSCompare(Self.CTS);
  ColorToRGB(Color2, Col2.R, Col2.G, Col2.B);

  Result := compare(ctsinfo, @Col2);

  Free_CTSInfo(ctsinfo);
end;

function TMFinder.CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, xx, yy: Integer;

  compare: TCTSCompareFunction;
  ctsinfo: TCTSInfo;
begin
  Result := 0;
  DefaultOperations(xs, ys, xe, ye);

  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;
  result := 0;

  ctsinfo := Create_CTSInfo(Color, Tolerance);
  compare := Get_CTSCompare(Self.CTS);

  for yy := ys to ye do
  begin;
    for xx := xs to xe do
    begin
      if compare(ctsinfo, Ptr) then
        inc(result);
      Inc(Ptr);
    end;
    Inc(Ptr, PtrInc)
  end;

  Free_CTSInfo(ctsinfo);
  TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.CountColor(Color, xs, ys, xe, ye: Integer): Integer;
var
  temp : integer;
begin
   temp := Self.CTS;
   Self.CTS := -1;
   try
       Result := CountColorTolerance(color,xs,ys,xe,ye,0);
   finally
       Self.CTS := temp;
   end;
end;

function TMFinder.FindColor(out x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
var
  temp : integer;
begin
   temp := Self.CTS;
   Self.CTS := -1;
   try
       result := FindColorTolerance(x,y,color,xs,ys,xe,ye,0);
   finally
       Self.CTS := temp;
   end;
end;

function TMFinder.FindColorSpiral(var x, y: Integer; color, xs, ys, xe,
  ye: Integer): Boolean;
var
  temp : integer;
begin
   temp := Self.CTS;
   Self.CTS := -1;
   try
       result := FindColorSpiralTolerance(x,y,color,xs,ys,xe,ye,0);
   finally
       Self.CTS := temp;
   end;
end;

function TMFinder.FindColorSpiralTolerance(var x, y: Integer; color, xs, ys,
  xe, ye, Tol: Integer): Boolean;
var
   PtrData: TRetData;
   RowData : TPRGB32Array;
   dX, dY, clR, clG, clB,i,Hispiral: Integer;

var
   j: integer;
   compare: TCTSCompareFunction;
   ctsinfo: TCTSInfo;

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);
  //Load rowdata
  RowData:= CalculateRowPtrs(ptrdata,dy+1);
  //Load the spiral path
  LoadSpiralPath(x-xs,y-ys,0,0,dx,dy);
  HiSpiral := (dy+1) * (dx + 1) -1;

  ctsinfo := Create_CTSInfo(Color, Tol);
  compare := Get_CTSCompare(Self.CTS);

  i := -1;
  for j := 0 to HiSpiral do
  begin
    if compare(ctsinfo, @RowData[ClientTPA[j].y][ClientTPA[j].x]) then
    begin
      i := j;
      break;
    end;
  end;

  Free_CTSInfo(ctsinfo);

  if i = -1 then
  begin
    Result := False;
    TClient(Client).IOManager.FreeReturnData;
    Exit;
  end else
  begin
    Result := True;
    x := ClientTPA[i].x + xs;
    y := ClientTPA[i].y + ys;
    TClient(Client).IOManager.FreeReturnData;
  end;
end;

function TMFinder.FindColoredArea(var x, y: Integer; Color, xs, ys, xe, ye, MinArea: Integer): Boolean;
var
  temp : integer;
begin
   temp := Self.CTS;
   Self.CTS := -1;
   try
       result := FindColoredAreaTolerance(x,y,color,xs,ys,xe,ye,MinArea,0);
   finally
       Self.CTS := temp;
   end;
end;

function TMFinder.FindColorTolerance(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY: Integer;
   xx, yy: integer;
   compare: TCTSCompareFunction;
   ctsinfo: TCTSInfo;
   label Hit;


begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  ctsinfo := Create_CTSInfo(Color, Tol);
  compare := Get_CTSCompare(Self.CTS);

  for yy := ys to ye do
  begin
    for xx := xs to xe do
    begin
      if compare(ctsinfo, Ptr) then
        goto Hit;
      inc(Ptr);
    end;
    Inc(Ptr, PtrInc);
  end;

  Result := False;
  Free_CTSInfo(ctsinfo);
  TClient(Client).IOManager.FreeReturnData;
  Exit;

  Hit:
    Result := True;
    x := xx;
    y := yy;
    Free_CTSInfo(ctsinfo);
    TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.FindColoredAreaTolerance(var x, y: Integer; Color, xs, ys, xe, ye, MinArea, tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr, Before: PRGB32;
   PtrInc: Integer;
   dX, dY, xx, yy, fx, fy, Count: Integer;
   clR, clG, clB : Byte;
   H1, S1, L1: Extended;
   NotFound : Boolean;

   compare: TCTSCompareFunction;
   ctsinfo: TCTSInfo;

   label Hit;


begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  if Cts = 2 then
    RGBToHSL(clR,clG,clB,H1,S1,L1);
  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;
  Count := 0;

  ctsinfo := Create_CTSInfo(Color, Tol);
  compare := Get_CTSCompare(Self.CTS);

  for yy := ys to ye do
  begin;
    for xx := xs to xe do
    begin;
      NotFound := False;
      // Colour comparison here.
      if compare(ctsinfo, Ptr) then
      begin
        Before := Ptr;
        for fy := yy to ye do
        begin
          for fx := xx to xe do
          begin
            Inc(Ptr);
            if not compare(ctsinfo, Ptr) then
            begin
              NotFound := True;
              Break;
            end;
            Inc(Count);
            if Count >= MinArea then
              goto Hit;
          end;

          if NotFound then
          begin
            Ptr := Before;
            Break;
          end;
          Inc(Ptr, PtrInc);
        end;
      end;
      Inc(Ptr);
    end;
    Inc(Ptr, PtrInc);
  end;

  Result := False;
  Free_CTSInfo(ctsinfo);
  TClient(Client).IOManager.FreeReturnData;
  Exit;

  Hit:
    Result := True;
    x := xx;
    y := yy;
    Free_CTSInfo(ctsinfo);
    TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.FindColorsTolerance(out Points: TPointArray; Color, xs, ys,
  xe, ye, Tol: Integer): Boolean;
var
  PtrData: TRetData;
  Ptr: PRGB32;
  PtrInc,C: Integer;
  dX, dY: Integer;

  xx, yy: integer;
  compare: TCTSCompareFunction;
  ctsinfo: TCTSInfo;

begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;
  c := 0;

  ctsinfo := Create_CTSInfo(Color, Tol);
  compare := Get_CTSCompare(Self.CTS);

  for yy := ys to ye do
  begin
    for xx := xs to xe do
    begin
      if compare(ctsinfo, Ptr) then
      begin
        ClientTPA[c].x := xx;
        ClientTPA[c].y := yy;
        inc(c);
      end;
      inc(Ptr);
    end;
    Inc(Ptr, PtrInc);
  end;

  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;

  Free_CTSInfo(ctsinfo);
  TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.FindColorsSpiralTolerance(x, y: Integer;
  out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tol: Integer
  ): boolean;
var
  PtrData: TRetData;
  c : integer;
  RowData : TPRGB32Array;
  dX, dY, SpiralHi, i: Integer;

  compare: TCTSCompareFunction;
  ctsinfo: TCTSInfo;

begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);

  c := 0;

  ctsinfo := Create_CTSInfo(Color, Tol);
  compare := Get_CTSCompare(Self.CTS);

  //Load rowdata
  RowData:= CalculateRowPtrs(ptrdata,dy+1);
  //Load the spiral path
  LoadSpiralPath(x-xs,y-ys,0,0,dx,dy); { Fills ClientTPA with Spiral path }

  SpiralHi := (dx + 1) * (dy + 1) - 1;
  for i := 0 to SpiralHi do
    if compare(ctsinfo, @RowData[ClientTPA[i].y][ClientTPA[i].x]) then
    begin;
      { We can re-use the ClientTPA to store results. }
      ClientTPA[c].x := ClientTPA[i].x + xs;
      ClientTPA[c].y := ClientTPA[i].y + ys;
      inc(c);
    end;

  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;

  Free_CTSInfo(ctsinfo);
  TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
var
  temp : integer;
begin
   temp := Self.CTS;
   Self.CTS := -1;
   try
       result := FindColorsTolerance(TPA,color,xs,ys,xe,ye,0);
   finally
       Self.CTS := temp;
   end;
end;

 { Only works with CTS 1 for now.. Since Colorsame doesn't return a boolean :-( }
//We do not check whether every white pixel is in tol range with every other white pixel..

function TMFinder.FindMaskTolerance(const mask: TMask; out x, y: Integer; xs,
  ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   PtrData : TRetData;
   MaskW,MaskH : integer;
   CheckerWhite,CheckerBlack,CurrWhite,CurrBlack: TRGB32;
   i,ii : integer;
   dX, dY,  xx, yy: Integer;
label NotFoundMask;
   { Don't know if the compiler has any speed-troubles with goto jumping in nested for loops. }

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);
  //Check the mask.
  CheckMask(Mask);
  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);
  //Caculate the row ptrs
  MainRowdata:= CalculateRowPtrs(PtrData,dy+1);

  //Get the 'fixed' mask size
  MaskW := Mask.W;
  MaskH := Mask.H;
  //Heck our mask cannot be outside the search area
  dX := dX - MaskW;
  dY := dY - MaskH;
  for yy := 0 to dY do
    for xx := 0 to dX do
    begin;
      CheckerWhite := MainRowdata[yy + mask.White[0].y][xx + mask.white[0].x];
      CheckerBlack := MainRowdata[yy + mask.Black[0].y][xx + mask.Black[0].x];
      //Just check two 'random' points against eachother, might be a time saver in some circumstances.
      if (Sqrt(sqr(CheckerWhite.r-CheckerBlack.r) + sqr(CheckerWhite.G-CheckerBlack.G) + sqr(CheckerWhite.b-CheckerBlack.B))
          <= ContourTolerance) then //The Tol between the white and black is lower than the minimum difference, so continue with looking!
        continue;
      for i := 0 to mask.WhiteHi do
      begin;
        CurrWhite := MainRowdata[yy + mask.White[i].y][xx + mask.white[i].x];
        if (Sqrt(sqr(CheckerWhite.r-CurrWhite.r) + sqr(CheckerWhite.G-CurrWhite.G) + sqr(CheckerWhite.b-CurrWhite.B))
            > Tolerance) then //The white checkpoint n' this point aren't in the same tol range -> goto nomatch;
          goto NotFoundMask;
        {$ifdef CheckAllBackground}
        for ii := 0 to mask.BlackHi do
        begin
          CurrBlack := MainRowdata[yy + mask.Black[ii].y][xx + mask.Black[ii].x];
          if (Sqrt(sqr(CurrWhite.r-CurrBlack.r) + sqr(CurrWhite.G-CurrBlack.G) + sqr(CurrWhite.b-CurrBlack.B))
                    <= ContourTolerance) then //The Tol between the white and black is lower than the minimum difference -> goto nomatch;
              goto NotFoundMask;
        end;
        {$endif}
      end;
      {$ifndef CheckAllBackground}
      for ii := 0 to mask.BlackHi do
      begin
        CurrBlack := MainRowdata[yy + mask.Black[ii].y][xx + mask.Black[ii].x];
        if (Sqrt(sqr(CheckerWhite.r-CurrBlack.r) + sqr(CheckerWhite.G-CurrBlack.G) + sqr(CheckerWhite.b-CurrBlack.B))
                  <= ContourTolerance) then //The Tol between the white and black is lower than the minimum difference -> goto nomatch;
            goto NotFoundMask;
      end;
      {$endif}
      //We have found the mask appearntly, otherwise we would have jumped! Gna Gna.
      x := xx + xs;
      y := yy + ys;
      TClient(Client).IOManager.FreeReturnData;
      Exit(true);
      //Bah not found the mask, lets do nothing and continue!
      NotFoundMask:
    end;
  TClient(Client).IOManager.FreeReturnData;
end;

procedure TMFinder.CheckMask(const Mask: TMask);
begin
  if (Mask.W < 1) or (Mask.H < 1) or (Mask.WhiteHi < 0) or (Mask.BlackHi < 0) then
    raise exception.CreateFMT('Mask is invalid. Width/Height: (%d,%d). WhiteHi/BlackHi: (%d,%d)',[Mask.W,Mask.H,Mask.WhiteHi,Mask.BlackHi]);
end;

function TMFinder.FindBitmap(bitmap: TMufasaBitmap; out x, y: Integer): Boolean;
var
  w,h : integer;
begin
  TClient(Client).IOManager.GetDimensions(w,h);
  result := Self.FindBitmapIn(bitmap,x,y,0,0,w-1,h-1);
end;

function TMFinder.FindBitmapIn(bitmap: TMufasaBitmap; out x, y: Integer; xs,
  ys, xe, ye: Integer): Boolean;

var
  temp: integer;

begin
  temp := Self.CTS;
  Self.CTS := -1;
  try
      Result := FindBitmapToleranceIn(bitmap, x, y, xs, ys, xe, ye, 0);
  finally
      Self.CTS := temp;
  end;
end;

function TMFinder.FindBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs,
  ys, xe, ye: Integer; tolerance: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  xx, yy: Integer;
   SkipCoords : T2DBoolArray;

   ctsinfoarray: TCTSInfo2DArray;
   compare: TCTSCompareFunction;

label NotFoundBmp;
  { Don't know if the compiler has any speed-troubles with goto jumping in nested for loops. }

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);
  //Caculate the row ptrs
  MainRowdata:= CalculateRowPtrs(PtrData,dy+1);
  BmpRowData:= CalculateRowPtrs(bitmap);
  //Get the 'fixed' bmp size
  BmpW := bitmap.Width - 1;
  BmpH := bitmap.Height - 1;
  //Heck our bitmap cannot be outside the search area
  dX := dX - bmpW;
  dY := dY - bmpH;

  ctsinfoarray := Create_CTSInfo2DArray(bmpW, bmpH, BmpRowData, Tolerance);
  compare := Get_CTSCompare(Self.CTS);

  //Get the "skip coords".
  CalculateBitmapSkipCoords(Bitmap,SkipCoords);
  for yy := 0 to dY do
    for xx := 0 to dX do
    begin;
      for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + yy;
        for xBmp := 0 to BmpW do
          if not SkipCoords[yBmp][xBmp] then
            if not compare(ctsinfoarray[yBmp][xBmp],
                           @MainRowData[tmpY][xBmp + xx]) then
               goto NotFoundBmp;
      end;

      //We did find the Bmp, otherwise we would be at the part below

      Free_CTSInfo2DArray(ctsinfoarray);
      TClient(Client).IOManager.FreeReturnData;

      x := xx + xs;
      y := yy + ys;
      result := true;
      Exit;
      NotFoundBmp:
    end;

  Free_CTSInfo2DArray(ctsinfoarray);
  TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer;
  xs, ys, xe, ye: Integer): Boolean;

var
  temp: integer;

begin
  temp := Self.CTS;
  Self.CTS := -1;
  try
      Result := FindBitmapSpiralTolerance(bitmap, x, y, xs, ys, xe, ye, 0);
  finally
      Self.CTS := temp;
  end;
end;

function TMFinder.FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x,
  y: Integer; xs, ys, xe, ye, tolerance: integer): Boolean;
var
  p: TPointArray;

begin
    Result := FindBitmapsSpiralTolerance(bitmap, x, y, p, xs, ys, xe, ye, tolerance, 1);
    if Result then
    begin
        x := p[0].x;
        y := p[0].y;
    end;
end;

function TMFinder.FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x,
  y: Integer; out Points: TPointArray; xs, ys, xe, ye, tolerance, maxToFind: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  i,HiSpiral: Integer;
   FoundC : integer;
   SkipCoords : T2DBoolArray;

   ctsinfoarray: TCTSInfo2DArray;
   compare: TCTSCompareFunction;

label theEnd;
label NotFoundBmp;
   { Don't know if the compiler has any speed-troubles with goto jumping in nested for loops. }

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);
  //Caculate the row ptrs
  MainRowdata:= CalculateRowPtrs(PtrData,dy+1);
  BmpRowData:= CalculateRowPtrs(bitmap);
  //Get the 'fixed' bmp size
  BmpW := bitmap.Width - 1;
  BmpH := bitmap.Height - 1;
  //Heck, our bitmap cannot be outside the search area
  dX := dX - bmpW;
  dY := dY - bmpH;
  //Load the spiral into memory
  LoadSpiralPath(x-xs,y-ys,0,0,dX,dY);
  HiSpiral := (dx+1) * (dy+1) - 1;
  FoundC := 0;

  ctsinfoarray := Create_CTSInfo2DArray(bmpW, bmpH, BmpRowData, Tolerance);
  compare := Get_CTSCompare(Self.CTS);

  //Get the "skip coords".
  CalculateBitmapSkipCoords(Bitmap,SkipCoords);
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
          if not SkipCoords[yBmp][xBmp] then
            if not compare(ctsinfoarray[yBmp][xBmp],
                           @MainRowData[tmpY][xBmp + ClientTPA[i].x]) then
              goto NotFoundBmp;

    end;
    //We did find the Bmp, otherwise we would be at the part below
    ClientTPA[FoundC].x := ClientTPA[i].x + xs;
    ClientTPA[FoundC].y := ClientTPA[i].y + ys;
    inc(FoundC);
    if FoundC = maxToFind then
        goto TheEnd;

    NotFoundBmp:
  end;

  TheEnd:
  if FoundC > 0 then
  begin;
    result := true;
    SetLength(Points,FoundC);
    Move(ClientTPA[0], Points[0], FoundC * SizeOf(TPoint));
  end;

  Free_CTSInfo2DArray(ctsinfoarray);
  TClient(Client).IOManager.FreeReturnData;
end;

function TMFinder.FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; out x,
  y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer;
  AllowPartialAccuracy: Boolean; out accuracy: Extended): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   dX, dY,  xx, yy: Integer;
   SearchdX,SearchdY : integer;
   GoodCount : integer;//Save the amount of pixels who have found a correspondening pixel
   BestCount : integer;//The best amount of pixels till now..
   BestPT : TPoint; //The point where it found the most pixels.
   RangeX,RangeY : Integer;
   yStart,yEnd,xStart,xEnd : integer;
   TotalC : integer;
   SkipCoords : T2DBoolArray;
   PointsLeft : T2DIntArray;

   ctsinfoarray: TCTSInfo2DArray;
   compare: TCTSCompareFunction;

label FoundBMPPoint, Madness;
  { Don't know if the compiler has any speed-troubles with goto jumping in nested for loops. }

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;
  SearchDx := dX;
  SearchDy := dY;
  PtrData := TClient(Client).IOManager.ReturnData(xs, ys, dX + 1, dY + 1);
  //Caculate the row ptrs
  MainRowdata:= CalculateRowPtrs(PtrData,dy+1);
  BmpRowData:= CalculateRowPtrs(bitmap);
  //Get the 'fixed' bmp size
  BmpW := bitmap.Width - 1;
  BmpH := bitmap.Height - 1;
  //Heck our bitmap cannot be outside the search area
  dX := dX - bmpW;
  dY := dY - bmpH;
  //Reset the accuracy :-)
  Accuracy := 0;
  BestCount := -1;
  BestPT := Point(-1,-1);

  ctsinfoarray := Create_CTSInfo2DArray(bmpW, bmpH, BmpRowData, Tolerance);
  compare := Get_CTSCompare(Self.CTS);

  //Get the "skip coords". and PointsLeft (so we can calc whether we should stop searching or not ;-).
  CalculateBitmapSkipCoordsEx(Bitmap,SkipCoords,TotalC,PointsLeft);

  for yy := 0 to dY do
    for xx := 0 to dX do
    begin;
      GoodCount := 0;
      for yBmp:= 0 to BmpH do
      begin;
        for xBmp := 0 to BmpW do
        begin;
          //We do not have to check this point, win win win <--- triple win <-- JACKPOT!
          if SkipCoords[yBmp][xBmp] then
            Continue;
          //Calculate points of the BMP left against Goodcount (if it cannot possibly get more points skip this x,y?
          if bestCount > (GoodCount + PointsLeft[yBmp][xBmp]) then
            goto Madness;
          //The point on the bitmap + the the coordinate we are on at the "screen" minus the range.
          yStart := max(yBmp + yy-Range,0);
          yEnd := Min(yBmp + yy+range,SearchdY);
          for RangeY := yStart to yEnd do
          begin;
            xStart := max(xx-Range + xBmp,0);
            xEnd := Min(xx+range + xBmp,SearchdX);
            for RangeX := xStart to xEnd do
            begin;
            if not compare(ctsinfoarray[yBmp][xBmp],
                           @MainRowData[rangeY][rangeX]) then
                goto FoundBMPPoint;
            end;
          end;
          //We did not find a good point so were continueing!
          Continue;
          FoundBMPPoint:
          //We found a pooint woot!
          inc(GoodCount);
        end;
      end;
      //If we jumped to Madness it means we did not have enuf points left to beat tha fu-king score.
      Madness:
      if GoodCount > BestCount then //This x,y has the best Acc so far!
      begin;
        BestCount := GoodCount;
        BestPT := Point(xx+xs,yy+ys);
        if GoodCount = TotalC then
        begin
          Free_CTSInfo2DArray(ctsinfoarray);
          TClient(Client).IOManager.FreeReturnData;
          x := BestPT.x;
          y := BestPT.y;
          accuracy:= 1;
          Exit(true);
        end;
      end;
    end;

  Free_CTSInfo2DArray(ctsinfoarray);
  TClient(Client).IOManager.FreeReturnData;
  if BestCount = 0 then
    Exit;
  accuracy := BestCount / TotalC;
  if (accuracy = 1) or AllowPartialAccuracy then
  begin
    x := BestPT.x;
    y := BestPT.y;
    Exit(true);
  end;
end;

{
  Tries to find the given DTM. If found will put the point the dtm has
  been found at in x, y and result to true.
}

function TMFinder.FindDTM(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
var
   P: TPointArray;
begin
  Result := Self.FindDTMs(DTM, P, x1, y1, x2, y2, 1);
  if Result then
  begin
    x := p[0].x;
    y := p[0].y;
  end;
end;

//MaxToFind, if it's < 1 it won't stop looking
function TMFinder.FindDTMs(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2, maxToFind: Integer): Boolean;
var
   //Cache DTM stuff
   Len : integer;       //Len of the points
   DPoints : PMDTMPoint; //DTM Points

   // Bitwise
   b: Array of Array of Integer;
   ch: array of array of integer;

   // bounds
   W, H: integer;
   MA: TBox;
   MaxX,MaxY : integer; //The maximum value X/Y can take (for subpoints)

   // for loops, etc
   xx, yy: integer;
   i, xxx,yyy: Integer;

   StartX,StartY,EndX,EndY : integer;

   //clientdata
   cd: TPRGB32Array;

   PtrData: TRetData;

   // point count
   pc: Integer = 0;
   Found : boolean;

   goodPoints: Array of Boolean;

   col_arr, tol_arr: Array of Integer;
   ctsinfoarray: TCTSInfoArray;
   compare: TCTSCompareFunction;

   label theEnd;
   label AnotherLoopEnd;

begin
  // Is the area valid?
  DefaultOperations(x1, y1, x2, y2);
  if not DTM.Valid then
    raise Exception.CreateFmt('FindDTMs: DTM[%s] is not valid.', [DTM.name]);

  // Get the area we should search in for the Main Point.
  MA := ValidMainPointBox(DTM, x1, y1, x2, y2);
  //Load the DTM-cache variables
  Len := dtm.Count;
  DPoints:= dtm.PPoints;
  // Turn the bp into a more usable array.
  setlength(goodPoints, Len);
  for i := 0 to Len - 1 do
    goodPoints[i] := not DPoints[i].bp;

  // Init data structure b and ch.
  W := x2 - x1;
  H := y2 - y1;

  setlength(b, (W + 1));
  setlength(ch, (W + 1));
  for i := 0 to W do
  begin
    setlength(ch[i], (H + 1));
    FillChar(ch[i][0], SizeOf(Integer) * (H+1), 0);
    setlength(b[i], (H + 1));
    FillChar(b[i][0], SizeOf(Integer) * (H+1), 0);
  end;

  // Retreive Client Data.
  PtrData := TClient(Client).IOManager.ReturnData(x1, y1, W + 1, H + 1);

  SetLength(col_arr, Len);
  SetLength(tol_arr, Len);
  // C = DTM.C
  for i := 0 to Len - 1 do
  begin
    col_arr[i] := DPoints[i].c;
    tol_arr[i] := DPoints[i].t;
  end;

  ctsinfoarray := Create_CTSInfoArray(col_arr, tol_arr);
  compare := Get_CTSCompare(Self.CTS);

  cd := CalculateRowPtrs(PtrData, h + 1);
  //CD starts at 0,0.. We must adjust the MA, since this is still based on the xs,ys,xe,ye box.
  MA.x1 := MA.x1 - x1;
  MA.y1 := MA.y1 - y1;
  MA.x2 := MA.x2 - x1;
  MA.y2 := MA.y2 - y1;

  MaxX := x2-x1;
  MaxY := y2-y1;
  //MA is now fixed to the new (0,0) box...

  for yy := MA.y1  to MA.y2  do //Coord of the mainpoint in the search area
    for xx := MA.x1  to MA.x2 do
    begin
      //Mainpoint can have area size as well, so we must check that just like any subpoint.
      for i := 0 to Len - 1 do
      begin //change to use other areashapes too.
        Found := false;
        //With area it can go out of bounds, therefore this max/min check
        StartX := max(0,xx - DPoints[i].asz + DPoints[i].x);
        StartY := max(0,yy - DPoints[i].asz + DPoints[i].y);
        EndX := Min(MaxX,xx + DPoints[i].asz + DPoints[i].x);
        EndY := Min(MaxY,yy + DPoints[i].asz + DPoints[i].y);
        for xxx := StartX to EndX do //The search area for the subpoint
        begin
          for yyy := StartY to EndY do
          begin
            // If we have not checked this point, check it now.
            if ch[xxx][yyy] and (1 shl i) = 0 then
            begin
              // Checking point i now. (Store that we matched it)
              ch[xxx][yyy]:= ch[xxx][yyy] or (1 shl i);
              if compare(ctsinfoarray[i], @cd[yyy][xxx]) then
                b[xxx][yyy] := b[xxx][yyy] or (1 shl i);
            end;

            //Check if the point matches the subpoint
            if (b[xxx][yyy] and (1 shl i) <> 0) then
            begin
              //Check if it was supposed to be a goodpoint..
              if GoodPoints[i] then
              begin
                Found := true;
                break;
              end else //It was not supposed to match!!
                goto AnotherLoopEnd;
            end;
          end;
          if Found then Break; //Optimalisation, we must break out of this second for loop, since we already found the subpoint
        end;
        if (not found) and (GoodPoints[i]) then      //This sub-point wasn't found, while it should.. Exit this mainpoint search
          goto AnotherLoopEnd;
      end;
      //We survived the sub-point search, add this mainpoint to the results.
      ClientTPA[pc] := Point(xx + x1, yy + y1);
      Inc(pc);
      if(pc = maxToFind) then
        goto theEnd;
      AnotherLoopEnd:
    end;
  TheEnd:

  Free_CTSInfoArray(ctsinfoarray);
  TClient(Client).IOManager.FreeReturnData;

  SetLength(Points, pc);
  if pc > 0 then
    Move(ClientTPA[0], Points[0], pc * SizeOf(TPoint));
  Result := (pc > 0);
end;

function TMFinder.FindDTMRotated(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended; Alternating : boolean): Boolean;

var
   P: TPointArray;
   F: T2DExtendedArray;
begin
  Result := FindDTMsRotated(dtm, P, x1, y1, x2, y2, sAngle, eAngle, aStep, F,Alternating,1);
  if not result then
      exit;

  aFound := F[0][0];
  x := P[0].x;
  y := P[0].y;
  Exit(True);
end;

procedure RotPoints_DTM(const P: TPointArray;var RotTPA : TPointArray; const A:
    Extended); inline;
var
   I, L: Integer;
begin
  L := High(P);
  for I := 0 to L do
  begin
    RotTPA[I].X := Round(cos(A) * p[i].x  - sin(A) * p[i].y);
    RotTPA[I].Y := Round(sin(A) * p[i].x  + cos(A) * p[i].y);
  end;
end;

function TMFinder.FindDTMsRotated(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray;Alternating : boolean; maxToFind: Integer): Boolean;
var
   //Cached variables
   Len : integer;
   DPoints : PMDTMPoint;
   DTPA : TPointArray;
   RotTPA: TPointArray;

   // Bitwise
   b: Array of Array of Integer;
   ch: Array of Array of Integer;

   // bounds
   W, H: integer;
   MA: TBox;
   MaxX,MaxY : integer;//The maximum value a (subpoint) can have!

   // for loops, etc
   xx, yy: integer;
   i, xxx,yyy: Integer;
   StartX,StartY,EndX,EndY : integer;

   Found : boolean;
   //clientdata
   cd: TPRGB32Array;

   PtrData: TRetData;

   //If we search alternating, we start in the middle and then +,-,+,- the angle step outwars
   MiddleAngle : extended;
   //Count the amount of anglesteps, mod 2 determines whether it's a + or a - search, and div 2 determines the amount of steps
   //you have to take.
   AngleSteps : integer;

   // point count
   pc: Integer = 0;

   goodPoints: Array of Boolean;
   s: extended;

   col_arr, tol_arr: Array of Integer;
   ctsinfoarray: TCTSInfoArray;
   compare: TCTSCompareFunction;

   label theEnd;
   label AnotherLoopEnd;


begin
  // Is the area valid?
  DefaultOperations(x1, y1, x2, y2);
  if not dtm.Valid then
    raise Exception.CreateFmt('FindDTMs: DTM[%s] is not consistent.', [DTM.name]);

  dtm.Normalize;;

  Len := DTM.Count;
  DPoints:= DTM.PPoints;

  setlength(goodPoints, Len);
  for i := 0 to Len - 1 do
    goodPoints[i] := not DPoints[i].bp;

  MaxX := x2 - x1;
  MaxY := y2 - y1;

  // Init data structure B.
  W := x2 - x1;
  H := y2 - y1;
  setlength(b, (W + 1));
  setlength(ch, (W + 1));
  for i := 0 to W do
  begin
    setlength(b[i], (H + 1));
    FillChar(b[i][0], SizeOf(Integer) * (H+1), 0);
    setlength(ch[i], (H + 1));
    FillChar(ch[i][0], SizeOf(Integer) * (H+1), 0);
  end;

  {
  When we search for a rotated DTM, everything is the same, except the coordinates..
  Therefore we create a TPA of the 'original' DTM, containing all the Points.
  This then will be used to rotate the points}
  SetLength(DTPA,len);
  SetLength(RotTPA,len);
  for i := 0 to len-1 do
    DTPA[i] := Point(DPoints[i].x,DPoints[i].y);

  // Retreive Client Data.
  PtrData := TClient(Client).IOManager.ReturnData(x1, y1, W + 1, H + 1);

  SetLength(col_arr, Len);
  SetLength(tol_arr, Len);
  // C = DTM.C
  for i := 0 to Len - 1 do
  begin
    col_arr[i] := DPoints[i].c;
    tol_arr[i] := DPoints[i].t;
  end;

  ctsinfoarray := Create_CTSInfoArray(col_arr, tol_arr);
  compare := Get_CTSCompare(Self.CTS);

  cd := CalculateRowPtrs(PtrData, h + 1);
  SetLength(aFound, 0);
  SetLength(Points, 0);
  if Alternating then
  begin
    MiddleAngle := (sAngle + eAngle) / 2.0;
    s := MiddleAngle;  //Start in the middle!
    AngleSteps := 0;
  end else
    s := sAngle;
  while s < eAngle do
  begin
    RotPoints_DTM(DTPA,RotTPA,s);
    //DTMRot now has the same points as the original DTM, just rotated!
    //The other stuff in the structure doesn't matter, as it's the same as the original DTM..
    //So from now on if we want to see what 'point' we're at, use RotTPA, for the rest just use the original DTM
    MA := ValidMainPointBox(RotTPA, x1, y1, x2, y2);
    //CD(ClientData) starts at 0,0.. We must adjust the MA, since this is still based on the xs,ys,xe,ye box.
    MA.x1 := MA.x1 - x1;
    MA.y1 := MA.y1 - y1;
    MA.x2 := MA.x2 - x1;
    MA.y2 := MA.y2 - y1;
    //MA is now fixed to the new (0,0) box...
    for yy := MA.y1  to MA.y2  do //(xx,yy) is now the coord of the mainpoint in the search area
      for xx := MA.x1  to MA.x2 do
      begin
        //Mainpoint can have area size as well, so we must check that just like any subpoint.
        for i := 0 to Len - 1 do
        begin //change to use other areashapes too.
          Found := false;
          //With area it can go out of bounds, therefore this max/min check
          StartX := max(0,xx - DPoints[i].asz + RotTPA[i].x);
          StartY := max(0,yy - DPoints[i].asz + RotTPA[i].y);
          EndX := Min(MaxX,xx + DPoints[i].asz + RotTPA[i].x);
          EndY := Min(MaxY,yy + DPoints[i].asz + RotTPA[i].y);
          for xxx := StartX to EndX do //The search area for the subpoint
          begin
            for yyy := StartY to EndY do
            begin
              // If we have not checked this point, check it now.
              if ch[xxx][yyy] and (1 shl i) = 0 then
              begin
                // Checking point i now. (Store that we matched it)
                ch[xxx][yyy]:= ch[xxx][yyy] or (1 shl i);

                if compare(ctsinfoarray[i], @cd[yyy][xxx]) then
                  b[xxx][yyy] := b[xxx][yyy] or (1 shl i);
              end;

              //Check if the point matches the subpoint
              if (b[xxx][yyy] and (1 shl i) <> 0) then
              begin
                //Check if it was supposed to be a goodpoint..
                if GoodPoints[i] then
                begin
                  Found := true;
                  break;
                end else //It was not supposed to match!!
                  goto AnotherLoopEnd;
              end;
            end;
            if Found then Break; //Optimalisation, we must break out of this second for loop, since we already found the subpoint
          end;
          if (not found) and (GoodPoints[i]) then      //This sub-point wasn't found, while it should.. Exit this mainpoint search
            goto AnotherLoopEnd;
        end;
        //We survived the sub-point search, add this mainpoint to the results.
        Inc(pc);
        setlength(Points,pc);
        Points[pc-1] := Point(xx + x1, yy + y1);
        Setlength(aFound, pc);
        setlength(aFound[pc-1],1);
        aFound[pc-1][0] := s;
        if(pc = maxToFind) then
          goto theEnd;
        AnotherLoopEnd:
      end;
    if Alternating then
    begin
      if AngleSteps mod 2 = 0 then   //This means it's an even number, thus we must add a positive step
        s := MiddleAngle + (aStep * (anglesteps div 2 + 1))  //Angle steps starts at 0, so we must add 1.
      else
        s := MiddleAngle - (aStep * (anglesteps div 2 + 1)); //We must search in the negative direction
      inc(AngleSteps);
    end else
      s := s + aStep;
  end;
  TheEnd:

  Free_CTSInfoArray(ctsinfoarray);
  TClient(Client).IOManager.FreeReturnData;

  Result := (pc > 0);
  { Don't forget to pre calculate the rotated points at the start.
   Saves a lot of rotatepoint() calls. }
//  raise Exception.CreateFmt('Not done yet!', []);
end;

function TMFinder.GetColors(const Coords: TPointArray): TIntegerArray;
var
  Box : TBox;
  Len, I,w,h : integer;
  PtrRet : TRetData;
  Ptr : PRGB32;
begin
  len := high(Coords);
  setlength(result,len+1);
  box := GetTPABounds(coords);
  w := 0;
  h := 0;
  DefaultOperations(w,h,box.x2,box.y2);
  TClient(Self.Client).IOManager.GetDimensions(w,h);
  PtrRet := TClient(Client).IOManager.ReturnData(0,0,Box.x2 + 1,box.y2+ 1);//Otherwise lotsashit.
  ptr := PtrRet.Ptr;
  for i := 0 to len do
    Result[i] := BGRToRGB(Ptr[Coords[i].y*w + Coords[i].x]);
end;

end.
