{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

interface

{$define CheckAllBackground}//Undefine this to only check the first white point against the background (in masks).
uses
  Classes, SysUtils,bitmaps,  MufasaTypes; // Types

{ TMFinder Class }

{
  Should be 100% independant, as all platform dependant code is in the
  Window and Input classes.

  Let's try not to use any OS-specific defines here? ;)
}

type
    TMFinder = class(TObject)
          constructor Create(aClient: TObject);
          destructor Destroy; override;
      private


        Procedure UpdateCachedValues(NewWidth,NewHeight : integer);
        procedure DefaultOperations(var xs,ys,xe,ye : integer);
        //Loads the Spiral into ClientTPA (Will not cause problems)
        procedure LoadSpiralPath(startX, startY, x1, y1, x2, y2: Integer);
      public
        function FindColorsToleranceOptimised(out Points: TPointArray; Color,
          xs, ys, xe, ye, Tol: Integer): Boolean;
        function FindColorToleranceOptimised(out x, y: Integer; Color, xs, ys,
          xe, ye, tol: Integer): Boolean;
        function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
        procedure SetToleranceSpeed(nCTS: Integer);
        function SimilarColors(Color1,Color2,Tolerance : Integer) : boolean;
        // Possibly turn x, y into a TPoint var.
        function FindColor(out x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
        function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;
        function FindColorTolerance(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
        function FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean;
        function FindColorsSpiralTolerance(x, y: Integer; out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;
        function FindColors(out TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
        //Mask
        function FindBitmapMaskTolerance(mask: TMask; out x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;
        procedure CheckMask(Mask : TMask);
        //Bitmap functions
        function FindBitmap(bitmap: TMufasaBitmap; out x, y: Integer): Boolean;
        function FindBitmapIn(bitmap: TMufasaBitmap; out x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;
        function FindBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;
        function FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;
        function FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;
        function FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x, y: Integer; out Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;
        function FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;

        function FindDTM(DTM: pDTM; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
        function FindDTMs(DTM: pDTM; out Points: TPointArray; x1, y1, x2, y2: Integer): Boolean;
        function FindDTMRotated(DTM: pDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;
        function FindDTMsRotated(DTM: pDTM; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray): Boolean;


    protected
        Client: TObject;
        Percentage : array[0..255] of Extended; //We store all the possible RGB / 255 divisions.
        CachedWidth, CachedHeight : integer;
        ClientTPA : TPointArray;
        hueMod, satMod: Extended;
        CTS: Integer;
    end;

implementation
uses
    Client, // For the Client Casts.
    colour_conv, // For RGBToColor, etc.
    math, //min/max
    dtmutil
    ;
type
  TPRGB32Array = array of PRGB32;

procedure TMFinder.LoadSpiralPath(startX, startY, x1, y1, x2, y2: Integer);
var
  i,c,Ring : integer;
  CurrBox : TBox;
begin;
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
    inc(c);
  end;
  Repeat
    if (CurrBox.x2 >= x1) and (CurrBox.x1 <= x2) and (Currbox.y1 >= y1) and (Currbox.y1 <= y2)  then
      for i := CurrBox.x1 + 1 to CurrBox.x2 do
        if (I >= x1) and ( I <= x2) then
        begin;
          ClientTPA[c] := Point(i,CurrBox.y1);
          inc(c);
        end;
    if (CurrBox.x2 >= x1) and (CurrBox.x2 <= x2) and (Currbox.y2 >= y1) and (Currbox.y1 <= y2)  then
      for i := CurrBox.y1 + 1 to CurrBox.y2 do
        if (I >= y1) and ( I <= y2) then
        begin;
          ClientTPA[c] := Point(Currbox.x2, I);
          inc(c);
        end;
    if (CurrBox.x2 >= x1) and (CurrBox.x1 <= x2) and (Currbox.y2 >= y1) and (Currbox.y2 <= y2)  then
      for i := CurrBox.x2 - 1 downto CurrBox.x1 do
        if (I >= x1) and ( I <= x2) then
        begin;
          ClientTPA[c] := Point(i,CurrBox.y2);
          inc(c);
        end;
    if (CurrBox.x1 >= x1) and (CurrBox.x1 <= x2) and (Currbox.y2 >= y1) and (Currbox.y1 <= y2)  then
      for i := CurrBox.y2 - 1 downto CurrBox.y1 do
        if (I >= y1) and ( I <= y2) then
        begin;
          ClientTPA[c] := Point(Currbox.x1, I);
          inc(c);
        end;
    inc(ring);
    CurrBox.x1 := Startx-ring;
    CurrBox.y1 := Starty-Ring;
    CurrBox.x2 := Startx+Ring;
    CurrBox.y2 := Starty+Ring;
  until (Currbox.x1 < x1) and (Currbox.x2 > x2) and (currbox.y1 < y1) and (currbox.y2 > y2);
end;

function CalculateRowPtrs(ReturnData : TRetData; RowCount : integer) : TPRGB32Array;overload;
var
  I : integer;
begin;
  setlength(result,RowCount);
  for i := 0 to RowCount - 1 do
    result[i] := ReturnData.Ptr + ReturnData.RowLen * i;
end;

function CalculateRowPtrs(Bitmap : TMufasaBitmap) : TPRGB32Array;overload;
var
  I : integer;
begin;
  setlength(result,Bitmap.Height);
  for i := 0 to Bitmap.Height - 1 do
    result[i] := Bitmap.FData + Bitmap.Width * i;
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
//Points left holds the amount of points that are "left" to be checked (Including the point itself.. So for example Pointsleft[0][0] would hold the total amount of pixels that are to be checked.
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

constructor TMFinder.Create(aClient: TObject);
var
  I : integer;

begin
  inherited Create;

  Self.Client := aClient;
  Self.CTS := 1;
  Self.hueMod := 0.2;
  Self.satMod := 0.2;
  for i := 0 to 255 do
    Percentage[i] := i / 255;

end;

destructor TMFinder.Destroy;
begin

  inherited;
end;

procedure TMFinder.SetToleranceSpeed(nCTS: Integer);
begin
  if (nCTS < 0) or (nCTS > 2) then
    raise Exception.CreateFmt('The given CTS ([%d]) is invalid.',[nCTS]);
  Self.CTS := nCTS;
end;

function TMFinder.SimilarColors(Color1, Color2,Tolerance: Integer) : boolean;
var
  R1,G1,B1,R2,G2,B2 : Byte;
  H1,S1,L1,H2,S2,L2 : extended;
begin
  Result := False;
  ColorToRGB(Color1,R1,G1,B1);
  ColorToRGB(Color2,R2,G2,B2);
  if Color1 = Color2 then
    Result := true
  else
  case CTS of
  0: Result := ((Abs(R1-R2) <= Tolerance) and (Abs(G1-G2) <= Tolerance) and (Abs(B1-B2) <= Tolerance));
  1: Result := (Sqrt(sqr(R1-R2) + sqr(G1-G2) + sqr(B1-B2)) <= Tolerance);
  2: begin
       RGBToHSL(R1,g1,b1,H1,S1,L1);
       RGBToHSL(R2,g2,b2,H2,S2,L2);
       Result := ((abs(H1 - H2) <= (hueMod * Tolerance)) and (abs(S2-S1) <= (satMod * Tolerance)) and (abs(L1-L2) <= Tolerance));
     end;
  end;
end;


function ColorSame(var CTS,Tolerance : Integer; var R1,G1,B1,R2,G2,B2 : byte; var H1,S1,L1,huemod,satmod : extended) : boolean; inline;
var
  H2,S2,L2 : extended;
begin
  Result := False;
  case CTS of
  0: Result := ((Abs(R1-R2) <= Tolerance) and (Abs(G1-G2) <= Tolerance) and (Abs(B1-B2) <= Tolerance));
  1: Result := (Sqrt(sqr(R1-R2) + sqr(G1-G2) + sqr(B1-B2)) <= Tolerance);
  2: begin
       RGBToHSL(R2,g2,b2,H2,S2,L2);
       Result := ((abs(H1 - H2) <= (hueMod * Tolerance)) and (abs(S2-S1) <= (satMod * Tolerance)) and (abs(L1-L2) <= Tolerance));
     end;
  end;
end;

procedure TMFinder.UpdateCachedValues(NewWidth, NewHeight: integer);
begin
  CachedWidth := NewWidth;
  CachedHeight := NewHeight;
  SetLength(ClientTPA,NewWidth * NewHeight);
end;

procedure TMFinder.DefaultOperations(var xs, ys, xe, ye: integer);
var
  w,h : integer;
begin
  if xs > xe then
    raise Exception.CreateFMT('Finder function: Xs > xe (%d,%d)',[xs,xe]);
  if ys > ye then
    raise Exception.CreateFMT('Finder function: Ys > ye (%d,%d)',[ys,ye]);
  if xs < 0 then
    // xs := 0;
    raise Exception.createFMT('Any Find Function, you did not pass a ' +
                              'correct xs: %d.', [xs]);
  if ys < 0 then
//    ys := 0;
    raise Exception.createFMT('Any Find Function, you did not pass a ' +
                              'correct ys: %d.', [ys]);

  TClient(Self.Client).MWindow.GetDimensions(w,h);
  if (w <> CachedWidth) or (h <> CachedHeight) then
    UpdateCachedValues(w,h);
  if xe >= w then
//    xe := w-1;
    raise Exception.createFMT('Any Find Function, you did not pass a ' +
                              'correct xe: %d.', [xe]);
  if ye >= h then
//    ye := h-1;
    raise Exception.createFMT('Any Find Function, you did not pass a ' +
                              'correct ye: %d.', [ye]);
end;

function TMFinder.CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   clR, clG, clB : byte;
   dX, dY, xx, yy: Integer;
   h,s,l,hmod,smod : extended;
   Ccts : integer;
begin
  Result := 0;
  DefaultOperations(xs, ys, xe, ye);
  dX := xe - xs;
  dY := ye - ys;
  ColorToRGB(Color, clR, clG, clB);
  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;
  CCts := Self.CTS;
  result := 0;
  if cts = 2 then
  begin;
    RGBToHSL(clR,clG,clB,h,s,l);
    hmod := Self.hueMod;
    smod := Self.satMod;
  end;
  for yy := ys to ye do
  begin;
    for xx := xs to xe do
    begin;
      if ColorSame(CCts,Tolerance,clR,clG,clB,Ptr^.r,Ptr^.g,Ptr^.b,H,S,L,hmod,smod) then
        inc(result);
      Inc(Ptr);
    end;
    Inc(Ptr, PtrInc)
  end;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColor(out x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  for yy := ys to ye do
  begin;
    for xx := xs to xe do
    begin;
      // Colour comparison here. Possibly with tolerance? ;)
      if (Ptr^.R = clR) and (Ptr^.G = clG) and (Ptr^.B = clB) then
      begin
        Result := True;
        x := xx;
        y := yy;

        TClient(Client).MWindow.FreeReturnData;
        Exit;
      end;
      Inc(Ptr);
    end;
    Inc(Ptr, PtrInc)
  end;

  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColorSpiral(var x, y: Integer; color, xs, ys, xe,
  ye: Integer): Boolean;
var
   PtrData: TRetData;
   RowData : TPRGB32Array;
   dX, dY, clR, clG, clB, i,HiSpiral: Integer;

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
  //Load rowdata
  RowData:= CalculateRowPtrs(ptrdata,dy+1);
  //Load the spiral path
  LoadSpiralPath(x-xs,y-ys,0,0,dx,dy);


  HiSpiral := (dy+1) * (dx + 1) -1;
  for i := 0 to HiSpiral do
    if (RowData[ClientTPA[i].y][ClientTPA[i].x].R = clR) and (RowData[ClientTPA[i].y][ClientTPA[i].x].G = clG)
        and (RowData[ClientTPA[i].y][ClientTPA[i].x].B = clB) then
      begin
        Result := True;
        x := ClientTPA[i].x + xs;
        y := ClientTPA[i].y + ys;
        TClient(Client).MWindow.FreeReturnData;
        Exit;
      end;

  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColorToleranceOptimised(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2: Extended;
   R,G,B : extended; //percentage R,G,B.. (Needed for HSL).
   D : Extended; //CMax - Cmin
   HueTol,SatTol, LumTol : extended;
   CMax, CMin : extended;
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
    RGBToHSLNonFixed(clR,clG,clB,H1,S1,L1);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  case CTS of
    0:
    for yy := ys to ye do
    begin
      for xx := xs to xe do
      begin
         if ((abs(clB-Ptr^.B) <= Tol) and (abs(clG-Ptr^.G) <= Tol) and (Abs(clR-Ptr^.R) <= Tol)) then
            goto Hit;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    1:
    begin
      Tol := Sqr(Tol);

      for yy := ys to ye do
      begin
        for xx := xs to xe do
        begin
           if (sqr(clB - Ptr^.B) + sqr(clG - Ptr^.G) + sqr(clR-Ptr^.R)) <= Tol then
              goto Hit;
          inc(ptr);
        end;
        Inc(Ptr, PtrInc);
      end;

    end;
    2:
    begin
      //Since we don't make (real) percentages of the HSL-values we need to change the tolerance..
      HueTol := hueMod * Tol / 100;
      SatTol := satMod * Tol / 100;
      LumTol := Tol / 100;
      for yy := ys to ye do
      begin
        for xx := xs to xe do
        begin
          R := Percentage[Ptr^.r];
          G := Percentage[Ptr^.g];
          B := Percentage[Ptr^.b];
          //We increase the Ptr already, since we do Continue in loops..
          inc(Ptr);
          CMin := R;
          CMax := R;
          if G  < Cmin then CMin := G;
          if B  < Cmin then CMin := B;
          if G  > Cmax then CMax := G;
          if B  > Cmax then CMax := B;
          L2 := 0.5 * (Cmax + Cmin);
          //The L-value is already calculated, lets see if the current point meats the requirements!
          if Abs(L2-L1) > LumTol then //if not (Abs(L2 - L1) <= LumTol) then
            Continue;
          if Cmax = Cmin then
          begin
            //S and H are both zero, lets check if we need found a point!
            if (H1 <= HueTol) and (S1 <= SatTol) then
              goto Hit
            else
              Continue;
          end;
          D := Cmax - Cmin;
          if L2 < 0.5 then
            S2 := D / (Cmax + Cmin)
          else
            S2 := D / (2 - Cmax - Cmin);
          //We've Calculated the S. Lets see if we need to continue.
          if Abs(S2 - S1) > SatTol then //if not (abs(S1 - S2) <= SatXTol) then
            Continue;
          if R = Cmax then
            H2 := (G - B) / D
          else
            if G = Cmax then
              H2  := 2 + (B - R) / D
            else
              H2 := 4 +  (R - G) / D;
          H2 := H2 / 6;
          if H2 < 0 then
            H2 := H2 + 1;
          //Finally lets test H2
          if Abs(H2 - H1) <= HueTol then
            goto hit;
        end;
        Inc(Ptr, PtrInc);
      end;
    end;
  end;
  Result := False;
  TClient(Client).MWindow.FreeReturnData;
  Exit;

  Hit:
    Result := True;
    x := xx;
    y := yy;
    TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColorTolerance(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2: Extended;
   R,G,B : extended; //percentage R,G,B.. (Needed for HSL).
   D : Extended; //CMax - Cmin
   HueXTol, SatXTol: Extended;
   CMax, CMin : extended;
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

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  case CTS of
    0:
    for yy := ys to ye do
    begin
      for xx := xs to xe do
      begin
         if ((abs(clB-Ptr^.B) <= Tol) and (abs(clG-Ptr^.G) <= Tol) and (Abs(clR-Ptr^.R) <= Tol)) then
            goto Hit;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    1:
    begin
      Tol := Sqr(Tol);

      for yy := ys to ye do
      begin
        for xx := xs to xe do
        begin
           if (sqr(clB - Ptr^.B) + sqr(clG - Ptr^.G) + sqr(clR-Ptr^.R)) <= Tol then
              goto Hit;
          inc(ptr);
        end;
        Inc(Ptr, PtrInc);
      end;

    end;
    2:
    // Can be optimized a lot... RGBToHSL isn't really inline,
    begin
      HueXTol := hueMod * Tol;
      SatXTol := satMod * Tol;
      for yy := ys to ye do
      begin
        for xx := xs to xe do
        begin
          RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,H2,S2,L2);
          if ((abs(H1 - H2) <= HueXTol) and (abs(S1 - S2) <= SatXTol) and (abs(L1 - L2) <= Tol)) then
            goto Hit;
         inc(Ptr);
        end;
        Inc(Ptr, PtrInc);
      end;
    end;
  end;
  Result := False;
  TClient(Client).MWindow.FreeReturnData;
  Exit;

  Hit:
    Result := True;
    x := xx;
    y := yy;
    TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColorsTolerance(out Points: TPointArray; Color, xs, ys,
  xe, ye, Tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc,C: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2, hueXTol, satXTol,LumTol,R,G,B,D,Cmin,Cmax: Extended;
label
  hit;
begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  if CTS = 2 then
    ColorToHSL(color,H1,S1,L1);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;
  c := 0;
  case CTS of
    0:
    for yy := ys to ye do
    begin
      for xx := xs to xe do
      begin
         if ((abs(clB-Ptr^.B) <= Tol) and (abs(clG-Ptr^.G) <= Tol) and (Abs(clR-Ptr^.R) <= Tol)) then
         begin;
           ClientTPA[c].x := xx;
           ClientTPA[c].y := yy;
           inc(c);
         end;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    1:
    for yy := ys to ye do
    begin
      for xx := xs to xe do
      begin
         if (Sqrt(sqr(clR-Ptr^.R) + sqr(clG - Ptr^.G) + sqr(clB - Ptr^.B)) <= Tol) then
         begin;
           ClientTPA[c].x := xx;
           ClientTPA[c].y := yy;
           inc(c);
         end;
        inc(ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    2:
    begin
      HueXTol := hueMod * Tol;
      SatXTol := satMod * Tol;
      for yy := ys to ye do
      begin
        for xx := xs to xe do
        begin
          RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,H2,S2,L2);
          if ((abs(H1 - H2) <= HueXTol) and (abs(S1 - S2) <= SatXTol) and (abs(L1 - L2) <= Tol)) then
          begin;
            ClientTPA[c].x := xx;
            ClientTPA[c].y := yy;
            Inc(c);
          end;
          Inc(Ptr)
        end;
        Inc(Ptr, PtrInc);
      end;
    end;
  end;
  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;
  TClient(Client).MWindow.FreeReturnData;
end;
function TMFinder.FindColorsToleranceOptimised(out Points: TPointArray; Color, xs, ys,
  xe, ye, Tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc,C: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2, hueTol, satTol,LumTol,R,G,B,D,Cmin,Cmax: Extended;
label
  hit;
begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  if CTS = 2 then
    RGBToHSLNonFixed(clR,clG,clB,H1,S1,L1);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;
  c := 0;
  case CTS of
    0:
    for yy := ys to ye do
    begin
      for xx := xs to xe do
      begin
         if ((abs(clB-Ptr^.B) <= Tol) and (abs(clG-Ptr^.G) <= Tol) and (Abs(clR-Ptr^.R) <= Tol)) then
         begin;
           ClientTPA[c].x := xx;
           ClientTPA[c].y := yy;
           inc(c);
         end;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    1:
    for yy := ys to ye do
    begin
      for xx := xs to xe do
      begin
         if (Sqrt(sqr(clR-Ptr^.R) + sqr(clG - Ptr^.G) + sqr(clB - Ptr^.B)) <= Tol) then
         begin;
           ClientTPA[c].x := xx;
           ClientTPA[c].y := yy;
           inc(c);
         end;
        inc(ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    2:
    begin
      HueTol := hueMod * Tol / 100;
      SatTol := satMod * Tol / 100;
      LumTol := Tol / 100;
      for yy := ys to ye do
      begin
        for xx := xs to xe do
        begin
          R := Percentage[Ptr^.r];
          G := Percentage[Ptr^.g];
          B := Percentage[Ptr^.b];
          //We increase the Ptr already, since we use Continue;
          inc(Ptr);
          CMin := R;
          CMax := R;
          if G  < Cmin then CMin := G;
          if B  < Cmin then CMin := B;
          if G  > Cmax then CMax := G;
          if B  > Cmax then CMax := B;
          L2 := 0.5 * (Cmax + Cmin);
          //The L-value is already calculated, lets see if the current point meats the requirements!
          if Abs(L2-L1) > LumTol then //if not (Abs(L2 - L1) <= LumTol) then
            Continue;
          if Cmax = Cmin then
          begin
            //S and H are both zero, lets check if we need found a point!
            if (H1 <= HueTol) and (S1 <= SatTol) then
              goto Hit
            else
              Continue;
          end;
          D := Cmax - Cmin;
          if L2 < 0.5 then
            S2 := D / (Cmax + Cmin)
          else
            S2 := D / (2 - Cmax - Cmin);
          //We've Calculated the S. Lets see if we need to continue.
          if Abs(S2 - S1) > SatTol then //if not (abs(S1 - S2) <= SatXTol) then
            Continue;
          if R = Cmax then
            H2 := (G - B) / D
          else
            if G = Cmax then
              H2  := 2 + (B - R) / D
            else
              H2 := 4 +  (R - G) / D;
          H2 := H2 / 6;
          if H2 < 0 then
            H2 := H2 + 1;
          //Finally lets test H2
          if Abs(H2 - H1) > HueTol then
            continue;
          hit:
            ClientTPA[c].x := xx;
            ClientTPA[c].y := yy;
            Inc(c);
        end;
        Inc(Ptr, PtrInc);
      end;
    end;
  end;
  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColorsSpiralTolerance(x, y: Integer;
  out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer
  ): boolean;
var
   PtrData: TRetData;
   c : integer;
   RowData : TPRGB32Array;
   dX, dY, clR, clG, clB, i,SpiralHi: Integer;
   H1, S1, L1, H2, S2, L2, HueXTol, SatXTol: Extended;
begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  ColorToHSL(Color, H1, S1, L1);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  c := 0;

  //Load rowdata
  RowData:= CalculateRowPtrs(ptrdata,dy+1);
  //Load the spiral path
  LoadSpiralPath(x-xs,y-ys,0,0,dx,dy);
  SpiralHi := (dx + 1) * (dy + 1) - 1;
  case CTS of
    0:
    for i := 0 to SpiralHi do
      if ((abs(clB-RowData[ClientTPA[i].y][ClientTPA[i].x].B) <= Tolerance) and
         (abs(clG-RowData[ClientTPA[i].y][ClientTPA[i].x].G) <= Tolerance) and
         (Abs(clR-RowData[ClientTPA[i].y][ClientTPA[i].x].R) <= Tolerance)) then
      begin;
        ClientTPA[c].x := ClientTPA[i].x + xs;
        ClientTPA[c].y := ClientTPA[i].y + ys;
        inc(c);
      end;


    1:
    for i := 0 to SpiralHi do
      if (Sqrt(sqr(clR - RowData[ClientTPA[i].y][ClientTPA[i].x].R) +
               sqr(clG - RowData[ClientTPA[i].y][ClientTPA[i].x].G) +
               sqr(clB - RowData[ClientTPA[i].y][ClientTPA[i].x].B)) <= Tolerance) then
      begin;
        ClientTPA[c].x := ClientTPA[i].x + xs;
        ClientTPA[c].y := ClientTPA[i].y + ys;
        inc(c);
      end;

    2:
    begin;
      HueXTol := hueMod * Tolerance;
      SatXTol := satMod * Tolerance;
      for i := 0 to SpiralHi do
      begin;
        RGBToHSL(RowData[ClientTPA[i].y][ClientTPA[i].x].R,
                 RowData[ClientTPA[i].y][ClientTPA[i].x].G,
                 RowData[ClientTPA[i].y][ClientTPA[i].x].B,
                 H2,S2,L2);
        if ((abs(H1 - H2) <= (HueXTol)) and (abs(S1 - S2) <= (satXTol)) and (abs(L1 - L2) <= Tolerance)) then
        begin;
          ClientTPA[c].x := ClientTPA[i].x + xs;
          ClientTPA[c].y := ClientTPA[i].y + ys;
          inc(c);
        end;
      end;
    end;
  end;
  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColors(out TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy, i: Integer;

begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;

  I := 0;

  ColorToRGB(Color, clR, clG, clB);

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);

  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  for yy := ys to ye do
  begin;
    for xx := xs to xe do
    begin;
      if (Ptr^.R = clR) and (Ptr^.G = clG) and (Ptr^.B = clB) then
      begin
        Self.ClientTPA[I].x := xx;
        Self.ClientTPA[i].y := yy;
        Inc(I);
      end;
      Inc(Ptr);
    end;
    Inc(Ptr, PtrInc);
  end;

  SetLength(TPA, I);

  Move(ClientTPA[0], TPA[0], i * SizeOf(TPoint));

  Result := I > 0;

  TClient(Client).MWindow.FreeReturnData;
end;

//Only works with CTS 1 for now.. Since Colorsame doesn't return a boolean :-(
//We do not check whether every white pixel is in tol range with every other white pixel..

function TMFinder.FindBitmapMaskTolerance(mask: TMask; out x, y: Integer; xs,
  ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   PtrData : TRetData;
   MaskW,MaskH : integer;
   CheckerWhite,CheckerBlack,CurrWhite,CurrBlack: TRGB32;
   i,ii : integer;
   dX, dY,  xx, yy: Integer;
label NotFoundMask;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);
  //Check the mask.
  CheckMask(Mask);
  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
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
      TClient(Client).MWindow.FreeReturnData;
      Exit(true);
      //Bah not found the mask, lets do nothing and continue!
      NotFoundMask:
    end;
  TClient(Client).MWindow.FreeReturnData;
end;

procedure TMFinder.CheckMask(Mask: TMask);
begin
  if (Mask.W < 1) or (Mask.H < 1) or (Mask.WhiteHi < 0) or (Mask.BlackHi < 0) then
    raise exception.CreateFMT('Mask is invalid. Width/Height: (%d,%d). WhiteHi/BlackHi: (%d,%d)',[Mask.W,Mask.H,Mask.WhiteHi,Mask.BlackHi]);
end;

function TMFinder.FindBitmap(bitmap: TMufasaBitmap; out x, y: Integer): Boolean;
var
  w,h : integer;
begin
  TClient(Client).MWindow.GetDimensions(w,h);
  result := Self.FindBitmapIn(bitmap,x,y,0,0,w-1,h-1);
end;

function TMFinder.FindBitmapIn(bitmap: TMufasaBitmap; out x, y: Integer; xs,
  ys, xe, ye: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  xx, yy: Integer;
   SkipCoords : T2DBoolArray;
label NotFoundBmp;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
  //Caculate the row ptrs
  MainRowdata:= CalculateRowPtrs(PtrData,dy+1);
  BmpRowData:= CalculateRowPtrs(bitmap);
  //Get the 'fixed' bmp size
  BmpW := bitmap.Width - 1;
  BmpH := bitmap.Height - 1;
  //Heck our bitmap cannot be outside the search area
  dX := dX - bmpW;
  dY := dY - bmpH;
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
            if (BmpRowData[yBmp][xBmp].R <> MainRowdata[tmpY][xBmp +  xx].R) or
               (BmpRowData[yBmp][xBmp].G <> MainRowdata[tmpY][xBmp +  xx].G) or
               (BmpRowData[yBmp][xBmp].B <> MainRowdata[tmpY][xBmp +  xx].B) then
               goto NotFoundBmp;

      end;
      //We did find the Bmp, otherwise we would be at the part below
      TClient(Client).MWindow.FreeReturnData;
      x := xx + xs;
      y := yy + ys;
      result := true;
      exit;
      NotFoundBmp:
    end;
  TClient(Client).MWindow.FreeReturnData;
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
   CCTS : integer;
   H,S,L,HMod,SMod : extended;
   SkipCoords : T2DBoolArray;
label NotFoundBmp;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
  //Caculate the row ptrs
  MainRowdata:= CalculateRowPtrs(PtrData,dy+1);
  BmpRowData:= CalculateRowPtrs(bitmap);
  //Get the 'fixed' bmp size
  BmpW := bitmap.Width - 1;
  BmpH := bitmap.Height - 1;
  //Heck our bitmap cannot be outside the search area
  dX := dX - bmpW;
  dY := dY - bmpH;
  //Compiler hints
  HMod := 0;SMod := 0;H := 0.0;S := 0.0; L := 0.0;
  CCTS := Self.CTS;
  //We wont want HSL comparison with BMPs, right? Not for now atleast.
  if CCTS > 1 then
    CCTS := 1;
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
            if not ColorSame(CCTS,tolerance,
                             BmpRowData[yBmp][xBmp].R,BmpRowData[yBmp][xBmp].G,BmpRowData[yBmp][xBmp].B,
                             MainRowdata[tmpY][xBmp +  xx].R,MainRowdata[tmpY][xBmp +  xx].G,MainRowdata[tmpY][xBmp +  xx].B,
                             H,S,L,HMod,SMod) then
               goto NotFoundBmp;

      end;
      //We did find the Bmp, otherwise we would be at the part below
      TClient(Client).MWindow.FreeReturnData;
      x := xx + xs;
      y := yy + ys;
      result := true;
      exit;
      NotFoundBmp:
    end;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer;
  xs, ys, xe, ye: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  i,HiSpiral: Integer;
   SkipCoords : T2DBoolArray;
label NotFoundBmp;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
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
  //Get the "skip coords".
  CalculateBitmapSkipCoords(Bitmap,SkipCoords);
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
          if not SkipCoords[yBmp][xBmp] then
            if (BmpRowData[yBmp][xBmp].R <> MainRowdata[tmpY][xBmp +  ClientTPA[i].x].R) or
               (BmpRowData[yBmp][xBmp].G <> MainRowdata[tmpY][xBmp +  ClientTPA[i].x].G) or
               (BmpRowData[yBmp][xBmp].B <> MainRowdata[tmpY][xBmp +  ClientTPA[i].x].B) then
               goto NotFoundBmp;

    end;
    //We did find the Bmp, otherwise we would be at the part below
    TClient(Client).MWindow.FreeReturnData;
    x := ClientTPA[i].x + xs;
    y := ClientTPA[i].y + ys;
    result := true;
    exit;
    NotFoundBmp:
  end;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x,
  y: Integer; xs, ys, xe, ye, tolerance: integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  i,HiSpiral: Integer;
   CCTS : integer;
   H,S,L,HMod,SMod : extended;
   SkipCoords : T2DBoolArray;
label NotFoundBmp;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
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
  //Compiler hints
  HMod := 0;SMod := 0;H := 0.0;S := 0.0; L := 0.0;
  //NO HSL.
  CCTS := Self.CTS;
  if CCTS > 1 then
    CCTS := 1;
  //Get the "skip coords".
  CalculateBitmapSkipCoords(Bitmap,SkipCoords);
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
          if not SkipCoords[yBmp][xBmp] then
            if not ColorSame(CCTS,tolerance,
                             BmpRowData[yBmp][xBmp].R,BmpRowData[yBmp][xBmp].G,BmpRowData[yBmp][xBmp].B,
                             MainRowdata[tmpY][xBmp +  ClientTPA[i].x].R,MainRowdata[tmpY][xBmp +  ClientTPA[i].x].G,
                             MainRowdata[tmpY][xBmp +  ClientTPA[i].x].B,
                             H,S,L,HMod,SMod) then
              goto NotFoundBmp;

    end;
    //We did find the Bmp, otherwise we would be at the part below
    x := ClientTPA[i].x + xs;
    y := ClientTPA[i].y + ys;
    result := true;
    exit;
    NotFoundBmp:
  end;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x,
  y: Integer; out Points: TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  i,HiSpiral: Integer;
   FoundC : integer;
   CCTS : integer;
   H,S,L,HMod,SMod : extended;
   SkipCoords : T2DBoolArray;
label NotFoundBmp;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;

  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
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
  //Compiler hints
  HMod := 0;SMod := 0;H := 0.0;S := 0.0; L := 0.0;
  //NO HSL.
  CCTS := Self.CTS;
  if CCTS > 1 then
    CCTS := 1;
  FoundC := 0;
  //Get the "skip coords".
  CalculateBitmapSkipCoords(Bitmap,SkipCoords);
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
          if not SkipCoords[yBmp][xBmp] then
            if not ColorSame(CCTS,tolerance,
                             BmpRowData[yBmp][xBmp].R,BmpRowData[yBmp][xBmp].G,BmpRowData[yBmp][xBmp].B,
                             MainRowdata[tmpY][xBmp +  ClientTPA[i].x].R,MainRowdata[tmpY][xBmp +  ClientTPA[i].x].G,
                             MainRowdata[tmpY][xBmp +  ClientTPA[i].x].B,
                             H,S,L,HMod,SMod) then
              goto NotFoundBmp;

    end;
    //We did find the Bmp, otherwise we would be at the part below
    ClientTPA[FoundC].x := ClientTPA[i].x + xs;
    ClientTPA[FoundC].y := ClientTPA[i].y + ys;
    inc(FoundC);
    NotFoundBmp:
  end;
  if FoundC > 0 then
  begin;
    result := true;
    SetLength(Points,FoundC);
    Move(ClientTPA[0], Points[0], FoundC * SizeOf(TPoint));
  end;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; out x,
  y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer;
  AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;
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
label FoundBMPPoint, Madness;
  //Don't know if the compiler has any speed-troubles with goto jumping in nested for loops.

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;
  SearchDx := dX;
  SearchDy := dY;
  PtrData := TClient(Client).MWindow.ReturnData(xs, ys, dX + 1, dY + 1);
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
              if Sqrt(sqr(BmpRowData[yBmp][xBmp].R - MainRowdata[RangeY][RangeX].R) + sqr(BmpRowData[yBmp][xBmp].G - MainRowdata[RangeY][RangeX].G)
                          +sqr(BmpRowData[yBmp][xBmp].B - MainRowdata[RangeY][RangeX].B)) <= tolerance then
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
        begin;
          TClient(Client).MWindow.FreeReturnData;
          x := BestPT.x;
          y := BestPT.y;
          accuracy:= 1;
          Exit(true);
        end;
      end;
    end;
  TClient(Client).MWindow.FreeReturnData;
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

function TMFinder.FindDTM(DTM: pDTM; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean;

var
   P: TPointArray;
begin
  Self.FindDTMs(DTM, P, x1, y1, x2, y2);
  if(Length(p) > 0) then
  begin
    x := p[0].x;
    y := p[0].y;
    Exit(True);
  end;
  Exit(False);
end;

// TODO: Add a max count, so we can use it more efficiently for FindDTM?
function TMFinder.FindDTMs(DTM: pDTM; out Points: TPointArray; x1, y1, x2, y2: Integer): Boolean;
var
   // Colours of DTMs
   C: Array of Integer;

   // Bitwise
   b: Array of Array of Integer;

   // bounds
   W, H: integer;
   MA: TBox;

   // for loops, etc
   xx, yy: integer;
   i, xxx,yyy: Integer;

   // for comparions.
   clR, clG, clB: Integer;

   //clientdata
   cd: TPRGB32Array;

   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;

   // point count
   pc: Integer = 0;

   label EndOfLoop;
   label AnotherLoopEnd;



begin
  if not DTMConsistent(dtm) then
  begin
    raise Exception.CreateFmt('FindDTMs: DTM is not consistent.', []);
    Exit;
  end;

  // Get the area we should search in for the Main Point.
  writeln(Format('%d, %d, %d, %d', [x1,y1,x2,y2]));
  MA := ValidMainPointBox(DTM, x1, y1, x2, y2);
  writeln(Format('%d, %d, %d, %d', [MA.x1,MA.y1,MA.x2,MA.y2]));

  DefaultOperations(MA.x1, MA.y1, MA.x2, MA.y2);

  // Init data structure B.
  W := x2 - x1;
  H := y2 - y1;
  setlength(b, W + 1);
  for i := 0 to W do
  begin
    setlength(b[i], H + 1);
    // does setlength init already? if it doesn't, do we want to init here?
    // or do we want to init in the loop, as we loop over every b anyway?
    //FillChar(b[i][0], SizeOf(Integer) * H, 0);
  end;

  // C = DTM.C
  C := DTM.c;

  // Now, we must find the occurances of all colours.
  // This part should be turned into a more general function (for DTM Finding).
  // Something like FindColorsMultiBooleanArray (?)

  // Retreive Client Data.
  PtrData := TClient(Client).MWindow.ReturnData(x1, y1, W + 1, H + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  cd := CalculateRowPtrs(PtrData, h + 1);
  writeln(format('w,h: %d, %d', [w,h]));
  for yy := 0 to h do
    for xx := 0 to w do
    begin
      //writeln(format('x,y: %d, %d', [xx,yy]));

      // main point
      if not TClient(Client).MFinder.SimilarColors(dtm.c[0], RGBToColor(cd[yy][xx].R, cd[yy][xx].G, cd[yy][xx].B), dtm.t[0]) then
        goto AnotherLoopEnd;
      b[xx][yy] := B[xx][yy] or 1;
      for i := 1 to dtm.l - 1 do
      begin // use square areashape - it covers all.
        for xxx := xx - dtm.asz[i] + dtm.p[i].x to xx + dtm.asz[i] + dtm.p[i].x do
          for yyy := yy - dtm.asz[i] + dtm.p[i].y to yy + dtm.asz[i]+ dtm.p[i].y do
            // may want to remove this line, but I think it is a good optimisation.
            if B[xxx][yyy] and (1 shl i) = 0 then
              if TClient(Client).MFinder.SimilarColors(dtm.c[i], RGBToColor(cd[yyy][xxx].R, cd[yyy][xxx].G, cd[yyy][xxx].B), dtm.t[i]) then
                b[xxx][yyy] := B[xxx][yyy] or (1 shl i)
              else
                goto AnotherLoopEnd;
      end;

      AnotherLoopEnd:
        //writeln(format('b[%d][%d]: %d' ,[xx,yy,b[xx][yy]]));
    end;

   {for yy := 0 to h do
    for xx := 0 to w do
      writeln(format('b[%d][%d]: %d' ,[xx,yy,b[xx][yy]])); }

  // Now iterate over the data. (Main Point Bounds)
  for yy := MA.y1-y1 to MA.y2-y1 do
    for xx := MA.x1-x1 to MA.x2-x1 do
    begin
      //writeln(format('Testing for MP at %d, %d', [xx,yy]));
      //writeln(format('Testing for MP RealPoints at %d, %d', [xx+x1,yy+y1]));
      if (b[xx][yy] and 1) = 0 then
        continue;
      //writeln(format('Got a MP at %d, %d', [xx,yy]));
      //writeln(format('Got a MP Real Points at %d, %d', [xx+x1,yy+y1]));
      for i := 1 to dtm.l - 1 do
      begin
        //writeln(format('i: %d',[i]));
        case dtm.ash[i] of

          { Example:
            3x3 (AreaSize = 1)
            X X X
            X X X
            X X X  }
          //areasize and areashape. areasize = 0 is completly valid
          dtm_Rectangle:
          begin
            //writeln(Format('X - From, To: %d, %d', [xx - dtm.asz[i]+ dtm.p[i].x, xx + dtm.asz[i]+ dtm.p[i].x]));
            for xxx := xx - dtm.asz[i] + dtm.p[i].x to xx + dtm.asz[i] + dtm.p[i].x do
              for yyy := yy - dtm.asz[i] + dtm.p[i].y to yy + dtm.asz[i]+ dtm.p[i].y do
                if b[xxx][yyy] and (1 shl i) = 0 then
                  goto EndOfLoop;
          end;

          { Example:
            3x3 (AreaSize = 1)
              X
            X X X
              X    }
          dtm_Cross:
          begin
            for xxx := xx - dtm.asz[i] + dtm.p[i].x to xx + dtm.asz[i] + dtm.p[i].x do
            begin
              //writeln(format('Cross - One. %d, %d', [xxx,dtm.p[i].y + yy]));
              if b[xxx][dtm.p[i].y + yy] and (1 shl i) = 0 then
                goto EndOfLoop;

            end;
            for yyy := yy - dtm.asz[i] + dtm.p[i].y to yy + dtm.asz[i]+ dtm.p[i].y do
            begin
              //writeln(format('Cross - One. %d, %d', [dtm.p[i].x + xx,yyy]));
              if b[dtm.p[i].x + xx][yyy] and (1 shl i) = 0 then
                goto EndOfLoop;
            end;
          end;

         { Example:
          3x3 (AreaSize = 1)
          X   X
            X
          X   X  }
         dtm_DiagonalCross:
          begin
            for xxx := -dtm.asz[i] to dtm.asz[i] do
            begin
              if b[xx + dtm.p[i].x + xxx][yy + dtm.p[i].y + xxx] and (1 shl i) = 0 then
                 goto EndOfLoop;
              if b[xx + dtm.p[i].x + xxx][yy + dtm.p[i].y + xxx] and (1 shl i) = 0 then
                goto EndOfLoop;
            end;
          end

          else
          begin
            raise exception.createFMT('FindDTMs: Invalid Areashape!', []);
            Exit;
          end;
        end;
        // point [xx,yy] found if we make it to here
        //writeln(Format('Wat (Real Points): %d, %d', [xx+x1, yy+y1])); //Good old wat!
      end;
      ClientTPA[pc] := Point(xx+x1, yy+y1);
      Inc(pc);
      //writeln(Format('Found a Point: %d, %d', [xx+x1, yy+y1]));
      EndOfLoop:
    end;
  TClient(Client).MWindow.FreeReturnData;
  SetLength(Points, pc);
  Move(ClientTPA[0], Points[0], pc * SizeOf(TPoint));
end;

function TMFinder.FindDTMRotated(DTM: pDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended): Boolean;

begin
  raise Exception.CreateFmt('Not done yet!', []);
end;

function TMFinder.FindDTMsRotated(DTM: pDTM; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray): Boolean;

begin
  // Don't forget to pre calculate the rotated points at the start.
  // Saves a lot of rotatepoint() calls.
  raise Exception.CreateFmt('Not done yet!', []);
end;

end.


{ crap code }

  // this is the probably the slowest part of the algorithm.
{  for yy := y1 to y2 do
    begin
      for xx := x1 to x2 do
      begin
         {for i := 0 to dtm.l - 1 do
         begin
            // optimise this later...
            if TClient(Client).MFinder.SimilarColors(dtm.c[i], RGBToColor(Ptr^.R,Ptr^.G,Ptr^.B) , dtm.t[i]) then
               B[xx][yy] := B[xx][yy] or (1 shl i);
         end;     }
         inc(Ptr);
      end;
      inc(Ptr, PtrInc);
    end;
             }

