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
        function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
        procedure SetToleranceSpeed(nCTS: Integer);
        function SimilarColors(Color1,Color2,Tolerance : Integer) : boolean;
        // Possibly turn x, y into a TPoint var.
        function FindColor(var x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
        function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;
        function FindColorTolerance(var x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
        function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean;
        function FindColorsSpiralTolerance(x, y: Integer; var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer) : boolean;
        function FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
        //Bitmap functions
        function FindBitmap(bitmap: TMufasaBitmap; var x, y: Integer): Boolean;
        function FindBitmapIn(bitmap: TMufasaBitmap; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;
        function FindBitmapToleranceIn(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;
        function FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;
        function FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;
        function FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x, y: Integer; var Points : TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;
        function FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;
      protected
        Client: TObject;
        CachedWidth, CachedHeight : integer;
        ClientTPA : TPointArray;
        hueMod, satMod: Extended;
        CTS: Integer;
    end;

implementation
uses
    Client, // For the Client Casts.
    colour_conv // For RGBToColor, etc.
    ;
type
  TPRGB32Array = array of PRGB32;

procedure TMFinder.LoadSpiralPath(startX, startY, x1, y1, x2, y2: Integer);
var
  i,y,x,c,Ring : integer;
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
  for i := 0 to RowCount - 1do
  begin;
    result[i] := ReturnData.Ptr;
    inc(ReturnData.Ptr,ReturnData.IncPtrWith);
  end;
end;

function CalculateRowPtrs(Bitmap : TMufasaBitmap) : TPRGB32Array;overload;
var
  I : integer;
begin;
  setlength(result,Bitmap.Height);
  for i := 0 to Bitmap.Height - 1 do
    result[i] := Bitmap.FData + Bitmap.Width;
end;

constructor TMFinder.Create(aClient: TObject);

begin
  inherited Create;

  Self.Client := aClient;
  Self.CTS := 1;
  Self.hueMod := 0.2;
  Self.satMod := 0.2;

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
       RGBToHSL(R1,g1,b1,H1,S1,L1);
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
{  if xs > xe then
    Swap(xs,xe);
  if ys > ye then
    Swap(ys,ye);}
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

function TMFinder.FindColor(var x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
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

function TMFinder.FindColorTolerance(var x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2: Extended;

   label Hit;
   label Miss;

begin
  Result := false;
  // checks for valid xs,ys,xe,ye? (may involve GetDimensions)
  DefaultOperations(xs,ys,xe,ye);

  // calculate delta x and y
  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  ColorToHSL(Color, H1, S1, L1);

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
    // and hueMod * tol is also calculated every time.
    begin
    for yy := ys to ye do
      for xx := xs to xe do
      begin
         RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,H2,S2,L2);
         if ((abs(H1 - H2) <= (hueMod * tol)) and (abs(S1 - S2) <= (satMod * tol)) and (abs(L1 - L2) <= Tol)) then
            goto Hit;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
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

function TMFinder.FindColorsTolerance(var Points: TPointArray; Color, xs, ys,
  xe, ye, Tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc,C: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2: Extended;
begin
  Result := false;
  DefaultOperations(xs,ys,xe,ye);

  dX := xe - xs;
  dY := ye - ys;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  ColorToHSL(Color, H1, S1, L1);

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
    for yy := ys to ye do
      for xx := xs to xe do
      begin
         RGBToHSL(Ptr^.R,Ptr^.G,Ptr^.B,H2,S2,L2);
         if ((abs(H1 - H2) <= (hueMod * tol)) and (abs(S1 - S2) <= (satMod * tol)) and (abs(L1 - L2) <= Tol)) then
         begin;
           ClientTPA[c].x := xx;
           ClientTPA[c].y := yy;
           inc(c);
         end;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
    end;
  end;
  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColorsSpiralTolerance(x, y: Integer;
  var Points: TPointArray; color, xs, ys, xe, ye: Integer; Tolerance: Integer
  ): boolean;
var
   PtrData: TRetData;
   c : integer;
   RowData : TPRGB32Array;
   dX, dY, clR, clG, clB, i,SpiralHi: Integer;
   H1, S1, L1, H2, S2, L2: Extended;
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
    for i := 0 to SpiralHi do
    begin;
      RGBToHSL(RowData[ClientTPA[i].y][ClientTPA[i].x].R,
               RowData[ClientTPA[i].y][ClientTPA[i].x].G,
               RowData[ClientTPA[i].y][ClientTPA[i].x].B,H2,S2,L2);
      if ((abs(H1 - H2) <= (hueMod * Tolerance)) and (abs(S1 - S2) <= (satMod * Tolerance)) and (abs(L1 - L2) <= Tolerance)) then
      begin;
        ClientTPA[c].x := ClientTPA[i].x + xs;
        ClientTPA[c].y := ClientTPA[i].y + ys;
        inc(c);
      end;
    end;
  end;
  SetLength(Points, C);
  Move(ClientTPA[0], Points[0], C * SizeOf(TPoint));
  Result := C > 0;
  TClient(Client).MWindow.FreeReturnData;
end;

function TMFinder.FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
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

function TMFinder.FindBitmap(bitmap: TMufasaBitmap; var x, y: Integer): Boolean;
var
  w,h : integer;
begin
  TClient(Client).MWindow.GetDimensions(w,h);
  result := Self.FindBitmapIn(bitmap,x,y,0,0,w-1,h-1);
end;

function TMFinder.FindBitmapIn(bitmap: TMufasaBitmap; var x, y: Integer; xs,
  ys, xe, ye: Integer): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY : integer;
   dX, dY,  xx, yy: Integer;
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
  for yy := 0 to dY do
    for xx := 0 to dX do
    begin;
      for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + yy;
        for xBmp := 0 to BmpW do
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

function TMFinder.FindBitmapToleranceIn(bitmap: TMufasaBitmap; var x, y: Integer; xs,
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
  //We wont want HSL comparison with BMPs, right? Not for now atleast.
  CCTS := Self.CTS;
  if CCTS > 1 then
    CCTS := 1;
  for yy := 0 to dY do
    for xx := 0 to dX do
    begin;
      for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + yy;
        for xBmp := 0 to BmpW do
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
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
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
  //NO HSL.
  CCTS := Self.CTS;
  if CCTS > 1 then
    CCTS := 1;
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
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
  y: Integer; var Points: TPointArray; xs, ys, xe, ye,tolerance: Integer): Boolean;
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
  //NO HSL.
  CCTS := Self.CTS;
  if CCTS > 1 then
    CCTS := 1;
  FoundC := 0;
  for i := 0 to HiSpiral do
  begin;
    for yBmp:= 0 to BmpH do
      begin;
        tmpY := yBmp + ClientTPA[i].y;
        for xBmp := 0 to BmpW do
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

function TMFinder.FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; var x,
  y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer;
  AllowPartialAccuracy: Boolean; var accuracy: Extended): Boolean;
var
   MainRowdata : TPRGB32Array;
   BmpRowData : TPRGB32Array;
   PtrData : TRetData;
   BmpW,BmpH : integer;
   xBmp,yBmp : integer;
   tmpY,tmpX : integer;
   dX, dY,  xx, yy: Integer;
   SearchdX,SearchdY : integer;
   CCTS : integer;
   GoodCount : integer;//Save the amount of pixels who have found a correspondening pixel
   BestCount : integer;//The best amount of pixels till now..
   BestPT : TPoint; //The point where it found the most pixels.
   RangeX,RangeY : Integer;
   H,S,L,HMod,SMod : extended;
label FoundBMPPoint;
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
  //We wont want HSL comparison with BMPs, right? Not for now atleast.
  CCTS := Self.CTS;
  if CCTS > 1 then
    CCTS := 1;
  //Reset the accuracy :-)
  Accuracy := 0;
  BestCount := -1;
  BestPT := Point(-1,-1);

  for yy := 0 to dY do
    for xx := 0 to dX do
    begin;
      GoodCount := 0;
      for yBmp:= 0 to BmpH do
      begin;
        //Calculate points of the BMP left against Goodcount (if it cannot possibly get more points skip this x,y?
        if bestCount > (goodcount + (Bmph - yBmp) * (bmpW)) then
          Break;
        for xBmp := 0 to BmpW do
        begin;

          for RangeY := (yy-Range) to (yy + Range) do
          begin;
            tmpY := yBmp + RangeY;
            if (tmpY < 0) or (tmpY > SearchdY ) then
              continue;
            for RangeX := (xx-Range) to (xx + Range) do
            begin;
              tmpX := xBmp + RangeX;
              if (tmpX < 0) or (tmpX > SearchdX) then
                Continue;
              if ColorSame(CCTS,tolerance,
                                BmpRowData[yBmp][xBmp].R,BmpRowData[yBmp][xBmp].G,BmpRowData[yBmp][xBmp].B,
                                MainRowdata[tmpY][tmpX].R,MainRowdata[tmpY][tmpX].G,MainRowdata[tmpY][tmpX].B,
                                H,S,L,HMod,SMod) then
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
      if GoodCount > BestCount then //This x,y has the best Acc so far!
      begin;
        BestCount := GoodCount;
        BestPT := Point(xx+xs,yy+ys);
      end;
    end;
  TClient(Client).MWindow.FreeReturnData;
  if BestCount = 0 then
    Exit;
  accuracy := BestCount / ((BmpW + 1) * (BmpH+1));
  if (accuracy = 1) or AllowPartialAccuracy then
  begin
    x := BestPT.x;
    y := BestPT.y;
    Exit(true);
  end;
end;

end.

