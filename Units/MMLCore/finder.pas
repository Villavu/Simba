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
      public
        function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
        procedure SetToleranceSpeed(nCTS: Integer);
        function SimilarColors(Color1,Color2,Tolerance : Integer) : boolean;
        // Possibly turn x, y into a TPoint var.
        function FindColor(var x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
        function FindColorTolerance(var x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
        function FindColorsTolerance(var Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean;
        function FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
        //Bitmap functions
        function FindBitmap(bitmap: TMufasaBitmap; var x, y: Integer): Boolean;
        function FindBitmapIn(bitmap: TMufasaBitmap; var x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;
        function FindBitmapToleranceIn(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;
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
  MainRowdata:= CalculateRowPtrs(PtrData,dy);
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
  MainRowdata:= CalculateRowPtrs(PtrData,dy);
  BmpRowData:= CalculateRowPtrs(bitmap);
  //Get the 'fixed' bmp size
  BmpW := bitmap.Width - 1;
  BmpH := bitmap.Height - 1;
  //Heck our bitmap cannot be outside the search area
  dX := dX - bmpW;
  dY := dY - bmpH;
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

end.

