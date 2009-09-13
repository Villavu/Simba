unit finder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,    MufasaTypes; // Types

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
        procedure DefaultOperations(var x1,y1,x2,y2 : integer);
      public
          procedure SetToleranceSpeed(nCTS: Integer);
          function SimilarColors(Color1,Color2,Tolerance : Integer) : boolean;
          // Possibly turn x, y into a TPoint var.
          function FindColor(var x, y: Integer; Color, x1, y1, x2, y2: Integer): Boolean;
          function FindColorTolerance(var x, y: Integer; Color, x1, y1, x2, y2, tol: Integer): Boolean;

          function FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Integer): Boolean;
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

procedure TMFinder.UpdateCachedValues(NewWidth, NewHeight: integer);
begin
  CachedWidth := NewWidth;
  CachedHeight := NewHeight;
  SetLength(ClientTPA,NewWidth * NewHeight);
end;

procedure TMFinder.DefaultOperations(var x1, y1, x2, y2: integer);
var
  w,h : integer;
begin
{  if x1 > x2 then
    Swap(x1,x2);
  if y1 > y2 then
    Swap(y1,y2);}
  if x1 < 0 then
    x1 := 0;
  if y1 < 0 then
    y1 := 0;
  TClient(Self.Client).MWindow.GetDimensions(w,h);
  if (w <> CachedWidth) or (h <> CachedHeight) then
    UpdateCachedValues(w,h);
  if x2 >= w then
    x2 := w-1;
  if y2 >= h then
    y2 := h-1;
end;

function TMFinder.FindColor(var x, y: Integer; Color, x1, y1, x2, y2: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;

begin

  // checks for valid x1,y1,x2,y2? (may involve GetDimensions)
  DefaultOperations(x1,y1,x2,y2);

  // calculate delta x and y
  dX := x2 - x1;
  dY := y2 - y1;

  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);

  PtrData := TClient(Client).MWindow.ReturnData(x1, y1, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  for yy := y1 to y2 do
  begin;
    for xx := x1 to x2 do
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

function TMFinder.FindColorTolerance(var x, y: Integer; Color, x1, y1, x2, y2, tol: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;
   H1, S1, L1, H2, S2, L2: Extended;

   label Hit;
   label Miss;

begin

  // checks for valid x1,y1,x2,y2? (may involve GetDimensions)
  DefaultOperations(x1,y1,x2,y2);

  // calculate delta x and y
  dX := x2 - x1;
  dY := y2 - y1;
  //next, convert the color to r,g,b
  ColorToRGB(Color, clR, clG, clB);
  ColorToHSL(Color, H1, S1, L1);

  PtrData := TClient(Client).MWindow.ReturnData(x1, y1, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  case CTS of
    0:
    for yy := y1 to y2 do
    begin
      for xx := x1 to x2 do
      begin
         if ((abs(clR-Ptr^.R) <= Tol) and (abs(clG-Ptr^.G) <= Tol) and (Abs(clG-Ptr^.B) <= Tol)) then
            goto Hit;
        inc(Ptr);
      end;
      Inc(Ptr, PtrInc);
    end;

    1:
    for yy := y1 to y2 do
    begin
      for xx := x1 to x2 do
      begin
         if (Sqrt(sqr(clR-Ptr^.R) + sqr(clG - Ptr^.G) + sqr(clB - Ptr^.B)) <= Tol) then
            goto Hit;
        inc(ptr);
      end;
      Inc(Ptr, PtrInc);
    end;
    2:
    begin
    for yy := y1 to y2 do
      for xx := x1 to x2 do
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

function TMFinder.FindColors(var TPA: TPointArray; Color, x1, y1, x2, y2: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy, i: Integer;

begin
  DefaultOperations(x1,y1,x2,y2);

  dX := x2 - x1;
  dY := y2 - y1;

  I := 0;

  ColorToRGB(Color, clR, clG, clB);

  PtrData := TClient(Client).MWindow.ReturnData(x1, y1, dX + 1, dY + 1);

  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  for yy := y1 to y2 do
  begin;
    for xx := x1 to x2 do
    begin;
      if (Ptr^.R = clR) and (Ptr^.G = clG) and (Ptr^.B = clB) then
      begin
        Self.ClientTPA[I] := Point(xx, yy);
        Inc(I);
      end;
      Inc(Ptr);
    end;
    Inc(Ptr, PtrInc);
  end;

  SetLength(TPA, I);

  Move(ClientTPA[0], TPA[0], i * SizeOf(TPoint));
 { for xx := 0 to I - 1 do
    TPA[xx] := ClientTPA[xx];}
  Result := I > 0;

  TClient(Client).MWindow.FreeReturnData;
end;

end.

