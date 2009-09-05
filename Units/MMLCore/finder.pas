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
          // Possibly turn x, y into a TPoint var.
          function FindColor(var x, y: Integer; Color, x1, y1, x2, y2: Integer): Boolean;
      protected
        Client: TObject;
        CachedWidth, CachedHeight : integer;
        ClientTPA : TPointArray;
        //CTS : integer;

      private

    end;

implementation
uses
    Client; // For the Client Casts.


constructor TMFinder.Create(aClient: TObject);

begin
  inherited Create;

  Self.Client := aClient;

end;

destructor TMFinder.Destroy;
begin

  inherited;
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
  {
     ColorToRGB(Color, clR, clG, clB);
  }

  PtrData := TClient(Client).MWindow.ReturnData(x1, y1, dX + 1, dY + 1);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

{ for yy := 0 to dY do
  begin
    for xx := 0 to dX do
    begin}
//Since we do an Inc on the Ptr, no need to start with an y:=0 value, unless it's faster ofcourse.
  for yy := y1 to y2 do
  begin;
    for xx := x1 to x2 do
    begin;
      // Colour comparison here. Possibly with tolerance? ;)
      if (Ptr^.R = clR) and (Ptr^.G = clG) and (Ptr^.B = clB) then
      begin
        {
         If we are only looking for one colour, result = true, free data, exit.

         Else, add to the "hit" tpa, and increate the count.
         Note to Wizzuop: FindColor doesnt have a TPA, dummy.
        }

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

end.

