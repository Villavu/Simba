unit finder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

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
      public
          // Possibly turn x, y into a TPoint var.
          function FindColor(var x, y: Integer; Color, x1, y1, x2, y2: Integer): Boolean;
      protected
        Client: TObject;
      private

    end;

implementation
uses
    Client, // For the Client Casts.
    MufasaTypes // Types
    ;

constructor TMFinder.Create(aClient: TObject);

begin
  inherited Create;

  Self.Client := aClient;

end;

destructor TMFinder.Destroy;
begin

  inherited;
end;

function TMFinder.FindColor(Var x, y: Integer; Color, x1, y1, x2, y2: Integer): Boolean;
var
   PtrData: TRetData;
   Ptr: PRGB32;
   PtrInc: Integer;
   dX, dY, clR, clG, clB, xx, yy: Integer;

begin

  // checks for valid x1,y1,x2,y2? (may involve GetDimensions)

  {if bla > bla) then etc }

  // calculate delta x and y
  dX := x2 - x1;
  dY := y2 - y1;
  //next, convert the color to r,g,b
  {
     ColorToRGB(Color, clR, clG, clB);
  }

  PtrData := TClient(Client).MWindow.ReturnData(x1, y1, dX, dY);

  // Do we want to "cache" these vars?
  // We will, for now. Easier to type.
  Ptr := PtrData.Ptr;
  PtrInc := PtrData.IncPtrWith;

  for yy := 0 to dY do
  begin
    for xx := 0 to dX do
    begin
      // Colour comparison here. Possibly with tolerance? ;)
      if (Ptr^.R = clR) and (Ptr^.G = clG) and (Ptr^.B = clB) then
      begin
        {
         If we are only looking for one colour, result = true, free data, exit.

         Else, add to the "hit" tpa, and increate the count.
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

