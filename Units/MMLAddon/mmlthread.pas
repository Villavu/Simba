unit MMLThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, client;

type
    TMMLThread = class(TThread)
    protected
        Client: TClient;

        procedure Execute; override;
    public
        constructor Create(CreateSuspended: Boolean);
        destructor Destroy; override;
    end;

implementation

{
  Note to Raymond: For PascalScript, Create it on the .Create,
  Execute it on the .Execute, and don't forget to Destroy it on .Destroy.

  Furthermore, all the wrappers can be in the unit "implementation" section.
  Better still to create an .inc for it, otherwise this unit will become huge.
  (You can even split up the .inc's in stuff like color, bitmap, etc. )

  Also, don't add PS to this unit, but make a seperate unit for it.
  Unit "MMLPSThread", perhaps?

  See the TestUnit for use of this thread, it's pretty straightforward.

  It may also be wise to turn the "Importing of wrappers" into an include as
  well, it will really make the unit more straightforward to use and read.
}


constructor TMMLThread.Create(CreateSuspended : boolean);
begin
  Client := TClient.Create;
  // Create Stuff here

  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

destructor TMMLThread.Destroy;
begin
  Client.Destroy;
  inherited Destroy;
end;

procedure TMMLThread.Execute;
var
   i,w,h: Integer;

begin
  w := 0;
  h := 0;
  i := 0;
  while (not Terminated) and (i < 10) do
  begin
    Sleep(1000);
    Client.MWindow.GetDimensions(W, H);
    writeln(inttostr(w) + ', ' + inttostr(h));
    Inc(i);
  end;
end;

{ Include stuff here? }

//{$I inc/colors.inc}
//{$I inc/bitmaps.inc}


end.


