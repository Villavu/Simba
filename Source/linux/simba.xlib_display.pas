unit simba.xlib_display;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.xlib;

function GetDisplay(Name: String = ''): PDisplay;

implementation

threadvar
  Display: PDisplay;

function GetDisplay(Name: String = ''): PDisplay;
begin
  if (Display = nil) then
  begin
    WriteLn('Opening XDisplay for thread ', GetCurrentThreadID());

    Display := XOpenDisplay(PChar(Name));
    if (Display = nil) then
      raise Exception.Create('Unable to open a new display');
  end;

  Result := Display;
end;

procedure CloseDisplay;
begin
  if (Display <> nil) then
  begin
    WriteLn('Closing XDisplay for thread ', GetCurrentThreadID());

    XCloseDisplay(Display);
  end;
end;

var
  MemoryManager: TMemoryManager;

initialization
  GetMemoryManager(MemoryManager);
  MemoryManager.DoneThread := @CloseDisplay;
  SetMemoryManager(MemoryManager);

finalization
  if (Display <> nil) then
    XCloseDisplay(Display);

end.

