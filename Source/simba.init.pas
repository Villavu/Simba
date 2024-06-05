{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Stuff that should be initialized before anything else at startup.
}
unit simba.init;

{$i simba.inc}

{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  simba.random;

implementation

{$IFDEF DARWIN}
uses
  CocoaAll;

var
  Token: NSObjectProtocol;

procedure DarwinInitialization;
begin
  Token := NSProcessInfo.processInfo.beginActivityWithOptions_reason(NSActivityUserInitiatedAllowingIdleSystemSleep, NSSTR('Sleeping will pause scripts!'));
end;

procedure DarwinFinalization;
begin
  NSProcessInfo.processInfo.endActivity(Token);
end;
{$ENDIF}

{$IFDEF LINUX}
uses
  xlib;

procedure LinuxInitialization;
begin
  // Scripts don't run on the processes main thread. Some methods used by a script like querying font info could use xlib.
  XInitThreads();
end;
{$ENDIF}

{$IFDEF WINDOWS}
uses
  Windows;

var
  AttachedToParentConsole: Boolean = False;

procedure WindowsInitialization;
begin
  AttachedToParentConsole := AttachConsole(ATTACH_PARENT_PROCESS);
  IsConsole := True;
  SysInitStdIO();
end;

procedure WindowsFinalization;
var
  InputRec: TINPUTRECORD;
  _: DWord;
begin
  if AttachedToParentConsole then
  begin
    InputRec.EventType := KEY_EVENT;
    InputRec.Event.KeyEvent.bKeyDown := True;
    InputRec.Event.KeyEvent.AsciiChar := Char(VK_RETURN);
    InputRec.Event.KeyEvent.wVirtualKeyCode := VK_RETURN;
    InputRec.Event.KeyEvent.wVirtualScanCode := MapVirtualKey(VK_RETURN, MAPVK_VK_TO_VSC);
    InputRec.Event.KeyEvent.wRepeatCount := 1;
    InputRec.Event.KeyEvent.dwControlKeyState := 0;

    WriteConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputRec, 1, _{%H-});
  end;
end;
{$ENDIF}

initialization
  {$IFDEF DARWIN}
  DarwinInitialization();
  {$ENDIF}

  {$IFDEF LINUX}
  LinuxInitialization();
  {$ENDIF}

  {$IFDEF WINDOWS}
  WindowsInitialization();
  {$ENDIF}

  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';

  BetterRandomize();

finalization
  {$IFDEF DARWIN}
  DarwinFinalization();
  {$ENDIF}

  {$IFDEF WINDOWS}
  WindowsFinalization();
  {$ENDIF}

end.
