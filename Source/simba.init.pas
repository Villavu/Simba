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
procedure WindowsInitialization;

  function HasOption(Option: String): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to ParamCount do
      if (ParamStr(I) = Option) then
        Exit(True);
    Exit(False);
  end;

begin
  if (not HasOption('--open')) and (HasOption('--run') or HasOption('--compile')) then
  begin
    IsConsole := True;
    SysInitStdIO();
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

end.


