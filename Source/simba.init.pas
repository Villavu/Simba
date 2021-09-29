{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.init;

{$i simba.inc}

interface

{$IFDEF LINUX}
uses
  cthreads, cmem,
  simba.linux_initialization;
{$ENDIF}

{$IFDEF DARWIN}
uses
  cthreads, cmem,
  simba.darwin_initialization;
{$ENDIF}

implementation

uses
  classes, sysutils, lazlogger;

type
  TSimbaDebugLogger = class(TLazLoggerFile);

initialization
  DebugLogger := TSimbaDebugLogger.Create();

  Randomize();
  FormatSettings.DecimalSeparator := '.';

end.


