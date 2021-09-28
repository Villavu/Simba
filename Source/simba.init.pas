unit simba.init;

{$mode objfpc}{$H+}
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


