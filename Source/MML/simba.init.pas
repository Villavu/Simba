unit simba.init;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads, cmem
  {$ENDIF}
  {$IFDEF LINUX},
  simba.linux_initialization,
  {$ENDIF}
  {$IFDEF DARWIN},
  simba.darwin_initialization,
  {$ENDIF}
  Classes, SysUtils;

implementation

initialization
  Randomize();

  FormatSettings.DecimalSeparator := '.';

end.


