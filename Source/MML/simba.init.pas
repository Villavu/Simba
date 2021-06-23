unit simba.init;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

{$IFDEF UNIX}
uses
  cthreads, cmem
  {$IFDEF LINUX},
  simba.linux_initialization
  {$ENDIF}
  {$IFDEF DARWIN},
  simba.darwin_initialization
  {$ENDIF};
{$ENDIF}

implementation

end.


