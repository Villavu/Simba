unit linux_startup;

{$mode objfpc}{$H+}

interface

uses
  pthreads, xlib;

implementation

initialization
  XInitThreads();

  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, nil); // for script thread force terminate

end.

