unit linux_startup;

{$mode objfpc}{$H+}

interface

uses
  xlib, pthreads;

implementation

initialization
  XInitThreads(); // for xlib usage on the script thread

  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, nil); // for script thread force terminate

end.

