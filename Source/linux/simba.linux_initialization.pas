unit simba.linux_initialization;

{$mode objfpc}{$H+}

interface

uses
  xlib;

implementation

initialization
  // Scripts do not run on the main thread and something like TCanvas.TextOut
  // Will use this xlib instance so we must initialize multithread support.
  XInitThreads();

  // XInitThread is also called on the simba.xlib instance at initialization.
  // We need another instance of xlib because a script can raise a valid x error
  // which lazarus will catch and crash us all.
  // Read Simba.xlib for more


end.

