unit simba.linux_initialization;

{$mode objfpc}{$H+}

interface

uses
  xlib;

implementation

initialization
  XInitThreads();


end.

