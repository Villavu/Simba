{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.linux_initialization;

{$i simba.inc}

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

