{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.darwin_initialization;

{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils;

implementation

uses
  CocoaAll;

var
  Token: NSObjectProtocol;

initialization
  Token := NSProcessInfo.processInfo.beginActivityWithOptions_reason(NSActivityUserInitiatedAllowingIdleSystemSleep, NSSTR('Sleeping will pause scripts!'));

finalization
  NSProcessInfo.processInfo.endActivity(Token);

end.

