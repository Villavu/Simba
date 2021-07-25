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

