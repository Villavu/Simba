unit simba.darwin_initialization;

{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils;

implementation

uses
  CocoaAll;

type

  NSProcessInfo_NSObject = objccategory external (NSProcessInfo)
    function beginActivityWithOptions_reason(options: UInt64; reason: NSString): NSObjectProtocol; message 'beginActivityWithOptions:reason:';
    procedure endActivity(activity: NSObjectProtocol); message 'endActivity:';
  end;

const
  NSActivityIdleSystemSleepDisabled = 1 shl 20;
  NSActivityUserInitiated = $00FFFFFF + NSActivityIdleSystemSleepDisabled;
  NSActivityUserInitiatedAllowingIdleSystemSleep = NSActivityUserInitiated and (not NSActivityIdleSystemSleepDisabled);

var
  Token: NSObjectProtocol;

initialization
  Token := NSProcessInfo.processInfo.beginActivityWithOptions_reason(NSActivityUserInitiatedAllowingIdleSystemSleep, NSSTR('Sleeping will pause scripts!'));

finalization
  NSProcessInfo.processInfo.endActivity(Token);

end.

