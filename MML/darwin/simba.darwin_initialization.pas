unit simba.darwin_initialization;

{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils;

implementation

uses
  CocoaAll;

type
  NSActivityOptions = UInt64;
  NSActivityOptionsPtr = ^NSActivityOptions;

  NSProcessInfo_NSObject = objccategory external (NSProcessInfo)
    function beginActivityWithOptions_reason(options: NSActivityOptions; reason: NSString): NSObjectProtocol; message 'beginActivityWithOptions:reason:';
    procedure endActivity(activity: NSObjectProtocol); message 'endActivity:';
  end;

const
  NSActivityIdleSystemSleepDisabled = 1 shl 20;
  NSActivityUserInitiated = $00FFFFFF + NSActivityIdleSystemSleepDisabled;
  NSActivityUserInitiatedAllowingIdleSystemSleep = NSActivityUserInitiated and (not NSActivityIdleSystemSleepDisabled);

var
  Token: NSObjectProtocol;

initialization
  Token := NSProcessInfo.processInfo.beginActivityWithOptions_reason(NSActivityUserInitiatedAllowingIdleSystemSleep, NSSTR('Don''t want to sleep'));

finalization
  NSProcessInfo.processInfo.endActivity(Token);

end.

