unit simba.ide_analytics;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms;

implementation

uses
  simba.ide_initialization, simba.httpclient;

procedure SendAnalytics;
begin
  if Application.HasOption('noanalytics') or (SIMBA_COMMIT = '') then // SIMBA_COMMIT = '' is a local build. Don't log.
    Exit;

  with TSimbaHTTPClient.Create() do
  try
    // Simple HTTP request - nothing extra is sent.
    // Only used for logging very basic (ide) launch count.
    Get(SIMBA_ANALYTICS_URL, []);
  finally
    Free();
  end;
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnAfterCreate(@SendAnalytics, 'Analytics');

end.

