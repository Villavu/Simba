{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
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
    RequestHeader['User-Agent'] := Format('Mozilla/5.0 (compatible; Simba/%d; Target/%s; Commit/%s)', [SIMBA_VERSION, {$I %FPCTARGETOS%} + '-' + {$I %FPCTARGETCPU%}, SIMBA_COMMIT]);

    Get(SIMBA_ANALYTICS_URL);
  finally
    Free();
  end;
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@SendAnalytics, 'Analytics', True);

end.

