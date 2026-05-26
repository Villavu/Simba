{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Temporary IDE variables for the lifespan of the process
}
unit simba.ide_vars;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.process;

type
  TSimbaIDEVars = record
  private
    FProcessSelection: TProcessID;
    FWindowSelection: TWindowHandle;

    function GetWindowSelection: TWindowHandle;
    procedure SetWindowSelection(AValue: TWindowHandle);
  public
    property WindowSelection: TWindowHandle read GetWindowSelection write SetWindowSelection;
    property ProcessSelection: TProcessID read FProcessSelection write FProcessSelection;
  end;

var
  SimbaIDEVars: TSimbaIDEVars;

implementation

uses
  simba.vartype_windowhandle;

function TSimbaIDEVars.GetWindowSelection: TWindowHandle;
begin
  if not FWindowSelection.IsValid() then
    FWindowSelection := GetDesktopWindow();
  Result := FWindowSelection;
end;

procedure TSimbaIDEVars.SetWindowSelection(AValue: TWindowHandle);
begin
  FWindowSelection := AValue;
end;

end.


