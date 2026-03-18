{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  IDE variables for the lifespan of the process
}
unit simba.ide_vars;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.process;

var
  SimbaIDEVars: record
    WindowSelection: TWindowHandle;
    ProcessSelection: TProcessID;
  end;

implementation

end.


