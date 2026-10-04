{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  A process with an IPC connection to it.
}
unit simba.ipc_process;

{$i simba.inc}

interface

uses
  Classes, SysUtils, process,
  simba.base, simba.ipc;

type
  TSimbaIPCConnectionClass = class of TSimbaIPCConnection;

  TSimbaIPCProcess = class(TProcess)
  protected
    FCommunication: TSimbaIPCConnection;
  public
    constructor Create(AOwner: TComponent; ConnectionClass: TSimbaIPCConnectionClass); reintroduce;

    procedure Execute; override;

    property Communication: TSimbaIPCConnection read FCommunication;
  end;

implementation

constructor TSimbaIPCProcess.Create(AOwner: TComponent; ConnectionClass: TSimbaIPCConnectionClass);
begin
  inherited Create(AOwner);

  FCommunication := ConnectionClass.Create(Self);

  Parameters.Add('--ipc=' + FCommunication.ClientID);
end;

procedure TSimbaIPCProcess.Execute;
begin
  inherited Execute();

  // release pipes so the client is fully in control (basically making their RefCount = 1)
  FCommunication.CloseClientPipes();
end;

end.
