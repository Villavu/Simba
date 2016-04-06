unit mmlmemoryworkerfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mmlbasememoryworker, {$IFNDEF MSWINDOWS}mmllinuxmemoryworker{$ELSE}mmlwindowsmemoryworker{$ENDIF};

type

  { TMemoryWorkerFactory }

  TMemoryWorkerFactory = class
  public
    class function GetInstance(PID: integer): TBaseMemoryWorker;
  end;

implementation

{ TMemoryWorkerFactory }

class function TMemoryWorkerFactory.GetInstance(PID: integer)
  : TBaseMemoryWorker;
begin
{$IFNDEF MSWINDOWS}
  result := TLinuxMemoryWorker.Create(PID);
{$ELSE}
  result := TWindowsMemoryWorker.Create(PID); 
{$ENDIF}
end;

end.
