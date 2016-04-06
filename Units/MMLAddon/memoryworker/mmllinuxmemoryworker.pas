unit mmllinuxmemoryworker;

{$mode objfpc}{$H+}

{ .$DEFINE OLDKERNEL }
interface

uses
  Classes, SysUtils, mmlbasememoryworker;

type

  { TLinuxMemoryWorker }

  TLinuxMemoryWorker = class(TBaseMemoryWorker)
  protected
    function Attach: boolean; override;
    function Detach: boolean; override;
    function GetValue(Address: integer; ValueSize: integer; Value: Pointer)
      : boolean; override;
    function SetValue(Address: integer; ValueSize: integer; Value: Pointer)
      : boolean; override;
  public
    property PID;
  end;

implementation
{$IFDEF OLDKERNEL}
uses libc;
{$ENDIF}
const
  ErrFailedToAttach = 'Failed to attach to %d!';
  ErrSigStopWaiting = 'There was an error waiting for the target to stop!';

function process_vm_readv(PID: pid_t; local_iov: piovec; liovcnt: ulong;
  remote_iov: piovec; riovcnt: ulong; flags: ulong): ssize_t; cdecl;
  external clib name 'process_vm_readv';
function process_vm_writev(PID: pid_t; local_iov: piovec; liovcnt: ulong;
  remote_iov: piovec; riovcnt: ulong; flags: ulong): ssize_t; cdecl;
  external clib name 'process_vm_writev';
{ TLinuxMemoryWorker }

function TLinuxMemoryWorker.Attach: boolean;
var
  Status: longint = 0;
begin
  if PTrace(PTRACE_ATTACH, pid_t(PID), nil, 0) = -1 then
  begin
    result := false;
    raise Exception.Create(Format(ErrFailedToAttach, [PID]));
  end;
  if (WaitPid(PID, @Status, 0) = -1) or not WIFSTOPPED(Status) then
  begin
    result := false;
    raise Exception.Create(ErrSigStopWaiting);
  end;
  result := true;
end;

function TLinuxMemoryWorker.Detach: boolean;
begin
  result := PTrace(PTRACE_DETACH, PID, nil, nil) > -1;
end;

{$IFDEF OLDKERNEL}

function TLinuxMemoryWorker.GetValue(Address: integer; ValueSize: integer;
  Value: Pointer): boolean;
var
  i: integer;
  x: longint = 0;
begin
  if not Attach then
  begin
    result := false;
    exit;
  end;
  i := 0;
  while i <= ValueSize do
  begin
    x := PTrace(PTRACE_PEEKDATA, PID, Address + i, nil);
    PByteArray(Value)^[i] := x;
    inc(i);
  end;
  Detach();
  result := true;
end;

function TLinuxMemoryWorker.SetValue(Address: integer; ValueSize: integer;
  Value: Pointer): boolean;
var
  i: integer;
  x: longint = 0;
begin
  if not Attach then
  begin
    result := false;
    exit;
  end;
  for i := 0 to ValueSize - 1 do
  begin
    x := PTrace(PTRACE_POKEDATA, PID, Address + i, PByteArray(Value)^[i]);
    if errno <> 0 then
    begin
      Detach;
      result := false;
    end;
  end;
  Detach();
  result := true;
end;
{$ELSE}

function TLinuxMemoryWorker.GetValue(Address: integer; ValueSize: integer;
  Value: Pointer): boolean;
var
  local, remote: array [0 .. 0] of iovec;
  NRead: longint = 0;
begin
  result := false;
  Local[0].iov_len := ValueSize;
  Local[0].iov_base := Value;
  remote[0].iov_base := @Address;
  remote[0].iov_len := ValueSize;
  NRead := process_vm_readv(PID, @local[0], 1, @remote[0], 1, 0);
  if not NRead <> ValueSize then
    result := true;
end;

function TLinuxMemoryWorker.SetValue(Address: integer; ValueSize: integer;
  Value: Pointer): boolean;
var
  local, remote: array [0 .. 0] of iovec;
  NWritten: longint = 0;
begin
  result := false;
  Local[0].iov_len := ValueSize;
  Local[0].iov_base := Value;
  remote[0].iov_base := @Address;
  remote[0].iov_len := ValueSize;
  NWritten := process_vm_writev(PID, @local[0], 1, @remote[0], 1, 0);
  if not NWritten <> ValueSize then
    result := true;
end;
{$ENDIF}

end.
