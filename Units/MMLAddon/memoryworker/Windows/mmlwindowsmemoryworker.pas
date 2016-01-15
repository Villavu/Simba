unit mmlwindowsmemoryworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, mmlbasememoryworker;

type
  { TWindowsMemoryWorker }

  TWindowsMemoryWorker = class(TBaseMemoryWorker)
  protected
    function GetValue(Address: integer; ValueSize: integer; Value: Pointer)
      : boolean; override;
    function SetValue(Address: integer; ValueSize: integer; Value: Pointer)
      : boolean; override;
  public
    property PID;
  end;

implementation
 const
   ErrProcessNotFound = 'Process with pid = %d not found!';
{ TWindowsMemoryWorker }

function TWindowsMemoryWorker.GetValue(Address: integer; ValueSize: integer;
  Value: Pointer): boolean;
var
  ProcessH: integer;
  Nread: dword = 0;
begin
  ProcessH := OpenProcess(PROCESS_VM_READ, False, PID);
  if (ProcessH = 0) then
   raise Exception.Create(Format(ErrProcessNotFound,[PID]));
  try
    result := ReadProcessMemory(ProcessH, @Address, Value, ValueSize, Nread);
  finally
    CloseHandle(ProcessH);
  end;
end;

function TWindowsMemoryWorker.SetValue(Address: integer; ValueSize: integer;
  Value: Pointer): boolean;
var
  ProcessH: integer;
  NWrite: dword = 0;
begin
  ProcessH := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
  if (ProcessH = 0) then
   raise Exception.Create(Format(ErrProcessNotFound,[PID]));
  try
    result := WriteProcessMemory(ProcessH, @Address, Value, ValueSize, NWrite);
  finally
    CloseHandle(ProcessH);
  end;
end;

end.
