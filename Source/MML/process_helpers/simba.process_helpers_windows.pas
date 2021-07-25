unit simba.process_helpers_windows;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils;

type
  TSimbaWindowsProcessHelpers = record
    function GetProcessPath(PID: SizeUInt): String;
    function IsProcess64Bit(PID: SizeUInt): Boolean;
    function IsProcessRunning(PID: SizeUInt): Boolean;
    procedure TerminateProcess(PID: SizeUInt);
  end;

var
  SimbaProcessHelpers: TSimbaWindowsProcessHelpers;

implementation

uses
  windows;

function TSimbaWindowsProcessHelpers.GetProcessPath(PID: SizeUInt): String;
var
  Buffer: array[1..MAX_PATH] of Char;
  BufferSize: UInt32 = MAX_PATH;
  Handle: THandle;
begin
  Result := '';

  Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
  if (Handle > 0) then
  begin
    if QueryFullProcessImageNameA(Handle, 0, @Buffer[1], @BufferSize) then
      Result := Copy(Buffer, 1, BufferSize);

    CloseHandle(Handle);
  end;
end;

function TSimbaWindowsProcessHelpers.IsProcess64Bit(PID: SizeUInt): Boolean;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
var
  Handle: THandle;
  SystemInfo: TSystemInfo;
  Wow64Process: LongBool;
begin
  Result := False;

  GetNativeSystemInfo(@SystemInfo);

  if (SystemInfo.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) then
  begin
    Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
    if (Handle = 0) then
      Exit;

    Result := IsWow64Process(Handle, @Wow64Process) and (not Wow64Process);

    CloseHandle(Handle);
  end;
end;

function TSimbaWindowsProcessHelpers.IsProcessRunning(PID: SizeUInt): Boolean;
var
  Handle: THandle;
  ExitCode: UInt32 = 0;
begin
  Result := False;

  Handle := OpenProcess(SYNCHRONIZE or PROCESS_QUERY_LIMITED_INFORMATION, False, PID);
  if (Handle = 0) then
    Exit;

  Result := GetExitCodeProcess(Handle, ExitCode) and (ExitCode = STILL_ACTIVE);

  CloseHandle(Handle);
end;

procedure TSimbaWindowsProcessHelpers.TerminateProcess(PID: SizeUInt);
var
  Handle: THandle;
begin
  Handle := OpenProcess(PROCESS_TERMINATE, False, PID);
  if (Handle = 0) then
    Exit;

  Windows.TerminateProcess(Handle, 0);

  CloseHandle(Handle);
end;

end.

