unit simba.process_helpers_darwin;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils;

type
  TSimbaDarwinProcessHelpers = record
    function GetProcessPath(PID: SizeUInt): String;
    function IsProcess64Bit(PID: SizeUInt): Boolean;
    function IsProcessRunning(PID: SizeUInt): Boolean;
    procedure TerminateProcess(PID: SizeUInt);
  end;

var
  SimbaProcessHelpers: TSimbaDarwinProcessHelpers;

implementation

uses
  baseunix;

function proc_pidpath(pid: longint; buffer: pbyte; bufferSize: longword): longint; cdecl; external 'libproc.dylib' name 'proc_pidpath';

function TSimbaDarwinProcessHelpers.GetProcessPath(PID: SizeUInt): String;
const
  PROC_PIDPATHINFO_MAXSIZE = 4096;
var
  Buffer: array[1..PROC_PIDPATHINFO_MAXSIZE] of Char;
  Len: Int32;
begin
  Result := '';

  Len := proc_pidpath(PID, @Buffer[1], Length(Buffer));
  if (Len > 0) then
    Result := Copy(Buffer, 1, Len);
end;

function TSimbaDarwinProcessHelpers.IsProcess64Bit(PID: SizeUInt): Boolean;
begin
  Result := True;
end;

function TSimbaDarwinProcessHelpers.IsProcessRunning(PID: SizeUInt): Boolean;
begin
  Result := fpkill(PID, 0) <> 0;
end;

procedure TSimbaDarwinProcessHelpers.TerminateProcess(PID: SizeUInt);
begin
  fpkill(PID, SIGKILL);
end;

end.

