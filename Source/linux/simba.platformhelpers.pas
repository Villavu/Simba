{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.platformhelpers;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  TSimbaPlatformHelpers = record
    function GetProcessPath(PID: SizeUInt): String;
    function IsProcess64Bit(PID: SizeUInt): Boolean;
    function IsProcessRunning(PID: SizeUInt): Boolean;
    procedure TerminateProcess(PID: SizeUInt);
    function HighResolutionTime: Double;
  end;

var
  SimbaPlatformHelpers: TSimbaPlatformHelpers;

implementation

uses
  baseunix, unix, linux;

function TSimbaPlatformHelpers.GetProcessPath(PID: SizeUInt): String;
begin
  Result := fpReadLink('/proc/' + IntToStr(PID) + '/exe');
end;

function TSimbaPlatformHelpers.IsProcess64Bit(PID: SizeUInt): Boolean;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();
  try
    Stream.LoadFromFile(Self.GetProcessPath(PID));

    Result := (Stream.ReadByte() = $7f) and
              (Stream.ReadByte() = $45) and
              (Stream.ReadByte() = $4c) and
              (Stream.ReadByte() = $46) and
              (Stream.ReadByte() = $02);
  except
    Result := False;
  end;

  Stream.Free();
end;

function TSimbaPlatformHelpers.IsProcessRunning(PID: SizeUInt): Boolean;
begin
  Result := fpkill(PID, 0) <> 0;
end;

procedure TSimbaPlatformHelpers.TerminateProcess(PID: SizeUInt);
begin
  fpkill(PID, SIGKILL);
end;

function TSimbaPlatformHelpers.HighResolutionTime: Double;
var
  tp: TTimeVal;
  ts: TTimeSpec;
begin
  if clock_gettime(CLOCK_MONOTONIC, @ts) = 0 then
  begin
    Result := (Int64(ts.tv_sec) * 1000) + (ts.tv_nsec / 1000000);
    Exit;
  end;

  fpgettimeofday(@tp, nil);

  Result := (Int64(tp.tv_sec) * 1000) + (tp.tv_usec / 1000);
end;

end.

