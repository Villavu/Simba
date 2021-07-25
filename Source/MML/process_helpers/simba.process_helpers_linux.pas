unit simba.process_helpers_linux;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  TSimbaLinuxProcessHelpers = record
    function GetProcessPath(PID: SizeUInt): String;
    function IsProcess64Bit(PID: SizeUInt): Boolean;
    function IsProcessRunning(PID: SizeUInt): Boolean;
    procedure TerminateProcess(PID: SizeUInt);
  end;

var
  SimbaProcessHelpers: TSimbaLinuxProcessHelpers;

implementation

uses
  baseunix;

function TSimbaLinuxProcessHelpers.GetProcessPath(PID: SizeUInt): String;
begin
  Result := fpReadLink('/proc/' + IntToStr(PID) + '/exe');
end;

function TSimbaLinuxProcessHelpers.IsProcess64Bit(PID: SizeUInt): Boolean;
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

function TSimbaLinuxProcessHelpers.IsProcessRunning(PID: SizeUInt): Boolean;
begin
  Result := fpkill(PID, 0) <> 0;
end;

procedure TSimbaLinuxProcessHelpers.TerminateProcess(PID: SizeUInt);
begin
  fpkill(PID, SIGKILL);
end;

end.

