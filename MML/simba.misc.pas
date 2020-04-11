{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    MMisc for the Mufasa Macro Library
}
unit simba.misc;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, process;

procedure ConvertTime(Time: Int64; var h, m, s: Int32);
procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Int32);
function TimeStamp(Time: Int64; IncludeMilliseconds: Boolean = False): String;
function PerformanceTimer: Double;
function GetCommandLine: TStringArray;
function RunCommandInDir(Directory: String; CommandLine: String; out OutputString: String): Int32; overload;
procedure RunCommandInDir(Directory: String; CommandLine: String); overload;
function RunCommand(CommandLine: String; out OutputString: String): Int32; overload;
procedure RunCommand(CommandLine: String); overload;

function OpenDirectory(Path: String): Boolean;

implementation

uses
  {$IFDEF WINDOWS}
  Windows
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix
  {$ENDIF};

procedure ConvertTime(Time: Int64; var h, m, s: Int32);
var
  x: Int64;
begin
  x := time;
  h := x div (3600000);
  x := x mod (3600000);
  m := x div (60000);
  x := x mod (60000);
  s := x div (1000);
end;

procedure ConvertTime64(Time: Int64; var y, m, w, d, h, min, s: Int32);
var
  x: Int64;
begin
  x := time;
  y := x div (31536000000); // 1000 * 60 * 60 * 24 * 365 (1 year or 365 days)
  x := x mod (31536000000);
  m := x div (2592000000); // 1000 * 60 * 60 * 24 * 30 (1 month or 30 days)
  x := x mod (2592000000);
  w := x div (604800000); // 1000 * 60 * 60 * 24 * 7 (1 week or 7 days)
  x := x mod (604800000);
  d := x div (86400000); // 1000 * 60 * 60 * 24 (1 day or 24 hours)
  x := x mod (86400000);
  h := x div (3600000); // 1000 * 60 * 60 (1 hour or 60 minutes)
  x := x mod (3600000);
  min := x div (60000); // 1000 * 60 (1 minute or 60 seconds)
  x := x mod (60000);
  s := x div (1000); // 1000 (1 second)
  x := x mod (1000);
end;

function TimeStamp(Time: Int64; IncludeMilliseconds: Boolean): String;
var
  Hours, Mins, Secs, Milliseconds: Int32;
begin
  Hours := Time div 3600000;
  Time  := Time mod 3600000;
  Mins  := Time div 60000;
  Time  := Time mod 60000;
  Secs  := Time div 1000;
  Milliseconds  := Time mod 1000;

  if IncludeMilliseconds then
    Result := Format('[%.2d:%.2d:%.2d:%.3d]', [Hours, Mins, Secs, Milliseconds])
  else
    Result := Format('[%.2d:%.2d:%.2d]', [Hours, Mins, Secs]);
end;

function PerformanceTimer: Double;
var
  Frequency, Count: Int64;
  {$IFDEF UNIX}
  TV: TTimeVal;
  TZ: PTimeZone;
  {$ENDIF}
begin
  Result := 0;

  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(Count);
  Result := Count / Frequency * 1000;
  {$ENDIF}

  {$IFDEF UNIX}
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  Count := Int64(TV.tv_sec) * 1000000 + Int64(TV.tv_usec);
  Result := Count / 1000;
  {$ENDIF}
end;

function GetCommandLine: TStringArray;
var
  i: Int32;
begin
  SetLength(Result, ParamCount + 1);
  for i := 0 to ParamCount do
    Result[i] := ParamStr(i);
end;

function RunCommandInDir(Directory: String; CommandLine: String; out OutputString: String): Int32;
const
  BUFFER_SIZE = 1024 * 16;
var
  Commands: TStringList;
  I, Count: Int32;
  Buffer: array[1..BUFFER_SIZE] of Char;
begin
  Result := -1;
  SetLength(OutputString, 0);

  Commands := TStringList.Create();

  try
    CommandToList(CommandLine, Commands);
    if (Commands.Count = 0) then
      Exit;

    with TProcess.Create(nil) do
    try
      Options := [poUsePipes];
      PipeBufferSize := BUFFER_SIZE;
      CurrentDirectory := Directory;
      Executable := Commands[0];

      {$IFDEF WINDOWS}
      if ExtractFileExt(Executable) = '' then
        Executable := Executable + '.exe';
      {$ENDIF}

      for I := 1 to Commands.Count - 1 do
        Parameters.Add(Commands[I]);

      Execute();

      while Running do
      begin
        while (Output.NumBytesAvailable > 0) do
        begin
          Count := Output.Read(Buffer[1], BUFFER_SIZE);
          if Count > 0 then
            OutputString := OutputString + Copy(Buffer, 1, Count);
        end;

        Sleep(50);
      end;

      // Make sure we empty the output
      while (Output.NumBytesAvailable > 0) do
      begin
        Count := Output.Read(Buffer[1], BUFFER_SIZE);
        if (Count > 0) then
          OutputString := OutputString + Copy(Buffer, 1, Count);
      end;

      Result := ExitStatus;
    finally
      Free();
    end;
  finally
    Commands.Free();
  end;
end;

procedure RunCommandInDir(Directory: String; CommandLine: String);
const
  BUFFER_SIZE = 1024 * 16;
var
  Commands: TStringList;
  I: Int32;
  Buffer: array[1..BUFFER_SIZE] of Char;
begin
  Commands := TStringList.Create();

  try
    CommandToList(CommandLine, Commands);
    if (Commands.Count = 0) then
      Exit;

    with TProcess.Create(nil) do
    try
      CurrentDirectory := Directory;
      Executable := Commands[0];

      {$IFDEF WINDOWS}
      if ExtractFileExt(Executable) = '' then
        Executable := Executable + '.exe';
      {$ENDIF}

      for I := 1 to Commands.Count - 1 do
        Parameters.Add(Commands[I]);

      Execute();
    finally
      Free();
    end;
  finally
    Commands.Free();
  end;
end;

function RunCommand(CommandLine: String; out OutputString: String): Int32;
begin
  Result := RunCommandInDir(GetCurrentDir(), CommandLine, OutputString);
end;

procedure RunCommand(CommandLine: String);
begin
  RunCommandInDir(GetCurrentDir(), CommandLine);
end;

function OpenDirectory(Path: String): Boolean;
var
  Executable: String = '';
  Output: String;
  ExitStatus: Int32;
begin
  {$IFDEF WINDOWS}
  Executable := 'explorer.exe';
  Path := '/root,"' + Path + '"';
  {$ENDIF}

  {$IFDEF LINUX}
  Executable := 'xdg-open';
  {$ENDIF}

  {$IFDEF DARWIN}
  Executable := 'open';
  {$ENDIF}

  if (Executable = '') then
    raise Exception.Create('OpenDirectory is unsupported on this system.');

  Result := RunCommandInDir('', Executable, [Path], Output, ExitStatus) = 0;
end;

end.

