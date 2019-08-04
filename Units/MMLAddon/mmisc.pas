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
unit mmisc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils, bzip2,
  bzip2comn, bzip2stream,
  libtar, mufasabase, mufasatypes,
  LazUTF8;

function DecompressBZip2(const input : TStream; const BlockSize : Cardinal = 4096) : TMemoryStream;
function UnTar(const Input : TStream) : TStringArray;overload;
function UnTar(const Input : TStream;const outputdir : string; overwrite : boolean): boolean;overload;

procedure ConvertTime(Time : int64; var h,m,s : integer);
procedure ConvertTime64(time: int64; var y, m, w, d, h, min, s: integer);
function TimeStamp(Time: Int64): String;
function MarkTime: Double;

type
  { TProcThread }
  TProcThread = class(TThread)
  public
    StartWait : Cardinal;
    ClassProc : procedure of object;
    NormalProc : procedure;
    constructor Create;
    procedure Execute; override;
  end;

  { TDecompressThread }

  TDecompressThread = class(TThread)
  private
    FFinished : boolean;
    Finput : TStream;
    FBlockSize : Cardinal;
  public
    Result : TMemoryStream;
    constructor Create(const input : TStream; const BlockSize : Cardinal = 4096);
    procedure Execute; override;
    property Finished : boolean read FFinished;
  end;

  { TUntarThread }

  TUntarThread = class(TThread)
  private
    FFinished : boolean;
    Finput : TStream;
    FOverWrite : boolean;
    FOutputDir : string;
  public
    Result : boolean;
    constructor Create(const Input : TStream;const outputdir : string; overwrite : boolean);
    procedure Execute; override;
    property Finished : boolean read FFinished;
  end;

  { TDownloadDecompressThread }

  TDownloadDecompressThread = class(TThread)
  private
    InputURL : string;
    Folder : string;
    FData : string;
    FOverWrite : boolean;
  public
    Done : boolean;
    Succeeded : boolean;
    property Rawdata : string read FData;
    constructor Create(const URL, OutputDir : string; const Overwrite : boolean);
    procedure execute; override;
  end;
implementation

uses
  FileUtil, Internets,
  {$IFDEF WINDOWS} Windows {$ELSE} BaseUnix, Unix {$ENDIF};

function DecompressBZip2(const input: TStream; const BlockSize: Cardinal): TMemoryStream;
var
  Unzipper : TDecompressBzip2Stream;
  Blocks : array of Byte;
  ReadSize : cardinal;
begin
  SetLength(Blocks,BlockSize);
  try
    Unzipper := TDecompressBzip2Stream.Create(input);
  except
    on e : exception do
    begin;
      mDebugLn(e.message);
      exit;
    end;
  end;
  Result := TMemoryStream.Create;
  try
    repeat
      ReadSize := BlockSize;
      ReadSize := Unzipper.read(blocks[0],readsize);  //Read ReadSize amount of bytes.
      Result.Write(Blocks[0],ReadSize);
    until readsize = 0;
  except
    on e : EBzip2 do
     if E.ErrCode <> bzip2_endoffile then
       raise Exception.CreateFmt('Decompression error: %s %d',[e.message,e.errcode]);
  end;
  Unzipper.Free;
end;

function UnTar(const Input : TStream) : TStringArray;overload;
var
  Tar : TTarArchive;
  DirRec : TTarDirRec;
  Len : integer;
begin;
  Tar := TTarArchive.Create(input);
  Tar.reset;
  Len := 0;
  while Tar.FindNext(DirRec) do
  begin
    inc(len);
    SetLength(result,len*2);
    result[len*2-2] := DirRec.Name;
    result[len*2-1] := Tar.ReadFile;
  end;
  Tar.Free;
end;

function UnTar(const Input: TStream; const outputdir: string; overwrite: boolean): boolean; overload;
var
  Tar : TTarArchive;
  Succ : boolean;
  DirRec : TTarDirRec;
  FS : TFileStream;
begin;
  result := false;
  if not DirectoryExists(outputdir) then
    if not CreateDir(outputdir) then
      exit;
  Tar := TTarArchive.Create(input);
  Tar.reset;
  Succ := True;
  while Tar.FindNext(DirRec) do
  begin
    if (DirRec.FileType = ftDirectory) then
    begin;
      if not DirectoryExists(outputdir + DirRec.Name) and not CreateDir(outputdir + DirRec.Name) then
      begin
        Succ := false;
        break;
      end;
    end else if (DirRec.FileType = ftNormal) then
    begin;
      if FileExists(outputdir + dirrec.name) and not overwrite then
        continue;
      try
        FS := TFileStream.Create(UTF8ToSys(outputdir +dirrec.name),fmCreate);
        tar.ReadFile(fs);
        FS.Free;
      except
        Succ := false;
        break;
      end;
    end else
      mDebugLn(format('Unknown filetype in archive. %s',[dirrec.name]));
  end;
  Tar.Free;
  Result := Succ;
end;

{ TDownloadDecompressThread }

constructor TDownloadDecompressThread.Create(const URL, OutputDir: string; const Overwrite : boolean);
begin
  inherited Create(true);
  FreeOnTerminate := False;
  InputURL:= URL;
  Folder:= OutputDir;
  Done := false;
  FData := '';
  Succeeded:= false;
  FOverWrite:= Overwrite;
end;

procedure TDownloadDecompressThread.execute;
var
  SStream : TStringStream;
  DecompressData : TMemoryStream;
begin
  Succeeded:= False;
  try
    if ((InputURL = '') or (Folder = '')) then
      Exit;
    FData := Getpage(InputURL);
    if FData = '' then
      Exit; //Goes to finally
    SStream := TStringStream.Create(FData);
    DecompressData := DecompressBZip2(SStream);
    if DecompressData = nil then
      Exit; //Goes to finally
    if not Untar(DecompressData,Folder,FOverwrite) then
      Exit;
    DecompressData.Free;
    SStream.free;
    Succeeded:= True;
  finally
    Done := true;
  end;
end;

constructor TProcThread.Create;
begin
  inherited Create(true);
  FreeOnTerminate := True;
end;


{ TProcThread }

procedure TProcThread.Execute;
begin
  if startwait <> 0 then
    sleep(StartWait);
  if NormalProc <> nil then
    NormalProc;
  if ClassProc <> nil then
    ClassProc;
end;

constructor TDecompressThread.Create(const input: TStream;
  const BlockSize: Cardinal);
begin
  inherited Create(True);
  FFinished:= False;
  FBlockSize:= BlockSize;
  FInput := Input;
  Result := nil;
end;

{ TDecompressThread }

procedure TDecompressThread.Execute;
begin
  if Finput <> nil then
    result := DecompressBZip2(Finput,FBlocksize);
  Ffinished := True;
end;

{ TUntarThread }

constructor TUntarThread.Create(const Input: TStream; const outputdir: string;
  overwrite: boolean);
begin
  inherited Create(true);
  FFinished:= false;
  FInput := Input;
  FOutputDir:= OutputDir;
  FOverWrite:= overwrite;
  Result:= False;
end;

procedure TUntarThread.Execute;
begin
  if (Finput <> nil) and (FOutputDir <> '') then
    result := UnTar(FInput,Foutputdir,FOverWrite);
  FFinished:= True;
end;

procedure ConvertTime(Time: int64; var h, m, s: integer);
var
  x : int64;
begin;
  x := time;
  h := x div (3600000);
  x := x mod (3600000);
  m := x div (60000);
  x := x mod (60000);
  s := x div (1000);
end;

procedure ConvertTime64(time: int64; var y, m, w, d, h, min, s: integer);
var
  x : int64;
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

function TimeStamp(Time: Int64): String;
var
  Hours, Mins, Secs: Int32;
begin
  Hours := Time div 3600000;
  Time  := Time mod 3600000;
  Mins  := Time div 60000;
  Time  := Time mod 60000;
  Secs  := Time div 1000;
  Time  := Time mod 1000;

  Result := Format('[%.2d:%.2d:%.2d:%.3d] ', [Hours, Mins, Secs, Time]);
end;

function MarkTime: Double;
var
  Frequency, Count: Int64;
  {$IFDEF UNIX}
  TV: TTimeVal;
  TZ: PTimeZone;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(Frequency);
  QueryPerformanceCounter(Count);
  Result := Count / Frequency * 1000;
  {$ELSE}
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  Count := Int64(TV.tv_sec) * 1000000 + Int64(TV.tv_usec);
  Result := Count / 1000;
  {$ENDIF}
end;

end.

