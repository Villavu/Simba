{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

    Files Class for the Mufasa Macro Library
}

unit files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MufasaTypes;
const
  File_AccesError = -1;
  File_EventError = -2;

type

  { TMFiles }

  TMFiles = class(TObject)
  public
    OpenFileEvent : TOpenFileEvent;
    WriteFileEvent: TWriteFileEvent;
    function CreateFile(Path: string): Integer;
    function OpenFile(Path: string; Shared: Boolean): Integer;
    function RewriteFile(Path: string; Shared: Boolean): Integer;
    function AppendFile(Path: string): Integer;
    procedure CloseFile(FileNum: Integer);
    function EndOfFile(FileNum: Integer): Boolean;
    function FileSizeMuf(FileNum: Integer): LongInt;
    function ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
    function WriteFileString(FileNum: Integer;const s: string): Boolean;
    function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
    function FilePointerPos(FileNum: Integer): Integer;
    constructor Create(Owner : TObject);
    destructor Destroy; override;
  private
    MFiles: TMufasaFilesArray;
    FreeSpots: Array Of Integer;
    Client : TObject;
    procedure CheckFileNum(FileNum : integer);
    procedure FreeFileList;
    function AddFileToManagedList(Path: string; FS: TFileStream; Mode: Integer): Integer;
  end;

    // We don't need one per object. :-)
  function GetFiles(Path, Ext: string): TStringArray;
  function GetDirectories(Path: string): TstringArray;
  function FindFile(filename : string; Dirs : array of string) : string; //Results '' if not found

implementation
uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} IniFiles,Client,FileUtil;

{ GetFiles in independant of the TMFiles class }

function GetFiles(Path, Ext: string): TstringArray;
var
    SearchRec : TSearchRec;
    c : integer;
begin
  c := 0;
  if FindFirst(Path + '*.' + ext, faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Continue;
      inc(c);
      SetLength(Result,c);
      Result[c-1] := SearchRec.Name;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function GetDirectories(Path: string): TstringArray;
var
    SearchRec : TSearchRec;
    c : integer;
begin
  c := 0;
  if FindFirst(Path + '*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name[1] = '.') or ((SearchRec.Attr and faDirectory) <> faDirectory) then
        continue;
      inc(c);
      SetLength(Result,c);
       Result[c-1] := SearchRec.Name;
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;

function FindFile(filename : string; Dirs : array of string) : string; //Results '' if not found
var
  i : integer;
begin;
  if FileExistsUTF8(filename) then
    result := filename
  else
  begin
    for i := 0 to high(Dirs) do
      if (Dirs[i] <> '') and DirectoryExists(dirs[i]) then
        if fileexistsUTF8(dirs[i] + filename) then
        begin
          result := dirs[i] + filename;
          exit;
        end;
  end;
end;

constructor TMFiles.Create(Owner : TObject);
begin
  inherited Create;
  self.Client := Owner;
  SetLength(Self.MFiles, 0);
  SetLength(Self.FreeSpots, 0);
end;

procedure TMFiles.FreeFileList;
var
  I : integer;
begin;
  For I := 0 To High(MFiles) Do
    if MFiles[i].FS <> nil then
    begin
      TClient(Client).Writeln(Format('File[%s] has not been freed in the script, freeing it now.',[MFiles[i].Path]));
      try
        MFiles[I].FS.Free;
      except
        TClient(Client).Writeln('FreeFileList - Exception when freeing FileStream');
      end;
    end;
  SetLength(MFiles, 0);
  SetLength(FreeSpots, 0);
end;

destructor TMFiles.Destroy;
begin
  FreeFileList;
  inherited;
end;

procedure TMFiles.CheckFileNum(FileNum: integer);
begin
  if(FileNum < 0) or (FileNum >= Length(MFiles)) then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);
end;

function TMFiles.AddFileToManagedList(Path: String; FS: TFileStream; Mode: Integer): Integer;
var
  tFile: TMufasaFile;
begin
  tFile.Path := Path;
  tFile.FS := FS;
  tFile.Mode := Mode;
  tFile.BytesRead := 0;
  if Length(FreeSpots) > 0 then
  begin
    MFiles[FreeSpots[High(FreeSpots)]] := tFile;
    Result := FreeSpots[High(FreeSpots)];
    SetLength(FreeSpots, High(FreeSpots));
  end else
  begin
    SetLength(MFiles, Length(MFiles) + 1);
    MFiles[High(MFiles)] := tFile;
    Result := High(MFiles);
  end;
end;

function TMFiles.SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
begin
  CheckFileNum(FileNum);
  case Origin of
    fsFromBeginning:
                      if(cChars < 0) then
                        raise Exception.CreateFmt('fsFromBeginning takes no negative cChars. (%d)',[cChars]);
    fsFromCurrent:
                  ;
    fsFromEnd:
                  if(cChars > 0) then
                    raise Exception.CreateFmt('fsFromEnd takes no positive cChars. (%d)',[cChars]);
    else
      raise Exception.CreateFmt('Invalid Origin: %d',[Origin]);
  end;

  try
    Result := MFiles[FileNum].FS.Seek(cChars, Origin);
  except
    TClient(Client).Writeln('SetFileCharPointer - Exception Occured.');
    Result := File_AccesError;
  end;
  //Result := FileSeek(Files[FileNum].Handle, cChars, Origin);
end;

{/\
  Creates a file for reading/writing.
  Returns the handle (index) to the File Array.
  Returns File_AccesError if unsuccesfull.
/\}

function TMFiles.CreateFile(Path: string): Integer;
var
  FS: TFileStream;
  Continue : Boolean;
begin
  if Assigned(WriteFileEvent) then
  begin;
    Continue := true;
    WriteFileEvent(Self,path,continue);
    if not Continue then
      exit(File_EventError);
  end;
  try
    FS := TFileStream.Create(UTF8ToSys(Path), fmCreate);
    Result := AddFileToManagedList(Path, FS, fmCreate);
  except
    Result := File_AccesError;
    TClient(Client).Writeln(Format('CreateFile - Exception. Could not create file: %s',[path]));
  end;
end;

{/\
  Opens a file for reading.
  Returns the handle (index) to the File Array.
  Returns File_AccesError if unsuccesfull.
/\}

function TMFiles.OpenFile(Path: string; Shared: Boolean): Integer;
var
  FS: TFileStream;
  fMode: Integer;
  Continue : Boolean;
begin
  if Assigned(OpenFileEvent) then
  begin;
    Continue := true;
    OpenFileEvent(Self,path,continue);
    if not Continue then
      exit(File_EventError);
  end;
  if Shared then
    fMode := fmOpenRead or fmShareDenyNone
  else
    fMode := fmOpenRead or fmShareExclusive;
  try
      FS := TFileStream.Create(UTF8ToSys(Path), fMode)
  except
    Result := File_AccesError;
    TClient(Client).Writeln(Format('OpenFile - Exception. Could not open file: %s',[path]));
    Exit;
  end;
  Result := AddFileToManagedList(Path, FS, fMode);
end;

function TMFiles.AppendFile(Path: string): Integer;
var
  FS: TFileStream;
  fMode: Integer;
  Continue : Boolean;
begin
  if Assigned(WriteFileEvent) then
  begin
    Continue := true;
    WriteFileEvent(Self,path,continue);
    if not Continue then
      exit(File_EventError);
  end;
  fMode := fmOpenReadWrite;
  if not FileExists(Path) then
    fMode := fMode or fmCreate;
  try
    FS := TFileStream.Create(UTF8ToSys(Path), fMode);
    FS.Seek(0, fsFromEnd);
    Result := AddFileToManagedList(Path, FS, fMode);
  except
    Result := File_AccesError;
    TClient(Client).Writeln(Format('AppendFile - Exception. Could not create file: %s',[path]));
  end;
end;

{/\
  Opens a file for writing. And deletes the contents.
  Returns the handle (index) to the File Array.
  Returns File_AccesError if unsuccesfull.
/\}

function TMFiles.RewriteFile(Path: string; Shared: Boolean): Integer;
var
  FS: TFileStream;
  fMode: Integer;
  Continue : Boolean;
begin
  if Assigned(WriteFileEvent) then
  begin;
    Continue := true;
    WriteFileEvent(Self,path,continue);
    if not Continue then
      exit(File_EventError);
  end;
  if Shared then
    fMode := fmOpenReadWrite or fmShareDenyNone  or fmCreate
  else
    fMode := fmOpenReadWrite or fmShareDenyWrite or fmShareDenyRead or fmCreate;
  try
    FS := TFileStream.Create(UTF8ToSys(Path), fMode);
    FS.Size:=0;
    Result := AddFileToManagedList(Path, FS, fMode);
  except
    Result := File_AccesError;
    TClient(Client).Writeln(Format('ReWriteFile - Exception. Could not create file: %s',[path]));
  end;
end;

{/\
  Free's the given File at the given index.
/\}
procedure TMFiles.CloseFile(FileNum: Integer);
begin
  CheckFileNum(filenum);
  try
    MFiles[FileNum].FS.Free;
    MFiles[FileNum].FS := nil;
    SetLength(FreeSpots, Length(FreeSpots) + 1);
    FreeSpots[High(FreeSpots)] := FileNum;
  except
    TClient(Client).Writeln(Format('CloseFile, exception when freeing the file: %d',[filenum]));
  end;
end;

{/\
  Returns true if the BytesRead of the given FileNum (Index) has been reached.
  Also returns true if the FileNum is not valid.
/\}

function TMFiles.EndOfFile(FileNum: Integer): Boolean;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    TClient(Client).Writeln(format('EndOfFile: Invalid Internal Handle of File: %d',[filenum]));
    Result := True;
    Exit;
  end;
  Result := FilePointerPos(FileNum) >= FileSizeMuf(FileNum);
end;

{/\
  Returns the FileSize of the given index (FileNum)
/\}

function TMFiles.FileSizeMuf(FileNum: Integer): LongInt;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    TClient(Client).Writeln(format('FileSize: Invalid Internal Handle of File: %d',[filenum]));
    Result := File_AccesError;
    Exit;
  end;

  Result := MFiles[FileNum].FS.Size;
end;

function TMFiles.FilePointerPos(FileNum: Integer): Integer;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    TClient(Client).Writeln(format('FilePointerPos: Invalid Internal Handle of File: %d',[filenum]));
    Result := File_AccesError;
    Exit;
  end;
  try
    Result := MFiles[FileNum].FS.Seek(0, fsFromCurrent);
  except
    TClient(Client).Writeln('Exception in FilePointerPos');
  end;
end;

{/\
  Reads x numbers of characters from a file, and stores it into s.
/\}

function TMFiles.ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
begin
  CheckFileNum(filenum);
  if MFiles[FileNum].FS = nil then
  begin
    TClient(Client).Writeln(format('ReadFileString: Invalid Internal Handle of File: %d',[filenum]));
    Exit;
  end;

  SetLength(S, X);
  MFiles[FileNum].FS.Read(S[1], x);

  {Files[FileNum].BytesRead := Files[FileNum].BytesRead + X;
  FileRead(Files[FileNum].Handle, S[1], X);
  SetLength(S, X); }
end;

{/\
  Writes s in the given File.
/\}

function TMFiles.WriteFileString(FileNum: Integer;const  s: string): Boolean;
begin
  result := false;
  CheckFileNum(filenum);
  if(MFiles[FileNum].FS = nil) then
  begin
    TClient(Client).Writeln(format('WriteFileString: Invalid Internal Handle of File: %d',[filenum]));
    Exit;
  end;
  if (MFiles[FileNum].Mode and (fmOpenWrite or fmOpenReadWrite)) = 0 then //Checks if we have write rights..
    exit;
  try
    Result := MFiles[FileNum].FS.Write(S[1], Length(S)) <> 1;
  except
    TClient(Client).Writeln('Exception - WriteFileString.');
    Result := False;
  end;
end;

end.

