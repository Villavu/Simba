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

type
    TMufasaFile = record
      Path: String;
      FS: TFileStream;
      BytesRead, Mode: Integer;
    end;

    TMufasaFilesArray = Array Of TMufasaFile;

    TMFiles = class(TObject)
          constructor Create(Owner : TObject);
          destructor Destroy; override;
      public
          function CreateFile(Path: string): Integer;
          function OpenFile(Path: string; Shared: Boolean): Integer;
          function RewriteFile(Path: string; Shared: Boolean): Integer;
          procedure CloseFile(FileNum: Integer);
          function EndOfFile(FileNum: Integer): Boolean;
          function FileSizeMuf(FileNum: Integer): LongInt;
          function ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
          function WriteFileString(FileNum: Integer; s: string): Boolean;
          Function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
          function FilePointerPos(FileNum: Integer): Integer;
      protected
          MFiles: TMufasaFilesArray;
          FreeSpots: Array Of Integer;
          Client : TObject;
      private
          procedure FreeFileList;
          function AddFileToManagedList(Path: string; FS: TFileStream; Mode: Integer): Integer;

    end;

    // We don't need one per object. :-)
    function GetFiles(Path, Ext: string): TStringArray;
    function GetDirectories(Path: string): TstringArray;
    function FindFile(filename : string; Dirs : array of string) : string; //Results '' if not found

implementation
uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} IniFiles,Client;

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
      if SearchRec.Name[1] = '.' then
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
  if fileexists(filename) then
    result := filename
  else
  begin
    for i := 0 to high(Dirs) do
      if (Dirs[i] <> '') and DirectoryExists(dirs[i]) then
        if fileexists(dirs[i] + filename) then
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
    If MFiles[i].FS <> nil Then
    Begin
      TClient(Client).Writeln(Format('File[%s] has not been freed in the script, freeing it now.',[MFiles[i].Path]));
      Try
        MFiles[I].FS.Free;
      Except
        TClient(Client).Writeln('FreeFileList - Exception when freeing FileStream');
      End;
    End;
  SetLength(MFiles, 0);
  SetLength(FreeSpots, 0);
end;

destructor TMFiles.Destroy;
begin
  FreeFileList;
  inherited;
end;

Function TMFiles.AddFileToManagedList(Path: String; FS: TFileStream; Mode: Integer): Integer;
Var
   tFile: TMufasaFile;
Begin
   tFile.Path := Path;
   tFile.FS := FS;
   tFile.Mode := Mode;
   tFile.BytesRead := 0;

   If Length(FreeSpots) > 0 Then
   Begin
     //WriteLn('There is a free spot: ' + IntToStr(FreeSpots[High(FreeSpots)]));
     MFiles[FreeSpots[High(FreeSpots)]] := tFile;
     Result := FreeSpots[High(FreeSpots)];
     SetLength(FreeSpots, High(FreeSpots));
   End Else
   Begin
     // Increase by * 2.
     //WriteLn('No Free Spot. Increasing the size');
     SetLength(MFiles, Length(MFiles) + 1);
     //Writeln('Length of Files: ' + IntToStr(Length(Files)));
     MFiles[High(MFiles)] := tFile;
     Result := High(MFiles);
   End;
End;

Function TMFiles.SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
Begin
  If(FileNum < 0) or (FileNum >= Length(MFiles)) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);

  {If Files[FileNum].Handle = -1 Then
  Begin
    WriteLn('SetFileCharPointer: Invalid Internal Handle');
    Result := -1;
    Exit;
  End;}

  case Origin of
    fsFromBeginning:
                      If(cChars < 0) Then
                        raise Exception.CreateFmt('fsFromBeginning takes no negative cChars. (%d)',[cChars]);
    fsFromCurrent:
                  ;
    fsFromEnd:
                  If(cChars > 0) Then
                    raise Exception.CreateFmt('fsFromEnd takes no positive cChars. (%d)',[cChars]);
    else
      raise Exception.CreateFmt('Invalid Origin: %d',[Origin]);
  end;

  Try
    Result := MFiles[FileNum].FS.Seek(cChars, Origin);
  Except
    TClient(Client).Writeln('SetFileCharPointer - Exception Occured.');
    Result := -1;
  End;
  //Result := FileSeek(Files[FileNum].Handle, cChars, Origin);
End;

{/\
  Creates a file for reading/writing.
  Returns the handle (index) to the File Array.
  Returns -1 if unsuccesfull.
/\}

function TMFiles.CreateFile(Path: string): Integer;

Var
   FS: TFileStream;

begin
  Try
    FS := TFileStream.Create(Path, fmCreate);
  Except
    Result := -1;
    TClient(Client).Writeln(Format('CreateFile - Exception. Could not create file: %s',[path]));
    Exit;
  End;

  Result := AddFileToManagedList(Path, FS, fmCreate);
end;

{/\
  Opens a file for reading.
  Returns the handle (index) to the File Array.
  Returns -1 if unsuccesfull.
/\}

function TMFiles.OpenFile(Path: string; Shared: Boolean): Integer;

Var
   FS: TFileStream;
   fMode: Integer;

begin
  If Shared Then
    fMode := fmOpenRead or fmShareDenyNone
  Else
    fMode := fmOpenRead or fmShareExclusive;

  Try
      FS := TFileStream.Create(Path, fMode)
  Except
    Result := -1;
    TClient(Client).Writeln(Format('OpenFile - Exception. Could not open file: %s',[path]));
    Exit;
  End;

  Result := AddFileToManagedList(Path, FS, fMode);

  {Result := FileOpen(Path, fmOpenRead);
  If Result <> -1 Then
  Begin
    //WriteLn('File was successfully opened');
    Result := AddFileToManagedList(Path, Result, fmOpenRead);
    {If Result <> -1 Then
      WriteLn('File was successfully added: ' + IntToStr(Result));}
  End Else
  Begin
    WriteLn('Could not open file. Returning -1');
  End; }
end;

{/\
  Opens a file for writing.
  Returns the handle (index) to the File Array.
  Returns -1 if unsuccesfull.
/\}

function TMFiles.RewriteFile(Path: string; Shared: Boolean): Integer;

Var
   FS: TFileStream;
   fMode: Integer;
begin
  If Shared Then
    fMode := fmOpenReadWrite or fmShareDenyNone  or fmCreate
  Else
    fMode := fmOpenReadWrite or fmShareDenyWrite or fmShareDenyRead or fmCreate;

  Try
    FS := TFileStream.Create(Path, fMode);
  Except
    Result := -1;
    TClient(Client).Writeln(Format('ReWriteFile - Exception. Could not create file: %s',[path]));
    Exit;
  End;

  Result := AddFileToManagedList(Path, FS, fMode);

  {Result := FileOpen(Path, fmOpenReadWrite);
  If Result <> -1 Then
  Begin
    //WriteLn('File was successfully opened.');
    Result := AddFileToManagedList(Path, Result, fmOpenReadWrite);
    {If Result <> -1 Then
      WriteLn('File was successfully added: ' + IntToStr(Result)); }
  End Else
  Begin
    WriteLn('Could not open file. Returning -1');
  End;  }
end;

{/\
  Free's the given File at the given index.
/\}
procedure TMFiles.CloseFile(FileNum: Integer);

begin
  If (FileNum >= Length(MFiles)) or (FileNum < 0) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);

  Try
     MFiles[FileNum].FS.Free;
  Except
    TClient(Client).Writeln(Format('CloseFile, exception when freeing the file: %d',[filenum]));
    Exit;
  End;

  MFiles[FileNum].FS := nil;
  SetLength(FreeSpots, Length(FreeSpots) + 1);
  FreeSpots[High(FreeSpots)] := FileNum;

  {If Files[FileNum].Handle = -1 Then
  Begin
    WriteLn('CloseFile: Invalid Internal Handle');
    Exit;
  End;
  FileClose(Files[FileNum].Handle);  }

end;

{/\
  Returns true if the BytesRead of the given FileNum (Index) has been reached.
  Also returns true if the FileNum is not valid.
/\}

function TMFiles.EndOfFile(FileNum: Integer): Boolean;
begin
  If(FileNum < 0) or (FileNum >= Length(MFiles)) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);
  If MFiles[FileNum].FS = nil Then
  Begin
    TClient(Client).Writeln(format('EndOfFile: Invalid Internal Handle of File: %d',[filenum]));
    Result := True;
    Exit;
  End;

  Result := FilePointerPos(FileNum) >= FileSizeMuf(FileNum);
end;

{/\
  Returns the FileSize of the given index (FileNum)
/\}

function TMFiles.FileSizeMuf(FileNum: Integer): LongInt;
begin
  If(FileNum < 0) or (FileNum >= Length(MFiles)) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);

  If MFiles[FileNum].FS = nil Then
  Begin
    TClient(Client).Writeln(format('FileSize: Invalid Internal Handle of File: %d',[filenum]));
    Result := -1;
    Exit;
  End;

  Result := MFiles[FileNum].FS.Size;

  {
  If Files[FileNum].Handle = -1 Then
  Begin
    WriteLn('FileSize: Invalid Internal Handle');
    Result := -1;
    Exit;
  End;
  // Get our current position.
  tempPos := FileSeek(Files[FileNum].Handle, 0, fsFromCurrent);

  // End of the file.
  Result := FileSeek(Files[FileNum].Handle, 0, fsFromEnd);

  // Reset the position.
  FileSeek(Files[FileNum].Handle, tempPos, fsFromBeginning);  }
end;

function TMFiles.FilePointerPos(FileNum: Integer): Integer;
begin
  If(FileNum < 0) or (FileNum >= Length(MFiles)) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);
  If MFiles[FileNum].FS = nil Then
  Begin
    TClient(Client).Writeln(format('FilePointerPos: Invalid Internal Handle of File: %d',[filenum]));
    Result := -1;
    Exit;
  End;

  try
    Result := MFiles[FileNum].FS.Seek(0, fsFromCurrent);
  Except
    TClient(Client).Writeln('Exception in FilePointerPos');
  End;
  //Result := FileSeek(Files[FileNum].FS, 0, fsFromCurrent);
end;

{/\
  Reads x numbers of characters from a file, and stores it into s.
/\}

function TMFiles.ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;

begin
  If(FileNum < 0) or (FileNum >= Length(MFiles)) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);
  If MFiles[FileNum].FS = nil Then
  Begin
    TClient(Client).Writeln(format('ReadFileString: Invalid Internal Handle of File: %d',[filenum]));
    Exit;
  End;

  SetLength(S, 0);
  SetLength(S, X);
  MFiles[FileNum].FS.Read(S[1], x);

  {Files[FileNum].BytesRead := Files[FileNum].BytesRead + X;
  FileRead(Files[FileNum].Handle, S[1], X);
  SetLength(S, X); }
end;

{/\
  Writes s in the given File.
/\}

function TMFiles.WriteFileString(FileNum: Integer; s: string): Boolean;
begin
  If(FileNum < 0) or (FileNum >= Length(MFiles)) Then
    raise Exception.CreateFmt('Invalid FileNum passed: %d',[FileNum]);
  If(MFiles[FileNum].FS = nil) Then
  Begin
    TClient(Client).Writeln(format('WriteFileString: Invalid Internal Handle of File: %d',[filenum]));
    Result := False;
    Exit;
  End;

  {If((Files[FileNum].Mode and fmOpenWrite) = 0) Then
  Begin
    WriteLn('This file may not write');
    Exit;
  End;  }

  try
    Result := MFiles[FileNum].FS.Write(S[1], Length(S)) <> 1;
  except
    TClient(Client).Writeln('Exception - WriteFileString.');
    Result := False;
  end;

  {If(FileWrite(Files[FileNum].Handle, S[1], Length(S)) <> -1) Then
    Result := True
  Else
    Result := False;  }
end;

end.

