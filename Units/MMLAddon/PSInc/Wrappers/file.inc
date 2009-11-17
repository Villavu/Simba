function ps_CreateFile(Path: string): Integer;
begin
  Result := CurrThread.Client.MFiles.CreateFile(Path);
end;

function ps_OpenFile(Path: string; Shared: Boolean): Integer;
begin
  Result := CurrThread.Client.MFiles.OpenFile(Path, Shared);
end;

function ps_RewriteFile(Path: string; Shared: Boolean): Integer;
begin
  Result := CurrThread.Client.MFiles.RewriteFile(Path, Shared);
end;

procedure ps_CloseFile(FileNum: Integer);
begin
  CurrThread.Client.MFiles.CloseFile(FileNum);
end;

function ps_EndOfFile(FileNum: Integer): Boolean;
begin
  Result := CurrThread.Client.MFiles.EndOfFile(FileNum);
end;

function ps_FileSize(FileNum: Integer): LongInt;
begin
  Result := CurrThread.Client.MFiles.FileSizeMuf(FileNum);
end;

function ps_ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
begin
  Result := CurrThread.Client.MFiles.ReadFileString(FileNum, s, x);
end;

function ps_WriteFileString(FileNum: Integer; s: string): Boolean;
begin
  Result := CurrThread.Client.MFiles.WriteFileString(FileNum, s);
end;
function ps_SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
begin
  Result := CurrThread.Client.MFiles.SetFileCharPointer(Filenum, cChars, Origin);
end;
function ps_FilePointerPos(FileNum: Integer): Integer;
begin
  Result := CurrThread.Client.MFiles.FilePointerPos(FileNum);
end;
