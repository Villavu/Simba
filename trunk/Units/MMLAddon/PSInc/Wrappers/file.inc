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

    File.inc for the Mufasa Macro Library
}

function ps_CreateFile(Path: string): Integer; stdcall;
begin
  Result := CurrThread.Client.MFiles.CreateFile(Path);
end;

function ps_OpenFile(Path: string; Shared: Boolean): Integer; stdcall;
begin
  Result := CurrThread.Client.MFiles.OpenFile(Path, Shared);
end;

function ps_RewriteFile(Path: string; Shared: Boolean): Integer; stdcall;
begin
  Result := CurrThread.Client.MFiles.RewriteFile(Path, Shared);
end;

procedure ps_CloseFile(FileNum: Integer); stdcall;
begin
  CurrThread.Client.MFiles.CloseFile(FileNum);
end;

function ps_EndOfFile(FileNum: Integer): Boolean; stdcall;
begin
  Result := CurrThread.Client.MFiles.EndOfFile(FileNum);
end;

function ps_FileSize(FileNum: Integer): LongInt; stdcall;
begin
  Result := CurrThread.Client.MFiles.FileSizeMuf(FileNum);
end;

function ps_ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean; stdcall;
begin
  Result := CurrThread.Client.MFiles.ReadFileString(FileNum, s, x);
end;

function ps_WriteFileString(FileNum: Integer; s: string): Boolean; stdcall;
begin
  Result := CurrThread.Client.MFiles.WriteFileString(FileNum, s);
end;
function ps_SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer; stdcall;
begin
  Result := CurrThread.Client.MFiles.SetFileCharPointer(Filenum, cChars, Origin);
end;
function ps_FilePointerPos(FileNum: Integer): Integer; stdcall;
begin
  Result := CurrThread.Client.MFiles.FilePointerPos(FileNum);
end;
