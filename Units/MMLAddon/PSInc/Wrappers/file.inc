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

function ps_CreateFile(const Path: string): Integer; extdecl;
begin
  Result := CurrThread.Client.MFiles.CreateFile(Path);
end;

function ps_OpenFile(const Path: string; Shared: Boolean): Integer; extdecl;
begin
  Result := CurrThread.Client.MFiles.OpenFile(Path, Shared);
end;

function ps_RewriteFile(const Path: string; Shared: Boolean): Integer; extdecl;
begin
  Result := CurrThread.Client.MFiles.RewriteFile(Path, Shared);
end;

function ps_AppendFile(const Path: string): Integer; extdecl;
begin
  Result := CurrThread.Client.MFiles.AppendFile(Path);
end;

procedure ps_CloseFile(FileNum: Integer); extdecl;
begin
  CurrThread.Client.MFiles.CloseFile(FileNum);
end;

function ps_EndOfFile(FileNum: Integer): Boolean; extdecl;
begin
  Result := CurrThread.Client.MFiles.EndOfFile(FileNum);
end;

function ps_FileSize(FileNum: Integer): LongInt; extdecl;
begin
  Result := CurrThread.Client.MFiles.FileSizeMuf(FileNum);
end;

function ps_ReadFileString(FileNum: Integer; var s: string; x: Integer): Boolean; extdecl;
begin
  Result := CurrThread.Client.MFiles.ReadFileString(FileNum, s, x);
end;

function ps_WriteFileString(FileNum: Integer; s: string): Boolean; extdecl;
begin
  Result := CurrThread.Client.MFiles.WriteFileString(FileNum, s);
end;
function ps_SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer; extdecl;
begin
  Result := CurrThread.Client.MFiles.SetFileCharPointer(Filenum, cChars, Origin);
end;
function ps_FilePointerPos(FileNum: Integer): Integer; extdecl;
begin
  Result := CurrThread.Client.MFiles.FilePointerPos(FileNum);
end;
function ps_FileExists ( const FileName : string ) : Boolean;extdecl;
begin
  result := FileExistsUTF8(FileName);
end;
function ps_DirectoryExists ( const DirectoryName : string ) : Boolean; extdecl;
begin
  result := DirectoryExistsUTF8(DirectoryName);
end;
function ps_CreateDirectory( const DirectoryName : string) : boolean; extdecl;
begin
  result := CreateDirUTF8(directoryName);
end;

function ps_ForceDirectores(const dir : string) : boolean; extdecl;
begin
  result := ForceDirectoriesUTF8(dir);
end;

function ps_GetFiles(const Path, Ext : string) : TStringArray;extdecl;
begin
  result := GetFiles(path,ext);
end;

function ps_GetDirectories(const path : string) : TStringArray;extdecl;
begin
  result := GetDirectories(path);
end;

function ps_DeleteFile(const Filename: string): Boolean; extdecl;
begin
  Result := DeleteFileUTF8(Filename);
end;

procedure ps_WriteINI(const Section, KeyName, NewString, FileName: string);extdecl;
var
  tempini : TIniFile;
begin;
  tempini := TIniFile.Create(UTF8ToSys(FileName),True);
  tempini.WriteString(Section,keyname,newstring);
  tempini.free;
end;

function ps_ReadINI(const Section, KeyName, FileName: string): string;extdecl;
var
  tempini : TIniFile;
begin;
  tempini := TIniFile.Create(UTF8ToSys(FileName),True);
  Result := tempini.ReadString(section,keyname,'');
  tempini.free;
end;

procedure ps_DeleteINI(const Section, KeyName, FileName: string); extdecl;
var
  tempini : TIniFile;
begin;
  tempini := TIniFile.Create(utf8tosys(FileName),True);
  if KeyName = '' then
    tempini.EraseSection(Section)
  else
    Tempini.DeleteKey(section,keyname);
  tempini.free;
end;

function ps_ExtractFileExt(const FileName: string): string; extdecl;
begin
  result := ExtractFileExt(filename);
end;
