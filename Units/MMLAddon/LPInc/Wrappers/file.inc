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

procedure lpCreateFile(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.CreateFile(PlpString(Params^[0])^);
end;

procedure lpOpenFile(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.OpenFile(PlpString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure lpRewriteFile(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.RewriteFile(PlpString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure lpAppendFile(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.AppendFile(PlpString(Params^[0])^);
end;

procedure lpCloseFile(const Params: PParamArray);
begin
  CurrThread.Client.MFiles.CloseFile(PLongInt(Params^[0])^);
end;

procedure lpEndOfFile(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFiles.EndOfFile(PLongInt(Params^[0])^);
end;

procedure lpFileSize(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.FileSizeMuf(PLongInt(Params^[0])^);
end;

procedure lpReadFileString(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFiles.ReadFileString(PLongInt(Params^[0])^, PlpString(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpWriteFileString(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := CurrThread.Client.MFiles.WriteFileString(PLongInt(Params^[0])^, PlpString(Params^[1])^);
end;

procedure lpSetFileCharPointer(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.SetFileCharPointer(PLongInt(Params^[0])^, PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

procedure lpFilePointerPos(const Params: PParamArray; const Result: Pointer);
begin
  PLongInt(Result)^ := CurrThread.Client.MFiles.FilePointerPos(PLongInt(Params^[0])^);
end;

procedure lpFileExists(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := FileExistsUTF8(PlpString(Params^[0])^);
end;

procedure lpDirectoryExists(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := DirectoryExistsUTF8(PlpString(Params^[0])^);
end;

procedure lpCreateDirectory(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := CreateDirUTF8(PlpString(Params^[0])^);
end;

procedure lpForceDirectores(const Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := ForceDirectoriesUTF8(PlpString(Params^[0])^);
end;

procedure lpGetFiles(const Params: PParamArray; const Result: Pointer);
begin
  PStringArray(Result)^ := GetFiles(PlpString(Params^[0])^, PlpString(Params^[1])^);
end;

procedure lpGetDirectories(const Params: PParamArray; const Result: Pointer);
begin
  PStringArray(Result)^ := GetDirectories(PlpString(Params^[0])^);
end;

procedure lpWriteINI(const Params: PParamArray);
var
  tempini : TIniFile;
begin;
  tempini := TIniFile.Create(UTF8ToSys(PlpString(Params^[3])^),True);
  tempini.WriteString(PlpString(Params^[0])^, PlpString(Params^[1])^, PlpString(Params^[2])^);
  tempini.free;
end;

procedure lpReadINI(const Params: PParamArray; const Result: Pointer);
var
  tempini : TIniFile;
begin;
  tempini := TIniFile.Create(UTF8ToSys(PlpString(Params^[2])^),True);
  PlpString(Result)^ := tempini.ReadString(PlpString(Params^[0])^, PlpString(Params^[1])^, '');
  tempini.free;
end;

procedure lpDeleteINI(const Params: PParamArray);
var
  tempini : TIniFile;
begin;
  tempini := TIniFile.Create(utf8tosys(PlpString(Params^[2])^),True);
  if PlpString(Params^[1])^ = '' then
    tempini.EraseSection(PlpString(Params^[0])^)
  else
    Tempini.DeleteKey(PlpString(Params^[0])^, PlpString(Params^[1])^);
  tempini.free;
end;

procedure lpExtractFileExt(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := ExtractFileExt(PlpString(Params^[0])^);
end;
