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

    extensions.inc for the Mufasa Macro Library
}

type
  TStringArray = array of string;
function ext_UnTar(const Input : string; out Content : TStringArray) : boolean;
var
  Stream : TStringStream;
begin
  result := false;
  try
    Stream := TStringStream.Create(Input);
    content := UnTar(Stream);
    result := (length(content) > 0);
  finally
    stream.free;
  end;
end;

function ext_UnTarEx(const Input : string;const outputdir : string; overwrite : boolean): boolean;
var
  Stream : TStringStream;
  MS : TMemoryStream;
begin
  result := false;
  try
    Stream := TStringStream.Create(Input);
    result := UnTar(stream,outputdir,overwrite);
  finally
    stream.free;
  end;
end;
function ext_DecompressBZip2(const input: string;out output : string; const BlockSize: Cardinal): boolean;
var
  Stream : TStringStream;
  MS : TMemoryStream;
begin
  result := false;
  try
    Stream := TStringStream.Create(Input);
    ms := DecompressBZip2(Stream,blocksize);
    if ms.size > 0 then
    begin
      ms.Position:= 0;
      SetLength(output,ms.Size);
      MS.Read(output[1],MS.size);
      ms.free;
      result := true;
    end;
  finally
    stream.free;
  end;
end;

function ext_GetPage(const url : string) : string;
var
  t: TDownloadThread;
begin
  result := '';
  try
    t := TDownloadThread.Create(true);
    t.InputURL:=url;
    t.Resume;
    while not t.done do
    begin
      Application.ProcessMessages;
      Sleep(25);
    end;
    Exit(t.ResultStr);
  except
    on e : exception do
      mDebugLn('Exception in GetPage in Extensions: ' + e.message);
  end;
end;
