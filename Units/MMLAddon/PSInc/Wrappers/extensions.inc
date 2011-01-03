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
function ext_UnTar(const Input : string; var Content : TStringArray) : boolean;
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
  Untarrer : TUntarThread;
begin
  result := false;
  try
    Stream := TStringStream.Create(Input);
    Untarrer := TUntarThread.Create(stream,outputdir,overwrite);
    Untarrer.Resume;
    while Untarrer.Finished = false do
    begin
      Application.ProcessMessages;
      sleep(25);
    end;
    result := Untarrer.Result;
  finally
    Untarrer.Free;
    stream.free;
  end;
end;
function ext_DecompressBZip2(const input: string;var output : string; const BlockSize: Cardinal): boolean;
var
  Stream : TStringStream;
  Decompress : TDecompressThread;
  MS : TMemoryStream;
begin
  result := false;
  try
    Stream := TStringStream.Create(Input);
    Decompress := TDecompressThread.Create(Stream);
    Decompress.Resume;
    while Decompress.Finished = false do
    begin
      Application.ProcessMessages;
      sleep(25);
    end;
    ms := Decompress.Result;
    if ms.size > 0 then
    begin
      ms.Position:= 0;
      SetLength(output,ms.Size);
      MS.Read(output[1],MS.size);
      result := true;
    end;
    ms.free;
  finally
    stream.free;
    Decompress.Free;
  end;
end;

function ext_GetPage(const url : string) : string;
var
  t: TDownloadThread;
begin
  result := '';
  try
    t := TDownloadThread.Create(url,@result);
    t.Resume;
    while not t.done do
    begin
      Application.ProcessMessages;
      Sleep(25);
    end;
  except
    on e : exception do
      mDebugLn('Exception in GetPage in Extensions: ' + e.message);
  end;
end;

function ext_MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  result := MessageDlg(acaption,amsg,dlgtype,buttons,helpctx);
end;

function ext_SDTMToMDTM(Const DTM: TSDTM): TMDTM;
begin
  result := SDTMToMDTM(DTM);
end;

function ext_InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
begin
  result := InputQuery(acaption,aprompt,value);
end;

function ext_ScriptText: string;
begin
  Result := SimbaForm.CurrScript.SynEdit.Lines.Text;
  Result := ReplaceRegExpr('([N|n][A|a][M|m][E|e]|[P|p][A|a][S|s]{2}|[P|p][I|i][N|n])\s*\:\=\s*\''.*?\'';', Result, '$1 := ''*********'';', True);
end;

procedure ext_OpenScript(vName, Data: string; Run: boolean);
begin
  if (vName = '') then
    vName := 'Untitled';
    
  with SimbaForm do
  begin
    AddTab();
    with CurrScript do
    begin
      SynEdit.Lines.Text := Data;
      ScriptName := vName;
      ScriptChanged := True;
    end;
    RefreshTab();
    UpdateTitle();
  end;

  if (Run) then
    SimbaForm.RunScript();
end;

procedure ext_OpenScriptEx(FileName: string; Run: boolean);
begin
  FileName := SetDirSeparators(FileName);
  with SimbaForm do
  begin
    AddTab();
    with CurrScript do
    begin
      SynEdit.Lines.LoadFromFile(FileName);
      StartText := SynEdit.Lines.Text;
      ScriptName := ExtractFileNameOnly(FileName);
      ScriptFile := FileName;
      ScriptChanged := False;
    end;
    RefreshTab();
    UpdateTitle();
  end;

  if (Run) then
    SimbaForm.RunScript();
end;

function ext_GetPageEx(const URL, PostData, MimeType: string): string;
var
  HTTPSend: THTTPSend;
begin
  HTTPSend := THTTPSend.Create;
  try
    HTTPSend.MimeType := MimeType;
    HTTPSend.Document.Clear;
    HTTPSend.Document.Write(PostData[1], Length(PostData));
    try
      if HTTPSend.HTTPMethod('POST', URL) then
      begin;
        SetLength(Result, HTTPSend.Document.Size);
        HTTPSend.Document.Read(Result[1], Length(Result));
      end else
        Result := '';
    except
      on e : exception do
        mDebugLn('Exception in GetPage in Extensions: ' + e.message);
	end;
  finally
    HTTPSend.Free;
  end;
end;

function ext_GetJSONValue(const Data, Value: string): string;
var
  SuperObject: ISuperObject;
begin
  SuperObject := SO(Data);
  Result := SuperObject.AsObject.S[Value];
end;
