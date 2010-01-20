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

    Plugins Class for the Mufasa Macro Library
}

unit plugins;

{
   Problems with SMART; you cannot free the plugin when smart is open..
   Therefore, loading & free-ing plugins per script run is not an option.
   Assigning a TMPlugin per Tab might be a do-able solution, but will still cope with the SMART Problems..
   So the question is: Plugins Per Tab,Per Run or Global?
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,dynlibs;

type
  TMPluginMethod = record
    FuncPtr : pointer;
    FuncStr : string;
  end;

  TMPlugin = record
    Methods : Array of TMPluginMethod;
    dllHandle : TLibHandle;
    filename : string;
    MethodLen : integer;
  end;
  TMPluginArray = array of TMPlugin;

  { TMPlugins }

  TMPlugins = class (TObject)
  private
    Plugins : TMPluginArray;
    PluginLen : integer;
    procedure FreePlugins;
  public
    PluginDirs : TStringList;
    procedure ValidateDirs;
    procedure LoadPluginsDir( DirIndex : integer);
    function LoadPlugin(PluginName : string) : integer;
    property Count : integer read PluginLen;
    property MPlugins : TMPluginArray read Plugins;
    constructor Create;
    destructor Destroy;override;
  end;



implementation

uses
  MufasaTypes,FileUtil;

{ TMPlugins }

procedure TMPlugins.FreePlugins;
var
  I : integer;
begin
  for i := 0 to PluginLen - 1 do
  begin;
    if (Plugins[i].dllHandle > 0) then
    try
      Writeln(inttostr(I));
      FreeLibrary(Plugins[i].dllHandle);
    except
    end;
  end;
  SetLength(Plugins,0);
  PluginLen:= 0;
end;

procedure TMPlugins.ValidateDirs;
var
  i : integer;
  TempStr : string;
begin
  for i := 0 to PluginDirs.Count - 1 do
  begin;
    if DirectoryExists(PluginDirs.Strings[i]) = false then
      raise Exception.createFMT('Directory(%s) does not exist',[PluginDirs[i]]);
    TempStr := PluginDirs.Strings[i];
    if (TempStr[Length(TempStr)] <> DS) then
    begin;
      if (TempStr[Length(TempStr)] = '\') or (TempStr[Length(TempStr)] = '/') then
        TempStr[Length(TempStr)] := DS
      else
        TempStr := TempStr + DS;
      PluginDirs.Strings[i] := TempStr;
    end;
  end;
end;

procedure TMPlugins.LoadPluginsDir(DirIndex: integer);
var
  PlugExt: String = {$IFDEF LINUX}'*.so';{$ELSE}'*.dll';{$ENDIF}
  FileSearcher : TSearchRec;
begin
  if (DirIndex < 0) or (DirIndex >= PluginDirs.Count) then
    Exit;
  if FindFirst(PluginDirs.Strings[DirIndex] + PlugExt, faAnyFile, FileSearcher) <> 0 then
  begin;
    FindClose(FileSearcher);
    Exit;
  end;
  repeat
    LoadPlugin(FileSearcher.Name);
  until FindNext(FileSearcher) <> 0;
  FindClose(FileSearcher);
end;

function TMPlugins.LoadPlugin(PluginName: string): Integer;
var
  i, ii : integer;
  pntrArrc     :  function : integer; stdcall;
  GetFuncInfo  :  function (x: Integer; var ProcAddr: Pointer; var ProcDef: PChar) : Integer; stdcall;
  GetTypeCount :  function : Integer; stdcall;
  GetTypeInfo  :  function (x: Integer; var sType, sTypeDef: string): Integer; stdcall;
  PD : PChar;
  pntr : Pointer;
  arrc : integer;
  Status : LongInt;
  PlugExt: String = {$IFDEF LINUX}'.so';{$ELSE}'.dll';{$ENDIF}
begin
  ii := -1;
  result := -1;
  if PluginDirs.Count = 0 then
    Exit;
  ValidateDirs;
  PluginName := ExtractFileNameWithoutExt(PluginName);
  for i := 0 to PluginDirs.Count - 1 do
    if FileExists(PluginDirs.Strings[i] + Pluginname + PlugExt) then
    begin;
      if ii <> -1 then
        Raise Exception.CreateFmt('Plugin(%s) has been found multiple times',[PluginName]);
      ii := i;
    end;
  if ii = -1 then
    raise Exception.CreateFMT('Plugins(%s) has not been found',[PluginName]);
  for i := 0 to PluginLen - 1 do
    if Plugins[i].filename = (PluginDirs.Strings[ii] + PluginName + PlugExt) then
      Exit(i);
  pd := StrAlloc(255);
  SetLength(Plugins,PluginLen + 1);
  Writeln(Format('Loading plugin %s at %s',[PluginName,PluginDirs.Strings[ii]]));
  Plugins[PluginLen].filename:= PluginDirs.Strings[ii] + Pluginname + PlugExt;
  Plugins[PluginLen].dllHandle:= LoadLibrary(PChar(Plugins[PluginLen].filename));
  if Plugins[PluginLen].dllHandle = 0 then
    Raise Exception.CreateFMT('Error loading plugin %s',[Plugins[PluginLen].filename]);
  Pointer(pntrArrc) := GetProcAddress(Plugins[PluginLen].dllHandle, PChar('GetFunctionCount'));
  if @pntrArrc = nil then
    Raise Exception.CreateFMT('Error loading plugin %s',[Plugins[PluginLen].filename]);
  arrc := pntrArrc();
  SetLength(Plugins[PluginLen].Methods, ArrC);
  Pointer(GetFuncInfo) := GetProcAddress(Plugins[PluginLen].dllHandle, PChar('GetFunctionInfo'));
  if @GetFuncInfo = nil then
    Raise Exception.CreateFMT('Error loading plugin %s',[Plugins[PluginLen].filename]);
  Plugins[PluginLen].MethodLen := Arrc;
  for ii := 0 to ArrC-1 do
  begin;
    if (GetFuncInfo(ii, pntr, pd) < 0) then
      Continue;
    Plugins[Pluginlen].Methods[ii].FuncPtr := pntr;
    Plugins[Pluginlen].Methods[ii].FuncStr := pd;
  end;
  Result := PluginLen;
  inc(PluginLen);
  StrDispose(pd);

end;


constructor TMPlugins.Create;
begin
  inherited Create;
  PluginLen := 0;
  PluginDirs := TStringList.Create;
end;

destructor TMPlugins.Destroy;
begin
  FreePlugins;
  PluginDirs.Free;
  inherited Destroy;
end;

end.

