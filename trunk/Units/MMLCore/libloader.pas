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

     Super unit for loading libraries for the Mufasa Macro Library
}

unit libloader;

{$mode objfpc}

interface

  uses
    Classes, SysUtils, dynlibs;

  type
    TGenericLib = record
      filename: string;
      handle: TLibHandle;
    end;
    TGenericLibArray = array of TGenericLib;

    TGenericLoader = class(TObject)
      private
        PluginLen : integer;
        Loaded: TGenericLibArray;
        procedure FreePlugins;
      protected
        function InitPlugin(plugin: TLibHandle): boolean; virtual; abstract;
      public
        PluginDirs : TStringList;
        constructor Create;
        destructor Destroy; override;
        procedure ValidateDirs;
        procedure LoadPluginsDir(DirIndex : integer);
        function LoadPlugin(PluginName : string) : integer;
    end;

implementation

  uses
    MufasaTypes,FileUtil;

  procedure TGenericLoader.FreePlugins;
  var
    I : integer;
  begin
    for i := 0 to PluginLen - 1 do
    begin;
      if (Loaded[i].handle > 0) then
      try
        Writeln(inttostr(I));
        FreeLibrary(Loaded[i].handle);
      except
      end;
    end;
    SetLength(Loaded,0);
    PluginLen:= 0;
  end;

  procedure TGenericLoader.ValidateDirs;
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

  procedure TGenericLoader.LoadPluginsDir(DirIndex: integer);
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


  function TGenericLoader.LoadPlugin(PluginName: string): Integer;
  var
    i, ii : integer;
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
      if Loaded[i].filename = (PluginDirs.Strings[ii] + PluginName + PlugExt) then
        Exit(i);
    SetLength(Loaded,PluginLen + 1);
    Writeln(Format('Loading plugin %s at %s',[PluginName,PluginDirs.Strings[ii]]));
    Loaded[PluginLen].filename:= PluginDirs.Strings[ii] + Pluginname + PlugExt;
    Loaded[PluginLen].handle:= LoadLibrary(PChar(@Loaded[PluginLen].filename));
    if Loaded[PluginLen].handle = 0 then
      Raise Exception.CreateFMT('Error loading plugin %s',[Loaded[PluginLen].filename]);
    if InitPlugin(Loaded[PluginLen].handle) then
      inc(PluginLen)
    else
      FreeLibrary(Loaded[PluginLen].handle);
    Result := PluginLen;
  end;


  constructor TGenericLoader.Create;
  begin
    inherited Create;
    PluginLen := 0;
    PluginDirs := TStringList.Create;
  end;

  destructor TGenericLoader.Destroy;
  begin
    FreePlugins;
    PluginDirs.Free;
    inherited Destroy;
  end;

end.

