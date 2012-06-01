{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

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

    TGenericLoader class for the Mufasa Macro Library
}
unit libloader;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, dynlibs, syncobjs;
  type
    TGenericLib = record
      filename: string;
      handle: TLibHandle;
    end;
    TGenericLibArray = array of TGenericLib;

    { TGenericLoader }

    TGenericLoader = class(TObject)
      private
        PluginLen : integer;
        Loaded: TGenericLibArray;
        PluginDirs : TStringList;
        LoadCriticalSection: syncobjs.TCriticalSection;
        procedure LoadPluginsDir(DirIndex : integer);
        function VerifyPath(Path : string) : string;
        function LoadPluginNoFallback(PluginName : string) : integer;
      protected
        function InitPlugin(plugin: TLibHandle): boolean; virtual; abstract;
      public
        constructor Create;
        destructor Destroy; override;
        procedure ValidateDirs;
        procedure AddPath(path: string);
        procedure FreePlugins; virtual;
        function LoadPlugin(PluginName : string) : integer;
    end;

implementation

  uses
    MufasaTypes,MufasaBase,FileUtil;

  procedure TGenericLoader.AddPath(path: string);
  var
    idx: integer;
    verified : string;
  begin
    verified := VerifyPath(path);
    //IDK who changed this to loading a dir, but DON'T
    if not PluginDirs.Find(verified,idx) then
    begin
      mDebugLn('Adding Plugin Path: ' + verified);
      PluginDirs.Add(verified);
    end;
  end;

  procedure TGenericLoader.ValidateDirs;
  var
    i : integer;
  begin
    for i := 0 to PluginDirs.Count - 1 do
    begin;
      if DirectoryExists(PluginDirs[i]) = false then
        raise Exception.createFMT('Directory(%s) does not exist',[PluginDirs[i]]);
      PluginDirs[i] := VerifyPath(PluginDirs[i]);
    end;
  end;

  procedure TGenericLoader.LoadPluginsDir(DirIndex: integer);
  var
    PlugExt: String = '*.' + SharedSuffix;
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

  function TGenericLoader.VerifyPath(Path: string): string;
  begin
    if (@path = nil) or (path = '') then
      exit('');
    Result := Path;
    if (Result[Length(Result)] <> DS) then
    begin;
      if (Result[Length(Result)] = '\') or (Result[Length(Result)] = '/') then
        Result[Length(Result)] := DS
      else
        Result := Result + DS;
    end;
  end;

  function TGenericLoader.LoadPluginNoFallback(PluginName: string): Integer;
  var
    i, ii : integer;
    PlugExt: String = '.' + SharedSuffix;
  begin
    ii := -1;
    result := -1;
    if PluginDirs.Count = 0 then
      Exit;
    ValidateDirs;
    PluginName := ExtractFileNameWithoutExt(PluginName);
    for i := 0 to PluginDirs.Count - 1 do
      if FileExistsUTF8(PluginDirs.Strings[i] + Pluginname + PlugExt) then
      begin;
        if ii <> -1 then
          Raise Exception.CreateFmt('Plugin(%s) has been found multiple times',[PluginName]);
        ii := i;
      end;
    if ii = -1 then
      raise Exception.CreateFMT('Plugin(%s) has not been found',[PluginName]);

    { Plugin already loaded }
    for i := 0 to PluginLen - 1 do
      if Loaded[i].filename = (PluginDirs.Strings[ii] + PluginName + PlugExt) then
      begin
        Writeln(Format('Plugin %s already loaded: %d', [PluginName, i]));
        Exit(i);
      end;
    SetLength(Loaded,PluginLen + 1);
    mDebugLn(Format('Loading plugin %s at %s',[PluginName,PluginDirs.Strings[ii]]));
    Loaded[PluginLen].filename:= PluginDirs.Strings[ii] + Pluginname + PlugExt;
    Loaded[PluginLen].handle:= LoadLibrary(Loaded[PluginLen].filename);
    if Loaded[PluginLen].handle = 0 then
      Raise Exception.CreateFMT('Error loading plugin %s',[Loaded[PluginLen].filename]);
    if InitPlugin(Loaded[PluginLen].handle) then
      inc(PluginLen)
    else
      FreeLibrary(Loaded[PluginLen].handle);
    Result:= PluginLen - 1;
  end;


  function TGenericLoader.LoadPlugin(PluginName: string): Integer;
  var
    CpuBits: String = {$IFDEF CPU32}'32';{$ELSE}'64';{$ENDIF}
  begin
    try
      LoadCriticalSection.Acquire;

        try
          Result := LoadPluginNoFallback(PluginName);
        except
          on exception do Result:= LoadPluginNoFallback(PluginName+CpuBits);
        end;

    finally
      LoadCriticalSection.Release;
    end;

  end;

  procedure TGenericLoader.FreePlugins;
  var
    I : integer;
  begin
    for i := 0 to PluginLen - 1 do
    begin;
      if (Loaded[i].handle > 0) then
      try
        mDebugLn('Freeing plugin[%d]',[i]);
        FreeLibrary(Loaded[i].handle);
      except
      end;
    end;
    SetLength(Loaded,0);
    PluginLen:= 0;
  end;

  constructor TGenericLoader.Create;
  begin
    inherited Create;
    PluginLen := 0;
    PluginDirs := TStringList.Create;
    PluginDirs.CaseSensitive:= {$IFDEF MSWINDOWS}False{$ELSE}True{$ENDIF};
    PluginDirs.Duplicates:= dupIgnore;
    LoadCriticalSection := syncobjs.TCriticalSection.Create;
  end;

  destructor TGenericLoader.Destroy;
  begin
    FreePlugins;
    PluginDirs.Free;
    LoadCriticalSection.Free;
    inherited Destroy;
  end;

end.

