unit libloader;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, dynlibs;
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
        procedure FreePlugins;
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
    PlugExt: String = {$IFDEF LINUX}'.so';{$ELSE}'.dll';{$ENDIF}
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
    for i := 0 to PluginLen - 1 do
      if Loaded[i].filename = (PluginDirs.Strings[ii] + PluginName + PlugExt) then
        Exit(i);
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
      Result := LoadPluginNoFallback(PluginName);
    except
      on exception do Result:= LoadPluginNoFallback(PluginName+CpuBits);
    end;

  end;


  constructor TGenericLoader.Create;
  begin
    inherited Create;
    PluginLen := 0;
    PluginDirs := TStringList.Create;
    PluginDirs.CaseSensitive:= {$IFDEF LINUX}true{$ELSE}false{$ENDIF};
    PluginDirs.Duplicates:= dupIgnore;
  end;

  destructor TGenericLoader.Destroy;
  begin
    FreePlugins;
    PluginDirs.Free;
    inherited Destroy;
  end;

end.

