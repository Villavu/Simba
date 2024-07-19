{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_plugin;

{$i simba.inc}

interface

uses
  Classes, SysUtils, dynlibs,
  simba.base,
  simba.containers,
  simba.script_compiler,
  simba.script_pluginmethods;

type
  PSimbaPluginInfo = ^TSimbaPluginInfo;
  TSimbaPluginInfo = packed record
    SimbaVersion: Int32;
    SimbaMajor: Int32;
    FileName: PChar;
    Compiler: Pointer;

    // Extend this but do not remove, reorder or change datatypes.
  end;

  TSimbaPluginExports = packed record
    GetFunctionInfo: function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
    GetFunctionCount: function: Int32; cdecl;
    GetTypeInfo: function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
    GetTypeCount: function: Int32; cdecl;
    GetCode: procedure(var Code: PChar); cdecl;
    GetCodeLength: function: Int32; cdecl;
    SetPluginMemManager: procedure(MemoryManager: TMemoryManager); cdecl;

    RegisterSimbaPlugin: procedure(Info: PSimbaPluginInfo; Methods: PSimbaPluginMethods); cdecl;

    OnAttach: procedure(Data: Pointer); cdecl;
    OnDetach: procedure; cdecl;
    OnPause: procedure; cdecl;
    OnResume: procedure; cdecl;
    OnStop: procedure; cdecl;
  end;

  TSimbaScriptPluginMethod = record
    Header: String;
    Address: Pointer;
    Native: Boolean;
  end;

  TSimbaScriptPluginType = record
    Name: String;
    Str: String;
  end;

  TSimbaScriptPlugin = class
  protected
    FFileName: String;
    FHandle: TLibHandle;

    FInfo: TSimbaPluginInfo;
    FMemoryManager: TMemoryManager;

    FExports: TSimbaPluginExports;

    FCode: String;
    FTypes: array of TSimbaScriptPluginType;
    FMethods: array of TSimbaScriptPluginMethod;
  public
    function Dump: TStringList;

    procedure Import(Compiler: TSimbaScript_Compiler);
    procedure Load;

    procedure CallOnPause;
    procedure CallOnResume;
    procedure CallOnStop;

    constructor Create(FileName: String; ExtraSearchDirs: TStringArray = nil);
    destructor Destroy; override;
  end;
  TSimbaScriptPluginList = specialize TSimbaObjectList<TSimbaScriptPlugin>;

implementation

uses
  ffi,
  simba.script_pluginloader;

procedure TSimbaScriptPlugin.Load;

  function LoadCode: String;
  var
    Buffer: PChar;
  begin
    Buffer := StrAlloc(FExports.GetCodeLength() + 1);

    try
      FExports.GetCode(Buffer);

      Result := Buffer;
    finally
      StrDispose(Buffer);
    end;
  end;

  function LoadMethod(Index: Int32): TSimbaScriptPluginMethod;
  var
    Buffer: PChar;
  begin
    Buffer := StrAlloc(4096);

    try
      FExports.GetFunctionInfo(Index, Result.Address, Buffer);

      Result.Header := Trim(Buffer);
      if not Result.Header.EndsWith(';') then
        Result.Header := Result.Header + ';';

      Result.Native := Result.Header.EndsWith('native;');
      if Result.Native then
      begin
        SetLength(Result.Header, Length(Result.Header) - Length('native;'));

        Result.Header := Trim(Result.Header);
      end;

      if not Result.Header.EndsWith(';') then
        Result.Header := Result.Header + ';';
    finally
      StrDispose(Buffer);
    end;
  end;

  function LoadType(Index: Int32): TSimbaScriptPluginType;
  var
    Buffer: record
      Name: PChar;
      Str: PChar;
    end;
  begin
    Buffer.Name := StrAlloc(4096);
    Buffer.Str := StrAlloc(4096);

    try
      FExports.GetTypeInfo(Index, Buffer.Name, Buffer.Str);

      Result.Name := Trim(Buffer.Name);
      Result.Str := Trim(Buffer.Str);

      if not Result.Str.EndsWith(';') then
        Result.Str := Result.Str + ';';
    finally
      StrDispose(Buffer.Name);
      StrDispose(Buffer.Str);
    end;
  end;

var
  Index: Int32;
begin
  with FExports do
  begin
    Pointer(GetFunctionInfo)     := GetProcedureAddress(FHandle, 'GetFunctionInfo');
    Pointer(GetFunctionCount)    := GetProcedureAddress(FHandle, 'GetFunctionCount');
    Pointer(GetTypeInfo)         := GetProcedureAddress(FHandle, 'GetTypeInfo');
    Pointer(GetTypeCount)        := GetProcedureAddress(FHandle, 'GetTypeCount');
    Pointer(GetCode)             := GetProcedureAddress(FHandle, 'GetCode');
    Pointer(GetCodeLength)       := GetProcedureAddress(FHandle, 'GetCodeLength');
    Pointer(SetPluginMemManager) := GetProcedureAddress(FHandle, 'SetPluginMemManager');
    Pointer(OnAttach)            := GetProcedureAddress(FHandle, 'OnAttach');
    Pointer(OnDetach)            := GetProcedureAddress(FHandle, 'OnDetach');
    Pointer(OnPause)             := GetProcedureAddress(FHandle, 'OnPause');
    Pointer(OnResume)            := GetProcedureAddress(FHandle, 'OnResume');
    Pointer(OnStop)              := GetProcedureAddress(FHandle, 'OnStop');
    Pointer(RegisterSimbaPlugin) := GetProcedureAddress(FHandle, 'RegisterSimbaPlugin');
  end;

  // Methods
  if Assigned(FExports.GetFunctionCount) and Assigned(FExports.GetFunctionInfo) then
  begin
    SetLength(FMethods, FExports.GetFunctionCount());
    for Index := 0 to High(FMethods) do
      FMethods[Index] := LoadMethod(Index);
  end;

  // Types
  if Assigned(FExports.GetTypeCount) and Assigned(FExports.GetTypeInfo) then
  begin
    SetLength(FTypes, FExports.GetTypeCount());
    for Index := 0 to High(FTypes) do
      FTypes[Index] := LoadType(Index);
  end;

  // Code
  if Assigned(FExports.GetCodeLength) and Assigned(FExports.GetCode) then
    FCode := LoadCode();

  if Assigned(FExports.OnAttach) then
    FExports.OnAttach(nil);
end;

procedure TSimbaScriptPlugin.CallOnPause;
begin
  if Assigned(FExports.OnPause) then FExports.OnPause();
end;

procedure TSimbaScriptPlugin.CallOnResume;
begin
  if Assigned(FExports.OnResume) then FExports.OnResume();
end;

procedure TSimbaScriptPlugin.CallOnStop;
begin
  if Assigned(FExports.OnStop) then FExports.OnStop();
end;

function TSimbaScriptPlugin.Dump: TStringList;
var
  I: Int32;
begin
  Result := TStringList.Create();

  for I := 0 to High(FTypes) do
    Result.Add('type ' + FTypes[i].Name + ' = ' + FTypes[i].Str);
  for I := 0 to High(FMethods) do
    Result.Add(FMethods[i].Header + 'external;');

  if (FCode <> '') then
    Result.Add(FCode);
end;

procedure TSimbaScriptPlugin.Import(Compiler: TSimbaScript_Compiler);
var
  I: Int32;
begin
  for I := 0 to High(FTypes) do
    Compiler.addGlobalType(FTypes[I].Str, FTypes[I].Name);

  for I := 0 to High(FMethods) do
  begin
    if FMethods[I].Native then
      Compiler.addGlobalFunc(FMethods[I].Header, FMethods[I].Address)
    else
      Compiler.addGlobalFunc(FMethods[I].Header, FMethods[I].Address, {$IF DECLARED(FFI_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
  end;

  if (FCode <> '') then
    Compiler.pushCode(FCode);

  GetMemoryManager(FMemoryManager);
  if Assigned(FExports.SetPluginMemManager) then
    FExports.SetPluginMemManager(FMemoryManager);

  FInfo.SimbaMajor := SIMBA_MAJOR;
  FInfo.SimbaVersion := SIMBA_VERSION;
  FInfo.FileName := PChar(FFileName);
  FInfo.Compiler := Compiler;

  if Assigned(FExports.RegisterSimbaPlugin) then
    FExports.RegisterSimbaPlugin(@FInfo, @SimbaPluginMethods);
end;

constructor TSimbaScriptPlugin.Create(FileName: String; ExtraSearchDirs: TStringArray);
begin
  inherited Create();

  FFileName := FileName;
  FHandle := LoadPlugin(FFileName, ExtraSearchDirs);

  Load();
end;

destructor TSimbaScriptPlugin.Destroy;
begin
  if Assigned(FExports.OnDetach) then
  try
    FExports.OnDetach();
  except
  end;

  if (FHandle > 0) then
    FreeLibrary(FHandle);

  inherited Destroy();
end;

end.

