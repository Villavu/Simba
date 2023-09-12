{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_plugin;

{$i simba.inc}

interface

uses
  Classes, SysUtils, DynLibs,
  simba.mufasatypes, simba.script_compiler, simba.script_pluginmethods;

type
  PSimbaPluginInfo = ^TSimbaPluginInfo;
  TSimbaPluginInfo = packed record
    SimbaVersion: Integer;
    SimbaMajor: Integer;

    FileName: PChar;

    Compiler: Pointer;

    // Extend this but do not remove, reorder or change datatypes.
  end;

  TSimbaPluginExports = packed record
    GetFunctionInfo: function(Index: Integer; var Address: Pointer; var Header: PChar): Integer; cdecl;
    GetFunctionCount: function: Integer; cdecl;
    GetTypeInfo: function(Index: Integer; var Name: PChar; var Str: PChar): Integer; cdecl;
    GetTypeCount: function: Integer; cdecl;
    GetCode: procedure(var Code: PChar); cdecl;
    GetCodeLength: function: Integer; cdecl;
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

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

  TSimbaScriptPluginArray = array of TSimbaScriptPlugin;
  TSimbaScriptPluginArrayHelper = type helper for TSimbaScriptPluginArray
  public
    procedure CallOnPause;
    procedure CallOnResume;
    procedure CallOnStop;
  end;

implementation

uses
  ffi;

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

  function LoadMethod(Index: Integer): TSimbaScriptPluginMethod;
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

  function LoadType(Index: Integer): TSimbaScriptPluginType;
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
  Index: Integer;
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

function TSimbaScriptPlugin.Dump: TStringList;
var
  I: Integer;
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
  I: Integer;
begin
  for I := 0 to High(FTypes) do
    Compiler.addGlobalType(FTypes[I].Str, FTypes[I].Name);

  for I := 0 to High(FMethods) do
  begin
    if FMethods[I].Native then
      Compiler.addGlobalFunc(FMethods[I].Header, FMethods[I].Address)
    else
      Compiler.addGlobalFunc(FMethods[I].Header, FMethods[I].Address, FFI_DEFAULT_ABI);
  end;

  if (FCode <> '') then
    Compiler.addDelayedCode(FCode, '!' + FFileName, False);

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

constructor TSimbaScriptPlugin.Create(FileName: String);
begin
  inherited Create();

  FFileName := FileName;
  if (not FileExists(FFileName)) then
    raise Exception.CreateFmt('Loading plugin: File "%s" does not exist', [FFileName]);

  FHandle := LoadLibrary(FFileName);

  if (FHandle = 0) then
  begin
    DebugLn('Plugin filename: ' + FFileName);
    DebugLn('Plugin load error: ' + GetLoadErrorStr());

    raise Exception.Create('Loading plugin failed. Architecture mismatch? (expected a ' + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + ' bit plugin)');
  end;

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

procedure TSimbaScriptPluginArrayHelper.CallOnPause;
var
  I: Integer;
begin
  for I := 0 to High(Self) do
    if Assigned(Self[I].FExports.OnPause) then
      Self[I].FExports.OnPause();
end;

procedure TSimbaScriptPluginArrayHelper.CallOnResume;
var
  I: Integer;
begin
  for I := 0 to High(Self) do
    if Assigned(Self[I].FExports.OnResume) then
      Self[I].FExports.OnResume();
end;

procedure TSimbaScriptPluginArrayHelper.CallOnStop;
var
  I: Integer;
begin
  for I := 0 to High(Self) do
    if Assigned(Self[I].FExports.OnStop) then
      Self[I].FExports.OnStop();
end;

end.

