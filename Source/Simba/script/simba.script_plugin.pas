unit simba.script_plugin;

{$DEFINE DEPRECATED}

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils, dynlibs,
  simba.script_compiler;

type
  TSimbaSynchronizeMethod = procedure(Data: Pointer); cdecl;

  PSimbaInfomation = ^TSimbaInfomation;
  TSimbaInfomation = packed record
    SimbaVersion: Int32;
    SimbaMajor: Int32;

    FileName: PChar;

    // Extend this as much as you want, do not remove, reorder or change datatypes.
  end;

  PSimbaMethods = ^TSimbaMethods;
  TSimbaMethods = packed record
    Synchronize: procedure(Method: TSimbaSynchronizeMethod; Data: Pointer = nil); cdecl;

    GetMem: function(Size: PtrUInt): Pointer; cdecl;
    FreeMem: function(P: Pointer): PtrUInt; cdecl;
    AllocMem: function(Size: PtrUInt): Pointer; cdecl;
    ReAllocMem: function(var P: Pointer; Size: PtrUInt): Pointer; cdecl;
    MemSize: function(P: Pointer): PtrUInt; cdecl;

    // Extend this as much as you want, do not remove, reorder or change datatypes.
  end;

  TGetPluginABIVersion = function: Int32; cdecl;
  TGetFunctionInfo = function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
  TGetFunctionCount = function: Int32; cdecl;
  TGetTypeInfo = function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
  TGetTypeCount = function: Int32; cdecl;
  TGetCode = procedure(var Code: PChar); cdecl;
  TGetCodeLength = function: Int32; cdecl;
  TSetPluginMemManager = procedure(MemoryManager: TMemoryManager); cdecl;
  TOnAttach = procedure(Data: Pointer); cdecl;
  TOnDetach = procedure; cdecl;
  TRegisterSimbaPlugin = procedure(Infomation: PSimbaInfomation; Methods: PSimbaMethods); cdecl;

  {$IFDEF DEPRECATED}
  TSimbaSyncMethods = packed record
    Synchronize: procedure(Method: TSimbaSynchronizeMethod; Data: Pointer = nil); cdecl;
  end;

  TSimbaMemoryAllocators = packed record
    GetMem: function(Size: PtrUInt): Pointer; cdecl;
    FreeMem: function(P: Pointer): PtrUInt; cdecl;
  end;

  TSetPluginSimbaMethods = procedure(Methods: TSimbaSyncMethods); cdecl;
  TSetPluginSimbaMemoryAllocators = procedure(Allocators: TSimbaMemoryAllocators); cdecl;
  {$ENDIF}

  TSimbaScriptPluginLoader_Method = record
    Header: String;
    Address: Pointer;
    Native: Boolean;
  end;

  TSimbaScriptPluginLoader_Type = record
    Name: String;
    Str: String;
  end;

  TSimbaScriptPluginLoader_Methods = array of TSimbaScriptPluginLoader_Method;
  TSimbaScriptPluginLoader_Types = array of TSimbaScriptPluginLoader_Type;

  TSimbaScriptPlugin = class
  protected
    FFileName: String;
    FHandle: TLibHandle;

    FSimbaInfomation: TSimbaInfomation;
    FSimbaMethods: TSimbaMethods;
    FSimbaMemoryManager: TMemoryManager;

    {$IFDEF DEPRECATED}
    FSimbaSyncMethods: TSimbaSyncMethods;
    FSimbaMemoryAllocators: TSimbaMemoryAllocators;
    {$ENDIF}

    FExports: record
      GetPluginABIVersion: TGetPluginABIVersion;
      GetFunctionInfo: TGetFunctionInfo;
      GetFunctionCount: TGetFunctionCount;
      GetTypeInfo: TGetTypeInfo;
      GetTypeCount: TGetTypeCount;
      GetCode: TGetCode;
      GetCodeLength: TGetCodeLength;
      SetPluginMemManager: TSetPluginMemManager;

      {$IFDEF DEPRECATED}
      SetPluginSimbaMethods: TSetPluginSimbaMethods;
      SetPluginSimbaMemoryAllocators: TSetPluginSimbaMemoryAllocators;
      {$ENDIF}

      RegisterSimbaPlugin: TRegisterSimbaPlugin;

      OnAttach: TOnAttach;
      OnDetach: TOnDetach;
    end;

    FCode: String;
    FTypes: TSimbaScriptPluginLoader_Types;
    FMethods: TSimbaScriptPluginLoader_Methods;
  public
    function Dump: TStringList;

    procedure Import(Compiler: TSimbaScript_Compiler);
    procedure Load;

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

  TSimbaScriptPluginArray = array of TSimbaScriptPlugin;

implementation

uses
  ffi;

// cdecl wrappers
function _GetMem(Size: PtrUInt): Pointer; cdecl;
begin
  Result := GetMem(Size);
end;

function _FreeMem(Ptr: Pointer): PtrUInt; cdecl;
begin
  Result := FreeMem(Ptr);
end;

function _AllocMem(Size: PtrUInt): Pointer; cdecl;
begin
  Result := AllocMem(Size);
end;

function _ReAllocMem(var P: Pointer; Size: PtrUInt): Pointer; cdecl;
begin
  Result := ReAllocMem(P, Size);
end;

function _MemSize(P: Pointer): PtrUInt; cdecl;
begin
  Result := MemSize(P);
end;

// Sync wrapper which includes a data parameter
type
  TSync = class
    Data: Pointer;
    Method: TSimbaSynchronizeMethod;

    procedure Execute;
  end;

procedure TSync.Execute;
begin
  Method(Data);
end;

procedure _Synchronize(Method: TSimbaSynchronizeMethod; Data: Pointer = nil); cdecl;
var
  Sync: TSync;
begin
  Sync := TSync.Create();
  Sync.Data := Data;
  Sync.Method := Method;

  TThread.Synchronize(TThread.CurrentThread, @Sync.Execute);

  Sync.Free();
end;

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

  function LoadMethod(Index: Int32): TSimbaScriptPluginLoader_Method;
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

  function LoadType(Index: Int32): TSimbaScriptPluginLoader_Type;
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
    Pointer(GetPluginABIVersion) := GetProcedureAddress(FHandle, 'GetPluginABIVersion');
    Pointer(GetFunctionInfo)     := GetProcedureAddress(FHandle, 'GetFunctionInfo');
    Pointer(GetFunctionCount)    := GetProcedureAddress(FHandle, 'GetFunctionCount');
    Pointer(GetTypeInfo)         := GetProcedureAddress(FHandle, 'GetTypeInfo');
    Pointer(GetTypeCount)        := GetProcedureAddress(FHandle, 'GetTypeCount');
    Pointer(GetCode)             := GetProcedureAddress(FHandle, 'GetCode');
    Pointer(GetCodeLength)       := GetProcedureAddress(FHandle, 'GetCodeLength');
    Pointer(SetPluginMemManager) := GetProcedureAddress(FHandle, 'SetPluginMemManager');
    Pointer(OnAttach)            := GetProcedureAddress(FHandle, 'OnAttach');
    Pointer(OnDetach)            := GetProcedureAddress(FHandle, 'OnDetach');
    Pointer(RegisterSimbaPlugin) := GetProcedureAddress(FHandle, 'RegisterSimbaPlugin');

    {$IFDEF DEPRECATED}
    Pointer(SetPluginSimbaMethods)          := GetProcedureAddress(FHandle, 'SetPluginSimbaMethods');
    Pointer(SetPluginSimbaMemoryAllocators) := GetProcedureAddress(FHandle, 'SetPluginSimbaMemoryAllocators');
    {$ENDIF}
  end;

  {$IFDEF DEPRECATED}
  if Assigned(FExports.SetPluginSimbaMemoryAllocators) then FExports.SetPluginSimbaMemoryAllocators(FSimbaMemoryAllocators);
  if Assigned(FExports.SetPluginSimbaMethods) then          FExports.SetPluginSimbaMethods(FSimbaSyncMethods);
  {$ENDIF}

  if Assigned(FExports.SetPluginMemManager) then            FExports.SetPluginMemManager(FSimbaMemoryManager);
  if Assigned(FExports.RegisterSimbaPlugin) then            FExports.RegisterSimbaPlugin(@FSimbaInfomation, @FSimbaMethods);

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

  if Assigned(FExports.OnAttach) then FExports.OnAttach(nil);
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
    Compiler.addDelayedCode(FCode, '!' + FFileName, False);
end;

constructor TSimbaScriptPlugin.Create(FileName: String);
begin
  inherited Create();

  FFileName := FileName;
  FHandle := LoadLibrary(FFileName);

  if (FHandle = 0) then
    raise Exception.Create('Loading plugin failed. Architecture mismatch? (expected a ' + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + ' bit plugin)');

  FSimbaMethods.Synchronize := @_Synchronize;
  FSimbaMethods.GetMem := @_GetMem;
  FSimbaMethods.FreeMem := @_FreeMem;
  FSimbaMethods.AllocMem := @_AllocMem;
  FSimbaMethods.MemSize := @_MemSize;
  FSimbaMethods.ReAllocMem := @_ReAllocMem;

  FSimbaInfomation.FileName := PChar(FFileName);
  FSimbaInfomation.SimbaMajor := SIMBA_MAJOR;
  FSimbaInfomation.SimbaVersion := SIMBA_VERSION;

  {$IFDEF DEPRECATED}
  FSimbaMemoryAllocators.GetMem := @_GetMem;
  FSimbaMemoryAllocators.FreeMem := @_FreeMem;

  FSimbaSyncMethods.Synchronize := @_Synchronize;
  {$ENDIF}

  GetMemoryManager(FSimbaMemoryManager);

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

