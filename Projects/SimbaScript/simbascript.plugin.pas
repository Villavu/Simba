unit simbascript.plugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  simba.scriptpluginloader, simbascript.compiler;

type
  TSimbaScriptPlugin = class(TSimbaScriptPluginLoader)
  protected
    FSimbaMethods: TSimbaMethods;
    FSimbaMemoryAllocators: TSimbaMemoryAllocators;
    FMemoryManager: TMemoryManager;
  public
    procedure Import(Compiler: TSimbaScript_Compiler);

    constructor Create(FileName: String); reintroduce;
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
    Method: TSynchronizeMethod;

    procedure Execute;
  end;

procedure TSync.Execute;
begin
  Method(Data);
end;

procedure _Synchronize(Method: TSynchronizeMethod; Data: Pointer = nil); cdecl;
var
  Sync: TSync;
begin
  Sync := TSync.Create();
  Sync.Data := Data;
  Sync.Method := Method;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
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
  inherited Create(FileName);

  if (Pointer(SetPluginMemManager) <> nil) then
  begin
    GetMemoryManager(FMemoryManager);

    SetPluginMemManager(FMemoryManager);
  end;

  if (Pointer(SetPluginSimbaMemoryAllocators) <> nil) then
  begin
    FSimbaMemoryAllocators.GetMem := @_GetMem;
    FSimbaMemoryAllocators.FreeMem := @_FreeMem;
    FSimbaMemoryAllocators.AllocMem := @_AllocMem;
    FSimbaMemoryAllocators.ReAllocMem := @_ReAllocMem;
    FSimbaMemoryAllocators.MemSize := @_MemSize;

    SetPluginSimbaMemoryAllocators(FSimbaMemoryAllocators);
  end;

  if (Pointer(SetPluginSimbaMethods) <> nil) then
  begin
    FSimbaMethods.Synchronize := @_Synchronize;

    SetPluginSimbaMethods(FSimbaMethods);
  end;

  if (Pointer(OnAttach) <> nil) then
    OnAttach(nil);
end;

destructor TSimbaScriptPlugin.Destroy;
begin
  if (Pointer(OnDetach) <> nil) then
    OnDetach();

  inherited Destroy();
end;

end.

