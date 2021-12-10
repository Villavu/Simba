{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script_plugin;

(*

  // procedure Test1(out test: TIntegerArray); native;
  procedure Test1(const Params: PParamArray); cdecl;
  var
    Arr, Element: Pointer;
    TypeInfo: Pointer;
    Index: Integer;
  begin
    Arr := nil;

    TypeInfo := MyMethods^.GetTypeInfo('TIntegerArray');

    MyMethods^.SetArrayLength(TypeInfo, Arr, 10);
    for Index := 0 to MyMethods^.GetArrayLength(Arr) - 1 do
    begin
      Element := MyMethods^.GetArrayElement(TypeInfo, Arr, Index);
      Integer(Element^) := Index;
    end;

    PPointer(Params^[0])^ := Arr;
  end;

*)

(*

  // function Test2: array of record Int: Integer; Str: String; end; native;
  procedure Test2(const Params: PParamArray; const Result: Pointer); cdecl;
  var
    TestInteger: Integer = 123456;
    TestString: String = 'Hello World';
  var
    Arr, Field: Pointer;
    ArrInfo, RecInfo, StrInfo: Pointer;
  begin
    Arr := nil;

    ArrInfo := MyMethods^.GetTypeInfo('array of record Int: Integer; Str: String; end');
    RecInfo := MyMethods^.GetTypeInfo('record Int: Integer; Str: String; end');
    StrInfo := MyMethods^.GetTypeInfo('String');

    MyMethods^.SetArrayLength(ArrInfo, Arr, 3);

    // Result[1].Int := testInt;
    Field := MyMethods^.GetArrayElement(ArrInfo, Arr, 1) + MyMethods^.GetFieldOffset(RecInfo, 'Int');
    Move(TestInteger, Field^, SizeOf(Integer));

    // SetLength(Result[1].Str, Length(TestString));
    Field := MyMethods^.GetArrayElement(ArrInfo, Arr, 1) + MyMethods^.GetFieldOffset(RecInfo, 'Str');
    MyMethods^.SetFieldArrayLength(StrInfo, Field, Length(TestString));

    // Result[1].Str := TestString;
    Field := MyMethods^.GetFieldArrayElement(StrInfo, Field, 0);
    Move(TestString[1], Field^, Length(TestString));

    PPointer(Result)^ := Arr;
  end;

*)

{$i simba.inc}

interface

uses
  classes, sysutils, dynlibs, lpcompiler,
  simba.script_compiler;

type
  TSimbaSynchronizeMethod = procedure(Data: Pointer); cdecl;

  PSimbaInfomation = ^TSimbaInfomation;
  TSimbaInfomation = packed record
    SimbaVersion: Integer;
    SimbaMajor: Integer;

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

    RaiseException: procedure(Message: PChar); cdecl;

    GetTypeInfo: function(Str: PChar): Pointer; cdecl;
    GetTypeSize: function(TypeInfo: Pointer): Integer; cdecl;
    GetTypeBaseType: function(TypeInfo: Pointer): Integer; cdecl;
    GetTypeClassName: function(TypeInfo: Pointer; Buffer: PChar): Integer; cdecl;

    GetArrayElement: function(TypeInfo: Pointer; AVar: Pointer; Index: Integer): Pointer;  cdecl;
    GetArrayLength: function(AVar: Pointer): Integer; cdecl;
    SetArrayLength: procedure(TypeInfo: Pointer; var AVar: Pointer; Len: Integer); cdecl;

    GetFieldOffset: function(TypeInfo: Pointer; FieldName: PChar): Integer; cdecl;
    GetFieldArrayLength: function(AVar: Pointer): Integer; cdecl;
    SetFieldArrayLength: procedure(TypeInfo: Pointer; var AVar: Pointer; Len: Integer); cdecl;
    GetFieldArrayElement: function(TypeInfo: Pointer; AVar: Pointer; Index: Integer): Pointer; cdecl;

    // Extend this as much as you want, do not remove, reorder or change datatypes.
  end;

  TSimbaPluginExports = packed record
    GetPluginABIVersion: function: Integer; cdecl;
    GetFunctionInfo: function(Index: Integer; var Address: Pointer; var Header: PChar): Integer; cdecl;
    GetFunctionCount: function: Integer; cdecl;
    GetTypeInfo: function(Index: Integer; var Name: PChar; var Str: PChar): Integer; cdecl;
    GetTypeCount: function: Integer; cdecl;
    GetCode: procedure(var Code: PChar); cdecl;
    GetCodeLength: function: Integer; cdecl;
    SetPluginMemManager: procedure(MemoryManager: TMemoryManager); cdecl;

    RegisterSimbaPlugin: procedure(Infomation: PSimbaInfomation; Methods: PSimbaMethods); cdecl;

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
  class var
    FBaseCompiler: TSimbaScript_Compiler; // Type info.. Use script compiler.
  protected
    FFileName: String;
    FHandle: TLibHandle;

    FSimbaInfomation: TSimbaInfomation;
    FSimbaMethods: TSimbaMethods;
    FSimbaMemoryManager: TMemoryManager;

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
  ffi, lptypes, lpvartypes, lpvartypes_array, lpvartypes_record;

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

procedure _RaiseException(Message: PChar); cdecl;
begin
  raise Exception.Create(Message);
end;

function _GetTypeInfo(Str: PChar): Pointer; cdecl;
var
  Typ: TLapeType absolute Result;
begin
  Result := nil;

  with TSimbaScriptPlugin.FBaseCompiler do
  try
    Typ := getGlobalType(Str);
    if (Typ <> nil) then
      Exit;

    Typ := addGlobalType(Str, '_GetTypeInfo');
    if (Typ <> nil) then
      Typ.Name := Str;
  except
  end;
end;

function _GetFieldOffset(TypeInfo: Pointer; Name: PChar): Integer; cdecl;
begin
  Result := TLapeType_Record(TypeInfo).FieldMap[Name].Offset;
end;

procedure _SetArrayLength(TypeInfo: Pointer; var AVar: Pointer; Len: Integer); cdecl;
begin
  TLapeType_DynArray(TypeInfo).VarSetLength(AVar, Len);
end;

function _GetArrayLength(AVar: Pointer): Integer; cdecl;
begin
  Result := DynArraySize(AVar);
end;

function _GetArrayElement(TypeInfo: Pointer; AVar: Pointer; Index: Integer): Pointer; cdecl;
begin
  Result := AVar + (Index * TLapeType_DynArray(TypeInfo).PType.Size);
end;

function _GetFieldArrayLength(AVar: Pointer): Integer; cdecl;
begin
  Result := DynArraySize(PPointer(AVar)^);
end;

procedure _SetFieldArrayLength(TypeInfo: Pointer; var AVar: Pointer; Len: Integer); cdecl;
begin
  with TLapeType_DynArray(TypeInfo) do
    VarSetLength(PPointer(AVar)^, Len);
end;

function _GetFieldArrayElement(TypeInfo: Pointer; AVar: Pointer; Index: Integer): Pointer; cdecl;
begin
  with TLapeType_DynArray(TypeInfo) do
    Result := PPointer(AVar + (Index * PType.Size))^;
end;

function _GetTypeSize(TypeInfo: Pointer): Integer; cdecl;
begin
  with TLapeType(TypeInfo) do
    Result := Size;
end;

function _GetTypeClassName(TypeInfo: Pointer; Buffer: PChar): Integer; cdecl;
begin
  with TLapeType(TypeInfo) do
  begin
    Result := Length(ClassName);
    if (Result > 0) then
      Move(ClassName[1], Buffer^, Result);
  end;
end;

function _GetTypeBaseType(TypeInfo: Pointer): Integer; cdecl;
begin
  with TLapeType(TypeInfo) do
    Result := Ord(BaseType);
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
    Pointer(OnPause)             := GetProcedureAddress(FHandle, 'OnPause');
    Pointer(OnResume)            := GetProcedureAddress(FHandle, 'OnResume');
    Pointer(OnStop)              := GetProcedureAddress(FHandle, 'OnStop');
    Pointer(RegisterSimbaPlugin) := GetProcedureAddress(FHandle, 'RegisterSimbaPlugin');
  end;

  if Assigned(FExports.SetPluginMemManager) then FExports.SetPluginMemManager(FSimbaMemoryManager);
  if Assigned(FExports.RegisterSimbaPlugin) then FExports.RegisterSimbaPlugin(@FSimbaInfomation, @FSimbaMethods);

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
      Compiler.addGlobalFunc(FMethods[I].Header, FMethods[I].Address, {$IF DECLARED(FFI_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
  end;

  if (FCode <> '') then
    Compiler.addDelayedCode(FCode, '!' + FFileName, False);
end;

constructor TSimbaScriptPlugin.Create(FileName: String);
begin
  inherited Create();

  if (FBaseCompiler = nil) then
  begin
    FBaseCompiler := TSimbaScript_Compiler.Create(nil);
    FBaseCompiler.Import();
  end;

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

  FSimbaMethods.RaiseException := @_RaiseException;

  FSimbaMethods.GetTypeInfo := @_GetTypeInfo;
  FSimbaMethods.GetTypeSize := @_GetTypeSize;
  FSimbaMethods.GetTypeBaseType := @_GetTypeBaseType;
  FSimbaMethods.GetTypeClassName := @_GetTypeClassName;

  FSimbaMethods.GetArrayElement := @_GetArrayElement;
  FSimbaMethods.GetArrayLength := @_GetArrayLength;
  FSimbaMethods.SetArrayLength := @_SetArrayLength;

  FSimbaMethods.GetFieldOffset := @_GetFieldOffset;
  FSimbaMethods.GetFieldArrayLength := @_GetFieldArrayLength;
  FSimbaMethods.SetFieldArrayLength := @_SetFieldArrayLength;
  FSimbaMethods.GetFieldArrayElement := @_GetFieldArrayElement;

  FSimbaInfomation.FileName := PChar(FFileName);
  FSimbaInfomation.SimbaMajor := SIMBA_MAJOR;
  FSimbaInfomation.SimbaVersion := SIMBA_VERSION;

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

finalization
  if (TSimbaScriptPlugin.FBaseCompiler <> nil) then
    FreeAndNil(TSimbaScriptPlugin.FBaseCompiler);

end.

