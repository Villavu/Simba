unit simba.scriptpluginloader;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, dynlibs;

type
  TSynchronizeMethod = procedure(Data: Pointer); cdecl;

  TSimbaMethods = packed record
    Synchronize: procedure(Method: TSynchronizeMethod; Data: Pointer = nil); cdecl; // Run a method on the main thread
  end;

  TSimbaMemoryAllocators = packed record
    GetMem: function(Size: PtrUInt): Pointer; cdecl;
    FreeMem: function(P: Pointer): PtrUInt; cdecl;
    AllocMem: function(Size: PtrUInt): Pointer; cdecl;
    ReAllocMem: function(var P: Pointer; Size: PtrUInt): Pointer; cdecl;
    MemSize: function(P: Pointer): PtrUInt; cdecl;
  end;

  TGetPluginABIVersion = function: Int32; cdecl;
  TGetFunctionInfo = function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
  TGetFunctionCount = function: Int32; cdecl;
  TGetTypeInfo = function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
  TGetTypeCount = function: Int32; cdecl;
  TGetCode = procedure(var Code: PChar); cdecl;
  TGetCodeLength = function: Int32; cdecl;
  TSetPluginMemManager = procedure(MemoryManager: TMemoryManager); cdecl;
  TSetPluginSimbaMethods = procedure(Methods: TSimbaMethods); cdecl;
  TSetPluginSimbaMemoryAllocators = procedure(Allocators: TSimbaMemoryAllocators); cdecl;
  TOnAttach = procedure(Data: Pointer); cdecl;
  TOnDetach = procedure; cdecl;

  TSimbaScriptPluginLoader_Method = record
    Header: String;
    Native: Boolean;
    Address: Pointer;
  end;

  TSimbaScriptPluginLoader_Type = record
    Name: String;
    Str: String;
  end;

  TSimbaScriptPluginLoader_Methods = array of TSimbaScriptPluginLoader_Method;
  TSimbaScriptPluginLoader_Types = array of TSimbaScriptPluginLoader_Type;

  TSimbaScriptPluginLoader = class
  protected
    FHandle: TLibHandle;
    FFileName: String;
    FCode: String;
    FTypes: TSimbaScriptPluginLoader_Types;
    FMethods: TSimbaScriptPluginLoader_Methods;
  public
    GetPluginABIVersion: TGetPluginABIVersion;
    GetFunctionInfo: TGetFunctionInfo;
    GetFunctionCount: TGetFunctionCount;
    GetTypeInfo: TGetTypeInfo;
    GetTypeCount: TGetTypeCount;
    GetCode: TGetCode;
    GetCodeLength: TGetCodeLength;
    SetPluginMemManager: TSetPluginMemManager;
    SetPluginSimbaMethods: TSetPluginSimbaMethods;
    SetPluginSimbaMemoryAllocators: TSetPluginSimbaMemoryAllocators;
    OnAttach: TOnAttach;
    OnDetach: TOnDetach;

    property Types: TSimbaScriptPluginLoader_Types read FTypes;
    property Methods: TSimbaScriptPluginLoader_Methods read FMethods;
    property Code: String read FCode;

    function Dump: String;

    class function FindFile(var FileName: String; SearchPaths: array of String): Boolean;

    constructor Create(FileName: String);
    destructor Destroy; override;
  end;

implementation

function TSimbaScriptPluginLoader.Dump: String;
var
  I: Int32;
begin
  Result := '';

  for I := 0 to High(FTypes) do
    Result := Result + 'type ' + FTypes[i].Name + ' = ' + FTypes[i].Str + LineEnding;
  for I := 0 to High(FMethods) do
    Result := Result + FMethods[i].Header + 'begin end;' + LineEnding;

  Result := Result + FCode;
end;

class function TSimbaScriptPluginLoader.FindFile(var FileName: String; SearchPaths: array of String): Boolean;
var
  I: Int32;
  Search: String;
begin
  Result := False;

  if FileExists(FileName) then
    Exit(True);

  for I := 0 to High(SearchPaths) do
  begin
    Search := IncludeTrailingPathDelimiter(SearchPaths[i]) + FileName;
    if FileExists(Search) then
    begin
      FileName := Search;

      Exit(True);
    end;

    Search := IncludeTrailingPathDelimiter(SearchPaths[i]) + FileName + '.' + SharedSuffix;
    if FileExists(Search) then
    begin
      FileName := Search;

      Exit(True);
    end;

    Search := IncludeTrailingPathDelimiter(SearchPaths[i]) + FileName + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + '.' + SharedSuffix;
    if FileExists(Search) then
    begin
      FileName := Search;

      Exit(True);
    end;
  end;
end;

constructor TSimbaScriptPluginLoader.Create(FileName: String);

  function LoadCode: String;
  var
    Buffer: PChar;
  begin
    Buffer := StrAlloc(GetCodeLength() + 1);

    try
      GetCode(Buffer);

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
      GetFunctionInfo(Index, Result.Address, Buffer);

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
      GetTypeInfo(Index, Buffer.Name, Buffer.Str);

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
  WriteLn('Loading Plugin: ', ExtractFileName(FileName));

  FFileName := FileName;
  FHandle := LoadLibrary(FFileName);

  if (FHandle = 0) then
    raise Exception.Create('Loading plugin failed. Architecture mismatch? (expected a ' + {$IFDEF CPU32}'32'{$ELSE}'64'{$ENDIF} + ' bit plugin)');

  Pointer(GetPluginABIVersion) := GetProcedureAddress(FHandle, 'GetPluginABIVersion');
  Pointer(GetFunctionInfo) := GetProcedureAddress(FHandle, 'GetFunctionInfo');
  Pointer(GetFunctionCount) := GetProcedureAddress(FHandle, 'GetFunctionCount');
  Pointer(GetTypeInfo) := GetProcedureAddress(FHandle, 'GetTypeInfo');
  Pointer(GetTypeCount) := GetProcedureAddress(FHandle, 'GetTypeCount');
  Pointer(GetCode) := GetProcedureAddress(FHandle, 'GetCode');
  Pointer(GetCodeLength) := GetProcedureAddress(FHandle, 'GetCodeLength');
  Pointer(SetPluginMemManager) := GetProcedureAddress(FHandle, 'SetPluginMemManager');
  Pointer(SetPluginSimbaMethods) := GetProcedureAddress(FHandle, 'SetPluginSimbaMethods');
  Pointer(SetPluginSimbaMemoryAllocators) := GetProcedureAddress(FHandle, 'SetPluginSimbaMemoryAllocators');
  Pointer(OnAttach) := GetProcedureAddress(FHandle, 'OnAttach');
  Pointer(OnDetach) := GetProcedureAddress(FHandle, 'OnDetach');

  if (Pointer(GetFunctionCount) <> nil) and (Pointer(GetFunctionInfo) <> nil) then
  begin
    SetLength(FMethods, GetFunctionCount());
    for Index := 0 to High(FMethods) do
      FMethods[Index] := LoadMethod(Index);
  end;

  if (Pointer(GetTypeCount) <> nil) and (Pointer(GetTypeInfo) <> nil) then
  begin
    SetLength(FTypes, GetTypeCount());
    for Index := 0 to High(FTypes) do
      FTypes[Index] := LoadType(Index);
  end;

  if (Pointer(GetCodeLength) <> nil) and (Pointer(GetCode) <> nil) then
    FCode := LoadCode();
end;

destructor TSimbaScriptPluginLoader.Destroy;
begin
  if (FHandle > 0) then
  begin
    WriteLn('Free Plugin: ' + ExtractFileName(FFileName));

    FreeLibrary(FHandle);
  end;

  inherited Destroy();
end;

end.

