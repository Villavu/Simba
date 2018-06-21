unit script_plugins_exports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSynchronizeMethod = procedure(Data: Pointer); cdecl;

  TSimbaMethods = packed record
    Synchronize: procedure(Method: TSynchronizeMethod; Data: Pointer = nil); cdecl;
  end;

  TSimbaMemoryAllocators = packed record
    GetMem: function(Size: PtrUInt): Pointer; cdecl;
    FreeMem: function(Ptr: Pointer): PtrUInt; cdecl;
  end;

var
  SimbaMethods: TSimbaMethods;
  SimbaMemoryAllocators: TSimbaMemoryAllocators;

implementation

function _GetMem(Size: PtrUInt): Pointer; cdecl;
begin
  Result := GetMem(Size);
end;

function _FreeMem(Ptr: Pointer): PtrUInt; cdecl;
begin
  Result := FreeMem(Ptr);
end;

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

initialization
  with SimbaMethods do
    Synchronize := @_Synchronize;

  with SimbaMemoryAllocators do
  begin
    GetMem := @_GetMem;
    FreeMem := @_FreeMem;
  end;

end.

