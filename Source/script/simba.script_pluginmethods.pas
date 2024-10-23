{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Functions passed to plugins in RegisterSimbaPlugin
}
unit simba.script_pluginmethods;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.externalcanvas,
  lpcompiler;

type
  TMainThreadMethod = procedure(Data: Pointer); cdecl;

  PSimbaPluginMethods = ^TSimbaPluginMethods;
  TSimbaPluginMethods = packed record
    RunOnMainThread: procedure(Method: TMainThreadMethod; Data: Pointer = nil); cdecl;

    GetMem: function(Size: NativeUInt): Pointer; cdecl;
    FreeMem: function(P: Pointer): NativeUInt; cdecl;
    AllocMem: function(Size: NativeUInt): Pointer; cdecl;
    ReAllocMem: function(var P: Pointer; Size: NativeUInt): Pointer; cdecl;
    MemSize: function(P: Pointer): NativeUInt; cdecl;

    RaiseException: procedure(Message: PChar); cdecl;

    GetTypeInfo: function(Compiler: Pointer; Typ: PChar): Pointer; cdecl;
    GetTypeInfoSize: function(TypeInfo: Pointer): NativeInt; cdecl;
    GetTypeInfoFieldOffset: function(TypeInfo: Pointer; FieldName: PChar): NativeInt; cdecl;

    AllocateRawArray: function(ElementSize, Len: NativeInt): Pointer; cdecl;
    ReAllocateRawArray: procedure(var Arr: Pointer; ElementSize, NewLen: NativeInt); cdecl;

    AllocateArray: function(TypeInfo: Pointer; Len: NativeInt): Pointer; cdecl;
    AllocateString: function(Data: PChar): Pointer; cdecl;
    AllocateUnicodeString: function(Data: PUnicodeChar): Pointer; cdecl;

    SetArrayLength: procedure(TypeInfo: Pointer; var AVar: Pointer; NewLen: NativeInt); cdecl;
    GetArrayLength: function(AVar: Pointer): NativeInt; cdecl;

    ExternalImage_Create: function(AutoResize: Boolean): Pointer; cdecl;
    ExternalImage_SetMemory: procedure(Img: Pointer; Data: PColorBGRA; AWidth, AHeight: Integer); cdecl;
    ExternalImage_Resize: procedure(Img: Pointer; NewWidth, NewHeight: Integer); cdecl;
    ExternalImage_SetUserData: procedure(Img: Pointer; UserData: Pointer); cdecl;
    ExternalImage_GetUserData: function(Img: Pointer): Pointer; cdecl;
    ExternalImage_SetName: procedure(Img: Pointer; Name: PChar); cdecl;

    // Safe to extend this but do not modify!
  end;

var
  SimbaPluginMethods: TSimbaPluginMethods;

implementation

uses
  lpvartypes, lpvartypes_record, lpvartypes_array;

// Sync wrapper which includes a data parameter
type
  TSync = class
    Data: Pointer;
    Method: TMainThreadMethod;

    procedure Execute;
  end;

procedure TSync.Execute;
begin
  Method(Data);
end;

procedure _RunOnMainThread(Method: TMainThreadMethod; Data: Pointer = nil); cdecl;
var
  Sync: TSync;
begin
  Sync := TSync.Create();
  Sync.Data := Data;
  Sync.Method := Method;

  TThread.Synchronize(TThread.CurrentThread, @Sync.Execute);

  Sync.Free();
end;

procedure _RaiseException(Message: PChar); cdecl;
begin
  raise Exception.Create(Message);
end;

function Plugin_GetMem(Size: NativeUInt): Pointer; cdecl;
begin
  Result := GetMem(Size);
end;

function Plugin_FreeMem(Ptr: Pointer): NativeUInt; cdecl;
begin
  Result := FreeMem(Ptr);
end;

function Plugin_AllocMem(Size: NativeUInt): Pointer; cdecl;
begin
  Result := AllocMem(Size);
end;

function Plugin_ReAllocMem(var P: Pointer; Size: NativeUInt): Pointer; cdecl;
begin
  Result := ReAllocMem(P, Size);
end;

function Plugin_MemSize(P: Pointer): NativeUInt; cdecl;
begin
  Result := MemSize(P);
end;

function Plugin_GetTypeInfo(Compiler: Pointer; Str: PChar): Pointer; cdecl;
var
  Typ: TLapeType absolute Result;
begin
  Result := nil;

  with TLapeCompiler(Compiler) do
  try
    Typ := getGlobalType(Str);

    // Check if plugin already added this
    if (Typ = nil) then
      Typ := getGlobalType('GetTypeInfo::' + Str);

    // Add it
    if (Typ = nil) then
    begin
      Typ := addGlobalType(Str, '_GetTypeInfo');
      if (Typ <> nil) then
        Typ.Name := 'GetTypeInfo::' + Str;
    end;
  except
    if (Typ <> nil) then
      Typ.Free();
    Result := nil;
  end;
end;

function Plugin_GetTypeInfoSize(TypeInfo: Pointer): NativeInt; cdecl;
begin
  Result := -1;
  if (TypeInfo <> nil) and (TObject(TypeInfo) is TLapeType) then
    Result := TLapeType(TypeInfo).Size;
end;

function Plugin_GetTypeInfoFieldOffset(TypeInfo: Pointer; FieldName: PChar): NativeInt; cdecl;
begin
  Result := -1;
  if (TypeInfo <> nil) and (TObject(TypeInfo) is TLapeType_Record) then
    Result := TLapeType_Record(TypeInfo).FieldMap[FieldName].Offset;
end;

procedure Plugin_ReAllocateRawArray(var AVar: Pointer; AElementSize, ALen: NativeInt); cdecl;
var
  OldLen, NewSize: NativeInt;
  DoFree: Boolean;
begin
  NewSize := ALen * AElementSize;
  DoFree := NewSize <= 0;
  Inc(NewSize, SizeOf(PtrInt) + SizeOf(SizeInt));

  if (AVar = nil) then
  begin
    if DoFree then
      Exit;
    AVar := AllocMem(NewSize);

    PtrInt(AVar^) := 1;
    Inc(PtrUInt(AVar), SizeOf(PtrInt));
    SizeInt(AVar^) := ALen {$IFDEF FPC}-1{$ENDIF};
    Inc(PtrUInt(AVar), SizeOf(SizeInt));
    Exit;
  end;

  Dec(PtrUInt(AVar), SizeOf(SizeInt));
  OldLen := SizeInt(AVar^) {$IFDEF FPC}+1{$ENDIF};
  Dec(PtrUInt(AVar), SizeOf(PtrInt));

  if (PtrInt(AVar^) <= 1) then
  begin
    if (ALen = OldLen) then
    begin
      Inc(PtrUInt(AVar), SizeOf(SizeInt) + SizeOf(PtrInt));
      Exit;
    end;

    if DoFree then
    begin
      FreeMem(AVar);
      AVar := nil;
      Exit;
    end;

    ReallocMem(AVar, NewSize);
    PtrInt(AVar^) := 1;
    Inc(PtrUInt(AVar), SizeOf(PtrInt));
    SizeInt(AVar^) := ALen {$IFDEF FPC}-1{$ENDIF};
    Inc(PtrUInt(AVar), SizeOf(SizeInt));

    if (ALen > OldLen) then
      FillChar(Pointer(PtrUInt(AVar) + (OldLen * AElementSize))^, (ALen - OldLen) * AElementSize, 0);
  end else
  begin
    Dec(PtrInt(AVar^));

    AVar := nil;
    Plugin_ReAllocateRawArray(AVar, AElementSize, ALen);
  end;
end;

function Plugin_AllocateRawArray(ElementSize, Len: NativeInt): Pointer; cdecl;
begin
  Result := nil;

  Plugin_ReAllocateRawArray(Result, ElementSize, Len);
end;

function Plugin_AllocateArray(TypeInfo: Pointer; Len: NativeInt): Pointer; cdecl;
begin
  Result := nil;

  TLapeType_DynArray(TypeInfo).VarSetLength(Result, Len);
end;

function Plugin_AllocateString(Data: PChar): Pointer; cdecl;
var
  Len: SizeInt;
begin
  Result := nil;

  Len := StrLen(Data);
  SetLength(String(Result), Len);
  if (Len > 0) then
    Move(Data^, String(Result)[1], Len);
end;

function Plugin_AllocateUnicodeString(Data: PUnicodeChar): Pointer; cdecl;
var
  Len: SizeInt;
begin
  Result := nil;

  Len := StrLen(Data);
  SetLength(UnicodeString(Result), Len);
  if (Len > 0) then
    Move(Data^, UnicodeString(Result)[1], Len * SizeOf(UnicodeChar));
end;

procedure Plugin_SetArrayLength(TypeInfo: Pointer; var Arr: Pointer; Len: NativeInt); cdecl;
begin
  TLapeType_DynArray(TypeInfo).VarSetLength(Arr, Len);
end;

function Plugin_GetArrayLength(Arr: Pointer): NativeInt; cdecl;
begin
  Result := DynArraySize(Arr);
end;

function Plugin_ExternalImage_Create(AutoResize: Boolean): Pointer; cdecl;
begin
  Result := TSimbaExternalCanvas.Create();

  TSimbaExternalCanvas(Result).AutoResize := AutoResize;
  TSimbaExternalCanvas(Result).FreeOnTerminate := True;
end;

procedure Plugin_ExternalImage_SetMemory(Img: Pointer; Data: PColorBGRA; AWidth, AHeight: Integer); cdecl;
begin
  TSimbaExternalCanvas(Img).SetMemory(Data, AWidth, AHeight);
end;

procedure Plugin_ExternalImage_Resize(Img: Pointer; NewWidth, NewHeight: Integer); cdecl;
begin
  TSimbaExternalCanvas(Img).Resize(NewWidth, NewHeight);
end;

procedure Plugin_ExternalImage_SetUserData(Img: Pointer; UserData: Pointer); cdecl;
begin
  TSimbaExternalCanvas(Img).UserData := UserData;
end;

function Plugin_ExternalImage_GetUserData(Img: Pointer): Pointer; cdecl;
begin
  Result := TSimbaExternalCanvas(Img).UserData;
end;

procedure Plugin_ExternalImage_SetName(Img: Pointer; Name: PChar); cdecl;
begin
  TSimbaExternalCanvas(Img).Name := Name;
end;

initialization

  with SimbaPluginMethods do
  begin
    RunOnMainThread := @_RunOnMainThread;
    RaiseException := @_RaiseException;

    GetMem := @Plugin_GetMem;
    FreeMem := @Plugin_FreeMem;
    AllocMem := @Plugin_AllocMem;
    MemSize := @Plugin_MemSize;
    ReAllocMem := @Plugin_ReAllocMem;

    GetTypeInfo := @Plugin_GetTypeInfo;
    GetTypeInfoSize := @Plugin_GetTypeInfoSize;
    GetTypeInfoFieldOffset := @Plugin_GetTypeInfoFieldOffset;

    AllocateRawArray := @Plugin_AllocateRawArray;
    ReAllocateRawArray := @Plugin_ReAllocateRawArray;

    AllocateArray := @Plugin_AllocateArray;
    AllocateString := @Plugin_AllocateString;
    AllocateUnicodeString := @Plugin_AllocateUnicodeString;

    SetArrayLength := @Plugin_SetArrayLength;
    GetArrayLength := @Plugin_GetArrayLength;

    ExternalImage_Create      := @Plugin_ExternalImage_Create;
    ExternalImage_SetMemory   := @Plugin_ExternalImage_SetMemory;
    ExternalImage_Resize      := @Plugin_ExternalImage_Resize;
    ExternalImage_GetUserData := @Plugin_ExternalImage_GetUserData;
    ExternalImage_SetUserData := @Plugin_ExternalImage_SetUserData;
    ExternalImage_SetName     := @Plugin_ExternalImage_SetName;
  end;

end.

