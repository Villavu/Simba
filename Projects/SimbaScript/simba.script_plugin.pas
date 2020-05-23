unit simba.script_plugin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, dynlibs,
  lpcompiler, lptypes, lpffiwrappers, ffi,
  simba.script_plugin_exports;

type
  TMPlugin = class;
  TMPluginDeclaration = class;
  TMPluginDeclarations = class(specialize TFPGObjectList<TMPluginDeclaration>);

  TMPluginDeclaration = class
    procedure Import(Compiler: TLapeCompiler); virtual; abstract;
    procedure Dump(var Str: String); virtual; abstract;
  end;

  TMPluginMethod = class(TMPluginDeclaration)
  protected
    FHeader: String;
    FAddress: Pointer;
    FNative: Boolean;
  public
    procedure Import(Compiler: TLapeCompiler); override;
    procedure Dump(var Str: String); override;

    constructor Create(Address: Pointer; Header: String);
  end;

  TMPluginType = class(TMPluginDeclaration)
  protected
    FName: String;
    FStr: String;
  public
    procedure Import(Compiler: TLapeCompiler); override;
    procedure Dump(var Str: String); override;

    constructor Create(Name, Str: String);
  end;

  TMPluginCode = class(TMPluginDeclaration)
  protected
    FCode: String;
    FName: String;
  public
    procedure Import(Compiler: TLapeCompiler); override;
    procedure Dump(var Str: String); override;

    constructor Create(Code, Name: String);
  end;

  TMPlugin = class
  protected
    FLib: TLibHandle;
    FFilePath: String;
    FDeclarations: TMPluginDeclarations;

    GetPluginABIVersion: function: Int32; cdecl;
    GetFunctionInfo: function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
    GetFunctionCount: function: Int32; cdecl;
    GetTypeInfo: function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
    GetTypeCount: function: Int32; cdecl;
    GetCode: procedure(var Code: PChar); cdecl;
    GetCodeLength: function: Int32; cdecl;
    SetPluginMemManager: procedure(MemoryManager: TMemoryManager); cdecl;
    SetPluginSimbaMethods: procedure(Methods: TSimbaMethods); cdecl;
    SetPluginSimbaMemoryAllocators: procedure(Allocators: TSimbaMemoryAllocators); cdecl;
    OnAttach: procedure(Data: Pointer); cdecl;
    OnDetach: procedure; cdecl;
  public
    property FilePath: String read FFilePath;
    property Declarations: TMPluginDeclarations read FDeclarations;

    constructor Create(AFilePath: String);
    destructor Destroy; override;
  end;

  TScriptPluginList = specialize TFPGObjectList<TMPlugin>;

implementation

uses
  LazFileUtils, LCLProc
  {$IFDEF WINDOWS}, JwaWinBase, JwaWinNT {$ENDIF},
  simba.files;


{$IFDEF WINDOWS}
function CorrectArchitecture(FilePath: WideString): Boolean; // returns true if we can't figure it out.
var
  FileHandle, MapHandle: THandle;
  MapView: Pointer;
  PIDH: PImageDosHeader;
  PINTH: PImageNtHeaders;
  Base: Pointer;
begin
  FileHandle := 0;
  MapHandle := 0;
  MapView := nil;

  try
    FileHandle := CreateFileW(PWideChar(FilePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (FileHandle = 0) then
      Exit(True);

    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if (MapHandle = 0) then
      Exit(True);

    MapView := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
    if (MapView = nil) then
      Exit(True);

    PIDH := PImageDosHeader(MapView);
    if (PIDH^.e_magic = IMAGE_DOS_SIGNATURE) then
    begin
      Base := PIDH;
      PINTH := PIMAGENTHEADERS(Base + LongWord(PIDH^.e_lfanew));
      if PINTH^.Signature = IMAGE_NT_SIGNATURE then
      begin
        case PINTH^.OptionalHeader.Magic of
          $10B: Exit({$IFDEF CPU32}True{$ELSE}False{$ENDIF});
          $20B: Exit({$IFDEF CPU64}True{$ELSE}False{$ENDIF});
        end;
      end;
    end;
  finally
    if (FileHandle > 0) then
      CloseHandle(FileHandle);
    if (MapHandle > 0) then
      CloseHandle(MapHandle);
    if (MapView <> nil) then
      UnmapViewOfFile(MapView);
  end;

  Exit(True);
end;
{$ENDIF}

type
  TFFIWrapper = class(TLapeDeclaration)
  protected
    Wrapper: TImportClosure;
  public
    constructor Create(var Address: Pointer; Compiler: TLapeCompiler; Header: String); reintroduce;

    destructor Destroy; override;
  end;

constructor TFFIWrapper.Create(var Address: Pointer; Compiler: TLapeCompiler; Header: String);
begin
  inherited Create('!plugin');

  {$IFDEF CPU32}
  Wrapper := LapeImportWrapper(Address, Compiler, Header, FFI_CDECL);
  {$ELSE}
  Wrapper := LapeImportWrapper(Address, Compiler, Header, FFI_DEFAULT_ABI);
  {$ENDIF}

  Address := Wrapper.Func;
end;

destructor TFFIWrapper.Destroy;
begin
  Wrapper.Free();

  inherited Destroy();
end;

procedure TMPluginMethod.Import(Compiler: TLapeCompiler);
var
  Address: Pointer;
begin
  Address := FAddress;
  if (not FNative) then
    Compiler.addManagedDecl(TFFIWrapper.Create(Address, Compiler, FHeader));

  Compiler.addGlobalFunc(FHeader, Address);
end;

procedure TMPluginMethod.Dump(var Str: String);
begin
  Str := Str + FHeader + ' begin end;' + LineEnding;
end;

constructor TMPluginMethod.Create(Address: Pointer; Header: String);
begin
  FAddress := Address;
  FHeader := Trim(Header);
  if (Header[Length(FHeader)] <> ';') then
    FHeader := FHeader + ';';

  if FHeader.EndsWith('native;') then
  begin
    Header := Copy(FHeader, 1, Length(FHeader) - Length('native;'));
    Header := Trim(Header);

    FNative := Header[Length(Header)] = ';';
    if FNative then
      FHeader := Header;
  end;
end;

procedure TMPluginType.Import(Compiler: TLapeCompiler);
begin
  Compiler.addGlobalType(FStr, FName);
end;

procedure TMPluginType.Dump(var Str: String);
begin
  Str := Str + 'type ' + FName + ' = ' + FStr + LineEnding;
end;

constructor TMPluginType.Create(Name, Str: String);
begin
  FName := Name;
  FStr := Str;
  if (FStr[Length(FStr)] <> ';') then
    FStr := FStr + ';';
end;

procedure TMPluginCode.Import(Compiler: TLapeCompiler);
begin
  Compiler.addDelayedCode(FCode, FName, False, True);
end;

procedure TMPluginCode.Dump(var Str: String);
begin
  Str := Str + FCode + LineEnding;
end;

constructor TMPluginCode.Create(Code, Name: String);
begin
  FCode := Code;
  FName := Name;
end;

constructor TMPlugin.Create(AFilePath: String);
var
  Name, Str, Header: PChar;
  Index: Int32;
  Address: Pointer;
  MemoryManager: TMemoryManager;
begin
  WriteLn('Loading plugin: "', AFilePath, '"');

  FFilePath := AFilePath;
  FDeclarations := TMPluginDeclarations.Create();

  {$IFDEF WINDOWS}
  if (not CorrectArchitecture(FFilePath)) then
    raise Exception.Create('Architecture mismatch');
  {$ENDIF}

  FLib := DynLibs.LoadLibrary(FFilePath);
  if (FLib = NilHandle) then
    raise Exception.Create('LoadLibrary failed');

  Pointer(GetPluginABIVersion) := GetProcedureAddress(FLib, 'GetPluginABIVersion');
  Pointer(GetFunctionInfo) := GetProcedureAddress(FLib, 'GetFunctionInfo');
  Pointer(GetFunctionCount) := GetProcedureAddress(FLib, 'GetFunctionCount');
  Pointer(GetTypeInfo) := GetProcedureAddress(FLib, 'GetTypeInfo');
  Pointer(GetTypeCount) := GetProcedureAddress(FLib, 'GetTypeCount');
  Pointer(GetCode) := GetProcedureAddress(FLib, 'GetCode');
  Pointer(GetCodeLength) := GetProcedureAddress(FLib, 'GetCodeLength');
  Pointer(SetPluginMemManager) := GetProcedureAddress(FLib, 'SetPluginMemManager');
  Pointer(SetPluginSimbaMethods) := GetProcAddress(FLib, 'SetPluginSimbaMethods');
  Pointer(SetPluginSimbaMemoryAllocators) := GetProcAddress(FLib, 'SetPluginSimbaMemoryAllocators');
  Pointer(OnAttach) := GetProcedureAddress(FLib, 'OnAttach');
  Pointer(OnDetach) := GetProcedureAddress(FLib, 'OnDetach');

  if (Pointer(GetPluginABIVersion) = nil) or (GetPluginABIVersion() <> 2) then
    raise Exception.Create('ABI version not supported');

  GetMemoryManager(MemoryManager);
  if (Pointer(SetPluginMemManager) <> nil) then
    SetPluginMemManager(MemoryManager);

  if (Pointer(SetPluginSimbaMethods) <> nil) then
    SetPluginSimbaMethods(SimbaMethods);

  if (Pointer(SetPluginSimbaMemoryAllocators) <> nil) then
    SetPluginSimbaMemoryAllocators(SimbaMemoryAllocators);

  if (Pointer(GetTypeCount) <> nil) and (Pointer(GetTypeInfo) <> nil) then
  begin
    Name := StrAlloc(2048);
    Str := StrAlloc(2048);

    try
      for Index := 0 to GetTypeCount() - 1 do
      begin
        GetTypeInfo(Index, Name, Str);

        FDeclarations.Add(TMPluginType.Create(Name, Str));
      end;
    finally
      StrDispose(Name);
      StrDispose(Str);
    end;
  end;

  if (Pointer(GetFunctionCount) <> nil) and (Pointer(GetFunctionInfo) <> nil) then
  begin
    Address := nil;
    Header := StrAlloc(2048);

    try
      for Index := 0 to GetFunctionCount() - 1 do
      begin
        GetFunctionInfo(Index, Address, Header);

        FDeclarations.Add(TMPluginMethod.Create(Address, Header));
      end;
    finally
      StrDispose(Header);
    end;
  end;

  if (Pointer(GetCodeLength) <> nil) and (Pointer(GetCode) <> nil) then
  begin
    Str := StrAlloc(GetCodeLength() + 1);

    try
      GetCode(Str);

      FDeclarations.Add(TMPluginCode.Create(Str, ExtractFileNameOnly(FFilePath)));
    finally
      StrDispose(Str);
    end;
  end;

  if (Pointer(OnAttach) <> nil) then
    OnAttach(nil);
end;

destructor TMPlugin.Destroy;
begin
  try
    if (Pointer(OnDetach) <> nil) then
      OnDetach();
  except
  end;

  FreeLibrary(FLib);

  if (FDeclarations <> nil) then
    FDeclarations.Free();

  inherited Destroy();
end;

end.

