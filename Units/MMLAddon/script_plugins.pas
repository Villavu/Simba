unit script_plugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils {$IFDEF WINDOWS}, JwaWindows {$ENDIF}, dynlibs, fgl,
  lpcompiler, lptypes, lpffiwrappers, ffi;

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

  TMPluginDelayedCode = class(TMPluginDeclaration)
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
    FData: record
      GetMem: function(Size: PtrUInt): Pointer; cdecl;
      FreeMem: function(Ptr: Pointer): PtrUInt; cdecl;
      // + methods for creating pascal strings & arrays?
    end;

    GetFunctionInfo: function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
    GetFunctionCount: function: Int32; cdecl;
    GetTypeInfo: function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
    GetTypeCount: function: Int32; cdecl;
    GetDelayedCode: procedure(var Code: PChar); cdecl;
    GetDelayedCodeLength: function: Int32; cdecl;
    SetPluginMemManager: procedure(MemoryManager: TMemoryManager); cdecl;
    OnAttach: procedure(Data: Pointer); cdecl;
    OnDetach: procedure; cdecl;
  public
    property FilePath: String read FFilePath;
    property Declarations: TMPluginDeclarations read FDeclarations;

    procedure Load;

    constructor Create(AFilePath: String);
    destructor Destroy; override;
  end;

  TMPluginsList = specialize TFPGObjectList<TMPlugin>;
  TMPlugins = class
  protected
    FPlugins: TMPluginsList;
    FPaths: TStringList;

    function FindFile(Sender, Argument: String): String;
  public
    property Paths: TStringList read FPaths;

    function Get(Sender, Argument: String): TMPlugin;

    constructor Create;
    destructor Destroy; override;
  end;

var
  Plugins: TMPlugins;

implementation

uses
  LazFileUtils, LCLProc;

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

function _GetMem(Size: PtrUInt): Pointer; cdecl;
begin
  Result := GetMem(Size);
end;

function _FreeMem(Ptr: Pointer): PtrUInt; cdecl;
begin
  Result := FreeMem(Ptr);
end;

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
  inherited Create('!ffi');

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

{ plugin }
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
  FHeader := Trim(Header);
  FAddress := Address;

  if (FHeader[Length(FHeader)] <> ';') then
    FHeader := FHeader + ';';

  if FHeader.EndsWith('native;', True) then
  begin
    SetLength(FHeader, Length(FHeader) - Length('native;'));

    FHeader := Trim(FHeader);
    if (FHeader[Length(FHeader)] <> ';') then
      FHeader := FHeader + ';';

    FNative := True;
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

procedure TMPluginDelayedCode.Import(Compiler: TLapeCompiler);
begin
  Compiler.addDelayedCode(FCode, FName);
end;

procedure TMPluginDelayedCode.Dump(var Str: String);
begin
  Str := Str + FCode + LineEnding;
end;

constructor TMPluginDelayedCode.Create(Code, Name: String);
begin
  FCode := Code;
  FName := Name;
end;

procedure TMPlugin.Load;
var
  MemoryManager: TMemoryManager;
  Name, Str, Header: PChar;
  Index: Int32;
  Address: Pointer;
begin
  WriteLn('Loading plugin "' + ExtractFileNameOnly(FFilePath) + '"');

  if (Pointer(SetPluginMemManager) <> nil) then
  begin
    GetMemoryManager(MemoryManager);

    SetPluginMemManager(MemoryManager);
  end;

  if (Pointer(OnAttach) <> nil) then
    OnAttach(@FData);

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

  if (Pointer(GetDelayedCodeLength) <> nil) and (Pointer(GetDelayedCode) <> nil) then
  begin
    Str := StrAlloc(GetDelayedCodeLength());

    try
      GetDelayedCode(Str);

      FDeclarations.Add(TMPluginDelayedCode.Create(Str, ExtractFileNameOnly(FFilePath)));
    finally
      StrDispose(Str);
    end;
  end;
end;

constructor TMPlugin.Create(AFilePath: String);
begin
  FFilePath := AFilePath;
  FDeclarations := TMPluginDeclarations.Create();
  FLib := LoadLibrary(FFilePath);

  with FData do
  begin
    FreeMem := @_FreeMem;
    GetMem := @_GetMem;
  end;

  if (FLib > 0) then
  begin
    Pointer(GetFunctionInfo) := GetProcedureAddress(FLib, 'GetFunctionInfo');
    Pointer(GetFunctionCount) := GetProcedureAddress(FLib, 'GetFunctionCount');
    Pointer(GetTypeInfo) := GetProcedureAddress(FLib, 'GetTypeInfo');
    Pointer(GetTypeCount) := GetProcedureAddress(FLib, 'GetTypeCount');
    Pointer(GetDelayedCode) := GetProcedureAddress(FLib, 'GetDelayedCode');
    Pointer(GetDelayedCodeLength) := GetProcedureAddress(FLib, 'GetDelayedCodeLength');
    Pointer(SetPluginMemManager) := GetProcedureAddress(FLib, 'SetPluginMemManager');
    Pointer(OnAttach) := GetProcedureAddress(FLib, 'OnAttach');
    Pointer(OnDetach) := GetProcedureAddress(FLib, 'OnDetach');
  end;
end;

destructor TMPlugin.Destroy;
begin
  WriteLn('Freeing plugin "', ExtractFileNameOnly(FFilePath), '"');

  try
    if (Pointer(OnDetach) <> nil) then
      OnDetach();
  except
    on e: Exception do
      WriteLn('ERROR on detaching plugin "', ExtractFileNameOnly(FFilePath), '"', e.Message);
  end;

  if (FLib > 0) then
    FreeLibrary(FLib);

  if (FDeclarations <> nil) then
    FDeclarations.Free();

  inherited Destroy();
end;

function TMPlugins.FindFile(Sender, Argument: String): String;

  function Find(Path: String; var Plugin: String): Boolean;
  begin
    Path := IncludeTrailingPathDelimiter(Path) + Argument;

    if FileExists(Path) then
      Plugin := Path
    else
    if FileExists(Path + '.' + SharedSuffix) then
      Plugin := Path + '.' + SharedSuffix
    else
    if FileExists(Path + {$IFDEF CPU64}'64'{$ELSE}'32'{$ENDIF} + '.' + SharedSuffix) then
      Plugin := Path + {$IFDEF CPU64}'64'{$ELSE}'32'{$ENDIF} + '.' + SharedSuffix;

    Result := Plugin <> '';
  end;

var
  i: Int32;
begin
  Result := '';

  if (not Find(Sender, Result)) then
  begin
    for i := 0 to FPaths.Count - 1 do
      if Find(FPaths[i], Result) then
        Break;
  end;
end;

function TMPlugins.Get(Sender, Argument: String): TMPlugin;
var
  i: Int32;
  Path: String;
begin
  Result := nil;

  Path := FindFile(ExtractFileDir(Sender), Argument);
  if (Path = '') then
    raise Exception.Create('ERROR: Plugin "' + ExtractFileNameOnly(Argument) + '" not found');

  {$IFDEF WINDOWS}
   if (not CorrectArchitecture(Path)) then
      raise Exception.Create('ERROR: Plugin "' + ExtractFileNameOnly(Argument) + '" architecture mismatch');
  {$ENDIF}

  // Already loaded?
  for i := 0 to FPlugins.Count - 1 do
    if (Path = FPlugins[i].FilePath) then
      Exit(FPlugins[i]);

  Result := TMPlugin.Create(Path);

  try
    Result.Load();
  except
    on e: Exception do
    begin
      Result.Free();
      Result := nil;

      raise Exception.Create('ERROR: Exception "' + e.Message + '" while loading plugin "' + ExtractFileNameOnly(Argument) + '"');
    end;
  end;

  if (Result <> nil) then
    FPlugins.Add(Result);
end;

constructor TMPlugins.Create;
begin
  FPlugins := TMPluginsList.Create();

  FPaths := TStringList.Create();
  FPaths.Add(GetCurrentDir());
end;

destructor TMPlugins.Destroy;
begin
  (* commented until functionlist is fixed.
  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Free();
  *)
  FPlugins.Free();
  FPaths.Free();

  inherited Destroy();
end;

initialization
  Plugins := TMPlugins.Create();

end.

