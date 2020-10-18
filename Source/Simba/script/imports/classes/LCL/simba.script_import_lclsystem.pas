unit simba.script_import_lclsystem;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLSystem(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

type
  PComponent = ^TComponent;
  PComponentState = ^TComponentState;
  PComponentName = ^String;
  PPersistent = ^TPersistent;
  PStrings = ^TStrings;
  PStream = ^TStream;
  PHandleStream = ^THandleStream;
  PFileStream = ^TFileStream;
  PCustomMemoryStream = ^TCustomMemoryStream;
  PMemoryStream = ^TMemoryStream;
  PStringStream = ^TStringStream;
  PStringArray = ^TStringArray;
  PObject = ^TObject;
  PHandle = ^THandle;
  PSeekOrigin = ^TSeekOrigin;
  PStringList = ^TStringList;
  PNotifyEvent = ^TNotifyEvent;
  PStringListSortCompare = ^TStringListSortCompare;
  PCollectionItem = ^TCollectionItem;
  PCollection = ^TCollection;

{TPersistent}
//procedure Assign(Source: TPersistent);
procedure Lape_TPersistent_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPersistent(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//function  GetNamePath: string;
procedure Lape_TPersistent_GetNamePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PPersistent(Params^[0])^.GetNamePath();
end;

//constructor Create();
procedure Lape_TPersistent_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPersistent(Params^[0])^ := TPersistent.Create();
end;

//procedure Free();
procedure Lape_TPersistent_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPersistent(Params^[0])^.Free();
end;

procedure Lape_Import_TPersistent(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TPersistent');

    addGlobalFunc('procedure TPersistent.Assign(Source: TPersistent); constref;', @Lape_TPersistent_Assign);
    addGlobalFunc('function TPersistent.GetNamePath(): string; constref;', @Lape_TPersistent_GetNamePath);
    addGlobalFunc('procedure TPersistent.Init(); override;', @Lape_TPersistent_Init);
   // addGlobalFunc('procedure TPersistent.Free(); constref;', @Lape_TPersistent_Free);
  end;
end;

{TStream}
//function Read(var Buffer; Count: Longint): Longint;
procedure Lape_TStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PStream(Params^[0])^.Read(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//function Write(const Buffer; Count: Longint): Longint;
procedure Lape_TStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PStream(Params^[0])^.Write(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//function Seek(Offset: Longint; Origin: Word): Longint;
procedure Lape_TStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PStream(Params^[0])^.Seek(PLongint(Params^[1])^, PWord(Params^[2])^);
end;

//procedure ReadBuffer(var Buffer; Count: Longint);
procedure Lape_TStream_ReadBuffer(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.ReadBuffer(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//procedure WriteBuffer(const Buffer; Count: Longint);
procedure Lape_TStream_WriteBuffer(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteBuffer(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//function CopyFrom(Source: TStream; Count: Int64): Int64;
procedure Lape_TStream_CopyFrom(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PStream(Params^[0])^.CopyFrom(PStream(Params^[1])^, PInt64(Params^[2])^);
end;

//function ReadComponent(Instance: TComponent): TComponent;
procedure Lape_TStream_ReadComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PStream(Params^[0])^.ReadComponent(PComponent(Params^[1])^);
end;

//function ReadComponentRes(Instance: TComponent): TComponent;
procedure Lape_TStream_ReadComponentRes(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PStream(Params^[0])^.ReadComponentRes(PComponent(Params^[1])^);
end;

//procedure WriteComponent(Instance: TComponent);
procedure Lape_TStream_WriteComponent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteComponent(PComponent(Params^[1])^);
end;

//procedure WriteComponentRes(const ResName: string; Instance: TComponent);
procedure Lape_TStream_WriteComponentRes(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteComponentRes(PlpString(Params^[1])^, PComponent(Params^[2])^);
end;

//procedure WriteDescendent(Instance, Ancestor: TComponent);
procedure Lape_TStream_WriteDescendent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteDescendent(PComponent(Params^[1])^, PComponent(Params^[2])^);
end;

//procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
procedure Lape_TStream_WriteDescendentRes(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteDescendentRes(PlpString(Params^[1])^, PComponent(Params^[2])^, PComponent(Params^[3])^);
end;

//procedure WriteResourceHeader(const ResName: string; var FixupInfo: Integer);
procedure Lape_TStream_WriteResourceHeader(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteResourceHeader(PlpString(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure FixupResourceHeader(FixupInfo: Integer);
procedure Lape_TStream_FixupResourceHeader(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.FixupResourceHeader(PInteger(Params^[1])^);
end;

//procedure ReadResHeader;
procedure Lape_TStream_ReadResHeader(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.ReadResHeader();
end;

//function ReadByte : Byte;
procedure Lape_TStream_ReadByte(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByte(Result)^ := PStream(Params^[0])^.ReadByte();
end;

//function ReadDWord : UInt32;
procedure Lape_TStream_ReadDWord(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := PStream(Params^[0])^.ReadDWord();
end;

//function ReadAnsiString : String;
procedure Lape_TStream_ReadAnsiString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PStream(Params^[0])^.ReadAnsiString();
end;

//procedure WriteByte(b : Byte);
procedure Lape_TStream_WriteByte(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteByte(PByte(Params^[1])^);
end;

//procedure WriteWord(w : Word);
procedure Lape_TStream_WriteWord(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteWord(PWord(Params^[1])^);
end;

//procedure WriteDWord(d : Cardinal);
procedure Lape_TStream_WriteDWord(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteDWord(PCardinal(Params^[1])^);
end;

//Procedure WriteAnsiString (const S : String);
procedure Lape_TStream_WriteAnsiString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteAnsiString(PlpString(Params^[1])^);
end;

//Read: property Position: Int64 read Position write Position;
procedure Lape_TStream_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PStream(Params^[0])^.Position;
end;

//Write: property Position: Int64 read Position write Position;
procedure Lape_TStream_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.Position := PInt64(Params^[1])^;
end;

//Read: property Size: Int64 read Size write Size;
procedure Lape_TStream_Size_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PStream(Params^[0])^.Size;
end;

//Write: property Size: Int64 read Size write Size;
procedure Lape_TStream_Size_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.Size := PInt64(Params^[1])^;
end;

//constructor Create();
procedure Lape_TStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^ := TStream.Create();
end;

//procedure Free();
procedure Lape_TStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.Free();
end;

procedure Lape_Import_TStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStream');

    addGlobalFunc('function TStream.Read(var Buffer; Count: Longint): Longint; constref;', @Lape_TStream_Read);
    addGlobalFunc('function TStream.Write(constref Buffer; Count: Longint): Longint; constref;', @Lape_TStream_Write); //test
    addGlobalFunc('function TStream.Seek(Offset: Longint; Origin: Word): Longint; constref;', @Lape_TStream_Seek);
    addGlobalFunc('procedure TStream.ReadBuffer(var Buffer; Count: Longint); constref;', @Lape_TStream_ReadBuffer);
    addGlobalFunc('procedure TStream.WriteBuffer(constref Buffer; Count: Longint); constref;', @Lape_TStream_WriteBuffer);
    addGlobalFunc('function TStream.CopyFrom(Source: TStream; Count: Int64): Int64; constref;', @Lape_TStream_CopyFrom);
    addGlobalFunc('function TStream.ReadComponent(Instance: TComponent): TComponent; constref;', @Lape_TStream_ReadComponent);
    addGlobalFunc('function TStream.ReadComponentRes(Instance: TComponent): TComponent; constref;', @Lape_TStream_ReadComponentRes);
    addGlobalFunc('procedure TStream.WriteComponent(Instance: TComponent); constref;', @Lape_TStream_WriteComponent);
    addGlobalFunc('procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent); constref;', @Lape_TStream_WriteComponentRes);
    addGlobalFunc('procedure TStream.WriteDescendent(Instance, Ancestor: TComponent); constref;', @Lape_TStream_WriteDescendent);
    addGlobalFunc('procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent); constref;', @Lape_TStream_WriteDescendentRes);
    addGlobalFunc('procedure TStream.WriteResourceHeader(const ResName: string; var FixupInfo: Integer); constref;', @Lape_TStream_WriteResourceHeader);
    addGlobalFunc('procedure TStream.FixupResourceHeader(FixupInfo: Integer); constref;', @Lape_TStream_FixupResourceHeader);
    addGlobalFunc('procedure TStream.ReadResHeader(); constref;', @Lape_TStream_ReadResHeader);
    addGlobalFunc('function TStream.ReadByte(): Byte; constref;', @Lape_TStream_ReadByte);
    addGlobalFunc('function TStream.ReadDWord(): UInt32; constref;', @Lape_TStream_ReadDWord);
    addGlobalFunc('function TStream.ReadAnsiString(): String; constref;', @Lape_TStream_ReadAnsiString);
    addGlobalFunc('procedure TStream.WriteByte(b : Byte); constref;', @Lape_TStream_WriteByte);
    addGlobalFunc('procedure TStream.WriteWord(w : Word); constref;', @Lape_TStream_WriteWord);
    addGlobalFunc('procedure TStream.WriteDWord(d : UInt32); constref;', @Lape_TStream_WriteDWord);
    addGlobalFunc('procedure TStream.WriteAnsiString(const S : String); constref;', @Lape_TStream_WriteAnsiString);
    addClassVar('TStream', 'Position', 'Integer', @Lape_TStream_Position_Read, @Lape_TStream_Position_Write);
    addClassVar('TStream', 'Size', 'Integer', @Lape_TStream_Size_Read, @Lape_TStream_Size_Write);
    addGlobalFunc('procedure TStream.Init(); override;', @Lape_TStream_Init);
    //addGlobalFunc('procedure TStream.Free(); constref;', @Lape_TStream_Free);
  end;
end;
{THandleStream}
//constructor Create(AHandle: THandle);
procedure Lape_THandleStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandleStream(Params^[0])^ := THandleStream.Create(PHandle(Params^[1])^);
end;

//function Read(var Buffer: pointer; Count: Longint): Longint;
procedure Lape_THandleStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PHandleStream(Params^[0])^.Read(Ppointer(Params^[1])^, PLongint(Params^[2])^);
end;

//function Write(const Buffer: pointer; Count: Longint): Longint;
procedure Lape_THandleStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PHandleStream(Params^[0])^.Write(Ppointer(Params^[1])^, PLongint(Params^[2])^);
end;

//function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
procedure Lape_THandleStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PHandleStream(Params^[0])^.Seek(PInt64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//Read: property Handle: Handle read Handle;
procedure Lape_THandleStream_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PHandleStream(Params^[0])^.Handle;
end;

//procedure Free();
procedure Lape_THandleStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandleStream(Params^[0])^.Free();
end;

procedure Lape_Import_THandleStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('THandleStream', 'TStream');

    addGlobalFunc('procedure THandleStream.Init(AHandle: THandle);', @Lape_THandleStream_Init);
    //addGlobalFunc('function THandleStream.Read(var Buffer; Count: Longint): Longint; constref;', @Lape_THandleStream_Read);
   // addGlobalFunc('function THandleStream.Write(constref Buffer; Count: Longint): Longint; constref;', @Lape_THandleStream_Write);
    addGlobalFunc('function THandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; constref;', @Lape_THandleStream_Seek);
    addClassVar('THandleStream', 'Handle', 'THandle', @Lape_THandleStream_Handle_Read);
   // addGlobalFunc('procedure THandleStream.Free(); constref;', @Lape_THandleStream_Free);
  end;
end;

{TFileStream}

//constructor Create(const AFileName: string; Mode: Word);
procedure Lape_TFileStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileStream(Params^[0])^ := TFileStream.Create(PlpString(Params^[1])^, PWord(Params^[2])^);
end;

//constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
procedure Lape_TFileStream_InitEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileStream(Params^[0])^ := TFileStream.Create(PlpString(Params^[1])^, PWord(Params^[2])^, PCardinal(Params^[3])^);
end;

//Read: property FileName : String Read Filename;
procedure Lape_TFileStream_FileName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PFileStream(Params^[0])^.FileName;
end;

//procedure Free();
procedure Lape_TFileStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileStream(Params^[0])^.Free();
end;

procedure Lape_Import_TFileStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TFileStream', 'THandleStream');

    addGlobalFunc('procedure TFileStream.Init(const AFileName: string; Mode: Word);', @Lape_TFileStream_Init);
    //addGlobalFunc('procedure TFileStream.Init(const AFileName: string; Mode: Word; Rights: Cardinal); overload;', @Lape_TFileStream_InitEx);
    addClassVar('TFileStream', 'FileName', 'String', @Lape_TFileStream_FileName_Read);
    //addGlobalFunc('procedure TFileStream.Free(); constref;', @Lape_TFileStream_Free);
  end;
end;

{TCustomMemoryStream and TMemoryStream}

//function Read(var Buffer: pointer; Count: LongInt): LongInt;
procedure Lape_TCustomMemoryStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PCustomMemoryStream(Params^[0])^.Read(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

//function Seek(const Offset: Integer; Origin: TSeekOrigin): Int64;
procedure Lape_TCustomMemoryStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PCustomMemoryStream(Params^[0])^.Seek(PInteger(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//procedure SaveToStream(Stream: TStream);
procedure Lape_TCustomMemoryStream_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//procedure SaveToFile(const FileName: string);
procedure Lape_TCustomMemoryStream_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//Read: property Memory: Pointer read Memory;
procedure Lape_TCustomMemoryStream_Memory_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PCustomMemoryStream(Params^[0])^.Memory;
end;

//constructor Create();
procedure Lape_TCustomMemoryStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^ := TCustomMemoryStream.Create();
end;

//procedure Free();
procedure Lape_TCustomMemoryStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomMemoryStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomMemoryStream', 'TStream');

    //addGlobalFunc('function TCustomMemoryStream.Read(var Buffer; Count: LongInt): LongInt; constref;', @Lape_TCustomMemoryStream_Read);
    addGlobalFunc('function TCustomMemoryStream.Seek(const Offset: Integer; Origin: TSeekOrigin): Int64; constref;', @Lape_TCustomMemoryStream_Seek);
    addGlobalFunc('procedure TCustomMemoryStream.SaveToStream(Stream: TStream); constref;', @Lape_TCustomMemoryStream_SaveToStream);
    addGlobalFunc('procedure TCustomMemoryStream.SaveToFile(const FileName: string); constref;', @Lape_TCustomMemoryStream_SaveToFile);
    addClassVar('TCustomMemoryStream', 'Memory', 'Pointer', @Lape_TCustomMemoryStream_Memory_Read);
    addGlobalFunc('procedure TCustomMemoryStream.Init(); override;', @Lape_TCustomMemoryStream_Init);
   // addGlobalFunc('procedure TCustomMemoryStream.Free(); constref;', @Lape_TCustomMemoryStream_Free);
  end;
end;

{TMemoryStream}
//procedure Clear;
procedure Lape_TMemoryStream_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.Clear();
end;

//procedure LoadFromStream(Stream: TStream);
procedure Lape_TMemoryStream_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure LoadFromFile(const FileName: string);
procedure Lape_TMemoryStream_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure SetSize(NewSize: PtrInt);
procedure Lape_TMemoryStream_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.SetSize(PPtrInt(Params^[1])^);
end;

//function Write(const Buffer: pointer; Count: LongInt): LongInt;
procedure Lape_TMemoryStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMemoryStream(Params^[0])^.Write(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

//constructor Create();
procedure Lape_TMemoryStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^ := TMemoryStream.Create();
end;

//procedure Free();
procedure Lape_TMemoryStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.Free();
end;

procedure Lape_Import_TMemoryStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMemoryStream', 'TCustomMemoryStream');

    addGlobalFunc('procedure TMemoryStream.Clear(); constref;', @Lape_TMemoryStream_Clear);
    addGlobalFunc('procedure TMemoryStream.LoadFromStream(Stream: TStream); constref;', @Lape_TMemoryStream_LoadFromStream);
    addGlobalFunc('procedure TMemoryStream.LoadFromFile(const FileName: string); constref;', @Lape_TMemoryStream_LoadFromFile);
    addGlobalFunc('procedure TMemoryStream.SetSize(NewSize: PtrInt); constref;', @Lape_TMemoryStream_SetSize);
   // addGlobalFunc('function TMemoryStream.Write(constref Buffer; Count: LongInt): LongInt; constref;', @Lape_TMemoryStream_Write);
    addGlobalFunc('procedure TMemoryStream.Init(); override;', @Lape_TMemoryStream_Init);
   // addGlobalFunc('procedure TMemoryStream.Free(); constref;', @Lape_TMemoryStream_Free);
  end;
end;

{TStringStream}
//constructor Create(const AString: string);
procedure Lape_TStringStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringStream(Params^[0])^ := TStringStream.Create(PlpString(Params^[1])^);
end;

//function Read(var Buffer: longint; Count: Longint): Longint;
procedure Lape_TStringStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PStringStream(Params^[0])^.Read(Plongint(Params^[1])^, PLongint(Params^[2])^);
end;

//function ReadString(Count: Longint): string;
procedure Lape_TStringStream_ReadString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PStringStream(Params^[0])^.ReadString(PLongint(Params^[1])^);
end;

//function Seek(Offset: Longint; Origin: Word): Longint;
procedure Lape_TStringStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PStringStream(Params^[0])^.Seek(PLongint(Params^[1])^, PWord(Params^[2])^);
end;

//function Write(const Buffer: LongInt; Count: Longint): Longint;
procedure Lape_TStringStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongint(Result)^ := PStringStream(Params^[0])^.Write(PLongInt(Params^[1])^, PLongint(Params^[2])^);
end;

//procedure WriteString(const AString: string);
procedure Lape_TStringStream_WriteString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringStream(Params^[0])^.WriteString(PlpString(Params^[1])^);
end;

//Read: property DataString: string read FDataString;
procedure Lape_TStringStream_DataString_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PStringStream(Params^[0])^.DataString;
end;

//procedure Free();
procedure Lape_TStringStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringStream(Params^[0])^.Free();
end;

procedure Lape_Import_TStringStream(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStringStream', 'TStream');

    addGlobalFunc('procedure TStringStream.Init(const AString: string);', @Lape_TStringStream_Init);
   // addGlobalFunc('function TStringStream.Read(var Buffer; Count: Longint): Longint; constref;', @Lape_TStringStream_Read);
    addGlobalFunc('function TStringStream.ReadString(Count: Longint): string; constref;', @Lape_TStringStream_ReadString);
    //addGlobalFunc('function TStringStream.Seek(Offset: Longint; Origin: Word): Longint; constref;', @Lape_TStringStream_Seek);
   // addGlobalFunc('function TStringStream.Write(constref Buffer; Count: Longint): Longint; constref;', @Lape_TStringStream_Write);
    addGlobalFunc('procedure TStringStream.WriteString(const AString: string); constref;', @Lape_TStringStream_WriteString);
    addClassVar('TStringStream', 'DataString', 'string', @Lape_TStringStream_DataString_Read);
    //addGlobalFunc('procedure TStringStream.Free(); constref;', @Lape_TStringStream_Free);
  end;
end;

{TStrings}
//function Add(const S: string): Integer;
procedure Lape_TStrings_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Add(PlpString(Params^[1])^);
end;

//function AddObject(const S: string; AObject: TObject): Integer;
procedure Lape_TStrings_AddObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.AddObject(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure Append(const S: string);
procedure Lape_TStrings_Append(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Append(PlpString(Params^[1])^);
end;

//procedure AddStrings(const TheStrings: TStrings);
procedure Lape_TStrings_AddStrings(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.AddStrings(PStrings(Params^[1])^);
end;

//procedure Assign(Source: TPersistent);
procedure Lape_TStrings_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//procedure BeginUpdate;
procedure Lape_TStrings_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.BeginUpdate();
end;

//procedure Clear;
procedure Lape_TStrings_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Clear();
end;

//procedure Delete(Index: Integer);
procedure Lape_TStrings_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//procedure EndUpdate;
procedure Lape_TStrings_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.EndUpdate();
end;

//function EqualsObj(Obj: TObject): Boolean;
procedure Lape_TStrings_EqualsObj(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStrings(Params^[0])^.Equals(PObject(Params^[1])^);
end;

//function EqualsStrings(TheStrings: TStrings): Boolean;
procedure Lape_TStrings_EqualsStrings(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStrings(Params^[0])^.Equals(PStrings(Params^[1])^);
end;

//procedure Exchange(Index1, Index2: Integer);
procedure Lape_TStrings_Exchange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function IndexOf(const S: string): Integer;
procedure Lape_TStrings_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOf(PlpString(Params^[1])^);
end;

//function IndexOfName(const Name: string): Integer;
procedure Lape_TStrings_IndexOfName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfName(PlpString(Params^[1])^);
end;

//function IndexOfObject(AObject: TObject): Integer;
procedure Lape_TStrings_IndexOfObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfObject(PObject(Params^[1])^);
end;

//procedure Insert(Index: Integer; const S: string);
procedure Lape_TStrings_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Insert(PInteger(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure InsertObject(Index: Integer; const S: string;
procedure Lape_TStrings_InsertObject(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.InsertObject(PInteger(Params^[1])^, PlpString(Params^[2])^,PObject(Params^[3])^);
end;

//procedure LoadFromFile(const FileName: string);
procedure Lape_TStrings_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure LoadFromStream(Stream: TStream);
procedure Lape_TStrings_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure Move(CurIndex, NewIndex: Integer);
procedure Lape_TStrings_Move(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Move(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SaveToFile(const FileName: string);
procedure Lape_TStrings_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//procedure SaveToStream(Stream: TStream);
procedure Lape_TStrings_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//Read: property Count: Integer read Count;
procedure Lape_TStrings_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Count;
end;

//Read: property Objects: TObject read Objects write Objects;
procedure Lape_TStrings_Objects_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Result)^ := PStrings(Params^[0])^.Objects[PInteger(Params^[1])^];
end;

//Write: property Objects: TObject read Objects write Objects;
procedure Lape_TStrings_Objects_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Objects[PInteger(Params^[1])^] := PObject(Params^[2])^;
end;

//Read: property Strings: string read Strings write Strings;
procedure Lape_TStrings_Strings_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PStrings(Params^[0])^.Strings[PInteger(Params^[1])^];
end;

//Write: property Strings: string read Strings write Strings;
procedure Lape_TStrings_Strings_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Strings[PInteger(Params^[1])^] := PlpString(Params^[2])^;
end;

//Read: property Values[string]: string
procedure Lape_TStrings_Values_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PStrings(Params^[0])^.Values[PlpString(Params^[1])^];
end;

//Write: property Values[string]: string
procedure Lape_TStrings_Values_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Values[PlpString(Params^[1])^] := PlpString(Params^[2])^;
end;

//Read: property Text: string read Text write Text;
procedure Lape_TStrings_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PStrings(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure Lape_TStrings_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//constructor Create();
procedure Lape_TStrings_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^ := TStrings.Create();
end;

//procedure Free();
procedure Lape_TStrings_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Free();
end;

procedure Lape_Import_TStrings(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStrings', 'TPersistent');

    addGlobalFunc('function TStrings.Add(const S: string): Integer; constref;', @Lape_TStrings_Add);
    addGlobalFunc('function TStrings.AddObject(const S: string; AObject: TObject): Integer; constref;', @Lape_TStrings_AddObject);
    addGlobalFunc('procedure TStrings.Append(const S: string); constref;', @Lape_TStrings_Append);
    addGlobalFunc('procedure TStrings.AddStrings(const TheStrings: TStrings); constref;', @Lape_TStrings_AddStrings);
   // addGlobalFunc('procedure TStrings.Assign(Source: TPersistent); constref;', @Lape_TStrings_Assign);
    addGlobalFunc('procedure TStrings.BeginUpdate(); constref;', @Lape_TStrings_BeginUpdate);
    addGlobalFunc('procedure TStrings.Clear(); constref;', @Lape_TStrings_Clear);
    addGlobalFunc('procedure TStrings.Delete(Index: Integer); constref;', @Lape_TStrings_Delete);
    addGlobalFunc('procedure TStrings.EndUpdate(); constref;', @Lape_TStrings_EndUpdate);
    addGlobalFunc('function TStrings.EqualsObj(Obj: TObject): Boolean; constref;', @Lape_TStrings_EqualsObj);
    addGlobalFunc('function TStrings.EqualsStrings(TheStrings: TStrings): Boolean; constref;', @Lape_TStrings_EqualsStrings);
    addGlobalFunc('procedure TStrings.Exchange(Index1, Index2: Integer); constref;', @Lape_TStrings_Exchange);
    addGlobalFunc('function TStrings.IndexOf(const S: string): Integer; constref;', @Lape_TStrings_IndexOf);
    addGlobalFunc('function TStrings.IndexOfName(const Name: string): Integer; constref;', @Lape_TStrings_IndexOfName);
    addGlobalFunc('function TStrings.IndexOfObject(AObject: TObject): Integer; constref;', @Lape_TStrings_IndexOfObject);
    addGlobalFunc('procedure TStrings.Insert(Index: Integer; const S: string); constref;', @Lape_TStrings_Insert);
    addGlobalFunc('procedure TStrings.InsertObject(Index: Integer; const S: string;AObject:Tobject); constref;', @Lape_TStrings_InsertObject);
    addGlobalFunc('procedure TStrings.LoadFromFile(const FileName: string); constref;', @Lape_TStrings_LoadFromFile);
    addGlobalFunc('procedure TStrings.LoadFromStream(Stream: TStream); constref;', @Lape_TStrings_LoadFromStream);
    addGlobalFunc('procedure TStrings.Move(CurIndex, NewIndex: Integer); constref;', @Lape_TStrings_Move);
    addGlobalFunc('procedure TStrings.SaveToFile(const FileName: string); constref;', @Lape_TStrings_SaveToFile);
    addGlobalFunc('procedure TStrings.SaveToStream(Stream: TStream); constref;', @Lape_TStrings_SaveToStream);
    addClassVar( 'TStrings', 'Count', 'Integer', @Lape_TStrings_Count_Read);

    addClassVar('TStrings', 'Objects', 'TObject', @Lape_TStrings_Objects_Read, @Lape_TStrings_Objects_Write, True);
    addClassVar('TStrings', 'Values', 'string', @Lape_TStrings_Values_Read, @Lape_TStrings_Values_Write, True, 'string');
    addClassVar('TStrings', 'Strings', 'string', @Lape_TStrings_Strings_Read, @Lape_TStrings_Strings_Write, True);

    addClassVar('TStrings', 'Text', 'string', @Lape_TStrings_Text_Read, @Lape_TStrings_Text_Write);
    addGlobalFunc('procedure TStrings.Init(); override;', @Lape_TStrings_Init);
   // addGlobalFunc('procedure TStrings.Free(); constref;', @Lape_TStrings_Free);
  end;
end;

{TStringList}

//function Add(const S: string): Integer;
procedure Lape_TStringList_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringList(Params^[0])^.Add(PlpString(Params^[1])^);
end;

//procedure Clear;
procedure Lape_TStringList_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Clear();
end;

//procedure Delete(Index: Integer);
procedure Lape_TStringList_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//procedure Exchange(Index1, Index2: Integer);
procedure Lape_TStringList_Exchange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function Find(const S: string; Out Index: Integer): Boolean;
procedure Lape_TStringList_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Find(PlpString(Params^[1])^, PInteger(Params^[2])^);
end;

//function IndexOf(const S: string): Integer;
procedure Lape_TStringList_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringList(Params^[0])^.IndexOf(PlpString(Params^[1])^);
end;

//procedure Insert(Index: Integer; const S: string);
procedure Lape_TStringList_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Insert(PInteger(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure Sort;
procedure Lape_TStringList_Sort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Sort();
end;

//procedure CustomSort(CompareFn: TStringListSortCompare);
procedure Lape_TStringList_CustomSort(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.CustomSort(PStringListSortCompare(Params^[1])^);
end;

//Read: property Sorted: Boolean read Sorted write Sorted;
procedure Lape_TStringList_Sorted_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Sorted;
end;

//Write: property Sorted: Boolean read Sorted write Sorted;
procedure Lape_TStringList_Sorted_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

//Read: property CaseSensitive: Boolean read CaseSensitive write CaseSensitive;
procedure Lape_TStringList_CaseSensitive_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.CaseSensitive;
end;

//Write: property CaseSensitive: Boolean read CaseSensitive write CaseSensitive;
procedure Lape_TStringList_CaseSensitive_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.CaseSensitive := PBoolean(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure Lape_TStringList_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure Lape_TStringList_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnChanging: TNotifyEvent read OnChanging write OnChanging;
procedure Lape_TStringList_OnChanging_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChanging;
end;

//Write: property OnChanging: TNotifyEvent read OnChanging write OnChanging;
procedure Lape_TStringList_OnChanging_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

//Read: property OwnsObjects : boolean read OwnsObjects write OwnsObjects;
procedure Lape_TStringList_OwnsObjects_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PStringList(Params^[0])^.OwnsObjects;
end;

//Write: property OwnsObjects : boolean read OwnsObjects write OwnsObjects;
procedure Lape_TStringList_OwnsObjects_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.OwnsObjects := Pboolean(Params^[1])^;
end;

//constructor Create();
procedure Lape_TStringList_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^ := TStringList.Create();
end;

//procedure Free();
procedure Lape_TStringList_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Free();
end;

procedure Lape_Import_TStringList(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStringList', 'TStrings');
    addGlobalType('function(List: TStringList; Index1, Index2: Integer): Integer','TStringListSortCompare');
    //addGlobalFunc('function TStringList.Add(const S: string): Integer; constref;', @Lape_TStringList_Add);
    //addGlobalFunc('procedure TStringList.Clear(); constref;', @Lape_TStringList_Clear);
    //addGlobalFunc('procedure TStringList.Delete(Index: Integer); constref;', @Lape_TStringList_Delete);
    //addGlobalFunc('procedure TStringList.Exchange(Index1, Index2: Integer); constref;', @Lape_TStringList_Exchange);
    addGlobalFunc('function TStringList.Find(const S: string; Out Index: Integer): Boolean; constref;', @Lape_TStringList_Find);
    //addGlobalFunc('function TStringList.IndexOf(const S: string): Integer; constref;', @Lape_TStringList_IndexOf);
    //addGlobalFunc('procedure TStringList.Insert(Index: Integer; const S: string); constref;', @Lape_TStringList_Insert);
    addGlobalFunc('procedure TStringList.Sort(); constref;', @Lape_TStringList_Sort);
    addGlobalFunc('procedure TStringList.CustomSort(CompareFn: TStringListSortCompare); constref;', @Lape_TStringList_CustomSort);
    addClassVar('TStringList', 'Sorted', 'Boolean', @Lape_TStringList_Sorted_Read, @Lape_TStringList_Sorted_Write);
    addClassVar('TStringList', 'CaseSensitive', 'Boolean', @Lape_TStringList_CaseSensitive_Read, @Lape_TStringList_CaseSensitive_Write);
    addClassVar('TStringList', 'OnChange', 'TNotifyEvent', @Lape_TStringList_OnChange_Read, @Lape_TStringList_OnChange_Write);
    addClassVar('TStringList', 'OnChanging', 'TNotifyEvent', @Lape_TStringList_OnChanging_Read, @Lape_TStringList_OnChanging_Write);
    addClassVar('TStringList', 'OwnsObjects', 'boolean', @Lape_TStringList_OwnsObjects_Read, @Lape_TStringList_OwnsObjects_Write);
    addGlobalFunc('procedure TStringList.Init(); override;', @Lape_TStringList_Init);
    //addGlobalFunc('procedure TStringList.Free(); constref;', @Lape_TStringList_Free);
  end;
end;


{TComponent}
//Read: FComponentStyle: TComponentStyle;

procedure Lape_TComponent_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^ := TComponent.Create(PComponent(Params^[1])^);
end;

//procedure DestroyComponents;
procedure Lape_TComponent_DestroyComponents(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.DestroyComponents();
end;

//procedure Destroying;
procedure Lape_TComponent_Destroying(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Destroying();
end;

//function FindComponent(const AName: string): TComponent;
procedure Lape_TComponent_FindComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PComponent(Params^[0])^.FindComponent(PlpString(Params^[1])^);
end;

//procedure InsertComponent(AComponent: TComponent);
procedure Lape_TComponent_InsertComponent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.InsertComponent(PComponent(Params^[1])^);
end;

//procedure RemoveComponent(AComponent: TComponent);
procedure Lape_TComponent_RemoveComponent(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.RemoveComponent(PComponent(Params^[1])^);
end;

//Read: property Components[Index: Integer]: TComponent read GetComponent;
procedure Lape_TComponent_Components(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PComponent(Result)^ := PComponent(Params^[0])^.Components[PInteger(Params^[1])^];
end;

//Read: property ComponentCount: Integer read GetComponentCount;
procedure Lape_TComponent_ComponentCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentCount;
end;

//Read: property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
procedure Lape_TComponent_ComponentIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentIndex;
end;

//Write: property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
procedure Lape_TComponent_ComponentIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.ComponentIndex := PInteger(Params^[1])^;
end;

//Read: property ComponentState: TComponentState read FComponentState;
procedure Lape_TComponent_ComponentState_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponentState(Result)^ := PComponent(Params^[0])^.ComponentState;
end;

//Read: property Owner: TComponent read FOwner;
procedure Lape_TComponent_Owner_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PComponent(Params^[0])^.Owner;
end;


//Read: property Name: TComponentName read FName write SetName stored False;
procedure Lape_TComponent_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponentName(Result)^ := PComponent(Params^[0])^.Name;
end;

//Write: property Name: TComponentName read FName write SetName stored False;
procedure Lape_TComponent_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Name := PComponentName(Params^[1])^;
end;

//Read: property Tag: PtrInt read FTag write FTag default 0;
procedure Lape_TComponent_Tag_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrInt(Result)^ := PComponent(Params^[0])^.Tag;
end;

//Write: property Tag: PtrInt read FTag write FTag default 0;
procedure Lape_TComponent_Tag_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Tag := PPtrInt(Params^[1])^;
end;

//procedure Free();
procedure Lape_TComponent_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Free();
end;

procedure Lape_Import_TComponent(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TComponent', 'TPersistent');

    addGlobalType('(csLoading, csReading, csWriting, csDestroying,csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,csInline, csDesignInstance)','TComponentState');
    addGlobalType('^TComponentState','PComponentState');
    addGlobalType('^string','PComponentName');
    addGlobalFunc('procedure TComponent.Init(AOwner: TComponent);', @Lape_TComponent_Init);
    addGlobalFunc('procedure TComponent.DestroyComponents(); constref;', @Lape_TComponent_DestroyComponents);
    addGlobalFunc('procedure TComponent.Destroying(); constref;', @Lape_TComponent_Destroying);
    addGlobalFunc('function TComponent.FindComponent(const AName: string): TComponent; constref;', @Lape_TComponent_FindComponent);
    addGlobalFunc('procedure TComponent.InsertComponent(AComponent: TComponent); constref;', @Lape_TComponent_InsertComponent);
    addGlobalFunc('procedure TComponent.RemoveComponent(AComponent: TComponent); constref;', @Lape_TComponent_RemoveComponent);
    addGlobalFunc('function TComponent.GetComponent(index: integer): TComponent; constref;', @Lape_TComponent_Components);
    addClassVar('TComponent', 'ComponentCount', 'Integer', @Lape_TComponent_ComponentCount_Read);
    addClassVar('TComponent', 'ComponentIndex', 'Integer', @Lape_TComponent_ComponentIndex_Read, @Lape_TComponent_ComponentIndex_Write);
    addClassVar('TComponent', 'ComponentState', 'TComponentState', @Lape_TComponent_ComponentState_Read);
    addClassVar('TComponent', 'Owner', 'TComponent', @Lape_TComponent_Owner_Read);
    addClassVar('TComponent', 'Name', 'TComponentName', @Lape_TComponent_Name_Read, @Lape_TComponent_Name_Write);
    addClassVar('TComponent', 'Tag', 'Integer', @Lape_TComponent_Tag_Read, @Lape_TComponent_Tag_Write);
    //addGlobalFunc('procedure TComponent.Free(); constref;', @Lape_TComponent_Free);
  end;
end;

procedure Lape_Import_TCollection_Forward(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
    addClass('TCollection', 'TPersistent');
end;

//constructor Create(ACollection: TCollection); virtual;
procedure TCollectionItem_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^ := TCollectionItem.Create(PCollection(Params^[1])^);
end;

//function GetNamePath: string; override;
procedure TCollectionItem_GetNamePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PCollectionItem(Params^[0])^.GetNamePath();
end;

//Read: property Collection: TCollection read FCollection write SetCollection;
procedure TCollectionItem_Collection_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Result)^ := PCollectionItem(Params^[0])^.Collection;
end;

//Write: property Collection: TCollection read FCollection write SetCollection;
procedure TCollectionItem_Collection_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.Collection := PCollection(Params^[1])^;
end;

//Read: property ID: Integer read FID;
procedure TCollectionItem_ID_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCollectionItem(Params^[0])^.ID;
end;

//Read: property Index: Integer read GetIndex write SetIndex;
procedure TCollectionItem_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCollectionItem(Params^[0])^.Index;
end;

//Write: property Index: Integer read GetIndex write SetIndex;
procedure TCollectionItem_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.Index := PInteger(Params^[1])^;
end;

//Read: property DisplayName: string read GetDisplayName write SetDisplayName;
procedure TCollectionItem_DisplayName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCollectionItem(Params^[0])^.DisplayName;
end;

//Write: property DisplayName: string read GetDisplayName write SetDisplayName;
procedure TCollectionItem_DisplayName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.DisplayName := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TCollectionItem_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.Free();
end;

procedure Lape_Import_TCollectionItem(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCollectionItem', 'TPersistent');

    addGlobalFunc('procedure TCollectionItem.Init(ACollection: TCollection);', @TCollectionItem_Init);
    //addGlobalFunc('function TCollectionItem.GetNamePath(): string; constref;', @TCollectionItem_GetNamePath);
    addClassVar('TCollectionItem', 'Collection', 'TCollection', @TCollectionItem_Collection_Read, @TCollectionItem_Collection_Write);
    addClassVar('TCollectionItem', 'ID', 'Integer', @TCollectionItem_ID_Read, nil);
    addClassVar('TCollectionItem', 'Index', 'Integer', @TCollectionItem_Index_Read, @TCollectionItem_Index_Write);
    addClassVar('TCollectionItem', 'DisplayName', 'string', @TCollectionItem_DisplayName_Read, @TCollectionItem_DisplayName_Write);
   // addGlobalFunc('procedure TCollectionItem.Free(); constref;', @TCollectionItem_Free);
  end;
end;

//function Owner: TPersistent;
procedure TCollection_Owner(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPersistent(Result)^ := PCollection(Params^[0])^.Owner();
end;

//function Add: TCollectionItem;
procedure TCollection_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Add();
end;

//procedure Assign(Source: TPersistent); override;
procedure TCollection_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//procedure BeginUpdate; virtual;
procedure TCollection_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.BeginUpdate();
end;

//procedure Clear;
procedure TCollection_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Clear();
end;

//procedure EndUpdate; virtual;
procedure TCollection_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.EndUpdate();
end;

//procedure Delete(Index: Integer);
procedure TCollection_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//function GetNamePath: string; override;
procedure TCollection_GetNamePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLPString(Result)^ := PCollection(Params^[0])^.GetNamePath();
end;

//function Insert(Index: Integer): TCollectionItem;
procedure TCollection_Insert(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Insert(PInteger(Params^[1])^);
end;

//function FindItemID(ID: Integer): TCollectionItem;
procedure TCollection_FindItemID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.FindItemID(PInteger(Params^[1])^);
end;

//procedure Exchange(Const Index1, index2: integer);
procedure TCollection_Exchange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//Read: property Count: Integer read GetCount;
procedure TCollection_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCollection(Params^[0])^.Count;
end;

//Read: property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
procedure TCollection_Items_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Write: property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
procedure TCollection_Items_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Items[PInteger(Params^[1])^] := PCollectionItem(Params^[2])^;
end;

//procedure Free();
procedure TCollection_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Free();
end;

procedure Lape_Import_TCollection(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addGlobalFunc('function TCollection.Owner(): TPersistent; constref;', @TCollection_Owner);
    addGlobalFunc('function TCollection.Add(): TCollectionItem; constref;', @TCollection_Add);
    //addGlobalFunc('procedure TCollection.Assign(Source: TPersistent); constref;', @TCollection_Assign);
    addGlobalFunc('procedure TCollection.BeginUpdate(); constref;', @TCollection_BeginUpdate);
    addGlobalFunc('procedure TCollection.Clear(); constref;', @TCollection_Clear);
    addGlobalFunc('procedure TCollection.EndUpdate(); constref;', @TCollection_EndUpdate);
    addGlobalFunc('procedure TCollection.Delete(Index: Integer); constref;', @TCollection_Delete);
    //addGlobalFunc('function TCollection.GetNamePath(): string; constref; override;', @TCollection_GetNamePath);
    addGlobalFunc('function TCollection.Insert(Index: Integer): TCollectionItem; constref;', @TCollection_Insert);
    addGlobalFunc('function TCollection.FindItemID(ID: Integer): TCollectionItem; constref;', @TCollection_FindItemID);
    addGlobalFunc('procedure TCollection.Exchange(Const Index1, index2: integer); constref;', @TCollection_Exchange);
    addClassVar('TCollection', 'Count', 'Integer', @TCollection_Count_Read, nil);
    addClassVar('TCollection', 'Items', 'TCollectionItem', @TCollection_Items_Index_Read, @TCollection_Items_Index_Write, True);
    //addGlobalFunc('procedure TCollection.Free(); constref;', @TCollection_Free);
  end;
end;

procedure Lape_Import_LCLSystem(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addGlobalType('procedure(Sender: TObject) of object', 'TNotifyEvent', FFI_DEFAULT_ABI);
    addGlobalType('^TNotifyEvent','PNotifyEvent');
    addGlobalType('UInt32','THandle');
    addGlobalType('string','TComponentName');
    addGlobalType('(soBeginning, soCurrent, soEnd)','TSeekOrigin');
    addGlobalType('string', 'TCaption');

    Lape_Import_TPersistent(Compiler);
    Lape_Import_TComponent(Compiler);
    Lape_Import_TStream(Compiler);
    Lape_Import_THandleStream(Compiler);
    Lape_Import_TCustomMemoryStream(Compiler);
    Lape_Import_TMemoryStream(Compiler);
    Lape_Import_TFileStream(Compiler);
    Lape_Import_TStringStream(Compiler);
    Lape_Import_TStrings(Compiler);
    Lape_Import_TStringList(Compiler);
    Lape_Import_TCollection_Forward(Compiler);
    Lape_Import_TCollectionItem(Compiler);
    Lape_Import_TCollection(Compiler);
  end;
end;

end.

