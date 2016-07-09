unit lplclsystem;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

type
  PComponent = ^TComponent;
  PComponentState = ^TComponentState;
  PComponentName = ^String;
  PPersistent = ^TPersistent;
  PStrings = ^TStrings;
  PStream = ^TStream;
  PHandleStream=^THandleStream;
  PFileStream=^TFileStream;
  PCustomMemoryStream = ^TCustomMemoryStream;
  PMemoryStream=^TMemoryStream;
  PStringStream=^TStringStream;
  PStringArray = ^TStringArray;
  PObject = ^TObject;
  PHandle = ^THandle;//register in Register Classes
  PSeekOrigin = ^TSeekOrigin;
  PStringList = ^TStringList;
  PNotifyEvent = ^TNotifyEvent;//register in Register Classes
  PStringListSortCompare = ^TStringListSortCompare;//register in Register Classes
  PCollectionItem = ^TCollectionItem;
  PCollection = ^TCollection;

procedure RegisterLCLSystem(Compiler: TLapeCompiler);

implementation

uses
  lpTObject, MufasaTypes;

{TPersistent}
//procedure Assign(Source: TPersistent);
procedure TPersistent_Assign(const Params: PParamArray); lape_extdecl
begin
  PPersistent(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//function  GetNamePath: string;
procedure TPersistent_GetNamePath(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PPersistent(Params^[0])^.GetNamePath();
end;

//constructor Create();
procedure TPersistent_Init(const Params: PParamArray); lape_extdecl
begin
  PPersistent(Params^[0])^ := TPersistent.Create();
end;

//procedure Free();
procedure TPersistent_Free(const Params: PParamArray); lape_extdecl
begin
  PPersistent(Params^[0])^.Free();
end;

procedure Register_TPersistent(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TPersistent');

    addGlobalFunc('procedure TPersistent.Assign(Source: TPersistent);', @TPersistent_Assign);
    addGlobalFunc('function TPersistent.GetNamePath(): string;', @TPersistent_GetNamePath);
    addGlobalFunc('procedure TPersistent.Init();', @TPersistent_Init);
    addGlobalFunc('procedure TPersistent.Free();', @TPersistent_Free);
  end;
end;

{TStream}
//function Read(var Buffer; Count: Longint): Longint;
procedure TStream_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PStream(Params^[0])^.Read(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//function Write(const Buffer; Count: Longint): Longint;
procedure TStream_Write(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PStream(Params^[0])^.Write(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//function Seek(Offset: Longint; Origin: Word): Longint;
procedure TStream_Seek(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PStream(Params^[0])^.Seek(PLongint(Params^[1])^, PWord(Params^[2])^);
end;

//procedure ReadBuffer(var Buffer; Count: Longint);
procedure TStream_ReadBuffer(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.ReadBuffer(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//procedure WriteBuffer(const Buffer; Count: Longint);
procedure TStream_WriteBuffer(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteBuffer(PLongint(Params^[1])^,PLongint(Params^[2])^);
end;

//function CopyFrom(Source: TStream; Count: Int64): Int64;
procedure TStream_CopyFrom(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := PStream(Params^[0])^.CopyFrom(PStream(Params^[1])^, PInt64(Params^[2])^);
end;

//function ReadComponent(Instance: TComponent): TComponent;
procedure TStream_ReadComponent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponent(Result)^ := PStream(Params^[0])^.ReadComponent(PComponent(Params^[1])^);
end;

//function ReadComponentRes(Instance: TComponent): TComponent;
procedure TStream_ReadComponentRes(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponent(Result)^ := PStream(Params^[0])^.ReadComponentRes(PComponent(Params^[1])^);
end;

//procedure WriteComponent(Instance: TComponent);
procedure TStream_WriteComponent(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteComponent(PComponent(Params^[1])^);
end;

//procedure WriteComponentRes(const ResName: string; Instance: TComponent);
procedure TStream_WriteComponentRes(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteComponentRes(PlpString(Params^[1])^, PComponent(Params^[2])^);
end;

//procedure WriteDescendent(Instance, Ancestor: TComponent);
procedure TStream_WriteDescendent(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteDescendent(PComponent(Params^[1])^, PComponent(Params^[2])^);
end;

//procedure WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);
procedure TStream_WriteDescendentRes(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteDescendentRes(PlpString(Params^[1])^, PComponent(Params^[2])^, PComponent(Params^[3])^);
end;

//procedure WriteResourceHeader(const ResName: string; var FixupInfo: Integer);
procedure TStream_WriteResourceHeader(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteResourceHeader(PlpString(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure FixupResourceHeader(FixupInfo: Integer);
procedure TStream_FixupResourceHeader(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.FixupResourceHeader(PInteger(Params^[1])^);
end;

//procedure ReadResHeader;
procedure TStream_ReadResHeader(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.ReadResHeader();
end;

//function ReadByte : Byte;
procedure TStream_ReadByte(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PByte(Result)^ := PStream(Params^[0])^.ReadByte();
end;

//function ReadWord : Word;
procedure TStream_ReadWord(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWord(Result)^ := PStream(Params^[0])^.ReadWord();
end;

//function ReadDWord : Cardinal;
procedure TStream_ReadDWord(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCardinal(Result)^ := PStream(Params^[0])^.ReadDWord();
end;

//function ReadAnsiString : String;
procedure TStream_ReadAnsiString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PStream(Params^[0])^.ReadAnsiString();
end;

//procedure WriteByte(b : Byte);
procedure TStream_WriteByte(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteByte(PByte(Params^[1])^);
end;

//procedure WriteWord(w : Word);
procedure TStream_WriteWord(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteWord(PWord(Params^[1])^);
end;

//procedure WriteDWord(d : Cardinal);
procedure TStream_WriteDWord(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteDWord(PCardinal(Params^[1])^);
end;

//Procedure WriteAnsiString (const S : String);
procedure TStream_WriteAnsiString(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.WriteAnsiString(PlpString(Params^[1])^);
end;

//Read: property Position: Int64 read Position write Position;
procedure TStream_Position_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := PStream(Params^[0])^.Position;
end;

//Write: property Position: Int64 read Position write Position;
procedure TStream_Position_Write(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.Position := PInt64(Params^[1])^;
end;

//Read: property Size: Int64 read Size write Size;
procedure TStream_Size_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := PStream(Params^[0])^.Size;
end;

//Write: property Size: Int64 read Size write Size;
procedure TStream_Size_Write(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.Size := PInt64(Params^[1])^;
end;

//constructor Create();
procedure TStream_Init(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^ := TStream.Create();
end;

//procedure Free();
procedure TStream_Free(const Params: PParamArray); lape_extdecl
begin
  PStream(Params^[0])^.Free();
end;

procedure Register_TStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStream');

    addGlobalFunc('function TStream.Read(var Buffer; Count: Longint): Longint;', @TStream_Read);
    addGlobalFunc('function TStream.Write(constref Buffer; Count: Longint): Longint;', @TStream_Write); //test
    addGlobalFunc('function TStream.Seek(Offset: Longint; Origin: Word): Longint;', @TStream_Seek);
    addGlobalFunc('procedure TStream.ReadBuffer(var Buffer; Count: Longint);', @TStream_ReadBuffer);
    addGlobalFunc('procedure TStream.WriteBuffer(constref Buffer; Count: Longint);', @TStream_WriteBuffer);
    addGlobalFunc('function TStream.CopyFrom(Source: TStream; Count: Int64): Int64;', @TStream_CopyFrom);
    addGlobalFunc('function TStream.ReadComponent(Instance: TComponent): TComponent;', @TStream_ReadComponent);
    addGlobalFunc('function TStream.ReadComponentRes(Instance: TComponent): TComponent;', @TStream_ReadComponentRes);
    addGlobalFunc('procedure TStream.WriteComponent(Instance: TComponent);', @TStream_WriteComponent);
    addGlobalFunc('procedure TStream.WriteComponentRes(const ResName: string; Instance: TComponent);', @TStream_WriteComponentRes);
    addGlobalFunc('procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);', @TStream_WriteDescendent);
    addGlobalFunc('procedure TStream.WriteDescendentRes(const ResName: string; Instance, Ancestor: TComponent);', @TStream_WriteDescendentRes);
    addGlobalFunc('procedure TStream.WriteResourceHeader(const ResName: string; var FixupInfo: Integer);', @TStream_WriteResourceHeader);
    addGlobalFunc('procedure TStream.FixupResourceHeader(FixupInfo: Integer);', @TStream_FixupResourceHeader);
    addGlobalFunc('procedure TStream.ReadResHeader();', @TStream_ReadResHeader);
    addGlobalFunc('function TStream.ReadByte(): Byte;', @TStream_ReadByte);
    addGlobalFunc('function TStream.ReadWord(): Word;', @TStream_ReadWord);
    addGlobalFunc('function TStream.ReadDWord(): Cardinal;', @TStream_ReadDWord);
    addGlobalFunc('function TStream.ReadAnsiString(): String;', @TStream_ReadAnsiString);
    addGlobalFunc('procedure TStream.WriteByte(b : Byte);', @TStream_WriteByte);
    addGlobalFunc('procedure TStream.WriteWord(w : Word);', @TStream_WriteWord);
    addGlobalFunc('procedure TStream.WriteDWord(d : Cardinal);', @TStream_WriteDWord);
    addGlobalFunc('procedure TStream.WriteAnsiString(const S : String);', @TStream_WriteAnsiString);
    addClassVar('TStream', 'Position', 'Integer', @TStream_Position_Read, @TStream_Position_Write);
    addClassVar('TStream', 'Size', 'Integer', @TStream_Size_Read, @TStream_Size_Write);
    addGlobalFunc('procedure TStream.Init();', @TStream_Init);
    addGlobalFunc('procedure TStream.Free();', @TStream_Free);
  end;
end;
{THandleStream}
//constructor Create(AHandle: THandle);
procedure THandleStream_Init(const Params: PParamArray); lape_extdecl
begin
  PHandleStream(Params^[0])^ := THandleStream.Create(PHandle(Params^[1])^);
end;

//function Read(var Buffer: pointer; Count: Longint): Longint;
procedure THandleStream_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PHandleStream(Params^[0])^.Read(Ppointer(Params^[1])^, PLongint(Params^[2])^);
end;

//function Write(const Buffer: pointer; Count: Longint): Longint;
procedure THandleStream_Write(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PHandleStream(Params^[0])^.Write(Ppointer(Params^[1])^, PLongint(Params^[2])^);
end;

//function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
procedure THandleStream_Seek(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := PHandleStream(Params^[0])^.Seek(PInt64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//Read: property Handle: Handle read Handle;
procedure THandleStream_Handle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PHandleStream(Params^[0])^.Handle;
end;

//procedure Free();
procedure THandleStream_Free(const Params: PParamArray); lape_extdecl
begin
  PHandleStream(Params^[0])^.Free();
end;

procedure Register_THandleStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('THandleStream', 'TStream');

    addGlobalFunc('procedure THandleStream.Init(AHandle: THandle);', @THandleStream_Init);
    addGlobalFunc('function THandleStream.Read(var Buffer; Count: Longint): Longint;', @THandleStream_Read);
    addGlobalFunc('function THandleStream.Write(constref Buffer; Count: Longint): Longint;', @THandleStream_Write);
    addGlobalFunc('function THandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;', @THandleStream_Seek);
    addClassVar('THandleStream', 'Handle', 'THandle', @THandleStream_Handle_Read);
    addGlobalFunc('procedure THandleStream.Free();', @THandleStream_Free);
  end;
end;

{TFileStream}

//constructor Create(const AFileName: string; Mode: Word);
procedure TFileStream_Init(const Params: PParamArray); lape_extdecl
begin
  PFileStream(Params^[0])^ := TFileStream.Create(PlpString(Params^[1])^, PWord(Params^[2])^);
end;

//constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
procedure TFileStream_InitEx(const Params: PParamArray); lape_extdecl
begin
  PFileStream(Params^[0])^ := TFileStream.Create(PlpString(Params^[1])^, PWord(Params^[2])^, PCardinal(Params^[3])^);
end;

//Read: property FileName : String Read Filename;
procedure TFileStream_FileName_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PFileStream(Params^[0])^.FileName;
end;

//procedure Free();
procedure TFileStream_Free(const Params: PParamArray); lape_extdecl
begin
  PFileStream(Params^[0])^.Free();
end;

procedure Register_TFileStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TFileStream', 'THandleStream');

    addGlobalFunc('procedure TFileStream.Init(const AFileName: string; Mode: Word);', @TFileStream_Init);
  //  addGlobalFunc('procedure TFileStream.Init(const AFileName: string; Mode: Word; Rights: Cardinal); overload;', @TFileStream_InitEx);
    addClassVar('TFileStream', 'FileName', 'String', @TFileStream_FileName_Read);
    addGlobalFunc('procedure TFileStream.Free();', @TFileStream_Free);
  end;
end;

{TCustomMemoryStream and TMemoryStream}

//function Read(var Buffer: pointer; Count: LongInt): LongInt;
procedure TCustomMemoryStream_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := PCustomMemoryStream(Params^[0])^.Read(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

//function Seek(const Offset: Integer; Origin: TSeekOrigin): Int64;
procedure TCustomMemoryStream_Seek(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInt64(Result)^ := PCustomMemoryStream(Params^[0])^.Seek(PInteger(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

//procedure SaveToStream(Stream: TStream);
procedure TCustomMemoryStream_SaveToStream(const Params: PParamArray); lape_extdecl
begin
  PCustomMemoryStream(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//procedure SaveToFile(const FileName: string);
procedure TCustomMemoryStream_SaveToFile(const Params: PParamArray); lape_extdecl
begin
  PCustomMemoryStream(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//Read: property Memory: Pointer read Memory;
procedure TCustomMemoryStream_Memory_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointer(Result)^ := PCustomMemoryStream(Params^[0])^.Memory;
end;

//constructor Create();
procedure TCustomMemoryStream_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomMemoryStream(Params^[0])^ := TCustomMemoryStream.Create();
end;

//procedure Free();
procedure TCustomMemoryStream_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomMemoryStream(Params^[0])^.Free();
end;

procedure Register_TCustomMemoryStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomMemoryStream', 'TStream');

    addGlobalFunc('function TCustomMemoryStream.Read(var Buffer; Count: LongInt): LongInt;', @TCustomMemoryStream_Read);
    addGlobalFunc('function TCustomMemoryStream.Seek(const Offset: Integer; Origin: TSeekOrigin): Int64;', @TCustomMemoryStream_Seek);
    addGlobalFunc('procedure TCustomMemoryStream.SaveToStream(Stream: TStream);', @TCustomMemoryStream_SaveToStream);
    addGlobalFunc('procedure TCustomMemoryStream.SaveToFile(const FileName: string);', @TCustomMemoryStream_SaveToFile);
    addClassVar('TCustomMemoryStream', 'Memory', 'Pointer', @TCustomMemoryStream_Memory_Read);
    addGlobalFunc('procedure TCustomMemoryStream.Init();', @TCustomMemoryStream_Init);
    addGlobalFunc('procedure TCustomMemoryStream.Free();', @TCustomMemoryStream_Free);
  end;
end;

{TMemoryStream}
//procedure Clear;
procedure TMemoryStream_Clear(const Params: PParamArray); lape_extdecl
begin
  PMemoryStream(Params^[0])^.Clear();
end;

//procedure LoadFromStream(Stream: TStream);
procedure TMemoryStream_LoadFromStream(const Params: PParamArray); lape_extdecl
begin
  PMemoryStream(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure LoadFromFile(const FileName: string);
procedure TMemoryStream_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PMemoryStream(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure SetSize(NewSize: PtrInt);
procedure TMemoryStream_SetSize(const Params: PParamArray); lape_extdecl
begin
  PMemoryStream(Params^[0])^.SetSize(PPtrInt(Params^[1])^);
end;

//function Write(const Buffer: pointer; Count: LongInt): LongInt;
procedure TMemoryStream_Write(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := PMemoryStream(Params^[0])^.Write(PLongInt(Params^[1])^, PLongInt(Params^[2])^);
end;

//constructor Create();
procedure TMemoryStream_Init(const Params: PParamArray); lape_extdecl
begin
  PMemoryStream(Params^[0])^ := TMemoryStream.Create();
end;

//procedure Free();
procedure TMemoryStream_Free(const Params: PParamArray); lape_extdecl
begin
  PMemoryStream(Params^[0])^.Free();
end;

procedure Register_TMemoryStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMemoryStream', 'TCustomMemoryStream');

    addGlobalFunc('procedure TMemoryStream.Clear();', @TMemoryStream_Clear);
    addGlobalFunc('procedure TMemoryStream.LoadFromStream(Stream: TStream);', @TMemoryStream_LoadFromStream);
    addGlobalFunc('procedure TMemoryStream.LoadFromFile(const FileName: string);', @TMemoryStream_LoadFromFile);
    addGlobalFunc('procedure TMemoryStream.SetSize(NewSize: PtrInt);', @TMemoryStream_SetSize);
    addGlobalFunc('function TMemoryStream.Write(constref Buffer; Count: LongInt): LongInt;', @TMemoryStream_Write);
    addGlobalFunc('procedure TMemoryStream.Init();', @TMemoryStream_Init);
    addGlobalFunc('procedure TMemoryStream.Free();', @TMemoryStream_Free);
  end;
end;

{TStringStream}
//constructor Create(const AString: string);
procedure TStringStream_Init(const Params: PParamArray); lape_extdecl
begin
  PStringStream(Params^[0])^ := TStringStream.Create(PlpString(Params^[1])^);
end;

//function Read(var Buffer: longint; Count: Longint): Longint;
procedure TStringStream_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PStringStream(Params^[0])^.Read(Plongint(Params^[1])^, PLongint(Params^[2])^);
end;

//function ReadString(Count: Longint): string;
procedure TStringStream_ReadString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PStringStream(Params^[0])^.ReadString(PLongint(Params^[1])^);
end;

//function Seek(Offset: Longint; Origin: Word): Longint;
procedure TStringStream_Seek(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PStringStream(Params^[0])^.Seek(PLongint(Params^[1])^, PWord(Params^[2])^);
end;

//function Write(const Buffer: LongInt; Count: Longint): Longint;
procedure TStringStream_Write(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PStringStream(Params^[0])^.Write(PLongInt(Params^[1])^, PLongint(Params^[2])^);
end;

//procedure WriteString(const AString: string);
procedure TStringStream_WriteString(const Params: PParamArray); lape_extdecl
begin
  PStringStream(Params^[0])^.WriteString(PlpString(Params^[1])^);
end;

//Read: property DataString: string read FDataString;
procedure TStringStream_DataString_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PStringStream(Params^[0])^.DataString;
end;

//procedure Free();
procedure TStringStream_Free(const Params: PParamArray); lape_extdecl
begin
  PStringStream(Params^[0])^.Free();
end;

procedure Register_TStringStream(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStringStream', 'TStream');

    addGlobalFunc('procedure TStringStream.Init(const AString: string);', @TStringStream_Init);
    addGlobalFunc('function TStringStream.Read(var Buffer; Count: Longint): Longint;', @TStringStream_Read);
    addGlobalFunc('function TStringStream.ReadString(Count: Longint): string;', @TStringStream_ReadString);
    addGlobalFunc('function TStringStream.Seek(Offset: Longint; Origin: Word): Longint;', @TStringStream_Seek);
    addGlobalFunc('function TStringStream.Write(constref Buffer; Count: Longint): Longint;', @TStringStream_Write);
    addGlobalFunc('procedure TStringStream.WriteString(const AString: string);', @TStringStream_WriteString);
    addClassVar('TStringStream', 'DataString', 'string', @TStringStream_DataString_Read);
    addGlobalFunc('procedure TStringStream.Free();', @TStringStream_Free);
  end;
end;

{TStrings}
//function Add(const S: string): Integer;
procedure TStrings_Add(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Add(PlpString(Params^[1])^);
end;

//function AddObject(const S: string; AObject: TObject): Integer;
procedure TStrings_AddObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStrings(Params^[0])^.AddObject(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure Append(const S: string);
procedure TStrings_Append(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Append(PlpString(Params^[1])^);
end;

//procedure AddStrings(const TheStrings: TStrings);
procedure TStrings_AddStrings(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.AddStrings(PStrings(Params^[1])^);
end;

//procedure Assign(Source: TPersistent);
procedure TStrings_Assign(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//procedure BeginUpdate;
procedure TStrings_BeginUpdate(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.BeginUpdate();
end;

//procedure Clear;
procedure TStrings_Clear(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Clear();
end;

//procedure Delete(Index: Integer);
procedure TStrings_Delete(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//procedure EndUpdate;
procedure TStrings_EndUpdate(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.EndUpdate();
end;

//function EqualsObj(Obj: TObject): Boolean;
procedure TStrings_EqualsObj(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStrings(Params^[0])^.Equals(PObject(Params^[1])^);
end;

//function EqualsStrings(TheStrings: TStrings): Boolean;
procedure TStrings_EqualsStrings(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStrings(Params^[0])^.Equals(PStrings(Params^[1])^);
end;

//procedure Exchange(Index1, Index2: Integer);
procedure TStrings_Exchange(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function IndexOf(const S: string): Integer;
procedure TStrings_IndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOf(PlpString(Params^[1])^);
end;

//function IndexOfName(const Name: string): Integer;
procedure TStrings_IndexOfName(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfName(PlpString(Params^[1])^);
end;

//function IndexOfObject(AObject: TObject): Integer;
procedure TStrings_IndexOfObject(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfObject(PObject(Params^[1])^);
end;

//procedure Insert(Index: Integer; const S: string);
procedure TStrings_Insert(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Insert(PInteger(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure InsertObject(Index: Integer; const S: string;
procedure TStrings_InsertObject(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.InsertObject(PInteger(Params^[1])^, PlpString(Params^[2])^,PObject(Params^[3])^);
end;

//procedure LoadFromFile(const FileName: string);
procedure TStrings_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure LoadFromStream(Stream: TStream);
procedure TStrings_LoadFromStream(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure Move(CurIndex, NewIndex: Integer);
procedure TStrings_Move(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Move(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SaveToFile(const FileName: string);
procedure TStrings_SaveToFile(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//procedure SaveToStream(Stream: TStream);
procedure TStrings_SaveToStream(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//Read: property Count: Integer read Count;
procedure TStrings_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Count;
end;

//Read: property Objects: TObject read Objects write Objects;
procedure TStrings_Objects_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PObject(Result)^ := PStrings(Params^[0])^.Objects[PInteger(Params^[1])^];
end;

//Write: property Objects: TObject read Objects write Objects;
procedure TStrings_Objects_Write(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Objects[PInteger(Params^[1])^] := PObject(Params^[2])^;
end;

//Read: property Strings: string read Strings write Strings;
procedure TStrings_Strings_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PStrings(Params^[0])^.Strings[PInteger(Params^[1])^];
end;

//Write: property Strings: string read Strings write Strings;
procedure TStrings_Strings_Write(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Strings[PInteger(Params^[1])^] := PlpString(Params^[2])^;
end;

//Read: property Values[string]: string
procedure TStrings_Values_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PStrings(Params^[0])^.Values[PlpString(Params^[1])^];
end;

//Write: property Values[string]: string
procedure TStrings_Values_Write(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Values[PlpString(Params^[1])^] := PlpString(Params^[2])^;
end;

//Read: property Text: string read Text write Text;
procedure TStrings_Text_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PStrings(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure TStrings_Text_Write(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//constructor Create();
procedure TStrings_Init(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^ := TStrings.Create();
end;

//procedure Free();
procedure TStrings_Free(const Params: PParamArray); lape_extdecl
begin
  PStrings(Params^[0])^.Free();
end;

procedure Register_TStrings(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStrings', 'TPersistent');

    addGlobalFunc('function TStrings.Add(const S: string): Integer;', @TStrings_Add);
    addGlobalFunc('function TStrings.AddObject(const S: string; AObject: TObject): Integer;', @TStrings_AddObject);
    addGlobalFunc('procedure TStrings.Append(const S: string);', @TStrings_Append);
    addGlobalFunc('procedure TStrings.AddStrings(const TheStrings: TStrings);', @TStrings_AddStrings);
    addGlobalFunc('procedure TStrings.Assign(Source: TPersistent);', @TStrings_Assign);
    addGlobalFunc('procedure TStrings.BeginUpdate();', @TStrings_BeginUpdate);
    addGlobalFunc('procedure TStrings.Clear();', @TStrings_Clear);
    addGlobalFunc('procedure TStrings.Delete(Index: Integer);', @TStrings_Delete);
    addGlobalFunc('procedure TStrings.EndUpdate();', @TStrings_EndUpdate);
    addGlobalFunc('function TStrings.EqualsObj(Obj: TObject): Boolean;', @TStrings_EqualsObj);
    addGlobalFunc('function TStrings.EqualsStrings(TheStrings: TStrings): Boolean;', @TStrings_EqualsStrings);
    addGlobalFunc('procedure TStrings.Exchange(Index1, Index2: Integer);', @TStrings_Exchange);
    addGlobalFunc('function TStrings.IndexOf(const S: string): Integer;', @TStrings_IndexOf);
    addGlobalFunc('function TStrings.IndexOfName(const Name: string): Integer;', @TStrings_IndexOfName);
    addGlobalFunc('function TStrings.IndexOfObject(AObject: TObject): Integer;', @TStrings_IndexOfObject);
    addGlobalFunc('procedure TStrings.Insert(Index: Integer; const S: string);', @TStrings_Insert);
    addGlobalFunc('procedure TStrings.InsertObject(Index: Integer; const S: string;AObject:Tobject);', @TStrings_InsertObject);
    addGlobalFunc('procedure TStrings.LoadFromFile(const FileName: string);', @TStrings_LoadFromFile);
    addGlobalFunc('procedure TStrings.LoadFromStream(Stream: TStream);', @TStrings_LoadFromStream);
    addGlobalFunc('procedure TStrings.Move(CurIndex, NewIndex: Integer);', @TStrings_Move);
    addGlobalFunc('procedure TStrings.SaveToFile(const FileName: string);', @TStrings_SaveToFile);
    addGlobalFunc('procedure TStrings.SaveToStream(Stream: TStream);', @TStrings_SaveToStream);
    addClassVar( 'TStrings', 'Count', 'Integer', @TStrings_Count_Read);

    addClassVar('TStrings', 'Objects', 'TObject', @TStrings_Objects_Read, @TStrings_Objects_Write, True);
    addClassVar('TStrings', 'Values', 'string', @TStrings_Values_Read, @TStrings_Values_Write, True, 'string');
    addClassVar('TStrings', 'Strings', 'string', @TStrings_Strings_Read, @TStrings_Strings_Write, True);

    addClassVar('TStrings', 'Text', 'string', @TStrings_Text_Read, @TStrings_Text_Write);
    addGlobalFunc('procedure TStrings.Init();', @TStrings_Init);
    addGlobalFunc('procedure TStrings.Free();', @TStrings_Free);
  end;
end;

{TStringList}

//function Add(const S: string): Integer;
procedure TStringList_Add(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStringList(Params^[0])^.Add(PlpString(Params^[1])^);
end;

//procedure Clear;
procedure TStringList_Clear(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Clear();
end;

//procedure Delete(Index: Integer);
procedure TStringList_Delete(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//procedure Exchange(Index1, Index2: Integer);
procedure TStringList_Exchange(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function Find(const S: string; Out Index: Integer): Boolean;
procedure TStringList_Find(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Find(PlpString(Params^[1])^, PInteger(Params^[2])^);
end;

//function IndexOf(const S: string): Integer;
procedure TStringList_IndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PStringList(Params^[0])^.IndexOf(PlpString(Params^[1])^);
end;

//procedure Insert(Index: Integer; const S: string);
procedure TStringList_Insert(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Insert(PInteger(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure Sort;
procedure TStringList_Sort(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Sort();
end;

//procedure CustomSort(CompareFn: TStringListSortCompare);
procedure TStringList_CustomSort(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.CustomSort(PStringListSortCompare(Params^[1])^);
end;

//Read: property Sorted: Boolean read Sorted write Sorted;
procedure TStringList_Sorted_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Sorted;
end;

//Write: property Sorted: Boolean read Sorted write Sorted;
procedure TStringList_Sorted_Write(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

//Read: property CaseSensitive: Boolean read CaseSensitive write CaseSensitive;
procedure TStringList_CaseSensitive_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.CaseSensitive;
end;

//Write: property CaseSensitive: Boolean read CaseSensitive write CaseSensitive;
procedure TStringList_CaseSensitive_Write(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.CaseSensitive := PBoolean(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TStringList_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TStringList_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnChanging: TNotifyEvent read OnChanging write OnChanging;
procedure TStringList_OnChanging_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChanging;
end;

//Write: property OnChanging: TNotifyEvent read OnChanging write OnChanging;
procedure TStringList_OnChanging_Write(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

//Read: property OwnsObjects : boolean read OwnsObjects write OwnsObjects;
procedure TStringList_OwnsObjects_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PStringList(Params^[0])^.OwnsObjects;
end;

//Write: property OwnsObjects : boolean read OwnsObjects write OwnsObjects;
procedure TStringList_OwnsObjects_Write(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.OwnsObjects := Pboolean(Params^[1])^;
end;

//constructor Create();
procedure TStringList_Init(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^ := TStringList.Create();
end;

//procedure Free();
procedure TStringList_Free(const Params: PParamArray); lape_extdecl
begin
  PStringList(Params^[0])^.Free();
end;

procedure Register_TStringList(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStringList', 'TStrings');
    addGlobalType('function(List: TStringList; Index1, Index2: Integer): Integer','TStringListSortCompare');
    addGlobalFunc('function TStringList.Add(const S: string): Integer;', @TStringList_Add);
    addGlobalFunc('procedure TStringList.Clear();', @TStringList_Clear);
    addGlobalFunc('procedure TStringList.Delete(Index: Integer);', @TStringList_Delete);
    addGlobalFunc('procedure TStringList.Exchange(Index1, Index2: Integer);', @TStringList_Exchange);
    addGlobalFunc('function TStringList.Find(const S: string; Out Index: Integer): Boolean;', @TStringList_Find);
    addGlobalFunc('function TStringList.IndexOf(const S: string): Integer;', @TStringList_IndexOf);
    addGlobalFunc('procedure TStringList.Insert(Index: Integer; const S: string);', @TStringList_Insert);
    addGlobalFunc('procedure TStringList.Sort();', @TStringList_Sort);
    addGlobalFunc('procedure TStringList.CustomSort(CompareFn: TStringListSortCompare);', @TStringList_CustomSort);
    addClassVar('TStringList', 'Sorted', 'Boolean', @TStringList_Sorted_Read, @TStringList_Sorted_Write);
    addClassVar('TStringList', 'CaseSensitive', 'Boolean', @TStringList_CaseSensitive_Read, @TStringList_CaseSensitive_Write);
    addClassVar('TStringList', 'OnChange', 'TNotifyEvent', @TStringList_OnChange_Read, @TStringList_OnChange_Write);
    addClassVar('TStringList', 'OnChanging', 'TNotifyEvent', @TStringList_OnChanging_Read, @TStringList_OnChanging_Write);
    addClassVar('TStringList', 'OwnsObjects', 'boolean', @TStringList_OwnsObjects_Read, @TStringList_OwnsObjects_Write);
    addGlobalFunc('procedure TStringList.Init();', @TStringList_Init);
    addGlobalFunc('procedure TStringList.Free();', @TStringList_Free);
  end;
end;


{TComponent}
//Read: FComponentStyle: TComponentStyle;

procedure TComponent_Init(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^ := TComponent.Create(PComponent(Params^[1])^);
end;

//procedure DestroyComponents;
procedure TComponent_DestroyComponents(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.DestroyComponents();
end;

//procedure Destroying;
procedure TComponent_Destroying(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.Destroying();
end;

//function FindComponent(const AName: string): TComponent;
procedure TComponent_FindComponent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponent(Result)^ := PComponent(Params^[0])^.FindComponent(PlpString(Params^[1])^);
end;

//procedure InsertComponent(AComponent: TComponent);
procedure TComponent_InsertComponent(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.InsertComponent(PComponent(Params^[1])^);
end;

//procedure RemoveComponent(AComponent: TComponent);
procedure TComponent_RemoveComponent(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.RemoveComponent(PComponent(Params^[1])^);
end;

//Read: property Components[Index: Integer]: TComponent read GetComponent;
procedure TComponent_Components(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
 PComponent(Result)^ := PComponent(Params^[0])^.Components[PInteger(Params^[1])^];
end;

//Read: property ComponentCount: Integer read GetComponentCount;
procedure TComponent_ComponentCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentCount;
end;

//Read: property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
procedure TComponent_ComponentIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentIndex;
end;

//Write: property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
procedure TComponent_ComponentIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.ComponentIndex := PInteger(Params^[1])^;
end;

//Read: property ComponentState: TComponentState read FComponentState;
procedure TComponent_ComponentState_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponentState(Result)^ := PComponent(Params^[0])^.ComponentState;
end;

//Read: property Owner: TComponent read FOwner;
procedure TComponent_Owner_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponent(Result)^ := PComponent(Params^[0])^.Owner;
end;


//Read: property Name: TComponentName read FName write SetName stored False;
procedure TComponent_Name_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponentName(Result)^ := PComponent(Params^[0])^.Name;
end;

//Write: property Name: TComponentName read FName write SetName stored False;
procedure TComponent_Name_Write(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.Name := PComponentName(Params^[1])^;
end;

//Read: property Tag: PtrInt read FTag write FTag default 0;
procedure TComponent_Tag_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPtrInt(Result)^ := PComponent(Params^[0])^.Tag;
end;

//Write: property Tag: PtrInt read FTag write FTag default 0;
procedure TComponent_Tag_Write(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.Tag := PPtrInt(Params^[1])^;
end;

//procedure Free();
procedure TComponent_Free(const Params: PParamArray); lape_extdecl
begin
  PComponent(Params^[0])^.Free();
end;

procedure Register_TComponent(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TComponent', 'TPersistent');

    addGlobalType('(csLoading, csReading, csWriting, csDestroying,csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,csInline, csDesignInstance)','TComponentState');
    addGlobalType('^TComponentState','PComponentState');
    addGlobalType('^string','PComponentName');
    addGlobalFunc('procedure TComponent.Init(AOwner: TComponent);', @TComponent_Init);
    addGlobalFunc('procedure TComponent.DestroyComponents();', @TComponent_DestroyComponents);
    addGlobalFunc('procedure TComponent.Destroying();', @TComponent_Destroying);
    addGlobalFunc('function TComponent.FindComponent(const AName: string): TComponent;', @TComponent_FindComponent);
    addGlobalFunc('procedure TComponent.InsertComponent(AComponent: TComponent);', @TComponent_InsertComponent);
    addGlobalFunc('procedure TComponent.RemoveComponent(AComponent: TComponent);', @TComponent_RemoveComponent);
    addGlobalFunc('function TComponent.GetComponent(index: integer): TComponent;', @TComponent_Components);
    addClassVar('TComponent', 'ComponentCount', 'Integer', @TComponent_ComponentCount_Read);
    addClassVar('TComponent', 'ComponentIndex', 'Integer', @TComponent_ComponentIndex_Read, @TComponent_ComponentIndex_Write);
    addClassVar('TComponent', 'ComponentState', 'TComponentState', @TComponent_ComponentState_Read);
    addClassVar('TComponent', 'Owner', 'TComponent', @TComponent_Owner_Read);
    addClassVar('TComponent', 'Name', 'TComponentName', @TComponent_Name_Read, @TComponent_Name_Write);
    addClassVar('TComponent', 'Tag', 'Integer', @TComponent_Tag_Read, @TComponent_Tag_Write);
    addGlobalFunc('procedure TComponent.Free();', @TComponent_Free);
  end;
end;

procedure Register_TCollection_Forward(Compiler: TLapeCompiler);
begin
  with Compiler do
    addClass('TCollection', 'TPersistent');
end;

//constructor Create(ACollection: TCollection); virtual;
procedure TCollectionItem_Init(const Params: PParamArray); lape_extdecl
begin
  PCollectionItem(Params^[0])^ := TCollectionItem.Create(PCollection(Params^[1])^);
end;

//function GetNamePath: string; override;
procedure TCollectionItem_GetNamePath(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PCollectionItem(Params^[0])^.GetNamePath();
end;

//Read: property Collection: TCollection read FCollection write SetCollection;
procedure TCollectionItem_Collection_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCollection(Result)^ := PCollectionItem(Params^[0])^.Collection;
end;

//Write: property Collection: TCollection read FCollection write SetCollection;
procedure TCollectionItem_Collection_Write(const Params: PParamArray); lape_extdecl
begin
  PCollectionItem(Params^[0])^.Collection := PCollection(Params^[1])^;
end;

//Read: property ID: Integer read FID;
procedure TCollectionItem_ID_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCollectionItem(Params^[0])^.ID;
end;

//Read: property Index: Integer read GetIndex write SetIndex;
procedure TCollectionItem_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCollectionItem(Params^[0])^.Index;
end;

//Write: property Index: Integer read GetIndex write SetIndex;
procedure TCollectionItem_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PCollectionItem(Params^[0])^.Index := PInteger(Params^[1])^;
end;

//Read: property DisplayName: string read GetDisplayName write SetDisplayName;
procedure TCollectionItem_DisplayName_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCollectionItem(Params^[0])^.DisplayName;
end;

//Write: property DisplayName: string read GetDisplayName write SetDisplayName;
procedure TCollectionItem_DisplayName_Write(const Params: PParamArray); lape_extdecl
begin
  PCollectionItem(Params^[0])^.DisplayName := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TCollectionItem_Free(const Params: PParamArray); lape_extdecl
begin
  PCollectionItem(Params^[0])^.Free();
end;

procedure Register_TCollectionItem(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCollectionItem', 'TPersistent');

    addGlobalFunc('procedure TCollectionItem.Init(ACollection: TCollection);', @TCollectionItem_Init);
    addGlobalFunc('function TCollectionItem.GetNamePath(): string;', @TCollectionItem_GetNamePath);
    addClassVar('TCollectionItem', 'Collection', 'TCollection', @TCollectionItem_Collection_Read, @TCollectionItem_Collection_Write);
    addClassVar('TCollectionItem', 'ID', 'Integer', @TCollectionItem_ID_Read, nil);
    addClassVar('TCollectionItem', 'Index', 'Integer', @TCollectionItem_Index_Read, @TCollectionItem_Index_Write);
    addClassVar('TCollectionItem', 'DisplayName', 'string', @TCollectionItem_DisplayName_Read, @TCollectionItem_DisplayName_Write);
    addGlobalFunc('procedure TCollectionItem.Free();', @TCollectionItem_Free);
  end;
end;

//function Owner: TPersistent;
procedure TCollection_Owner(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPersistent(Result)^ := PCollection(Params^[0])^.Owner();
end;

//function Add: TCollectionItem;
procedure TCollection_Add(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Add();
end;

//procedure Assign(Source: TPersistent); override;
procedure TCollection_Assign(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

//procedure BeginUpdate; virtual;
procedure TCollection_BeginUpdate(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.BeginUpdate();
end;

//procedure Clear;
procedure TCollection_Clear(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.Clear();
end;

//procedure EndUpdate; virtual;
procedure TCollection_EndUpdate(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.EndUpdate();
end;

//procedure Delete(Index: Integer);
procedure TCollection_Delete(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//function GetNamePath: string; override;
procedure TCollection_GetNamePath(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLPString(Result)^ := PCollection(Params^[0])^.GetNamePath();
end;

//function Insert(Index: Integer): TCollectionItem;
procedure TCollection_Insert(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Insert(PInteger(Params^[1])^);
end;

//function FindItemID(ID: Integer): TCollectionItem;
procedure TCollection_FindItemID(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.FindItemID(PInteger(Params^[1])^);
end;

//procedure Exchange(Const Index1, index2: integer);
procedure TCollection_Exchange(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//Read: property Count: Integer read GetCount;
procedure TCollection_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCollection(Params^[0])^.Count;
end;

//Read: property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
procedure TCollection_Items_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Write: property Items[Index: Integer]: TCollectionItem read GetItem write SetItem;
procedure TCollection_Items_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.Items[PInteger(Params^[1])^] := PCollectionItem(Params^[2])^;
end;

//procedure Free();
procedure TCollection_Free(const Params: PParamArray); lape_extdecl
begin
  PCollection(Params^[0])^.Free();
end;

procedure Register_TCollection(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addGlobalFunc('function TCollection.Owner(): TPersistent;', @TCollection_Owner);
    addGlobalFunc('function TCollection.Add(): TCollectionItem;', @TCollection_Add);
    addGlobalFunc('procedure TCollection.Assign(Source: TPersistent);', @TCollection_Assign);
    addGlobalFunc('procedure TCollection.BeginUpdate();', @TCollection_BeginUpdate);
    addGlobalFunc('procedure TCollection.Clear();', @TCollection_Clear);
    addGlobalFunc('procedure TCollection.EndUpdate();', @TCollection_EndUpdate);
    addGlobalFunc('procedure TCollection.Delete(Index: Integer);', @TCollection_Delete);
    addGlobalFunc('function TCollection.GetNamePath(): string; override;', @TCollection_GetNamePath);
    addGlobalFunc('function TCollection.Insert(Index: Integer): TCollectionItem;', @TCollection_Insert);
    addGlobalFunc('function TCollection.FindItemID(ID: Integer): TCollectionItem;', @TCollection_FindItemID);
    addGlobalFunc('procedure TCollection.Exchange(Const Index1, index2: integer);', @TCollection_Exchange);
    addClassVar('TCollection', 'Count', 'Integer', @TCollection_Count_Read, nil);
    addClassVar('TCollection', 'Items', 'TCollectionItem', @TCollection_Items_Index_Read, @TCollection_Items_Index_Write, True);
    addGlobalFunc('procedure TCollection.Free();', @TCollection_Free);
  end;
end;

{Registration classes procedure}
procedure RegisterLCLSystem(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addNativeGlobalType('procedure(Sender: TObject)', 'TNotifyEvent');
    addGlobalType('^TNotifyEvent','PNotifyEvent');
    addGlobalType('dword','THandle');
    addGlobalType('string','TComponentName');
    addGlobalType('(soBeginning, soCurrent, soEnd)','TSeekOrigin');
    addGlobalType('string', 'TCaption');

    Register_TPersistent(Compiler);
    Register_TComponent(Compiler);
    Register_TStream(Compiler);
    Register_THandleStream(Compiler);
    Register_TCustomMemoryStream(Compiler);
    Register_TMemoryStream(Compiler);
    Register_TFileStream(Compiler);
    Register_TStringStream(Compiler);
    Register_TStrings(Compiler);
    Register_TStringList(Compiler);
    Register_TCollection_Forward(Compiler);
    Register_TCollectionItem(Compiler);
    Register_TCollection(Compiler);
  end;
end;

end.

