{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Like a zip file but (massively) optimized for reading speed rather than compression.
}
unit simba.resource;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.baseclass,
  simba.image,
  simba.container_dict;

const
  ResourceSignature = UInt32($53455253); // "SRES"
  EntrySignature    = UInt32($4E455253); // "SREN"

  CompressMethod_None     = UInt8(0);
  CompressMethod_SynLZ    = UInt8(1);
  CompressMethod_RleSynLZ = UInt8(2);

type
  PResourceHeader = ^TResourceHeader;
  TResourceHeader = packed record
    Signature: UInt32;             // signature
    Version: UInt32;               // version for any future changes
    Count: UInt32;                 // entry count
    HeadersDataSize: UInt32;       // entry headers data size
    CompressedDataOffset: UInt32;  // offset to the start of the compressed data
  end;

  PEntryHeader = ^TEntryHeader;
  TEntryHeader = packed record
    Signature: UInt32;        // signature
    CompressMethod: UInt8;    // 0 = no compression, 1 = SynLZ, 2 = RleSynLZ
    UncompressedSize: UInt32; // uncompressed data size
    CompressedSize: UInt32;   // compressed data size
    DataHash: UInt32;         // crc32 of uncompressed data
    DataOffset: UInt32;       // offset to data compressed data
    MetaOffset: UInt32;       // todo
    MetaSize: UInt32;         // todo
    NameSize: UInt32;         // name size in chars, the characters follow this
    // char[NameSize]
  end;

type
  PSimbaResourceWriter = ^TSimbaResourceWriter;
  TSimbaResourceWriter = class(TSimbaBaseClass)
  protected
    FCount: Integer;
    FHeaders: TMemoryStream;
    FData: TMemoryStream;

    procedure Build(Dest: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AName: String; Data: PByte; DataSize: Integer);
    procedure AddString(AName: String; Str: String);
    procedure AddImage(AName: String; Image: TSimbaImage);
    procedure AddImages(Dir: String; Mask: String; Recursive: Boolean = False);
    procedure AddFiles(Dir: String; Mask: String; Recursive: Boolean = False);

    procedure Save(FileName: String);
  end;

  PSimbaResourceReader = ^TSimbaResourceReader;
  TSimbaResourceReader = class(TSimbaBaseClass)
  protected
  type
    TLoadedEntry = record
      Header: TEntryHeader;
      Name: String;
      Data: TByteArray;
    end;
    TEntryLookupMap = specialize TDictionary<String, Integer>;
  protected
    FStream: TFileStream;
    FCount: Integer;
    FCompressedDataOffset: Integer;
    FEntryLookup: TEntryLookupMap; // use a dict for fast lookup of string to index
    FEntries: array of TLoadedEntry;

    procedure CheckIndex(Index: Integer);
    function LoadEntry(Index: Integer): Boolean;

    function GetNames: TStringArray;
    function GetName(Index: Integer): String;
    function GetHash(Index: Integer): UInt32;
    function GetCompressAlgo(Index: Integer): UInt8;
    function GetCompressedSize(Index: Integer): UInt32;
    function GetUncompressedSize(Index: Integer): UInt32;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;

    property Names: TStringArray read GetNames;
    property Count: Integer read FCount;

    property Name[Index: Integer]: String read GetName;
    property Hash[Index: Integer]: UInt32 read GetHash;
    property CompressAlgo[Index: Integer]: UInt8 read GetCompressAlgo;
    property CompressedSize[Index: Integer]: UInt32 read GetCompressedSize;
    property UncompressedSize[Index: Integer]: UInt32 read GetUncompressedSize;

    function Find(AName: String): Integer;

    function Load(Index: Integer): TByteArray; overload;
    function Load(AName: String): TByteArray; overload;
    function LoadString(Index: Integer): String; overload;
    function LoadString(AName: String): String; overload;
    function LoadImage(Index: Integer): TSimbaImage; overload;
    function LoadImage(AName: String): TSimbaImage; overload;

    function Save(Index: Integer; FileName: String): Boolean; overload;
    function Save(AName: String; FileName: String): Boolean; overload;

    procedure Unload;
  end;

implementation

uses
  mormot2_synlz,
  mormot2_rle,
  simba.fs,
  simba.hash,
  simba.vartype_ordarray;

procedure TSimbaResourceWriter.Build(Dest: TStream);
var
  CompressedHeaders: TByteArray;
  MainHeader: TResourceHeader;
begin
  Dest.Position := 0;

  CompressedHeaders := SynLZcompressSimple(FHeaders.Memory, FHeaders.Position);

  // set main Header
  MainHeader := Default(TResourceHeader);
  MainHeader.Signature := ResourceSignature;
  MainHeader.Version := 1;
  MainHeader.Count := FCount;
  MainHeader.HeadersDataSize := Length(CompressedHeaders);
  MainHeader.CompressedDataOffset := Length(CompressedHeaders) + SizeOf(TResourceHeader);

  Dest.Write(MainHeader, SizeOf(MainHeader));
  Dest.Write(CompressedHeaders[0], Length(CompressedHeaders));
  Dest.Write(FData.Memory^, FData.Position);
end;

constructor TSimbaResourceWriter.Create;
begin
  inherited Create();

  FHeaders := TMemoryStream.Create();
  FData := TMemoryStream.Create();
end;

destructor TSimbaResourceWriter.Destroy;
begin
  FreeAndNil(FHeaders);
  FreeAndNil(FData);

  inherited Destroy();
end;

procedure TSimbaResourceWriter.Add(AName: String; Data: PByte; DataSize: Integer);

  procedure addHeader(ACompressedData: PByte; ACompressedDataSize: Integer; ACompressMethod: Integer);
  var
    Header: TEntryHeader;
  begin
    Header := Default(TEntryHeader);
    Header.Signature := EntrySignature;
    Header.CompressMethod := ACompressMethod;
    Header.UncompressedSize := DataSize;
    Header.DataOffset := FData.Position;
    Header.CompressedSize := ACompressedDataSize;
    Header.DataHash := CRC32(Data, DataSize);
    Header.NameSize := Length(AName);

    FHeaders.Write(Header, SizeOf(TEntryHeader));
    if (Length(AName) > 0) then
      FHeaders.Write(AName[1], Length(AName));
    FData.Write(ACompressedData^, ACompressedDataSize);
  end;

var
  CompressedData: TByteArray;
  CompressedDataSize: Integer;
  RleData: Pointer;
  RleSize: Integer;
begin
  Inc(FCount);

  // try to reduce at least by 1/8
  RleData := GetMem(DataSize - DataSize shr 3);
  RleSize := RleCompress(Data, RleData, DataSize, DataSize - DataSize shr 3);

  // RLE was not worth it (no 1/8 reduction) -> apply only SynLZ
  if (RleSize < 0) then
  begin
    CompressedData := SynLZcompressSimple(Data, DataSize);
    CompressedDataSize := Length(CompressedData);

    // If compresion didnt do much, dont bother.
    if (CompressedDataSize >= Round(DataSize * 0.95)) then
      addHeader(Data, DataSize, CompressMethod_None)
    else
      addHeader(@CompressedData[0], CompressedDataSize, CompressMethod_SynLZ);
  end else
  // RLE did reduce the size enough -> compress the RLE data
  begin
    CompressedData := SynLZcompressSimple(RleData, RleSize);
    CompressedDataSize := Length(CompressedData);

    addHeader(@CompressedData[0], CompressedDataSize, CompressMethod_RleSynLZ);
  end;

  FreeMem(RleData);
end;

procedure TSimbaResourceWriter.AddString(AName: String; Str: String);
begin
  Add(AName, @Str[1], Length(Str));
end;

procedure TSimbaResourceWriter.AddImage(AName: String; Image: TSimbaImage);
var
  Size: PtrUInt;
  Data: PByte;
begin
  // width,height,pixels
  // todo eventually implement metadata stuff and store width/height there
  Size := SizeOf(Integer)*2 + ((Image.Width * Image.Height) * SizeOf(TColorBGRA));
  Data := GetMem(Size);

  Move(Image.Width,  Data[0], SizeOf(Integer));
  Move(Image.Height, Data[SizeOf(Integer)], SizeOf(Integer));
  Move(Image.Data^,  Data[SizeOf(Integer)*2], (Image.Width * Image.Height) * SizeOf(TColorBGRA));

  Add(AName, Data, Size);
  FreeMem(Data);
end;

procedure TSimbaResourceWriter.AddImages(Dir: String; Mask: String;  Recursive: Boolean);
var
  Files: TStringArray;
  I: Integer;
  Image: TSimbaImage;
begin
  Files := TSimbaDir.DirSearch(Dir, Mask, Recursive);

  Image := TSimbaImage.Create();
  for I := 0 to High(Files) do
  begin
    Image.Load(Files[I]);

    AddImage(TSimbaPath.PathExtractName(Files[I]), Image);
  end;
  Image.Free();
end;

procedure TSimbaResourceWriter.AddFiles(Dir: String; Mask: String; Recursive: Boolean);
var
  Files: TStringArray;
  I: Integer;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();
  Files := TSimbaDir.DirSearch(Dir, Mask, Recursive);
  for I := 0 to High(Files) do
  begin
    Stream.LoadFromFile(Files[I]);

    Add(TSimbaPath.PathExtractName(Files[I]), Stream.Memory, Stream.Size);
  end;
  Stream.Free();
end;

procedure TSimbaResourceWriter.Save(FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  Build(Stream);
  Stream.Free();
end;

constructor TSimbaResourceReader.Create(FileName: String);
var
  I: Integer;
  ResourceHeader: TResourceHeader;
  Headers: TByteArray;
  Ptr: PByte;
begin
  inherited Create();

  FEntryLookup := TEntryLookupMap.Create();

  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  FStream.Read(ResourceHeader, SizeOf(TResourceHeader));
  if (ResourceHeader.Signature <> ResourceSignature) then
    SimbaException('Invalid resource file (invalid signature)');

  FCount := ResourceHeader.Count;
  FCompressedDataOffset := ResourceHeader.CompressedDataOffset;
  SetLength(FEntries, FCount);

  Headers := SynLZdecompressSimple(FStream, ResourceHeader.HeadersDataSize);

  Ptr := @Headers[0];
  for I := 0 to FCount - 1 do
  begin
    FEntries[I].Header := PEntryHeader(Ptr)^;
    if (FEntries[I].Header.Signature <> EntrySignature) then
      SimbaException('Invalid entry signature for index: %d', [I]);

    Inc(Ptr, SizeOf(TEntryHeader));
    if (FEntries[I].Header.NameSize > 0) then
    begin
      SetLength(FEntries[I].Name, FEntries[I].Header.NameSize);
      Move(Ptr^, FEntries[I].Name[1], FEntries[I].Header.NameSize);
      Inc(Ptr, FEntries[I].Header.NameSize);
    end;
  end;

  for I := 0 to FCount - 1 do
    FEntryLookup.AddFast(FEntries[I].Name, I);
end;

destructor TSimbaResourceReader.Destroy;
begin
  inherited Destroy();

  if (FStream <> nil) then
    FreeAndNil(FStream);
  if (FEntryLookup <> nil) then
    FreeAndNil(FEntryLookup);
end;

procedure TSimbaResourceReader.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Entry %d out of range %d..%d', [Index, 0, FCount-1]);
end;

function TSimbaResourceReader.LoadEntry(Index: Integer): Boolean;

  procedure NoCompression(var Entry: TLoadedEntry);
  begin
    SetLength(Entry.Data, Entry.Header.CompressedSize);
    FStream.Seek(FCompressedDataOffset + Entry.Header.DataOffset, soFromBeginning);
    FStream.Read(Entry.Data[0], Entry.Header.CompressedSize);
  end;

  procedure SynLZCompression(var Entry: TLoadedEntry);
  begin
    FStream.Seek(FCompressedDataOffset + Entry.Header.DataOffset, soFromBeginning);
    Entry.Data := SynLZdecompressSimple(FStream, Entry.Header.CompressedSize);
  end;

  procedure RleSynLZCompression(var Entry: TLoadedEntry);
  begin
    FStream.Seek(FCompressedDataOffset + Entry.Header.DataOffset, soFromBeginning);
    Entry.Data := SynLZdecompressSimple(FStream, Entry.Header.CompressedSize);
    Entry.Data := RleUnCompressSimple(Entry.Data, Entry.Header.UncompressedSize);
  end;

begin
  Result := (Index >= 0) and (Index < FCount);

  if Result and (FEntries[Index].Data = nil) then
    case FEntries[Index].Header.CompressMethod of
      CompressMethod_None:     NoCompression(FEntries[Index]);
      CompressMethod_SynLZ:    SynLZCompression(FEntries[Index]);
      CompressMethod_RleSynLZ: RleSynLZCompression(FEntries[Index]);
      else
        SimbaException('Invalid compresion mode: %d. Corrupt resource?', [FEntries[Index].Header.CompressMethod]);
    end;
end;

function TSimbaResourceReader.GetNames: TStringArray;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  for I := 0 to High(Result) do
    Result[I] := FEntries[I].Name;
end;

function TSimbaResourceReader.GetName(Index: Integer): String;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Name;
end;

function TSimbaResourceReader.GetHash(Index: Integer): UInt32;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.DataHash;
end;

function TSimbaResourceReader.GetCompressAlgo(Index: Integer): UInt8;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.CompressMethod;
end;

function TSimbaResourceReader.GetCompressedSize(Index: Integer): UInt32;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.CompressedSize;
end;

function TSimbaResourceReader.GetUncompressedSize(Index: Integer): UInt32;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.UncompressedSize;
end;

function TSimbaResourceReader.Find(AName: String): Integer;
begin
  Result := FEntryLookup.GetDef(AName, -1);
end;

function TSimbaResourceReader.Load(Index: Integer): TByteArray;
begin
  if LoadEntry(Index) then
    Result := FEntries[Index].Data
  else
    Result := [];
end;

function TSimbaResourceReader.Load(AName: String): TByteArray;
begin
  Result := Load(Find(AName));
end;

function TSimbaResourceReader.LoadString(Index: Integer): String;
begin
  Result := '';
  if LoadEntry(Index) then
    Result := FEntries[Index].Data.ToString();
end;

function TSimbaResourceReader.LoadString(AName: String): String;
begin
  Result := LoadString(Find(AName));
end;

function TSimbaResourceReader.LoadImage(Index: Integer): TSimbaImage;
begin
  Result := nil;

  if LoadEntry(Index) then
    Result := TSimbaImage.CreateFromData(
      PInteger(FEntries[Index].Data)^,
      PInteger(@FEntries[Index].Data[SizeOf(Integer)])^,
      PColorBGRA(@FEntries[Index].Data[SizeOf(Integer)*2]),
      PInteger(FEntries[Index].Data)^
    );
end;

function TSimbaResourceReader.LoadImage(AName: String): TSimbaImage;
begin
  Result := LoadImage(Find(AName));
end;

function TSimbaResourceReader.Save(Index: Integer; FileName: String): Boolean;
var
  Stream: TFileStream;
begin
  Result := LoadEntry(Index);
  if Result then
  begin
    Stream := TFileStream.Create(FileName, fmCreate);
    Stream.Write(FEntries[Index].Data[0], Length(FEntries[Index].Data));
  end;
end;

function TSimbaResourceReader.Save(AName: String; FileName: String): Boolean;
begin
  Result := Save(Find(AName), FileName);
end;

procedure TSimbaResourceReader.Unload;
var
  I: Integer;
begin
  for I := 0 to High(FEntries) do
    FEntries[I].Data := nil;;
end;

end.

