{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Think a zip file, but (massively) optimized for reading speed rather than compression
}
unit simba.resource;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.baseclass,
  simba.image, simba.container_dict;

const
  ResourceSignature = UInt32($53455253); // "SRES"
  EntrySignature    = UInt32($4E455253); // "SREN"

  CompressMethod_None  = UInt8(0);
  CompressMethod_SynLZ = UInt8(1);

type
  PResourceHeader = ^TResourceHeader;
  TResourceHeader = packed record
    Signature: UInt32;            // signature
    Version: UInt32;              // version for any future changes
    Count: UInt32;                // entry count
    HeadersDataSize: UInt32;      // entry headers data size
    HeadersCompressed: Boolean;   // if headers are compressed
    CompressedDataOffset: UInt32; // offset to the start of the compressed data
  end;

  PEntryHeader = ^TEntryHeader;
  TEntryHeader = packed record
    Signature: UInt32;        // signature
    CompressMethod: Byte;     // 0 = no compression, 1 = SynLZ
    UncompressedSize: UInt32; // uncompressed data size
    CompressedSize: UInt32;   // compressed data size
    DataHash: UInt32;         // crc32 of uncompressed data
    DataOffset: UInt32;       // offset to data compressed data
    NameSize: UInt32;         // name size in chars, the characters follow this
    // char[NameSize]
  end;

type
  PSimbaResourceWriter = ^TSimbaResourceWriter;
  TSimbaResourceWriter = class(TSimbaBaseClass)
  protected
    FCompressHeaders: Boolean;
    FCount: Integer;
    FEntryHeaders: Pointer;
    FEntryHeadersSize: Integer;

    FCompressed: Pointer;
    FCompressedSize: Integer;

    FBuiltSize: Integer;

    procedure Build;
  public
    constructor Create;
    destructor Destroy; override;

    property CompressHeaders: Boolean read FCompressHeaders write FCompressHeaders;

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
      Data: Pointer;
      DataSize: UInt32;
      PartialData: Pointer; // partial being data that can be uncompresed at the start without needing full uncompression (e.g. to read some metadata Header)
      PartialDataSize: UInt32;
    end;
  protected
    FStream: TFileStream;
    FCount: Integer;
    FCompressedDataOffset: Integer;
    FEntryLookup: specialize TDictionary<String, Integer>; // use a dict for fast lookup of string to index

    FEntries: array of TLoadedEntry;

    function DoLoadEntry(Index: Integer; Partial: Integer = 0): Boolean;

    function LoadEntryPartial(Index: Integer; Size: Integer): Boolean;
    function LoadEntry(Index: Integer): Boolean;

    function GetNames: TStringArray;
    function GetName(Index: Integer): String;
    function GetHash(Index: Integer): UInt32;
    function GetCompressedSize(Index: Integer): UInt32;
    function GetUncompressedSize(Index: Integer): UInt32;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;

    property Names: TStringArray read GetNames;
    property Count: Integer read FCount;

    property Name[Index: Integer]: String read GetName;
    property Hash[Index: Integer]: UInt32 read GetHash;
    property CompressedSize[Index: Integer]: UInt32 read GetCompressedSize;
    property UncompressedSize[Index: Integer]: UInt32 read GetUncompressedSize;

    function Find(AName: String): Integer;

    function Load(Index: Integer; out Data: PByte; out DataSize: Integer): Boolean;
    function LoadPartial(Index: Integer; Size: Integer; out Data: PByte): Boolean;
    function LoadString(Index: Integer): String; overload;
    function LoadString(AName: String): String; overload;
    function LoadImage(Index: Integer): TSimbaImage; overload;
    function LoadImage(AName: String): TSimbaImage; overload;

    function Save(Index: Integer; FileName: String): Boolean; overload;
    function Save(AName: String; FileName: String): Boolean; overload;

    procedure UnloadData;
  end;

implementation

uses
  mormot2_synlz, crc,
  simba.fs;

procedure TSimbaResourceWriter.Build;
var
  CompressedHeaders: PByte;
begin
  if FCompressHeaders then
  begin
    CompressedHeaders := GetMem(SynLZcompressdestlen(FEntryHeadersSize));
    FEntryHeadersSize := SynLZcompress(FEntryHeaders, FEntryHeadersSize, CompressedHeaders);
    FreeMem(FEntryHeaders);
    FEntryHeaders := CompressedHeaders;
  end;

  FBuiltSize := FCompressedSize + FEntryHeadersSize + SizeOf(TResourceHeader);

  // Get enough space for everything
  ReAllocMem(FCompressed, FBuiltSize);
  // move compressed data down to make space for ResourceHeader + EntryHeaders
  Move(FCompressed^, PByte(FCompressed)[FEntryHeadersSize + SizeOf(TResourceHeader)], FCompressedSize);
  // move headers in
  Move(FEntryHeaders^, PByte(FCompressed)[SizeOf(TResourceHeader)], FEntryHeadersSize);

  // set resource Header
  PResourceHeader(FCompressed)^.Signature := ResourceSignature;
  PResourceHeader(FCompressed)^.Version := 1;
  PResourceHeader(FCompressed)^.Count := FCount;
  PResourceHeader(FCompressed)^.HeadersDataSize := FEntryHeadersSize;
  PResourceHeader(FCompressed)^.HeadersCompressed := FCompressHeaders;
  PResourceHeader(FCompressed)^.CompressedDataOffset := FEntryHeadersSize + SizeOf(TResourceHeader);
end;

constructor TSimbaResourceWriter.Create;
begin
  inherited Create();

  FEntryHeaders    := GetMem(256 * 256);
  FCompressed      := GetMem(4 * (1024 * 1024));
  FCompressHeaders := True;
end;

destructor TSimbaResourceWriter.Destroy;
begin
  FreeMem(FEntryHeaders);
  FreeMem(FCompressed);

  inherited Destroy();
end;

procedure TSimbaResourceWriter.Add(AName: String; Data: PByte; DataSize: Integer);

  procedure addHeader(size: Integer; crc: UInt32);
  var
    Header: PEntryHeader;
    Needed: Integer;
  begin
    Needed := FEntryHeadersSize + SizeOf(TEntryHeader) + Length(AName);
    if (Needed >= MemSize(FEntryHeaders)) then
      ReAllocMem(FEntryHeaders, Needed * 2);

    Header := PEntryHeader(FEntryHeaders + FEntryHeadersSize);
    Header^.Signature := EntrySignature;
    Header^.CompressMethod := CompressMethod_SynLZ;
    Header^.UncompressedSize := DataSize;
    Header^.DataOffset := FCompressedSize;
    Header^.CompressedSize := size;
    Header^.DataHash := crc;
    Header^.NameSize := Length(AName);
    if Header^.NameSize > 0 then
      Move(AName[1], PByte(Pointer(Header) + SizeOf(TEntryHeader))^, Length(AName));

    Inc(FEntryHeadersSize, SizeOf(TEntryHeader) + Header^.NameSize);
  end;

var
  tmp: Pointer;
  tmpSize: Integer;
begin
  Inc(FCount);

  tmp := GetMem(SynLZcompressdestlen(DataSize));
  tmpSize := SynLZcompress(Data, DataSize, tmp);
  addHeader(tmpSize, crc32(crc.crc32(0, nil, 0), tmp, tmpSize));

  if (FCompressedSize + tmpSize > MemSize(FCompressed)) then
    ReAllocMem(FCompressed, (FCompressedSize + tmpSize) * 2);
  Move(tmp^, PByte(FCompressed)[FCompressedSize], tmpSize);
  Inc(FCompressedSize, tmpSize);

  FreeMem(tmp);
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
  Size := SizeOf(Integer)*2 + ((Image.Width * Image.Height) * SizeOf(TColorBGRA));
  Data := GetMem(Size);

  Move(Image.Width,  Data[0], SizeOf(Integer));
  Move(Image.Height, Data[SizeOf(Integer)], SizeOf(Integer));
  Move(Image.Data^,  Data[SizeOf(Integer)*2], (Image.Width * Image.Height) * SizeOf(TColorBGRA));

  Add(AName, Data, Size);
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
begin
  Build();

  with TFileStream.Create(FileName, fmCreate) do
  try
    Write(FCompressed^, FBuiltSize);
  finally
    Free();
  end;
end;

function TSimbaResourceReader.DoLoadEntry(Index: Integer; Partial: Integer): Boolean;

  procedure NoCompression(var Entry: TLoadedEntry);
  begin
    if (Partial > 0) then
    begin
      Entry.PartialData := GetMem(Partial);
      Entry.PartialDataSize := Partial;

      FStream.Seek(FCompressedDataOffset + Entry.Header.DataOffset, soFromBeginning);
      FStream.Read(Entry.PartialData^, Partial);
    end else
    begin
      Entry.Data := GetMem(Entry.Header.CompressedSize);
      Entry.DataSize := Entry.Header.UncompressedSize;

      FStream.Seek(FCompressedDataOffset + Entry.Header.DataOffset, soFromBeginning);
      FStream.Read(Entry.Data^, Entry.Header.CompressedSize);
    end;
  end;

  procedure SynLZCompression(var Entry: TLoadedEntry);
  var
    Buffer: PByte;
  begin
    Buffer := GetMem(Entry.Header.CompressedSize);
    FStream.Seek(FCompressedDataOffset + Entry.Header.DataOffset, soFromBeginning);
    FStream.Read(Buffer^, Entry.Header.CompressedSize);

    if (Partial > 0) then
    begin
      Entry.PartialData := GetMem(Partial);
      Entry.PartialDataSize := SynLZdecompress1partial(Buffer, Entry.Header.CompressedSize, Entry.PartialData, Partial);
    end else
    begin
      Entry.Data := GetMem(SynLZdecompressdestlen(Buffer));
      Entry.DataSize := SynLZdecompress(Buffer, Entry.Header.CompressedSize, Entry.Data);
    end;

    FreeMem(Buffer);
  end;

begin
  Result := (Index >= 0) and (Index < FCount);

  if Result and ((Partial = 0) and (FEntries[Index].Data = nil)) or ((Partial > 0) and (FEntries[Index].PartialData = nil)) then
    case FEntries[Index].Header.CompressMethod of
      CompressMethod_None:  NoCompression(FEntries[Index]);
      CompressMethod_SynLZ: SynLZCompression(FEntries[Index]);
      else
        SimbaException('Invalid compresion mode: %d. Corrupt resource?', [FEntries[Index].Header.CompressMethod]);
    end;
end;

function TSimbaResourceReader.LoadEntryPartial(Index: Integer; Size: Integer): Boolean;
begin
  Result := DoLoadEntry(Index, Size);
end;

function TSimbaResourceReader.LoadEntry(Index: Integer): Boolean;
begin
  Result := DoLoadEntry(Index);
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
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Entry %d is out of range %d..%d', [Index, 0, FCount]);
  Result := FEntries[Index].Name;
end;

function TSimbaResourceReader.GetHash(Index: Integer): UInt32;
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Entry %d is out of range %d..%d', [Index, 0, FCount]);
  Result := FEntries[Index].Header.DataHash;
end;

function TSimbaResourceReader.GetCompressedSize(Index: Integer): UInt32;
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Entry %d is out of range %d..%d', [Index, 0, FCount]);
  Result := FEntries[Index].header.CompressedSize;
end;

function TSimbaResourceReader.GetUncompressedSize(Index: Integer): UInt32;
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Entry %d is out of range %d..%d', [Index, 0, FCount]);
  Result := FEntries[Index].Header.UncompressedSize;
end;

constructor TSimbaResourceReader.Create(FileName: String);
var
  I: Integer;
  ResourceHeader: TResourceHeader;
  EntryHeadersCompressed, EntryHeadersDecompressed: PByte;
  Ptr: PByte;
begin
  inherited Create();

  FEntryLookup := specialize TDictionary<String, Integer>.Create();

  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  FStream.Read(ResourceHeader, SizeOf(TResourceHeader));

  if (ResourceHeader.Signature <> ResourceSignature) then
    SimbaException('Invalid resource file (missing signature)');

  FCount := ResourceHeader.Count;
  FCompressedDataOffset := ResourceHeader.CompressedDataOffset;
  SetLength(FEntries, FCount);

  if ResourceHeader.HeadersCompressed then
  begin
    EntryHeadersCompressed := GetMem(ResourceHeader.HeadersDataSize);
    FStream.Read(EntryHeadersCompressed^, ResourceHeader.HeadersDataSize);
    EntryHeadersDecompressed := GetMem(SynLZdecompressdestlen(EntryHeadersCompressed));
    ResourceHeader.HeadersDataSize := SynLZdecompress(EntryHeadersCompressed, ResourceHeader.HeadersDataSize, EntryHeadersDecompressed);

    Ptr := EntryHeadersDecompressed;
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

    FreeMem(EntryHeadersCompressed);
    FreeMem(EntryHeadersDecompressed);
  end else
  begin
    for I := 0 to FCount - 1 do
    begin
      FStream.Read(FEntries[I].Header, SizeOf(TEntryHeader));
      if (FEntries[I].Header.Signature <> EntrySignature) then
        SimbaException('Invalid entry signature for index: %d', [I]);

      if (FEntries[I].Header.NameSize > 0) then
      begin
        SetLength(FEntries[I].Name, FEntries[I].Header.NameSize);
        FStream.Read(FEntries[I].Name[1], FEntries[I].Header.NameSize);
      end;
    end;
  end;

  for I := 0 to FCount - 1 do
    FEntryLookup.AddFast(FEntries[I].Name, I);
end;

destructor TSimbaResourceReader.Destroy;
begin
  inherited Destroy();

  UnloadData();

  if (FStream <> nil) then
    FreeAndNil(FStream);
  if (FEntryLookup <> nil) then
    FreeAndNil(FEntryLookup);
end;

function TSimbaResourceReader.Find(AName: String): Integer;
begin
  Result := FEntryLookup.GetDef(AName, -1);
end;

function TSimbaResourceReader.LoadImage(Index: Integer): TSimbaImage;
begin
  Result := nil;

  if LoadEntry(Index) then
    Result := TSimbaImage.CreateFromData(
      PInteger(FEntries[Index].Data)^,
      PInteger(FEntries[Index].Data + SizeOf(Integer))^,
      PColorBGRA(FEntries[Index].Data + (SizeOf(Integer) * 2)),
      PInteger(FEntries[Index].Data)^
    );
end;

function TSimbaResourceReader.LoadImage(AName: String): TSimbaImage;
begin
  Result := LoadImage(Find(AName));
end;

function TSimbaResourceReader.LoadPartial(Index: Integer; Size: Integer; out Data: PByte): Boolean;
begin
  Result := LoadEntryPartial(Index, Size);
  if Result then
    Data := FEntries[Index].PartialData;
end;

function TSimbaResourceReader.Save(Index: Integer; FileName: String): Boolean;
var
  Stream: TFileStream;
begin
  Result := LoadEntry(Index);
  if Result then
  begin
    Stream := TFileStream.Create(FileName, fmCreate);
    Stream.Write(FEntries[Index].Data^, FEntries[Index].DataSize);
  end;
end;

function TSimbaResourceReader.Save(AName: String; FileName: String): Boolean;
begin
  Result := Save(Find(AName), FileName);
end;

function TSimbaResourceReader.Load(Index: Integer; out Data: PByte; out DataSize: Integer): Boolean;
begin
  Result := LoadEntry(Index);

  if Result then
  begin
    Data := FEntries[Index].Data;
    DataSize := FEntries[Index].DataSize;
  end;
end;

function TSimbaResourceReader.LoadString(Index: Integer): String;
begin
  Result := '';

  if LoadEntry(Index) then
  begin
    SetLength(Result, FEntries[Index].DataSize);
    Move(FEntries[Index].Data^, Result[1], Length(Result));
  end;
end;

function TSimbaResourceReader.LoadString(AName: String): String;
begin
  Result := LoadString(Find(AName));
end;

procedure TSimbaResourceReader.UnloadData;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if (FEntries[I].PartialData <> nil) then
      FreeMemAndNil(FEntries[I].PartialData);
    if (FEntries[I].Data <> nil) then
      FreeMemAndNil(FEntries[I].Data);
  end;
end;

end.

