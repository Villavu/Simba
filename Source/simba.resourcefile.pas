{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Like a zip file but (massively) optimized for reading speed rather than compression.
  This means the compression ratio isn't focused on.
}
unit simba.resourcefile;

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

  CompressMethod_None         = UInt8(0);
  CompressMethod_SynLZ        = UInt8(1);
  CompressMethod_RleSynLZ     = UInt8(2);
  CompressMethod_ShufSynLZ    = UInt8(3);
  CompressMethod_ShufRleSynLZ = UInt8(4);

type
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
    CompressMethod: UInt8;    // CompressMethod_XXX
    UncompressedSize: UInt32; // uncompressed data size
    CompressedSize: UInt32;   // compressed data size
    DataHash: UInt32;         // crc32 of uncompressed data
    DataOffset: UInt32;       // offset to data compressed data
    MetaOffset: UInt32;       // TODO: metadata stuff but just reserve it for future
    MetaSize: UInt32;         // ...
    NameSize: UInt32;         // name size in chars, the characters follow this
    // char[NameSize]
  end;

type
  PSimbaResourceWriter = ^TSimbaResourceWriter;
  TSimbaResourceWriter = class(TSimbaBaseClass)
  protected
    FCount: Int32;
    FHeaders: TMemoryStream;
    FData: TMemoryStream;

    procedure Build(Dest: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AName: String; Data: PByte; DataSize: Int32; IsImage: Boolean = False);
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
    TEntryLookupMap = specialize TDictionary<String, Int32>;
  protected
    FStream: TFileStream;
    FCount: Int32;
    FCompressedDataOffset: Int32;
    FEntryLookup: TEntryLookupMap; // use a dict for fast lookup of string to index
    FEntries: array of TLoadedEntry;

    procedure CheckIndex(Index: Int32);
    function LoadEntry(Index: Int32): Boolean;

    function GetNames: TStringArray;
    function GetName(Index: Int32): String;
    function GetHash(Index: Int32): UInt32;
    function GetCompressAlgo(Index: Int32): UInt8;
    function GetCompressedSize(Index: Int32): UInt32;
    function GetUncompressedSize(Index: Int32): UInt32;
  public
    constructor Create(FileName: String);
    destructor Destroy; override;

    property Names: TStringArray read GetNames;
    property Count: Int32 read FCount;

    property Name[Index: Int32]: String read GetName;
    property Hash[Index: Int32]: UInt32 read GetHash;
    property CompressAlgo[Index: Int32]: UInt8 read GetCompressAlgo;
    property CompressedSize[Index: Int32]: UInt32 read GetCompressedSize;
    property UncompressedSize[Index: Int32]: UInt32 read GetUncompressedSize;

    function Find(AName: String): Int32;

    function Load(Index: Int32): TByteArray; overload;
    function Load(AName: String): TByteArray; overload;
    function LoadString(Index: Int32): String; overload;
    function LoadString(AName: String): String; overload;
    function LoadImage(Index: Int32): TSimbaImage; overload;
    function LoadImage(AName: String): TSimbaImage; overload;

    function Save(Index: Int32; FileName: String): Boolean; overload;
    function Save(AName: String; FileName: String): Boolean; overload;

    procedure Unload;
  end;

implementation

uses
  simba.compress_synlz,
  simba.crc,
  simba.fs,
  simba.vartype_ordarray;

procedure Preprocess(var Data: TByteArray; IsImage: Boolean; out Method: UInt8);

  function RunLength(const Src: TByteArray): TByteArray;
  const
    RLE_ESCAPE = $5A;
  var
    Dst, DstBeg, DstEnd: PByte;
    I: PtrUInt;
    Value, Count: Int32;

    function Emit(V, N: Int32): Boolean;
    begin
      Result := False;
      if (N > 3) or (V = RLE_ESCAPE) then
      begin
        while (N > 255) do
        begin
          if (Dst + 3 > DstEnd) then
            Exit;
          Dst[0] := RLE_ESCAPE;
          Dst[1] := 255;
          Dst[2] := V;
          Inc(Dst, 3);
          Dec(N, 255);
        end;
        if (Dst + 3 > DstEnd) then
          Exit;
        Dst[0] := RLE_ESCAPE;
        Dst[1] := N;
        Dst[2] := V;
        Inc(Dst, 3);
      end else
      begin
        if (Dst + N > DstEnd) then
          Exit;
        while (N > 0) do
        begin
          Dst^ := V;
          Inc(Dst);
          Dec(N);
        end;
      end;
      Result := True;
    end;

  begin
    Result := nil;
    SetLength(Result, Length(Src) - (Length(Src) shr 3));
    if (Length(Result) = 0) then
      Exit(nil);

    DstBeg := @Result[0];
    Dst := DstBeg;
    DstEnd := DstBeg + Length(Result);

    Value := Src[0];
    Count := 0;
    for I := 0 to High(Src) do
      if (Src[I] = Value) then
        Inc(Count)
      else
      begin
        if not Emit(Value, Count) then
          Exit(nil);
        Value := Src[I];
        Count := 1;
      end;
    if not Emit(Value, Count) then
      Exit(nil);

    SetLength(Result, Dst - DstBeg);
  end;

  // B0 G0 R0 A0, B1 G1 R1 A1 -> B0 B1, G0 G1, R0 R1, A0 A1 ... etc
  function SplitPlanes(const Src: TByteArray): TByteArray;
  var
    Groups, I: PtrUInt;
    Channel: Int32;
    P, Dst: PByte;
  begin
    Result := nil;
    SetLength(Result, Length(Src));
    if (Length(Src) = 0) then
      Exit;

    Groups := PtrUInt(Length(Src)) div 4;
    Dst := @Result[0];

    for Channel := 0 to 3 do
    begin
      P := @Src[0] + Channel;
      for I := 1 to Groups do
      begin
        Dst^ := P^;
        Inc(Dst);
        Inc(P, 4);
      end;
    end;

    Move((@Src[0] + Groups * 4)^, Dst^, PtrUInt(Length(Src)) - Groups * 4);
  end;

var
  Raw, Candidate, Planes: TByteArray;
begin
  Raw := Data;
  Method := CompressMethod_SynLZ;

  Candidate := RunLength(Raw);
  if (Candidate <> nil) and (Length(Candidate) < Length(Data)) then
  begin
    Data := Candidate;
    Method := CompressMethod_RleSynLZ;
  end;

  if IsImage and (Length(Raw) > 0) then
  begin
    Planes := SplitPlanes(Raw);
    Candidate := RunLength(Planes);

    if (Candidate <> nil) and (Length(Candidate) < Length(Data)) then
    begin
      Data := Candidate;
      Method := CompressMethod_ShufRleSynLZ;
    end
    else if (Method = CompressMethod_SynLZ) then
    begin
      Data := Planes;
      Method := CompressMethod_ShufSynLZ;
    end;
  end;
end;

procedure UnPreprocess(var Data: TByteArray; Method: UInt8; UncompressedSize: Int32);

  function RunLengthUndo(const Src: TByteArray): TByteArray;
  const
    RLE_ESCAPE = $5A;
  var
    S, SrcEnd, Dst, DstEnd: PByte;
    Count: Int32;
  begin
    Result := nil;
    SetLength(Result, UncompressedSize);
    if (UncompressedSize = 0) or (Length(Src) = 0) then
      Exit;

    S := @Src[0];
    SrcEnd := S + Length(Src);
    Dst := @Result[0];
    DstEnd := Dst + UncompressedSize;

    while (S < SrcEnd) do
      if (S^ <> RLE_ESCAPE) then
      begin
        if (Dst >= DstEnd) then
          Break;
        Dst^ := S^;
        Inc(Dst);
        Inc(S);
      end else
      begin
        if (S + 3 > SrcEnd) then
          Break;
        Count := S[1];
        if (Dst + Count > DstEnd) then
          Break;
        FillChar(Dst^, Count, S[2]);
        Inc(Dst, Count);
        Inc(S, 3);
      end;
  end;

  function JoinPlanes(const Src: TByteArray): TByteArray;
  var
    Groups, I: PtrUInt;
    Channel: Int32;
    P, S: PByte;
  begin
    Result := nil;
    SetLength(Result, Length(Src));
    if (Length(Src) = 0) then
      Exit;

    Groups := PtrUInt(Length(Src)) div 4;
    S := @Src[0];

    for Channel := 0 to 3 do
    begin
      P := @Result[0] + Channel;
      for I := 1 to Groups do
      begin
        P^ := S^;
        Inc(S);
        Inc(P, 4);
      end;
    end;

    Move(S^, (@Result[0] + Groups * 4)^, PtrUInt(Length(Src)) - Groups * 4);
  end;

begin
  case Method of
    CompressMethod_None: { nothing };
    CompressMethod_SynLZ: { nothing };
    CompressMethod_RleSynLZ:     Data := RunLengthUndo(Data);
    CompressMethod_ShufSynLZ:    Data := JoinPlanes(Data);
    CompressMethod_ShufRleSynLZ: Data := JoinPlanes(RunLengthUndo(Data));
  else
    SimbaException('Invalid compresion mode: %d. Corrupt resource?', [Method]);
  end;
end;

procedure TSimbaResourceWriter.Build(Dest: TStream);
var
  CompressedHeaders: TByteArray;
  MainHeader: TResourceHeader;
begin
  Dest.Position := 0;

  CompressedHeaders := SynLZ.Compress(FHeaders.Memory, FHeaders.Position);

  // set main header
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

procedure TSimbaResourceWriter.Add(AName: String; Data: PByte; DataSize: Int32; IsImage: Boolean);

  procedure addHeader(ACompressedData: PByte; ACompressedDataSize: Int32; ACompressMethod: Int32);
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
  Work, Compressed: TByteArray;
  Method: UInt8;
begin
  Inc(FCount);

  SetLength(Work, DataSize);
  if (DataSize > 0) then
    Move(Data^, Work[0], DataSize);

  Preprocess(Work, IsImage, Method);
  Compressed := SynLZ.Compress(PByte(Pointer(Work)), Length(Work));

  // If compresion didnt do much, dont bother.
  if (Length(Compressed) >= Round(DataSize * 0.95)) then
    addHeader(Data, DataSize, CompressMethod_None)
  else
    addHeader(@Compressed[0], Length(Compressed), Method);
end;

procedure TSimbaResourceWriter.AddString(AName: String; Str: String);
begin
  Add(AName, Pointer(Str), Length(Str));
end;

procedure TSimbaResourceWriter.AddImage(AName: String; Image: TSimbaImage);
var
  Size: PtrUInt;
  Data: PByte;
begin
  // Width, Height then pixel data.
  // TODO: impl metadata stuff and store width/height there
  Size := SizeOf(Int32)*2 + ((Image.Width * Image.Height) * SizeOf(TColorBGRA));
  Data := GetMem(Size);
  try
    Move(Image.Width,  Data[0], SizeOf(Int32));
    Move(Image.Height, Data[SizeOf(Int32)], SizeOf(Int32));
    Move(Image.Data^,  Data[SizeOf(Int32)*2], (Image.Width * Image.Height) * SizeOf(TColorBGRA));

    Add(AName, Data, Size, True);
  finally
    FreeMem(Data);
  end;
end;

procedure TSimbaResourceWriter.AddImages(Dir: String; Mask: String;  Recursive: Boolean);
var
  Files: TStringArray;
  I: Int32;
  Image: TSimbaImage;
begin
  Files := TSimbaDir.DirSearch(Dir, Mask, Recursive);

  Image := TSimbaImage.Create();
  try
    for I := 0 to High(Files) do
    begin
      Image.Load(Files[I]);

      AddImage(TSimbaPath.PathExtractName(Files[I]), Image);
    end;
  finally
    Image.Free();
  end;
end;

procedure TSimbaResourceWriter.AddFiles(Dir: String; Mask: String; Recursive: Boolean);
var
  Files: TStringArray;
  I: Int32;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create();
  try
    Files := TSimbaDir.DirSearch(Dir, Mask, Recursive);
    for I := 0 to High(Files) do
    begin
      Stream.LoadFromFile(Files[I]);

      Add(TSimbaPath.PathExtractName(Files[I]), Stream.Memory, Stream.Size);
    end;
  finally
    Stream.Free();
  end;
end;

procedure TSimbaResourceWriter.Save(FileName: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Build(Stream);
  finally
    Stream.Free();
  end;
end;

constructor TSimbaResourceReader.Create(FileName: String);
var
  I: Int32;
  ResourceHeader: TResourceHeader;
  Headers: TByteArray;
  Ptr, PtrEnd: PByte;
begin
  inherited Create();

  FEntryLookup := TEntryLookupMap.Create();

  ResourceHeader := Default(TResourceHeader);

  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  if (FStream.Size < SizeOf(TResourceHeader)) then
    SimbaException('Resource file is only %d bytes. Corrupt resource?', [FStream.Size]);
  FStream.ReadBuffer(ResourceHeader, SizeOf(TResourceHeader));
  if (ResourceHeader.Signature <> ResourceSignature) then
    SimbaException('Invalid resource file (invalid signature)');

  // where the entry data starts. Compared as Int64 so an offset above 2^31 is
  // rejected here rather than turning negative in the Int32 field.
  if (ResourceHeader.CompressedDataOffset > FStream.Size) then
    SimbaException('Resource says its data starts at %d, past the end of a %d byte file. Corrupt resource?', [Int64(ResourceHeader.CompressedDataOffset), FStream.Size]);
  FCompressedDataOffset := ResourceHeader.CompressedDataOffset;

  Headers := SynLZ.Decompress(FStream, ResourceHeader.HeadersDataSize);

  // the count comes off disk and decides both the allocation and how far the
  // walk below runs; every entry needs at least a header, which caps it
  FCount := ResourceHeader.Count;
  if (FCount < 0) or (FCount > Length(Headers) div SizeOf(TEntryHeader)) then
    SimbaException('Resource claims %d entries but only carries headers for %d. Corrupt resource?', [Int64(ResourceHeader.Count), Length(Headers) div SizeOf(TEntryHeader)]);
  SetLength(FEntries, FCount);

  if (FCount > 0) then
  begin
    Ptr := @Headers[0];
    PtrEnd := Ptr + Length(Headers);

    for I := 0 to FCount - 1 do
    begin
      if (Ptr + SizeOf(TEntryHeader) > PtrEnd) then
        SimbaException('Resource header data ends inside entry %d. Corrupt resource?', [I]);
      FEntries[I].Header := PEntryHeader(Ptr)^;
      if (FEntries[I].Header.Signature <> EntrySignature) then
        SimbaException('Invalid entry signature for index: %d', [I]);

      // DataOffset and CompressedSize come off disk and decide how much gets
      // allocated and read, so they have to name bytes the file actually holds
      if (Int64(FEntries[I].Header.DataOffset) + FEntries[I].Header.CompressedSize >
          FStream.Size - FCompressedDataOffset) then
        SimbaException('Resource entry %d wants %d bytes at offset %d, past the end of the file. Corrupt resource?', [I, Int64(FEntries[I].Header.CompressedSize), Int64(FEntries[I].Header.DataOffset)]);

      Inc(Ptr, SizeOf(TEntryHeader));
      if (FEntries[I].Header.NameSize > 0) then
      begin
        // NameSize is off disk too, so it can point past everything we read
        if (FEntries[I].Header.NameSize > PtrUInt(PtrEnd - Ptr)) then
          SimbaException('Resource entry %d has a %d byte name with only %d bytes left. Corrupt resource?', [I, FEntries[I].Header.NameSize, PtrEnd - Ptr]);

        SetLength(FEntries[I].Name, FEntries[I].Header.NameSize);
        Move(Ptr^, FEntries[I].Name[1], FEntries[I].Header.NameSize);
        Inc(Ptr, FEntries[I].Header.NameSize);
      end;
    end;
  end;

  for I := 0 to FCount - 1 do
    FEntryLookup.AddFast(FEntries[I].Name, I);
end;

destructor TSimbaResourceReader.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FEntryLookup);

  inherited Destroy();
end;

procedure TSimbaResourceReader.CheckIndex(Index: Int32);
begin
  if (Index < 0) or (Index >= FCount) then
    SimbaException('Entry %d out of range %d..%d', [Index, 0, FCount-1]);
end;

function TSimbaResourceReader.LoadEntry(Index: Int32): Boolean;
begin
  Result := (Index >= 0) and (Index < FCount);

  if Result and (FEntries[Index].Data = nil) then
    with FEntries[Index] do
    begin
      FStream.Seek(FCompressedDataOffset + Header.DataOffset, soFromBeginning);

      if (Header.CompressMethod = CompressMethod_None) then
      begin
        SetLength(Data, Header.CompressedSize);
        if (Header.CompressedSize > 0) then
          FStream.ReadBuffer(Data[0], Header.CompressedSize);
      end else
        Data := SynLZ.Decompress(FStream, Header.CompressedSize);

      UnPreprocess(Data, Header.CompressMethod, Header.UncompressedSize);

      // the header's own hash, so a corrupt file is an error rather than invalid data
      if (Length(Data) <> Header.UncompressedSize) then
        SimbaException('Resource "%s" unpacked to %d bytes, header says %d. Corrupt resource?', [Name, Length(Data), Header.UncompressedSize]);
      if (Length(Data) > 0) and (CRC32(@Data[0], Length(Data)) <> Header.DataHash) then
        SimbaException('Resource "%s" failed its checksum. Corrupt resource?', [Name]);
    end;
end;

function TSimbaResourceReader.GetNames: TStringArray;
var
  I: Int32;
begin
  SetLength(Result, FCount);
  for I := 0 to High(Result) do
    Result[I] := FEntries[I].Name;
end;

function TSimbaResourceReader.GetName(Index: Int32): String;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Name;
end;

function TSimbaResourceReader.GetHash(Index: Int32): UInt32;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.DataHash;
end;

function TSimbaResourceReader.GetCompressAlgo(Index: Int32): UInt8;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.CompressMethod;
end;

function TSimbaResourceReader.GetCompressedSize(Index: Int32): UInt32;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.CompressedSize;
end;

function TSimbaResourceReader.GetUncompressedSize(Index: Int32): UInt32;
begin
  CheckIndex(Index);
  Result := FEntries[Index].Header.UncompressedSize;
end;

function TSimbaResourceReader.Find(AName: String): Int32;
begin
  Result := FEntryLookup.GetDef(AName, -1);
end;

function TSimbaResourceReader.Load(Index: Int32): TByteArray;
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

function TSimbaResourceReader.LoadString(Index: Int32): String;
begin
  Result := '';
  if LoadEntry(Index) then
    Result := FEntries[Index].Data.ToString();
end;

function TSimbaResourceReader.LoadString(AName: String): String;
begin
  Result := LoadString(Find(AName));
end;

function TSimbaResourceReader.LoadImage(Index: Int32): TSimbaImage;
var
  AWidth, AHeight: Int32;
  Pixels: Int64;
begin
  Result := nil;

  if LoadEntry(Index) then
    with FEntries[Index] do
    begin
      // width, height, pixels is a must
      if (Length(Data) < SizeOf(Int32) * 2) then
        SimbaException('Resource "%s" is %d bytes, too short to be an image', [Name, Length(Data)]);

      AWidth  := PInteger(Data)^;
      AHeight := PInteger(PByte(Data) + SizeOf(Int32))^;
      Pixels  := Int64(Length(Data) - SizeOf(Int32) * 2) div SizeOf(TColorBGRA);

      if (AWidth < 0) or (AHeight < 0) or (Int64(AWidth) * AHeight <> Pixels) then
        SimbaException('Resource "%s" declares a %dx%d image but carries %d pixels', [Name, AWidth, AHeight, Pixels]);

      Result := TSimbaImage.CreateFromData(AWidth, AHeight, PColorBGRA(PByte(Data) + SizeOf(Int32) * 2), AWidth);
    end;
end;

function TSimbaResourceReader.LoadImage(AName: String): TSimbaImage;
begin
  Result := LoadImage(Find(AName));
end;

function TSimbaResourceReader.Save(Index: Int32; FileName: String): Boolean;
var
  Stream: TFileStream;
begin
  Result := LoadEntry(Index);
  if Result then
  begin
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      if (Length(FEntries[Index].Data) > 0) then
        Stream.Write(FEntries[Index].Data[0], Length(FEntries[Index].Data));
    finally
      Stream.Free();
    end;
  end;
end;

function TSimbaResourceReader.Save(AName: String; FileName: String): Boolean;
begin
  Result := Save(Find(AName), FileName);
end;

procedure TSimbaResourceReader.Unload;
var
  I: Int32;
begin
  for I := 0 to High(FEntries) do
    FEntries[I].Data := nil;
end;

end.

