{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Compress images fast using SynLZ.
}
unit simba.image_fastcompress;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.image, simba.simplelock;

procedure SimbaImage_FastCompress(Images: TSimbaImageArray; var Data: Pointer; out DataSize: SizeUInt);
function SimbaImage_FastDeCompress(Data: Pointer; DataLen: SizeUInt): TSimbaImageArray;

type
  TImageCompressThread = class;
  TImageCompressedEvent = procedure(Sender: TImageCompressThread; Images: TSimbaImageArray; Data: Pointer; DataSize: SizeUInt) of object;

  TImageCompressThread = class(TThread)
  protected
    FOnCompressed: TImageCompressedEvent;
    FEvent: TSimpleWaitableLock;
    FImages: TSimbaImageArray;
    FTimeUsed: Double;

    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Push(Images: TSimbaImageArray);
    procedure WaitCompressing;
    function IsCompressing: Boolean;

    property TimeUsed: Double read FTimeUsed;
    property OnCompressed: TImageCompressedEvent read FOnCompressed write FOnCompressed;
  end;

  PImageCompressThread = ^TImageCompressThread;

implementation

uses
  SynLZ,
  simba.datetime;

type
  PImageHeader = ^TImageHeader;
  TImageHeader = packed record
    Size: SizeUInt; // compressed size in bytes
    Width, Height: Integer;
  end;

procedure SimbaImage_FastCompress(Images: TSimbaImageArray; var Data: Pointer; out DataSize: SizeUInt);
var
  TotalSize: SizeUInt;
  I: Integer;
  Ptr: PByte;
begin
  DataSize := 0;
  TotalSize := SizeOf(Integer) + (Length(Images) * SizeOf(TImageHeader));
  for I := 0 to High(Images) do
    Inc(TotalSize, Images[I].DataSize);

  if (Data = nil) or (MemSize(Data) < SynLZcompressdestlen(TotalSize)) then
    ReAllocMem(Data, SynLZcompressdestlen(TotalSize));

  Ptr := Data;
  PInteger(Ptr)^ := Length(Images);
  Inc(Ptr, SizeOf(Integer) + (Length(Images) * SizeOf(TImageHeader)));

  for I := 0 to High(Images) do
    with PImageHeader(Data + SizeOf(Integer) + (I * SizeOf(TImageHeader)))^ do
    begin
      Width := Images[I].Width;
      Height := Images[I].Height;
      Size := SynLZcompress(PByte(Images[I].Data), Images[I].DataSize, Ptr);
      Inc(Ptr, Size);
      Inc(DataSize, Size);
    end;
end;

function SimbaImage_FastDeCompress(Data: Pointer; DataLen: SizeUInt): TSimbaImageArray;
var
  Ptr: PByte;
  I: Integer;
begin
  Ptr := Data;
  SetLength(Result, PInteger(Ptr)^);
  Inc(Ptr, SizeOf(Integer) + (Length(Result) * SizeOf(TImageHeader)));

  for I := 0 to High(Result) do
    with PImageHeader(Data + SizeOf(Integer) + (I * SizeOf(TImageHeader)))^ do
    begin
      Result[I] := TSimbaImage.Create(Width, Height);
      SynLZdecompress(Ptr, Size, PByte(Result[I].Data));
      Inc(Ptr, Size);
    end;
end;

constructor TImageCompressThread.Create;
begin
  inherited Create(False, 512*512);

  FEvent.Lock();
end;

destructor TImageCompressThread.Destroy;
begin
  Terminate();
  FEvent.Lock();
  WaitFor();

  inherited Destroy();
end;

procedure TImageCompressThread.Execute;
var
  Data: Pointer;
  DataSize: SizeUInt;
  T: Double;
begin
  Data := nil;

  while not Terminated do
  begin
    FEvent.WaitLocked(); // unlocked = we have images to compress
    if Terminated then
      Break;

    T := HighResolutionTime();
    SimbaImage_FastCompress(FImages, Data, DataSize);
    FTimeUsed := HighResolutionTime() - T;

    if Assigned(FOnCompressed) then
      FOnCompressed(Self, FImages, Data, DataSize);

    FEvent.Lock(); // lock, and wait for unlock again
  end;

  FreeMem(Data);
end;

procedure TImageCompressThread.Push(Images: TSimbaImageArray);
begin
  FImages := Images;

  FEvent.Unlock();
end;

procedure TImageCompressThread.WaitCompressing;
begin
  while not FEvent.IsLocked() do
    Sleep(20);
end;

function TImageCompressThread.IsCompressing: Boolean;
begin
  Result := not FEvent.IsLocked();
end;

end.

