unit simba.compressionthread;

{$i simba.inc}

interface

uses
  classes, sysutils, syncobjs, synlz,
  simba.baseclass, simba.simplelock;

type
  PLZCompressedEvent = ^TLZCompressedEvent;
  TLZCompressedEvent = procedure(Sender: TObject; Data: Pointer; DataSize: Integer; TimeUsed: Double) of object;

  PLZCompressionThread = ^TLZCompressionThread;
  TLZCompressionThread = class(TSimbaBaseClass)
  protected
    FThread: TThread;
    FCompressingLock: TSimpleWaitableLock;
    FCompressingEvent: TSimpleEvent;
    FSourceStream: TMemoryStream;
    FOnCompressed: TLZCompressedEvent;

    procedure DoCompressingThread;

    function GetIsCompressing: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Write(const Data; DataSize: Integer);
    procedure WaitCompressing;

    property OnCompressed: TLZCompressedEvent read FOnCompressed write FOnCompressed;
    property IsCompressing: Boolean read GetIsCompressing;
  end;

implementation

uses
  simba.datetime;

type
  TCompressingStream = class(TMemoryStream)
  public
    TimeUsed: Double;

    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

function TCompressingStream.Write(const Buffer; Count: LongInt): LongInt;
var
  NeededCapacity: PtrInt;
begin
  NeededCapacity := SynLZcompressdestlen(Count);
  if (Capacity < NeededCapacity) then
    Capacity := NeededCapacity;

  TimeUsed := HighResolutionTime();
  Result := SynLZcompress1pas(Pointer(Buffer), Count, Self.Memory);
  TimeUsed := HighResolutionTime() - TimeUsed;
end;

function TLZCompressionThread.GetIsCompressing: Boolean;
begin
  Result := FCompressingLock.IsLocked;
end;

procedure TLZCompressionThread.DoCompressingThread;
var
  DestStream: TCompressingStream;
begin
  DestStream := TCompressingStream.Create();

  while (not TThread.CheckTerminated) do
  begin
    FCompressingEvent.WaitFor(INFINITE);
    if TThread.CheckTerminated then
      Break;

    DestStream.Position := 0;
    DestStream.Position := DestStream.Write(FSourceStream.Memory, FSourceStream.Position);
    if (FOnCompressed <> nil) then
      FOnCompressed(Self, DestStream.Memory, DestStream.Position, DestStream.TimeUsed);

    FCompressingEvent.ResetEvent();
    FCompressingLock.Unlock();
  end;

  DestStream.Free();
end;

constructor TLZCompressionThread.Create;
begin
  inherited Create();

  FCompressingEvent := TSimpleEvent.Create();
  FSourceStream := TMemoryStream.Create();

  FThread := TThread.ExecuteInThread(@DoCompressingThread);
end;

destructor TLZCompressionThread.Destroy;
begin
  FCompressingLock.WaitLocked();

  if (FThread <> nil) then
  begin
    FThread.FreeOnTerminate := False;
    FThread.Terminate();

    FCompressingEvent.SetEvent(); // Wake thread

    FThread.WaitFor();
  end;

  FreeAndNil(FCompressingEvent);
  FreeAndNil(FSourceStream);
  FreeAndNil(FThread);

  inherited Destroy();
end;

procedure TLZCompressionThread.Write(const Data; DataSize: Integer);
begin
  FCompressingLock.WaitLocked();
  FCompressingLock.Lock();

  FSourceStream.Position := 0;
  FSourceStream.Write(Data, DataSize);

  FCompressingEvent.SetEvent(); // Start compressing
end;

procedure TLZCompressionThread.WaitCompressing;
begin
  FCompressingLock.WaitLocked();
end;

end.

