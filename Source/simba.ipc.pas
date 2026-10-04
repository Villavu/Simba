{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Messages both ways between two processes: a lane of two pipes each way, one
  message and its response at a time.
}
unit simba.ipc;

{$i simba.inc}

{.$DEFINE IPC_DEBUG}

interface

uses
  Classes, SysUtils, syncobjs,
  simba.base;

type
  TSimbaIPCKey = String[31];

  ESimbaIPCClosed = class(Exception);

  TSimbaIPCLane = class
  protected
    FReadPipe: THandle;
    FWritePipe: THandle;

    FKey: TSimbaIPCKey;
    // -1 until the response's header
    FReadLeft: Int64;
    FWriteLeft: Int64;
    {$IFDEF IPC_DEBUG}
    FPeerPID: UInt32;
    {$ENDIF}

    procedure Close; virtual;

    procedure PipeRead(var Buffer; Count: Int64);
    procedure PipeWrite(const Buffer; Count: Int64);

    procedure ReadHeader;
    procedure WriteHeader(Size: Int64);
  public
    constructor Create(ReadPipe, WritePipe: THandle);
    destructor Destroy; override;

    procedure ReadInteger(out Value: Int32);
    procedure ReadBoolean(out Value: Boolean);
    procedure ReadString(out Value: String);
    procedure ReadData(var Data; Size: Int64);

    procedure WriteInteger(Value: Int32);
    procedure WriteBoolean(Value: Boolean);
    procedure WriteString(const Value: String);
    procedure WriteData(const Data; Size: Int64);

    property Key: TSimbaIPCKey read FKey;
  end;

  TSimbaIPCOutgoing = class(TSimbaIPCLane)
  protected
    FLock: TCriticalSection;

    procedure Close; override;
    procedure Lock;
  public
    constructor Create(ReadPipe, WritePipe: THandle);
    destructor Destroy; override;

    procedure BeginMessage(const AKey: String; Size: Int64);
    procedure Send;
    procedure EndMessage;
    procedure SendMessage(const AKey: String);
  end;

  TSimbaIPCIncoming = class(TSimbaIPCLane)
  protected
    procedure Receive;
    procedure EndResponse;
  public
    procedure BeginResponse(Size: Int64);
  end;

  //   Outgoing.BeginMessage('Foo', SizeOf(Int32));
  //   try
  //     Outgoing.WriteInteger(X);
  //     Outgoing.Send();
  //     Outgoing.ReadInteger(Y);
  //   finally
  //     Outgoing.EndMessage();
  //   end;
  //
  //   case Incoming.Key of
  //     'Foo':
  //       begin
  //         Incoming.ReadInteger(X);
  //         Incoming.BeginResponse(SizeOf(Int32));
  //         Incoming.WriteInteger(Y);
  //       end;
  //   end;

  TSimbaIPCConnection = class(TComponent)
  protected
    FOutgoing: TSimbaIPCOutgoing;
    FIncoming: TSimbaIPCIncoming;
    FThread: TThread;
    FClientOutgoingRead: THandle;
    FClientOutgoingWrite: THandle;
    FClientIncomingRead: THandle;
    FClientIncomingWrite: THandle;

    function GetClientID: String;

    procedure Execute;
    procedure HandleIncoming; virtual; abstract;

    property Outgoing: TSimbaIPCOutgoing read FOutgoing;
    property Incoming: TSimbaIPCIncoming read FIncoming;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(const ClientID: String); reintroduce; overload;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    // release pipes so the client is fully in control
    // basically making their refcount=1
    procedure CloseClientPipes;

    property ClientID: String read GetClientID;
  end;

implementation

uses
  Math, pipes,
  simba.nativeinterface, simba.threading;

const
  PIPE_CHUNK = 256 * 1024; // max read/write at a time

type
  TSimbaIPCHeader = packed record
    Key: TSimbaIPCKey;
    Size: Int64;
    {$IFDEF IPC_DEBUG}
    PID: UInt32; // the writer's
    {$ENDIF}
  end;

{$IFDEF IPC_DEBUG}
procedure Trace(Lane: TSimbaIPCLane; const Msg: String; Args: array of const);
var
  PIDs: String;
begin
  PIDs := IntToStr(GetProcessID());
  if (Lane is TSimbaIPCOutgoing) then
    PIDs := PIDs + ' -> ' + IntToStr(Lane.FPeerPID);
  if (Lane is TSimbaIPCIncoming) then
    PIDs := PIDs + ' <- ' + IntToStr(Lane.FPeerPID);

  {$PUSH}{$I-}
  WriteLn('[IPC ' + PIDs + '] ' + Format(Msg, Args));
  Flush(Output);
  {$POP}
end;
{$ENDIF}

procedure ClosePipe(var Pipe: THandle);
begin
  if (Pipe <> feInvalidHandle) then
    FileClose(Pipe);

  Pipe := feInvalidHandle;
end;

constructor TSimbaIPCLane.Create(ReadPipe, WritePipe: THandle);
begin
  inherited Create();

  FReadPipe := ReadPipe;
  FWritePipe := WritePipe;

  // else child processes hold them open
  SimbaNativeInterface.SetHandleInheritable(FReadPipe, False);
  SimbaNativeInterface.SetHandleInheritable(FWritePipe, False);
end;

destructor TSimbaIPCLane.Destroy;
begin
  Close();

  inherited Destroy();
end;

procedure TSimbaIPCLane.Close;
begin
  ClosePipe(FReadPipe);
  ClosePipe(FWritePipe);
end;

procedure TSimbaIPCLane.PipeRead(var Buffer; Count: Int64);
var
  P: PByte;
  N: Integer;
begin
  P := @Buffer;
  while (Count > 0) do
  begin
    N := FileRead(FReadPipe, P^, Min(Count, PIPE_CHUNK));
    if (N <= 0) then
      raise ESimbaIPCClosed.Create('IPC pipe closed');
    Inc(P, N);
    Dec(Count, N);
  end;
end;

procedure TSimbaIPCLane.PipeWrite(const Buffer; Count: Int64);
var
  P: PByte;
  N: Integer;
begin
  P := @Buffer;
  while (Count > 0) do
  begin
    N := FileWrite(FWritePipe, P^, Min(Count, PIPE_CHUNK));
    if (N <= 0) then
      raise ESimbaIPCClosed.Create('IPC pipe closed');
    Inc(P, N);
    Dec(Count, N);
  end;
end;

procedure TSimbaIPCLane.ReadHeader;
var
  Header: TSimbaIPCHeader;
begin
  PipeRead(Header, SizeOf(TSimbaIPCHeader));

  FKey := Header.Key;
  FReadLeft := Header.Size;
  {$IFDEF IPC_DEBUG}
  FPeerPID := Header.PID;
  {$ENDIF}
end;

procedure TSimbaIPCLane.WriteHeader(Size: Int64);
var
  Header: TSimbaIPCHeader;
begin
  Header.Key := FKey;
  Header.Size := Size;
  {$IFDEF IPC_DEBUG}
  Header.PID := GetProcessID();
  {$ENDIF}

  FWriteLeft := Size;

  PipeWrite(Header, SizeOf(TSimbaIPCHeader));
end;

procedure TSimbaIPCLane.ReadInteger(out Value: Int32);
begin
  ReadData(Value, SizeOf(Int32));
end;

procedure TSimbaIPCLane.ReadBoolean(out Value: Boolean);
begin
  ReadData(Value, SizeOf(Boolean));
end;

procedure TSimbaIPCLane.ReadString(out Value: String);
var
  Len: Int32;
begin
  ReadInteger(Len);

  SetLength(Value, Len);
  if (Len > 0) then
    ReadData(Value[1], Len);
end;

procedure TSimbaIPCLane.ReadData(var Data; Size: Int64);
begin
  if (Size > FReadLeft) then
    raise Exception.Create('IPC message is shorter than its reads');

  PipeRead(Data, Size);
  Dec(FReadLeft, Size);
end;

procedure TSimbaIPCLane.WriteInteger(Value: Int32);
begin
  WriteData(Value, SizeOf(Int32));
end;

procedure TSimbaIPCLane.WriteBoolean(Value: Boolean);
begin
  WriteData(Value, SizeOf(Boolean));
end;

procedure TSimbaIPCLane.WriteString(const Value: String);
var
  Len: Int32;
begin
  Len := Length(Value);

  WriteInteger(Len);
  if (Len > 0) then
    WriteData(Value[1], Len);
end;

procedure TSimbaIPCLane.WriteData(const Data; Size: Int64);
begin
  if (Size > FWriteLeft) then
    raise Exception.Create('IPC message is longer than its size');

  PipeWrite(Data, Size);
  Dec(FWriteLeft, Size);
end;

constructor TSimbaIPCOutgoing.Create(ReadPipe, WritePipe: THandle);
begin
  inherited Create(ReadPipe, WritePipe);

  FLock := TCriticalSection.Create();
end;

destructor TSimbaIPCOutgoing.Destroy;
begin
  // FLock is needed here
  inherited Destroy();

  FLock.Free();
end;

procedure TSimbaIPCOutgoing.Close;
begin
  Lock();
  inherited Close();
  FLock.Leave();
end;

procedure TSimbaIPCOutgoing.Lock;
begin
  // the holder could be waiting on a Synchronize
  if (GetCurrentThreadId() = MainThreadID) then
  begin
    while not FLock.TryEnter() do
      CheckSynchronize(1);
  end else
    FLock.Enter();
end;

procedure TSimbaIPCOutgoing.BeginMessage(const AKey: String; Size: Int64);
begin
  Lock();

  FKey := AKey;
  FReadLeft := -1;
  try
    WriteHeader(Size);
  except
    EndMessage();
    raise;
  end;
end;

procedure TSimbaIPCOutgoing.Send;
begin
  if (FWriteLeft > 0) then
    raise Exception.CreateFmt('IPC message "%s" wrote %d bytes less than its size', [FKey, FWriteLeft]);

  ReadHeader();

  {$IFDEF IPC_DEBUG}
  Trace(Self, 'sent "%s", response %d bytes', [FKey, FReadLeft]);
  {$ENDIF}
end;

procedure TSimbaIPCOutgoing.EndMessage;
begin
  try
    if (FReadLeft <> 0) then
    begin
      {$IFDEF IPC_DEBUG}
      Trace(Self, '"%s" left the lane out of step: closed', [FKey]);
      {$ENDIF}
      Close();

      if (ExceptObject = nil) then
      begin
        if (FReadLeft < 0) then
          raise Exception.CreateFmt('IPC message "%s" was not sent', [FKey]);
        raise Exception.CreateFmt('IPC response to "%s" has %d bytes unread', [FKey, FReadLeft]);
      end;
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaIPCOutgoing.SendMessage(const AKey: String);
begin
  BeginMessage(AKey, 0);
  try
    Send();
  finally
    EndMessage();
  end;
end;

procedure TSimbaIPCIncoming.Receive;
begin
  ReadHeader();
  FWriteLeft := -1;
end;

procedure TSimbaIPCIncoming.BeginResponse(Size: Int64);
begin
  if (FReadLeft > 0) then
    raise Exception.CreateFmt('IPC message "%s" has %d bytes unread', [FKey, FReadLeft]);

  WriteHeader(Size);
end;

procedure TSimbaIPCIncoming.EndResponse;
begin
  if (FWriteLeft < 0) then
    BeginResponse(0);
  if (FWriteLeft > 0) then
    raise Exception.CreateFmt('IPC response to "%s" wrote %d bytes less than its size', [FKey, FWriteLeft]);
end;

// four handles, 16 hex digits each
function TSimbaIPCConnection.GetClientID: String;
begin
  Result := IntToHex(Int64(FClientOutgoingRead), 16) + IntToHex(Int64(FClientOutgoingWrite), 16) +
            IntToHex(Int64(FClientIncomingRead), 16) + IntToHex(Int64(FClientIncomingWrite), 16);
end;

procedure TSimbaIPCConnection.Execute;
begin
  try
    while True do
    begin
      FIncoming.Receive();
      {$IFDEF IPC_DEBUG}
      Trace(FIncoming, 'got "%s" (%d bytes)', [FIncoming.FKey, FIncoming.FReadLeft]);
      {$ENDIF}

      try
        HandleIncoming();
      except
        on E: ESimbaIPCClosed do
          raise;
        on E: Exception do
          DebugLn('IPC message "%s" failed: %s', [FIncoming.FKey, E.Message]);
      end;

      FIncoming.EndResponse();
    end;
  except
    on E: ESimbaIPCClosed do
      ;
    on E: Exception do
      DebugLn(E.Message);
  end;

  // closing ours ends their thread
  FIncoming.Close();
  FOutgoing.Close();
end;

// we read one pipe and write the other, the client the reverse
procedure CreateLane(out ReadPipe, WritePipe, ClientRead, ClientWrite: THandle);
begin
  if not (CreatePipeHandles(ReadPipe, ClientWrite, 4096) and CreatePipeHandles(ClientRead, WritePipe, 4096)) then
    raise Exception.Create('Unable to create IPC pipes');

  SimbaNativeInterface.SetHandleInheritable(ClientRead, True);
  SimbaNativeInterface.SetHandleInheritable(ClientWrite, True);
end;

constructor TSimbaIPCConnection.Create(AOwner: TComponent);
var
  ReadPipe, WritePipe: THandle;
begin
  inherited Create(AOwner);

  // the client's outgoing lane is our incoming one
  CreateLane(ReadPipe, WritePipe, FClientOutgoingRead, FClientOutgoingWrite);
  FIncoming := TSimbaIPCIncoming.Create(ReadPipe, WritePipe);
  CreateLane(ReadPipe, WritePipe, FClientIncomingRead, FClientIncomingWrite);
  FOutgoing := TSimbaIPCOutgoing.Create(ReadPipe, WritePipe);

  {$IFDEF IPC_DEBUG}
  Trace(nil, 'Server created', []);
  Trace(nil, '  FOutgoing.FReadPipe=%d', [FOutgoing.FReadPipe]);
  Trace(nil, '  FOutgoing.FWritePipe=%d', [FOutgoing.FWritePipe]);
  Trace(nil, '  FIncoming.FReadPipe=%d', [FIncoming.FReadPipe]);
  Trace(nil, '  FIncoming.FWritePipe=%d', [FIncoming.FWritePipe]);
  Trace(nil, '  FClientOutgoingRead=%d', [FClientOutgoingRead]);
  Trace(nil, '  FClientOutgoingWrite=%d', [FClientOutgoingWrite]);
  Trace(nil, '  FClientIncomingRead=%d', [FClientIncomingRead]);
  Trace(nil, '  FClientIncomingWrite=%d', [FClientIncomingWrite]);
  {$ENDIF}
end;

constructor TSimbaIPCConnection.Create(const ClientID: String);
begin
  inherited Create(nil);

  if (Length(ClientID) <> 64) then
    raise Exception.CreateFmt('Invalid IPC client ID "%s"', [ClientID]);

  // the server's only: else these close handle 0, which is stdin on Unix
  FClientOutgoingRead := feInvalidHandle;
  FClientOutgoingWrite := feInvalidHandle;
  FClientIncomingRead := feInvalidHandle;
  FClientIncomingWrite := feInvalidHandle;

  FOutgoing := TSimbaIPCOutgoing.Create(THandle(StrToInt64('$' + Copy(ClientID, 1, 16))), THandle(StrToInt64('$' + Copy(ClientID, 17, 16))));
  FIncoming := TSimbaIPCIncoming.Create(THandle(StrToInt64('$' + Copy(ClientID, 33, 16))), THandle(StrToInt64('$' + Copy(ClientID, 49, 16))));

  {$IFDEF IPC_DEBUG}
  Trace(nil, 'Client created', []);
  Trace(nil, '  FOutgoing.FReadPipe=%d', [FOutgoing.FReadPipe]);
  Trace(nil, '  FOutgoing.FWritePipe=%d', [FOutgoing.FWritePipe]);
  Trace(nil, '  FIncoming.FReadPipe=%d', [FIncoming.FReadPipe]);
  Trace(nil, '  FIncoming.FWritePipe=%d', [FIncoming.FWritePipe]);
  {$ENDIF}
end;

destructor TSimbaIPCConnection.Destroy;
begin
  {$IFDEF IPC_DEBUG}
  Trace(nil, 'freeing', []);
  {$ENDIF}

  if (FOutgoing <> nil) then
    CloseClientPipes();

  if (FThread <> nil) then
  begin
    FOutgoing.Close();
    FThread.WaitFor();
    FThread.Free();
  end;

  FIncoming.Free();
  FOutgoing.Free();

  inherited Destroy();
end;

procedure TSimbaIPCConnection.AfterConstruction;
begin
  inherited AfterConstruction();

  FThread := RunInThread(@Execute);
end;

procedure TSimbaIPCConnection.CloseClientPipes;
begin
  ClosePipe(FClientOutgoingRead);
  ClosePipe(FClientOutgoingWrite);
  ClosePipe(FClientIncomingRead);
  ClosePipe(FClientIncomingWrite);
end;

end.
