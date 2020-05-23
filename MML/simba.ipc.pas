unit simba.ipc;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, pipes;

const
  PIPE_TERMINATED = 0;

type
  TSimbaIPC = class
  protected
    FInputStream: TInputPipeStream;
    FOutputStream: TOutputPipeStream;
  public
  const
    BUFFER_SIZE = 1024 * 64;
  public
    function Peek: Int32; virtual;

    function Read(Stream: TMemoryStream): Int32; virtual; overload;
    function Read(var Buffer; Count: Int32): Int32; virtual; overload;

    procedure Write(const Buffer; Count: Int32); virtual; overload;
    procedure Write(const Stream: TMemoryStream); virtual; overload;
  end;

  TSimbaIPC_Server = class(TSimbaIPC)
  protected
    FClient: String;
    FTerminating: Boolean;
    FInputClient: TOutputPipeStream;
    FOutputClient: TInputPipeStream;
  public
    function Read(var Buffer; Count: Int32): Int32; override;

    procedure Terminate;

    property Client: String read FClient;

    constructor Create;
    destructor Destroy; override;
  end;

  TSimbaIPC_Client = class(TSimbaIPC)
  public
    constructor Create(Server: String);
  end;

implementation

{$IFDEF WINDOWS}
uses
  windows;
{$ENDIF}

function DuplicateHandle(Handle: THandle): THandle;
begin
  {$IFDEF WINDOWS}
  if not Windows.DuplicateHandle(GetCurrentProcess(), Handle, GetCurrentProcess(), @Result, 0, True, DUPLICATE_SAME_ACCESS) then
    raise Exception.Create('Unable to duplicate handle');

  CloseHandle(Handle);
  {$ELSE}
  Result := Handle; // Unix subprocesses inherit handles already
  {$ENDIF}
end;

function TSimbaIPC.Peek: Int32;
begin
  Result := FInputStream.NumBytesAvailable;
end;

function TSimbaIPC.Read(Stream: TMemoryStream): Int32;
var
  Count: Int32;
  Buffer: array[0..BUFFER_SIZE - 1] of Byte;
begin
  Result := 0;

  Count := Read(Buffer[0], Length(Buffer));
  if Count > 0 then
  begin
    Result := Result + Stream.Write(Buffer[0], Count);

    while Peek() > 0 do
    begin
      Count := Read(Buffer[0], Length(Buffer));
      if (Count = 0) then
        Break;

      Result := Result + Stream.Write(Buffer[0], Count);
    end;
  end;

  Stream.Position := 0;
end;

function TSimbaIPC.Read(var Buffer; Count: Int32): Int32;
begin
  Result := FInputStream.Read(Buffer, Count);
end;

procedure TSimbaIPC.Write(const Buffer; Count: Int32);
begin
  FOutputStream.Write(Buffer, Count);
end;

procedure TSimbaIPC.Write(const Stream: TMemoryStream);
begin
  FOutputStream.Write(PByte(Stream.Memory)^, Stream.Size);
end;

constructor TSimbaIPC_Client.Create(Server: String);

  function HexToHandle(Hex: String; Index, Count: Int32): THandle;
  begin
    Result := StrToInt64('$' + Copy(Hex, Index, Count));
  end;

begin
  FOutputStream := TOutputPipeStream.Create(HexToHandle(Server, 1, 16));
  FInputStream := TInputPipeStream.Create(HexToHandle(Server, 16 + 1, 16));
end;

constructor TSimbaIPC_Server.Create;
var
  InputHandle: THandle = 0;
  OutputHandle: THandle = 0;
begin
  // Input
  if (not CreatePipeHandles(InputHandle, OutputHandle, BUFFER_SIZE)) then
    raise Exception.Create('Unable to create input pipe handles');

  FInputStream := TInputPipeStream.Create(InputHandle);
  FInputClient := TOutputPipeStream.Create(DuplicateHandle(OutputHandle));

  // Output
  if (not CreatePipeHandles(InputHandle, OutputHandle, BUFFER_SIZE)) then
    raise Exception.Create('Unable to create output pipe handles');

  FOutputStream := TOutputPipeStream.Create(OutputHandle);
  FOutputClient := TInputPipeStream.Create(DuplicateHandle(InputHandle));

  FClient := IntToHex(FInputClient.Handle, 16) + IntToHex(FOutputClient.Handle, 16);
end;

procedure TSimbaIPC_Server.Terminate;
begin
  FTerminating := True;

  try
    FInputClient.WriteByte(0); // Wake from Read
  except
  end;
end;

function TSimbaIPC_Server.Read(var Buffer; Count: Int32): Int32;
begin
  Result := FInputStream.Read(Buffer, Count);
  if FTerminating then
    Result := 0;
end;

destructor TSimbaIPC_Server.Destroy;
begin
  FInputStream.Free();
  FInputClient.Free();
  FOutputStream.Free();
  FOutputClient.Free();

  inherited Destroy();
end;


end.

