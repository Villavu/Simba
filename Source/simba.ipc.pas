{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple inter process communication server & client using regular pipes.
}
unit simba.ipc;

{$i simba.inc}

interface

uses
  classes, sysutils,
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  pipes, syncobjs;

type
  TSimbaIPCHeader = record
    Size: Integer;
    MessageID: Integer;
  end;

  TSimbaIPCServer = class
  protected
    FInputClient: TOutputPipeStream;
    FInputStream: TInputPipeStream;
    FOutputClient: TInputPipeStream;
    FOutputStream: TOutputPipeStream;

    FClientID: String;
    FThread: TThread;

    procedure Execute;

    procedure OnMessage(MessageID: Integer; Params, Result: TMemoryStream); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    property ClientID: String read FClientID;
  end;

  TSimbaIPCClient = class
  protected
    FInputStream: TInputPipeStream;
    FOutputStream: TOutputPipeStream;
    FLock: TCriticalSection;

    FMessageID: Integer;
    FParams: TMemoryStream;
    FResult: TMemoryStream;
  public
    constructor Create(ServerID: String);
    destructor Destroy; override;

    procedure BeginInvoke(MethodID: Integer);
    procedure EndInvoke;

    procedure Invoke;
  end;

implementation

uses
  lazloggerbase,
  simba.mufasatypes, simba.helpers_string;

procedure TSimbaIPCServer.Execute;
var
  Params: TMemoryStream;
  Result: TMemoryStream;
  Header: TSimbaIPCHeader;
begin
  Params := TMemoryStream.Create();
  Result := TMemoryStream.Create();

  try
    while (FInputStream.Read(Header, SizeOf(TSimbaIPCHeader)) = SizeOf(TSimbaIPCHeader)) and (not TThread.CheckTerminated) do
    begin
      // Keep a small amount of memory (0.25 mb) if allocated
      Params.Position := 0;
      if (Params.Size > 512*512) then
        Params.Size := 512*512;

      Result.Position := 0;
      if (Result.Size > 512*512) then
        Result.Size := 512*512;

      // Copy parameters
      if (Header.Size > 0) then
        Params.CopyFrom(FInputStream, Header.Size);

      Params.Position := 0;

      OnMessage(Header.MessageID, Params, Result);

      // Write result header
      Header.Size := Result.Position;
      Header.MessageID := Header.MessageID;

      FOutputStream.Write(Header, SizeOf(TSimbaIPCHeader));

      // Write result data
      if (Header.Size > 0) then
      begin
        Result.Position := 0;

        FOutputStream.CopyFrom(Result, Header.Size);
      end;
    end;
  except
    on E: Exception do
      DebugLn('TSimbaIPCServer.Execute ' + E.Message);
  end;

  Params.Free();
  Result.Free();
end;

constructor TSimbaIPCServer.Create;

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

var
  InputHandle: THandle = 0;
  OutputHandle: THandle = 0;
begin
  inherited Create();

  // Input
  if (not CreatePipeHandles(InputHandle, OutputHandle, 65536)) then
    raise Exception.Create('Unable to create input pipe');

  FInputStream := TInputPipeStream.Create(InputHandle);
  FInputClient := TOutputPipeStream.Create(DuplicateHandle(OutputHandle));

  // Output
  if (not CreatePipeHandles(InputHandle, OutputHandle, 65536)) then
    raise Exception.Create('Unable to create output pipe');

  FOutputStream := TOutputPipeStream.Create(OutputHandle);
  FOutputClient := TInputPipeStream.Create(DuplicateHandle(InputHandle));

  FClientID := IntToHex(FInputClient.Handle, 16) + IntToHex(FOutputClient.Handle, 16);
  FThread := TThread.ExecuteInThread(@Execute);
end;

destructor TSimbaIPCServer.Destroy;
begin
  FThread.FreeOnTerminate := False;
  FThread.Terminate();

  while not FThread.Finished do
  begin
    try
      FInputClient.WriteByte(0); // Wake from FInputStream.Read
    except
    end;

    Sleep(50);
  end;

  FThread.Free();

  FInputStream.Free();
  FInputClient.Free();

  FOutputStream.Free();
  FOutputClient.Free();

  inherited Destroy();
end;

procedure TSimbaIPCClient.BeginInvoke(MethodID: Integer);
begin
  FLock.Enter();

  FMessageID := MethodID;

  // Keep a small amount of memory (0.25 mb) if allocated
  FParams.Position := 0;
  if (FParams.Size > 512*512) then
    FParams.Size := 512*512;

  FResult.Position := 0;
  if (FResult.Size > 512*512) then
    FResult.Size := 512*512;
end;

procedure TSimbaIPCClient.EndInvoke;
begin
  FLock.Leave();
end;

procedure TSimbaIPCClient.Invoke;
var
  Header: TSimbaIPCHeader;
begin
  FLock.Enter();

  try
    // Send request
    Header.Size := FParams.Position;
    Header.MessageID := FMessageID;

    FOutputStream.Write(Header, SizeOf(TSimbaIPCHeader));
    FOutputStream.Write(FParams.Memory^, FParams.Position);

    // Read result
    FInputStream.Read(Header, SizeOf(TSimbaIPCHeader));

    if (Header.Size > 0) then
    begin
      FResult.CopyFrom(FInputStream, Header.Size);
      FResult.Position := 0;
    end;
  finally
    FLock.Leave();
  end;
end;

constructor TSimbaIPCClient.Create(ServerID: String);
begin
  FLock := TCriticalSection.Create();

  FOutputStream := TOutputPipeStream.Create(StrToInt('$' + ServerID.Copy(1, 16)));
  FInputStream := TInputPipeStream.Create(StrToInt('$' + ServerID.Copy(16+1, 16)));

  FParams := TMemoryStream.Create();
  FResult := TMemoryStream.Create();
end;

destructor TSimbaIPCClient.Destroy;
begin
  FLock.Free();

  FOutputStream.Free();
  FInputStream.Free();

  FParams.Free();
  FResult.Free();

  inherited Destroy();
end;

end.

