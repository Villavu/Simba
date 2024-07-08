{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Basic internet socket functionally.
}
unit simba.internetsocket;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ssockets,
  simba.base, simba.baseclass, simba.threading;

type
  TInternetSocket = class(TSimbaBaseClass)
  protected
    FSocket: TInetSocket;
    FHost: String;
    FPort: UInt16;
    FUseSSL: Boolean;
    FReadWriteLock: TEnterableLock;

    // these should be used and not FSocket.Read/Write
    function DoReadBytes(const MaxLen: Integer = 8192): TByteArray; virtual;
    function DoWriteBytes(const Data: TByteArray): Integer; virtual;

    function GetLastError: Integer; virtual;
    function GetConnectTimeout: Integer; virtual;
    function GetReadWriteTimeout: Integer; virtual;
    function GetLocalAddress: String; virtual;
    function GetRemoteAddress: String; virtual;

    procedure SetConnectTimeout(Value: Integer); virtual;
    procedure SetReadWriteTimeout(Value: Integer); virtual;
  public
    constructor Create(AHost: String; APort: UInt16; UseSSL: Boolean); reintroduce; overload;
    constructor Create(Sock: TInetSocket); reintroduce; overload;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Close; virtual;

    function HasData: Boolean; virtual;

    function Read(MaxLen: Integer = 8192): TByteArray; virtual;
    function ReadString(MaxLen: Integer = 8192): String; virtual;
    function ReadUntil(Seq: TByteArray; Timeout: Integer): TByteArray; virtual;
    function ReadStringUntil(Seq: String; Timeout: Integer): String; virtual;

    function Write(Data: TByteArray): Integer; virtual;
    function WriteString(Str: String): Integer; virtual;

    property ReadWriteTimeout: Integer read GetReadWriteTimeout Write SetReadWriteTimeout;
    property ConnectTimeout: Integer read GetConnectTimeout Write SetConnectTimeout;
    property LastError: Integer read GetLastError;
    property LocalAddress: String read GetLocalAddress;
    property RemoteAddress: String read GetRemoteAddress;
  end;

  TInternetSocketASync = class(TInternetSocket)
  public type
    TDataEvent = procedure(Socket: TInternetSocketASync) of object;
    TDisconnectEvent = procedure(Socket: TInternetSocketASync) of object;
  protected
    FDataEvent: TDataEvent;
    FDisconnectEvent: TDisconnectEvent;
    FRunning: TWaitableLock;

    procedure DoExecute;

    function GetRunning: Boolean; virtual;
  public
    procedure Connect; override;

    property OnData: TDataEvent read FDataEvent write FDataEvent;
    property OnDisconnect: TDisconnectEvent read FDisconnectEvent write FDisconnectEvent;
    property Running: Boolean read GetRunning;
  end;

  TInternetSocketServer = class(TSimbaBaseClass)
  protected
  type
    TServerThread = class(TThread)
    protected
      FServer: TInternetSocketServer;

      procedure Execute; override;
    public
      constructor Create(AServer: TInternetSocketServer); reintroduce;
      destructor Destroy; override;
    end;

    TClientThread = class(TThread)
    protected
      FServer: TInternetSocketServer;
      FSock: TInternetSocket;

      procedure Execute; override;
    public
      constructor Create(Server: TInternetSocketServer; Sock: TInternetSocket); reintroduce;
      destructor Destroy; override;
    end;
  public type
    THandleClientEvent = procedure(Sender: TInternetSocketServer; Sock: TInternetSocket) of object;
    TAllowClientEvent = procedure(Sender: TInternetSocketServer; Address: String; ConnectionCount: Integer; var Allow: Boolean) of object;
  protected
    FRunning: TWaitableLock;
    FConnectionCount: Integer;
    FServer: TInetServer;
    FOnHandleClient: THandleClientEvent;
    FOnAllowClient: TAllowClientEvent;

    procedure DoConnection(Sender: TObject; Data: TSocketStream);
    procedure DoAllowConnection(Sender: TObject; ASocket: Longint; var Allow: Boolean);

    function GetRunning: Boolean;
  public
    constructor Create(AHost: String; APort: Integer); reintroduce; overload;
    constructor Create(APort: Integer); reintroduce; overload;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Running: Boolean read GetRunning;
    property ConnectionCount: Integer read FConnectionCount;
    property OnHandleClient: THandleClientEvent read FOnHandleClient write FOnHandleClient;
    property OnAllowClient: TAllowClientEvent read FOnAllowClient write FOnAllowClient;
  end;

implementation

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  WinSock2,
  {$ENDIF}
  opensslsockets,
  sockets,
  simba.vartype_string,
  simba.vartype_ordarray,
  simba.openssl;

// is protected
type
  TInetSocketHelper = class helper for TInetSocket
    function SetSocketBlockingMode(ASocket: Integer; ABlockMode: TBlockingMode; AFDSPtr: Pointer): Boolean;
  end;

function TInetSocketHelper.SetSocketBlockingMode(ASocket: Integer; ABlockMode: TBlockingMode; AFDSPtr: Pointer): Boolean;
begin
  Result := inherited SetSocketBlockingMode(ASocket, ABlockMode, AFDSPtr);
end;

function TInternetSocket.GetLocalAddress: String;
begin
  Result := NetAddrToStr(FSocket.LocalAddress.sin_addr);
end;

function TInternetSocket.GetRemoteAddress: String;
begin
  Result := NetAddrToStr(FSocket.RemoteAddress.sin_addr);
end;

function TInternetSocket.DoReadBytes(const MaxLen: Integer): TByteArray;
begin
  FReadWriteLock.Enter();
  try
    SetLength(Result, MaxLen);
    SetLength(Result, FSocket.Read(Result[0], MaxLen));
  finally
    FReadWriteLock.Leave();
  end;
end;

function TInternetSocket.DoWriteBytes(const Data: TByteArray): Integer;
begin
  FReadWriteLock.Enter();
  try
    if (Length(Data) > 0) then
      Result := FSocket.Write(Data[0], Length(Data))
    else
      Result := 0;
  finally
    FReadWriteLock.Leave();
  end;
end;

function TInternetSocket.GetLastError: Integer;
begin
  Result := FSocket.LastError;
end;

function TInternetSocket.GetConnectTimeout: Integer;
begin
  Result := FSocket.ConnectTimeout;
end;

function TInternetSocket.GetReadWriteTimeout: Integer;
begin
  Result := FSocket.IOTimeout;
end;

procedure TInternetSocket.SetConnectTimeout(Value: Integer);
begin
  FSocket.ConnectTimeout := Value;
end;

procedure TInternetSocket.SetReadWriteTimeout(Value: Integer);
begin
  FSocket.IOTimeout := Value;
end;

constructor TInternetSocket.Create(AHost: String; APort: UInt16; UseSSL: Boolean);
begin
  inherited Create();

  FHost := AHost;
  FPort := APort;
  FUseSSL := UseSSL;
end;

constructor TInternetSocket.Create(Sock: TInetSocket);
begin
  inherited Create();

  FSocket := Sock;
end;

destructor TInternetSocket.Destroy;
begin
  if (FSocket <> nil) then
  begin
    Close();

    FreeAndNil(FSocket);
  end;

  inherited Destroy;
end;

procedure TInternetSocket.Connect;
begin
  if FUseSSL then
    LoadSSL();

  if (FSocket <> nil) then
    FreeAndNil(FSocket);

  if FUseSSL then
    FSocket := TInetSocket.Create(FHost, FPort, TOpenSSLSocketHandler.GetDefaultHandler())
  else
    FSocket := TInetSocket.Create(FHost, FPort, TSocketHandler.Create());

  FSocket.Connect();
end;

procedure TInternetSocket.Close;
begin
  CloseSocket(FSocket.Handle);
end;

function TInternetSocket.HasData: Boolean;
var
  FDS: TFDSet;
  B: Byte;
begin
  FSocket.SetSocketBlockingMode(FSocket.Handle, bmNonBlocking, @FDS);
  FSocket.ReadFlags := MSG_PEEK;

  try
    Result := FSocket.Read(B{%H-}, 1) > 0;
  finally
    FSocket.SetSocketBlockingMode(FSocket.Handle, bmBlocking, @FDS);
    FSocket.ReadFlags := 0;
  end;
end;

function TInternetSocket.Read(MaxLen: Integer): TByteArray;
begin
  Result := DoReadBytes(MaxLen);
end;

function TInternetSocket.ReadString(MaxLen: Integer): String;
begin
  Result := Read(MaxLen).ToString();
end;

function TInternetSocket.ReadUntil(Seq: TByteArray; Timeout: Integer): TByteArray;
var
  SeqLen: Integer;

  function EndsWithSeq(const Data: TByteArray): Boolean;
  begin
    Result := (Length(Data) >= SeqLen) and CompareMem(@Seq[0], @Data[High(Data) - SeqLen], SeqLen);
  end;

var
  T: UInt64;
begin
  Result := [];

  SeqLen := Length(Seq);
  if (SeqLen > 0) then
  begin
    T := GetTickCount64() + Timeout;
    repeat
      if HasData() then
        Result += Read()
      else
        Sleep(50);
    until EndsWithSeq(Result) or (GetTickCount64() > T);
  end;
end;

function TInternetSocket.ReadStringUntil(Seq: String; Timeout: Integer): String;
begin
  Result := ReadUntil(Seq.ToBytes(), Timeout).ToString();
end;

function TInternetSocket.Write(Data: TByteArray): Integer;
begin
  Result := DoWriteBytes(Data);
end;

function TInternetSocket.WriteString(Str: String): Integer;
begin
  Result := Write(Str.ToBytes());
end;

procedure TInternetSocketASync.Connect;
begin
  if Running then
    Exit;
  if (not Assigned(FDataEvent)) then
    SimbaException('OnData cannot be nil');

  inherited Connect();

  RunInThread(@DoExecute, True);
end;

function TInternetSocketASync.GetRunning: Boolean;
begin
  Result := FRunning.IsLocked;
end;

procedure TInternetSocketASync.DoExecute;

  function HasDataAndRunning: Boolean;
  var
    B: Byte;
  begin
    FReadWriteLock.Enter();
    try
      FSocket.ReadFlags := MSG_PEEK;

      Result := FSocket.Read(B{%H-}, SizeOf(Byte)) = SizeOf(Byte);
    finally
      FSocket.ReadFlags := 0;
      FReadWriteLock.Leave();
    end;
  end;

begin
  FRunning.Lock();
  while HasDataAndRunning() and Assigned(FDataEvent) do
    FDataEvent(Self);
  Close();
  FRunning.Unlock();

  if Assigned(FDisconnectEvent) then
    FDisconnectEvent(Self);
end;

procedure TInternetSocketServer.DoConnection(Sender: TObject; Data: TSocketStream);
begin
  TClientThread.Create(Self, TInternetSocket.Create(TInetSocket(Data)));
end;

procedure TInternetSocketServer.DoAllowConnection(Sender: TObject; ASocket: Longint; var Allow: Boolean);
var
  Len: TSockLen;
  Addr: TSockAddr;
begin
  if Assigned(FOnAllowClient) then
  begin
    Len := SizeOf(TSockAddr);
    if fpGetPeerName(ASocket, @Addr, @Len) <> 0 then
      Addr := Default(TSockAddr);

    FOnAllowClient(Self, NetAddrToStr(Addr.sin_addr), FConnectionCount, Allow);
  end;
end;

function TInternetSocketServer.GetRunning: Boolean;
begin
  Result := FRunning.IsLocked;
end;

constructor TInternetSocketServer.Create(AHost: String; APort: Integer);
begin
  FServer := TInetServer.Create(AHost, APort, TSocketHandler.Create());
  FServer.MaxConnections := -1;
  FServer.OnConnect := @DoConnection;
  FServer.OnConnectQuery := @DoAllowConnection;
end;

constructor TInternetSocketServer.Create(APort: Integer);
begin
  Create('127.0.0.1', APort);
end;

destructor TInternetSocketServer.Destroy;
begin
  FreeAndNil(FServer);

  inherited Destroy();
end;

procedure TInternetSocketServer.Start;
begin
  if FRunning.IsLocked() then
    SimbaException('Already running');

  RunInThread(@FServer.StartAccepting, True);
end;

procedure TInternetSocketServer.Stop;
begin
  if FRunning.IsLocked() then
    FServer.StopAccepting(True);
end;

procedure TInternetSocketServer.TServerThread.Execute;
begin
  FServer.FServer.StartAccepting();
end;

constructor TInternetSocketServer.TServerThread.Create(AServer: TInternetSocketServer);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FServer := AServer;
  FServer.FRunning.Lock();
end;

destructor TInternetSocketServer.TServerThread.Destroy;
begin
  FServer.FRunning.Unlock();

  inherited Destroy();
end;

procedure TInternetSocketServer.TClientThread.Execute;
begin
  if Assigned(FServer.FOnHandleClient) then
    FServer.FOnHandleClient(FServer, FSock);
end;

constructor TInternetSocketServer.TClientThread.Create(Server: TInternetSocketServer; Sock: TInternetSocket);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FServer := Server;
  FSock := Sock;

  Inc(FServer.FConnectionCount);
end;

destructor TInternetSocketServer.TClientThread.Destroy;
begin
  Dec(FServer.FConnectionCount);
  FreeAndNil(FSock);

  inherited Destroy();
end;

end.

