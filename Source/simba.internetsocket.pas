{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Basic internet socket to connect to and read/write strings.
}
unit simba.internetsocket;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ssockets,
  simba.base, simba.baseclass, simba.openssl,
  simba.vartype_string, simba.vartype_ordarray;

type
  PSimbaInternetSocket = ^TSimbaInternetSocket;
  TSimbaInternetSocket = class(TSimbaBaseClass)
  protected
    FSocket: TInetSocket;
    FHost: String;
    FPort: UInt16;
    FUseSSL: Boolean;

    function GetLastError: Integer;
    function GetConnectTimeout: Integer;
    function GetReadWriteTimeout: Integer;

    procedure SetConnectTimeout(Value: Integer);
    procedure SetReadWriteTimeout(Value: Integer);
  public
    constructor Create(AHost: String; APort: UInt16; UseSSL: Boolean); reintroduce;
    destructor Destroy; override;

    procedure Connect;

    function HasData: Boolean;

    function Read(MaxLen: Integer = 8192): TByteArray;
    function ReadUntil(Seq: TByteArray; Timeout: Integer): TByteArray;
    function Write(Data: TByteArray): Integer;
    function ReadString(MaxLen: Integer = 8192): String;
    function ReadStringUntil(Seq: String; Timeout: Integer): String;
    function WriteString(Str: String): Integer;

    property ReadWriteTimeout: Integer read GetReadWriteTimeout Write SetReadWriteTimeout;
    property ConnectTimeout: Integer read GetConnectTimeout Write SetConnectTimeout;
    property LastError: Integer read GetLastError;
  end;

implementation

uses
{$ifdef unix}
  BaseUnix,
{$endif}
{$ifdef windows}
  WinSock2,
{$endif}
  opensslsockets, sockets;

// is protected
type
  TInetSocketHelper = class helper for TInetSocket
    function SetSocketBlockingMode(ASocket: Integer; ABlockMode: TBlockingMode; AFDSPtr: Pointer): Boolean;
  end;

function TInetSocketHelper.SetSocketBlockingMode(ASocket: Integer; ABlockMode: TBlockingMode; AFDSPtr: Pointer): Boolean;
begin
  Result := inherited SetSocketBlockingMode(ASocket, ABlockMode, AFDSPtr);
end;

function TSimbaInternetSocket.GetLastError: Integer;
begin
  Result := FSocket.LastError;
end;

function TSimbaInternetSocket.GetConnectTimeout: Integer;
begin
  Result := FSocket.ConnectTimeout;
end;

function TSimbaInternetSocket.GetReadWriteTimeout: Integer;
begin
  Result := FSocket.IOTimeout;
end;

procedure TSimbaInternetSocket.SetConnectTimeout(Value: Integer);
begin
  FSocket.ConnectTimeout := Value;
end;

procedure TSimbaInternetSocket.SetReadWriteTimeout(Value: Integer);
begin
  FSocket.IOTimeout := Value;
end;

constructor TSimbaInternetSocket.Create(AHost: String; APort: UInt16; UseSSL: Boolean);
begin
  inherited Create();

  FHost := AHost;
  FPort := APort;
  FUseSSL := UseSSL;

  if FUseSSL then
    FSocket := TInetSocket.Create(FHost, FPort, TOpenSSLSocketHandler.GetDefaultHandler())
  else
    FSocket := TInetSocket.Create(FHost, FPort, TSocketHandler.Create());
end;

destructor TSimbaInternetSocket.Destroy;
begin
  if (FSocket <> nil) then
    FreeAndNil(FSocket);

  inherited Destroy;
end;

procedure TSimbaInternetSocket.Connect;
begin
  if FUseSSL then
    LoadSSL();

  FSocket.Connect();
end;

function TSimbaInternetSocket.HasData: Boolean;
var
  FDS: TFDSet;
  b: Byte;
begin
  FSocket.SetSocketBlockingMode(FSocket.Handle, bmNonBlocking, @FDS);
  FSocket.ReadFlags := MSG_PEEK;

  try
    Result := FSocket.Read(b{%H-}, 1) > 0;
  finally
    FSocket.SetSocketBlockingMode(FSocket.Handle, bmBlocking, @FDS);
    FSocket.ReadFlags := 0;
  end;
end;

function TSimbaInternetSocket.Write(Data: TByteArray): Integer;
begin
  if (Length(Data) > 0) then
    Result := FSocket.Write(Data[0], Length(Data))
  else
    Result := 0;
end;

function TSimbaInternetSocket.ReadString(MaxLen: Integer): String;
begin
  Result := Read(MaxLen).ToString();
end;

function TSimbaInternetSocket.ReadStringUntil(Seq: String; Timeout: Integer): String;
begin
  Result := ReadUntil(Seq.ToBytes(), Timeout).ToString();
end;

function TSimbaInternetSocket.WriteString(Str: String): Integer;
begin
  Result := Write(Str.ToBytes());
end;

function TSimbaInternetSocket.Read(MaxLen: Integer): TByteArray;
begin
  SetLength(Result, MaxLen);
  SetLength(Result, FSocket.Read(Result[0], MaxLen));
end;

function TSimbaInternetSocket.ReadUntil(Seq: TByteArray; Timeout: Integer): TByteArray;
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

end.

