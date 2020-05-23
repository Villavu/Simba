{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Internets for the Mufasa Macro Library
}
unit simba.internet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, simba.MufasaTypes, math, simba.httpclient;

function GetPage(URL: String): String;

type
  THTTPClient = class(TObject)
  private
    FHTTPClient: TSimbaHTTPClient;
    FHandleCookies: Boolean;
    FPostVariables: TStringList;
    FClient: TObject;

    function GetHeaders: String;
    function GetResponseCode: Int32;
    function GetUserAgent: String;
    procedure SetUserAgent(Value: String);
  public
    property UserAgent: String read GetUserAgent write SetUserAgent;
    property Headers: String read GetHeaders;
    property ResponseCode: Int32 read GetResponseCode;

    function GetHTTPPage(URL: String): String;
    function GetHTTPPage(URL: String; FilePath: String): Int32; overload;
    function PostHTTPPage(URL: String; PostData: String): String; overload;
    function PostHTTPPage(URL: String): String; overload;

    procedure ClearPostData;
    procedure AddPostVariable(Name, Value: String);

    procedure SetProxy(Host, Port: String);

    constructor Create(Owner: TObject; HandleCookies: Boolean = True);
    destructor Destroy; override;
  end;

  { TMInternet }
  TMInternet = class(TObject)
  protected
    Client: TObject;
    Connections: TList;
    HTTPClients: TList;
  public
    function CreateHTTPClient(HandleCookies : boolean = true) : integer;
    function GetHTTPClient(Index : integer) : THTTPClient;
    procedure FreeHTTPClient(Index: Integer);
    constructor Create(Owner : TObject);
    destructor Destroy;override;
  end;

  { TSock }
  TSock = class(TObject)
  private
    Sock: TTCPBlockSocket;
    Timeout: integer;
    Client: TObject;
  public
    function RecvBufferStr(Length: integer): string;
    function RecvString: string;
    function Recv: string;
    procedure Send(Data: string);
    procedure Connect(IP, Port: string);
    procedure Close;
    procedure SetTimeout(Time: integer);
    procedure Bind(IP, Port: string);
    procedure Listen;
    function Accept: TTCPBlockSocket;
    procedure Info(out IP, Port: string);
    constructor Create(Owner: TObject; Socket: TTCPBlockSocket = nil);
    destructor Destroy; override;
  end;

  { TSocks }
  TMSocks = class(TObject)
  protected
    Client: TObject;
    SockList: TList;
  public
    function CreateSocket: integer;
    function CreateSocketEx(Socket: TTCPBlockSocket): integer;
    function GetSocket(Index: integer): TSock;
    procedure FreeSocket(Index: integer);
    constructor Create(Owner : TObject);
    destructor Destroy; override;
  end;

implementation

uses
  simba.client;

function GetPage(URL: String): String;
var
  HTTPClient: TSimbaHTTPClient;
begin
  HTTPClient := TSimbaHTTPClient.Create;

  try
    Result := HTTPClient.Get(URL);
  finally
    HTTPClient.Free();
  end;
end;

{ TMInternet }

function TMInternet.CreateHTTPClient(HandleCookies: boolean = true): integer;
begin
  Result := HTTPClients.Add(THTTPClient.Create(Client,HandleCookies));
end;

function TMInternet.GetHTTPClient(Index: integer): THTTPClient;
begin
  if (index < 0) or (index >= HTTPClients.Count) then
    raise exception.CreateFmt('GetHTTPClient: Trying to acces an index(%d) that is out of range',[index]);
  if HTTPClients[index] = nil then
    raise exception.CreateFmt('GetHTTPClient: Trying to acces an index(%d) that is freed',[index]);
  result := THTTPClient(httpclients[index]);
end;

procedure TMInternet.FreeHTTPClient(Index: Integer);
begin
  if (index < 0) or (index >= HTTPClients.Count) then
    raise exception.CreateFmt('FreeHTTPClient: Trying to free an index(%d) that is out of range',[index]);
  if HTTPClients[index] = nil then
    raise exception.CreateFmt('FreeHTTPClient: Trying to free an index(%d) that is already freed',[index]);
  THTTPClient(HTTPClients[index]).Free;
  HTTPClients[index] := nil;
end;

constructor TMInternet.Create(Owner: TObject);
begin
  inherited Create;
  Client := Owner;
  Connections := TList.Create;
  HTTPClients := TList.Create;
end;

destructor TMInternet.Destroy;
var
  i : integer;
begin
  for i := Connections.Count -1 downto 0 do
    if Connections[i] <> nil then
    begin
      TObject(Connections[i]).Free;
      TClient(Client).Writeln(Format('Connection[%d] has not been freed in the script, freeing it now.',[i]));
    end;
  for i := HTTPClients.Count -1 downto 0 do
    if HTTPClients[i] <> nil then
    begin
      THTTPClient(HTTPClients[i]).Free;
      TClient(Client).Writeln(Format('HTTPClient[%d] has not been freed in the script, freeing it now.',[i]));
    end;
  Connections.Free;
  HTTPClients.Free;
  inherited Destroy;
end;

function THTTPClient.GetHeaders: String;
begin
  Result := FHTTPClient.ResponseHeaders.Text;
end;

function THTTPClient.GetResponseCode: Int32;
begin
  Result := FHTTPClient.ResponseCode;
end;

function THTTPClient.GetUserAgent: String;
begin
  Result := FHTTPClient.RequestHeader['User-Agent'];
end;

procedure THTTPClient.SetUserAgent(Value: String);
begin
  FHTTPClient.RequestHeader['User-Agent'] := Value;
end;

function THTTPClient.GetHTTPPage(URL: String): String;
begin
  Result := '';

  if (not URL.StartsWith('http://', True)) and (not URL.StartsWith('https://', True)) then
    URL := 'http://' + URL;

  try
    if (not FHandleCookies) then
      FHTTPClient.Cookies.Clear();

    Result := FHTTPClient.Get(URL);
  except
    on e: Exception do
      if (FClient <> nil) then
        TClient(FClient).Writeln('THTTPClient Exception: ' + e.Message)
      else
        WriteLn('THTTPClient Exception: ' + e.Message);
  end;
end;

function THTTPClient.GetHTTPPage(URL: String; FilePath: String): Int32;
begin
  if (not URL.StartsWith('http://', True)) and (not URL.StartsWith('https://', True)) then
    URL := 'http://' + URL;

  try
    if (not FHandleCookies) then
      FHTTPClient.Cookies.Clear();

    FHTTPClient.Get(URL, FilePath);
  except
    on e: Exception do
      if (FClient <> nil) then
        TClient(FClient).Writeln('THTTPClient Exception: ' + e.Message)
      else
        WriteLn('THTTPClient Exception: ' + e.Message);
  end;

  Result := FHTTPClient.ResponseCode;
end;

function THTTPClient.PostHTTPPage(URL: String; PostData: String): String;
begin
  Result := '';

  if (not URL.StartsWith('http://', True)) and (not URL.StartsWith('https://', True)) then
    URL := 'http://' + URL;

  try
    if (not FHandleCookies) then
      FHTTPClient.Cookies.Clear();

    Result := FHTTPClient.Post(URL, PostData);
  except
    on e: Exception do
      if (FClient <> nil) then
        TClient(FClient).Writeln('THTTPClient Exception: ' + e.Message)
      else
        WriteLn('THTTPClient Exception: ' + e.Message);
  end;
end;

function THTTPClient.PostHTTPPage(URL: String): String;
var
  PostData: String = '';
  i: Int32;
begin
  Result := '';

  for i := 0 to FPostVariables.Count - 1 do
    PostData := PostData + FPostVariables[i] + '&';
  if (Length(PostData) > 1) then
    SetLength(PostData, Length(PostData) - 1);

  Result := PostHTTPPage(URL, PostData);
end;

procedure THTTPClient.SetProxy(Host, Port: String);
var
  Proxy: TSimbaHTTPClientProxy;
begin
  Proxy := Default(TSimbaHTTPClientProxy);
  Proxy.Host := Host;
  Proxy.Port := StrToInt(Port);

  FHTTPClient.Proxy := Proxy;
end;

procedure THTTPClient.ClearPostData;
begin
  FPostVariables.Clear();
end;

procedure THTTPClient.AddPostVariable(Name, Value: String);
begin
  FPostVariables.Add(Name + '=' + Value);
end;

constructor THTTPClient.Create(Owner: TObject; HandleCookies: Boolean);
begin
  inherited Create();

  FClient := Owner;

  FHTTPClient := TSimbaHTTPClient.Create();
  FHandleCookies := HandleCookies;
  FPostVariables := TStringList.Create;
end;

destructor THTTPClient.Destroy;
begin
  FHTTPClient.Free();
  FPostVariables.Free();

  inherited Destroy;
end;

{ TMSocks }

function TMSocks.CreateSocket: integer;
begin;
  Result := SockList.Add(TSock.Create(Client));
end;

function TMSocks.CreateSocketEx(Socket: TTCPBlockSocket): integer;
begin;
  Result := SockList.Add(TSock.Create(Client, Socket));
end;

function TMSocks.GetSocket(Index: integer): TSock;
begin
  if (not (InRange(Index, 0, SockList.Count))) then
    raise exception.CreateFmt('GetSocket: Trying to acces an index(%d) that is out of range', [index]);
  if (SockList[index] = nil) then
    raise exception.CreateFmt('GetSocket: Trying to acces an index(%d) that is freed', [index]);
  Result := TSock(SockList[Index]);
end;

procedure TMSocks.FreeSocket(Index: Integer);
begin
  if (not (InRange(Index, 0, SockList.Count))) then
    raise exception.CreateFmt('GetSocket: Trying to free an index(%d) that is out of range', [index]);
  if (SockList[index] = nil) then
    raise exception.CreateFmt('GetSocket: Trying to free an index(%d) that is already freed', [index]);
  TSock(SockList[Index]).Free;
  SockList[Index] := nil;
end;

constructor TMSocks.Create(Owner : TObject);
begin
  inherited Create;
  Client := Owner;
  SockList := TList.Create;
end;

destructor TMSocks.Destroy;
var
  i: integer;
begin
  for i := SockList.Count - 1 downto 0 do
    if SockList[i] <> nil then
    begin
      TSock(SockList[i]).Free;
      TClient(Client).WriteLn(Format('Socket[%d] has not been freed in the script, freeing it now.',[i]));
    end;
  SockList.Free;
  inherited Destroy;
end;

{ TSock }

function TSock.RecvBufferStr(Length: integer): string;
begin
  Result := Sock.RecvBufferStr(Length, Timeout);
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

function TSock.RecvString: string;
begin
  Result := Sock.RecvString(Timeout);
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

function TSock.Recv: string;
begin
  Result := Sock.RecvPacket(Timeout);
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

procedure TSock.Send(Data: string);
begin
  Sock.SendString(Data);
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

procedure TSock.Connect(IP, Port: string);
begin
  Sock.Connect(IP, Port);
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

procedure TSock.Close;
begin
  Sock.CloseSocket;
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

procedure TSock.SetTimeout(Time: integer);
begin
  Timeout := Time;
end;

procedure TSock.Bind(IP, Port: string);
begin
  Sock.Bind(IP, Port);
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

procedure TSock.Listen;
begin
  Sock.Listen;
end;

function TSock.Accept: TTCPBlockSocket;
var
  Socket: TTCPBlockSocket;
begin
  Socket := TTCPBlockSocket.Create;
  Socket.Socket := Sock.Accept;
  Result := Socket;
  if (Sock.LastError <> 0) then
     raise Exception.Create('Socket Error ' + IntToStr(Sock.LastError) + ': ' + Sock.LastErrorDesc);
end;

procedure TSock.Info(out IP, Port: string);
begin
  IP := Sock.GetRemoteSinIP;
  Port := IntToStr(Sock.GetRemoteSinPort);
end;

constructor TSock.Create(Owner: TObject; Socket: TTCPBlockSocket = nil);
begin
  inherited Create;
  Client := Owner;
  Timeout := 1500;
  if (Socket <> nil) then
    Sock := Socket
  else
    Sock := TTCPBlockSocket.Create;
end;

destructor TSock.Destroy;
begin
  Sock.Free;
  inherited Destroy;
end;

end.
