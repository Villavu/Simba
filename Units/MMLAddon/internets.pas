unit internets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, blcksock, MufasaTypes, math;

function GetPage(URL: String): String;

type
  { THTTPClient }
  THTTPClient = class(TObject)
  private
    HTTPSend : THTTPSend;
    fHandleCookies : boolean;
    PostVariables : TStringList;
    Client : TObject;
  public
    OpenConnectionEvent : TOpenConnectionEvent;
    procedure SetHTTPUserAgent(agent : string);
    function GetHTTPPage(url : string ) : string;
    function PostHTTPPage(Url: string; PostData: string): string;overload;
    function PostHTTPPage(Url: string): string;overload;
    function GetRawHeaders: string;
    procedure ClearPostData;
    procedure AddPostVariable(VarName, VarValue: string);
    procedure SetProxy(pHost, pPort : String);
    constructor Create(Owner : TObject; HandleCookies : boolean = true);
    destructor Destroy;override;
  end;

  { TMInternet }
  TMInternet = class(TObject)
  protected
    Client : TObject;
    Connections : TList;
    HTTPClients : TList;
  public
    OpenConnectionEvent : TOpenConnectionEvent;
    function GetPage(URL: String): String;
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
    constructor Create(Owner: TObject; Socket: TTCPBlockSocket = nil);
    destructor Destroy; override;
  end;

  { TSocks }
  TSocks = class(TObject)
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

var
  ProxyHost, ProxyPort : String;
implementation

uses
  Client;
{ OTHER }
function GetPage(URL: String): String;
var
  HTTP : THTTPSend;
begin;
  HTTP := THTTPSend.Create;
  Result := '';
  try
    if HTTP.HTTPMethod('GET', URL) then
    begin
      SetLength(result,HTTP.Document.Size);
      HTTP.Document.Read(result[1],length(result));
    end;
  finally
    HTTP.Free;
  end;
end;

function TMInternet.GetPage(URL: String): String;
var
  Continue : boolean = true;
begin
  Result := '';
  if Assigned(OpenConnectionEvent) then
  begin;
    OpenConnectionEvent(Self,url,continue);
    if not Continue then
      exit;
  end;
  Result := Internets.GetPage(url);
end;

{ TMInternet }

function TMInternet.CreateHTTPClient(HandleCookies: boolean = true): integer;
begin;
  Result := HTTPClients.Add(THTTPClient.Create(Client,HandleCookies));
  THttpClient(HTTPClients[result]).OpenConnectionEvent:= OpenConnectionEvent;
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
  client := Owner;
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

{ THTTPClient }

procedure THTTPClient.SetHTTPUserAgent(agent: string);
begin
  HTTPSend.UserAgent := agent;
end;

function THTTPClient.GetHTTPPage(url: string): string;
var
  Continue : boolean = true;
begin
  Result := '';
  if Assigned(OpenConnectionEvent) then
  begin;
    OpenConnectionEvent(Self,url,continue);
    if not Continue then
      exit;
  end;
  if not fHandleCookies then
    HTTPSend.Cookies.Clear;
  if (ProxyHost <> '') and (ProxyPort <> '') then
  begin
    HTTPSend.ProxyHost := ProxyHost;
    HTTPSend.ProxyPort := ProxyPort;
  end;
  HTTPSend.MimeType :=  'text/html';
  try
    if HTTPSend.HTTPMethod('GET',url) then
    begin;
      SetLength(result,HTTPSend.Document.Size);
      HTTPSend.Document.Read(result[1],length(result));
    end else
      result := '';
  except
    on e : exception do
      TClient(Client).Writeln('THTTPClient error: ' + e.message);
  end;
end;

function THTTPClient.PostHTTPPage(Url: string; PostData: string): string;
begin
  if (ProxyHost <> '') and (ProxyPort <> '') then
  begin
    HTTPSend.ProxyHost := ProxyHost;
    HTTPSend.ProxyPort := ProxyPort;
  end;
  HTTPSend.MimeType := 'application/x-www-form-urlencoded';
  HTTPSend.Document.Clear;
  HTTPSend.Document.Write(Postdata[1],length(postdata));
  try
    if HTTPSend.HTTPMethod('POST',url) then
    begin;
      SetLength(result,HTTPSend.Document.Size);
      HTTPSend.Document.Read(result[1],Length(result));
    end else
      result := '';
  except
    on e : exception do
      TClient(Client).Writeln('THTTPClient error: ' + e.message);
  end;
end;

function THTTPClient.PostHTTPPage(Url: string): string;
var
  PostData : string;
  i : integer;
  Continue : boolean = true;
begin
  Result := '';
  if Assigned(OpenConnectionEvent) then
  begin;
    OpenConnectionEvent(Self,url,continue);
    if not Continue then
      exit;
  end;
  PostData := '';
  for i := 0 to PostVariables.Count - 1 do
    PostData := PostData + PostVariables[i] +'&';
  if Length(PostData) > 1 then
    setlength(postdata,length(postdata) - 1); //Wipe away that last &
  result := PostHTTPPage(url,postdata);
end;

function THTTPClient.GetRawHeaders: string;
begin
  Result := HTTPSend.Headers.Text;
end;

procedure THTTPClient.ClearPostData;
begin
  PostVariables.Clear;
end;

procedure THTTPClient.AddPostVariable(VarName, VarValue: string);
begin
  PostVariables.Add(Varname + '=' + VarValue);
end;

procedure THTTPClient.SetProxy(pHost, pPort : String);
begin
  ProxyHost := pHost;
  ProxyPort := pPort;
end;

constructor THTTPClient.Create(Owner : TObject; HandleCookies : boolean = true);
begin
  inherited Create;
  Client := Owner;
  HTTPSend := THTTPSend.Create;
  fHandleCookies:= HandleCookies;
  PostVariables := TStringList.Create;
end;

destructor THTTPClient.Destroy;
begin
  HTTPSend.Free;
  PostVariables.Free;
  inherited Destroy;
end;

{ TSocks }

function TSocks.CreateSocket: integer;
begin;
  Result := SockList.Add(TSock.Create(Client));
end;

function TSocks.CreateSocketEx(Socket: TTCPBlockSocket): integer;
begin;
  Result := SockList.Add(TSock.Create(Client, Socket));
end;

function TSocks.GetSocket(Index: integer): TSock;
begin
  if (not (InRange(Index, 0, SockList.Count))) then
    raise exception.CreateFmt('GetSocket: Trying to acces an index(%d) that is out of range', [index]);
  if (SockList[index] = nil) then
    raise exception.CreateFmt('GetSocket: Trying to acces an index(%d) that is freed', [index]);
  Result := TSock(SockList[Index]);
end;

procedure TSocks.FreeSocket(Index: Integer);
begin
  if (not (InRange(Index, 0, SockList.Count))) then
    raise exception.CreateFmt('GetSocket: Trying to free an index(%d) that is out of range', [index]);
  if (SockList[index] = nil) then
    raise exception.CreateFmt('GetSocket: Trying to free an index(%d) that is already freed', [index]);
  TSock(SockList[Index]).Free;
  SockList[Index] := nil;
end;

constructor TSocks.Create(Owner : TObject);
begin
  inherited Create;
  Client := Owner;
  SockList := TList.Create;
end;

destructor TSocks.Destroy;
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
