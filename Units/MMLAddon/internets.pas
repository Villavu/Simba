unit internets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,httpsend,MufasaTypes;

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

end.