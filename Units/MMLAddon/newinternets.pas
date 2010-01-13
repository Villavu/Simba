unit newinternets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPostVariable = record
    variable, value: String;
  end;

  {
       Store the internet connection information
  }
  TInternetConnection = class(TObject)
  protected
    //url of the connection
    ConnURL: String;
    //contains post paramaters and vars
    PostVars: TList;
    PostFreeSpots: array of Integer;
    PostVarsLen, PostVarsHigh, FreeSpotsHigh, FreeSpotsLen : integer;
  private
    function createPostVariable(variable, value: String): TPostVariable;
  public
    constructor Create(URL: String);
    destructor Destroy; override;
    //POST variable functions for PHP transmission
    procedure AddPostVariable(theVar, theValue: String);
    procedure DelPostVariable(theVar: String);
    procedure ReplacePostVariable(searchVar, replaceVar, value: String);
    function PostHTTP(out dataStream: TStream): Boolean;
  end;




  TInternetArray = class(TObject)
  protected
    FreeSpots: array of Integer;
    ConnList : TList;
    ConnArray: array of TInternetConnection;
    ConnHigh, FreeSpotsHigh, FreeSpotsLen : integer;

  public
    function ConnectionOpen(URL: String): Integer;
    function ConnectionClose(ConnInd: Integer): Boolean;
    destructor Destroy; override;
    //POST variable functions for PHP transmission
    procedure AddPostVariable(connInd: Integer; theVar, theValue: String);
    procedure DelPostVariable(connInd: Integer; theVar: String);
    procedure ReplacePostVariable(connInd: Integer; searchVar, replaceVar, value: String);
    function PostHTTP(connInd: Integer; out dataStream: TStream): Boolean;
  end;

function GetPage(URL: String): String;

implementation
uses
  httpsend;


function replace(sStr, rStr, iStr: String): String;
var
  ind: Integer;
begin
  ind := Pos(sStr, iStr);
  while (ind <> 0) do
  begin
    Delete(iStr, ind, Length(sStr));
    Insert(rStr, iStr, ind);
  end;
end;

{ TInternetConnection }

procedure TInternetConnection.Create(URL: String);
begin
  inherited;
  Self.ConnURL := URL;
  Self.PostVars.Create;
end;

procedure TInternetConnection.Destroy; overload;
begin
  inherited;
  // ADD CLOSING OF CONNECTION

  // Clear it up
  PostVars.Clear;
end;

procedure TInternetConnect.AddPostVariable(theVar, theValue: String);
var
  currentIndex: Integer;
begin
  theVar := replace(' ', '%20', theVar);        // more needs to be done, I only knew the ' ' replace.
  theValue := replace(' ', '%20', theValue);
  Self.PostVars.Add(createPostVariable(theVar, theValue));
 { with Self do
  begin


    if (FreeSpotsHigh = -1) then
    begin
      setLength(PostVars, PostVarsHigh + 2);
      inc(PostVarsHigh);
      currentIndex := PostVarsHigh;
    end else
    begin
      currentIndex := PostFreeSpots[FreeSpotsHigh];
      dec(FreeSpotsHigh);
    end;
    PostVars[currentIndex].variable := theVar;
    PostVars[currentIndex].value := theValue;

  end;  }
end;

procedure TInternetConnection.DelPostVariable(theVar: String);
var
  i: Integer;
  tempPostVar: TPostVariable;
begin
  for i := (Self.PostVars.Count - 1) downto 0 do
  begin
    if (theVar = Self.PostVars.Items[i].variable) then
    begin
      tempPostVar := PostVars.Items[i];
      Self.PostVars.Remove(tempPostVar);
      break;
    end;
  end;
end;

procedure TInternetConnection.ReplacePostVariable(searchVar, replaceVar, theValue: String);
var
  i: Integer;
  tempPostVar: TPostVariable;
begin
  with Self do
  begin
    for i := (PostVars.Count - 1) downto 0 do
    begin
      tempPostVar := PostVars.Items[i];
      if (searchVar = tempPostVar.variable) then
      begin
        tempPostVar.variable := replaceVar;
        tempPostVar.value := theValue;
        break;
      end;
    end;
  end;
end;

function TInternetConnection.PostHTTP(out dataStream: TStream): Boolean;
var
  // holds the vars when they are placed together
  URLData: String;
  tempPostVar: TPostVariable;
begin
  try
    with Self do
    begin
      //ADD Connection stuffs

      for i := (PostVars.Count - 1) downto 0 do
      begin
        tempPostVar := PostVars.Items[i];
        if (tempPostVar.variable <> '') then
        begin
          URLData := URLData + format('%d=%d+', [tempPostVar.variable,
                                                 tempPostVar.value]);
        end;
      end;
      Delete(URLData, Length(URLData) - 1, 1);

      {I DONT KNOW (TStream), this should work since we don't reuse it after.}
      dataStream := nil;
      HttpPostURL(ConnURL, URLData, dataStream);

      // Lets remove all Post Variable data so fresh start next time.
      PostVars.Clear;
    end;
  except
    raise Exception.createFMT('TInternetConnection.PostHTTP: Something went wrong, could not complete. URL: %d', URLData);
    exit(false);
  end;
  result := true;
end;

{ TInternetArray }

{
  Allocate space in the ConnArray, then open the connection.
}
function TInternetArray.ConnectionOpen(URL: String): Integer;
var
  currentIndex: Integer;
begin
  try
    Result := Self.ConnList.Add(TInternetConnection.Create(URL));
  except
    raise Exception.createFMT('TInternetArray.ConnectionClose: Could not close connection %d URL: %d',
                               [theInd, ConnURL]);
  end;
end;

{
  Close the connection, add the index to the FreeSpots.
}
function TInternetArray.ConnectionClose(theInd: Integer): Boolean;
var
  tempConn: TInternetConnection;
begin
  try
    tempConn := Self.ConnList.Items[theInd];
    Self.ConnList.Remove(tempConn);
    tempConn.Destroy;
    {
    with Self do
    begin
      ConnArray[theInd].Destroy;
      if (FreeSpotsHigh = FreeSpotsLen) then
      begin
        FreeSpotsLen := FreeSpotsLen + 1;
        setLength(FreeSpots, FreeSpotsLen);
      end;
      FreeSpots[FreeSpotsHigh] := theInd;
    end;  }
  except
    raise Exception.createFMT('TInternetArray.ConnectionClose: Could not close connection %d URL: %d',
                               [theInd, ConnList.Items[theInd].ConnURL]);
    exit(false);
  end;
  result := True;
end;

{
  Wrapper for the TInternetConnection.PostVariableAdd procedure which accepts
  a connection index.
}
procedure TInternetArray.PostVariableAdd(connInd: Integer; theVar, theValue: String);
begin
  try
    ConnList.Items[commInd].PostVariableAdd(theVar, theValue);
  except
    raise Exception.createFMT('TInternetArray.PostVariableAdd: %d is not in the ConnArray',
                               [connInd]);
  end;
end;

{
  Wrapper for the TInternetConnection.PostVariableDel procedure which accepts
  a connection index.
}
procedure TInternetArray.PostVariableDel(connInd: Integer; theVar: String);
begin
  try
    ConnList.Items[commInd].PostVariableAdd(theVar);
  except
    raise Exception.createFMT('TInternetArray.PostVariableDel: %d is not in the ConnArray',
                               [connInd]);
  end;
end;

{
  Wrapper for the TInternetConnection.PostVariableReplace procedure which accepts
  a connection index.
}
procedure TInternetArray.PostVariableReplace(connInd: Integer; searchVar, replaceVar, value: String);
begin
  try
    ConnList.Items[commInd].PostVariableAdd(searchVar, replaceVar, value);
  except
    raise Exception.createFMT('TInternetArray.PostVariableReplace: %d is not in the ConnArray',
                               [connInd]);
  end;
end;

function TInternetArray.PostHTTP(connInd: Integer; out dataStream: TStream): Boolean;
begin
  try
    result := ConnList.Items[commInd].PostHTTP(dataStream);
  except
    raise Exception.createFMT('TInternetArray.PostHTTP: %d is not in the ConnArray',
                               [connInd]);
  end;
end;

{ OTHER }
function GetPage(URL: String): String;
var
  s: TStringList;
begin
  s:=TStringList.Create;
  HttpGetText(URL, s);
  result := String(s.GetText);
  s.Free;
end;

end.


