unit newinternets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  POSTVARS_VARIABLE = 0;
  POSTVARS_VALUE    = 1;

type
  {
       Store the internet connection information
  }
  TInternetConnection = class(TObject)
  protected
    //url of the connection
    ConnURL: String;
    //contains post paramaters and vars
    PostVars: array [0..1] of TStringArray;
    PostFreeSpots: array of Integer;
    PostVarsLen, PostVarsHigh, FreeSpotsHigh, FreeSpotsLen : integer;
  public
    constructor Create(URL: String);
    destructor Destroy; override;
    //POST variable functions for PHP transmission
    procedure PostVariableAdd(theVar, theValue: String);
    procedure PostVariableDel(theVar: String);
    procedure PostVariableReplace(searchVar, replaceVar, value: String);
    function PostHTTP: Boolean;
  end;




  TInternetArray = class(TObject)
  protected
    FreeSpots: array of Integer;
    ConnArray: array of TInternetConnection;
    ConnHigh, FreeSpotsHigh, FreeSpotsLen : integer;

  public
    function ConnectionOpen(URL: String): Integer;
    function ConnectionClose(ConnInd: Integer): Boolean;
    destructor Destroy; override;
    //POST variable functions for PHP transmission
    procedure PostVariableAdd(connInd: Integer; theVar, theValue: String);
    procedure PostVariableDel(connInd: Integer; theVar: String);
    procedure PostVariableReplace(connInd: Integer; searchVar, replaceVar, value: String);
    function PostHTTP(connInd: Integer): Boolean;
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
end;

procedure TInternetConnection.Destroy; overload;
begin
  inherited;
  // ADD CLOSING OF CONNECTION

  // is this all necessary? Memory leaks etc trying to avoid them :p
  setLength(PostVars[POSTVARS_VARIABLE], PostVars[POSTVARS_VALUE], 0);
  setLength(PostFreeSpots, 0);
  FreeSpotsHigh := -1;
  FreeSpotsLen := 0;
  PostVarsHigh := -1;
  PostVarsLen := 0;
end;

procedure TInternetConnect.PostVariableAdd(theVar, theValue: String);
var
  currentIndex: Integer;
begin
  theVar := replace(' ', '%20', theVar);        // more needs to be done, I only knew the ' ' replace.
  theValue := replace(' ', '%20', theValue);
  with Self do
  begin
    if (FreeSpotsHigh = -1) then
    begin
      setLength(PostVars[POSTVARS_VARIABLE], PostVars[POSTVARS_VALUE], PostVarsHigh + 2);
      inc(PostVarsHigh);
      currentIndex := PostVarsHigh;
    end else
    begin
      currentIndex := PostFreeSpots[FreeSpotsHigh];
      dec(FreeSpotsHigh);
    end;
    PostVars[POSTVARS_VARIABLE][currentIndex] := theVar;
    PostVars[POSTVARS_VALUE][currentIndex] := theValue;
  end;
end;

procedure TInternetConnection.PostVariableDel(theVar: String);
var
  i: Integer;
begin
  for i := PostVarHigh downto 0 do
  begin
    if (theVar = Self.PostVars[POSTVARS_VARIABLE][i]) then
    begin
      with Self do
      begin
        PostVars[POSTVARS_VARIABLE][i] := '';
        PostVars[POSTVARS_VALUE][i] := '';
        if (FreeSpotsHigh = FreeSpotsLen) then
        begin
          FreeSpotsLen := FreeSpotsLen + 1;
          setLength(PostFreeSpots, FreeSpotsLen);
        end;
        PostFreeSpots[FreeSpotsHigh] := i;
      end;
    end;
  end;
end;

procedure TInternetConnection.PostVariableReplace(searchVar, replaceVar, value: String);
var
  i: Integer;;
begin
  with Self do
  begin
    for i := PostVarHigh downto 0 do
    begin
      if (searchVar = PostVars[POSTVARS_VARIABLE][i]) then
      begin
        PostVars[POSTVARS_VARIABLE][i] := replaceVar;
        PostVars[POSTVARS_VALUE][i] := value;
      end;
    end;
  end;
end;

function TInternetConnection.PostHTTP: Boolean;
var
  // holds the vars when they are placed together
  URLData: String;
  theStream: TStream;
begin
  try
    with Self do
    begin
      //ADD Connection stuffs

      for i := PostVarsHigh downto 0 do
      begin
        if (PostVars[POSTVARS_VARIABLE] <> '') then
        begin
          URLData := URLData + format('%d=%d+', [PostVars[POSTVARS_VARIABLE][i],
                                                 PostVars[POSTVARS_VALUE][i]]);
        end;
      end;
      Delete(URLData, Length(URLData) - 1, 1);

      {I DONT KNOW (TStream), this should work since we don't reuse it after.}
      HttpPostURL(ConnURL, URLData, theStream);

      // Lets remove all Post Variable data so fresh start next time.
      setLength(PostVars[POSTVARS_VARIABLE], PostVars[POSTVARS_VALUE], 0);
      setLength(PostFreeSpots, 0);
      FreeSpotsHigh := -1;
      FreeSpotsLen := 0;
      PostVarsHigh := -1;
      PostVarsLen := 0;
    end;
  except
    raise Exception.createFMT('TInternetConnection.PostHTTP: Something went wrong, could not complete. (%d)', URLData);
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
  with Self do
  begin
    if (FreeSpotsHigh = -1) then
    begin
      setLength(ConnArray, ConnHigh + 2);
      inc(ConnHigh);
      currentIndex := ConnHigh;
    end else
    begin
      currentIndex := FreeSpots[FreeSpotsHigh];
      dec(FreeSpotsHigh);
    end;
    ConnArray[currentIndex].Create(URL);
  end;
end;

{
  Close the connection, add the index to the FreeSpots.
}
function TInternetArray.ConnectionClose(theInd: Integer): Boolean;
begin
  try
    with Self do
    begin
      ConnArray[theInd].Destroy;
      if (FreeSpotsHigh = FreeSpotsLen) then
      begin
        FreeSpotsLen := FreeSpotsLen + 1;
        setLength(FreeSpots, FreeSpotsLen);
      end;
      FreeSpots[FreeSpotsHigh] := theInd;
    end;
  except
    raise Exception.createFMT('TInternetArray.ConnectionClose: Could not close connection %d URL: %d',
                               [theInd, ConnArray[theInd].ConnURL]);
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
    result := ConnArray[connInd].PostVariableAdd(theVar, theValue);
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
    result := ConnArray[connInd].PostVariableAdd(theVar);
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
    result := ConnArray[connInd].PostVariableAdd(searchVar, replaceVar, value);
  except
    raise Exception.createFMT('TInternetArray.PostVariableReplace: %d is not in the ConnArray',
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


