unit settings;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ComCtrls;



type
  // remove later
  TStringArray = Array Of String;

  TSettingData = class(TObject)
    public
      Val: String;

      constructor Create;
      destructor Destroy; override;

  end;

  TMMLSettings = class(TObject)

    public
      constructor Create(aNodes: TTreeNodes);
      destructor Destroy; override;

    private
      Nodes: TTreeNodes;
      function KeyNameToKeys(KeyName: String): TStringArray;
      function WalkToNode(KeyName: String): TTreeNode;
      function GetNodePath(Node: TTreeNode): String;

    public
      function ListKeys(KeyName: String): TStringArray;
      function KeyExists(KeyName: String): Boolean;
      function IsKey(KeyName: String): Boolean;
      function IsDirectory(KeyName: String): Boolean;
      procedure SetKey(KeyName: String; KeyValue: String);
      function CreateKey(KeyName: String; CreatePath: Boolean = False): Boolean;
      function GetKeyValue(KeyName: String): String;

  end;

implementation
uses
  strutils;

constructor TSettingData.Create;
begin
  inherited;
  Val := '';
end;

destructor TSettingData.Destroy;
begin
  Val := '';
  inherited;
end;


constructor TMMLSettings.Create(aNodes: TTreeNodes);
begin
  Self.Nodes := aNodes;
end;

destructor TMMLSettings.Destroy;
begin
  Nodes := nil;

  inherited;
end;

function TMMLSettings.KeyNameToKeys(KeyName: String): TStringArray;
  // yay for SRL!
  function srl_Explode(str, del: string): TStringArray;
  var
    i, l, dL: Integer;
  begin
    i := 0;
    l := -1;
    SetLength(Result, 0);
    if (str = '') then
      Exit;
    dL := Length(del) - 1;
    repeat
      Inc(l);
      SetLength(Result, l + 1);
      i := Pos(del, str);
      if i <= 0 then
        Break;
      Result[l] := Copy(str, 1, i - 1);
      Delete(str, 1, i + dL);
    until false;
    Result[l] := Copy(str, 1, Length(str));
  end;
begin
  Result := srl_Explode(KeyName, '/');
end;

function TMMLSettings.WalkToNode(KeyName: String): TTreeNode;
var
  N: TTreeNode;
  i: Integer;
  S: TStringArray;
begin
  Result := nil;

  if KeyName[length(KeyName)]='/' then setlength(KeyName,length(KeyName)-1);
  S := KeyNameToKeys(KeyName);

  if not assigned(s) then
    Exit(nil);

  N := Nodes.GetFirstNode;
  i := 0;

  while N <> nil do
  begin
    if N.Text = s[i] then
    begin
      inc(i);
      if i = length(s) then
        break;
      n := N.GetFirstChild;
    end else
      n := n.GetNextSibling;
  end;

  Result := N;

end;

function TMMLSettings.GetNodePath(Node: TTreeNode): String;
var
  N: TTreeNode;
  s: TStringArray;
  i: Integer;
begin
  if Node = nil then
    Exit('');

  N := Node;
  setlength(s, 0);
  while N <> nil do
  begin
    setlength(s,length(s) + 1);
    s[high(s)] := N.Text;
    N := N.Parent;
  end;

  result := '';
  for i := high(s) downto 0 do
    result := result + s[i] + '/';
end;

function TMMLSettings.ListKeys(KeyName: String): TStringArray;
var
  N: TTreeNode;
  i: Integer;
  S: TStringArray;
begin
  SetLength(Result, 0);
  N := WalkToNode(KeyName);
  if N <> nil then
    N := N.GetFirstChild;

  while N <> nil do
  begin
    setlength(result,length(result)+1);
    result[high(result)] := N.Text;
    N := N.GetNextSibling;
  end;
end;

function TMMLSettings.KeyExists(KeyName: String): Boolean;

begin
  Result := WalkToNode(KeyName) <> nil;
end;

function TMMLSettings.IsKey(KeyName: String): Boolean;
var
  N: TTreeNode;
  i: Integer;
begin
  N := WalkToNode(KeyName);
  if N = nil then
    Exit(False);

  i := 0;

  N := N.GetNextSibling;
  while N <> nil do
  begin
    if N.Text <> 'Value' then
    begin
      writeln(N.text);
      inc(i);
    end;
    N := N.GetNextSibling;
  end;

  Exit(i = 0);
end;

function TMMLSettings.IsDirectory(KeyName: String): Boolean;
var
  N: TTreeNode;
begin
  N := WalkToNode(KeyName);
  if N <> nil then
    Exit(N.HasChildren);
  Exit(False);
end;

function TMMLSettings.GetKeyValue(KeyName: String): String;
var
    N: TTreeNode;
begin
  if IsKey(KeyName) then
    Exit('');
  N := WalkToNode(KeyName);
  if N <> nil then
    if N.GetFirstChild <> nil then
      if assigned(n.GetFirstChild.Data) then
        Exit(TSettingData(n.GetFirstChild.Data).Val);

  Exit('');
end;

function TMMLSettings.CreateKey(KeyName: String; CreatePath: Boolean = False): Boolean;
var
    N, newN, nParent: TTreeNode;
    Path: TStringArray;
    NewPath: String;
    i: Integer;

begin
  if KeyExists(KeyName) then
  begin
    writeln('Key: ' + KeyName + ' exists');
    Exit(False);
  end;
  NewPath := '';
  N := nil;
  nParent := Nodes.GetFirstNode;

  Path := KeyNameToKeys(KeyName);
  if length(path) < 2 then
  begin
    writeln('Path too short!');
    exit(false);
  end;

  if path[0] <> nParent.Text then
  begin
    writeln('First key doesn''t match. First key should always match');
  end;
  for i := 0 to length(Path) - 2 do
  begin
    if Path[i] = '' then
    begin
      writeln('Invalid Key Path / Name');
      exit(false);
    end;
    NewPath := NewPath + Path[i] + '/';
    N := WalkToNode(NewPath);

    if (N = nil) and (not CreatePath) then
    begin
      writeln('(N = nil) and (not CreatePath)');
      exit(false);
    end;

    if (N = nil) and CreatePath then
    begin
      newN := TTreeNode.Create(Nodes);
      newN.Text := Path[i];
      if (nParent = nil) then
      begin
        writeln('This shouldn''t happen...');
        newN.MoveTo(Nodes.GetFirstNode, naAddChild);
        nParent := newN;
      end
      else
      begin
        newN.MoveTo(nParent, naAddChild);
        nParent := newN;
      end;
    end;

    if N <> nil then
      nParent := N;
  end;

  if nParent = nil then
    exit(false);

  newN := TTreeNode.Create(Nodes);
  newN.Text := Path[High(Path)];
  newN.MoveTo(nParent, naAddChild);
end;

procedure TMMLSettings.SetKey(KeyName: String; KeyValue: String);
begin
  if IsKey(KeyName) then
    Exit;
end;

end.

