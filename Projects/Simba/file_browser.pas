unit file_browser;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, comctrls;

type
  TSimbaFileBrowser_Node = class(TTreeNode)
  public
    Path: String;
    IsDirectory: Boolean;
  end;

  TSimbaFileBrowser = class(TTreeView)
  protected
    FRoot: String;

    procedure Expand(Node: TTreeNode); override;
    procedure SetRoot(Value: String);
  public
    property Root: String read FRoot write SetRoot;

    procedure Refresh;

    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  fileutil,
  simbaunit;

procedure TSimbaFileBrowser.Expand(Node: TTreeNode);

  procedure Populate(Parent: TTreeNode; List: TStrings; IsDirectory: Boolean; Free: Boolean);
  var
    Node: TSimbaFileBrowser_Node;
    i: Int32;
  begin
    for i := 0 to List.Count - 1 do
    begin
      Node := TSimbaFileBrowser_Node.Create(Items);
      Node.Path := List[i];
      Node.IsDirectory := IsDirectory;
      Node.HasChildren := IsDirectory;

      if Node.IsDirectory then
      begin
        Node.ImageIndex := 44;
        Node.SelectedIndex := 44;
      end else
      begin
        if ExtractFileExt(Node.Path) = '.simba' then
        begin
          Node.ImageIndex := 8;
          Node.SelectedIndex := 8;
        end else
        begin
          Node.ImageIndex := 45;
          Node.SelectedIndex := 45;
        end;
      end;

      Items.AddNode(Node, Parent, ExtractFileName(Node.Path), nil, naAddChild);
    end;

    if Free then
      List.Free();
  end;

begin
  inherited Expand(Node);

  if (Node.Count = 0) then
  begin
    BeginUpdate();

    try
      Populate(Node, FindAllFiles(TSimbaFileBrowser_Node(Node).Path, '', False), False, True);
      Populate(Node, FindAllDirectories(TSimbaFileBrowser_Node(Node).Path, False), True, True);

      Node.AlphaSort();
    finally
      EndUpdate();
    end;
  end;
end;

procedure TSimbaFileBrowser.SetRoot(Value: String);
var
  Node: TSimbaFileBrowser_Node;
begin
  FRoot := IncludeTrailingPathDelimiter(Value);
  if (not DirectoryExists(FRoot)) then
    Exit;

  BeginUpdate();

  try
    Items.Clear();

    Node := TSimbaFileBrowser_Node.Create(Items);
    Node.Path := FRoot;
    Node.IsDirectory := True;
    Node.HasChildren := True;

    with Items.AddNode(Node, nil, ExtractFileName(ExcludeTrailingPathDelimiter(FRoot)), nil, naAdd) do
    begin
      Expand(False);

      ImageIndex := 44;
      SelectedIndex := 44;
    end;
  finally
    EndUpdate();
  end;
end;

procedure TSimbaFileBrowser.Refresh;
begin
  Root := Root;
end;

constructor TSimbaFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Images := SimbaForm.Mufasa_Image_List;
  Options := Options + [tvoRightClickSelect];
end;

end.

