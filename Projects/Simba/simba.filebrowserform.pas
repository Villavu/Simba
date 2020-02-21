unit simba.filebrowserform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ExtCtrls, StdCtrls;

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
    procedure Refresh;

    property Root: String read FRoot write SetRoot;
  end;

  TSimbaFileBrowserForm = class(TForm)
    Edit1: TEdit;
    MenuItemCopyRelativePath: TMenuItem;
    MenuItemCopyPath: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem_Open: TMenuItem;
    MenuItem_OpenExternally: TMenuItem;
    MenuItem_Seperator: TMenuItem;
    MenuItem_Refresh: TMenuItem;
    Panel1: TPanel;
    Popup: TPopupMenu;

    procedure Edit1Change(Sender: TObject);
    procedure MenuCopyPathClick(Sender: TObject);
    procedure MenuItemCopyRelativePathClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemOpenExternallyClick(Sender: TObject);
    procedure MenuItemRefreshClick(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure HandleFileBrowserDoubleClick(Sender: TObject);
  protected
    FFileBrowser: TSimbaFileBrowser;

    procedure SetVisible(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaFileBrowserForm: TSimbaFileBrowserForm;

implementation

uses
  fileutil, lclintf, lcltype, clipbrd, lazfileutils, AnchorDocking,
  simba.misc, simba.scripttabsform, simba.main;

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
        Node.ImageIndex := IMAGE_DIRECTORY;
        Node.SelectedIndex := IMAGE_DIRECTORY;
      end else
      begin
        if ExtractFileExt(Node.Path) = '.simba' then
        begin
          Node.ImageIndex := IMAGE_SIMBA;
          Node.SelectedIndex := IMAGE_SIMBA;
        end else
        begin
          Node.ImageIndex := IMAGE_FILE;
          Node.SelectedIndex := IMAGE_FILE;
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

      ImageIndex := IMAGE_DIRECTORY;
      SelectedIndex := IMAGE_DIRECTORY;
    end;
  finally
    EndUpdate();
  end;
end;

procedure TSimbaFileBrowser.Refresh;
begin
  Root := Root;
end;

procedure TSimbaFileBrowserForm.MenuItemOpenClick(Sender: TObject);
begin
  if (FFileBrowser.Selected <> nil) then
    with FFileBrowser.Selected as TSimbaFileBrowser_Node do
    begin
      if not IsDirectory then
        SimbaScriptTabsForm.Open(Path);
    end;
end;

procedure TSimbaFileBrowserForm.Edit1Change(Sender: TObject);
var
  List: TStringList;
  i: Int32;
  Node: TSimbaFileBrowser_Node;
begin
  FFileBrowser.Items.BeginUpdate();
  FFileBrowser.Items.Clear();

  if Edit1.Caption <> '' then
  begin
    List := FindAllFiles(FFileBrowser.Root, '*' + Edit1.Caption + '*', True);

    for i := 0 to List.Count - 1 do
    begin
      Node := TSimbaFileBrowser_Node.Create(FFileBrowser.Items);
      Node.Path := List[i];
      Node.IsDirectory := False;

      if ExtractFileExt(Node.Path) = '.simba' then
      begin
        Node.ImageIndex := IMAGE_SIMBA;
        Node.SelectedIndex := IMAGE_SIMBA;
      end else
      begin
        Node.ImageIndex := IMAGE_FILE;
        Node.SelectedIndex := IMAGE_FILE;
      end;

      FFileBrowser.Items.AddNode(Node, nil, ExtractFileName(Node.Path), nil, naAdd);
    end;

    List.Free();
  end else
    FFileBrowser.Refresh();

  FFileBrowser.Items.EndUpdate();
end;

procedure TSimbaFileBrowserForm.MenuCopyPathClick(Sender: TObject);
begin
  if (FFileBrowser.Selected <> nil) then
    with FFileBrowser.Selected as TSimbaFileBrowser_Node do
      Clipboard.AsText := Path;
end;

procedure TSimbaFileBrowserForm.MenuItemCopyRelativePathClick(Sender: TObject);
begin
  if (FFileBrowser.Selected <> nil) then
    with FFileBrowser.Selected as TSimbaFileBrowser_Node do
      Clipboard.AsText := CreateRelativePath(Path, GetCurrentDir());
end;

procedure TSimbaFileBrowserForm.MenuItemOpenExternallyClick(Sender: TObject);
begin
  if (FFileBrowser.Selected <> nil) then
    with FFileBrowser.Selected as TSimbaFileBrowser_Node do
    begin
      if IsDirectory then
        OpenDirectory(Path)
      else
        OpenDocument(Path);
    end;
end;

procedure TSimbaFileBrowserForm.MenuItemRefreshClick(Sender: TObject);
begin
  FFileBrowser.Refresh();
end;

procedure TSimbaFileBrowserForm.PopupPopup(Sender: TObject);
begin
  MenuItem_Open.Enabled := (FFileBrowser.Selected <> nil) and (not TSimbaFileBrowser_Node(FFileBrowser.Selected).IsDirectory);
  MenuItem_OpenExternally.Enabled := (FFileBrowser.Selected <> nil);
end;

procedure TSimbaFileBrowserForm.HandleFileBrowserDoubleClick(Sender: TObject);
begin
  if (FFileBrowser.Selected <> nil) then
    with FFileBrowser.Selected as TSimbaFileBrowser_Node do
    begin
      if not IsDirectory then
        SimbaScriptTabsForm.Open(Path);
    end;
end;

procedure TSimbaFileBrowserForm.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);

  if DockMaster.GetAnchorSite(Self) <> nil then
    DockMaster.GetAnchorSite(Self).Visible := Value;
end;

constructor TSimbaFileBrowserForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFileBrowser := TSimbaFileBrowser.Create(Self);
  FFileBrowser.Parent := Panel1;
  FFileBrowser.Align := alClient;
  FFileBrowser.Root := Application.Location;
  FFileBrowser.Images := SimbaForm.Images;
  FFileBrowser.Options := FFileBrowser.Options + [tvoRightClickSelect];
  FFileBrowser.PopupMenu := Popup;
  FFileBrowser.ReadOnly := True;
  FFileBrowser.OnDblClick := @HandleFileBrowserDoubleClick;
  FFileBrowser.Options := FFileBrowser.Options + [tvoAutoItemHeight];
end;

initialization
  {$I simba.filebrowserform.lrs}

end.

