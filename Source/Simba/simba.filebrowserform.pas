unit simba.filebrowserform;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, ExtCtrls, StdCtrls,
  simba.hintwindow;

type
  TSimbaFileBrowser_Node = class(TTreeNode)
  public
    Path: String;
    IsDirectory: Boolean;
  end;

  TSimbaFileBrowser_Hint = class(TSimbaHintWindow)
  protected
    procedure DoHide; override;
  public
    Node: TTreeNode;
  end;

  TSimbaFileBrowser = class(TTreeView)
  protected
    FRoot: String;
    FHint: TSimbaFileBrowser_Hint;

    function Sort(ALeft,ARight: TTreeNode): Integer;

    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Expand(Node: TTreeNode); override;
    procedure SetRoot(Value: String);
  public
    procedure Refresh;

    property Root: String read FRoot write SetRoot;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaFileBrowserForm = class(TForm)
    MenuItemCopyRelativePath: TMenuItem;
    MenuItemCopyPath: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem_Open: TMenuItem;
    MenuItem_OpenExternally: TMenuItem;
    MenuItem_Seperator: TMenuItem;
    MenuItem_Refresh: TMenuItem;
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
    FFilter: TEdit;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaFileBrowserForm: TSimbaFileBrowserForm;

implementation

{$R *.lfm}

uses
  fileutil, lclintf, lcltype, clipbrd, lazfileutils,
  simba.misc, simba.scripttabsform, simba.main;

procedure TSimbaFileBrowser_Hint.DoHide;
begin
  inherited DoHide();

  Node := nil;
end;

function TSimbaFileBrowser.Sort(ALeft, ARight: TTreeNode): Integer;
var
  LeftWeight, RightWeight: Integer;
begin
  LeftWeight := 0;
  RightWeight := 0;
  if not TSimbaFileBrowser_Node(ALeft).IsDirectory then
    LeftWeight += 100000;
  if not TSimbaFileBrowser_Node(ARight).IsDirectory then
    RightWeight += 100000;

  Result := CompareText(ALeft.Text, ARight.Text);
  Result += LeftWeight - RightWeight;
end;

procedure TSimbaFileBrowser.MouseLeave;
begin
  inherited MouseLeave();

  FHint.Visible := False;
end;

procedure TSimbaFileBrowser.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  R: TRect;
  FileName: String;
begin
  inherited MouseMove(Shift, X, Y);

  Node := GetNodeAt(X, Y);
  if (Node is TSimbaFileBrowser_Node) then
  begin
    if (FHint.Node = Node) then
      Exit;

    FileName := CreateRelativePath(TSimbaFileBrowser_Node(Node).Path, Application.Location);

    if (FileName <> '') then
    begin
      R := Node.DisplayRect(True);
      R.TopLeft := ClientToScreen(R.TopLeft);
      R.BottomRight := ClientToScreen(R.BottomRight);

      FHint.Node := Node;
      FHint.ActivateHint(R, FileName);

      Exit;
    end;
  end;

  FHint.Hide();
end;

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

      Node.CustomSort(@Sort);
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

constructor TSimbaFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHint := TSimbaFileBrowser_Hint.Create(Self);
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

  if FFilter.Caption <> '' then
  begin
    List := FindAllFiles(FFileBrowser.Root, '*' + FFilter.Caption + '*', True);

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

constructor TSimbaFileBrowserForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFileBrowser := TSimbaFileBrowser.Create(Self);
  FFileBrowser.Parent := Self;
  FFileBrowser.Align := alClient;
  FFileBrowser.BorderSpacing.Left := 3;
  FFileBrowser.BorderSpacing.Right := 3;
  FFileBrowser.Root := Application.Location;
  FFileBrowser.Images := SimbaForm.Images;
  FFileBrowser.Options := FFileBrowser.Options + [tvoRightClickSelect, tvoReadOnly, tvoAutoItemHeight] - [tvoToolTips];
  FFileBrowser.PopupMenu := Popup;
  FFileBrowser.OnDblClick := @HandleFileBrowserDoubleClick;
  FFileBrowser.BorderStyle := bsNone;

  FFilter := TEdit.Create(Self);
  FFilter.BorderSpacing.Around := 3;
  FFilter.Parent := Self;
  FFilter.Align := alBottom;
  FFilter.OnChange := @Edit1Change;
  FFilter.TextHint := '(search)';
end;

end.

