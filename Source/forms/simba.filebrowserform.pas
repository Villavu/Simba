{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.filebrowserform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, comctrls, menus,
  extctrls, treefilteredit, gtree,
  simba.hintwindow;

type
  TFileInfo = record
    FileName: String;
    Name: String;
    Directory: Boolean;
    ImageIndex: Integer;
  end;

  TSimbaFileBrowserNode = class(TTreeNode)
  public
    FileInfo: TFileInfo;
  end;

  TSimbaFileBrowser = class(TTreeView)
  protected
  type
    TFileTree = specialize TTree<TFileInfo>;
    TFileTreeNode = TFileTree.TTreeNodeType;
  protected
    FRoot: String;
    FHint: TSimbaHintWindow;
    FFileTree: TFileTree;

    procedure DoBuildFileTree;
    procedure DoFill(Sender: TObject);
    procedure DoCreateNodeClass(var NodeClass: TTreeNodeClass); override;

    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Fill;

    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaFileBrowserForm = class(TForm)
    PopupMenu_CopyRelativePath: TMenuItem;
    PopupMenu_CopyPath: TMenuItem;
    PopupMenu_Seperator1: TMenuItem;
    PopupMenu_Open: TMenuItem;
    PopupMenu_OpenExternally: TMenuItem;
    PopupMenu_Seperator2: TMenuItem;
    PopupMenu_Refresh: TMenuItem;
    Popup: TPopupMenu;

    procedure PopupItemClick(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
  protected
    FFileBrowser: TSimbaFileBrowser;
    FFilter: TTreeFilterEdit;

    procedure DoFileBrowserDoubleClick(Sender: TObject);
    procedure DoAfterFilter(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaFileBrowserForm: TSimbaFileBrowserForm;

implementation

{$R *.lfm}

uses
  fileutil, clipbrd, lclintf, lazfileutils,
  simba.misc, simba.scripttabsform, simba.main;

procedure TSimbaFileBrowser.DoCreateNodeClass(var NodeClass: TTreeNodeClass);
begin
  NodeClass := TSimbaFileBrowserNode;
end;

procedure TSimbaFileBrowser.DoBuildFileTree;

  function CreateFileInfo(FileName: String; Directory: Boolean): TFileInfo; inline;
  begin
    Result.FileName := FileName;
    Result.Directory := Directory;
    Result.Name := ExtractFileName(ExcludeTrailingPathDelimiter(FileName));

    if Directory then
      Result.ImageIndex := IMAGE_DIRECTORY
    else
    if ExtractFileExt(FileName) = '.simba' then
      Result.ImageIndex := IMAGE_SIMBA
    else
      Result.ImageIndex := IMAGE_FILE;
  end;

  procedure BuildFileTree(parent: TFileTreeNode; Directory: String);
  var
    Rec: TSearchRec;
    Node: TFileTreeNode;
  begin
    if (Directory = FRoot) then
      Node := FFileTree.Root
    else
    begin
      Node := TFileTreeNode.Create(CreateFileInfo(Directory, True));

      Parent.Children.Insert(0, Node);
    end;

    if (FindFirst(Directory + '*', faAnyFile, Rec) = 0) then
    try
      repeat
        if (Rec.Attr and faDirectory <> 0) then
        begin
          if (Rec.Name <> '.') and (Rec.Name <> '..') then
            BuildFileTree(Node, Directory + Rec.Name + DirectorySeparator);
        end else
          Node.Children.PushBack(TFileTreeNode.Create(CreateFileInfo(Directory + Rec.Name, False)));
      until (FindNext(Rec) <> 0);
    finally
      FindClose(Rec);
    end;
  end;

begin
  FFileTree.Root := TFileTreeNode.Create(CreateFileInfo(FRoot, True));

  BuildFileTree(FFileTree.Root, FRoot);
end;

procedure TSimbaFileBrowser.DoFill(Sender: TObject);
var
  RootNode: TTreeNode;

  procedure Populate(ParentNode: TTreeNode; Node: TFileTreeNode);
  var
    I: Integer;
  begin
    if Node.Data.Directory then
    begin
      ParentNode := Items.AddChild(ParentNode, Node.Data.Name);

      with TSimbaFileBrowserNode(ParentNode) do
      begin
        FileInfo := Node.Data;

        SelectedIndex := Node.Data.ImageIndex;
        ImageIndex := Node.Data.ImageIndex;
      end;
    end else
    begin
      with TSimbaFileBrowserNode(Items.AddChild(ParentNode, Node.Data.Name)) do
      begin
        FileInfo := Node.Data;

        SelectedIndex := Node.Data.ImageIndex;
        ImageIndex := Node.Data.ImageIndex;
      end;
    end;

    for I := 0 to Node.Children.Size - 1 do
      Populate(ParentNode, Node.Children[I]);
  end;

var
  Node: TFileTreeNode;
begin
  Assert(GetCurrentThreadID() = MainThreadID);
  Assert(FFileTree <> nil);

  Items.BeginUpdate();
  Items.Clear();

  if (FFileTree.Root <> nil) then
  begin
    RootNode := Items.Add(nil, FFileTree.Root.Data.Name);
    RootNode.SelectedIndex := FFileTree.Root.Data.ImageIndex;
    RootNode.ImageIndex := FFileTree.Root.Data.ImageIndex;

    for Node in FFileTree.Root.Children do
      Populate(RootNode, Node);

    RootNode.Expanded := True;
  end;

  Items.EndUpdate();

  FFileTree.Free();
  FFileTree := nil;
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
  if (Node is TSimbaFileBrowserNode) then
  begin
    FileName := CreateRelativePath(TSimbaFileBrowserNode(Node).FileInfo.FileName, Application.Location);
    if (FileName = '') or (FHint.Visible and (FHint.Caption = FileName)) then
      Exit;

    R := Node.DisplayRect(True);
    R.TopLeft := ClientToScreen(R.TopLeft);
    R.BottomRight := ClientToScreen(R.BottomRight);

    FHint.ActivateHint(R, FileName);
  end else
    FHint.Visible := False;
end;

procedure TSimbaFileBrowser.Fill;
begin
  Assert(GetCurrentThreadID() = MainThreadID);

  if (FFileTree = nil) then
  begin
    FFileTree := TFileTree.Create();

    TThread.ExecuteInThread(@DoBuildFileTree, @DoFill);
  end;
end;

constructor TSimbaFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHint := TSimbaHintWindow.Create(Self);
  FRoot := Application.Location;
end;

procedure TSimbaFileBrowserForm.PopupItemClick(Sender: TObject);
var
  FileInfo: TFileInfo;
begin
  if (FFileBrowser.Selected is TSimbaFileBrowserNode) then
  begin
    FileInfo := TSimbaFileBrowserNode(FFileBrowser.Selected).FileInfo;

    if (Sender = PopupMenu_Open) then
      SimbaScriptTabsForm.Open(FileInfo.FileName)
    else
    if (Sender = PopupMenu_OpenExternally) then
    begin
      if FileInfo.Directory then
        OpenDirectory(FileInfo.FileName)
      else
        OpenDocument(FileInfo.FileName);
    end else
    if (Sender = PopupMenu_CopyPath) then
      Clipboard.AsText := FileInfo.FileName
    else
    if (Sender = PopupMenu_CopyRelativePath) then
      Clipboard.AsText := CreateRelativePath(FileInfo.FileName, Application.Location);
  end;

  if (Sender = PopupMenu_Refresh) then
  begin
    FFilter.ResetFilter();
    FFileBrowser.Fill();
  end;
end;

procedure TSimbaFileBrowserForm.PopupPopup(Sender: TObject);
var
  FileInfo: TFileInfo;
begin
  if (FFileBrowser.Selected is TSimbaFileBrowserNode) then
  begin
    FileInfo := TSimbaFileBrowserNode(FFileBrowser.Selected).FileInfo;

    PopupMenu_Open.Enabled := not FileInfo.Directory;
    PopupMenu_OpenExternally.Enabled := True;
  end else
  begin
    PopupMenu_Open.Enabled := False;
    PopupMenu_OpenExternally.Enabled := False;
  end;
end;

procedure TSimbaFileBrowserForm.DoFileBrowserDoubleClick(Sender: TObject);
var
  FileInfo: TFileInfo;
begin
  if (FFileBrowser.Selected is TSimbaFileBrowserNode) then
  begin
    FileInfo := TSimbaFileBrowserNode(FFileBrowser.Selected).FileInfo;
    if (not FileInfo.Directory) then
      SimbaScriptTabsForm.Open(FileInfo.FileName);
  end;
end;

procedure TSimbaFileBrowserForm.DoAfterFilter(Sender: TObject);
begin
  if (FFilter.Filter <> '') then
    FFileBrowser.FullExpand()
  else
  begin
    FFileBrowser.FullCollapse();
    if (FFileBrowser.Items.GetFirstNode() <> nil) then
      FFileBrowser.Items.GetFirstNode().Expanded := True;
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
  FFileBrowser.Images := SimbaForm.Images;
  FFileBrowser.Options := FFileBrowser.Options + [tvoRightClickSelect, tvoReadOnly, tvoAutoItemHeight] - [tvoToolTips];
  FFileBrowser.PopupMenu := Popup;
  FFileBrowser.OnDblClick := @DoFileBrowserDoubleClick;
  FFileBrowser.BorderStyle := bsNone;
  FFileBrowser.Fill();

  FFilter := TTreeFilterEdit.Create(Self);
  FFilter.FilteredTreeview := FFileBrowser;
  FFilter.BorderSpacing.Around := 3;
  FFilter.Parent := Self;
  FFilter.Align := alBottom;
  FFilter.OnAfterFilter := @DoAfterFilter;
  FFilter.TextHint := '(search)';
  FFilter.Spacing := 2;
  FFilter.Flat := True;
end;

end.

