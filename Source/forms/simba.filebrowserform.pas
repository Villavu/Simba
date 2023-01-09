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
  extctrls, treefilteredit,
  simba.hintwindow;

type
  TFileInfo = record
    FileName: String;
    Name: String;
    ImageIndex: Integer;
    Directory: Boolean;
  end;

  PDirectoryInfo = ^TDirectoryInfo;
  TDirectoryInfo = record
    Info: TFileInfo;
    Files: array of TFileInfo;
    Directories: array of TDirectoryInfo;
  end;

  TSimbaFileBrowserNode = class(TTreeNode)
  public
    FileInfo: TFileInfo;
  end;

  TSimbaFileBrowser = class(TTreeView)
  protected
    FHint: TSimbaHintWindow;
    FFiles: TDirectoryInfo;
    FUpdating: Boolean;

    procedure DoFindFiles;
    procedure DoPopluateTreeView(Sender: TObject);
    procedure DoCreateNodeClass(var NodeClass: TTreeNodeClass); override;

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
  fileutil, clipbrd, lazfileutils,
  simba.scripttabsform, simba.main, simba.nativeinterface;

procedure TSimbaFileBrowser.DoCreateNodeClass(var NodeClass: TTreeNodeClass);
begin
  NodeClass := TSimbaFileBrowserNode;
end;

procedure TSimbaFileBrowser.DoFindFiles;

  function FileInfo(const FileName: String; const Directory: Boolean): TFileInfo; inline;
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

  procedure Build(const Node: PDirectoryInfo);
  var
    Files, Directories: TStringList;
    I: Integer;
  begin
    Files := FindAllFiles(node^.Info.FileName, '', False);
    Directories := FindAllDirectories(node^.Info.FileName, False);

    SetLength(Node^.Files, Files.Count);
    SetLength(Node^.Directories, Directories.Count);

    for I := 0 to Directories.Count - 1 do
    begin
      Node^.Directories[I].Info := FileInfo(Directories[I], True);

      Build(@Node^.Directories[I]);
    end;

    for I := 0 to Files.Count - 1 do
      Node^.Files[I] := FileInfo(Files[I], False);

    Files.Free();
    Directories.Free();
  end;

begin
  FFiles.Info := FileInfo(Application.Location, True);

  Build(@FFiles);
end;

procedure TSimbaFileBrowser.DoPopluateTreeView(Sender: TObject);

  procedure Populate(ParentNode: TTreeNode; Dir: TDirectoryInfo);
  var
    I: Integer;
  begin
    if (ParentNode = nil) then
      ParentNode := Items.Add(nil, 'Simba')
    else
      ParentNode := Items.AddChild(ParentNode, Dir.Info.Name);

    with TSimbaFileBrowserNode(ParentNode) do
    begin
      FileInfo := Dir.Info;

      SelectedIndex := FileInfo.ImageIndex;
      ImageIndex := FileInfo.ImageIndex;
    end;

    for I := 0 to High(Dir.Directories) do
      Populate(ParentNode, Dir.Directories[i]);

    for I := 0 to High(Dir.Files) do
      with TSimbaFileBrowserNode(Items.AddChild(ParentNode, Dir.Files[i].Name)) do
      begin
        FileInfo := Dir.Files[i];

        SelectedIndex := FileInfo.ImageIndex;
        ImageIndex := FileInfo.ImageIndex;
      end;
  end;

begin
  Items.BeginUpdate();
  Items.Clear();

  Populate(nil, FFiles);

  if (Items.GetFirstNode() <> nil) then
    Items.GetFirstNode().Expanded := True;
  Items.EndUpdate();

  FFiles := Default(TDirectoryInfo);
  FUpdating := False;
end;

procedure TSimbaFileBrowser.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
  FileName: String;
begin
  inherited MouseMove(Shift, X, Y);

  Node := GetNodeAt(X, Y);
  if (Node is TSimbaFileBrowserNode) then
  begin
    FileName := CreateRelativePath(TSimbaFileBrowserNode(Node).FileInfo.FileName, Application.Location);
    if (FileName = '')  then
      Exit;

    FHint.Show(Node, FileName);
  end else
    FHint.Hide();
end;

procedure TSimbaFileBrowser.Fill;
begin
  if FUpdating then
    Exit;
  FUpdating := True;

  TThread.ExecuteInThread(@DoFindFiles, @DoPopluateTreeView);
end;

constructor TSimbaFileBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHint := TSimbaHintWindow.Create(Self);
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
        SimbaNativeInterface.OpenDirectory(FileInfo.FileName)
      else
        SimbaNativeInterface.OpenFile(FileInfo.FileName);
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
  FFileBrowser.Options := FFileBrowser.Options + [tvoRightClickSelect, tvoReadOnly, tvoAutoItemHeight] - [tvoToolTips, tvoThemedDraw];
  FFileBrowser.PopupMenu := Popup;
  FFileBrowser.OnDblClick := @DoFileBrowserDoubleClick;
  FFileBrowser.BorderStyle := bsNone;
  FFileBrowser.Fill();
  FFileBrowser.TabStop := False;

  FFilter := TTreeFilterEdit.Create(Self);
  FFilter.FilteredTreeview := FFileBrowser;
  FFilter.BorderSpacing.Around := 3;
  FFilter.Parent := Self;
  FFilter.Align := alBottom;
  FFilter.OnAfterFilter := @DoAfterFilter;
  FFilter.TextHint := '(search)';
  FFilter.Spacing := 2;
  FFilter.Flat := True;
  FFilter.TabStop := False;
end;

end.

