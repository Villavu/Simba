{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.filebrowserform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Menus,
  simba.mufasatypes, simba.component_treeview;

type
  TSimbaFileBrowserNode = class(TTreeNode)
  public
    FileName: String;
    IsDirectory: Boolean;
    IsSimbaScript: Boolean;
  end;

  TSimbaFileBrowserForm = class(TForm)
  published
    PopupMenu_CopyRelativePath: TMenuItem;
    PopupMenu_CopyFullPath: TMenuItem;
    PopupMenu_Seperator1: TMenuItem;
    PopupMenu_Open: TMenuItem;
    PopupMenu_OpenExternally: TMenuItem;
    PopupMenu_Seperator2: TMenuItem;
    PopupMenu_Refresh: TMenuItem;
    Popup: TPopupMenu;

    procedure DoUpdate(Sender: TObject);
    procedure DoPopupClick(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
  protected
  type
    TFileInfo = record
      FileName: String;
      Name: String;
      IsDirectory: Boolean;
      IsSimbaScript: Boolean;
    end;

    PDirectoryInfo = ^TDirectoryInfo;
    TDirectoryInfo = record
      Info: TFileInfo;
      Files: array of TFileInfo;
      Directories: array of TDirectoryInfo;
    end;
  protected
    FTreeView: TSimbaTreeView;
    FFiles: TDirectoryInfo;
    FUpdating: Boolean;

    procedure DoFindFiles;
    procedure DoPopluateTreeView(Sender: TObject);
    function DoGetNodeHint(const Node: TTreeNode): String;
    procedure DoDoubleClick(Sender: TObject);
    procedure DoAfterFilter(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Fill;
  end;

var
  SimbaFileBrowserForm: TSimbaFileBrowserForm;

implementation

{$R *.lfm}

uses
  FileUtil, LazFileUtils, Clipbrd, TreeFilterEdit,
  simba.main, simba.scripttabsform, simba.nativeinterface;

procedure TSimbaFileBrowserForm.DoFindFiles;

  function FileInfo(const FileName: String; const IsDirectory: Boolean): TFileInfo; inline;
  begin
    Result.FileName := FileName;
    Result.Name := ExtractFileName(ExcludeTrailingPathDelimiter(FileName));
    Result.IsDirectory := IsDirectory;
    Result.IsSimbaScript := FileName.EndsWith('.simba');
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

procedure TSimbaFileBrowserForm.DoPopluateTreeView(Sender: TObject);

  function Add(ParentNode: TTreeNode; AName, AFileName: String; AIsDirectory, AIsSimbaScript: Boolean): TTreeNode;
  begin
    Result := FTreeView.AddNode(ParentNode, AName);

    with TSimbaFileBrowserNode(Result) do
    begin
      FileName := AFileName;
      IsDirectory := AIsDirectory;
      IsSimbaScript := AIsSimbaScript;

      if IsDirectory   then ImageIndex := IMAGE_DIRECTORY else
      if IsSimbaScript then ImageIndex := IMAGE_SIMBA     else
                            ImageIndex := IMAGE_FILE;

      SelectedIndex := ImageIndex;
    end;
  end;

  procedure Populate(ParentNode: TTreeNode; Dir: TDirectoryInfo);
  var
    I: Integer;
    Node: TTreeNode;
  begin
    ParentNode := Add(ParentNode, Dir.Info.Name, Dir.Info.FileName, True, False);
    for I := 0 to High(Dir.Directories) do
      Populate(ParentNode, Dir.Directories[i]);

    for I := 0 to High(Dir.Files) do
    begin
      if Dir.Files[I].IsSimbaScript then
        Add(ParentNode, Dir.Files[I].Name, Dir.Files[I].FileName, False, True)
      else
        Add(ParentNode, Dir.Files[I].Name, Dir.Files[I].FileName, False, False);
    end;
  end;

begin
  FTreeView.BeginUpdate();
  FTreeView.Clear();

  Populate(nil, FFiles);

  if Assigned(FTreeView.Items.GetFirstNode()) then
    FTreeView.Items.GetFirstNode.Expanded := True;
  FTreeView.EndUpdate();

  FFiles := Default(TDirectoryInfo);
  FUpdating := False;
end;

function TSimbaFileBrowserForm.DoGetNodeHint(const Node: TTreeNode): String;
begin
  if (Node is TSimbaFileBrowserNode) then
    Result := CreateRelativePath(TSimbaFileBrowserNode(Node).FileName, Application.Location)
  else
    Result := '';
end;

procedure TSimbaFileBrowserForm.Fill;
begin
  if FUpdating then
    Exit;
  FUpdating := True;

  TThread.ExecuteInThread(@DoFindFiles, @DoPopluateTreeView);
end;

procedure TSimbaFileBrowserForm.DoPopupClick(Sender: TObject);
var
  Node: TSimbaFileBrowserNode;
begin
  Node := TSimbaFileBrowserNode(FTreeView.Selected);

  if (Node is TSimbaFileBrowserNode) then
  begin
    if (Sender = PopupMenu_CopyFullPath) then
      Clipboard.AsText := Node.FileName
    else
    if (Sender = PopupMenu_CopyRelativePath) then
      Clipboard.AsText := CreateRelativePath(Node.FileName, Application.Location)
    else
    if (Sender = PopupMenu_Open) then
      SimbaScriptTabsForm.Open(Node.FileName)
    else
    if (Sender = PopupMenu_OpenExternally) and Node.IsDirectory then
      SimbaNativeInterface.OpenDirectory(Node.FileName)
    else
    if (Sender = PopupMenu_OpenExternally) then
      SimbaNativeInterface.OpenFile(Node.FileName);
  end;
end;

procedure TSimbaFileBrowserForm.PopupPopup(Sender: TObject);
var
  Node: TSimbaFileBrowserNode;
begin
  Node := TSimbaFileBrowserNode(FTreeView.Selected);

  if (Node is TSimbaFileBrowserNode) then
  begin
    if Node.IsDirectory then
    begin
      PopupMenu_Open.Caption := 'Open';
      PopupMenu_Open.Enabled := False;
    end else
    begin
      PopupMenu_Open.Caption := 'Open "' + Node.Text + '"';
      PopupMenu_Open.Enabled := True;
    end;

    PopupMenu_Open.Caption := 'Open "' + Node.Text + '"';
    PopupMenu_OpenExternally.Caption := 'Open Externally "' + Node.Text + '"';
  end else
  begin
    PopupMenu_Open.Caption := 'Open';
    PopupMenu_Open.Enabled := False;
    PopupMenu_OpenExternally.Caption := 'Open Externally';
    PopupMenu_OpenExternally.Enabled := False;
  end;
end;

procedure TSimbaFileBrowserForm.DoUpdate(Sender: TObject);
begin
  Fill();
end;

procedure TSimbaFileBrowserForm.DoDoubleClick(Sender: TObject);
var
  Node: TSimbaFileBrowserNode;
begin
  Node := TSimbaFileBrowserNode(FTreeView.Selected);

  if (Node is TSimbaFileBrowserNode) then
  begin
    if Node.IsDirectory then
      SimbaNativeInterface.OpenDirectory(Node.FileName)
    else
    if Node.IsSimbaScript then
      SimbaScriptTabsForm.Open(Node.FileName)
    else
      SimbaNativeInterface.OpenFile(Node.FileName);
  end;
end;

procedure TSimbaFileBrowserForm.DoAfterFilter(Sender: TObject);
begin
  //if (Sender is TTreeFilterEdit) and (TTreeFilterEdit(Sender).Filter = '') then
  //begin
  //  FTreeView.FullCollapse();
  //  FTreeView.ExpandFirstNode();
  //end;
end;

constructor TSimbaFileBrowserForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTreeView := TSimbaTreeView.Create(Self, TSimbaFileBrowserNode);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.Images := SimbaForm.Images;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;
  FTreeView.OnDoubleClick := @DoDoubleClick;
  //FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.PopupMenu := Popup;

  Fill();
end;

end.

