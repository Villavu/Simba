{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_filebrowser;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, Graphics, Menus,
  simba.base, simba.component_treeview;

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
    procedure PopupMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
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

    function CanOpenInSimba(FileName: String): Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Fill;
  end;

var
  SimbaFileBrowserForm: TSimbaFileBrowserForm;

implementation

{$R *.lfm}

uses
  Clipbrd,
  simba.form_main, simba.form_tabs, simba.nativeinterface, simba.ide_utils, simba.fs;

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
    Files, Directories: TStringArray;
    I: Integer;
  begin
    Files := TSimbaDir.DirListFiles(Node^.Info.FileName);
    Directories := TSimbaDir.DirListDirectories(Node^.Info.FileName);

    SetLength(Node^.Files, Length(Files));
    SetLength(Node^.Directories, Length(Directories));

    for I := 0 to High(Directories) do
    begin
      Node^.Directories[I].Info := FileInfo(Directories[I], True);

      Build(@Node^.Directories[I]);
    end;

    for I := 0 to High(Files) do
      Node^.Files[I] := FileInfo(Files[I], False);
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

      if IsDirectory   then ImageIndex := IMG_FOLDER else
      if IsSimbaScript then ImageIndex := IMG_SIMBA  else
                            ImageIndex := IMG_FILE;

      SelectedIndex := ImageIndex;
    end;
  end;

  procedure Populate(ParentNode: TTreeNode; Dir: TDirectoryInfo);
  var
    I: Integer;
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
  if (Node is TSimbaFileBrowserNode) and (Node.Level > 1) then
    Result := TSimbaPath.PathExtractRelative(Application.Location, TSimbaFileBrowserNode(Node).FileName)
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
      Clipboard.AsText := TSimbaPath.PathExtractRelative(Application.Location, Node.FileName)
    else
    if (Sender = PopupMenu_Open) then
      SimbaTabsForm.Open(Node.FileName)
    else
    if (Sender = PopupMenu_OpenExternally) and Node.IsDirectory then
      SimbaNativeInterface.OpenDirectory(Node.FileName)
    else
    if (Sender = PopupMenu_OpenExternally) then
      SimbaNativeInterface.OpenFile(Node.FileName);
  end;
end;

procedure TSimbaFileBrowserForm.PopupMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

procedure TSimbaFileBrowserForm.PopupPopup(Sender: TObject);
var
  Node: TSimbaFileBrowserNode;
begin
  Node := TSimbaFileBrowserNode(FTreeView.Selected);

  PopupMenu_Open.Caption := 'Open';
  PopupMenu_Open.Enabled := False;
  PopupMenu_OpenExternally.Caption := 'Open Externally';
  PopupMenu_OpenExternally.Enabled := False;

  if (Node is TSimbaFileBrowserNode) then
  begin
    if CanOpenInSimba(TSimbaFileBrowserNode(Node).FileName) then
    begin
      PopupMenu_Open.Caption := 'Open "' + Node.Text + '"';
      PopupMenu_Open.Enabled := True;
    end;

    PopupMenu_Open.Caption := 'Open "' + Node.Text + '"';
    PopupMenu_OpenExternally.Caption := 'Open Externally "' + Node.Text + '"';
    PopupMenu_OpenExternally.Enabled := True;
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
    if CanOpenInSimba(Node.FileName) then
      SimbaTabsForm.Open(Node.FileName)
    else
      SimbaNativeInterface.OpenFile(Node.FileName);
  end;
end;

procedure TSimbaFileBrowserForm.DoAfterFilter(Sender: TObject);
begin
  if (FTreeView.Filter = '') then
  begin
    FTreeView.FullCollapse();
    if Assigned(FTreeView.Items.GetFirstNode()) then
      FTreeView.Items.GetFirstNode.Expanded := True;
  end;
end;

function TSimbaFileBrowserForm.CanOpenInSimba(FileName: String): Boolean;
begin
  Result := TSimbaPath.PathHasExt(FileName, ['.simba', '.txt', '.inc', '.pas', '.ini', '.md']);
end;

constructor TSimbaFileBrowserForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTreeView := TSimbaTreeView.Create(Self, TSimbaFileBrowserNode);
  FTreeView.Parent := Self;
  FTreeView.Align := alClient;
  FTreeView.Images := SimbaMainForm.Images;
  FTreeView.OnGetNodeHint := @DoGetNodeHint;
  FTreeView.OnDoubleClick := @DoDoubleClick;
  FTreeView.OnAfterFilter := @DoAfterFilter;
  FTreeView.PopupMenu := Popup;

  Fill();
end;

end.

