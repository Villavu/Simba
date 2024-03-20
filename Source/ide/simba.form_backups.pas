{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_backups;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Zipper,
  simba.ide_editor, simba.component_treeview, simba.component_buttonpanel;

type
  TSimbaBackupsForm = class(TForm)
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Splitter: TSplitter;
    ButtonPanel: TSimbaButtonPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SplitterPaint(Sender: TObject);
    procedure DoSplitterEnterExit(Sender: TObject);
    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoButtonOkClick(Sender: TObject);
    procedure DoCreateStream(Sender : TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure AddBackups;
  private
    TreeView: TSimbaTreeView;
    Editor: TSimbaEditor;
    CurrentNode: TTreeNode;
  end;

var
  SimbaBackupsForm: TSimbaBackupsForm;

implementation

{$R *.lfm}

uses
  AnchorDocking,
  simba.env, simba.files, simba.ide_theme, simba.form_main, simba.form_tabs;

type
  TBackupNode = class(TTreeNode)
  public
    Contents: String;
  end;

procedure TSimbaBackupsForm.FormShow(Sender: TObject);
begin
  Splitter.Width := DockMaster.SplitterWidth;
  TreeView.Clear();
  AddBackups();
end;

procedure TSimbaBackupsForm.SplitterPaint(Sender: TObject);
begin
  with TSplitter(Sender) do
  begin
    Canvas.Brush.Color := SimbaTheme.ColorFrame;
    Canvas.FillRect(ClientRect);

    if MouseInClient then
    begin
      Canvas.Brush.Color := SimbaTheme.ColorActive;
      Canvas.FillRect(3, 3, Width-3, Height-3);
    end;
  end;
end;

procedure TSimbaBackupsForm.DoSplitterEnterExit(Sender: TObject);
begin
  TSplitter(Sender).Invalidate();
end;

procedure TSimbaBackupsForm.DoTreeViewSelectionChanged(Sender: TObject);
var
  Node: TBackupNode;
begin
  Node := TBackupNode(TreeView.Selected);
  if (Node is TBackupNode) then
  begin
    Editor.Text := Node.Contents;
    Editor.Visible := True;
  end else
    Editor.Visible := False;
end;

procedure TSimbaBackupsForm.DoButtonOkClick(Sender: TObject);
begin
  if Editor.Visible then
  begin
    SimbaTabsForm.AddTab().Editor.Text := Editor.Text;
    if (Sender is TTreeView) then
      Close();
  end;
end;

procedure TSimbaBackupsForm.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TStringStream.Create();
end;

procedure TSimbaBackupsForm.DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  with TreeView.AddNode(CurrentNode, FormatDateTime('c', AItem.DateTime), IMG_FILE) as TBackupNode do
    Contents := TStringStream(AStream).DataString;

  AStream.Free();
end;

procedure TSimbaBackupsForm.AddBackups;
var
  FileName: String;
  UnZipper: TUnZipper;
begin
  for FileName in TSimbaDir.DirSearch(SimbaEnv.BackupsPath, '*.zip') do
  begin
    CurrentNode := TreeView.AddNode(TSimbaPath.PathExtractName(FileName).Replace('.zip', ''), IMG_FOLDER);

    UnZipper := TUnZipper.Create();
    try
      UnZipper.FileName := FileName;
      UnZipper.Examine();
      UnZipper.OnCreateStream := @DoCreateStream;
      UnZipper.OnDoneStream := @DoDoneStream;
      UnZipper.UnZipAllFiles();
    except
    end;
    UnZipper.Free();
  end;
end;

procedure TSimbaBackupsForm.FormCreate(Sender: TObject);
begin
  Width := Scale96ToScreen(800);
  Height := Scale96ToScreen(600);

  Editor := TSimbaEditor.Create(Self);
  Editor.Parent := RightPanel;
  Editor.Align := alClient;
  Editor.Visible := False;

  TreeView := TSimbaTreeView.Create(Self, TBackupNode);
  TreeView.Parent := LeftPanel;
  TreeView.Align := alClient;
  TreeView.OnSelectionChange := @DoTreeViewSelectionChanged;
  TreeView.OnDoubleClick := @DoButtonOkClick;

  RightPanel.Color := SimbaTheme.ColorBackground;
  RightPanel.Font.Color := SimbaTheme.ColorFont;

  ButtonPanel := TSimbaButtonPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.ButtonOk.OnClick := @DoButtonOkClick;

  Splitter.OnEnter := @DoSplitterEnterExit;
  Splitter.OnExit := @DoSplitterEnterExit;
end;

end.

