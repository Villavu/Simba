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
  TBackups = record
    FileName: String;
    Files: array of record
      TimeStr: String;
      Contents: String;
    end;
  end;

  TSimbaBackupsForm = class(TForm)
    LeftPanel: TPanel;
    RightPanel: TPanel;
    Splitter: TSplitter;
    ButtonPanel: TSimbaButtonPanel;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RightPanelClick(Sender: TObject);
    procedure SplitterPaint(Sender: TObject);
    procedure DoSplitterEnterExit(Sender: TObject);
    procedure DoTreeViewSelectionChanged(Sender: TObject);
    procedure DoButtonOkClick(Sender: TObject);
    procedure DoCreateStream(Sender : TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  protected
    FLastUpdate: UInt64;

    FBackups: array of record
      FileName: String;
      Files: array of record
        Time: String;
        Contents: String;
      end;
    end;

    procedure DoBackupsLoaded(Sender: TObject);
    procedure DoLoadBackups;

    procedure DoFormDocked(Sender: TObject);
    procedure DoFormUndocked(Sender: TObject);

    procedure Activate; override;
    procedure Fill;
  public
    TreeView: TSimbaTreeView;
    Editor: TSimbaEditor;
  end;

var
  SimbaBackupsForm: TSimbaBackupsForm;

implementation

{$R *.lfm}

uses
  AnchorDocking,
  simba.component_button,
  simba.env, simba.fs, simba.ide_theme, simba.ide_events, simba.form_main, simba.form_tabs;

type
  TBackupNode = class(TTreeNode)
  public
    Contents: String;
  end;

procedure TSimbaBackupsForm.RightPanelClick(Sender: TObject);
begin
  Activate();
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
  if (Node is TBackupNode) and (Node.Contents <> '') then
  begin
    Editor.Text := Node.Contents;
    Editor.Visible := True;
  end else
    Editor.Visible := False;
end;

procedure TSimbaBackupsForm.DoButtonOkClick(Sender: TObject);
begin
  if Editor.Visible then
    SimbaTabsForm.AddTab().Editor.Text := Editor.Text;
end;

procedure TSimbaBackupsForm.DoCreateStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  AStream := TStringStream.Create();
end;

procedure TSimbaBackupsForm.DoDoneStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  with FBackups[High(FBackups)] do
  begin
    Files[AItem.Index].Time := FormatDateTime('c', AItem.DateTime);
    Files[AItem.Index].Contents := TStringStream(AStream).DataString;
  end;

  AStream.Free();
end;

procedure TSimbaBackupsForm.DoBackupsLoaded(Sender: TObject);
var
  I, J: Integer;
  CurrentNode: TTreeNode;
begin
  TreeView.Clear();

  for I := 0 to High(FBackups) do
  begin
    CurrentNode := TreeView.AddNode(FBackups[I].FileName, IMG_FOLDER);
    for J := 0 to High(FBackups[I].Files) do
      with TreeView.AddNode(CurrentNode, FBackups[I].Files[J].Time, IMG_FILE) as TBackupNode do
        Contents := FBackups[I].Files[J].Contents;
  end;
end;

procedure TSimbaBackupsForm.DoLoadBackups;
var
  FileName: String;
  UnZipper: TUnZipper;
begin
  FBackups := [];

  UnZipper := TUnZipper.Create();

  for FileName in TSimbaDir.DirSearch(SimbaEnv.BackupsPath, '*.zip') do
  try
    UnZipper.Clear();
    UnZipper.FileName := FileName;
    UnZipper.Examine();

    SetLength(FBackups, Length(FBackups) + 1);
    SetLength(FBackups[High(FBackups)].Files, UnZipper.Entries.Count);
    FBackups[High(FBackups)].FileName := TSimbaPath.PathExtractName(FileName).Replace('.zip', '');

    UnZipper.OnCreateStream := @DoCreateStream;
    UnZipper.OnDoneStream := @DoDoneStream;
    UnZipper.UnZipAllFiles();
  except
  end;

  UnZipper.Free();
end;

procedure TSimbaBackupsForm.DoFormDocked(Sender: TObject);
begin
  if (Sender = HostDockSite) then
  begin
    ButtonPanel.ButtonCancel.Visible := False;
    Fill();
  end;
end;

procedure TSimbaBackupsForm.DoFormUndocked(Sender: TObject);
begin
  if (Sender = HostDockSite) then
  begin
    ButtonPanel.ButtonCancel.Visible := True;
    Fill();
  end;
end;

procedure TSimbaBackupsForm.Activate;
begin
  inherited Activate();

  Fill();
end;

procedure TSimbaBackupsForm.Fill;
begin
  if ((GetTickCount64() - FLastUpdate) > 60000) then
  begin
    TThread.ExecuteInThread(@DoLoadBackups, @DoBackupsLoaded);

    FLastUpdate := GetTickCount64();
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
  Editor.ReadOnly := True;

  TreeView := TSimbaTreeView.Create(Self, TBackupNode);
  TreeView.Parent := LeftPanel;
  TreeView.Align := alClient;
  TreeView.OnSelectionChange := @DoTreeViewSelectionChanged;
  TreeView.OnDoubleClick := @DoButtonOkClick;
  TreeView.FilterOnlyTopLevel := True;
  TreeView.FilterCollapseOnClear := True;

  RightPanel.Color := SimbaTheme.ColorBackground;
  RightPanel.Font.Color := SimbaTheme.ColorFont;

  ButtonPanel := TSimbaButtonPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.ButtonOk.OnClick := @DoButtonOkClick;
  ButtonPanel.ButtonOk.Caption := 'Open';
  ButtonPanel.ButtonOk.Image := ESimbaButtonImage.NONE;
  ButtonPanel.ButtonCancel.Image := ESimbaButtonImage.NONE;
  ButtonPanel.CloseOnOk := False;

  Splitter.OnEnter := @DoSplitterEnterExit;
  Splitter.OnExit := @DoSplitterEnterExit;

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.FORM_DOCK, @DoFormDocked);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.FORM_UNDOCK, @DoFormUndocked);
end;

procedure TSimbaBackupsForm.FormShow(Sender: TObject);
begin
  Splitter.Width := DockMaster.SplitterWidth;
end;

end.

