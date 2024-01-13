{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Very simple downloader that parses the README here `https://github.com/Villavu/Simba-Build-Archive`
  to quickly download different version to test.
}
unit simba.downloadsimbaform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Controls, Graphics, Dialogs, ExtCtrls,
  simba.base, simba.settings,
  simba.component_treeview, simba.component_buttonpanel, simba.component_button, simba.component_edit;

type
  TSimbaDownloadSimbaForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressUpdateTimer: TTimer;
    procedure DoProgressTimerStop(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DoProgressUpdate(Sender: TObject);
    procedure DoProgressTimerStart(Sender: TObject);
  private
    FData: array of record
      Date: String;
      Branch: String;
      Commit: String;
      Link: String;
    end;
    FTreeView: TSimbaTreeView;
    FPathEdit: TSimbaEdit;
    FDownloadButton: TSimbaButton;

    FDownloadURL: String;
    FDownloadFileName: String;

    FDownloadProgress: record
      Pos, Size: Int64;
    end;

    procedure DoGetNodeColor(const Node: TTreeNode; var TheColor: TColor);

    procedure DoDownloadButtonClick(Sender: TObject);
    procedure DoSelectionChange(Sender: TObject);

    procedure DoDownloadProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
    procedure DoDownload;
    procedure DoDownloadFinished(Sender: TObject);

    procedure DoPopulate;
    procedure DoPopulated(Sender: TObject);

    procedure DoSettingChanged_FontName(Setting: TSimbaSetting);
  end;

var
  SimbaDownloadSimbaForm: TSimbaDownloadSimbaForm;

implementation

{$R *.lfm}

uses
  ATCanvasPrimitives,
  simba.httpclient, simba.theme, simba.main;

type
  TUpdateFormNode = class(TTreeNode)
  public
    Commit: String;
    DownloadURL: String;
  end;

const
  URL_BUILD_ARCHIVE_README = 'https://raw.githubusercontent.com/Villavu/Simba-Build-Archive/main/README.md';

procedure TSimbaDownloadSimbaForm.FormCreate(Sender: TObject);
begin
  Color := SimbaTheme.ColorBackground;
  Width := Scale96ToScreen(750);
  Height := Scale96ToScreen(450);

  FTreeView := TSimbaTreeView.Create(Self, TUpdateFormNode);
  FTreeView.Parent := Panel1;
  FTreeView.Align := alClient;
  FTreeView.FilterVisible := False;
  FTreeView.OnSelectionChange := @DoSelectionChange;
  if (SIMBA_COMMIT <> '') then
    FTreeView.OnGetNodeColor := @DoGetNodeColor;

  FDownloadButton := TSimbaButton.Create(Self);
  FDownloadButton.Parent := Panel2;
  FDownloadButton.Align := alLeft;
  FDownloadButton.Caption := 'Download selected';
  FDownloadButton.OnClick := @DoDownloadButtonClick;

  FPathEdit := TSimbaEdit.Create(Self);
  FPathEdit.Parent := Panel2;
  FPathEdit.Align := alClient;
  FPathEdit.BorderSpacing.Left := 10;
  FPathEdit.ColorBorder := SimbaTheme.ColorLine;

  with TSimbaButtonPanel.Create(Self) do
  begin
    Parent := Self;
    ButtonCancel.Visible := False;
  end;

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.Editor.FontName, @DoSettingChanged_FontName, True);
end;

procedure TSimbaDownloadSimbaForm.FormResize(Sender: TObject);
begin
  FDownloadButton.Constraints.MinWidth := Width div 3;
end;

procedure TSimbaDownloadSimbaForm.DoProgressTimerStart(Sender: TObject);
begin
  FDownloadButton.Enabled := False;
  FDownloadButton.Caption := 'Downloading';

  FDownloadProgress.Pos := 0;
  FDownloadProgress.Size := 0;
end;

procedure TSimbaDownloadSimbaForm.DoGetNodeColor(const Node: TTreeNode; var TheColor: TColor);
begin
  if (TUpdateFormNode(Node).Commit.StartsWith(SIMBA_COMMIT)) then
    TheColor := ColorBlend(TheColor, clPurple, 210);
end;

procedure TSimbaDownloadSimbaForm.DoProgressTimerStop(Sender: TObject);
begin
  FDownloadButton.Caption := 'Download';
  FDownloadButton.Enabled := True;
end;

procedure TSimbaDownloadSimbaForm.FormShow(Sender: TObject);
begin
  TThread.ExecuteInThread(@DoPopulate, @DoPopulated);
end;

procedure TSimbaDownloadSimbaForm.DoProgressUpdate(Sender: TObject);
begin
  with FDownloadProgress do
    if (Size > 0) then
      FDownloadButton.Caption := 'Downloading: %f / %f MB'.Format([Pos / (1024 * 1024), Size / (1024 * 1024)])
    else
      FDownloadButton.Caption := 'Downloading: %f MB'.Format([Pos / (1024 * 1024)]);
end;

procedure TSimbaDownloadSimbaForm.DoDownloadButtonClick(Sender: TObject);
var
  Node: TUpdateFormNode;
begin
  Node := TUpdateFormNode(FTreeView.Selected);
  if (Node <> nil) and (Node.DownloadURL <> '') then
  begin
    ProgressUpdateTimer.Enabled := True;
    FDownloadURL := Node.DownloadURL;
    FDownloadFileName := FPathEdit.Text;

    TThread.ExecuteInThread(@DoDownload, @DoDownloadFinished);
  end;
end;

procedure TSimbaDownloadSimbaForm.DoSelectionChange(Sender: TObject);
var
  Node: TUpdateFormNode;
begin
  Node := TUpdateFormNode(FTreeView.Selected);
  if (Node <> nil) and (Node.DownloadURL <> '') then
    FPathEdit.Text := Application.Location + Copy(Node.Commit, 1, 8) + '_' + Node.Text;
end;

procedure TSimbaDownloadSimbaForm.DoDownloadProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
begin
  FDownloadProgress.Pos := Pos;
  FDownloadProgress.Size := Size;
end;

procedure TSimbaDownloadSimbaForm.DoDownload;
begin
  try
    with TSimbaHTTPClient.Create() do
    try
      OnDownloadProgress := @DoDownloadProgress;

      GetFile(FDownloadURL, FDownloadFileName, [EHTTPStatus.OK]);
    finally
      Free();
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TSimbaDownloadSimbaForm.DoDownloadFinished(Sender: TObject);
begin
  ProgressUpdateTimer.Enabled := False;
end;

procedure TSimbaDownloadSimbaForm.DoPopulate;
var
  Lines: TStringArray;
  str: TStringArray;
  I: Integer;
  Count: Integer;
begin
  Lines := TSimbaHTTPClient.SimpleGet(URL_BUILD_ARCHIVE_README, []).Split(#10);

  SetLength(FData, Length(Lines));
  Count := 0;

  for I := 5 to High(Lines) do
  begin
    str := Lines[I].Split(' | ');
    if Length(Str) = 4 then
    begin
      Str[2] := Str[2].Between('[', ']');
      Str[3] := Str[3].Between('(', ')');

      with FData[Count] do
      begin
        Date := Str[0];
        Branch := Str[1];
        Commit := Str[2];
        Link := Str[3];
      end;
      Inc(Count);
    end;
  end;

  SetLength(FData, Count);
end;

procedure TSimbaDownloadSimbaForm.DoPopulated(Sender: TObject);

  function Add(ParentNode: TTreeNode; Text: String; DataIndex: Integer; ImageIndex: Integer = -1): TUpdateFormNode;
  begin
    Result := TUpdateFormNode(FTreeView.AddNode(ParentNode, Text, ImageIndex));
    Result.Commit := FData[DataIndex].Commit;
    if (ImageIndex = IMG_SIMBA) then
      Result.DownloadURL := FData[DataIndex].Link.Replace('/tree/', '/raw/') + '/' + Text;
  end;

var
  I:Integer;
  Node: TTreeNode;
begin
  FTreeView.BeginUpdate();
  FTreeView.Clear();
  for I := 0 to High(FData) do
  begin
    Node := Add(nil, FData[I].Date + ' | ' + FData[I].Commit + ' | ' + FData[I].Branch, I);

    Add(Node, 'Simba-Win32.exe', I, IMG_SIMBA);
    Add(Node, 'Simba-Win64.exe', I, IMG_SIMBA);
    Add(Node, 'Simba-Linux-AArch64', I, IMG_SIMBA);
    Add(Node, 'Simba-Linux64', I, IMG_SIMBA);
    Add(Node, 'Simba-MacOS-AArch64.dmg', I, IMG_SIMBA);
    Add(Node, 'Simba-MacOS.dmg', I, IMG_SIMBA);
  end;
  FTreeView.EndUpdate();
  if (FTreeView.Items.Count > 0) then
    FTreeView.Items[0].Expanded := True;
end;

procedure TSimbaDownloadSimbaForm.DoSettingChanged_FontName(Setting: TSimbaSetting);
begin
  FTreeView.Font.Name := Setting.Value;
end;

end.

