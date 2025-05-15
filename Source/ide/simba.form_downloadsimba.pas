{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Very simple downloader that parses the README here `https://github.com/Villavu/Simba-Build-Archive`
  to quickly download different version to test.
}
unit simba.form_downloadsimba;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Zipper, syncobjs,
  simba.base,
  simba.settings,
  simba.httpclient,
  simba.component_treeview,
  simba.component_buttonpanel;

type
  TDownloaderFormNode = class(TTreeNode)
  public
    Commit: String;
    DownloadURL: String;
  end;

  TDownloader = class
  protected
    FUnZipper: TUnZipper;
    FHttpClient: TSimbaHTTPClient;
    FData: TMemoryStream;
    FURL: String;
    FCommit: String;
    FFile: String;
    FError: String;

    FStatusLock: TCriticalSection;
    FStatusTimer: TTimer;
    FStatusLabel: TLabel;
    FStatus: String;

    procedure DoStatusTimer(Sender: TObject);
    procedure DoDownloadProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
    procedure DoUnZipperInput(Sender: TObject; var Stream: TStream);
    procedure DoUnZipOutput(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);

    procedure Execute;
    procedure Finished(Sender: TObject);
  public
    constructor Create(Node: TDownloaderFormNode; StatusLabel: TLabel); // non blocking - run on another thread
    destructor Destroy; override; // automatically called once thread is finished
  end;

  TSimbaDownloadSimbaForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Notebook1: TNotebook;
    MainPage: TPage;
    ErrorPage: TPage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label6MouseEnter(Sender: TObject);
    procedure Label6MouseLeave(Sender: TObject);
  private
    FData: array of record
      Date: String;
      Branch: String;
      Commit: String;
      Link: String;
      Downloads: TStringArray;
    end;
    FTreeView: TSimbaTreeView;
    FStatusLabel: TLabel;

    procedure DoGetNodeColor(Node: TTreeNode; var TheColor: TColor);
    procedure DoTreeDoubleClick(Sender: TObject);

    procedure DoPopulate;
    procedure DoPopulated(Sender: TObject);
  end;

var
  SimbaDownloadSimbaForm: TSimbaDownloadSimbaForm;

implementation

uses
  ATCanvasPrimitives,
  simba.ide_theme,
  simba.form_main,
  simba.vartype_string,
  simba.fs,
  simba.nativeinterface;

procedure TDownloader.DoStatusTimer(Sender: TObject);
begin
  FStatusLock.Enter();
  FStatusLabel.Caption := FStatus;
  FStatusLock.Leave();
end;

procedure TDownloader.DoDownloadProgress(Sender: TObject; URL, ContentType: String; Pos, Size: Int64);
begin
  if (Size > 0) then
    FStatus := 'Downloading: %f / %f MB'.Format([Pos / (1024 * 1024), Size / (1024 * 1024)])
  else
    FStatus := 'Downloading: %f MB'.Format([Pos / (1024 * 1024)]);
end;

procedure TDownloader.DoUnZipperInput(Sender: TObject; var Stream: TStream);
begin
  FData.Position := 0;

  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FData, 0);
end;

procedure TDownloader.DoUnZipOutput(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
begin
  FFile := Application.Location + TSimbaPath.PathExtractNameWithoutExt(AItem.ArchiveFileName) + '_' + FCommit + TSimbaPath.PathExtractExt(AItem.ArchiveFileName);
  if FileExists(FFile) then
    AStream := TFileStream.Create(FFile, fmOpenReadWrite)
  else
    AStream := TFileStream.Create(FFile, fmCreate);
end;

procedure TDownloader.Execute;
begin
  try
    if FHttpClient.Get(FURL, FData) > 0 then
      FUnZipper.UnZipAllFiles()
    else
      FError := 'No data received';
  except
    on E: Exception do
      FError := E.Message;
  end;
end;

procedure TDownloader.Finished(Sender: TObject);
begin
  FStatusTimer.Enabled := False;
  if (FError <> '') then
    FStatusLabel.Caption := 'Error: ' + FError
  else
    FStatusLabel.Caption := 'Downloaded: ' + FFile;

  Self.Free();
end;

constructor TDownloader.Create(Node: TDownloaderFormNode; StatusLabel: TLabel);
begin
  inherited Create();

  FStatusLabel := StatusLabel;
  FStatusLock := TCriticalSection.Create();
  FStatusTimer := TTimer.Create(nil);
  FStatusTimer.Interval := 500;
  FStatusTimer.OnTimer := @DoStatusTimer;

  FData := TMemoryStream.Create();
  FURL := Node.DownloadURL;
  FCommit := Node.Commit;

  FHttpClient := TSimbaHTTPClient.Create();
  FHttpClient.OnDownloadProgress := @DoDownloadProgress;

  FUnZipper := TUnZipper.Create();
  FUnZipper.OnOpenInputStream := @DoUnZipperInput;
  FUnZipper.OnCreateStream := @DoUnZipOutput;

  TThread.ExecuteInThread(@Execute, @Finished);
end;

destructor TDownloader.Destroy;
begin
  FStatusLock.Free();
  FStatusTimer.Free();
  FData.Free();

  FUnZipper.Free();
  FHttpClient.Free();

  inherited Destroy();
end;

procedure TSimbaDownloadSimbaForm.FormCreate(Sender: TObject);
var
  ButtonPanel: TSimbaButtonPanel;
  Control: TControl;
begin
  Color := SimbaTheme.ColorBackground;
  Width := Scale96ToScreen(750);
  Height := Scale96ToScreen(450);

  FTreeView := TSimbaTreeView.Create(Self, TDownloaderFormNode);
  FTreeView.Parent := Mainpage;
  FTreeView.Align := alClient;
  FTreeView.FilterVisible := False;
  FTreeView.OnDoubleClick := @DoTreeDoubleClick;
  FTreeView.Images := SimbaMainForm.Images;
  if (SIMBA_COMMIT <> '') then
    {%H-}FTreeView.OnGetNodeColor := @DoGetNodeColor;

  ButtonPanel := TSimbaButtonPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.ButtonCancel.Visible := False;

  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := ButtonPanel;
  FStatusLabel.Align := alLeft;
  FStatusLabel.Layout := tlCenter;
  FStatusLabel.AutoSize := True;
  FStatusLabel.BorderSpacing.Left := 5;
  FStatusLabel.Font.Color := SimbaTheme.ColorFont;
  FStatusLabel.Caption := 'Double click on an item to download it!';

  MainPage.Color := SimbaTheme.ColorBackground;
  ErrorPage.Color := SimbaTheme.ColorBackground;
  for Control in ErrorPage.GetEnumeratorControls() do
    Control.Font.Color := SimbaTheme.ColorFont;
  Label5.Font.Color := $FFE385;
  Label6.Font.Color := $FFE385;

  Notebook1.PageIndex := 0;
end;

procedure TSimbaDownloadSimbaForm.DoGetNodeColor(Node: TTreeNode; var TheColor: TColor);
begin
  if (TDownloaderFormNode(Node).Commit.StartsWith(SIMBA_COMMIT)) then
    TheColor := ColorBlend(TheColor, clPurple, 200);
end;

procedure TSimbaDownloadSimbaForm.FormShow(Sender: TObject);
begin
  TThread.ExecuteInThread(@DoPopulate, @DoPopulated);
end;

procedure TSimbaDownloadSimbaForm.Label6Click(Sender: TObject);
begin
  SimbaNativeInterface.OpenURL(TLabel(Sender).Caption);
end;

procedure TSimbaDownloadSimbaForm.Label6MouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := True;
end;

procedure TSimbaDownloadSimbaForm.Label6MouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := False;
end;

procedure TSimbaDownloadSimbaForm.DoTreeDoubleClick(Sender: TObject);
var
  Node: TDownloaderFormNode;
begin
  Node := TDownloaderFormNode(FTreeView.Selected);
  if (Node <> nil) and (Node.DownloadURL <> '') then
    TDownloader.Create(Node, FStatusLabel);
end;

procedure TSimbaDownloadSimbaForm.DoPopulate;
var
  Count: Integer = 0;

  function Valid(Index: Integer): Boolean;
  begin
    with FData[Index] do
      Result := (Date <> '') and (Branch <> '') and (Length(Commit) >= 8) and (Length(Downloads) > 0);
  end;

  procedure Add(Date, Branch, Commit, Downloads: String);
  begin
    FData[Count].Date := Date;
    FData[Count].Branch := Branch;
    FData[Count].Commit := Commit.Between('[', ']');
    FData[Count].Downloads := Downloads.BetweenAll('(', ')');
    if Valid(Count) then
      Inc(Count);
  end;

var
  Lines, Args: TStringArray;
  I: Integer;
begin
  Lines := URLFetch('https://raw.githubusercontent.com/Villavu/Simba-Build-Archive/main/README.md').SplitLines();

  if (Length(Lines) > 0) then
  begin
    SetLength(FData, Length(Lines));
    for I := 6 to High(Lines) do
    begin
      Args := Lines[I].Split(' | ');
      if (Length(Args) = 4) then
        Add(Args[0], Args[1], Args[2], Args[3]);
    end;
    SetLength(FData, Count);
  end;
end;

procedure TSimbaDownloadSimbaForm.DoPopulated(Sender: TObject);

  function AddDownloadNode(ParentNode: TTreeNode; Download, Commit: String): TDownloaderFormNode;
  begin
    Result := TDownloaderFormNode(FTreeView.AddNode(ParentNode, TSimbaPath.PathExtractNameWithoutExt(Download).Replace('%20', ' '), IMG_SIMBA));
    Result.DownloadURL := 'https://github.com/Villavu/Simba-Build-Archive/blob/main' + Download;
    Result.Commit := Commit;
  end;

var
  I: Integer;
  Download: String;
  Node: TTreeNode;
begin
  if (Length(FData) = 0) then
  begin
    Notebook1.PageIndex := 1;
    Exit;
  end;

  FTreeView.BeginUpdate();
  FTreeView.Clear();
  for I := 0 to High(FData) do
  begin
    Node := FTreeView.AddNode(FData[I].Date + ' | ' + FData[I].Branch + ' | ' + FData[I].Commit);
    for Download in FData[i].Downloads do
      AddDownloadNode(Node, Download, FData[I].Commit);
  end;
  FTreeView.EndUpdate();
  if (FTreeView.Items.Count > 0) then
    FTreeView.Items[0].Expanded := True;
end;

{$R *.lfm}

end.

