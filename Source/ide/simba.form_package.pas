{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_package;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls, ComCtrls, SynEdit, Dialogs,
  simba.base, simba.ide_package, simba.ide_package_components;

type
  TSimbaPackageForm = class(TForm)
    Bevel3: TBevel;
    ImageList: TImageList;
    InstallingButton: TButton;
    ImageList36: TImageList;
    LoadingLabel: TLabel;
    Notebook1: TNotebook;
    BottomNotebook: TNotebook;
    MainPage: TPage;
    LoadingPage: TPage;
    PageVersions: TPage;
    PageInstalling: TPage;
    ListPanel: TPanel;
    PanelBottomMiddle: TPanel;
    PanelBottom: TPanel;
    ScrollBox1: TScrollBox;
    OutputSynEdit: TSynEdit;
    Splitter1: TSplitter;
    ToolBar: TToolBar;
    ButtonRefresh: TToolButton;
    ButtonAddRepository: TToolButton;
    ToolButton1: TToolButton;

    procedure InstallingButtonClick(Sender: TObject);
    procedure ButtonAddRepositoryClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  protected
    FListBox: TPackageListBox;
    FInfoBox: TPackageInfoGrid;
    FVersionBox: TPackageVersionGrid;

    procedure BeginLoading;
    procedure EndLoading;

    procedure DoAutoUpdateClicked(Sender: TObject);
    procedure DoRefresh(Data: PtrInt);
    procedure DoPackageSelectionChanged(Sender: TObject; User: Boolean);
    procedure DoInstallClick(Sender: TObject);
    procedure DoAdvancedClick(Sender: TObject);

    // Returns false if need advanced install
    function SimpleInstall(Package: TSimbaPackage): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaPackageForm: TSimbaPackageForm;

implementation

{$R *.lfm}

uses
  simba.form_packageinstall, simba.ide_package_installer, simba.ide_package_autoupdater,
  simba.dialog, simba.ide_utils, simba.vartype_string, simba.vartype_stringarray,
  simba.httpclient, simba.fs, simba.env, simba.misc;

procedure TSimbaPackageForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoRefresh, 0);
end;

procedure TSimbaPackageForm.BeginLoading;
begin
  Notebook1.ShowControl(LoadingPage);
  Application.ProcessMessages();
end;

procedure TSimbaPackageForm.EndLoading;
begin
  Notebook1.ShowControl(MainPage);
  Application.ProcessMessages();
end;

procedure TSimbaPackageForm.DoAutoUpdateClicked(Sender: TObject);
begin
  if (FListBox.Selected <> nil) then
    FListBox.Selected.AutoUpdateEnabled := TCheckBox(Sender).Checked;
end;

procedure TSimbaPackageForm.DoRefresh(Data: PtrInt);
var
  URLs: TStringArray;

  procedure DoLoadURLs;
  begin
    URLs := GetRemotePackageURLs() + GetLocalPackageURLs(False);
    URLs := URLs.Unique();
  end;

var
  I: Integer;
  Packages: TSimbaPackageArray;
  Procs: array of TProcedureOfObject;
begin
  BeginLoading();

  Application.RunInThreadAndWait(@DoLoadURLs);

  SetLength(Packages, Length(URLs));
  SetLength(Procs, Length(URLs));
  for I := 0 to High(URLs) do
  begin
    Packages[I] := TSimbaPackage.Create(URLs[I]);
    Procs[I] := @Packages[I].Load;
  end;

  Application.RunInThreadsAndWait(Procs);

  // reorder so updates are first, then installed, then uninstalled
  for I := 0 to High(Packages) do
    if Packages[I].IsInstalled() then
      specialize MoveElement<TSimbaPackage>(Packages, I, 0);
  for I := 0 to High(Packages) do
    if Packages[I].HasUpdate() then
      specialize MoveElement<TSimbaPackage>(Packages, I, 0);

  FListBox.Items.BeginUpdate();
  FListBox.Items.Clear();
  for I := 0 to High(Packages) do
    FListBox.Add(Packages[I]);
  FListBox.Items.EndUpdate();
  if (FListBox.Count > 0) then
    FListBox.ItemIndex := 0;

  EndLoading();
end;

procedure TSimbaPackageForm.ButtonAddRepositoryClick(Sender: TObject);
var
  Package: TSimbaPackage;

  procedure DoLoad;
  begin
    Package.Load();
  end;

var
  URL, ExceptionMsg: String;
begin
  URL := '';
  ExceptionMsg := '';

  if InputQuery('Add Package', 'Enter package URL', URL) then
  begin
    // "ollydev/moo" assume github repo
    if (not URL.Contains('.')) and URL.Contains('/') then
      URL := 'https://github.com/' + URL;

    Package := TSimbaPackage.Create(URL);

    BeginLoading();
    try
      ExceptionMsg := Application.RunInThreadAndWait(@DoLoad);

      if (ExceptionMsg = '') and (Package.EndPoint.LastHTTPStatus = EHTTPStatus.OK) then
      begin
        Package.InstalledVersion := '';

        FListBox.ItemIndex := FListBox.Add(Package);
      end else
      begin
        SimbaErrorDlg('Package error', ['Package not found: %s', 'Error: %s'], [URL, IfThen(ExceptionMsg <> '', ExceptionMsg, Package.EndPoint.LastHTTPStatus.AsString)]);

        Package.Free();
      end;
    finally
      EndLoading();
    end;
  end;
end;

procedure TSimbaPackageForm.InstallingButtonClick(Sender: TObject);
begin
  BottomNotebook.ShowControl(PageVersions);
end;

procedure TSimbaPackageForm.ButtonRefreshClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoRefresh, 0);
end;

procedure TSimbaPackageForm.DoPackageSelectionChanged(Sender: TObject; User: Boolean);
var
  Package: TSimbaPackage;
begin
  Package := FListBox.Selected;
  if (Package = nil) then
    Exit;

  PageVersions.Show();

  FInfoBox.SetInfo(Package.URL, Package.InstalledVersion, Package.LatestVersion);
  FInfoBox.AutoUpdateChecked := Package.AutoUpdateEnabled;

  FVersionBox.BeginUpdate();
  FVersionBox.Fill(Package);
  FVersionBox.EndUpdate();
end;

function TSimbaPackageForm.SimpleInstall(Package: TSimbaPackage): Boolean;
var
  Installer: TSimbaPackageInstaller;
begin
  Result := False;

  if Package.HasVersions() then
  begin
    Installer := TSimbaPackageInstaller.Create(Package, OutputSynEdit);
    try
      Installer.Version := Package.Versions[0];

      if Installer.HasRemoteInstallOpts then
      begin
        OutputSynEdit.Clear();

        if SimbaQuestionDlg('Install Package', 'Install package "%s" to "%s" ?', [Package.DisplayName, TSimbaPath.PathExtractRelative(SimbaEnv.SimbaPath, Installer.RemoteInstallOpts.Path)]) = ESimbaDialogResult.YES then
        begin
          InstallingButton.Caption := 'Installing...';
          InstallingButton.Enabled := False;

          PageInstalling.Show();

          Installer.Install(Installer.RemoteInstallOpts);

          InstallingButton.Caption := 'Close';
          InstallingButton.Enabled := True;
        end;

        Result := True;
      end;
    finally
      Installer.Free();
    end;
  end;
end;

procedure TSimbaPackageForm.DoInstallClick(Sender: TObject);
var
  Package: TSimbaPackage;
begin
  Package := FListBox.Selected;
  if (Package = nil) then
    Exit;

  FListBox.Enabled := False;

  if not SimpleInstall(Package) then
    with TSimbaPackageInstallForm.Create(Self, Package) do
    try
      ShowModal();
    finally
      Free();
    end;

  FListBox.Enabled := True;
end;

procedure TSimbaPackageForm.DoAdvancedClick(Sender: TObject);
var
  Package: TSimbaPackage;
begin
  Package := FListBox.Selected;
  if (Package = nil) then
    Exit;

  case Package.IsInstalled() of
    True:
      case SimbaQuestionDlg('Uninstall Package', ['Uninstall "%s" ?', 'Do you also want to delete the files?',  '*All* files in "%s" will be deleted.'], [Package.DisplayName, Package.InstalledPath]) of
        ESimbaDialogResult.YES: Package.UnInstall(True);
        ESimbaDialogResult.NO:  Package.UnInstall(False);
      end;

    False:
      with TSimbaPackageInstallForm.Create(Self, Package) do
      try
        ShowModal();
      finally
        Free();
      end;
  end;

  DoPackageSelectionChanged(Self, True); // update new visible info
end;

constructor TSimbaPackageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := Scale96ToScreen(650);
  Height := Scale96ToScreen(600);

  FInfoBox := TPackageInfoGrid.Create(Self);
  FInfoBox.Parent := ScrollBox1;
  FInfoBox.Align := alTop;
  FInfoBox.BorderSpacing.Around := 5;
  FInfoBox.OnAutoUpdateChange := @DoAutoUpdateClicked;

  FVersionBox := TPackageVersionGrid.Create(Self);
  FVersionBox.Parent := ScrollBox1;
  FVersionBox.BorderSpacing.Around := 5;
  FVersionBox.Anchors := [akTop, akLeft, akRight];
  FVersionBox.AnchorSideLeft.Control := ScrollBox1;
  FVersionBox.AnchorSideLeft.Side := asrLeft;
  FVersionBox.AnchorSideRight.Control := ScrollBox1;
  FVersionBox.AnchorSideRight.Side := asrRight;
  FVersionBox.AnchorSideTop.Control := FInfoBox;
  FVersionBox.AnchorSideTop.Side := asrBottom;

  FListBox := TPackageListBox.Create(Self);
  FListBox.Parent := ListPanel;
  FListBox.Align := alClient;
  FListBox.BorderSpacing.Around := 5;
  FListBox.OnSelectionChange := @DoPackageSelectionChanged;
  FListBox.OnInstallClick := @DoInstallClick;
  FListBox.OnAdvancedClick := @DoAdvancedClick;
  FListBox.ImageList := ImageList36;

  ListPanel.Height := Scale96ToScreen(260);

  OutputSynEdit.Font.Size := GetDefaultFontSize();
  {$IFDEF WINDOWS}
  OutputSynEdit.Font.Name := 'Consolas';
  {$ENDIF}
end;

end.

