{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_form;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, stdctrls, extctrls, comctrls, synedit,
  simba.package, simba.package_components;

type
  TSimbaPackageForm = class(TForm)
    Bevel2: TBevel;
    Bevel3: TBevel;
    ImageList: TImageList;
    InstallingButton: TButton;
    ImageList36: TImageList;
    LoadingLabel: TLabel;
    Notebook1: TNotebook;
    BottomNotebook: TNotebook;
    Page1: TPage;
    Page2: TPage;
    PageVersions: TPage;
    PageInstalling: TPage;
    ListPanel: TPanel;
    PanelBottomMiddle: TPanel;
    PanelBottom: TPanel;
    ScrollBox1: TScrollBox;
    OutputSynEdit: TSynEdit;
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
  simba.package_installform, simba.package_installer, simba.package_autoupdater,
  simba.base, simba.dialog, simba.threading;

procedure TSimbaPackageForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@DoRefresh, 0);
end;

procedure TSimbaPackageForm.BeginLoading;
begin
  Notebook1.ShowControl(Page2);
  Application.ProcessMessages();
end;

procedure TSimbaPackageForm.EndLoading;
begin
  Notebook1.ShowControl(Page1);
  Application.ProcessMessages();
end;

procedure TSimbaPackageForm.DoAutoUpdateClicked(Sender: TObject);
begin
  if (FListBox.Selected <> nil) then
    FListBox.Selected.AutoUpdateEnabled := TCheckBox(Sender).Checked;
end;

procedure TSimbaPackageForm.DoRefresh(Data: PtrInt);
var
  Packages: TSimbaPackageArray;

  procedure Load;
  begin
    Packages := LoadPackages();
  end;

var
  Thread: TThread;
  I: Integer;
begin
  BeginLoading();

  Thread := RunInThread(@Load);
  while (not Thread.Finished) do
  begin
    Application.ProcessMessages();

    Sleep(50);
  end;
  Thread.Free();

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

  procedure Load;
  begin
    Package.Load();
  end;

var
  Thread: TThread;
  URL: String;
begin
  URL := '';

  if InputQuery('Add Package', 'Enter package URL', URL) then
  begin
    // "ollydev/moo" assume github repo
    if (not URL.Contains('.')) and URL.Contains('/') then
      URL := 'https://github.com/' + URL;

    Package := TSimbaPackage.Create(URL);

    BeginLoading();

    Thread := RunInThread(@Load);
    while (not Thread.Finished) do
    begin
      Application.ProcessMessages();

      Sleep(50);
    end;
    Thread.Free();

    if Package.Exists then
    begin
      Package.InstalledVersion := '';

      FListBox.ItemIndex := FListBox.Add(Package);
    end else
      MessageDlg('Package not found: ' + URL, mtError, [mbOK], 0);

    EndLoading();
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

  FInfoBox.SetInfo(Package.Info.HomepageURL, Package.InstalledVersion, Package.LatestVersion);
  FInfoBox.AutoUpdateChecked := Package.AutoUpdateEnabled;

  FVersionBox.BeginUpdate();
  FVersionBox.Fill(Package);
  FVersionBox.EndUpdate();
end;

function TSimbaPackageForm.SimpleInstall(Package: TSimbaPackage): Boolean;
var
  Installer: TSimbaPackageInstaller;
  Options: TSimbaPackageInstallOptions;
begin
  Result := False;

  Installer := TSimbaPackageInstaller.Create(Package, OutputSynEdit);
  if Package.HasReleases() and Installer.GetOptions(Package.Releases[0], Options) then
  begin
    OutputSynEdit.Clear();

    if SimbaQuestionDlg('Install Package', ['Install package "' + Package.Info.FullName + '" to', '"' + Options.Path + '" ?']) = ESimbaDialogResult.YES then
    begin
      InstallingButton.Caption := 'Installing...';
      InstallingButton.Enabled := False;

      PageInstalling.Show();

      Installer.Install(Package.Releases[0], Options);

      InstallingButton.Caption := 'Close';
      InstallingButton.Enabled := True;
    end;

    Result := True;
  end;
  Installer.Free();
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

  DoPackageSelectionChanged(Self, True); // update new visible info
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
      case SimbaQuestionDlg('Uninstall Package', ['Uninstall "' + Package.Info.FullName + '"', 'Do you also want to delete the files?',  '*All* files in "' + Package.InstalledPath + '" will be deleted!']) of
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

  {$IFDEF WINDOWS}
  OutputSynEdit.Font.Name := 'Consolas';
  {$ENDIF}
end;

end.

