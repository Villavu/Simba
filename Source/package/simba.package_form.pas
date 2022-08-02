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
    Label1: TLabel;
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

    procedure DoRefresh(Data: PtrInt);
    procedure DoPackageSelectionChanged(Sender: TObject; User: Boolean);
    procedure DoInstallClick(Sender: TObject);
    procedure DoAdvancedClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

var
  SimbaPackageForm: TSimbaPackageForm;

implementation

{$R *.lfm}

uses
  simba.mufasatypes, simba.package_installform, simba.helpers_string, simba.package_installer, simba.files;

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

  Thread := Threaded(@Load);
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

    Thread := Threaded(@Load);
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

  FVersionBox.BeginUpdate();
  FVersionBox.Fill(Package);
  FVersionBox.EndUpdate();
end;

procedure TSimbaPackageForm.DoInstallClick(Sender: TObject);
var
  Package: TSimbaPackage;
  Installer: TSimbaPackageInstaller;
  Options: TSimbaPackageInstallOptions;
begin
  FListBox.Enabled := False;

  try
    Package := FListBox.Selected;

    if (Package <> nil) and Package.HasVersions() then
    begin
      Installer := TSimbaPackageInstaller.Create(Package, OutputSynEdit);

      try
        if Installer.GetOptions(Package.Versions[0], Options) then
        begin
          if QuestionDlg('Install Package', 'Install "%s" to "%s" ?'.Format([Package.Info.FullName, ExtractRelativePath(GetSimbaPath(), Options.Path)]), mtConfirmation, [mrYes, mrNo], 0) = mrYes then
          begin
            InstallingButton.Caption := 'Installing...';
            InstallingButton.Enabled := False;

            PageInstalling.Show();

            Installer.Install(Package.Versions[0], Options);

            InstallingButton.Caption := 'Close';
            InstallingButton.Enabled := True;
          end;
        end else
        begin
          // no install options, advanced install
          with TSimbaPackageInstallForm.Create(Self, Package) do
          try
            ShowModal();
          finally
            Free();
          end;
        end;
      finally
        Installer.Free();
      end;
    end;

    FInfoBox.SetInfo(Package.Info.HomepageURL, Package.InstalledVersion, Package.LatestVersion);
  finally
    FListBox.Enabled := True;
  end;
end;

procedure TSimbaPackageForm.DoAdvancedClick(Sender: TObject);
begin
  if (FListBox.Selected <> nil) and FListBox.Selected.HasVersions() then
  begin
    with TSimbaPackageInstallForm.Create(Self, FListBox.Selected) do
    try
      ShowModal();
    finally
      Free();
    end;

    DoPackageSelectionChanged(Self, True); // update new visible info
  end;
end;

constructor TSimbaPackageForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FInfoBox := TPackageInfoGrid.Create(Self);
  FInfoBox.Parent := ScrollBox1;
  FInfoBox.Align := alTop;
  FInfoBox.BorderSpacing.Around := 8;

  FVersionBox := TPackageVersionGrid.Create(Self);
  FVersionBox.Parent := ScrollBox1;
  FVersionBox.BorderSpacing.Around := 8;
  FVersionBox.Anchors := [akTop, akLeft, akRight];
  FVersionBox.AnchorSideLeft.Control := ScrollBox1;
  FVersionBox.AnchorSideLeft.Side := asrLeft;
  FVersionBox.AnchorSideRight.Control := ScrollBox1;
  FVersionBox.AnchorSideRight.Side := asrRight;
  FVersionBox.AnchorSideTop.Control := FInfoBox;
  FVersionBox.AnchorSideTop.Side := asrBottom;

  FListBox := TPackageListBox.Create(Self);
  FListBox.Font.Size := 11;
  FListBox.Parent := ListPanel;
  FListBox.Align := alClient;
  FListBox.BorderSpacing.Around := 8;
  FListBox.OnSelectionChange := @DoPackageSelectionChanged;
  FListBox.OnInstallClick := @DoInstallClick;
  FListBox.OnAdvancedClick := @DoAdvancedClick;
  FListBox.ImageList := ImageList36;

  ListPanel.Height := Scale96ToScreen(250);
  Width := Scale96ToScreen(650);
  Height := Scale96ToScreen(550);

  {$IFDEF WINDOWS}
  OutputSynEdit.Font.Name := 'Consolas';
  {$ENDIF}
end;

end.

