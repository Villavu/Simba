{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_form;

(* .simbapackage

  // The directory name to install under.
  name=SRL

  // The path to install into, from simba's exectuable location.
  directory=Includes

  // flat extraction into directory.
  flat=false

  // files not to install.
  ignore=.git,.gitignore,.simbapackage,.gitattributes

  // scripts (files or directories) which will be added under a menu item.
  // can be added to a child menu item with `TheMenuName=` before the file.
  scripts=docs/docgen/docgen.simba,TestChildMenu=tests

  // examples (files or directories) which will be added to "file > open example" form.
  examples=examples

*)

{$i simba.inc}

interface

uses
  classes, sysutils, dividerbevel, forms, controls, graphics,
  dialogs, stdctrls, buttons, buttonpanel, extctrls, menus,
  simba.mufasatypes, simba.package, simba.main;

type
  TSimbaPackageForm = class(TForm)
    PackageListPanel: TPanel;
    MainPanel: TPanel;
    ReleasesList: TComboBox;
    IgnoreListEdit: TEdit;
    InstallButton: TButton;
    ButtonPanel: TButtonPanel;
    IgnoreListLabel: TLabel;
    SelectDirectoryButton: TButton;
    FlatExtractionButton: TCheckBox;
    InstallDirectoryList: TComboBox;
    OptionsGroup: TGroupBox;
    InstallationDivider: TDividerBevel;
    InstallNameEdit: TEdit;
    Splitter1: TSplitter;
    UpdateTimer: TTimer;
    UpdatePackageLabel: TLabel;
    InstallDiretoryLabel: TLabel;
    InstallNameLabel: TLabel;
    LatestVersionLabel: TLabel;
    BasicInstallButton: TRadioButton;
    CustomInstallButton: TRadioButton;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    StatusLabel: TLabel;
    ReleasesListLabel: TLabel;
    InstalledVersionLabel: TLabel;
    PackagesListBox: TListBox;
    NotesMemo: TMemo;
    ReleasesPanel: TPanel;
    InstallationPanel: TPanel;
    AddPackageButton: TSpeedButton;
    RemovePackageButton: TSpeedButton;

    // Update package components when package changes
    procedure DoSelectedPackageChanged(Sender: TObject; User: Boolean);
    // Update release components when release changes
    procedure DoSelectedReleaseChanged(Sender: TObject);
    // Append release age to the combo box item.
    procedure DoReleasesListDraw(Control: TWinControl; Index: Integer; R: TRect; State: TOwnerDrawState);
    // Add a new package. If no releases are found we add the master branch providing the repository was found
    procedure DoAddPackageClick(Sender: TObject);
    // Removes the active package. Does not delete files.
    procedure DoRemovePackageClick(Sender: TObject);
    // Installs the active package
    procedure DoInstallPackageClick(Sender: TObject);
    // Load packages on show. This is for a better interface between multiple running Simba's
    procedure DoFormShow(Sender: TObject);
    // Force update active package releases
    procedure DoUpdateClick(Sender: TObject);
    // Don't show popup menu if no item is selected
    procedure DoPackageListPopup(Sender: TObject; Mouse: TPoint; var Handled: Boolean);
    // Change active package on list right click
    procedure DoPackageListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // Enable or disable option components
    procedure DoBasicInstallButtonClick(Sender: TObject);
    // Show select directory dialog
    procedure DoSelectDirectoryClick(Sender: TObject);
    // Update installation options components based on if basic or custom install checked
    procedure DoInstallationModeChange(Sender: TObject);
    // Underline when update label is hovered
    procedure DoUpdateLabelMouseEnter(Sender: TObject);
    procedure DoUpdateLabelMouseLeave(Sender: TObject);
    // Create a thread to check for updates every 5mins
    procedure DoUpdateTimer(Sender: TObject);
  protected
    FUpdates: TStringList;
    FUpdateThread: TThread;

    FSuggestedPackages: TStringList;
    FSuggestedPackagesLoaded: Boolean;

    procedure FontChanged(Sender: TObject); override;

    procedure SizeComponents;

    procedure UpdateSelectedRelease;
    procedure UpdateSelectedPackage;
    procedure UpdateMenuItems;

    procedure DoSuggestedPackages;
    procedure DoSuggestedPackagesFinished(Sender: TObject);

    // Check for package updates. This is called on another thread
    procedure DoPackageUpdate;
    // Package updating has finished. This is synchronized so components are updated here.
    procedure DoPackageUpdateFinished(Sender: TObject);
    // Menu item was clicked. Open and run script
    procedure DoMenuItemClick(Sender: TObject);

    function GetSelectedPackage: TSimbaPackage;
    function GetSelectedRelease: TSimbaPackageRelease;
  public
    property SelectedPackage: TSimbaPackage read GetSelectedPackage;
    property SelectedRelease: TSimbaPackageRelease read GetSelectedRelease;

    // Show error dialog
    procedure Error(const Message: String);
    // Update status (bottom left)
    procedure Status(const Message: String); overload;
    procedure Status(const Message: String; Args: array of const); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaPackageForm: TSimbaPackageForm;

implementation

{$R *.lfm}

uses
  dateutils, lcltype, lazfileutils, types,
  simba.files, simba.scripttabsform, simba.httpclient, simba.package_endpoint;

procedure TSimbaPackageForm.DoSelectedPackageChanged(Sender: TObject; User: Boolean);
begin
  UpdateSelectedPackage();
end;

procedure TSimbaPackageForm.DoSelectedReleaseChanged(Sender: TObject);
begin
  UpdateSelectedRelease();
end;

procedure TSimbaPackageForm.DoReleasesListDraw(Control: TWinControl; Index: Integer; R: TRect; State: TOwnerDrawState);
begin
  if (SelectedPackage = nil) then
    Exit;

  with SelectedPackage do
  begin
    if (Index < 0) or (Index >= Releases.Count) then
      Exit;

    with ReleasesList, Releases[Index] do
    begin
      if (odSelected in State) then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end else
      begin
        Canvas.Brush.Color := clWindow;
        Canvas.Font.Color := clWindowText;
      end;

      Canvas.FillRect(R);
      Canvas.Font.Italic := False;
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(R.Left + 2, R.Top, Version);
      Canvas.Font.Italic := True;

      if Branch then
        Canvas.TextOut(R.Left + Canvas.TextWidth(Version + '  '), R.Top, '(branch)')
      else
        case DaysBetween(Now(), Time) of
          0: Canvas.TextOut(R.Left + Canvas.TextWidth(Version + '  '), R.Top, '(today)');
          1: Canvas.TextOut(R.Left + Canvas.TextWidth(Version + '  '), R.Top, '(yesterday)');
          else
             Canvas.TextOut(R.Left + Canvas.TextWidth(Version + '  '), R.Top, '(' + IntToStr(DaysBetween(Now(), Time)) + ' days ago)');
        end;
    end;
  end;
end;

procedure TSimbaPackageForm.DoAddPackageClick(Sender: TObject);
var
  URL: String;
  Package: TSimbaPackage;
begin
  if FSuggestedPackagesLoaded and (FSuggestedPackages.Count > 0) then
    URL := InputComboEx('Add Package', 'Enter or select package URL:', FSuggestedPackages, True)
  else
    URL := InputComboEx('Add Package', 'Enter package URL:', [], True);

  URL := URL.Trim([' ', '/']);
  if (URL = '') then
    Exit;

  Package := TSimbaPackage.Create(URL);
  Package.Endpoint.OnError := @Error;
  Package.Endpoint.OnStatus := @Status;

  if Package.LoadReleases(False) then // no cache when adding
  begin
    PackagesListBox.AddItem(Package.Name, Package);
    PackagesListBox.ItemIndex := PackagesListBox.Count - 1;

    Status('Added package %s. (%d versions)', [Package.Name, Package.Releases.Count]);
  end else
  begin
    Status('Package not found: %s', [URL]);

    Package.Free();
  end;
end;

procedure TSimbaPackageForm.DoRemovePackageClick(Sender: TObject);
var
  Package: TSimbaPackage;
  Message: String;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Message := Format('Continue to remove %s? The files will not be deleted.', [Package.Name]);

    if MessageDlg('Remove Package', Message, mtConfirmation, mbYesNo, '') = mrYes then
    begin
      PackagesListBox.ItemIndex := -1;
      PackagesListBox.Items.Delete(PackagesListBox.Items.IndexOfObject(Package));

      Package.Remove();
      Package.Free();

      UpdateTimer.Interval := 1000;

      Status('Removed package: %s', [Package.Name]);
    end;
  end;
end;

procedure TSimbaPackageForm.DoInstallPackageClick(Sender: TObject);
var
  Package: TSimbaPackage;
  Path, Message: String;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Path := ExpandFileName(ConcatPaths([Application.Location, InstallDirectoryList.Text, InstallNameEdit.Text]));

    if FileIsInPath(Path, Application.Location) then
    begin
      Message := Format('Continue to install %s? Files in "%s" will be permanently overwritten.', [Package.Name, Path]);

      if MessageDlg('Simba', Message, mtConfirmation, mbYesNo, '') = mrYes then
      try
        InstallButton.Enabled := False;

        if Package.Install(SelectedRelease, Path, FlatExtractionButton.Checked, IgnoreListEdit.Text) then
        begin
          UpdateSelectedPackage();

          Status('Succesfully installed %s', [Package.Name]);
        end else
          Status('Failed to install %s', [Package.Name]);

        UpdateTimer.Interval := 1000;
      finally
        InstallButton.Enabled := True;
      end;
    end else
      Self.Error('Cannot install outside of Simba''s directory');
  end;
end;

procedure TSimbaPackageForm.DoFormShow(Sender: TObject);
var
  Packages: TSimbaPackageList;
  I: Integer;
begin
  for I := 0 to PackagesListBox.Items.Count - 1 do
    PackagesListBox.Items.Objects[I].Free();
  PackagesListBox.Clear();

  Packages := LoadPackages(False);
  for I := 0 to Packages.Count - 1 do
  begin
    Packages[I].Endpoint.OnError := @Error;
    Packages[I].Endpoint.OnStatus := @Status;

    PackagesListBox.AddItem(Packages[I].Name, Packages[I]);
  end;

  UpdateSelectedPackage();
end;

procedure TSimbaPackageForm.DoUpdateClick(Sender: TObject);
begin
  if (SelectedPackage <> nil) then
  begin
    SelectedPackage.LoadReleases(False);

    UpdateSelectedPackage();
  end;
end;

procedure TSimbaPackageForm.DoSelectDirectoryClick(Sender: TObject);
begin
  SelectDirectoryDialog.InitialDir := ExpandFileName(InstallDirectoryList.Text);
  if SelectDirectoryDialog.Execute then
    InstallDirectoryList.Text := CreateRelativePath(SelectDirectoryDialog.FileName, Application.Location);
end;

procedure TSimbaPackageForm.DoBasicInstallButtonClick(Sender: TObject);
begin
  UpdateSelectedRelease();
end;

procedure TSimbaPackageForm.DoPackageListPopup(Sender: TObject; Mouse: TPoint; var Handled: Boolean);
begin
  Handled := PackagesListBox.GetIndexAtXY(Mouse.X, Mouse.Y) = -1;
end;

procedure TSimbaPackageForm.DoPackageListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (PackagesListBox.GetIndexAtXY(X, Y) > -1) then
    PackagesListBox.ItemIndex := PackagesListBox.GetIndexAtXY(X, Y);
end;

procedure TSimbaPackageForm.DoInstallationModeChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to OptionsGroup.ControlCount - 1 do
    OptionsGroup.Controls[i].Enabled := CustomInstallButton.Checked;

  OptionsGroup.Enabled := CustomInstallButton.Checked;
end;

procedure TSimbaPackageForm.DoUpdateLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := True;
end;

procedure TSimbaPackageForm.DoUpdateLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := False;
end;

procedure TSimbaPackageForm.DoPackageUpdateFinished(Sender: TObject);
begin
  SimbaForm.ToolbarButtonPackages.Hint := FUpdates.Text.Trim();

  if (FUpdates.Count = 1) then // Only master
    SimbaForm.ToolbarButtonPackages.ImageIndex := IMAGE_PACKAGE
  else
    SimbaForm.ToolbarButtonPackages.ImageIndex := IMAGE_PACKAGE_NOTIFCATION;

  UpdateMenuItems();

  FUpdateThread := nil;
end;

procedure TSimbaPackageForm.DoPackageUpdate;
var
  Packages: TSimbaPackageList;
  I: Integer;
begin
  Packages := LoadPackages();

  try
    FUpdates.Clear();
    FUpdates.Add('Simba Packages');

    for I := 0 to Packages.Count - 1 do
    begin
      if DirectoryExists(Packages[I].InstalledPath) and (Packages[I].InstalledVersionTime > 0) then // Not a branch
      begin
        Packages[I].LoadReleases(True);
        if (Packages[I].Releases.Count > 1) and (Packages[I].Releases.First.Time > Packages[I].InstalledVersionTime) then
          FUpdates.Add('%s (%s) can be updated to version %s', [Packages[I].Name, Packages[I].InstalledVersion, Packages[I].Releases.First.Version]);
      end;
    end;
  finally
    Packages.Free();
  end;
end;

procedure TSimbaPackageForm.DoUpdateTimer(Sender: TObject);
begin
  if (FUpdateThread = nil) then
    FUpdateThread := TThread.ExecuteInThread(@DoPackageUpdate, @DoPackageUpdateFinished);

  TTimer(Sender).Interval := 5 * 60000;
end;

procedure TSimbaPackageForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  SizeComponents();
end;

procedure TSimbaPackageForm.SizeComponents;
begin
  if (ControlCount = 0) then
    Exit;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    ReleasesList.ItemHeight := Canvas.TextHeight('TaylorSwift');
  finally
    Free();
  end;

  UpdatePackageLabel.Font.Size := Font.Size;
  UpdatePackageLabel.Font.Color := clNavy;
end;

procedure TSimbaPackageForm.UpdateSelectedRelease;
begin
  if (SelectedRelease = nil) then
    Exit;

  with SelectedRelease do
  begin
    if Options.IsEmpty() then
    begin
      InstallNameEdit.Text         := SelectedPackage.Name;
      InstallDirectoryList.Text    := InstallDirectoryList.Items[0];
      IgnoreListEdit.Text          := '';
      FlatExtractionButton.Checked := False;

      BasicInstallButton.Checked   := False;
      BasicInstallButton.Enabled   := False;

      CustomInstallButton.Checked  := True;
    end else
    begin
      InstallNameEdit.Text         := Options.Name;
      InstallDirectoryList.Text    := Options.Directory;
      IgnoreListEdit.Text          := Options.IgnoreList;
      FlatExtractionButton.Checked := Options.Flat;

      BasicInstallButton.Checked   := True;
      BasicInstallButton.Enabled   := True;

      CustomInstallButton.Checked  := False;
    end;

    BasicInstallButton.OnChange(BasicInstallButton);

    NotesMemo.Font.Italic := False;
    NotesMemo.Text := Notes;
    if (NotesMemo.Text = '') then
    begin
      NotesMemo.Text := '(no release notes)';
      NotesMemo.Font.Italic := True;
    end;

    NotesMemo.SelStart := 0;
  end;
end;

procedure TSimbaPackageForm.UpdateSelectedPackage;
var
  Package: TSimbaPackage;
  I: Integer;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Package.LoadReleases(True);

    ReleasesList.Items.BeginUpdate();
    ReleasesList.Items.Clear();
    for I := 0 to Package.Releases.Count - 1 do
      ReleasesList.Items.Add(Package.Releases[I].Version);

    if (ReleasesList.Items.Count > 0) then
    begin
      LatestVersionLabel.Caption := 'Latest Version: ' + ReleasesList.Items[0];

      if (Package.InstalledVersion <> '') then
        InstalledVersionLabel.Caption := 'Installed Version: ' + Package.InstalledVersion
      else
        InstalledVersionLabel.Caption := 'Installed Version: Not Installed';

      UpdatePackageLabel.Hint := Format('Click to check for updates. Automatically checked hourly. (checked %d minutes ago)', [Package.CacheAge]);
    end else
      Status('No releases found');

    ReleasesList.ItemIndex := 0;
    ReleasesList.Items.EndUpdate();

    UpdateSelectedRelease();
  end;

  ReleasesPanel.Visible := PackagesListBox.ItemIndex > -1;
  InstallationPanel.Visible := PackagesListBox.ItemIndex > -1;

  if (PackagesListBox.Count > 0) then
    MainPanel.Caption := '(no package selected)'
  else
    MainPanel.Caption := '(no packages added)';
end;

function TSimbaPackageForm.GetSelectedPackage: TSimbaPackage;
begin
  Result := nil;
  if (PackagesListBox.ItemIndex > -1) then
    Result := PackagesListBox.Items.Objects[PackagesListBox.ItemIndex] as TSimbaPackage;
end;

function TSimbaPackageForm.GetSelectedRelease: TSimbaPackageRelease;
var
  Package: TSimbaPackage;
begin
  Result := nil;

  Package := SelectedPackage;
  if (Package = nil) then
    Exit;

  if (ReleasesList.ItemIndex >= 0) and (ReleasesList.ItemIndex < Package.Releases.Count) then
    Result := Package.Releases[ReleasesList.ItemIndex];
end;

procedure TSimbaPackageForm.Error(const Message: String);
begin
  MessageDlg(Message, mtError, [mbOK], 0);
end;

procedure TSimbaPackageForm.Status(const Message: String);
begin
  StatusLabel.Caption := Message;
end;

procedure TSimbaPackageForm.Status(const Message: String; Args: array of const);
begin
  StatusLabel.Caption := Format(Message, Args);
end;

procedure TSimbaPackageForm.DoMenuItemClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.Open(TMenuItem(Sender).Hint, True) then
    SimbaForm.ToolbarButtonRun.Click()
end;

procedure TSimbaPackageForm.UpdateMenuItems;
var
  I, J: Integer;
  Packages: TSimbaPackageList;
  Scripts: TStringArray;
  PackageMenu, ItemParent, Item: TMenuItem;
begin
  for I := SimbaForm.Menu.Items.Count - 1 downto 0 do
    if (SimbaForm.Menu.Items[I].Tag = 1) then
      SimbaForm.Menu.Items[I].Free();

  Packages := LoadPackages();
  try
    for I := 0 to Packages.Count - 1 do
    begin
      Scripts := Packages[I].InstalledScripts;
      if (Length(Scripts) = 0) then
        Continue;

      PackageMenu := TMenuItem.Create(SimbaForm.Menu);
      PackageMenu.Caption := Packages[I].InstalledName;
      PackageMenu.Tag := 1;

      // Scripts array is ['menu_name', 'filename', 'menu_name', 'filename']
      for J := 0 to High(Scripts) div 2 do
      begin
        if (Scripts[J*2] <> '') then
        begin
          ItemParent := PackageMenu.Find(Scripts[J*2]);

          if (ItemParent = nil) then
          begin
            ItemParent := TMenuItem.Create(PackageMenu);
            ItemParent.Caption := Scripts[J*2];

            PackageMenu.Add(ItemParent);
          end;
        end else
          ItemParent := PackageMenu;

        Item := TMenuItem.Create(PackageMenu);
        Item.Caption := ExtractFileName(ExcludeTrailingPathDelimiter(Scripts[J*2+1]));
        Item.Hint := Scripts[J*2+1];
        Item.OnClick := @DoMenuItemClick;

        ItemParent.Add(Item);
      end;

      SimbaForm.Menu.Items.Add(PackageMenu);
    end;
  finally
    Packages.Free();
  end;
end;

procedure TSimbaPackageForm.DoSuggestedPackages;
begin
  with TSimbaHTTPClient.Create() do
  try
    FSuggestedPackages.LineBreak := #10;
    FSuggestedPackages.Text := Get(SIMBA_SUGGESTEDPACKAGES_URL).Trim();
  finally
    Free();
  end;
end;

procedure TSimbaPackageForm.DoSuggestedPackagesFinished(Sender: TObject);
begin
  FSuggestedPackagesLoaded := True;
end;

constructor TSimbaPackageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := Scale96ToScreen(800);
  Height := Scale96ToScreen(600);

  SizeComponents();

  InstallDirectoryList.Items.Add(CreateRelativePath(GetIncludePath(), Application.Location));
  InstallDirectoryList.Items.Add(CreateRelativePath(GetPluginPath(), Application.Location));
  InstallDirectoryList.Items.Add(CreateRelativePath(GetScriptPath(), Application.Location));
  InstallDirectoryList.Items.Add(CreateRelativePath(GetFontPath(), Application.Location));
  InstallDirectoryList.ItemIndex := 0;

  FUpdates := TStringList.Create();
  FSuggestedPackages := TStringList.Create();

  TThread.ExecuteInThread(@DoSuggestedPackages, @DoSuggestedPackagesFinished);
end;

destructor TSimbaPackageForm.Destroy;
begin
  if (FUpdates <> nil) then
    FreeAndNil(FUpdates);
  if (FSuggestedPackages <> nil) then
    FreeAndNil(FSuggestedPackages);

  inherited Destroy();
end;

end.

