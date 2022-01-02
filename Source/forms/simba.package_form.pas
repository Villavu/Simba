{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.package_form;

(* .simbapackage

  // The package name to install under.
  name=SRL
  // The path to install into, from simba's exectuable location
  directory=Includes
  // flat extraction into directory
  flat=false
  // files not to install
  ignore=.git,.gitignore,.simbapackage,.gitattributes
  // scripts (files or directories) which will be added under a menu item.
  scripts=docs/docgen/docgen.simba,tests/
  // examples (files or directories) which will be added to "file > open example" form.
  examples=examples
*)

{$i simba.inc}

interface

uses
  classes, sysutils, dividerbevel, forms, controls, graphics,
  dialogs, stdctrls, buttons, buttonpanel, extctrls, menus,
  simba.mufasatypes, simba.package;

type
  TSimbaPackageForm = class(TForm)
    ReleasesList: TComboBox;
    IgnoreListEdit: TEdit;
    Images: TImageList;
    InstallButton: TButton;
    ButtonPanel: TButtonPanel;
    PackageListPopup_VistRepository: TMenuItem;
    PackageListPopup_SubmitIssue: TMenuItem;
    DisabledLabel: TLabel;
    IgnoreListLabel: TLabel;
    PackageListPopup: TPopupMenu;
    SelectDirectoryButton: TButton;
    FlatExtractionButton: TCheckBox;
    InstallDirectoryList: TComboBox;
    OptionsGroup: TGroupBox;
    InstallationDivider: TDividerBevel;
    InstallNameEdit: TEdit;
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
    // Only show package list popup if a package is selected
    procedure DoPackageListPopupClick(Sender: TObject);
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
    FProgress: String; // UpdateDownloadProgress and UpdateExtractProgress will update this.

    FUpdates: TStringList;
    FUpdateThread: TThread;

    FSuggestedPackages: TStringArray;

    procedure FontChanged(Sender: TObject); override;

    procedure SizeComponents;

    procedure UpdateProgress;
    procedure UpdateSelectedRelease;
    procedure UpdateSelectedPackage;
    procedure UpdateDownloadProgress(Sender: TObject; URL: String; APosition, ASize: Int64);
    procedure UpdateExtractProgress(Sender: TObject; FileName: String; APosition, ASize: Int64);

    procedure UpdateMenuItems;

    // Check for package updates. This is called on another thread
    procedure DoPackageUpdate;
    // Package updating has finished. This is synchronized so components are updated here.
    procedure DoPackageUpdateFinished(Sender: TObject);
    procedure DoMenuItemClick(Sender: TObject);

    function GetSelectedPackage: TSimbaPackage;
    function GetSelectedRelease: String;
  public
    property SelectedPackage: TSimbaPackage read GetSelectedPackage;
    property SelectedRelease: String read GetSelectedRelease;

    // Callback for TSimbaPackage to download a page. No progress is shown.
    function GetPageSilent(URL: String; AllowedResponseCodes: array of Integer; out ResponseCode: Integer): String;
    // Callback for TSimbaPackage to download a page. Progress is shown with Status
    function GetPage(URL: String; AllowedResponseCodes: array of Integer; out ResponseCode: Integer): String;
    // Callback for TSimbaPackage to download and extract a zip. Progress is shown with Status
    function GetZIP(URL, OutputPath: String; Flat: Boolean; IgnoreList: TStringArray): Boolean;

    // Show error dialog
    procedure Error(Message: String; Args: array of const);
    // Update status (bottom left)
    procedure Status(Message: String; Args: array of const);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaPackageForm: TSimbaPackageForm;

implementation

{$R *.lfm}

uses
  dateutils, lcltype, lclintf, lazfileutils, types,
  simba.main, simba.files, simba.httpclient_async, simba.httpclient, simba.scripttabsform;

procedure TSimbaPackageForm.DoSelectedPackageChanged(Sender: TObject; User: Boolean);
begin
  UpdateSelectedPackage();
end;

procedure TSimbaPackageForm.DoSelectedReleaseChanged(Sender: TObject);
begin
  UpdateSelectedRelease();
end;

procedure TSimbaPackageForm.DoReleasesListDraw(Control: TWinControl; Index: Integer; R: TRect; State: TOwnerDrawState);
var
  Days: Integer;
  Package: TSimbaPackage;
  ReleaseTime: TDateTime;
  ReleaseName: String;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    ReleaseName := ReleasesList.Items[Index];
    ReleaseTime := Package.Release[ReleaseName].Time;

    with ReleasesList do
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
      Canvas.TextOut(R.Left + 2, R.Top, ReleaseName);

      Inc(R.Left, Canvas.TextWidth(ReleaseName + '  '));

      Canvas.Font.Italic := True;

      if (ReleaseTime > 0) then
      begin
        case DaysBetween(Now(), ReleaseTime) of
          0: Canvas.TextOut(R.Left, R.Top, '(today)');
          1: Canvas.TextOut(R.Left, R.Top, '(yeserday)');
          else
             Canvas.TextOut(R.Left, R.Top, '(' + IntToStr(DaysBetween(Now(), ReleaseTime)) + ' days ago)');
        end;
      end else
        Canvas.TextOut(R.Left, R.Top, '(branch)');
    end;
  end;
end;

procedure TSimbaPackageForm.DoPackageListPopupClick(Sender: TObject);
var
  Package: TSimbaPackage;
begin
  Package := SelectedPackage;
  if (Package = nil) then
    Exit;

  if (Sender = PackageListPopup_SubmitIssue) then
    OpenURL(Package.IssuesURL);
  if (Sender = PackageListPopup_VistRepository) then
    OpenURL(Package.URL);
end;

procedure TSimbaPackageForm.DoAddPackageClick(Sender: TObject);
var
  URL: String;
  URLPath: TStringArray;
  Package: TSimbaPackage;
  ResponseCode: Integer;
begin
  if (Length(FSuggestedPackages) = 0) then
    FSuggestedPackages := GetPage(SIMBA_SUGGESTEDPACKAGES_URL, [HTTP_OK], ResponseCode).Trim().Split(LineEnding);

  URL := InputComboEx('Add Package', 'Enter or select package URL:', FSuggestedPackages, True).Trim([' ', '/']);
  if (URL = '') then
    Exit;

  URLPath := URL.Split('/');
  while (Length(URLPath) > 2) do
    URLPath := Copy(URLPath, 1);

  if (Length(URLPath) = 2) then // ollydev/simba
  begin
    Package := TSimbaPackage.Create(URLPath[0] + '/' + URLPath[1]);
    Package.GetPageFunction := @Self.GetPage;
    Package.GetZIPFunction := @Self.GetZIP;

    if Package.LoadReleases() then
    begin
      PackagesListBox.AddItem(Package.Name, Package);
      PackagesListBox.ItemIndex := PackagesListBox.Count - 1;

      Status('Added package %s. (%d versions)', [Package.Name, Package.Releases.Count]);
    end else
    begin
      Status('Package not found: %s', [URL]);

      Package.Free();
    end;
  end else
    Status('Invalid URL: %s', [URL]);
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

      if MessageDlg('Install Package', Message, mtConfirmation, mbYesNo, '') = mrYes then
      begin
        if Package.Install(SelectedRelease, Path, FlatExtractionButton.Checked, IgnoreListEdit.Text) then
        begin
          UpdateSelectedPackage();

          Status('Succesfully installed %s', [Package.Name]);
        end else
          Status('Failed to install %s', [Package.Name]);

        UpdateTimer.Interval := 1000;
      end;
    end else
      Self.Error('Cannot install outside of Simba''s directory', []);
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

  Packages := LoadPackages();
  Packages.FreeObjects := False;
  for I := 0 to Packages.Count - 1 do
  begin
    Packages[I].GetPageFunction := @Self.GetPage;
    Packages[I].GetZIPFunction := @Self.GetZIP;

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
      Packages[I].GetPageFunction := @Self.GetPageSilent;

      if DirectoryExists(Packages[I].InstalledVersion.Path) and (Packages[I].InstalledVersion.VersionTime > 0) then // Not a branch
      begin
        Packages[I].LoadReleases();
        if (Packages[I].Releases.Count > 1) and (Packages[I].Releases.First.Time > Packages[I].InstalledVersion.VersionTime) then
          FUpdates.Add('%s (%s) can be updated to version %s', [Packages[I].Name, Packages[I].InstalledVersion.Version, Packages[I].Releases.First.Version]);
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
var
  Size: TSize;
begin
  if (ControlCount = 0) then
    Exit;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    Size := Canvas.TextExtent('TaylorSwift');
  finally
    Free();
  end;

  ReleasesList.ItemHeight := Size.Height;

  UpdatePackageLabel.Font.Size := Self.Font.Size;
  UpdatePackageLabel.Font.Color := clNavy;
end;

procedure TSimbaPackageForm.UpdateSelectedRelease;
var
  Options: TSimbaPackage_Options;
  Package: TSimbaPackage;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Options := Package.Options[SelectedRelease];

    if Options.Loaded then
    begin
      InstallNameEdit.Text         := Options.Name;
      InstallDirectoryList.Text    := Options.Directory;
      IgnoreListEdit.Text          := Options.IgnoreList;
      FlatExtractionButton.Checked := Options.Flat;
    end else
    begin
      InstallNameEdit.Text         := Package.Name;
      InstallDirectoryList.Text    := InstallDirectoryList.Items[0];
      IgnoreListEdit.Text          := '';
      FlatExtractionButton.Checked := False;
    end;

    BasicInstallButton.Checked := Options.Loaded;
    BasicInstallButton.Enabled := Options.Loaded;
    BasicInstallButton.OnChange(BasicInstallButton);

    CustomInstallButton.Checked := not Options.Loaded;

    NotesMemo.Font.Italic := False;
    NotesMemo.Text := Package.Release[SelectedRelease].Notes;
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
    Package.LoadReleases();

    ReleasesList.Items.BeginUpdate();
    ReleasesList.Items.Clear();
    for I := 0 to Package.Releases.Count - 1 do
      ReleasesList.Items.Add(Package.Releases[I].Version);

    if (ReleasesList.Items.Count > 0) then
    begin
      LatestVersionLabel.Caption := 'Latest Version: ' + ReleasesList.Items[0];

      with Package.InstalledVersion do
        if (Version <> '') then
          InstalledVersionLabel.Caption := 'Installed Version: ' + Version
        else
          InstalledVersionLabel.Caption := 'Installed Version: Not Installed';

      UpdatePackageLabel.Hint := Format('Click to check for updates. Automatically checked hourly. (checked %d minutes ago)', [Package.CacheAge]);
    end else
      Status('Error loading releases', []);

    ReleasesList.ItemIndex := 0;
    ReleasesList.Items.EndUpdate();

    UpdateSelectedRelease();
  end;

  ReleasesPanel.Visible := PackagesListBox.ItemIndex > -1;
  InstallationPanel.Visible := PackagesListBox.ItemIndex > -1;
  DisabledLabel.Visible := PackagesListBox.ItemIndex = -1;

  if (PackagesListBox.Count > 0) then
    DisabledLabel.Caption := '(no package selected)'
  else
    DisabledLabel.Caption := '(no packages added)';
end;

procedure TSimbaPackageForm.UpdateDownloadProgress(Sender: TObject; URL: String; APosition, ASize: Int64);
begin
  if (APosition = -1) and (ASize = -1) then
    FProgress := 'Connecting...'
  else
  if (ASize > 0) then
    FProgress := Format('Downloading... (%f / %f MB)', [APosition / (1024 * 1024), ASize / (1024 * 1024)])
  else
    FProgress := Format('Downloading... (%f MB)', [APosition / (1024 * 1024)]);
end;

procedure TSimbaPackageForm.UpdateExtractProgress(Sender: TObject; FileName: String; APosition, ASize: Int64);
begin
  if (ASize > 0) then
    FProgress := Format('Extracting... (%f / %f MB)', [APosition / (1024 * 1024), ASize / (1024 * 1024)])
  else
    FProgress := Format('Extracting... (%f MB)', [APosition / (1024 * 1024)]);
end;

function TSimbaPackageForm.GetSelectedPackage: TSimbaPackage;
begin
  Result := nil;
  if (PackagesListBox.ItemIndex > -1) then
    Result := PackagesListBox.Items.Objects[PackagesListBox.ItemIndex] as TSimbaPackage;
end;

function TSimbaPackageForm.GetSelectedRelease: String;
begin
  Result := ReleasesList.Text;
end;

procedure TSimbaPackageForm.UpdateProgress;
begin
  Status(FProgress, []);

  Application.ProcessMessages();
end;

function TSimbaPackageForm.GetPageSilent(URL: String; AllowedResponseCodes: array of Integer; out ResponseCode: Integer): String;
var
  HTTPRequest: TSimbaHTTPRequest;
begin
  HTTPRequest := TSimbaHTTPRequest.Create(URL);

  try
    while HTTPRequest.Running do
      Sleep(25);

    ResponseCode := HTTPRequest.ResponseCode;

    Result := HTTPRequest.Contents;
  finally
    HTTPRequest.Free();
  end;
end;

function TSimbaPackageForm.GetPage(URL: String; AllowedResponseCodes: array of Integer; out ResponseCode: Integer): String;

  function IsAllowedResponseCode(ResponseCode: Integer): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to High(AllowedResponseCodes) do
      if ResponseCode = AllowedResponseCodes[i] then
        Exit(True);

    Exit(False);
  end;

var
  HTTPRequest: TSimbaHTTPRequest;
begin
  Result := '';

  HTTPRequest := TSimbaHTTPRequest.Create(URL, @UpdateDownloadProgress);

  try
    while HTTPRequest.Running do
    begin
      UpdateProgress();

      Sleep(25);
    end;

    Status('', []);

    ResponseCode := HTTPRequest.ResponseCode;

    if HTTPRequest.Exception <> nil then
      Self.Error('Request failed: Exception %s', [HTTPRequest.Exception.Message])
    else
    if not IsAllowedResponseCode(HTTPRequest.ResponseCode) then
      Self.Error('Request failed: HTTP error %d', [HTTPRequest.ResponseCode])
    else
    if HTTPRequest.ResponseCode = HTTP_OK then
      Result := HTTPRequest.Contents;
  finally
    HTTPRequest.Free();
  end;
end;

function TSimbaPackageForm.GetZIP(URL, OutputPath: String; Flat: Boolean; IgnoreList: TStringArray): Boolean;
var
  HTTPRequest: TSimbaHTTPRequestZIP;
begin
  InstallButton.Enabled := False;

  try
    HTTPRequest := TSimbaHTTPRequestZIP.Create(URL, OutputPath, Flat, IgnoreList, @UpdateDownloadProgress, @UpdateExtractProgress);

    try
      while HTTPRequest.Running do
      begin
        UpdateProgress();

        Sleep(25);
      end;

      Status('', []);

      if HTTPRequest.Exception <> nil then
      begin
        if HTTPRequest.Exception is EFCreateError then
          Self.Error('Install failed: %s. A file is most likely in use. Restart Simba(s) and try again.', [HTTPRequest.Exception.Message])
        else
          Self.Error('Install failed: %s', [HTTPRequest.Exception.Message]);
      end else
      if HTTPRequest.ResponseCode <> HTTP_OK then
        Self.Error('Install failed: HTTP error %d', [HTTPRequest.ResponseCode]);

      Result := HTTPRequest.ResponseCode = HTTP_OK;
    finally
      HTTPRequest.Free();
    end;
  finally
    InstallButton.Enabled := True;
  end;
end;

procedure TSimbaPackageForm.Error(Message: String; Args: array of const);
begin
  MessageDlg('Package Error', Format(Message, Args), mtError, [mbOK], 0);
end;

procedure TSimbaPackageForm.Status(Message: String; Args: array of const);
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
  ParentMenu: TMenuItem;

  procedure AddScript(PackageName, PackagePath, Script: String);
  var
    Item, ItemParent: TMenuItem;
    Scripts, Path: TStringArray;
    I: Integer;
  begin
    Script := ConcatPaths([PackagePath, Script]);

    if DirectoryExists(Script) then
      Scripts := FindFiles([Script], '*.simba')
    else
    if FileExists(Script) then
      Scripts := [Script];

    if (Length(Scripts) = 0) then
      Exit;

    for Script in Scripts do
    begin
      if (ParentMenu = nil) then
      begin
        ParentMenu := TMenuItem.Create(SimbaForm.Menu);
        ParentMenu.Caption := PackageName;
        ParentMenu.Hint := 'Simba Package';

        SimbaForm.Menu.Items.Add(ParentMenu);
      end;

      Path := CreateRelativePath(Script, PackagePath).Split(DirectorySeparator);

      ItemParent := ParentMenu;
      for I := 0 to High(Path) - 1 do
      begin
        if (ItemParent.Find(Path[I].Capitalize()) = nil) then
        begin
          Item := TMenuItem.Create(ItemParent);
          Item.Caption := Path[I].Capitalize();

          ItemParent.Add(Item);
        end;

        ItemParent := ItemParent.Find(Path[I].Capitalize());
      end;

      Item := TMenuItem.Create(ItemParent);
      Item.Caption := ExtractFileName(ExcludeTrailingPathDelimiter(Script));
      Item.Hint := Script;
      Item.OnClick := @DoMenuItemClick;

      ItemParent.Add(Item);
    end;
  end;

var
  I, J: Integer;
  Packages: TSimbaPackageList;
  Scripts: TStringArray;
begin
  for I := SimbaForm.Menu.Items.Count - 1 downto 0 do
    if SimbaForm.Menu.Items[I].Hint = 'Simba Package' then
      SimbaForm.Menu.Items[I].Free();

  Packages := LoadPackages();
  try
    for I := 0 to Packages.Count - 1 do
    begin
      ParentMenu := nil;

      Scripts := Packages[I].InstalledVersion.Scripts.Split(',');
      for J := 0 to High(Scripts) do
        AddScript(Packages[I].InstalledVersion.Name, Packages[I].InstalledVersion.Path, Scripts[J]);
    end;
  finally
    Packages.Free();
  end;
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
end;

destructor TSimbaPackageForm.Destroy;
begin
  if (FUpdates <> nil) then
    FreeAndNil(FUpdates);

  inherited Destroy();
end;

end.

