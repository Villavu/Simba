unit simba.package_form;

(* .simbapackage
  // The package name to install under. Can be empty to install directly into the directory
  name=SRL
  // The directory to install into from simba's executable location.
  directory=Includes
  // flat extraction.
  flat=false
  // Files or directories not to install delimited with commas.
  ignore=.gitignore,.simbapackage
*)

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  classes, sysutils, dividerbevel, lresources, forms, controls, graphics,
  dialogs, stdctrls, buttons, buttonpanel, extctrls, menus,
  simba.package;

// Eventually we can populate from a webpage, but this will do for now.
const
  SUGGESTED_PACKAGES: TStringArray = (
    'https://github.com/SRL/SRL',
    'https://github.com/SRL/SRL-Plugins',
    'https://github.com/SRL/SRL-Fonts'
  );

type
  TSimbaPackageForm = class(TForm)
  published
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
    FPackages: TSimbaPackageList;
    FProgress: String; // UpdateDownloadProgress and UpdateExtractProgress will update this.

    FUpdates: TStringList;
    FUpdateThread: TThread;

    procedure FontChanged(Sender: TObject); override;

    procedure UpdateProgress;
    procedure UpdateSelectedRelease;
    procedure UpdateSelectedPackage;
    procedure UpdateDownloadProgress(Sender: TObject; URL: String; APosition, ASize: Int64);
    procedure UpdateExtractProgress(Sender: TObject; FileName: String; APosition, ASize: Int64);

    // Check for package updates. This is called on another thread
    procedure DoPackageUpdate;
    // Package updating has finished. This is synchronized so components are updated here.
    procedure DoPackageUpdateFinished(Sender: TObject);

    function GetSelectedPackage: TSimbaPackage;
    function GetSelectedRelease: String;
  public
    property SelectedPackage: TSimbaPackage read GetSelectedPackage;
    property SelectedRelease: String read GetSelectedRelease;

    // Callback for TSimbaPackage to download a page. No progress is shown.
    function GetPageSilent(URL: String; AllowedResponseCodes: array of Int32; out ResponseCode: Int32): String;
    // Callback for TSimbaPackage to download a page. Progress is shown with Status
    function GetPage(URL: String; AllowedResponseCodes: array of Int32; out ResponseCode: Int32): String;
    // Callback for TSimbaPackage to download and extract a zip. Progress is shown with Status
    function GetZIP(URL, OutputPath: String; Flat: Boolean; IgnoreList: TStringArray): Boolean;

    // Write to console
    procedure Debug(Message: String; Args: array of const);
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

uses
  dateutils, lcltype, lclintf, lazfileutils,
  simba.main, simba.files, simba.httpclient_async, simba.settings, simba.httpclient, simba.package_github_releases;

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
  Days: Int32;
  Package: TSimbaPackage;
  Time: TDateTime;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    with ReleasesList do
    begin
      if (odSelected in State) then
      begin
        Canvas.Pen.Color := clHighlight;
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end else
      begin
        Canvas.Pen.Color := clWindow;
        Canvas.Brush.Color := clWindow;
        Canvas.Font.Color := clWindowText;
      end;

      Canvas.Rectangle(R);
      Canvas.Font.Italic := False;
      Canvas.TextOut(R.Left + 2, R.Top, Items[Index]);
      Canvas.Font.Italic := True;

      Time := Package.Release[Items[Index]].Time;

      if (Time > 0) then
      begin
        Days := DaysBetween(Now(), Time);

        case Days of
          0: Canvas.TextOut(R.Left + Canvas.TextWidth(Items[Index] + ' ') + 2, R.Top, '(today)');
          1: Canvas.TextOut(R.Left + Canvas.TextWidth(Items[Index] + ' ') + 2, R.Top, '(yeserday)');
          else
             Canvas.TextOut(R.Left + Canvas.TextWidth(Items[Index] + ' ') + 2, R.Top, '(' + IntToStr(Days) + ' days ago)');
        end;
      end;
    end;
  end;
end;

procedure TSimbaPackageForm.DoPackageListPopupClick(Sender: TObject);
var
  Package: TSimbaPackage;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    if (Sender = PackageListPopup_SubmitIssue)    then OpenURL(Package.IssuesURL);
    if (Sender = PackageListPopup_VistRepository) then OpenURL(Package.URL);
  end;
end;

procedure TSimbaPackageForm.DoAddPackageClick(Sender: TObject);
var
  URL: String;
  Package: TSimbaPackage;
  Releases: TGithubReleases;
begin
  URL := InputComboEx('Add Package', 'Enter or select package URL:', SUGGESTED_PACKAGES, True);

  if (Length(URL.Split(['/'])) >= 2) then // at least ollydev/simba
  begin
    Package := TSimbaPackage.Create(URL, @GetPage, @GetZIP);
    Releases := Package.Releases;

    case Length(Releases) of
      0: Status('Package not found: %s', [URL]);
      1: Status('Added package %s. (Master branch only)', [Package.Name])
      else
         Status('Added package %s. (%d releases)', [Package.Name, Length(Releases) - 1]);
    end;

    if Length(Releases) > 0 then
    begin
      FPackages.Add(Package);

      PackagesListBox.AddItem(Package.Name, Package);
      PackagesListBox.ItemIndex := PackagesListBox.Count - 1;
    end else
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
      Package.Remove();

      PackagesListBox.ItemIndex := -1;
      PackagesListBox.Items.Delete(PackagesListBox.Items.IndexOfObject(Package));

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
  Package: TSimbaPackage;
begin
  FPackages.Load(@GetPage, @GetZIP);

  PackagesListBox.Clear();
  for Package in FPackages do
    PackagesListBox.AddItem(Package.Name, Package);

  UpdateSelectedPackage();
end;

procedure TSimbaPackageForm.DoUpdateClick(Sender: TObject);
begin
  if (SelectedPackage <> nil) then
  begin
    SelectedPackage.UpdateReleases(False);

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
  i: Int32;
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
  SimbaForm.PackageButton.Hint := FUpdates.Text.Trim();

  if FUpdates.Count = 1 then
    SimbaForm.PackageButton.ImageIndex := IMAGE_PACKAGE
  else
    SimbaForm.PackageButton.ImageIndex := IMAGE_PACKAGE_NOTIFCATION;

  FUpdateThread := nil;
end;

procedure TSimbaPackageForm.DoPackageUpdate;
var
  Packages: TSimbaPackageList;
  Package: TSimbaPackage;
  Releases: TGithubReleases;
begin
  Packages := TSimbaPackageList.Create();
  Packages.Load(@Self.GetPageSilent, @Self.GetZIP);

  try
    FUpdates.Clear();
    FUpdates.Add('Simba Packages');

    for Package in Packages do
    begin
      if Package.InstalledVersion = 'master' then
        Continue;

      Releases := Package.Releases;
      if Length(Package.Releases) > 1 then // Has more than Master
        if (Releases[0].Time > Package.InstalledVersionTime) then
          FUpdates.Add('%s (%s) can be updated to version %s', [Package.Name, Package.InstalledVersion, Releases[0].Version]);
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

  if (UpdatePackageLabel <> nil) and (ReleasesList <> nil) then
  begin
    UpdatePackageLabel.Font.Color := clNavy;

    with TBitmap.Create() do
    try
      Canvas.Font := Self.Font;

      ReleasesList.ItemHeight := Canvas.TextHeight('Fj');
    finally
      Free();
    end;
  end;
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

    CustomInstallButton.Checked := not Options.Loaded;

    if (Package.Release[SelectedRelease].Notes = '') then
    begin
      NotesMemo.Text := '(no release notes)';
      NotesMemo.Font.Italic := True;
    end else
      NotesMemo.Text := Package.Release[SelectedRelease].Notes;

    NotesMemo.SelStart := 0;
  end;
end;

procedure TSimbaPackageForm.UpdateSelectedPackage;
var
  Package: TSimbaPackage;
  Rel: TGithubRelease;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    ReleasesList.Items.BeginUpdate();
    ReleasesList.Items.Clear();
    for Rel in Package.Releases do
      ReleasesList.Items.Add(Rel.Version);

    if (ReleasesList.Items.Count > 0) then
    begin
      LatestVersionLabel.Caption := 'Latest Version: ' + ReleasesList.Items[0];

      if Package.InstalledVersion <> '' then
        InstalledVersionLabel.Caption := 'Installed Version: ' + Package.InstalledVersion
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

function TSimbaPackageForm.GetPageSilent(URL: String; AllowedResponseCodes: array of Int32; out ResponseCode: Int32): String;
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

function TSimbaPackageForm.GetPage(URL: String; AllowedResponseCodes: array of Int32; out ResponseCode: Int32): String;

  function IsAllowedResponseCode(ResponseCode: Int32): Boolean;
  var
    i: Int32;
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

procedure TSimbaPackageForm.Debug(Message: String; Args: array of const);
begin
  WriteLn('[PACKAGES]: ' + Format(Message, Args));
end;

procedure TSimbaPackageForm.Error(Message: String; Args: array of const);
begin
  MessageDlg('Package Error', Format(Message, Args), mtError, [mbOK], 0);
end;

procedure TSimbaPackageForm.Status(Message: String; Args: array of const);
begin
  StatusLabel.Caption := Format(Message, Args);
end;

constructor TSimbaPackageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 800;
  Height := 650;

  InstallDirectoryList.Items.Add(CreateRelativePath(GetIncludePath(), Application.Location));
  InstallDirectoryList.Items.Add(CreateRelativePath(GetPluginPath(), Application.Location));
  InstallDirectoryList.Items.Add(CreateRelativePath(GetScriptPath(), Application.Location));
  InstallDirectoryList.Items.Add(CreateRelativePath(GetFontPath(), Application.Location));
  InstallDirectoryList.ItemIndex := 0;

  FPackages := TSimbaPackageList.Create(False);
  FUpdates := TStringList.Create();
end;

destructor TSimbaPackageForm.Destroy;
begin
  if (FPackages <> nil) then
    FPackages.Free();
  if (FUpdates <> nil) then
    FUpdates.Free();

  inherited Destroy();
end;

initialization
  {$i simba.package_form.lrs}

end.

