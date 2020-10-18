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

interface

uses
  classes, sysutils, dividerbevel, lresources, forms, controls, graphics,
  dialogs, stdctrls, buttons, buttonpanel, extctrls, comctrls, menus,
  simba.package, types;

// Eventually we can populate from a webpage, but this will do for now.
const
  SUGGESTED_PACKAGES: array[0..2] of String = (
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
    installationDivider: TDividerBevel;
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
    procedure HandePackageListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure InstallationModeChangedUser(Sender: TObject);
    procedure SelectDirectoryButtonClick(Sender: TObject);

    // Update installation options components based on if basic or custom install checked
    procedure DoInstallationModeChange(Sender: TObject);

    // Underline when update label is hovered
    procedure DoUpdateLabelMouseEnter(Sender: TObject);
    procedure DoUpdateLabelMouseLeave(Sender: TObject);

    procedure DoUpdateTimer(Sender: TObject);
  protected
    FPackages: TSimbaPackageList;
    FProgress: String; // UpdateDownloadProgress and UpdateExtractProgress will update this.

    FUpdates: TStringList;
    FUpdateThread: TThread;

    procedure FontChanged(Sender: TObject); override;

    function GetSelectedPackage: TSimbaPackage;

    procedure UpdateProgress;
    procedure UpdateSelectedRelease;
    procedure UpdateSelectedPackage;
    procedure UpdateDownloadProgress(Sender: TObject; URL: String; APosition, ASize: Int64);
    procedure UpdateExtractProgress(Sender: TObject; FileName: String; APosition, ASize: Int64);

    // Check for package updates. This is called on another thread
    procedure DoPackageUpdate;
    // Package updating has finished. This is synchronized so components are updated here.
    procedure DoPackageUpdateFinished(Sender: TObject);
  public
    property SelectedPackage: TSimbaPackage read GetSelectedPackage;

    // Callback for TSimbaPackage to download a page. No progress is shown.
    function GetPageSilent(URL: String; AllowedResponseCodes: array of Int32): String;

    // Callback for TSimbaPackage to download a page. Progress is shown with Status
    function GetPage(URL: String; AllowedResponseCodes: array of Int32): String;
    // Callback for TSimbaPackage to download a ZIP. Progress is shown with Status
    function GetZIP(URL, OutputPath: String; Flat: Boolean): Boolean;

    // Write to console
    procedure Debug(Message: String; Args: array of const); overload;
    procedure Debug(Message: String); overload;

    // Show error dialog
    procedure Error(Message: String; Args: array of const); overload;
    procedure Error(Message: String); overload;

    // Update bottom left status label
    procedure Status(Message: String; Args: array of const); overload;
    procedure Status(Message: String); overload;

    // Initalize dynamic components.
    constructor Create(AOwner: TComponent); override;
    // Free package list
    destructor Destroy; override;
  end;

var
  SimbaPackageForm: TSimbaPackageForm;

implementation

uses
  dateutils, lcltype, lclintf, lazfileutils, fileutil, simba.main,
  simba.httpclient_async, simba.settings, simba.httpclient;

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

      Time := Package.Releases[Items[Index]].Time;

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
    if Sender = PackageListPopup_SubmitIssue then
      OpenURL(Package.IssuesURL)
    else
    if Sender = PackageListPopup_VistRepository then
      OpenURL(Package.URL);
  end;
end;

procedure TSimbaPackageForm.DoAddPackageClick(Sender: TObject);
var
  URL: TStringArray;
  H: Int32;
  Package: TSimbaPackage;
  Versions: TStringArray;
begin
  URL := InputComboEx('Add Package', 'Enter or select package URL:', SUGGESTED_PACKAGES, True).Split('/');
  H := High(URL);
  if (H > 0) then
  begin
    Package := TSimbaPackage.Create(URL[H-1], URL[H]);
    Package.GetPage := @Self.GetPage;
    Package.GetZIP := @Self.GetZIP;

    Versions := Package.Releases.Versions;

    if (Length(Versions) > 1) then
      Status('Added package %s. (%d releases)', [Package.Name, Length(Versions)])
    else
    if (Length(Versions) = 1) then
      Status('Added package %s. (no releases - master branch only)', [Package.Name])
    else
    begin
      Status('Package not found');

      Package.Free();
      Package := nil;
    end;

    if (Package <> nil) then
    begin
      FPackages.Add(Package);

      PackagesListBox.AddItem(Package.Name, Package);
      PackagesListBox.ItemIndex := PackagesListBox.Count - 1;
    end;
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

      Status('Removed package ' + Package.Name);
    end;
  end;
end;

procedure TSimbaPackageForm.DoInstallPackageClick(Sender: TObject);
var
  Package: TSimbaPackage;
  Options: TSimbaPackage_InstallOptions;
  Message: String;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Options.Path := IncludeTrailingPathDelimiter(Application.Location + SetDirSeparators(InstallDirectoryList.Text));
    if InstallNameEdit.Text <> '' then
      Options.Path := Options.Path + IncludeTrailingPathDelimiter(InstallNameEdit.Text);

    Options.Flat := FlatExtractionButton.Checked;
    Options.Version := ReleasesList.Text;
    Options.IgnoreList := IgnoreListEdit.Text;

    Message := Format('Continue to install %s? Files in "%s" will be permanently overwritten.', [Package.Name, IncludeTrailingPathDelimiter(CreateRelativePath(Options.Path, Application.Location))]);

    if MessageDlg('Install Package', Message, mtConfirmation, mbYesNo, '') = mrYes then
    begin
      Debug('Installing %s %s to %s', [Package.Name, Options.Version, Options.Path]);

      if Package.Install(Options) then
      begin
        UpdateSelectedPackage();

        Status('Succesfully installed ' + Package.Name);
      end else
        Status('Failed to install ' + Package.Name);

      UpdateTimer.Interval := 1000;
    end;
  end;
end;

procedure TSimbaPackageForm.DoFormShow(Sender: TObject);
var
  Package: TSimbaPackage;
begin
  FPackages.Load();

  for Package in FPackages do
  begin
    Package.GetPage := @Self.GetPage;
    Package.GetZIP := @Self.GetZIP;
  end;

  PackagesListBox.Clear();
  for Package in FPackages do
    PackagesListBox.AddItem(Package.Name, Package);

  UpdateSelectedPackage();
end;

procedure TSimbaPackageForm.DoUpdateClick(Sender: TObject);
begin
  if (SelectedPackage <> nil) then
  begin
    SelectedPackage.Releases.UseCache := False;
    SelectedPackage.Releases.Versions;
    SelectedPackage.Releases.UseCache := True;

    UpdateSelectedPackage();
  end;
end;

procedure TSimbaPackageForm.SelectDirectoryButtonClick(Sender: TObject);
begin
  SelectDirectoryDialog.InitialDir := ExpandFileNameUTF8(InstallDirectoryList.Text);
  if SelectDirectoryDialog.Execute then
    InstallDirectoryList.Text := CreateRelativePath(SelectDirectoryDialog.FileName, Application.Location);
end;

procedure TSimbaPackageForm.InstallationModeChangedUser(Sender: TObject);
begin
  UpdateSelectedRelease();
end;

procedure TSimbaPackageForm.DoPackageListPopup(Sender: TObject; Mouse: TPoint; var Handled: Boolean);
begin
  Handled := PackagesListBox.GetIndexAtXY(Mouse.X, Mouse.Y) = -1;
end;

procedure TSimbaPackageForm.HandePackageListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
begin
  Packages := TSimbaPackageList.Create();
  Packages.Load();

  try
    FUpdates.Clear();
    FUpdates.Add('Simba Packages');

    for Package in Packages do
    begin
      Package.GetPage := @Self.GetPageSilent;
      if Package.InstalledVersion = 'master' then
        Continue;

      with Package.Releases.LatestRelease do
        if (Time > Package.InstalledVersionTime) then
          FUpdates.Add('%s (%s) can be updated to version %s', [Package.Name, Package.InstalledVersion, Version]);
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
  Options: TStringList;
  Package: TSimbaPackage;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Options := TStringList.Create();
    Options.Text := Package.Releases.Options[ReleasesList.Text];

    if Options.Text <> '' then
    begin
      InstallNameEdit.Text := Options.Values['name'];
      InstallDirectoryList.Text := Options.Values['directory'];
      IgnoreListEdit.Text := Options.Values['blacklist'];
      FlatExtractionButton.Checked := StrToBoolDef(Options.Values['collapse'], False);

      BasicInstallButton.Checked := True;
      BasicInstallButton.Enabled := True;

      CustomInstallButton.Checked := False;
    end else
    begin
      InstallNameEdit.Text := Package.Name;
      InstallDirectoryList.Text := 'Includes';
      IgnoreListEdit.Text := '';
      FlatExtractionButton.Checked := False;

      BasicInstallButton.Checked := False;
      BasicInstallButton.Enabled := False;

      CustomInstallButton.Checked := True;
    end;

    DoInstallationModeChange(nil);

    InstallDirectoryList.Text := IncludeTrailingPathDelimiter(InstallDirectoryList.Text);
    InstallDirectoryList.Text := SetDirSeparators(InstallDirectoryList.Text);

    NotesMemo.Text := Package.Releases[ReleasesList.Text].Notes;
    NotesMemo.SelStart := 0;
    NotesMemo.Font.Italic := NotesMemo.Text = '';
    if NotesMemo.Font.Italic then
      NotesMemo.Text := '(no release notes)';
  end;
end;

procedure TSimbaPackageForm.UpdateSelectedPackage;
var
  Package: TSimbaPackage;
  Versions: TStringArray;
begin
  Package := SelectedPackage;

  if (Package <> nil) then
  begin
    Versions := Package.Releases.Versions;

    if Length(Versions) > 0 then
    begin
      LatestVersionLabel.Caption := 'Latest Version: ' + Versions[0];

      if Package.InstalledVersion <> '' then
        InstalledVersionLabel.Caption := 'Installed Version: ' + Package.InstalledVersion
      else
        InstalledVersionLabel.Caption := 'Installed Version: Not Installed';

      UpdatePackageLabel.Hint := Format('Click to check for updates. Automatically checked hourly. (checked %d minutes ago)', [MinutesBetween(Now(), Package.Releases.CacheAge)]);

      ReleasesList.Items.Clear();
      ReleasesList.Items.AddStrings(Package.Releases.Versions);
      ReleasesList.ItemIndex := 0;

      UpdateSelectedRelease();
    end else
    begin
      Status('Error loading releases');

      PackagesListBox.ItemIndex := -1;
    end;
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

procedure TSimbaPackageForm.UpdateProgress;
begin
  Status(FProgress);

  Application.ProcessMessages();
end;

function TSimbaPackageForm.GetPageSilent(URL: String; AllowedResponseCodes: array of Int32): String;
var
  HTTPRequest: TSimbaHTTPRequest;
begin
  HTTPRequest := TSimbaHTTPRequest.Create(URL);

  try
    while HTTPRequest.Running do
      Sleep(25);

    Result := HTTPRequest.Contents;
  finally
    HTTPRequest.Free();
  end;
end;

function TSimbaPackageForm.GetPage(URL: String; AllowedResponseCodes: array of Int32): String;

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

    Status('');

    if HTTPRequest.Exception <> nil then
      Self.Error('Request failed: HTTP error %d', [HTTPRequest.ResponseCode])
    else
    if not IsAllowedResponseCode(HTTPRequest.ResponseCode) then
      Self.Error(Format('Request failed: HTTP error %d', [HTTPRequest.ResponseCode]))
    else
    if HTTPRequest.ResponseCode = HTTP_OK then
      Result := HTTPRequest.Contents;
  finally
    HTTPRequest.Free();
  end;
end;

function TSimbaPackageForm.GetZIP(URL, OutputPath: String; Flat: Boolean): Boolean;
var
  HTTPRequest: TSimbaHTTPRequestZIP;
begin
  InstallButton.Enabled := False;

  try
    HTTPRequest := TSimbaHTTPRequestZIP.Create(URL, OutputPath, Flat, @UpdateDownloadProgress, @UpdateExtractProgress);

    try
      while HTTPRequest.Running do
      begin
        UpdateProgress();

        Sleep(25);
      end;

      Status('');

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

procedure TSimbaPackageForm.Debug(Message: String);
begin
  WriteLn('[PACKAGES]: ' + Message);
end;

procedure TSimbaPackageForm.Error(Message: String; Args: array of const);
begin
  MessageDlg('Package Error', Format(Message, Args), mtError, [mbOK], 0);
end;

procedure TSimbaPackageForm.Error(Message: String);
begin
  MessageDlg('Package Error', Message, mtError, [mbOK], 0);
end;

procedure TSimbaPackageForm.Status(Message: String; Args: array of const);
begin
  StatusLabel.Caption := Format(Message, Args);
end;

procedure TSimbaPackageForm.Status(Message: String);
begin
  StatusLabel.Caption := Message;
end;

constructor TSimbaPackageForm.Create(AOwner: TComponent);

  function GetRootDirectories: TStringArray;
  var
    i: Int32;
  begin
    with FindAllDirectories(Application.Location, False) do
    try
      SetLength(Result, Count);
      for i := 0 to Count - 1 do
        Result[i] := IncludeTrailingPathDelimiter(CreateRelativePath(Strings[i], Application.Location));
    finally
      Free();
    end;
  end;

begin
  inherited Create(AOwner);

  Width := 800;
  Height := 650;

  InstallDirectoryList.Items.AddStrings(GetRootDirectories());
  InstallDirectoryList.ItemIndex := InstallDirectoryList.Items.IndexOf(IncludeTrailingPathDelimiter('Includes'));

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

