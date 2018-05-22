unit package;

(* .simbapackage
  // The package name to install under.
  name=WindowOverlay
  // The path to install into, from the working directory.
  directory=Includes
  // If true, files will be installed directly into the directory.
  extract_in_directory=false
  // collapse top level folders.
  collapse=false
  // files not to install delimited with commas.
  blacklist=.gitignore,.simbapackage
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DividerBevel, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, ButtonPanel, ExtCtrls, fpjson, types,
  ComCtrls;

type
  TPackageRelease = record
    Version: String;
    Download: String;
    Notes: String;
    Time: TDateTime;
    ConfigURL: String;
    Config: TStringList;
    Blacklist: TStringList;
  end;

  TPackageReleases = array of TPackageRelease;

const
  NullRelease: TPackageRelease = (
    Version: 'N/A';
    Download: '';
    Notes: '';
    Time: 0;
    ConfigURL: '';
    Config: nil;
    Blacklist: nil;
  );

type
  TPackageData = class
  protected
    FReleases: TPackageReleases;
    FName: String;
    FOwner: String;
    FVersion: String;
    FURL: String;
  public
    property Releases: TPackageReleases read FReleases write FReleases;
    property Name: String read FName write FName;
    property Owner: String read FOwner write FOwner;
    property Version: String read FVersion write FVersion;
    property URL: String read FURL write FURL;

    function Load(Key: String): Boolean; overload;
    function Load(JSON: TJSONArray): Boolean; overload;

    procedure Save;
    procedure Delete;

    destructor Destroy; override;
  end;

  TPackageForm = class(TForm)
    btnInstall: TButton;
    btnPanel: TButtonPanel;
    btnInstallDirectory: TButton;
    checkExtractInDirectory: TCheckBox;
    checkCollapse: TCheckBox;
    comboInstallDirectory: TComboBox;
    comboVersions: TComboBox;
    optionsGroup: TGroupBox;
    installationDivider: TDividerBevel;
    editInstallName: TEdit;
    lblUpdate: TLabel;
    lblInstallDiretory: TLabel;
    lblInstallName: TLabel;
    lblLatestVersion: TLabel;
    btnBasicInstall: TRadioButton;
    btnCustomInstall: TRadioButton;
    selectDirectory: TSelectDirectoryDialog;
    timerCheckUpdate: TIdleTimer;
    lblStatus: TLabel;
    lblSelectVersion: TLabel;
    lblInstalledVersion: TLabel;
    lbPackages: TListBox;
    memoReleaseNotes: TMemo;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    btnAdd: TSpeedButton;
    btnRemove: TSpeedButton;

    procedure btnInstallDirectoryClick(Sender: TObject);
    procedure installationChange(Sender: TObject);
    procedure lblUpdateClick(Sender: TObject);
    procedure lblUpdateMouseEnter(Sender: TObject);
    procedure lblUpdateMouseLeave(Sender: TObject);
    procedure packagedChanged(Sender: TObject; User: Boolean);
    procedure checkUpdates(Sender: TObject);
    procedure checkUpdatesSync(Data: PtrInt);
    procedure comboVersionsChange(Sender: TObject);
    procedure comboVersionsDrawItem(Control: TWinControl; Index: Integer; R: TRect; State: TOwnerDrawState);
    procedure btnInstallClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  protected
    FShowButton: TToolButton;
    FShowButtonImage: Int32;

    procedure Disable(Packages: Boolean = True);
    procedure Enable;

    function LoadPackage(Package: TPackageData): Boolean;

    function GetPackage(var Package: TPackageData): Boolean;
  public
    procedure UpdateStatus(Status: String; Process: Boolean = False);

    constructor Create(AOwner: TComponent; ShowButton: TToolButton); reintroduce;
  end;

var
  PackageForm: TPackageForm;

implementation

uses
  LCLType, newsimbasettings, dateutils, MufasaTypes, httpd, Zipper, XMLRead, DOM,
  Math, fphttpclient, LazFileUtils, FileUtil, jsonparser;

// pretty neat imo.
type
  TCopyDirectory = class(TFileSearcher)
  protected
    FSource: string;
    FDestination: string;
    FCollapse: Boolean;
    FBlacklist: TStringList;

    procedure DoFileFound; override;
    procedure DoDirectoryFound; override;
  public
    class function Copy(Source, Destination: String; Collapse: Boolean; Blacklist: TStringList): Boolean;
  end;

procedure TCopyDirectory.DoFileFound;

  function SameFile(Left, Right: String): Boolean;
  begin
    Result := (FileSize(Left) = FileSize(Right)) and (ReadFileToString(Left) = ReadFileToString(Right));
  end;

var
  S: String;
begin
  S := StringReplace(FileName, FSource, FDestination, []);

  if (not DirectoryExists(ExtractFileDir(S))) then
    Exit;
  if (FBlacklist <> nil) and (FBlacklist.IndexOf(CreateRelativePath(FileName, FSource)) >= 0) then
    Exit;

  if (not FileExists(S)) or (not SameFile(FileName, S)) then
  begin
    PackageForm.UpdateStatus('Copying... ' + ExtractFileName(S), True);

    if FileExists(S) and (not DeleteFile(S)) then
      raise Exception.Create('Failed to delete file "' + S + '"');
    if (not CopyFile(FileName, S)) then
      raise Exception.Create('Failed to copy file "' + FileName + '"');
  end;
end;

procedure TCopyDirectory.DoDirectoryFound;
var
  S: String;
begin
  S := StringReplace(FileName, FSource, FDestination, []);

  if (FBlacklist <> nil) and (FBlacklist.IndexOf(CreateRelativePath(FileName, FSource)) >= 0) then
    Exit;

  if FCollapse and (Level = 0) then
    TCopyDirectory.Copy(ExcludeTrailingPathDelimiter(FileName), ExcludeTrailingPathDelimiter(ExtractFilePath(S)), False, FBlacklist)
  else
  if (not DirectoryExistsUTF8(S)) and (not ForceDirectoriesUTF8(S)) then
    raise Exception.Create('Failed to create directory "' + S + '"');
end;

class function TCopyDirectory.Copy(Source, Destination: String; Collapse: Boolean; Blacklist: TStringList): Boolean;
begin
  with TCopyDirectory.Create() do
  try
    FSource := SetDirSeparators(Source);
    FDestination := SetDirSeparators(Destination);
    FCollapse := Collapse;
    FBlacklist := Blacklist;

    try
      Search(FSource);

      Exit(True);
    except
      on e: Exception do
        WriteLn('TCopyDirectory.Copy: ', e.ClassName + '::' + e.Message);
    end;
  finally
    DeleteDirectory(FSource, False);

    Free();
  end;

  Exit(False);
end;

type
  TDownloader = class
  protected
    FClient: TFPHTTPClient;
    FData: TMemoryStream;
    FURL: String;
    FContentLength: Int32;

    function GetData: String;

    procedure GetStream(Sender: TObject; var Stream: TStream);

    procedure Downloading(Sender: TObject; const Size, Position: Int64);
    procedure Extracting(Sender: TObject; const FilePath: String);
    procedure Redirect(Sender : TObject; Const Source: String; Var Dest: String);
  public
    property Stream: TMemoryStream read FData;
    property Data: String read GetData;

    function Get: Boolean;
    function Extract(Path: String; Collapse: Boolean; Blacklist: TStringList): Boolean;

    constructor Create(URL: String);
    destructor Destroy; override;
  end;

procedure TDownloader.Downloading(Sender: TObject; const Size, Position: Int64);
var
  Headers: TStringList;
  i: Int32;
begin
  if (Size > 0) then
    FContentLength := Size;

  // Github can redirect to another place to download such as `codeload.github`
  // HTTPClient does not check for the content length header again, so we do that here.
  if (Size = -1) and (FContentLength = -1) then
  begin
    Headers := TStringList.Create();

    try
      with TFPHTTPClient.Create(nil) do
      try
        Head(FURL, Headers);

        for i := 0 to Headers.Count - 1 do
          if Headers[i].StartsWith('Content-Length: ', True) then
            FContentLength := StrToInt(Copy(Headers[i], Length('Content-Length: '), $FFFFFF));
      finally
        Free();
      end;
    except
    end;

    Headers.Free();
  end;

  if (FContentLength > 0) then
    PackageForm.UpdateStatus('Downloading... (' + IntToStr(Round(Position / FContentLength * 100)) + '%)', True)
  else
    PackageForm.UpdateStatus('Downloading...', True);
end;

procedure TDownloader.Extracting(Sender: TObject; const FilePath: String);
var
  i, Current, Total: Int32;
begin
  Current := 0;
  Total := 0;

  with Sender as TUnZipper do
    for i := 0 to Entries.Count - 1 do
    begin
      if (not Entries[i].IsLink) and (not Entries[i].IsDirectory) then
      begin
        Inc(Total);
        if (Entries[i].ArchiveFileName = Copy(FilePath, Length(OutputPath) + 1, $FFFFFF)) then
          Current := Total;
      end;
    end;

  if (Current > 0) and (Total > 0) then
    PackageForm.UpdateStatus('Extracting...' + IntToStr(Current) + '/' + IntToStr(Total), True)
  else
    PackageForm.UpdateStatus('Extracting...', True);
end;

procedure TDownloader.Redirect(Sender: TObject; const Source: String; var Dest: String);
begin
  FURL := Dest;
end;

function TDownloader.GetData: String;
begin
  SetString(Result, PAnsiChar(FData.Memory), FData.Size);
end;

procedure TDownloader.GetStream(Sender: TObject; var Stream: TStream);
begin
  FData.Position := 0;

  // The zipper will free this for us.
  Stream := TMemoryStream.Create();
  Stream.CopyFrom(FData, FData.Size);
end;

function TDownloader.Get: Boolean;
begin
  Result := False;

  PackageForm.UpdateStatus('Connecting...', True);

  try
    FClient.HTTPMethod('GET', FURL, FData, [HTTP_OK, HTTP_NOT_FOUND]);

    Result := FClient.ResponseStatusCode = HTTP_OK;
  except
    on e: Exception do
    begin
      PackageForm.UpdateStatus('ERROR downloading: ' + e.ClassName + '::' + e.Message, True);

      Exit(False);
    end;
  end;

  PackageForm.UpdateStatus('', True);
end;

function TDownloader.Extract(Path: String; Collapse: Boolean; Blacklist: TStringList): Boolean;
var
  Zipper: TUnZipper;
  Archive: String;
begin
  Result := False;

  Zipper := TUnZipper.Create();
  Zipper.OnOpenInputStream := @GetStream;
  Zipper.OnStartFile := @Extracting;
  Zipper.OutputPath := ExtractFilePath(Path);

  try
    Zipper.Examine();
    if (Zipper.Entries.Count = 0) then
      raise Exception.Create('No files to extract');

    Archive := ExpandFileName(ExtractFilePath(Path) + Zipper.Entries[0].ArchiveFileName);

    // delete previously downloaded archive if it exists.
    if DirectoryExists(Archive) then
      DeleteDirectory(Archive, False);

    // create destination lblInstallDiretory.
    if (not DirectoryExists(Path)) then
      ForceDirectories(Path);

    Zipper.Clear();
    Zipper.UnZipAllFiles();

    // Copy all the files if they need updating.
    Result := TCopyDirectory.Copy(ExcludeTrailingPathDelimiter(Archive), ExcludeTrailingPathDelimiter(Path), Collapse, Blacklist);
  except
    on e: Exception do
      WriteLn('TDownloader.Extract: ', e.ClassName, '::', e.Message);
  end;

  Zipper.Free();
end;

constructor TDownloader.Create(URL: String);
begin
  FContentLength := -1;
  FURL := URL;
  FData := TMemoryStream.Create();
  FClient := TFPHTTPClient.Create(nil);
  with FClient do
  begin
    AllowRedirect := True;
    AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36');
    HTTPVersion := '1.0'; // say NO! to chunked encoding - git is weird and we can't display our progress.
    OnDataReceived := @Downloading;
    OnRedirect := @Redirect;
  end;
end;

destructor TDownloader.Destroy;
begin
  FClient.Free();
  FData.Free();
end;

// Check for updates on another thread so nobody notices anything!
// git API has a 60 request limit P/H, we will use another method to get the latest version without messing with the API. :)
type
  TPackageUpdater = class(TThread)
  protected
    FPackages: array of record
      Name: String;
      Owner: String;
      Version: String;
      HasUpdate: Boolean;
    end;

    FButton: TToolButton;
    FButtonImage: Int32;
    FButtonHint: String;

    procedure Update;
    procedure Execute; override;
  public
    constructor Create(Packages: TStrings; Button: TToolButton; ButtonImage: Int32); reintroduce;
  end;

procedure TPackageUpdater.Update;
var
  i: Int32;
  Updates: TStringArray;
begin
  SetLength(Updates, 0);

  for i := 0 to High(FPackages) do
    if (FPackages[i].HasUpdate) then
    begin
      SetLength(Updates, Length(Updates) + 1);
      Updates[High(Updates)] := FPackages[i].Name;
    end;

  if (Length(Updates) > 0) then
  begin
    FButtonHint := 'Package Manager' + LineEnding +
                   'Updates are available for: ' + LineEnding;

    for i := 0 to High(Updates) do
      FButtonHint := FButtonHint + ' - ' + Updates[i] + LineEnding;
  end else
    FButtonHint := 'Package Manager';

  FButton.Hint := Trim(FButtonHint);
  FButton.ImageIndex := Min(FButtonImage + Length(Updates), FButtonImage + 9);
end;

procedure TPackageUpdater.Execute;

  // Don't use API for this (60 requests P/H limit)
  function GetLatestVersion(Owner, Name: String): String;
  var
    Client: TFPHTTPClient;
    XML: TXMLDocument;
    Stream: TMemoryStream;
    Entries: TDOMNodeList;
    i: Int32;
    Time: String;
    Latest: record Time: TDateTime; Version: String; end;
  begin
    Result := '';

    Stream := TMemoryStream.Create();

    Client := TFPHTTPClient.Create(nil);
    Client.AllowRedirect := True;
    Client.AddHeader('User-Agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.186 Safari/537.36');

    try
      Client.Get(Format('https://github.com/%s/%s/releases.atom', [Owner, Name]), Stream);

      if (Client.ResponseStatusCode = HTTP_OK) then
      begin
        Stream.Position := 0;

        ReadXMLFile(XML, Stream);

        try
          Entries := XML.DocumentElement.GetElementsByTagName('entry');

          Latest.Time := 0;
          Latest.Version := '';

          for i := 0 to Entries.Count - 1 do
          begin
            if (Entries[i].FindNode('updated') = nil) or (Entries[i].FindNode('id') = nil) then
              Continue;

            Time := Entries[i].FindNode('updated').TextContent;

            System.Delete(Time, Pos('T', Time), 1);
            System.Delete(Time, Pos('Z', Time), 1);

            try
              if ScanDateTime('yyyy-mm-ddhh:nn:ss', Time) > Latest.Time then
              begin
                Latest.Time := ScanDateTime('yyyy-mm-ddhh:nn:ss', Time);
                Latest.Version := ExtractFileName(Entries[i].FindNode('id').TextContent);
              end;
            except
            end;
          end;

          if (Latest.Time > 0) then
            Result := Latest.Version;

          Entries.Free();
        finally
          XML.Free();
        end;
      end;
    except
      on e: Exception do
        WriteLn('TPackageUpdater.Execute: ', e.ClassName, '::', e.Message);
    end;

    Client.Free();
    Stream.Free();
  end;

var
  i: Int32;
  Version: String;
begin
  for i := 0 to High(FPackages) do
  begin
    if (FPackages[i].Version = 'N/A') then
      Continue;

    Version := GetLatestVersion(FPackages[i].Owner, FPackages[i].Name);
    if (Version <> '') and (Version <> FPackages[i].Version) then
      FPackages[i].HasUpdate := True;
  end;

  Synchronize(@Update);

  Terminate();
end;

constructor TPackageUpdater.Create(Packages: TStrings; Button: TToolButton; ButtonImage: Int32);
var
  i: Int32;
  Package: TPackageData;
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FButton := Button;
  FButtonImage := ButtonImage;
  FButtonHint := '';

  for i := 0 to Packages.Count - 1 do
  begin
    Package := Packages.Objects[i] as TPackageData;

    SetLength(FPackages, Length(FPackages) + 1);
    with FPackages[High(FPackages)] do
    begin
      Name := Package.Name;
      Owner := Package.Owner;
      Version := Package.Version;
      HasUpdate := False;
    end;
  end;
end;

procedure TPackageData.Save;
begin
  SimbaSettings.MMLSettings.SetKeyValue('Packages/' + FName + '/Version', FVersion);
  SimbaSettings.MMLSettings.SetKeyValue('Packages/' + FName + '/URL', FURL);
  SimbaSettings.MMLSettings.SetKeyValue('Packages/' + FName + '/Owner', FOwner);
  SimbaSettings.Save(SimbaSettingsFile);
end;

procedure TPackageData.Delete;
begin
  SimbaSettings.MMLSettings.DeleteKey('Packages/' + FName);
  SimbaSettings.Save(SimbaSettingsFile);
end;

destructor TPackageData.Destroy;
var
  i: Int32;
begin
  for i := 0 to High(FReleases) do
    if (FReleases[i].Config <> nil) then
      FReleases[i].Config.Free()
    else
    if (FReleases[i].Blacklist <> nil) then
      FReleases[i].Blacklist.Free();

  inherited Destroy();
end;

function TPackageData.Load(Key: String): Boolean;
begin
  FName := Key;
  FVersion := SimbaSettings.MMLSettings.GetKeyValueDef('Packages/' + FName + '/Version', 'N/A');
  FURL := SimbaSettings.MMLSettings.GetKeyValue('Packages/' + FName + '/URL');
  FOwner := SimbaSettings.MMLSettings.GetKeyValue('Packages/' + FName + '/Owner');

  Exit(True);
end;

function TPackageData.Load(JSON: TJSONArray): Boolean;

  procedure AddRelease(JSON: TJSONObject);
  var
    Release: TPackageRelease;
    Time: String;
  begin
    Release := NullRelease;

    try
      if (JSON.Elements['body'] <> nil) then
        Release.Notes := JSON.Elements['body'].AsString;
      if (JSON.Elements['tag_name'] <> nil) then
        Release.Version := JSON.Elements['tag_name'].AsString;
      if (JSON.Elements['zipball_url'] <> nil) then
        Release.Download := JSON.Elements['zipball_url'].AsString;
      if (JSON.Elements['tag_name'] <> nil) then
        Release.ConfigURL := Format('https://raw.github.com/%s/%s/%s/.simbapackage', [FOwner, FName, JSON.Elements['tag_name'].AsString]);
      if (JSON.Elements['published_at'] <> nil) then
      begin
        Time := JSON.Elements['published_at'].AsString;

        System.Delete(Time, Pos('T', Time), 1);
        System.Delete(Time, Pos('Z', Time), 1);

        try
          Release.Time := ScanDateTime('yyyy-mm-ddhh:nn:ss', Time);
        except
        end;
      end;

      {
      if (JSON.Elements['assets'] <> nil) and (JSON.Elements['assets'] is TJSONArray) and (TJSONArray(JSON.Elements['assets']).Count > 0) then
        with TJSONArray(JSON.Elements['assets']).Objects[0] do
        begin
          if (Elements['browser_download_url'] <> nil) and (ExtractFileExt(Elements['browser_download_url'].AsString) = '.zip') then
            Release.Download := Elements['browser_download_url'].AsString;
        end;
      }

      if (Release.Version <> '') and (Release.Download <> '') then
      begin
        SetLength(FReleases, Length(FReleases) + 1);

        Self.Releases[High(FReleases)] := Release;
      end;
    except
      on e: Exception do
        WriteLn('TPackageData.LoadFromURL: ', e.ClassName, '::', e.Message);
    end;
 end;

var
  i: Int32;
  Remote, Local: String;
begin
  // Set correctly. `merlijnwajer/simba` >> `MerlijnWajer/Simba`
  if (JSON.Count > 0) and (JSON.Objects[0].Elements['url'] <> nil) then
  begin
    Local := LowerCase(Self.FOwner + '/' + Self.FName);
    Remote := JSON.Objects[0].Elements['url'].AsString;

    i := Pos(Local, LowerCase(Remote));
    if (i > 0) then
    begin
      Move(Remote[i], FOwner[1], Length(FOwner));
      Move(Remote[i + Length(FOwner) + Length('/')], FName[1], Length(FName));
    end;
  end;

  SetLength(FReleases, 0);
  for i := 0 to JSON.Count - 1 do
    AddRelease(JSON.Objects[i]);

  Result := Length(FReleases) > 0;
end;

procedure TPackageForm.comboVersionsChange(Sender: TObject);
var
  Package: TPackageData;
  i: Int32;
  Downloader: TDownloader;
begin
  memoReleaseNotes.Clear();
  memoReleaseNotes.Font.Italic := True;
  memoReleaseNotes.Text := '(no release notes)';

  if GetPackage(Package) then
  begin
    for i := 0 to high(Package.Releases) do
      if (Package.Releases[i].Version = comboVersions.Text) then
      begin
        if (Package.Releases[i].Config = nil) then
        begin
          Downloader := TDownloader.Create(Package.Releases[i].ConfigURL);

          if Downloader.Get() then
          begin
            Package.Releases[i].Config := TStringList.Create();
            Package.Releases[i].Config.Text := Downloader.Data;
          end;

          Downloader.Free();
        end;

        if (Package.Releases[i].Config <> nil) then
        begin
          with Package.Releases[i].Config do
          begin
            btnBasicInstall.Enabled := True;
            btnBasicInstall.Checked := True;

            editInstallName.Text := Values['name'];
            comboInstallDirectory.Text := SetDirSeparators(IncludeTrailingPathDelimiter(Values['directory']));
            checkExtractInDirectory.Checked := StrToBoolDef(Values['extract_in_directory'], False);
            checkCollapse.Checked := StrToBoolDef(Values['collapse'], False);

            if (Values['blacklist'] <> '') then
            begin
              Package.Releases[i].Blacklist := TStringList.Create();
              Package.Releases[i].Blacklist.Delimiter := ',';
              Package.Releases[i].Blacklist.DelimitedText := Values['blacklist'];
            end;
          end;
        end else
        begin
          btnBasicInstall.Enabled := False;
          btnCustomInstall.Checked := True;

          editInstallName.Text := Package.Name;
          comboInstallDirectory.Text := IncludeTrailingPathDelimiter('Includes');
          checkExtractInDirectory.Checked := False;
          checkCollapse.Checked := False;
        end;

        if (Package.Releases[i].Notes <> '') then
        begin
          memoReleaseNotes.Font.Italic := False;
          memoReleaseNotes.Text := package.releases[i].Notes;
        end;
      end;
  end;
end;

procedure TPackageForm.comboVersionsDrawItem(Control: TWinControl; Index: Integer; R: TRect; State: TOwnerDrawState);
var
  TimeSpan: String;
begin
  with Control as TComboBox do
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

      if (not Control.Parent.Enabled) then
        Canvas.Font.Color := clGrayText;
    end;

    Canvas.Rectangle(R);
    Canvas.Font.Italic := False;
    Canvas.TextOut(R.Left + 2, R.Top, Items[Index]);

    if (Items.Objects[Index] <> nil) then
    begin
      TimeSpan := '(' + IntToStr(SizeInt(Items.Objects[Index]) - 1) + ' days ago)';

      case TimeSpan of
        '(0 days ago)': TimeSpan := '(today)';
        '(1 days ago)': TimeSpan := '(yesterday)'
      end;

      Canvas.Font.Italic := True;
      Canvas.TextOut(R.Left + Canvas.TextWidth(Items[Index] + ' ') + 2, R.Top, TimeSpan);
    end;
  end;
end;

procedure TPackageForm.btnInstallClick(Sender: TObject);
var
  Package: TPackageData;
  SelectedRelease: TPackageRelease;
  Downloader: TDownloader;
  Path: String;
begin
  if GetPackage(Package) then
  begin
    Path := IncludeTrailingPathDelimiter(comboInstallDirectory.Text);
    if (not checkExtractInDirectory.Checked) then
      Path := Path + IncludeTrailingPathDelimiter(editInstallName.Text);

    SelectedRelease := Package.Releases[comboVersions.Items.IndexOf(comboVersions.Text)];

    try
      Disable();

      if (MessageDlg('Install Package', 'Continue to install this package? Files in ' + #39 + Path + #39 + ' will be permanently overwritten.', mtConfirmation, mbYesNo, '') = mrYes) then
      begin
        Downloader := TDownloader.Create(SelectedRelease.Download);

        try
          if Downloader.Get() then
          begin
            if Downloader.Extract(Path, checkCollapse.Checked, SelectedRelease.Blacklist) then
            begin
              Package.Version := comboVersions.Text;
              Package.Save();

              UpdateStatus('Succesfully installed package: ' + Package.Name + ' v' + Package.Version);
            end else
              UpdateStatus('ERROR: Unable to extract, the installed package is currently in use. Restart Simba and retry.');
          end;
        finally
          Downloader.Free();
        end;
      end;
    finally
      Enable();

      packagedChanged(nil, False);
      checkUpdates(nil);
    end;
  end;
end;

procedure TPackageForm.btnRemoveClick(Sender: TObject);
var
  Package: TPackageData;
begin
  if GetPackage(Package) and (MessageDlg('Delete Package', 'Do you wish to delete this package? The package files will not be deleted.', mtConfirmation, mbYesNo, '') = mrYes) then
  begin
    Package.Delete();

    lbPackages.Items.Delete(lbPackages.ItemIndex);
    lbPackages.OnSelectionChange(Self, False);
  end;
end;

procedure TPackageForm.packagedChanged(Sender: TObject; User: Boolean);
var
  Package: TPackageData;
  i: Int32;
begin
  if GetPackage(Package) then
  begin
    if (Length(Package.Releases) = 0) then
    begin
      Disable();

      LoadPackage(Package);
    end;

    Enable();

    comboVersions.Clear();
    comboVersions.Items.BeginUpdate();

    if (Length(Package.Releases) > 0) then
    begin
      for i := 0 to High(Package.Releases) do
        if (Package.Releases[i].Time > 0) then
          comboVersions.Items.AddObject(Package.Releases[i].Version, TObject(DaysBetween(Now(), Package.Releases[i].Time) + 1))
        else
          comboVersions.Items.Add(Package.Releases[i].Version);

      comboVersions.Text := comboVersions.Items[0];
      comboVersions.OnChange(nil);

      lblLatestVersion.Caption := 'Latest Version: ' + comboVersions.Text;
      lblInstalledVersion.Caption := 'Installed Version: ' + Package.Version;
    end else
    begin
      lblLatestVersion.Caption := 'Latest Version: N/A';
      lblInstalledVersion.Caption := 'Installed Version: N/A';

      memoReleaseNotes.Font.Italic := True;
      memoReleaseNotes.Text := '(unknown)';

      comboVersions.Items.Add('(unknown)');
      comboVersions.Text := comboVersions.Items[0];

      editInstallName.Text := '';

      checkExtractInDirectory.Checked := False;
      checkCollapse.Checked := False;
    end;

    comboVersions.Items.EndUpdate();
  end else
  begin
    Disable(False);

    memoReleaseNotes.Clear();
    comboVersions.Clear();
    editInstallName.Clear();

    lblInstalledVersion.Caption := 'Installed Version:';
    lblLatestVersion.Caption := 'Latest Version:';
  end;
end;

procedure TPackageForm.btnInstallDirectoryClick(Sender: TObject);
begin
  selectDirectory.InitialDir := ExpandFileNameUTF8(comboInstallDirectory.Text);
  if selectDirectory.Execute then
    comboInstallDirectory.Text := CreateRelativePath(selectDirectory.FileName, GetCurrentDirUTF8());
end;

procedure TPackageForm.installationChange(Sender: TObject);
begin
  optionsGroup.Enabled := btnCustomInstall.Checked;
end;

procedure TPackageForm.lblUpdateClick(Sender: TObject);
var
  Package: TPackageData;
begin
  if GetPackage(Package) then
    LoadPackage(Package);

  packagedChanged(nil, False);
end;

procedure TPackageForm.lblUpdateMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := True;
end;

procedure TPackageForm.lblUpdateMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := False;
end;

procedure TPackageForm.checkUpdates(Sender: TObject);
begin
  if (lbPackages.Count > 0) then;
  begin
    WriteLn('Checking for package updates...');

    TPackageUpdater.Create(lbPackages.Items, FShowButton, FShowButtonImage);
  end;
end;

procedure TPackageForm.checkUpdatesSync(Data: PtrInt);
begin
  checkUpdates(nil);
end;

procedure TPackageForm.btnAddClick(Sender: TObject);
var
  URL: String;
  Path: TStringList;
  Package: TPackageData;
begin
  URL := 'https://github.com/';

  if InputQuery('New Package', 'Git Repository URL:', URL) then
  begin
    Path := TStringList.Create();
    Path.Delimiter := '/';
    Path.DelimitedText := ExcludeTrailingPathDelimiter(URL);
    while (Path.Count > 2) do
      Path.Delete(0);

    if (Path.Count = 2) then
    begin
      Package := TPackageData.Create();
      Package.Owner := Path[0];
      Package.Name := Path[1];
      Package.URL := Format('https://api.github.com/repos/%s/%s/releases', [Package.Owner, Package.Name]);
      Package.Version := 'N/A';

      if LoadPackage(Package) then
      begin
        lbPackages.AddItem(Package.Name, Package);
        lbPackages.ItemIndex := lbPackages.Count - 1;

        UpdateStatus('Added package: ' + Package.Name);
      end else
        Package.Free();
    end else
      UpdateStatus('ERROR: Invalid URL entered');

    Path.Free();
  end;
end;

procedure TPackageForm.Disable(Packages: Boolean);
begin
  ActiveControl := nil;

  lbPackages.Enabled := not Packages;
  btnBasicInstall.Enabled := False;
  btnCustomInstall.Enabled := False;
  btnInstall.Enabled := False;
  optionsGroup.Enabled := False;
  pnlTop.Enabled := False;
end;

procedure TPackageForm.Enable;
begin
  lbPackages.Enabled := True;
  pnlTop.Enabled := True;
  btnBasicInstall.Enabled := True;
  btnCustomInstall.Enabled := True;
  btnInstall.Enabled := True;
  optionsGroup.Enabled := btnCustomInstall.Checked;
end;

function TPackageForm.LoadPackage(Package: TPackageData): Boolean;
var
  Downloader: TDownloader;
  JSON: TJSONData;
begin
  Downloader := TDownloader.Create(Format('https://api.github.com/repos/%s/%s/releases', [Package.Owner, Package.Name]));

  try
    if Downloader.Get() then
    begin
      JSON := GetJSON(Downloader.Data);

      if (JSON <> nil) and (JSON is TJSONArray) then
      begin
        if Package.Load(JSON as TJSONArray) then
          UpdateStatus('')
        else
          UpdateStatus('ERROR: No releases found');
      end else
        UpdateStatus('ERROR: Invalid JSON data');
    end;
  finally
    Downloader.Free();
  end;

  Result := Length(Package.Releases) > 0;
end;

function TPackageForm.GetPackage(var Package: TPackageData): Boolean;
begin
  if (lbPackages.ItemIndex >= 0) then
  begin
    Package := lbPackages.Items.Objects[lbPackages.ItemIndex] as TPackageData;

    Exit(True);
  end;

  Exit(False);
end;

procedure TPackageForm.UpdateStatus(Status: String; Process: Boolean);
begin
  lblStatus.Caption := Status;
  if Process then
    Application.ProcessMessages();
end;

constructor TPackageForm.Create(AOwner: TComponent; ShowButton: TToolButton);
var
  i: Int32;
  Packages: TStringArray;
  Package: TPackageData;
  Directories: TStringList;
begin
  inherited Create(AOwner);

  FShowButton := ShowButton;
  FShowButtonImage := ShowButton.ImageIndex;

  lbPackages.Font.Size := 10;
  lbPackages.ItemHeight := 20;
  with lbPackages.Items as TStringList do
    OwnsObjects := True;

  Directories := FindAllDirectories(GetCurrentDirUTF8(), False);
  try
    for i := 0 to Directories.Count - 1 do
      comboInstallDirectory.Items.Add(IncludeTrailingPathDelimiter(CreateRelativePath(Directories[i], GetCurrentDirUTF8())));
    comboInstallDirectory.Text := IncludeTrailingPathDelimiter('Includes');
  finally
    Directories.Free();
  end;

  SimbaSettings.MMLSettings.ListKeys('Packages', Packages);

  for i := 0 to High(Packages) do
  begin
    Package := TPackageData.Create();
    Package.Load(Packages[i]);

    lbPackages.AddItem(Package.Name, Package);
  end;

  Application.QueueASyncCall(@checkUpdatesSync, 0);

  Disable(False);
end;

initialization
  {$I package.lrs}

end.

