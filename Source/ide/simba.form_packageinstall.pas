{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_packageinstall;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, EditBtn,
  Buttons, SynEdit,
  simba.base, simba.ide_package, simba.ide_package_installer;

type
  TSimbaPackageInstallForm = class(TForm)
    ImageList1: TImageList;
    InstallButton: TButton;
    FlatCheckbox: TCheckBox;
    IgnoreListMemo: TMemo;
    LoadingLabel: TLabel;
    Notebook1: TNotebook;
    MainPage: TPage;
    LoadingPage: TPage;
    RefreshVersionButton: TSpeedButton;
    VersionsList: TComboBox;
    PathEdit: TFileNameEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OptionsPanel: TPanel;
    Output: TSynEdit;

    procedure DoVersionChange(Sender: TObject);
    procedure DoInstallButtonClick(Sender: TObject);
    procedure DoRefreshVersions(Sender: TObject);
    procedure DoFormShow(Sender: TObject);
  private
    FPackage: TSimbaPackage;
    FInstaller: TSimbaPackageInstaller;
    FLoading: Boolean;

    procedure BeginLoading;
    procedure EndLoading;

    procedure AddVersions;
    procedure Log(S: String; Args: array of const);
    procedure SetInstallOptions(Path: String; Flat: Boolean; Ignore: TStringArray);

    procedure DoStartInstall(Sender: TObject);
    procedure DoEndInstall(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; APackage: TSimbaPackage); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  simba.env, simba.misc, simba.dialog,
  simba.ide_package_endpointgithub, simba.threading;

constructor TSimbaPackageInstallForm.Create(AOwner: TComponent; APackage: TSimbaPackage);
begin
  inherited Create(AOwner);

  Output.Font.Size := GetDefaultFontSize();
  {$IFDEF WINDOWS}
  Output.Font.Name := 'Consolas';
  {$ENDIF}

  Width := Scale96ToScreen(600);
  Height := Scale96ToScreen(450);

  FPackage := APackage;

  FInstaller := TSimbaPackageInstaller.Create(FPackage, Output);
  FInstaller.OnStartInstall := @DoStartInstall;
  FInstaller.OnEndInstall := @DoEndInstall;

  QueueOnMainThread(@AddVersions);
end;

destructor TSimbaPackageInstallForm.Destroy;
begin
  if (FInstaller <> nil) then
    FreeAndNil(FInstaller);

  inherited Destroy();
end;

procedure TSimbaPackageInstallForm.DoVersionChange(Sender: TObject);
begin
  BeginLoading();

  // defaults
  SetInstallOptions(SimbaEnv.IncludesPath + FPackage.Name, False, []);

  if (VersionsList.ItemIndex >= 0) and (VersionsList.ItemIndex < Length(FPackage.Versions)) then
  begin
    InstallButton.Enabled := True;

    FInstaller.Version := FPackage.Versions[VersionsList.ItemIndex];
    if FInstaller.HasRemoteInstallOpts then
      SetInstallOptions(FInstaller.RemoteInstallOpts.Path, FInstaller.RemoteInstallOpts.Flat, FInstaller.RemoteInstallOpts.IgnoreList);
  end else
    InstallButton.Enabled := False;

  EndLoading();
end;

procedure TSimbaPackageInstallForm.DoRefreshVersions(Sender: TObject);
begin
  BeginLoading();

  FPackage.EndPoint.DeleteCache();
  if (FPackage.EndPoint is TSimbaPackageEndpoint_Github) then
    TSimbaPackageEndpoint_Github(FPackage.EndPoint).DownloadBranches();
  FPackage.Load();

  Log('Found %d versions', [Length(FPackage.Versions)]);

  AddVersions();

  EndLoading();
end;

procedure TSimbaPackageInstallForm.DoFormShow(Sender: TObject);
begin
  RefreshVersionButton.Width := PathEdit.ButtonWidth;
end;

procedure TSimbaPackageInstallForm.BeginLoading;
begin
  if FLoading then
    Exit;
  FLoading := True;

  LoadingLabel.Height := MainPage.Height; // Ensure the notebook stays the same size
  LoadingPage.Show();

  Application.ProcessMessages();
end;

procedure TSimbaPackageInstallForm.EndLoading;
begin
  if not FLoading then
    Exit;
  FLoading := False;

  MainPage.Show();

  Application.ProcessMessages();
end;

procedure TSimbaPackageInstallForm.SetInstallOptions(Path: String; Flat: Boolean; Ignore: TStringArray);
begin
  PathEdit.Text := ExtractRelativePath(Application.Location, Path);
  FlatCheckbox.Checked := Flat;
  IgnoreListMemo.Lines.AddStrings(Ignore, True);
end;

procedure TSimbaPackageInstallForm.AddVersions;
var
  I: Integer;
begin
  BeginLoading();

  VersionsList.Items.BeginUpdate();
  VersionsList.Items.Clear();
  for I := 0 to High(FPackage.Versions) do
    if FPackage.Versions[I].IsBranch then
      VersionsList.Items.Add(FPackage.Versions[I].Name + ' (branch)')
    else
      VersionsList.Items.Add(FPackage.Versions[I].Name + ' ' + FPackage.Versions[I].Age);
  if (VersionsList.Items.Count > 0) then
    VersionsList.ItemIndex := 0;
  VersionsList.Items.EndUpdate();

  DoVersionChange(VersionsList);

  EndLoading();
end;

procedure TSimbaPackageInstallForm.Log(S: String; Args: array of const);
begin
  Output.Lines.Add(S, Args);
  Output.CaretXY := TPoint.Create(0, Output.Lines.Count);
end;

procedure TSimbaPackageInstallForm.DoStartInstall(Sender: TObject);
begin
  InstallButton.Caption := 'Installing...';
  InstallButton.Enabled := False;
end;

procedure TSimbaPackageInstallForm.DoEndInstall(Sender: TObject);
begin
  InstallButton.Caption := 'Install';
  InstallButton.Enabled := True;
end;

procedure TSimbaPackageInstallForm.DoInstallButtonClick(Sender: TObject);
var
  Opts: TSimbaPackageInstallOptions;
begin
  Opts := Default(TSimbaPackageInstallOptions);
  Opts.Path := PathEdit.Text;
  Opts.Flat := FlatCheckbox.Checked;
  Opts.IgnoreList := IgnoreListMemo.Lines.ToStringArray();

  if SimbaQuestionDlg('Install Package', 'Install "%s" to "%s" ?', [FPackage.DisplayName, ExtractRelativePath(SimbaEnv.SimbaPath, Opts.Path)]) = ESimbaDialogResult.YES then
    FInstaller.Install(Opts);
end;

end.

