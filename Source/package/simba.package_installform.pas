unit simba.package_installform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, stdctrls, editbtn, extctrls, synedit,
  simba.package, simba.package_installer;

type
  TSimbaPackageInstallForm = class(TForm)
    InstallButton: TButton;
    FlatCheckbox: TCheckBox;
    IgnoreListMemo: TMemo;
    VersionsList: TComboBox;
    PathEdit: TFileNameEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OptionsPanel: TPanel;
    Output: TSynEdit;

    procedure DoInstallButtonClick(Sender: TObject);
    // Update the install option components
    procedure DoVersionChange(Sender: TObject);
  private
    FPackage: TSimbaPackage;
    FInstaller: TSimbaPackageInstaller;

    function GetSelectedRelease(out ARelease: TSimbaPackageRelease): Boolean;
    function GetSelectedBranch(out ABranch: TSimbaPackageBranch): Boolean;

    procedure Log(S: String);

    procedure DoStartInstall(Sender: TObject);
    procedure DoEndInstall(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; APackage: TSimbaPackage); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  simba.mufasatypes, simba.env, simba.fonthelpers, simba.dialog;

constructor TSimbaPackageInstallForm.Create(AOwner: TComponent; APackage: TSimbaPackage);
var
  I: Integer;
begin
  inherited Create(AOwner);

  Output.Font.Size := GetDefaultFontSize() + 1;
  {$IFDEF WINDOWS}
  Output.Font.Name := 'Consolas';
  {$ENDIF}

  Width := Scale96ToScreen(550);
  Height := Scale96ToScreen(400);

  FPackage := APackage;
  FInstaller := TSimbaPackageInstaller.Create(FPackage, Output);
  FInstaller.OnStartInstall := @DoStartInstall;
  FInstaller.OnEndInstall := @DoEndInstall;

  VersionsList.Items.Clear();
  for I := 0 to High(FPackage.Releases) do
    VersionsList.Items.AddObject(FPackage.Releases[I].Name + ' ' + FPackage.Releases[I].Age, TObject(@FPackage.Releases[I]));
  for I := 0 to High(FPackage.Branches) do
    VersionsList.Items.AddObject(FPackage.Branches[I].Name + ' (branch)', TObject(@FPackage.Branches[I]));

  if (VersionsList.Items.Count > 0) then
    VersionsList.ItemIndex := 0;
  VersionsList.OnChange(VersionsList);
end;

destructor TSimbaPackageInstallForm.Destroy;
begin
  if (FInstaller <> nil) then
    FreeAndNil(FInstaller);

  inherited Destroy();
end;

procedure TSimbaPackageInstallForm.DoVersionChange(Sender: TObject);
var
  Rel: TSimbaPackageRelease;
  Options: TSimbaPackageInstallOptions;
begin
  if GetSelectedRelease(Rel) then
    if FInstaller.GetOptions(Rel, Options) then
    begin
      PathEdit.Text        := Options.Path;
      FlatCheckbox.Checked := Options.Flat;
      IgnoreListMemo.Lines.AddStrings(Options.IgnoreList, True);

      Exit;
    end else
      Log('Release "%s" has no install options'.Format([Rel.Name]));

  PathEdit.Text        := GetIncludePath() + FPackage.Info.Name;
  FlatCheckbox.Checked := False;
  IgnoreListMemo.Clear();
end;

function TSimbaPackageInstallForm.GetSelectedRelease(out ARelease: TSimbaPackageRelease): Boolean;
begin
  Result := (VersionsList.ItemIndex > -1) and (not VersionsList.Items[VersionsList.ItemIndex].Contains('branch'));
  if Result then
    ARelease := TSimbaPackageRelease(Pointer(VersionsList.Items.Objects[VersionsList.ItemIndex])^);
end;

function TSimbaPackageInstallForm.GetSelectedBranch(out ABranch: TSimbaPackageBranch): Boolean;
begin
  Result := (VersionsList.ItemIndex > -1) and VersionsList.Items[VersionsList.ItemIndex].Contains('branch');
  if Result then
    ABranch := TSimbaPackageBranch(Pointer(VersionsList.Items.Objects[VersionsList.ItemIndex])^);
end;

procedure TSimbaPackageInstallForm.Log(S: String);
begin
  Output.Append(S);
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
  Options: TSimbaPackageInstallOptions;
  Rel: TSimbaPackageRelease;
  Branch: TSimbaPackageBranch;
begin
  Options := Default(TSimbaPackageInstallOptions);
  Options.Path := PathEdit.Text;
  Options.Flat := FlatCheckbox.Checked;
  Options.IgnoreList := IgnoreListMemo.Lines.ToStringArray();
  Options.AutoUpdate := False;

  if GetSelectedRelease(Rel) and (SimbaQuestionDlg('Install Package', 'Install "%s" to "%s" ?'.Format([FPackage.Info.FullName, ExtractRelativePath(GetSimbaPath(), Options.Path)])) = ESimbaDialogResult.YES) then
    FInstaller.Install(Rel, Options)
  else
  if GetSelectedBranch(Branch) and (SimbaQuestionDlg('Install Package', 'Install "%s" to "%s" ?'.Format([FPackage.Info.FullName, ExtractRelativePath(GetSimbaPath(), Options.Path)])) = ESimbaDialogResult.YES) then
    FInstaller.InstallBranch(Branch, Options.Path);
end;

end.

