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
    procedure DoVersionChange(Sender: TObject);
  private
    FPackage: TSimbaPackage;
    FVersion: TSimbaPackageVersion;
    FInstaller: TSimbaPackageInstaller;

    procedure Log(S: String);
  public
    constructor Create(AOwner: TComponent; APackage: TSimbaPackage); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

uses
  simba.files,  simba.fonthelpers;

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

  VersionsList.Items.Clear();
  for I := 0 to High(FPackage.Versions) do
  begin
    if FPackage.Versions[I] is TSimbaPackageBranch then
      VersionsList.Items.AddObject(FPackage.Versions[I].Name + ' (branch)', FPackage.Versions[I])
    else
      VersionsList.Items.AddObject(FPackage.Versions[I].Name + ' ' + FPackage.Versions[I].Age, FPackage.Versions[I]);
  end;
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
  Options: TSimbaPackageInstallOptions;
begin
  if (VersionsList.ItemIndex > -1) then
  begin
    FVersion := VersionsList.Items.Objects[VersionsList.ItemIndex] as TSimbaPackageVersion;
    if not FInstaller.GetOptions(FVersion, Options) then
      Log('Version "%s" has no install options'.Format([FVersion.Name]));

    PathEdit.Text        := Options.Path;
    FlatCheckbox.Checked := Options.Flat;
    IgnoreListMemo.Lines.AddStrings(Options.IgnoreList, True);
  end else
    FVersion := nil;
end;

procedure TSimbaPackageInstallForm.Log(S: String);
begin
  Output.Append(S);
  Output.CaretXY := TPoint.Create(0, Output.Lines.Count);
end;

procedure TSimbaPackageInstallForm.DoInstallButtonClick(Sender: TObject);
var
  Options: TSimbaPackageInstallOptions;
begin
  if (FVersion = nil) then
    Exit;

  Options := Default(TSimbaPackageInstallOptions);
  Options.Path := PathEdit.Text;
  Options.Flat := FlatCheckbox.Checked;
  Options.IgnoreList := IgnoreListMemo.Lines.ToStringArray();
  Options.AutoUpdate := False;

  if QuestionDlg('Install Package', 'Install "%s" to "%s" ?'.Format([FPackage.Info.FullName, ExtractRelativePath(GetSimbaPath(), Options.Path)]), mtConfirmation, [mrYes, mrNo], 0) = mrYes then
  begin
    InstallButton.Caption := 'Installing...';
    InstallButton.Enabled := False;

    FInstaller.Install(FVersion, Options);

    InstallButton.Caption := 'Install';
    InstallButton.Enabled := True;
  end;
end;

end.

