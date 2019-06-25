unit simba.package_buttons;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, comctrls, graphtype, imglist, buttons, extctrls, graphics,
  simba.package;

type
  TSimbaPackage_ToolButton = class(TToolButton)
  protected
    FUpdater: TTimer;
    FUpdates: TStringList;

    function GetPage(URL: String; AllowedResponseCodes: array of Int32): String;

    procedure CheckForUpdates;
    procedure UpdateAppearance;

    procedure DoTimerExecute(Sender: TObject);
  public
    procedure GetCurrentIcon(var ImageList: TCustomImageList; var AIndex: Integer; var AEffect: TGraphicsDrawEffect); override;

    constructor Create(ToolBar: TToolBar); reintroduce;
    destructor Destroy; override;
  end;

  TSpeedButton_Flasher = class(TComponent)
  protected
    FButton: TSpeedButton;
    FTimer: TTimer;
    FAlpha: Int32;
    FIncreasement: Int32;
    FBaseColor: TColor;
    FBlendColor: TColor;
    FPaused: Boolean;

    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);

    procedure DoMouseEnter(Sender: TObject);
    procedure DoMouseLeave(Sender: TObject);

    procedure DoTimerExecute(Sender: TObject);
  public
    property BaseColor: TColor read FBaseColor write FBaseColor;
    property BlendColor: TColor read FBlendColor write FBlendColor;
    property Enabled: Boolean read GetEnabled write SetEnabled;

    constructor Create(Button: TSpeedButton); reintroduce;
  end;

  TSpeedButton_Helper = class helper for TSpeedButton
  private
    function GetFlasher: TSpeedButton_Flasher;
    function GetFlashing: Boolean;
    procedure SetFlashing(Value: Boolean);
  public
    property Flashing: Boolean read GetFlashing write SetFlashing;
    property Flasher: TSpeedButton_Flasher read GetFlasher;
  end;

implementation

uses
  forms, math,
  simba.package_form, simba.package_github_releases, simba.httpclient_async;

function TSimbaPackage_ToolButton.GetPage(URL: String; AllowedResponseCodes: array of Int32): String;
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

procedure TSimbaPackage_ToolButton.CheckForUpdates;
var
  Packages: TSimbaPackageList;
  Package: TSimbaPackage;
  Release: TSimbaPackage_GithubRelease;
begin
  Packages := TSimbaPackageList.Create();
  Packages.Load();

  try
    FUpdates.Clear();
    FUpdates.Add('Simba Packages');

    for Package in Packages do
    begin
      Package.GetPage := @Self.GetPage;
      if Package.InstalledVersion = 'master' then
        Continue;

      Release := Package.Releases.LatestRelease;
      if (Release.Time > Package.InstalledVersionTime) then
        FUpdates.Add('%s (%s) can be updated to version %s', [Package.Name, Package.InstalledVersion, Release.Version]);
    end;
  finally
    Packages.Free();
  end;

  TThread.Synchronize(TThread.CurrentThread, @UpdateAppearance);
end;

procedure TSimbaPackage_ToolButton.UpdateAppearance;
begin
  Hint := FUpdates.Text.Trim();
  ImageIndex := Min(FUpdates.Count - 1, 1);
end;

procedure TSimbaPackage_ToolButton.DoTimerExecute(Sender: TObject);
begin
  TThread.ExecuteInThread(@CheckForUpdates);

  TTimer(Sender).Interval := 60000;
end;

procedure TSimbaPackage_ToolButton.GetCurrentIcon(var ImageList: TCustomImageList; var AIndex: Integer; var AEffect: TGraphicsDrawEffect);
begin
  ImageList := SimbaPackageForm.Images;

  AIndex := ImageIndex;
  AEffect := gdeNormal;
end;

constructor TSimbaPackage_ToolButton.Create(ToolBar: TToolBar);
var
  i: Int32;
begin
  inherited Create(ToolBar);

  FUpdates := TStringList.Create();

  for i := 0 to ToolBar.ButtonCount - 1 do
    if (ToolBar.Buttons[i].Left + ToolBar.ButtonWidth > Left) then
      Left := ToolBar.Buttons[i].Left + ToolBar.ButtonWidth;

  Parent := ToolBar;
  Hint := 'Simba Packages';
  ImageIndex := 0;

  with TTimer.Create(Self) do
  begin
    Enabled := True;
    OnTimer := @DoTimerExecute;
  end;
end;

destructor TSimbaPackage_ToolButton.Destroy;
begin
  FUpdates.Free();

  inherited Destroy();
end;

function TSpeedButton_Flasher.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TSpeedButton_Flasher.SetEnabled(Value: Boolean);
begin
  if Value <> FTimer.Enabled then
  begin
    FAlpha := 100;
    FIncreasement := -5;
    FButton.Color := FBaseColor;
    FTimer.Enabled := Value
  end;
end;

procedure TSpeedButton_Flasher.DoMouseEnter(Sender: TObject);
begin
  if Enabled then
  begin
    FPaused := True;

    Enabled := False;
  end;
end;

procedure TSpeedButton_Flasher.DoMouseLeave(Sender: TObject);
begin
  if FPaused then
  begin
    FPaused := False;

    Enabled := True;
  end;
end;

procedure TSpeedButton_Flasher.DoTimerExecute(Sender: TObject);

  function Blend(Left, Right: Int32; Alpha: Single): Int32;
  begin
    Result := RGBToColor(
      Round((Red(Right) * (1.0 - Alpha)) + (Red(Left) * Alpha)),
      Round((Green(Right) * (1.0 - Alpha)) + (Green(Left) * Alpha)),
      Round((Blue(Right) * (1.0 - Alpha)) + (Blue(Left) * Alpha))
    );
  end;

begin
  FButton.Color := Blend(FBaseColor, FBlendColor, FAlpha / 100);
  FAlpha := FAlpha + FIncreasement;

  if (FAlpha = 100) then
    FIncreasement := -5
  else
  if (FAlpha = 0) then
    FIncreasement := 5;
end;

constructor TSpeedButton_Flasher.Create(Button: TSpeedButton);
begin
  Name := 'TSpeedButton_Flasher';

  FButton := Button;
  FButton.Transparent := False;
  FButton.OnMouseEnter := @DoMouseEnter;
  FButton.OnMouseLeave := @DoMouseLeave;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.OnTimer := @DoTimerExecute;
  FTimer.Enabled := False;
end;

function TSpeedButton_Helper.GetFlasher: TSpeedButton_Flasher;
begin
  if FindComponent('TSpeedButton_Flasher') = nil then
    InsertComponent(TSpeedButton_Flasher.Create(Self));

  Result := TSpeedButton_Flasher(FindComponent('TSpeedButton_Flasher'));
end;

function TSpeedButton_Helper.GetFlashing: Boolean;
begin
  if FindComponent('TSpeedButton_Flasher') = nil then
    InsertComponent(TSpeedButton_Flasher.Create(Self));

  Result := TSpeedButton_Flasher(FindComponent('TSpeedButton_Flasher')).Enabled;
end;

procedure TSpeedButton_Helper.SetFlashing(Value: Boolean);
begin
  if FindComponent('TSpeedButton_Flasher') = nil then
    InsertComponent(TSpeedButton_Flasher.Create(Self));

  TSpeedButton_Flasher(FindComponent('TSpeedButton_Flasher')).Enabled := Value;
end;

end.

