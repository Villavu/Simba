{ FileAssociation 2013-2019 by Lainz }
unit FileAssoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, ShlObj;

type

  { TFileAssociation }

  TFileAssociation = class(TComponent)
  private
    { Options }
    FRegistry: TRegistry;
    FRegisterForAllUsers: boolean;
    FRegisterFileAssociation: boolean;
    FAddApplicationToDefaultPrograms: boolean;
    FAddExtensionToDefaultPrograms: boolean;
    FUnReg: boolean;
    { Data }
    FApplicationName: string;
    FApplicationDescription: string;
    FExtension: string;
    FExtensionName: string;
    FExtensionIcon: string;
    FAction: string;
    FActionName: string;
    FActionText: string;
    FActionIcon: string;
    procedure SetRoot;
    procedure SetFAction(AValue: string);
    procedure SetFActionIcon(AValue: string);
    procedure SetFActionName(AValue: string);
    procedure SetFActionText(AValue: string);
    procedure SetFApplicationDescription(AValue: string);
    procedure SetFApplicationName(AValue: string);
    procedure SetFExtension(AValue: string);
    procedure SetFExtensionIcon(AValue: string);
    procedure SetFExtensionName(AValue: string);
    procedure SetFRegisterForAllUsers(AValue: boolean);
    procedure SetFUnReg(AValue: boolean);
    procedure SetFAddApplicationToDefaultPrograms(AValue: boolean);
    procedure SetFAddExtensionToDefaultPrograms(AValue: boolean);
    procedure SetFRegisterFileAssociation(AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    { String remove spaces }
    function StrNoSpaces(const s: string): string;
    { Add Root\SubKey\ ValueName=ValueData }
    function WriteStringValue(SubKey: string; ValueName: string;
      ValueData: string): boolean;
    { Delete Root\SuyKey\ ValueName=ValueData }
    function DeleteValue(SubKey: string; ValueName: string): boolean;
    { Add-Delete (UnReg option) Root\SubKey\ ValueName=ValueData }
    function WriteString(SubKey: string; ValueName: string; ValueData: string): boolean;
  public
    { Registry 'Class' containing the icon }
    function WriteFileAssociationClass: boolean;
    { Add a command like 'Open', 'Edit', 'Print' or other }
    function WriteFileAssociationClassCommand: boolean;
    { Associate the 'Class' with the file extension }
    function WriteFileAssociation: boolean;
    { Add application to Default Programs (Vista+) }
    function WriteDefaultPrograms: boolean;
    { Add extension to application Default Programs page }
    function WriteDefaultProgramsAddExt: boolean;
    { Run all }
    function Execute: boolean;
    { Rebuild Icons }
    procedure ClearIconCache;
  published
    { Data }
    { 'Lazarus IDE' }
    property ApplicationName: string read FApplicationName write SetFApplicationName;
    { 'RAD for Free Pascal' }
    property ApplicationDescription: string
      read FApplicationDescription write SetFApplicationDescription;
    { '.lpr' }
    property Extension: string read FExtension write SetFExtension;
    { 'Lazarus Project' }
    property ExtensionName: string read FExtensionName write SetFExtensionName;
    { 'C:\lazarus\images\lprfile.ico' }
    property ExtensionIcon: string read FExtensionIcon write SetFExtensionIcon;
    { '"C:\lazarus\lazarus.exe" "%1"' }
    property Action: string read FAction write SetFAction;
    { 'Open' }
    property ActionName: string read FActionName write SetFActionName;
    { 'Open With Lazarus' or empty '' to use default MUI translation for 'Open', 'Print' and 'Edit' }
    property ActionText: string read FActionText write SetFActionText;
    { 'C:\lazarus\lazarus.exe,0' }
    property ActionIcon: string read FActionIcon write SetFActionIcon;
  published
    { Options }
    { True uses HKLM, false HKCU }
    property RegisterForAllUsers: boolean read FRegisterForAllUsers
      write SetFRegisterForAllUsers default True;
    { Do '.lpr' association with 'Lazarus IDE' }
    property RegisterFileAssociation: boolean
      read FRegisterFileAssociation write SetFRegisterFileAssociation default True;
    { Add 'Lazarus IDE' to 'Default Programs' requires RegisterForAllUsers:True }
    property AddApplicationToDefaultPrograms: boolean
      read FAddApplicationToDefaultPrograms write SetFAddApplicationToDefaultPrograms default
      True;
    { Add '.lpr' to 'Lazarus IDE' in 'Default Programs' requires RegisterForAllUsers:True }
    property AddExtensionToDefaultPrograms: boolean
      read FAddExtensionToDefaultPrograms write SetFAddExtensionToDefaultPrograms default True;
    { Remove from registry the current data }
    property UnReg: boolean read FUnReg write SetFUnReg default False;
  end;

procedure Register;

implementation

procedure Register;
begin
  { $I fileassociation_icon.lrs}
  RegisterComponents('System', [TFileAssociation]);
end;

{ TFileAssociation }

procedure TFileAssociation.SetRoot;
begin
  if RegisterForAllUsers then
    FRegistry.RootKey := HKEY_LOCAL_MACHINE
  else
    FRegistry.RootKey := HKEY_CURRENT_USER;
end;

procedure TFileAssociation.SetFAction(AValue: string);
begin
  if FAction = AValue then
    Exit;
  FAction := AValue;
end;

procedure TFileAssociation.SetFActionIcon(AValue: string);
begin
  if FActionIcon = AValue then
    Exit;
  FActionIcon := AValue;
end;

procedure TFileAssociation.SetFActionName(AValue: string);
begin
  if FActionName = AValue then
    Exit;
  FActionName := AValue;
end;

procedure TFileAssociation.SetFApplicationDescription(AValue: string);
begin
  if FApplicationDescription = AValue then
    Exit;
  FApplicationDescription := AValue;
end;

procedure TFileAssociation.SetFApplicationName(AValue: string);
begin
  if FApplicationName = AValue then
    Exit;
  FApplicationName := AValue;
end;

procedure TFileAssociation.SetFExtension(AValue: string);
begin
  if FExtension = AValue then
    Exit;
  FExtension := AValue;
end;

procedure TFileAssociation.SetFExtensionIcon(AValue: string);
begin
  if FExtensionIcon = AValue then
    Exit;
  FExtensionIcon := AValue;
end;

procedure TFileAssociation.SetFExtensionName(AValue: string);
begin
  if FExtensionName = AValue then
    Exit;
  FExtensionName := AValue;
end;

procedure TFileAssociation.SetFRegisterForAllUsers(AValue: boolean);
begin
  FRegisterForAllUsers := AValue;
  SetRoot;
end;

procedure TFileAssociation.SetFUnReg(AValue: boolean);
begin
  if FUnReg = AValue then
    Exit;
  FUnReg := AValue;
end;

procedure TFileAssociation.SetFAddApplicationToDefaultPrograms(AValue: boolean);
begin
  if FAddApplicationToDefaultPrograms = AValue then
    Exit;
  FAddApplicationToDefaultPrograms := AValue;
end;

procedure TFileAssociation.SetFAddExtensionToDefaultPrograms(AValue: boolean);
begin
  if FAddExtensionToDefaultPrograms = AValue then
    Exit;
  FAddExtensionToDefaultPrograms := AValue;
end;

procedure TFileAssociation.SetFRegisterFileAssociation(AValue: boolean);
begin
  if FRegisterFileAssociation = AValue then
    Exit;
  FRegisterFileAssociation := AValue;
end;

constructor TFileAssociation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegistry := TRegistry.Create;
  AddApplicationToDefaultPrograms := True;
  AddExtensionToDefaultPrograms := True;
  RegisterFileAssociation := True;
  UnReg := False;
  RegisterForAllUsers := True;
end;

destructor TFileAssociation.Destroy;
begin
  FRegistry.Free;
  inherited Destroy;
end;

function TFileAssociation.Execute: boolean;
var
  b1, b2, b3, b4, b5: boolean;
begin
  b1:=False;
  b2:=False;
  b3:=False;
  b4:=False;
  b5:=False;

  b1 := WriteFileAssociationClass;
  b2 := WriteFileAssociationClassCommand;

  if RegisterFileAssociation then
    b3 := WriteFileAssociation;

  if RegisterForAllUsers then
  begin
    if AddApplicationToDefaultPrograms then
      b4 := WriteDefaultPrograms;
    if AddExtensionToDefaultPrograms then
      b5 := WriteDefaultProgramsAddExt;
  end;

  Result := False;
  if b1 and b2 and b3 and b4 and b5 then
    Result := True;
end;

function TFileAssociation.WriteStringValue(SubKey: string; ValueName: string;
  ValueData: string): boolean;
begin
  Result := FRegistry.OpenKey(SubKey, True);

  if Result then
  begin
    FRegistry.WriteString(ValueName, ValueData);
    FRegistry.CloseKey;
  end;
end;

function TFileAssociation.DeleteValue(SubKey: string; ValueName: string): boolean;
begin
  Result := FRegistry.OpenKey(SubKey, True);
  if Result then
  begin
    FRegistry.DeleteValue(ValueName);
    FRegistry.DeleteKey(ValueName);
    FRegistry.CloseKey;
  end;
end;

procedure TFileAssociation.SetFActionText(AValue: string);
begin
  if FActionText = AValue then
    Exit;
  FActionText := AValue;
end;

procedure TFileAssociation.ClearIconCache;
begin
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

function TFileAssociation.StrNoSpaces(const s: string): string;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
end;

function TFileAssociation.WriteString(SubKey: string; ValueName: string;
  ValueData: string): boolean;
begin
  if not UnReg then
    Result := WriteStringValue(SubKey, ValueName, ValueData)
  else
    Result := DeleteValue(SubKey, ValueName);
end;

function TFileAssociation.WriteFileAssociationClass: boolean;
var
  b1, b2: boolean;
  sub: string;
begin
  sub := 'Software\Classes\' + StrNoSpaces(ApplicationName) +
    '.AssocFile.' + StrNoSpaces(ExtensionName);

  b1 := WriteString(sub, '', ExtensionName);
  b2 := WriteString(sub + '\DefaultIcon', '', ExtensionIcon);

  Result := False;
  if b1 and b2 then
    Result := True;
end;

function TFileAssociation.WriteFileAssociationClassCommand: boolean;
var
  b1, b2, b3: boolean;
  sub: string;
begin
  sub := 'Software\Classes\' + StrNoSpaces(ApplicationName) +
    '.AssocFile.' + StrNoSpaces(ExtensionName) + '\Shell\' + StrNoSpaces(ActionName);

  b1 := WriteString(sub, '', ActionText);
  b2 := WriteString(sub, 'Icon', ActionIcon);
  b3 := WriteString(sub + '\Command', '', Action);

  Result := False;
  if b1 and b2 and b3 then
    Result := True;
end;

function TFileAssociation.WriteFileAssociation: boolean;
begin
  Result := WriteString('Software\Classes\' + Extension, '',
    StrNoSpaces(ApplicationName) + '.AssocFile.' + StrNoSpaces(ExtensionName));
end;

function TFileAssociation.WriteDefaultPrograms: boolean;
var
  b1, b2, b3, b4: boolean;
  sub: string;
begin
  sub := 'Software\' + StrNoSpaces(ApplicationName) + '\Capabilities';

  b1 := WriteString(sub, '', '');
  b2 := WriteString(sub, 'ApplicationName', ApplicationName);
  b3 := WriteString(sub, 'ApplicationDescription', ApplicationDescription);
  b4 := WriteString('Software\RegisteredApplications',
    StrNoSpaces(ApplicationName), sub);

  Result := False;
  if b1 and b2 and b3 and b4 then
    Result := True;
end;

function TFileAssociation.WriteDefaultProgramsAddExt: boolean;
begin
  Result := WriteString('Software\' + StrNoSpaces(ApplicationName) +
    '\Capabilities\FileAssociations', Extension, StrNoSpaces(ApplicationName) +
    '.AssocFile.' + StrNoSpaces(ExtensionName));
end;

end.
