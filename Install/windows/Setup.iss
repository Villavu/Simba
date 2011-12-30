; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{524C9B9A-B57F-4FEC-89BE-292202EBA44D}
AppName=Simba
AppVerName=Simba 0.99
AppPublisherURL=http://wizzup.org/simba
AppSupportURL=http://wizzup.org/simba
AppUpdatesURL=http://wizzup.org/simba
DefaultDirName={sd}\Simba
DefaultGroupName=Simba
OutputDir=C:\simba\Install\windows\
OutputBaseFilename=setup
Compression=lzma
SolidCompression=yes
ChangesAssociations=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}";

[Dirs]
Name: "{app}\Extensions"
Name: "{app}\Includes"
Name: "{app}\Plugins"
Name: "{app}\Scripts"
; Name: "{app}\Scripts\Tests"

[Files]
Source: "C:\Simba\Simba.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\Simba\Extensions\srl.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\extension.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\msi.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\associate.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\dtm_editor.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\security.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\paster.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\CRov.sex"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Extensions\Updater.sei"; DestDir: "{app}\Extensions"; Flags: ignoreversion
Source: "C:\Simba\Includes\mml.simba"; DestDir: "{app}\Includes"; Flags: ignoreversion
; Source: "C:\Simba\settings.xml"; DestDir: "{app}\"; Flags: ignoreversion
; XXX Make sure to use a MINIMAL settings.xml XXX

; Source: "C:\Simba\Fonts\*"; DestDir: "{app}\Fonts"; Flags: ignoreversion recursesubdirs createallsubdirs
; Source: "C:\Simba\Tests\PS\*"; DestDir:"{app}\Scripts\Tests"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\Simba"; Filename: "{app}\Simba.exe"
Name: "{group}\{cm:UninstallProgram,Simba}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\Simba"; Filename: "{app}\Simba.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\Simba.exe"; Description: "{cm:LaunchProgram,Simba}"; Flags: nowait postinstall skipifsilent

[Registry]
Root: HKCR; Subkey: ".simba"; ValueType: string; ValueName: ""; ValueData: "Simba"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "Simba"; ValueType: string; ValueName: ""; ValueData: "Simba script"; Flags: uninsdeletekey
Root: HKCR; Subkey: "Simba\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\Simba.exe,0"
Root: HKCR; Subkey: "Simba\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\Simba.exe"" ""%1"""
