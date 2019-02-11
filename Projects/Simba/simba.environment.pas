unit simba.environment;

(*
  Simba environment class:

  • Creates and manages base directories for Simba
  • If a empty string is passed the path will be reset to default
  • Simba's settings keeps this class updated with the correct paths if changed by the user

  DataPath:
    • Stores Simba's application data
    • Default: "Simba/Data/"

  LibPath:
    • Stores libraries that Simba uses internally to function

    Windows default:
      • "Simba/Data/Lib32/" or "Simba/Data/Lib64" depending on Simba's bitness
      • SetDLLDirectory is used. This allows two different simba bitness working correctly

    Linux default:
      • "Simba" (SimbaPath)
      • There is no equivalent to SetDLLDirectory (adding path at runtime)
      • We only (officially) support x64 so this shouldn't cause any issues

  PluginPath:
    • Stores plugins for scripts
    • Default: "Simba/Plugins/"

  IncludePath:
    • Stores includes for scripts
    • Default: "Simba/Includes/"

  FontPath:
    • Stores fonts for the OCR
    • Default: "Simba/Fonts/"

  ScriptPath:
    • Directory to store scripts in
    • Default: "Simba/Scripts/"

  SimbaPath:
    • Directory which contains the Simba executable
    • Default "Simba/"

*)

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, forms;

type
  TSimbaEnvironment = class
  protected
    FDataPath: String;
    FFontPath: String;
    FIncludePath: String;
    FLibPath: String;
    FPluginPath: String;
    FScriptPath: String;
    FSimbaPath: String;

    procedure SetDataPath(Value: String);
    procedure SetFontPath(Value: String);
    procedure SetIncludePath(Value: String);
    procedure SetLibPath(Value: String);
    procedure SetPluginPath(Value: String);
    procedure SetScriptPath(Value: String);
  public
    property DataPath: String read FDataPath write SetDataPath;
    property LibPath: String read FLibPath write SetLibPath;
    property PluginPath: String read FPluginPath write SetPluginPath;
    property FontPath: String read FFontPath write SetFontPath;
    property IncludePath: String read FIncludePath write SetIncludePath;
    property ScriptPath: String read FScriptPath write SetScriptPath;
    property SimbaPath: String read FSimbaPath;

    constructor Create;
  end;

var
  SimbaEnvironment: TSimbaEnvironment;

implementation

uses
  lazfileutils;

{$IFDEF WINDOWS}
function SetDLLDirectory(Directory: PChar): LongBool; stdcall; external 'kernel32.dll' name 'SetDllDirectoryA';
{$ENDIF}

procedure TSimbaEnvironment.SetScriptPath(Value: String);
begin
  FScriptPath := Value;
  if FScriptPath = '' then
    FScriptPath := Application.Location + 'Scripts' + DirectorySeparator;

  ForceDirectories(FScriptPath);
end;

procedure TSimbaEnvironment.SetDataPath(Value: String);
begin
  FDataPath := Value;
  if FDataPath = '' then
    FDataPath := Application.Location + 'Data' + DirectorySeparator;

  ForceDirectories(FDataPath);
end;

procedure TSimbaEnvironment.SetFontPath(Value: String);
begin
  FFontPath := Value;
  if FFontPath = '' then
    FFontPath := Application.Location + 'Fonts' + DirectorySeparator;

  ForceDirectories(FFontPath);
end;

procedure TSimbaEnvironment.SetIncludePath(Value: String);
begin
  FIncludePath := Value;
  if FIncludePath = '' then
    FIncludePath := Application.Location + 'Includes' + DirectorySeparator;

  ForceDirectories(FIncludePath);
end;

procedure TSimbaEnvironment.SetLibPath(Value: String);
begin
  FLibPath := Value;
  if FLibPath = '' then
  begin
    {$IFDEF WINDOWS}
    FLibPath := FDataPath + {$IFDEF CPU32}'lib32'{$ELSE}'lib64'{$ENDIF} + DirectorySeparator;
    {$ELSE}
    FLibPath := FSimbaPath;
    {$ENDIF}
  end;

  ForceDirectories(FLibPath);

  {$IFDEF WINDOWS}
  SetDLLDirectory(PChar(FLibPath));
  {$ENDIF}
end;

procedure TSimbaEnvironment.SetPluginPath(Value: String);
begin
  FPluginPath := Value;
  if FPluginPath = '' then
    FPluginPath := Application.Location + 'Plugins' + DirectorySeparator;
end;

constructor TSimbaEnvironment.Create;
begin
  FSimbaPath := Application.Location;

  // Load defaults
  DataPath := '';
  LibPath := '';
  PluginPath := '';
  IncludePath := '';
  FontPath := '';
  ScriptPath := '';
end;

initialization
  SimbaEnvironment := TSimbaEnvironment.Create();

finalization
  SimbaEnvironment.Free();

end.

