{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lptypes, lpvartypes, lpcompiler, lpparser, lpinterpreter, lpmessages,
  simba.script_compiler, simba.script_communication,
  simba.script_plugin, simba.mufasatypes, simba.windowhandle;

type
  TSimbaScript = class
  protected
    FState: TInitBool;
    FUserTerminated: Boolean;
    FTargetWindow: TWindowHandle;

    FScript: String;
    FScriptFileName: String;

    FCompiler: TSimbaScript_Compiler;
    FCompileTime: Double;
    FRunningTime: Double;

    FPlugins: TSimbaScriptPluginArray;
    FSimbaCommunication: TSimbaScriptCommunication;

    procedure DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
    function DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
    function DoFindMacro(Sender: TLapeCompiler; Name: lpString; var Value: lpString): Boolean;

    function GetState: ESimbaScriptState;

    procedure SetSimbaCommunicationServer(Value: String);
    procedure SetTargetWindow(Value: String);
    procedure SetState(Value: ESimbaScriptState);
  public
    property CompileTime: Double read FCompileTime;
    property RunningTime: Double read FRunningTime;

    property Compiler: TSimbaScript_Compiler read FCompiler;

    property SimbaCommunication: TSimbaScriptCommunication read FSimbaCommunication;
    property SimbaCommunicationServer: String write SetSimbaCommunicationServer;

    property TargetWindow: String write SetTargetWindow;

    function Compile: Boolean;
    function Run: Boolean;

    property State: ESimbaScriptState read GetState write SetState;
    property Script: String read FScript write FScript;
    property ScriptFileName: String read FScriptFileName write FScriptFileName;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.env, simba.datetime, simba.httpclient, simba.target;

procedure TSimbaScript.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  SimbaDebugLn([EDebugLn.YELLOW], Hint);
end;

function TSimbaScript.DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), GetIncludePath(), GetSimbaPath()])) then
    FileName := '';
end;

function TSimbaScript.DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

  function DoIncludeFromURL: Boolean;
  var
    URL, Source: String;
  begin
    Result := ((Directive = 'I') or (Directive = 'INCLUDE')) and (Argument.ToUpper().Between('URL(', ')') <> '');

    if Result then
    begin
      URL    := Argument.ToUpper().Between('URL(', ')');
      Source := TSimbaHTTPClient.SimpleGet(URL, [EHTTPStatus.OK]);

      FCompiler.pushTokenizer(TLapeTokenizerString.Create(Source, '!' + URL));
    end;
  end;

  function DoLoadLib: Boolean;
  var
    Plugin: TSimbaScriptPlugin;
  begin
    Result := True;
    if InIgnore or InPeek then
      Exit;

    if not FindPlugin(Argument, [FCompiler.CurrentDir(), GetPluginPath(), GetSimbaPath()]) then
      raise Exception.Create('Plugin "' + Argument + '" not found');

    CopyPlugin(Argument);

    Plugin := TSimbaScriptPlugin.Create(Argument);
    Plugin.Import(FCompiler);

    FPlugins := FPlugins + [Plugin];
  end;

  function DoHasLib: Boolean;
  begin
    Result := True;

    if InIgnore then
      FCompiler.pushConditional(False, Sender.DocPos)
    else
      FCompiler.pushConditional(FindPlugin(Argument, [FCompiler.CurrentDir(), GetPluginPath(), GetSimbaPath()]), Sender.DocPos);
  end;

  function DoHasFile: Boolean;
  begin
    Result := True;

    if InIgnore then
      FCompiler.pushConditional(False, Sender.DocPos)
    else
      FCompiler.pushConditional(FindFile(Argument, '', [FCompiler.CurrentDir(), GetIncludePath(), GetSimbaPath()]), Sender.DocPos);
  end;

  function DoError: Boolean;
  begin
    Result := True;
    if InIgnore or InPeek then
      Exit;

    raise Exception.Create('User defined error: "' + Argument + '"');
  end;

begin
  Directive := UpperCase(Directive);

  try
    Result := DoIncludeFromURL();
    if Result then
      Exit;

    case Directive of
      'LOADLIB':   Result := DoLoadLib();
      'IFHASLIB':  Result := DoHasLib();
      'IFHASFILE': Result := DoHasFile();
      'ERROR':     Result := DoError();
    end;
  except
    on E: Exception do
      raise lpException.Create(E.Message, Sender.DocPos);
  end;
end;

function TSimbaScript.DoFindMacro(Sender: TLapeCompiler; Name: lpString; var Value: lpString): Boolean;

  // {$MACRO ENV(HOME)}
  function DoEnvVar: Boolean;
  var
    EnvVar: String;
  begin
    EnvVar := Name.ToUpper().Between('ENV(', ')');

    Result := EnvVar <> '';
    if Result then
      Value := '"' + GetEnvironmentVariable(EnvVar) + '"';
  end;

  // {$MACRO LIBPATH(plugin.dll)}
  function DoLibPath: Boolean;
  var
    Lib: String;
  begin
    Lib :=  Name.ToUpper().Between('LIBPATH(', ')');

    Result := Lib <> '';
    if Result then
    begin
      if not FindPlugin(Lib, [FCompiler.CurrentDir(), GetPluginPath(), GetSimbaPath()]) then
        Lib := '';

      Value := '"' + Lib + '"';
    end;
  end;

begin
  Result := DoEnvVar() or DoLibPath();
end;

function TSimbaScript.GetState: ESimbaScriptState;
begin
  case FState of
    bTrue:    Result := ESimbaScriptState.STATE_RUNNING;
    bFalse:   Result := ESimbaScriptState.STATE_STOP;
    bUnknown: Result := ESimbaScriptState.STATE_PAUSED;
  end;
end;

procedure TSimbaScript.SetSimbaCommunicationServer(Value: String);
begin
  if (Value = '') then
    Exit;

  FSimbaCommunication := TSimbaScriptCommunication.Create(Value);
end;

procedure TSimbaScript.SetTargetWindow(Value: String);
begin
  if (Value = '') then
    Exit;

  FTargetWindow.FromString(Value);
end;

function TSimbaScript.Compile: Boolean;
begin
  FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFileName));
  FCompiler.OnFindFile := @DoCompilerFindFile;
  FCompiler.OnHint := @DoCompilerHint;
  FCompiler.OnHandleDirective := @DoCompilerHandleDirective;
  FCompiler.OnFindMacro := @DoFindMacro;

  if (FSimbaCommunication = nil) then
    FCompiler.addBaseDefine('SIMBAHEADLESS');

  FCompiler.Import();

  FCompileTime := HighResolutionTime();
  FCompiler.Compile();
  FCompileTime := HighResolutionTime() - FCompileTime;

  Result := True;
end;

function TSimbaScript.Run: Boolean;
begin
  if (FTargetWindow = 0) or (not FTargetWindow.IsValid()) then
    FTargetWindow := GetDesktopWindow();

  PSimbaTarget(FCompiler['Target'].Ptr)^.SetWindow(FTargetWindow);

  PString(FCompiler['ScriptFile'].Ptr)^ := FScriptFileName;
  PString(FCompiler['ScriptName'].Ptr)^ := ExtractFileName(FScriptFileName);

  FRunningTime := HighResolutionTime();
  try
    RunCode(FCompiler.Emitter, FState);
  finally
    FRunningTime := HighResolutionTime() - FRunningTime;

    FPlugins.CallOnStop();

    if FUserTerminated then
      FCompiler.CallProc('_CallOnUserTerminate', True);
    FCompiler.CallProc('_CallOnTerminate', True);
  end;

  Result := True;
end;

constructor TSimbaScript.Create;
begin
  inherited Create();

  FState := bTrue;
end;

destructor TSimbaScript.Destroy;
var
  Plugin: TSimbaScriptPlugin;
begin
  for Plugin in FPlugins do
    Plugin.Free();
  FPlugins := nil;

  if (FCompiler <> nil) then
    FreeAndNil(FCompiler);
  if (FSimbaCommunication <> nil) then
    FreeAndNil(FSimbaCommunication);

  inherited Destroy();
end;

procedure TSimbaScript.SetState(Value: ESimbaScriptState);
begin
  case Value of
    ESimbaScriptState.STATE_RUNNING:
      begin
        FPlugins.CallOnResume();
        FCompiler.CallProc('_CallOnResume', True);

        FState := bTrue;
      end;

    ESimbaScriptState.STATE_PAUSED:
      begin
        FState := bUnknown;

        FPlugins.CallOnPause();
        FCompiler.CallProc('_CallOnPause', True);
      end;

    ESimbaScriptState.STATE_STOP:
      begin
        FUserTerminated := True;
        FState := bFalse;
      end;
  end;

  if (FSimbaCommunication <> nil) then
    FSimbaCommunication.ScriptStateChanged(Value);
end;

end.

