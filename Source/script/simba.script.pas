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
  simba.script_compiler, simba.script_communication, simba.script_debugger,
  simba.client, simba.script_plugin, simba.mufasatypes;

type
  TSimbaScriptState = TInitBool;

const
  SCRIPT_STATE_RUN   = bTrue;
  SCRIPT_STATE_STOP  = bFalse;
  SCRIPT_STATE_PAUSE = bUnknown;

type
  TSimbaScript = class
  protected
    FState: TSimbaScriptState;
    FTargetWindow: TWindowHandle;

    FScript: String;
    FScriptFileName: String;

    FCompiler: TSimbaScript_Compiler;
    FCompileTime: Double;
    FRunningTime: Double;

    FClient: TClient;
    FSimbaCommunication: TSimbaCommunicationClient;

    FDebugger: TSimbaScript_Debugger;
    FPlugins: TSimbaScriptPluginArray;

    procedure Resumed; virtual;
    procedure Paused; virtual;
    procedure Stopped; virtual;

    procedure DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
    function DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure SetSimbaCommunication(Value: String);
    procedure SetTargetWindow(Value: String);
    procedure SetState(Value: TSimbaScriptState);
  public
    property CompileTime: Double read FCompileTime;
    property RunningTime: Double read FRunningTime;

    property Compiler: TSimbaScript_Compiler read FCompiler;

    property SimbaCommunication: String write SetSimbaCommunication;

    property Client: TClient read FClient;
    property TargetWindow: String write SetTargetWindow;
    property Debugger: TSimbaScript_Debugger read FDebugger write FDebugger;

    function CanInvoke: Boolean;
    procedure Invoke(Method: TSimbaMethod; DoFree: Boolean = False);

    function Compile: Boolean;
    function Run: Boolean;

    property State: TSimbaScriptState read FState write SetState;
    property Script: String read FScript write FScript;
    property ScriptFileName: String read FScriptFileName write FScriptFileName;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  fileutil, forms, lazloggerbase,
  simba.files, simba.datetime, simba.script_compiler_onterminate;

procedure TSimbaScript.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  DebugLn(Hint);
end;

function TSimbaScript.DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), GetIncludePath(), GetSimbaPath()])) then
    FileName := '';
end;

function TSimbaScript.DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  Plugin: TSimbaScriptPlugin;
  CurrentDirectory, FileName: String;
begin
  Directive := UpperCase(Directive);

  Result := Directive.IndexOfAny(['ERROR', 'LOADLIB', 'LIBPATH', 'IFHASLIB', 'IFHASFILE', 'INCLUDE_ALL', 'ENV']) > -1;

  if Result then
  try
    if InIgnore or InPeek or (Argument = '') then
      Exit;

    if FileExists(Sender.Tokenizer.FileName) then
      CurrentDirectory := ExtractFileDir(Sender.Tokenizer.FileName)
    else
      CurrentDirectory := '';

    case Directive.ToUpper() of
      'LOADLIB':
        begin
          if not FindPlugin(Argument, [CurrentDirectory, GetPluginPath(), GetSimbaPath()]) then
            raise Exception.Create('Plugin "' + Argument + '" not found');

          Plugin := TSimbaScriptPlugin.Create(Argument);
          Plugin.Import(FCompiler);

          FPlugins := FPlugins + [Plugin];
        end;

      'LIBPATH':
        begin
          if not FindPlugin(Argument, [CurrentDirectory, GetPluginPath(), GetSimbaPath()]) then
            Argument := '';

          FCompiler.pushTokenizer(TLapeTokenizerString.Create(#39 + Argument + #39));
        end;

      'IFHASLIB':
        begin
          if not FindPlugin(Argument, [CurrentDirectory, GetPluginPath(), GetSimbaPath()]) then
            FCompiler.pushConditional((not InIgnore) and True, Sender.DocPos)
          else
            FCompiler.pushConditional((not InIgnore) and False, Sender.DocPos);
        end;

      'IFHASFILE':
        begin
          FCompiler.pushConditional((not InIgnore) and FindFile(Argument, '', [CurrentDirectory, GetIncludePath(), GetSimbaPath()]), Sender.DocPos);
        end;

      'ERROR':
        begin
          raise Exception.Create('User defined error: "' + Argument + '"');
        end;

      'ENV':
        begin
          FCompiler.pushTokenizer(TLapeTokenizerString.Create(#39 + GetEnvironmentVariable(Argument) + #39));
        end;
    end;
  except
    on E: Exception do
      raise lpException.Create(E.Message, Sender.DocPos);
  end;
end;

procedure TSimbaScript.SetSimbaCommunication(Value: String);
begin
  if (Value = '') then
    Exit;

  FSimbaCommunication := TSimbaCommunicationClient.Create(Value);
end;

procedure TSimbaScript.SetTargetWindow(Value: String);
begin
  if (Value = '') then
    Exit;

  FTargetWindow := StrToInt64Def(Value, 0);
end;

function TSimbaScript.Compile: Boolean;
begin
  FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFileName));
  FCompiler.OnFindFile := @DoCompilerFindFile;
  FCompiler.OnHint := @DoCompilerHint;
  FCompiler.OnHandleDirective := @DoCompilerHandleDirective;

  if (FSimbaCommunication = nil) then
    FCompiler.addBaseDefine('SIMBAHEADLESS');

  FCompiler.Import();
  if (FDebugger <> nil) then
    FDebugger.Compile();

  FCompileTime := HighResolutionTime();
  FCompiler.Compile();
  FCompileTime := HighResolutionTime() - FCompileTime;

  Result := True;
end;

function TSimbaScript.Run: Boolean;
begin
  FClient := TClient.Create();
  FClient.MOCR.FontPath := GetFontPath();
  if (FTargetWindow > 0) then
    FClient.IOManager.SetTarget(FTargetWindow);

  // Only available at runtime
  PClient(FCompiler['Client'].Ptr)^ := FClient;
  PString(FCompiler['ScriptFile'].Ptr)^ := FScriptFileName;

  FRunningTime := HighResolutionTime();
  try
    if (FDebugger <> nil) then
      FDebugger.Run();

    RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FState);
  finally
    FRunningTime := HighResolutionTime() - FRunningTime;

    Stopped();
  end;

  Result := True;
end;

constructor TSimbaScript.Create;
begin
  inherited Create();

  FState := SCRIPT_STATE_RUN;
end;

destructor TSimbaScript.Destroy;
begin
  if (FClient <> nil) then
    FreeAndNil(FClient);
  if (FCompiler <> nil) then
    FreeAndNil(FCompiler);
  if (FSimbaCommunication <> nil) then
    FreeAndNil(FSimbaCommunication);

  inherited Destroy();
end;

procedure TSimbaScript.Resumed;
begin
  FPlugins.CallOnResume();
end;

procedure TSimbaScript.Paused;
begin
  FPlugins.CallOnPause();
end;

procedure TSimbaScript.Stopped;
begin
  FPlugins.CallOnStop();

  CallOnTerminateMethods(FCompiler);

  if (FDebugger <> nil) then
  begin
    FDebugger.Terminate();
    FDebugger.WaitFor();
  end;
end;

procedure TSimbaScript.SetState(Value: TSimbaScriptState);
begin
  FState := Value;

  case FState of
    SCRIPT_STATE_PAUSE: Paused();
    SCRIPT_STATE_RUN:   Resumed();
  end;

  if (FSimbaCommunication <> nil) then
    FSimbaCommunication.Invoke(TSimbaMethod_ScriptStateChanged.Create(Ord(FState)));
end;

function TSimbaScript.CanInvoke: Boolean;
begin
  Result := FSimbaCommunication <> nil;
end;

procedure TSimbaScript.Invoke(Method: TSimbaMethod; DoFree: Boolean);
begin
  try
    if (FSimbaCommunication = nil) then
      raise Exception.Create(Method.ClassName + ' not available when running headless');

    FSimbaCommunication.Invoke(Method);
  finally
    if DoFree then
      Method.Free();
  end;
end;

end.

