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
  TSimbaScript = class
  protected
    FState: TInitBool;
    FTargetWindow: TWindowHandle;

    FScript: String;
    FScriptFileName: String;

    FCompiler: TSimbaScript_Compiler;
    FCompileTime: Double;
    FRunningTime: Double;

    FClient: TClient;

    FDebugger: TSimbaScript_Debugger;
    FPlugins: TSimbaScriptPluginArray;
    FSimbaCommunication: TSimbaScriptCommunication;

    procedure Resumed; virtual;
    procedure Paused; virtual;
    procedure Stopped; virtual;

    procedure DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
    function DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

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

    property Client: TClient read FClient;
    property TargetWindow: String write SetTargetWindow;
    property Debugger: TSimbaScript_Debugger read FDebugger write FDebugger;

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
  fileutil, forms, lazloggerbase,
  simba.outputform, simba.files, simba.datetime, simba.script_compiler_onterminate,
  simba.helpers_string;

procedure TSimbaScript.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  DebugLnHint(Hint);
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
  CurrentDirectory: String;
begin
  Result := Directive.ContainsAny(['ERROR', 'LOADLIB', 'LIBPATH', 'IFHASLIB', 'IFHASFILE', 'ENV'], False);

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

  State := ESimbaScriptState.STATE_RUNNING;
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

procedure TSimbaScript.SetState(Value: ESimbaScriptState);
begin
  case Value of
    ESimbaScriptState.STATE_RUNNING:
      begin
        FState := bTrue;

        Resumed();
      end;

    ESimbaScriptState.STATE_STOP:
      begin
        FState := bFalse;

        Stopped();
      end;

    ESimbaScriptState.STATE_PAUSED:
      begin
        FState := bUnknown;

        Paused();
      end;
  end;

  if (FSimbaCommunication <> nil) then
    FSimbaCommunication.ScriptStateChanged(Value);
end;

end.

