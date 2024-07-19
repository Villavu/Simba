{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.script;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpcompiler, lpparser, lpinterpreter, lpmessages,
  simba.base,
  simba.script_compiler,
  simba.script_communication,
  simba.script_plugin;

type
  PSimbaScript = ^TSimbaScript;
  TSimbaScript = class(TObject)
  protected
    FUserTerminated: Boolean;
    FTargetWindow: TWindowHandle;
    FHints: Boolean;

    FScript: String;
    FScriptFileName: String;

    FCompiler: TSimbaScript_Compiler;
    FCodeRunner: TLapeCodeRunner;
    FCompileTime: Double;
    FRunningTime: Double;

    FPlugins: TSimbaScriptPluginList;
    FSimbaCommunication: TSimbaScriptCommunication;

    function DoCompilerPreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
    function DoCompilerMacro(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
    procedure DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
    procedure DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString);
    function DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
    function GetState: ESimbaScriptState;

    procedure SetTargetWindow(Value: String);
    procedure SetState(Value: ESimbaScriptState);
  public
    property CompileTime: Double read FCompileTime;
    property RunningTime: Double read FRunningTime;

    property Compiler: TSimbaScript_Compiler read FCompiler;

    property SimbaCommunication: TSimbaScriptCommunication read FSimbaCommunication;

    property TargetWindow: String write SetTargetWindow;
    property Hints: Boolean write FHints;

    function Compile: Boolean;
    function Run: Boolean;

    property State: ESimbaScriptState read GetState write SetState;
    property Script: String read FScript write FScript;
    property ScriptFileName: String read FScriptFileName write FScriptFileName;

    constructor Create; overload;
    constructor Create(Communication: TSimbaScriptCommunication); overload;
    constructor Create(FileName: String; Communication: TSimbaScriptCommunication = nil); overload;
    destructor Destroy; override;
  end;

implementation

uses
  simba.env, simba.fs, simba.datetime, simba.target, simba.vartype_windowhandle,
  simba.script_pluginloader;

function TSimbaScript.DoCompilerPreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
begin
  Value := '';

  case Name of
    'LIBLOADED': Value := BoolToStr(FindLoadedPlugin(SimbaEnv.FindPlugin(Argument, [Sender.Tokenizer.FileName])) <> '', True);
    'LIBEXISTS': Value := BoolToStr(SimbaEnv.HasPlugin(Argument, [Sender.Tokenizer.FileName]), True);
  end;
end;

function TSimbaScript.DoCompilerMacro(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
var
  FileName: String;
begin
  Result := True;

  case Name of
    'FINDLIBPATH':
      begin
        Value := #39 + SimbaEnv.FindPlugin(Argument, [Sender.Tokenizer.FileName]) + #39;
      end;

    'LOADEDLIBPATH':
      begin
        Value := #39 + FindLoadedPlugin(SimbaEnv.FindPlugin(Argument, [Sender.Tokenizer.FileName])) + #39;
      end;

    'LOADEDLIBS':
      begin
        Value := '';
        for FileName in GetLoadedPlugins() do
        begin
          if (Value <> '') then
            Value += ', ';
          Value += #39 + FileName + #39;
        end;
        Value := 'TStringArray([' + Value + '])';
      end;
  end;
end;

procedure TSimbaScript.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  DebugLn([EDebugLn.YELLOW], Hint);
end;

procedure TSimbaScript.DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString);
var
  IncludeFile: lpString;
begin
  IncludeFile := SimbaEnv.FindInclude(FileName, [TSimbaPath.PathExtractDir(Sender.Tokenizer.FileName)]);
  if (IncludeFile <> '') then
    FileName := IncludeFile;
end;

function TSimbaScript.DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  Plugin: TSimbaScriptPlugin;
begin
  Result := False;

  try
    case Directive.ToUpper() of
      'LOADLIB':
        begin
          Result := True;
          if InIgnore or InPeek then
            Exit;

          Plugin := TSimbaScriptPlugin.Create(Argument, [ExtractFileDir(Sender.Tokenizer.FileName)]);
          Plugin.Import(FCompiler);

          FPlugins.Add(Plugin);
        end;
    end;
  except
    on E: Exception do
      raise lpException.Create(E.Message, Sender.DocPos);
  end;
end;

function TSimbaScript.GetState: ESimbaScriptState;
begin
  Result := ESimbaScriptState.STATE_NONE;

  if (FCodeRunner <> nil) then
    if FCodeRunner.isRunning then
      Result := ESimbaScriptState.STATE_RUNNING
    else if FCodeRunner.isStopped then
      Result := ESimbaScriptState.STATE_STOP
    else if FCodeRunner.isPaused then
      Result := ESimbaScriptState.STATE_PAUSED;
end;

procedure TSimbaScript.SetTargetWindow(Value: String);
begin
  if (Value <> '') then
    FTargetWindow.FromString(Value);
end;

function TSimbaScript.Compile: Boolean;
begin
  FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFileName));
  if FHints then
    FCompiler.Options := FCompiler.Options + [lcoHints]
  else
    FCompiler.Options := FCompiler.Options - [lcoHints];

  FCompiler.addPreprocessorMacro('FINDLIBPATH', @DoCompilerMacro);
  FCompiler.addPreprocessorMacro('LOADEDLIBPATH', @DoCompilerMacro);
  FCompiler.addPreprocessorMacro('LOADEDLIBS', @DoCompilerMacro);

  FCompiler.addPreprocessorFunc('LIBLOADED', @DoCompilerPreprocessorFunc);
  FCompiler.addPreprocessorFunc('LIBEXISTS', @DoCompilerPreprocessorFunc);

  FCompiler.OnFindFile := @DoCompilerFindFile;
  FCompiler.OnHint := @DoCompilerHint;
  FCompiler.OnHandleDirective := @DoCompilerHandleDirective;

  if (FSimbaCommunication = nil) then
    FCompiler.addBaseDefine('SIMBAHEADLESS');

  FCompiler.Import();

  FCompileTime := HighResolutionTime();
  FCompiler.Compile();
  FCompileTime := HighResolutionTime() - FCompileTime;

  Result := True;
end;

function TSimbaScript.Run: Boolean;
var
  I: Integer;
begin
  if (FTargetWindow = 0) or (not FTargetWindow.IsValid()) then
    FTargetWindow := GetDesktopWindow();

  PString(FCompiler['SCRIPT_FILE'].Ptr)^ := FScriptFileName;
  PUInt64(FCompiler['SCRIPT_START_TIME'].Ptr)^ := GetTickCount64();

  PSimbaTarget(FCompiler['Target'].Ptr)^.SetWindow(FTargetWindow);
  PPointer(FCompiler['_SimbaScript'].Ptr)^ := Self;

  FRunningTime := HighResolutionTime();
  try
    FCodeRunner := TLapeCodeRunner.Create(FCompiler.Emitter);
    FCodeRunner.Run();
  finally
    FRunningTime := HighResolutionTime() - FRunningTime;

    for I := 0 to FPlugins.Count - 1 do
      FPlugins[I].CallOnStop();

    if FUserTerminated then
      FCompiler.CallProc('_CallOnUserTerminate');
    FCompiler.CallProc('_CallOnTerminate');
  end;

  Result := True;
end;

constructor TSimbaScript.Create;
begin
  inherited Create();

  FPlugins := TSimbaScriptPluginList.Create(True);
end;

constructor TSimbaScript.Create(Communication: TSimbaScriptCommunication);
begin
  Create();

  FSimbaCommunication := Communication;
  FScript := FSimbaCommunication.GetScript(FScriptFileName);
end;

constructor TSimbaScript.Create(FileName: String; Communication: TSimbaScriptCommunication);
begin
  Create();

  FSimbaCommunication := Communication;

  FScriptFileName := FileName;
  FScript := TSimbaFile.FileRead(FileName);
end;

destructor TSimbaScript.Destroy;
begin
  if (FCompiler <> nil) then
  begin
    if (FCompiler.Globals['HTTPClient'] <> nil) then
      TObject(FCompiler.Globals['HTTPClient'].Ptr^).Free();
    if (FCompiler.Globals['ASyncMouse'] <> nil) then
      TObject(FCompiler.Globals['ASyncMouse'].Ptr^).Free();

    FreeAndNil(FCompiler);
  end;
  FreeAndNil(FPlugins);
  FreeAndNil(FSimbaCommunication);
  FreeAndNil(FCodeRunner);

  inherited Destroy();
end;

procedure TSimbaScript.SetState(Value: ESimbaScriptState);
var
  I: Integer;
begin
  if (FCodeRunner = nil) then
    Exit;

  case Value of
    ESimbaScriptState.STATE_RUNNING:
      begin
        for I := 0 to FPlugins.Count - 1 do
          FPlugins[I].CallOnResume();
        FCompiler.CallProc('_CallOnResume');
        FCodeRunner.Resume();
      end;

    ESimbaScriptState.STATE_PAUSED:
      begin
        FCodeRunner.Pause();
        for I := 0 to FPlugins.Count - 1 do
          FPlugins[I].CallOnPause();
        FCompiler.CallProc('_CallOnPause');
      end;

    ESimbaScriptState.STATE_STOP:
      begin
        FUserTerminated := True;
        FCodeRunner.Stop();
      end;
  end;

  if (FSimbaCommunication <> nil) then
    FSimbaCommunication.ScriptStateChanged(Value);
end;

end.

