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
  simba.script_compiler, simba.script_communication, simba.script_plugin;

type
  PSimbaScript = ^TSimbaScript;
  TSimbaScript = class(TObject)
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

    function Compile: Boolean;
    function Run: Boolean;

    property State: ESimbaScriptState read GetState write SetState;
    property Script: String read FScript write FScript;
    property ScriptFileName: String read FScriptFileName write FScriptFileName;

    constructor Create(Communication: TSimbaScriptCommunication); reintroduce; overload;
    constructor Create(FileName: String; Communication: TSimbaScriptCommunication = nil); reintroduce; overload;
    destructor Destroy; override;
  end;

implementation

uses
  simba.env, simba.fs, simba.datetime, simba.target, simba.vartype_windowhandle,
  simba.script_pluginloader;

function TSimbaScript.DoCompilerPreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
begin
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

          FPlugins := FPlugins + [Plugin];
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
    bTrue:  Result := ESimbaScriptState.STATE_RUNNING;
    bFalse: Result := ESimbaScriptState.STATE_STOP;
    else
      Result := ESimbaScriptState.STATE_PAUSED;
  end;
end;

procedure TSimbaScript.SetTargetWindow(Value: String);
begin
  if (Value <> '') then
    FTargetWindow.FromString(Value);
end;

function TSimbaScript.Compile: Boolean;
begin
  FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFileName));

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
begin
  if (FTargetWindow = 0) or (not FTargetWindow.IsValid()) then
    FTargetWindow := GetDesktopWindow();

  PString(FCompiler['SCRIPT_FILE'].Ptr)^ := FScriptFileName;
  PUInt64(FCompiler['SCRIPT_START_TIME'].Ptr)^ := GetTickCount64();

  PSimbaTarget(FCompiler['Target'].Ptr)^.SetWindow(FTargetWindow);
  PPointer(FCompiler['_SimbaScript'].Ptr)^ := Self;

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

constructor TSimbaScript.Create(Communication: TSimbaScriptCommunication);
begin
  inherited Create();

  FState := bTrue;

  FSimbaCommunication := Communication;
  FScript := FSimbaCommunication.GetScript(FScriptFileName);
end;

constructor TSimbaScript.Create(FileName: String; Communication: TSimbaScriptCommunication);
begin
  inherited Create();

  FState := bTrue;

  FSimbaCommunication := Communication;

  FScriptFileName := FileName;
  FScript := TSimbaFile.FileRead(FileName);
end;

destructor TSimbaScript.Destroy;
var
  Plugin: TSimbaScriptPlugin;
begin
  for Plugin in FPlugins do
    Plugin.Free();
  FPlugins := nil;

  if (FCompiler <> nil) then
  begin
    if (FCompiler.Globals['HTTPClient'] <> nil) then
      TObject(FCompiler.Globals['HTTPClient'].Ptr^).Free();
    if (FCompiler.Globals['ASyncMouse'] <> nil) then
      TObject(FCompiler.Globals['ASyncMouse'].Ptr^).Free();

    FreeAndNil(FCompiler);
  end;
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

