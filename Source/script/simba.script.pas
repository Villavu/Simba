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
  simba.target,
  simba.script_compiler,
  simba.script_communication,
  simba.script_plugin;

type
  TSimbaScript = class(TObject)
  protected
    FUserTerminated: Boolean;
    FTargetWindow: String;
    FHints: Boolean;

    FScript: String;
    FScriptFileName: String;

    FCompiler: TScriptCompiler;
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

    procedure SetState(Value: ESimbaScriptState);
  public
    property CompileTime: Double read FCompileTime;
    property RunningTime: Double read FRunningTime;

    property Compiler: TScriptCompiler read FCompiler;

    property SimbaCommunication: TSimbaScriptCommunication read FSimbaCommunication;

    property TargetWindow: String read FTargetWindow write FTargetWindow;
    property Hints: Boolean write FHints;

    function Compile: Boolean;
    function Run: Boolean;
    procedure Dump(FileName: String);

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
  simba.env, simba.fs, simba.datetime,
  simba.vartype_string,
  simba.script_pluginloader,
  simba.script_imports;

function TSimbaScript.{%H-}DoCompilerPreprocessorFunc(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
begin
  Value := '';

  case Name of
    'LOADEDLIB': Value := BoolToStr(FindLoadedPlugin(Argument) <> '', True);
    'FINDLIB':   Value := BoolToStr(SimbaEnv.HasPlugin(Argument, [Sender.Tokenizer.FileName]), True);
  end;
end;

function TSimbaScript.DoCompilerMacro(Sender: TLapeCompiler; Name, Argument: lpString; out Value: lpString): Boolean;
var
  FileName: String;
begin
  Result := True;

  case Name of
    'FINDLIB':
      begin
        Value := #39 + SimbaEnv.FindPlugin(Argument, [Sender.Tokenizer.FileName]) + #39;
      end;

    'LOADEDLIB':
      begin
        Value := #39 + FindLoadedPlugin(Argument) + #39;
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

function TSimbaScript.Compile: Boolean;
begin
  FCompiler := TScriptCompiler.Create(FScript, FScriptFileName);
  if FHints then
    FCompiler.Options := FCompiler.Options + [lcoHints]
  else
    FCompiler.Options := FCompiler.Options - [lcoHints];

  FCompiler.addPreprocessorMacro('FINDLIB', @DoCompilerMacro);
  FCompiler.addPreprocessorMacro('LOADEDLIB', @DoCompilerMacro);
  FCompiler.addPreprocessorMacro('LOADEDLIBS', @DoCompilerMacro);

  FCompiler.addPreprocessorFunc('LOADEDLIB', @DoCompilerPreprocessorFunc);
  FCompiler.addPreprocessorFunc('FINDLIB', @DoCompilerPreprocessorFunc);

  FCompiler.OnFindFile := @DoCompilerFindFile;
  FCompiler.OnHint := @DoCompilerHint;
  FCompiler.OnHandleDirective := @DoCompilerHandleDirective;

  if (FSimbaCommunication = nil) then
    FCompiler.addBaseDefine('SIMBAHEADLESS');

  FCompiler.Options := FCompiler.Options + [lcoAutoInvoke, lcoExplicitSelf, lcoAutoObjectify, lcoRelativeFileNames] - [lcoInheritableRecords];

  AddSimbaInternalMethods(Self);
  AddSimbaImports(Self);

  FCompileTime := HighResolutionTime();
  FCompiler.Compile();
  FCompileTime := HighResolutionTime() - FCompileTime;

  Result := True;
end;

function TSimbaScript.Run: Boolean;
var
  I: Integer;
begin
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

procedure TSimbaScript.Dump(FileName: String);
var
  I: Integer;
begin
  FCompiler := TScriptCompiler.CreateDump();

  AddSimbaInternalMethods(Self);
  AddSimbaImports(Self);

  with TStringList.Create() do
  try
    LineBreak := #0;
    for I := 0 to FCompiler.Dump.Count - 1 do
      Values[FCompiler.Dump[I].Name] := Values[FCompiler.Dump[I].Name] + FCompiler.Dump[I].Value + LineEnding;

    SaveToFile(FileName);
  finally
    Free();
  end;
end;

constructor TSimbaScript.Create;
begin
  inherited Create();

  //FTarget := TSimbaTarget.Create();
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
    FreeAndNil(FCompiler);
  if (FPlugins <> nil) then
    FreeAndNil(FPlugins);
  if (FSimbaCommunication <> nil) then
    FreeAndNil(FSimbaCommunication);
  if (FCodeRunner <> nil) then
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

