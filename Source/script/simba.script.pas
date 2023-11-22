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
  simba.script_compiler, simba.script_communication,
  simba.script_plugin, simba.mufasatypes, simba.windowhandle;

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
  simba.env, simba.datetime, simba.httpclient, simba.target,
  simba.script_pluginloader;

procedure TSimbaScript.DoCompilerHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  SimbaDebugLn([EDebugLn.YELLOW], Hint);
end;

function TSimbaScript.DoCompilerFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindInclude(FileName, [ExtractFileDir(Sender.Tokenizer.FileName)])) then
    FileName := '';
end;

function TSimbaScript.DoCompilerHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

  function DoMaybeIncludeFromURL: Boolean;
  var
    URL, Source: String;
  begin
    URL := Argument.ToUpper().Between('URL(', ')');

    Result := URL <> '';
    if Result then
    begin
      URL    := Argument.ToUpper().Between('URL(', ')');
      Source := TSimbaHTTPClient.SimpleGet(URL, [EHTTPStatus.OK]);

      FCompiler.pushTokenizer(
        TLapeTokenizerString.Create(Source, '!' + URL)
      );
    end;
  end;

  function DoLoadLib: Boolean;
  var
    Plugin: TSimbaScriptPlugin;
  begin
    Result := True;
    if InIgnore or InPeek then
      Exit;

    Plugin := TSimbaScriptPlugin.Create(Argument, [ExtractFileDir(Sender.Tokenizer.FileName)]);
    Plugin.Import(FCompiler);

    FPlugins := FPlugins + [Plugin];
  end;

  function DoFindLib(Want: Boolean): Boolean;
  begin
    Result := True;

    if InIgnore then
      FCompiler.pushConditional(False, Sender.DocPos)
    else
      FCompiler.pushConditional(FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName)]) = Want, Sender.DocPos);
  end;

  function DoFindFile(Want: Boolean): Boolean;
  begin
    Result := True;

    if InIgnore then
      FCompiler.pushConditional(False, Sender.DocPos)
    else
      FCompiler.pushConditional(FindInclude(Argument, [ExtractFileDir(Sender.Tokenizer.FileName)]) = Want, Sender.DocPos);
  end;

  function DoIncluded(Want: Boolean): Boolean;
  begin
    Result := True;

    if InIgnore then
      FCompiler.pushConditional(False, Sender.DocPos)
    else
      FCompiler.pushConditional((FindInclude(Argument, [ExtractFileDir(Sender.Tokenizer.FileName)]) and FCompiler.HasInclude(Argument)) = Want, Sender.DocPos);
  end;

begin
  Result := False;

  try
    case Directive.ToUpper() of
      'I', 'INCLUDE':    Result := DoMaybeIncludeFromURL();

      'LOADLIB':         Result := DoLoadLib();

      'IF_FINDLIB':      Result := DoFindLib(True);
      'IF_NOT_FINDLIB':  Result := DoFindLib(False);

      'IF_FINDFILE':     Result := DoFindFile(True);
      'IF_NOT_FINDFILE': Result := DoFindFile(False);

      'IF_INCLUDED':     Result := DoIncluded(True);
      'IF_NOT_INCLUDED': Result := DoIncluded(False);
    end;
  except
    on E: Exception do
      raise lpException.Create(E.Message, Sender.DocPos);
  end;
end;

function TSimbaScript.DoFindMacro(Sender: TLapeCompiler; Name: lpString; var Value: lpString): Boolean;

  // {$MACRO ENV(HOME)}
  function DoEnvVar(Param: String): Boolean;
  begin
    Result := Param <> '';
    if Result then
      Value := '"' + GetEnvironmentVariable(Param) + '"';
  end;

  // {$MACRO LIBPATH(plugin.dll)}
  function DoLibPath(Param: String): Boolean;
  var
    I: Integer;
  begin
    Result := FindPlugin(Param, [ExtractFileDir(Sender.Tokenizer.FileName)]);

    if Result then
    begin
      for I := 0 to High(LoadedPlugins) do
        if (LoadedPlugins[I].OrginalFileName = Param) then
          Param := LoadedPlugins[I].FileName;

      Value := '"' + Param + '"';
    end;
  end;

begin
  Result := False;
  Value := '';

  case Name.Before('(').ToUpper() of
    'LIBPATH': Result := DoLibPath(Name.Between('(', ')'));
    'ENV':     Result := DoEnvVar(Name.Between('(', ')'));
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
  PString(FCompiler['SCRIPT_FILE'].Ptr)^ := FScriptFileName;
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

