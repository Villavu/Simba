unit simbascript.script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simba.client, Forms, lptypes, lpinterpreter, lpparser, lpcompiler, lpvartypes, lpmessages,
  simbascript.compiler, simba.ipc, syncobjs, simba.script_common, simbascript.plugin;

type
  TSimbaScript = class;

  TSimbaMethod = class
  public
    Method: Int32;
    Params: TMemoryStream;
    Result: TMemoryStream;

    procedure Invoke(Script: TSimbaScript);

    constructor Create(AMethod: ESimbaMethod);
    destructor Destroy; override;
  end;

  TSimbaScript = class(TThread)
  protected
    FCompiler: TScriptCompiler;
    FTokenizier: TLapeTokenizerString;
    FCompiled: Boolean;
    FClient: TClient;

    FStartTime: UInt64;

    FState: TInitBool;
    FStateServer: TSimbaIPC_Client;
    FStateThread: TThread;

    FMethodServer: TSimbaIPC_Client;
    FMethodLock: TCriticalSection;

    FOutputServer: TSimbaIPC_Client;
    FOutputBuffer: String;

    FScript: String;
    FScriptFile: String;

    FAppPath: String;
    FDataPath: String;
    FIncludePath: String;
    FFontPath: String;
    FPluginPath: String;

    FIsTerminating: Boolean;
    FIsUserTerminated: Boolean;

    FTargetWindow: THandle;

    FPlugins: TSimbaScriptPluginArray;

    procedure Execute; override;

    procedure HandleStateReading;

    procedure HandleException(E: Exception);
    procedure HandleHint(Sender: TLapeCompilerBase; Hint: lpString);

    function HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure SetState(Value: TInitBool);
  public
    CompileOnly: Boolean;
    Dump: Boolean;

    property State: TInitBool read FState write SetState;
    property StartTime: UInt64 read FStartTime;
    property Client: TClient read FClient;

    property IsUserTerminated: Boolean read FIsUserTerminated write FIsUserTerminated;
    property IsTerminating: Boolean read FIsTerminating write FIsTerminating;

    property ScriptFile: String read FScriptFile write FScriptFile;
    property Script: String read FScript write FScript;

    property AppPath: String read FAppPath write FAppPath;
    property DataPath: String read FDataPath write FDataPath;
    property FontPath: String read FFontPath write FFontPath;
    property PluginPath: String read FPluginPath write FPluginPath;
    property IncludePath: String read FIncludePath write FIncludePath;

    property OutputServer: TSimbaIPC_Client read FOutputServer write FOutputServer;
    property MethodServer: TSimbaIPC_Client read FMethodServer write FMethodServer;
    property StateServer: TSimbaIPC_Client read FStateServer write FStateServer;

    property TargetWindow: THandle read FTargetWindow write FTargetWindow;

    procedure _Write(constref S: String);
    procedure _WriteLn(constref S: String);

    procedure Invoke(Message: Int32; Params, Result: TMemoryStream);

    destructor Destroy; override;
  end;

var
  Script: TSimbaScript;

implementation

uses
  fileutil, simba.misc, simba.files, fpexprpars, typinfo, ffi,

  // Base types
  simbascript.import_types,

  // Lazarus classes
  simbascript.import_tobject,
  simbascript.import_lclsystem,
  simbascript.import_lclgraphics,
  simbascript.import_lclcontrols,
  simbascript.import_lclforms,
  simbascript.import_lclstdctrls,
  simbascript.import_lclextctrls,
  simbascript.import_lclcomctrls,
  simbascript.import_lcldialogs,
  simbascript.import_lclmenus,
  simbascript.import_lclspin,
  simbascript.import_lclprocess,
  simbascript.import_lclregexpr,

  // Simba classes
  simbascript.import_tmdtm,
  simbascript.import_tmdtms,
  simbascript.import_tmufasabitmap,
  simbascript.import_tmbitmaps,
  simbascript.import_tmfiles,
  simbascript.import_tmfinder,
  simbascript.import_tmfont,
  simbascript.import_tmfonts,
  simbascript.import_tmocr,
  simbascript.import_ttarget,
  simbascript.import_tiomanager,
  simbascript.import_tclient,
  simbascript.import_tmmltimer,
  simbascript.import_json,
  simbascript.import_xml,
  simbascript.import_simbaimagebox,

  // Simba
  simbascript.import_system,
  simbascript.import_target,
  simbascript.import_input,
  simbascript.import_finder,
  simbascript.import_web,
  simbascript.import_arrays_algorithms,
  simbascript.import_matrix,
  simbascript.import_math,
  simbascript.import_time_date,
  simbascript.import_ocr,
  simbascript.import_string,
  simbascript.import_colormath,
  simbascript.import_bitmap,
  simbascript.import_dtm,
  simbascript.import_file,
  simbascript.import_other,
  simbascript.import_crypto,
  simbascript.import_deprecated,
  simbascript.import_oswindow,
  simbascript.import_dialog,
  simbascript.import_simba,
  simbascript.import_process;

procedure TSimbaMethod.Invoke(Script: TSimbaScript);
begin
  Script.Invoke(Self.Method, Params, Result);
end;

constructor TSimbaMethod.Create(AMethod: ESimbaMethod);
begin
  Method := Ord(AMethod);
  Result := TMemoryStream.Create();
  Params := TMemoryStream.Create();
end;

destructor TSimbaMethod.Destroy;
begin
  if (Params <> nil) then
    Params.Free();
  if (Result <> nil) then
    Result.Free();
end;

procedure TSimbaScript.SetState(Value: TInitBool);
var
  ScriptState: ESimbaScriptState;
begin
  FState := Value;

  if (FStateServer <> nil) then
  begin
    case FState of
      bTrue: ScriptState := SCRIPT_RUNNING;
      bFalse: ScriptState := SCRIPT_STOPPING;
      bUnknown: ScriptState := SCRIPT_PAUSED;
    end;

    FStateServer.Write(ScriptState, SizeOf(Int32));
  end;
end;

procedure TSimbaScript.Execute;
var
  T: Double;
  Method: TLapeGlobalVar;
begin
  ExitCode := SCRIPT_EXIT_CODE_SUCCESS;

  try
    FCompiler := TScriptCompiler.Create(TLapeTokenizerString.Create(FScript, FScriptFile));
    FCompiler.OnFindFile := @HandleFindFile;
    FCompiler.OnHint := @HandleHint;
    FCompiler.OnHandleDirective := @HandleDirective;

    FClient := TClient.Create(FPluginPath);
    FClient.MOCR.FontPath := FFontPath;
    FClient.WriteLnProc := @_WriteLn;
    FClient.IOManager.SetTarget(FTargetWindow);

    if (FStateServer <> nil) then
      FStateThread := TThread.ExecuteInThread(@HandleStateReading);

    if (FMethodServer <> nil) then
      FMethodLock := TCriticalSection.Create();
  except
    on E: Exception do
    begin
      ExitCode := SCRIPT_EXIT_CODE_INITIALIZE;
      HandleException(E);
      Exit;
    end;
  end;

  try
    FCompiler.Import(Dump);
  except
    on E: Exception do
    begin
      ExitCode := SCRIPT_EXIT_CODE_IMPORT;
      HandleException(E);
      Exit;
    end;
  end;

  if Dump then
  begin
    _WriteLn(FCompiler.Dump.Text);

    Exit;
  end;

  try
    T := PerformanceTimer();
    if (not FCompiler.Compile()) then
      raise Exception.Create('Compiling failed');

    _WriteLn(Format('Succesfully compiled in %d milliseconds.', [Round(PerformanceTimer() - T)]));
  except
    on E: Exception do
    begin
      ExitCode := SCRIPT_EXIT_CODE_COMPILE;
      HandleException(E);
      Exit;
    end;
  end;

  if CompileOnly then
    Exit;

  try
    T := PerformanceTimer();

    FStartTime := GetTickCount64();
    FState := bTrue;

    try
      RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FState);
    finally
      FIsTerminating := True;

      Method := FCompiler.GetGlobalVar('__OnTerminate');
      if (Method <> nil) then
        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, nil, PCodePos(Method.Ptr)^);
    end;

    if (GetTickCount64() - FStartTime < 60000) then
      _WriteLn('Succesfully executed in ' + FormatFloat('0.00', PerformanceTimer() - T) + ' milliseconds.')
    else
      _WriteLn('Succesfully executed in ' + TimeStamp(GetTickCount64() - FStartTime) + '.');
  except
    on E: Exception do
    begin
      ExitCode := SCRIPT_EXIT_CODE_RUNTIME;

      HandleException(E);
    end;
  end;
end;

procedure TSimbaScript.HandleException(E: Exception);
var
  Param: TSimbaMethod_ScriptError;
  Method: TSimbaMethod;
begin
  Param := Default(TSimbaMethod_ScriptError);
  Param.Line := -1;
  Param.Column := -1;

  if (FMethodServer <> nil) then
  begin
    if (E is lpException) then
    begin
      Param.Message := E.Message;

      with E as lpException do
      begin
        Param.FileName := DocPos.FileName;
        Param.Line := DocPos.Line;
        Param.Column := DocPos.Col;
      end;
    end else
      Param.Message := E.Message + ' (' + E.ClassName + ')';

    Self._WriteLn(Param.Message);

    Method := TSimbaMethod.Create(SIMBA_METHOD_SCRIPT_ERROR);
    Method.Params.Write(Param, SizeOf(Param));
    Method.Invoke(Self);
    Method.Free();
  end else
    Self._WriteLn(E.Message);
end;

procedure TSimbaScript.HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  _WriteLn(Hint);
end;

function TSimbaScript.HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), FIncludePath, FAppPath])) then
    FileName := '';
end;

function TSimbaScript.HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  Arguments: TStringArray;
  Parser: TFPExpressionParser;
  Plugin: TSimbaScriptPlugin;
  i: Int32;
begin
  if (UpperCase(Directive) = 'LOADLIB') or (UpperCase(Directive) = 'IFHASLIB') or
     (UpperCase(Directive) = 'IFVALUE') or (UpperCase(Directive) = 'ERROR') or
     (UpperCase(Directive) = 'IFHASFILE') then
  begin
    if InPeek or (Argument = '') then
      Exit(True);

    try
      case UpperCase(Directive) of
        'IFVALUE':
          begin
            Arguments := Argument.Split(['<>', '>=', '<=', '=', '<', '>']);
            if Length(Arguments) <> 2 then
              raise Exception.Create('IFVALUE directive must have two arguments');

            Parser := TFPExpressionParser.Create(nil);
            Parser.Expression := Argument.Replace(Arguments[0].Trim(), Sender.Defines[Arguments[0].Trim()]);

            try
              FCompiler.pushConditional((not InIgnore) and Parser.AsBoolean, Sender.DocPos);
            finally
              Parser.Free();
            end;
          end;

        'ERROR':
          begin
            if (not InIgnore) then
              raise Exception.Create('User defined error: "' + Argument + '"');
          end;

        'LOADLIB':
          begin
            if InIgnore then
              Exit;

            if not TSimbaScriptPlugin.FindFile(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath, FAppPath]) then
              raise Exception.Create('Plugin "' + Argument + '" not found');

            Plugin := TSimbaScriptPlugin.Create(Argument);
            Plugin.Import(FCompiler);

            SetLength(FPlugins, Length(FPlugins) + 1);
            FPlugins[High(FPlugins)] := Plugin;
          end;

        'IFHASLIB':
          begin
            if TSimbaScriptPlugin.FindFile(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath, FAppPath]) then
              FCompiler.pushConditional((not InIgnore) and True, Sender.DocPos)
            else
              FCompiler.pushConditional((not InIgnore) and False, Sender.DocPos);
          end;

        'IFHASFILE':
          begin
            FCompiler.pushConditional((not InIgnore) and FindFile(Argument, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), FIncludePath, FAppPath]), Sender.DocPos);
          end;
      end;
    except
      on E: Exception do
        raise lpException.Create(E.Message, Sender.DocPos);
    end;

    Result := True;
  end else
    Result := False;
end;

// Isn't terminated safely, but the program is exiting soo...
procedure TSimbaScript.HandleStateReading;
var
  ScriptState: ESimbaScriptState;
begin
  while FStateServer.Read(ScriptState, SizeOf(Int32)) = SizeOf(Int32) do
  begin
    FIsUserTerminated := False;

    case ScriptState of
      SCRIPT_RUNNING:  FState := bTrue;
      SCRIPT_PAUSED:   FState := bUnknown;
      SCRIPT_STOPPING:
        begin
          FIsUserTerminated := True;

          FState := bFalse;
        end;
    end;

    FStateServer.Write(ScriptState, SizeOf(Int32)); // return the message so Simba can update accordingly
  end;
end;

procedure TSimbaScript._Write(constref S: String);
begin
  FOutputBuffer := FOutputBuffer + S;
end;

procedure TSimbaScript._WriteLn(constref S: String);
begin
  if (S = '') then
    FOutputBuffer := FOutputBuffer + ' ';
  if (S <> '') then
    FOutputBuffer := FOutputBuffer + S;

  if FOutputServer <> nil then
    FOutputServer.Write(FOutputBuffer[1], Length(FOutputBuffer) + 1) // Include null termination. This is how lines are detected simba sided.
  else
    WriteLn(FOutputBuffer);

  FOutputBuffer := '';
end;

procedure TSimbaScript.Invoke(Message: Int32; Params, Result: TMemoryStream);
begin
  try
    if (FMethodServer = nil) then
      raise Exception.Create('Method "' + GetEnumName(TypeInfo(ESimbaMethod), Message) + '" unavailable when running detached from Simba');

    FMethodLock.Enter();

    try
      FMethodServer.WriteMessage(Message, Params);
      FMethodServer.ReadMessage(Message, Result);
    finally
      FMethodLock.Leave();
    end;
  except
    on E: Exception do
      HandleException(E);
  end;
end;

destructor TSimbaScript.Destroy;
var
  I: Int32;
begin
  for I := 0 to High(FPlugins) do
    FPlugins[I].Free();

  if (FMethodLock <> nil) then
    FMethodLock.Free();

  if (FCompiler <> nil) then
    FCompiler.Free();
  if (FClient <> nil) then
    FClient.Free();

  if (FStateServer <> nil) then
    FStateServer.Free();
  if (FOutputServer <> nil) then
    FOutputServer.Free();
  if (FMethodServer <> nil) then
    FMethodServer.Free();

  inherited Destroy();
end;

{$IF DEFINED(DARWIN) and DECLARED(LoadFFI)} { DynamicFFI }
initialization
  if not FFILoaded then
    LoadFFI('/usr/local/opt/libffi/lib/');
{$ENDIF}

end.

