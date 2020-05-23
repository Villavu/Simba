unit simbascript.script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simba.client, Forms, lptypes, lpinterpreter, lpparser, lpcompiler, lpvartypes, lpmessages,
  simbascript.script_compiler, simba.ipc, syncobjs, simba.script_common, simba.script_plugin;

type
  TSimbaScript = class;

  TSimbaMethod = class
  public
    Params: TMemoryStream;
    Result: TMemoryStream;

    procedure Invoke(Script: TSimbaScript);

    constructor Create(Method: ESimbaMethod);
    destructor Destroy; override;
  end;

  TSimbaScript = class(TThread)
  protected
    FCompiler: TScriptCompiler;
    FCompiled: Boolean;
    FClient: TClient;
    FWriteBuffer: String;
    FStartTime: UInt64;
    FRunning: TInitBool;

    FState: TSimbaIPC_Client;
    FOutputIPC: TSimbaIPC_Client;
    FSimbaMethodClient: TSimbaIPC_Client;
    FSimbaMethodLock: TCriticalSection;

    FScript: String;
    FScriptFile: String;
    FScriptName: String;

    FAppPath: String;
    FDataPath: String;
    FIncludePath: String;
    FFontPath: String;
    FPluginPath: String;
    FScriptPath: String;

    FWriteTimeStamp: Boolean;

    FIsTerminating: Boolean;
    FIsUserTerminated: Boolean;

    FPlugins: TScriptPluginList;

    procedure Execute; override;

    procedure HandleState;
    procedure HandleException(E: Exception);

    procedure HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
    function HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
  public
    property StartTime: UInt64 read FStartTime;
    property Client: TClient read FClient;

    procedure PauseScript;
    procedure RunScript;
    procedure StopScript;


    property IsUserTerminated: Boolean read FIsUserTerminated write FIsUserTerminated;
    property IsTerminating: Boolean read FIsTerminating write FIsTerminating;

    property ScriptFile: String read FScriptFile;
    property ScriptName: String read FScriptName;

    property AppPath: String read FAppPath;
    property DataPath: String read FDataPath;
    property ScriptPath: String read FScriptPath;
    property FontPath: String read FFontPath;
    property PluginPath: String read FPluginPath;
    property IncludePath: String read FIncludePath;

    property WriteTimeStamp: Boolean read FWriteTimeStamp write FWriteTimeStamp;

    procedure Terminate(Data: PtrInt); overload;
    procedure Initialize(Data: PtrInt);
    procedure Run(Data: PtrInt);
    procedure Compile(Data: PtrInt);
    procedure Dump(Data: PtrInt);

    procedure _Write(constref S: String);
    procedure _WriteLn(constref S: String);

    procedure Invoke(Params, Result: TMemoryStream);

    destructor Destroy; override;
  end;

var
  Script: TSimbaScript;

implementation

uses
  fileutil, simba.misc, simba.files, fpexprpars,

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
  simbascript.import_simba;

procedure TSimbaMethod.Invoke(Script: TSimbaScript);
begin
  Script.Invoke(Params, Result);
end;

constructor TSimbaMethod.Create(Method: ESimbaMethod);
begin
  Result := TMemoryStream.Create();

  Params := TMemoryStream.Create();
  Params.Write(Ord(Method), SizeOf(Int32));
end;

destructor TSimbaMethod.Destroy;
begin
  if (Params <> nil) then
    Params.Free();
  if (Result <> nil) then
    Result.Free();
end;

procedure TSimbaScript.Execute;
var
  T: Double;
begin
  if FCompiled then
  begin
    FStartTime := GetTickCount64();

    T := PerformanceTimer();

    try
      try
        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FRunning);
      finally
        FIsTerminating := True;

        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, nil, TCodePos(FCompiler.getGlobalVar('__OnTerminate').Ptr^));
      end;

      if GetTickCount64() - FStartTime < 60000 then
        _WriteLn(Format('Succesfully executed in %d milliseconds.', [Round(PerformanceTimer() - T)]))
      else
        _WriteLn(Format('Succesfully executed in %s.', [TimeStamp(GetTickCount64() - FStartTime)]));
    except
      on E: Exception do
      begin
        ExitCode := SCRIPT_ERROR_RUNTIME;

        HandleException(E);
      end;
    end;
  end;

  Terminate(0);
end;

procedure TSimbaScript.HandleState;
var
  ScriptState: ESimbaScriptState;
begin
  while True do
  begin
    FState.Read(ScriptState, SizeOf(Int32));

    case ScriptState of
      SCRIPT_RUNNING:  RunScript();
      SCRIPT_PAUSED:   PauseScript();
      SCRIPT_STOPPING: StopScript();
    end;
  end;
end;

procedure TSimbaScript.HandleException(E: Exception);
var
  Message, FileName: ShortString;
  Line, Column: Int32;
  Method: TSimbaMethod;
begin
  Message := '';
  FileName := '';
  Line := -1;
  Column := -1;

  if (E is lpException) then
  begin
    Message := E.Message;

    with E as lpException do
    begin
      FileName := DocPos.FileName;
      Line := DocPos.Line;
      Column := DocPos.Col;
    end;

    if not FileExists(FileName) and (FileName <> ScriptName) then
      Message := StringReplace(Message, 'in file', 'in internal file', []);
  end else
    Message := E.Message + ' (' + E.ClassName + ')';

  Self._WriteLn(Message);

  Method := TSimbaMethod.Create(SIMBA_METHOD_SCRIPT_ERROR);
  Method.Params.Write(Message, SizeOf(ShortString));
  Method.Params.Write(FileName, SizeOf(ShortString));
  Method.Params.Write(Line, SizeOf(Int32));
  Method.Params.Write(Column, SizeOf(Int32));
  Method.Invoke(Script);
  Method.Free();
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
  Plugin: TMPlugin;
  Path: String;
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

            Path := FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath]);
            if Path = '' then
              raise Exception.Create('Plugin "' + Argument + '" not found');

            Plugin := TMPlugin.Create(Path);
            for i := 0 to Plugin.Declarations.Count - 1 do
              Plugin.Declarations[i].Import(Sender);

            FPlugins.Add(Plugin);
          end;

        'IFHASLIB':
          begin
            Path := FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath]);

            FCompiler.pushConditional((not InIgnore) and (Path <> ''), Sender.DocPos);
          end;

        'IFHASFILE':
          begin
            FCompiler.pushConditional((not InIgnore) and FindFile(Argument, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), FIncludePath, FAppPath]), Sender.DocPos);
          end;
      end;
    except
      on e: Exception do
        raise lpException.Create(e.Message, Sender.DocPos);
    end;

    Result := True;
  end else
    Result := False;
end;

procedure TSimbaScript.PauseScript;
var
  i: Int32 = Ord(SCRIPT_PAUSED);
begin
  FRunning := bUnknown;
  FState.Write(i, SizeOf(Int32));
end;

procedure TSimbaScript.RunScript;
var
  i: Int32 = Ord(SCRIPT_RUNNING);
begin
  FRunning := bTrue;
  FState.Write(i, SizeOf(Int32));
end;

procedure TSimbaScript.StopScript;
var
  i: Int32 = Ord(SCRIPT_STOPPING);
begin
  FRunning := bFalse;
  FState.Write(i, SizeOf(Int32));
end;

procedure TSimbaScript.Terminate(Data: PtrInt);
begin
  inherited Terminate();

  Application.Terminate();
  if WakeMainThread <> nil then
    WakeMainThread(nil);
end;

procedure TSimbaScript.Run(Data: PtrInt);
begin
  Start();
end;

procedure TSimbaScript.Compile(Data: PtrInt);
var
  T: Double;
begin
  try
    FCompiler.Import();

    T := PerformanceTimer();

    FCompiled := FCompiler.Compile();
    if (not FCompiled) then
      raise Exception.Create('Compiling failed');

    _WriteLn(Format('Succesfully compiled in %d milliseconds.', [Round(PerformanceTimer() - T)]));
  except
    on E: Exception do
    begin
      ExitCode := SCRIPT_ERROR_COMPILE;

      HandleException(E);
    end;
  end;
end;

procedure TSimbaScript.Dump(Data: PtrInt);
begin
  try
    _WriteLn(FCompiler.Dump());
  except
    on E: Exception do
      WriteLn('Dumping exception: ', E.Message);
  end;
end;

procedure TSimbaScript._Write(constref S: String);
begin
  FWriteBuffer := FWriteBuffer + S;
end;

procedure TSimbaScript._WriteLn(constref S: String);
begin
  if (S <> '') then
    FWriteBuffer := FWriteBuffer + S;
  if FWriteTimeStamp then
    FWriteBuffer := TimeStamp(GetTickCount64() - FStartTime) + FWriteBuffer;

  if FOutputIPC <> nil then
  begin
    FOutputIPC.Write(FWriteBuffer[1], Length(FWriteBuffer) + 1); // Include null terminated character. This is how lines are split Simba sided.
  end else
    WriteLn(FWriteBuffer);

  FWriteBuffer := '';
end;

procedure TSimbaScript.Invoke(Params, Result: TMemoryStream);
begin
  FSimbaMethodLock.Enter();

  try
    FSimbaMethodClient.Write(Params);
    FSimbaMethodClient.Read(Result);

    Result.Position := SizeOf(Int32); // Skip message id
  finally
    FSimbaMethodLock.Leave();
  end;
end;

procedure TSimbaScript.Initialize(Data: PtrInt);
begin
  FAppPath := Application.GetOptionValue('apppath');
  FDataPath := Application.GetOptionValue('datapath');
  FPluginPath := Application.GetOptionValue('pluginpath');
  FFontPath := Application.GetOptionValue('fontpath');
  FScriptPath := Application.GetOptionValue('scriptpath');
  FIncludePath := Application.GetOptionValue('includepath');

  FScriptFile := Application.GetOptionValue('scriptfile');
  FScriptName := Application.GetOptionValue('scriptname');

  if (not FileExists(FScriptFile)) then
    raise Exception.Create('Script "' + FScriptFile + '" not found');


  try
    with TStringList.Create() do
    try
      LoadFromFile(Application.GetOptionValue('scriptfile'));

      FScript := Text;
    finally
      if ExtractFileExt(FScriptFile) = '.tmp' then
        DeleteFile(FScriptFile);

      Free();
    end;
  except
    on e: Exception do
      raise Exception.Create('Unable to load script: ' + e.Message);
  end;

  FCompiler := TScriptCompiler.Create(FScript, FScriptName);
  FCompiler.OnFindFile := @HandleFindFile;
  FCompiler.OnHint := @HandleHint;
  FCompiler.OnHandleDirective := @HandleDirective;

  FRunning := bTrue;

  FPlugins := TScriptPluginList.Create(True);

  FClient := TClient.Create(FPluginPath);
  FClient.MOCR.FontPath := FFontPath;
  FClient.WriteLnProc := @_WriteLn;

  if Application.HasOption('target-window') then
    FClient.IOManager.SetTarget(Application.GetOptionValue('target-window').ToInt64());

  if Application.HasOption('output-client') then FOutputIPC := TSimbaIPC_Client.Create(Application.GetOptionValue('output-client'));
  if Application.HasOption('method-client') then FSimbaMethodClient := TSimbaIPC_Client.Create(Application.GetOptionValue('method-client'));
  if Application.HasOption('state-client') then FState := TSimbaIPC_Client.Create(Application.GetOptionValue('state-client'));

  FSimbaMethodLock := TCriticalSection.Create();

  ExecuteInThread(@HandleState);

  {
  if Application.HasOption('function-writer') and Application.HasOption('function-reader') then
  begin
    FExternalLock := TCriticalSection.Create();

    FExternalMethodWriter := TIPCClient_WriterStream.Create(Application.GetOptionValue('function-writer'));
    FExternalMethodReader := TIPCClient_ReaderStream.Create(Application.GetOptionValue('function-reader'));
  end;

  if Application.HasOption('state-reader') then
  begin
    FStateReader := TIPCClient_ReaderStream.Create(Application.GetOptionValue('state-reader'));

    ExecuteInThread(@HandleStateReading);
  end;

  if Application.HasOption('state-writer') then
  begin
    FStateWriter := TIPCClient_WriterStream.Create(Application.GetOptionValue('state-writer'));

    ExecuteInThread(@HandleStateWriting);
  end; }
end;

destructor TSimbaScript.Destroy;
begin
  if (FPlugins <> nil) then
    FPlugins.Free();
  if (FCompiler <> nil) then
    FCompiler.Free();
  if (FClient <> nil) then
    FClient.Free();

  FOutputIPC.Free();
  FSimbaMethodClient.Free();
  FState.Free();

  inherited Destroy();
end;

initialization
  Script := TSimbaScript.Create(True);

finalization
  Script.Free();

end.

