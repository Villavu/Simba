unit simbascript.script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simba.client, Forms, lptypes, lpinterpreter, lpparser, lpcompiler, lpvartypes, lpmessages,
  simbascript.compiler, simba.ipc, syncobjs, simba.script_common, simbascript.plugin;

const
  SIMBA_SCRIPT_RESUME = UInt8(0);
  SIMBA_SCRIPT_STOP   = UInt8(1);
  SIMBA_SCRIPT_PAUSE  = UInt8(2);

var
  SIMBA_SCRIPT_STOPPING: UInt8 = 0;
  SIMBA_SCRIPT_PAUSED:   UInt8 = 1;
  SIMBA_SCRIPT_RUNNING:  UInt8 = 2;

type
  PSimbaScript = ^TSimbaScript;
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

  TDebuggerThread = class(TThread)
  protected
    FStream: TMemoryStream;
    FLock: TCriticalSection;
    FWarned: Boolean;
    FScript: TSimbaScript;

    procedure Execute; override;
  public
    procedure Queue(Event: TSimbaScript_DebuggerEvent);

    constructor Create(Script: TSimbaScript); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaScript = class(TThread)
  protected
    FCompiler: TSimbaScript_Compiler;
    FTokenizier: TLapeTokenizerString;
    FClient: TClient;

    FStartTime: UInt64;

    FState: TInitBool;

    FSimbaStateThread: TThread;

    FSimbaIPC: TSimbaIPC_Client;
    FSimbaIPCLock: TCriticalSection;

    FWriteBuffer: String;

    FScript: String;
    FScriptFile: String;

    FAppPath: String;
    FDataPath: String;
    FIncludePath: String;
    FFontPath: String;
    FPluginPath: String;

    FIsTerminating: Boolean;
    FIsUserTerminated: Boolean;

    FTarget: THandle;

    FPlugins: TSimbaScriptPluginArray;

    procedure Execute; override;

    procedure HandleSimbaState;
    procedure HandleException(E: Exception);
    procedure HandleHint(Sender: TLapeCompilerBase; Hint: lpString);

    function HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure SetState(Value: TInitBool);
  public
    CompileOnly: Boolean;
    Debugging: Boolean;
    FDebuggerThread: TDebuggerThread;
    OnDestroyed: TNotifyEvent;

    _DebuggingMethods: TSimbaScript_DebuggingMethods;
    _DebuggingIndent: Int16;
    _DebuggingMethod: Int16;

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

    property SimbaIPC: TSimbaIPC_Client read FSimbaIPC write FSimbaIPC;

    property Target: THandle read FTarget write FTarget;

    procedure Write(constref S: String);
    procedure WriteLn(constref S: String);

    procedure Invoke(Message: Int32; Params, Result: TMemoryStream);

    destructor Destroy; override;
  end;

implementation

uses
  fileutil, simba.misc, simba.files, fpexprpars, typinfo, ffi;

procedure TDebuggerThread.Execute;

  procedure Send;
  begin
    FLock.Enter();

    try
      if (FStream.Position > 0) then
      begin
        with TSimbaMethod.Create(SIMBA_METHOD_DEBUGGER_EVENT) do
        try
          Params.Write(FStream.Memory^, FStream.Position);

          Invoke(FScript);
        finally
          Free();
        end;

        FStream.Position := 0;
      end;
    finally
      FLock.Leave();
    end;
  end;

begin
  while (not Terminated) do
  begin
    Send();

    Sleep(500);
  end;

  Send();
end;

procedure TDebuggerThread.Queue(Event: TSimbaScript_DebuggerEvent);
begin
  if FStream.Position >= 1024 * 1024 then
  begin
    if not FWarned then
    begin
      WriteLn('Debugger cannot keep up with your scripts function calling!');
      WriteLn('Your script will be throttled since it cannot keep up.');

      FWarned := True;
    end;

    while FStream.Position > 0 do
      Sleep(100);
  end;

  FLock.Enter();

  try
    FStream.Write(Event, SizeOf(TSimbaScript_DebuggerEvent));
  finally
    FLock.Leave();
  end;
end;

constructor TDebuggerThread.Create(Script: TSimbaScript);
begin
  inherited Create(False);

  FScript := Script;
  FStream := TMemoryStream.Create();
  FLock := TCriticalSection.Create();
end;

destructor TDebuggerThread.Destroy;
begin
  FStream.Free();
  FLock.Free();

  inherited Destroy();
end;

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
begin
  FState := Value;

  with TSimbaMethod.Create(SIMBA_METHOD_SCRIPT_STATE_CHANGED) do
  try
    case FState of
      bTrue:    Params.Write(SIMBA_SCRIPT_RUNNING,  SizeOf(UInt8));
      bFalse:   Params.Write(SIMBA_SCRIPT_STOPPING, SizeOf(UInt8));
      bUnknown: Params.Write(SIMBA_SCRIPT_PAUSED,   SizeOf(UInt8));
    end;

    Invoke(Self);
  finally
    Free();
  end;
end;

procedure TSimbaScript.Execute;
var
  T: Double;
  Method: TLapeGlobalVar;
  I: Int32;
begin
  try
    // Create
    FSimbaStateThread := TThread.ExecuteInThread(@HandleSimbaState);
    FSimbaIPCLock := TCriticalSection.Create();

    if Debugging then
       FDebuggerThread := TDebuggerThread.Create(Self);

    FClient := TClient.Create(FPluginPath);
    FClient.MOCR.FontPath := FFontPath;
    FClient.WriteLnProc := @WriteLn;
    FClient.IOManager.SetTarget(FTarget);

    // Import
    FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFile));
    FCompiler.OnFindFile := @HandleFindFile;
    FCompiler.OnHint := @HandleHint;
    FCompiler.OnHandleDirective := @HandleDirective;
    FCompiler.Debugging := Debugging;
    FCompiler.Import(Self);

    // Compile
    T := PerformanceTimer();
    if (not FCompiler.Compile()) then
      raise Exception.Create('Compiling failed');

    WriteLn(Format('Succesfully compiled in %d milliseconds.', [Round(PerformanceTimer() - T)]));

    if CompileOnly then
      Exit;

    if Debugging then
    begin
      _DebuggingIndent := -1;
      _DebuggingMethods := FCompiler.DebuggingMethods;

      for I := 0 to High(_DebuggingMethods) do
        with TSimbaMethod.Create(SIMBA_METHOD_DEBUGGER_METHOD) do
        try
          Params.Write(_DebuggingMethods[I], SizeOf(ShortString));

          Invoke(Self);
        finally
          Free();
        end;
    end;

    // Run
    T := PerformanceTimer();

    FStartTime := GetTickCount64();
    FState := bTrue;

    try
      RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FState);
    finally
      FIsTerminating := True;
      FIsUserTerminated := FState <> bTrue;

      Method := FCompiler.GetGlobalVar('__OnTerminate');
      if (Method <> nil) then
        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, nil, PCodePos(Method.Ptr)^);
    end;

    if (PerformanceTimer() - T < 10000) then
      WriteLn(Format('Succesfully executed in %d milliseconds.', [Round(PerformanceTimer() - T)]))
    else
      WriteLn(Format('Succesfully executed in %s.', [TimeStamp(Round(PerformanceTimer() - T))]));
  except
    on E: Exception do
    begin
      HandleException(E);

      raise;
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

  if (FSimbaIPC <> nil) then
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

    WriteLn(Param.Message);

    Method := TSimbaMethod.Create(SIMBA_METHOD_SCRIPT_ERROR);
    Method.Params.Write(Param, SizeOf(Param));
    Method.Invoke(Self);
    Method.Free();
  end else
    WriteLn(E.Message);
end;

procedure TSimbaScript.HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  WriteLn(Hint);
end;

function TSimbaScript.HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), FIncludePath, FAppPath])) then
    FileName := '';
end;

function TSimbaScript.HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  Arguments: TStringArray;
  Parser: TFPExpressionParser;
  Plugin: TSimbaScriptPlugin;
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

            if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath, FAppPath]) then
              raise Exception.Create('Plugin "' + Argument + '" not found');

            Plugin := TSimbaScriptPlugin.Create(Argument);
            Plugin.Import(FCompiler);

            SetLength(FPlugins, Length(FPlugins) + 1);
            FPlugins[High(FPlugins)] := Plugin;
          end;

        'IFHASLIB':
          begin
            if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath, FAppPath]) then
              FCompiler.pushConditional((not InIgnore) and True, Sender.DocPos)
            else
              FCompiler.pushConditional((not InIgnore) and False, Sender.DocPos);
          end;

        'IFHASFILE':
          begin
            FCompiler.pushConditional((not InIgnore) and FindFile(Argument, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), FIncludePath, FAppPath]), Sender.DocPos);
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

procedure TSimbaScript.HandleSimbaState;
var
  Stream: THandleStream;
  Value: UInt8;
begin
  Stream := THandleStream.Create(StdInputHandle);

  while Stream.Read(Value, SizeOf(UInt8)) = SizeOf(UInt8) do
  begin
    case Value of
      SIMBA_SCRIPT_RESUME: Self.State := bTrue;
      SIMBA_SCRIPT_STOP:   Self.State := bFalse;
      SIMBA_SCRIPT_PAUSE:  Self.State := bUnknown;
    end;
  end;

  Stream.Free();
end;

procedure TSimbaScript.Write(constref S: String);
begin
  FWriteBuffer := FWriteBuffer + S;
end;

procedure TSimbaScript.WriteLn(constref S: String);
begin
  System.WriteLn(FWriteBuffer + S);
  System.Flush(Output);

  FWriteBuffer := '';
end;

procedure TSimbaScript.Invoke(Message: Int32; Params, Result: TMemoryStream);
begin
  try
    if (FSimbaIPC = nil) then
      raise Exception.Create('Method "' + GetEnumName(TypeInfo(ESimbaMethod), Message) + '" unavailable when running detached from Simba');

    FSimbaIPCLock.Enter();

    try
      FSimbaIPC.WriteMessage(Message, Params);
      FSimbaIPC.ReadMessage(Message, Result);
    finally
      FSimbaIPCLock.Leave();
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
  try
    if (FDebuggerThread <> nil) then
    begin
      FDebuggerThread.Terminate();
      FDebuggerThread.WaitFor();
      FDebuggerThread.Free();
    end;

    for I := 0 to High(FPlugins) do
      FPlugins[I].Free();

    FCompiler.Free();
    FClient.Free();

    FSimbaIPC.Free();
    FSimbaIPCLock.Free();
  except
    // The process is ending anyway...
  end;

  inherited Destroy();

  if (OnDestroyed <> nil) then
    OnDestroyed(Self);
end;

initialization
  Randomize(); // Else we get the same sequence everytime...

  {$IF DEFINED(DARWIN) and DECLARED(LoadFFI)} { DynamicFFI }
  if not FFILoaded then
    LoadFFI('/usr/local/opt/libffi/lib/');
  {$ENDIF}

end.

