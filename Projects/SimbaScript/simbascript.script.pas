unit simbascript.script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simba.client, Forms, lptypes, lpinterpreter, lpparser, lpcompiler, lpvartypes, lpmessages,
  simbascript.compiler, simba.ipc, syncobjs, simba.script_common, simbascript.plugin;

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

  TSimbaScript = class(TThread)
  protected
    FCompiler: TSimbaScript_Compiler;
    FTokenizier: TLapeTokenizerString;
    FClient: TClient;

    FStartTime: UInt64;

    FState: TInitBool;

    FSimbaStateThread: TThread;
    FSimbaStateServer: TSimbaIPC_Client;

    FSimbaOutputServer: TSimbaIPC_Client;
    FSimbaMethodServer: TSimbaIPC_Client;
    FSimbaMethodLock: TCriticalSection;

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

    property SimbaOutputServer: TSimbaIPC_Client read FSimbaOutputServer write FSimbaOutputServer;
    property SimbaMethodServer: TSimbaIPC_Client read FSimbaMethodServer write FSimbaMethodServer;
    property SimbaStateServer: TSimbaIPC_Client read FSimbaStateServer write FSimbaStateServer;

    property Target: THandle read FTarget write FTarget;

    procedure Write(constref S: String);
    procedure WriteLn(constref S: String);

    procedure Invoke(Message: Int32; Params, Result: TMemoryStream);

    destructor Destroy; override;
  end;

implementation

uses
  fileutil, simba.misc, simba.files, fpexprpars, typinfo, ffi;

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

  if (FSimbaStateServer <> nil) then
  begin
    case FState of
      bTrue: ScriptState := SCRIPT_RUNNING;
      bFalse: ScriptState := SCRIPT_STOPPING;
      bUnknown: ScriptState := SCRIPT_PAUSED;
    end;

    FSimbaStateServer.Write(ScriptState, SizeOf(Int32));
  end;
end;

procedure TSimbaScript.Execute;
var
  T: Double;
  Method: TLapeGlobalVar;
begin
  try
    // Create
    FSimbaStateThread := TThread.ExecuteInThread(@HandleSimbaState);
    FSimbaMethodLock := TCriticalSection.Create();

    FClient := TClient.Create(FPluginPath);
    FClient.MOCR.FontPath := FFontPath;
    FClient.WriteLnProc := @WriteLn;
    FClient.IOManager.SetTarget(FTarget);

    // Import
    FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFile));
    FCompiler.OnFindFile := @HandleFindFile;
    FCompiler.OnHint := @HandleHint;
    FCompiler.OnHandleDirective := @HandleDirective;
    FCompiler.Import(Self);

    // Compile
    T := PerformanceTimer();
    if (not FCompiler.Compile()) then
      raise Exception.Create('Compiling failed');

    WriteLn(Format('Succesfully compiled in %d milliseconds.', [Round(PerformanceTimer() - T)]));

    if CompileOnly then
      Exit;

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

  if (FSimbaMethodServer <> nil) then
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
procedure TSimbaScript.HandleSimbaState;
var
  ScriptState: ESimbaScriptState;
begin
  if (SimbaStateServer <> nil) then
    while FSimbaStateServer.Read(ScriptState, SizeOf(Int32)) = SizeOf(Int32) do
    begin
      case ScriptState of
        SCRIPT_RUNNING:  FState := bTrue;
        SCRIPT_PAUSED:   FState := bUnknown;
        SCRIPT_STOPPING: FState := bFalse;
      end;

      FSimbaStateServer.Write(ScriptState, SizeOf(Int32)); // return the message so Simba can update accordingly
    end;
end;

procedure TSimbaScript.Write(constref S: String);
begin
  FOutputBuffer := FOutputBuffer + S;
end;

procedure TSimbaScript.WriteLn(constref S: String);
begin
  if (S = '') then
    FOutputBuffer := FOutputBuffer + ' ';
  if (S <> '') then
    FOutputBuffer := FOutputBuffer + S;

  if FSimbaOutputServer <> nil then
    FSimbaOutputServer.Write(FOutputBuffer[1], Length(FOutputBuffer) + 1) // Include null termination. This is how lines are detected simba sided.
  else
    System.WriteLn(FOutputBuffer);

  FOutputBuffer := '';
end;

procedure TSimbaScript.Invoke(Message: Int32; Params, Result: TMemoryStream);
begin
  try
    if (FSimbaMethodServer = nil) then
      raise Exception.Create('Method "' + GetEnumName(TypeInfo(ESimbaMethod), Message) + '" unavailable when running detached from Simba');

    FSimbaMethodLock.Enter();

    try
      FSimbaMethodServer.WriteMessage(Message, Params);
      FSimbaMethodServer.ReadMessage(Message, Result);
    finally
      FSimbaMethodLock.Leave();
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
    for I := 0 to High(FPlugins) do
      FPlugins[I].Free();

    FCompiler.Free();
    FClient.Free();

    FSimbaStateServer.Free();
    FSimbaOutputServer.Free();
    FSimbaMethodServer.Free();
    FSimbaMethodLock.Free();
  except
    // The process is ending anyway...
  end;

  inherited Destroy();
end;

initialization
  Randomize(); // Else we get the same sequence everytime...

  {$IF DEFINED(DARWIN) and DECLARED(LoadFFI)} { DynamicFFI }
  if not FFILoaded then
    LoadFFI('/usr/local/opt/libffi/lib/');
  {$ENDIF}

end.

