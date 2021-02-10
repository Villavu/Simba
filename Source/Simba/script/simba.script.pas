unit simba.script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpcompiler, lpparser, lpinterpreter, lpmessages,
  simba.script_compiler, simba.script_communication, simba.client;

type
  TSimbaScript = class(TThread)
  protected
    FState: TInitBool;
    FTarget: String;
    FStartTime: UInt64;

    FScriptFile: String;
    FScriptName: String;

    FCompiler: TSimbaScript_Compiler;
    FCompileOnly: Boolean;

    FWriteBuffer: String;
    FClient: TClient;
    FSimbaCommunicationServer: String;
    FSimbaCommunication: TSimbaCommunicationClient;

    FTerminated: Boolean;
    FUserTerminated: Boolean;
    FDebugging: Boolean;
    FDebugger: TThread;

    procedure DoStateThread;

    procedure HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
    procedure HandleException(E: Exception);

    function HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure SetState(Value: TInitBool);

    procedure DoTerminate; override;
    procedure Execute; override;
  public
    // Script is terminating
    property Terminated: Boolean read FTerminated;
    // Stop button has been clicked
    property UserTerminated: Boolean read FUserTerminated;

    property Compiler: TSimbaScript_Compiler read FCompiler;
    property CompileOnly: Boolean read FCompileOnly write FCompileOnly;

    property SimbaCommunicationServer: String read FSimbaCommunicationServer write FSimbaCommunicationServer;

    property Client: TClient read FClient;
    property State: TInitBool read FState write SetState;
    property StartTime: UInt64 read FStartTime;
    property Target: String read FTarget write FTarget;
    property Debugging: Boolean read FDebugging write FDebugging;

    property ScriptFile: String read FScriptFile write FScriptFile;
    property ScriptName: String read FScriptName write FScriptName;

    procedure Invoke(Method: TSimbaMethod);

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  SimbaScript: TSimbaScript;

implementation

uses
  simba.files, simba.script_plugin, simba.misc, simba.script_debugger
  {$IFDEF WINDOWS},
  windows
  {$ENDIF};

procedure TSimbaScript.HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  WriteLn(Hint);
end;

procedure TSimbaScript.HandleException(E: Exception);
var
  Method: TSimbaMethod;
begin
  WriteLn(E.Message);

  if (FSimbaCommunication <> nil) then
  begin
    if (E is lpException) then
      with E as lpException do
      begin
        Method := TSimbaMethod_ScriptError.Create(DocPos.Line, DocPos.Col, DocPos.FileName);

        try
          Self.Invoke(Method);
        finally
          Method.Free();
        end;
      end;
  end else
    ExitCode := 1;
end;

function TSimbaScript.HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), GetIncludePath(), GetSimbaPath()])) then
    FileName := '';
end;

function TSimbaScript.HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  Plugin: TSimbaScriptPlugin;
begin
  Result := Directive.ToUpper().IndexOfAny(['ERROR', 'LOADLIB', 'LIBPATH', 'IFHASLIB', 'IFHASFILE']) > -1;

  if Result then
  try
    if InIgnore or InPeek or (Argument = '') then
      Exit;

    case Directive.ToUpper() of
      'LOADLIB':
        begin
          if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), GetPluginPath(), GetSimbaPath()]) then
            raise Exception.Create('Plugin "' + Argument + '" not found');

          Plugin := TSimbaScriptPlugin.Create(Argument);
          Plugin.Import(FCompiler);
        end;

      'LIBPATH':
        begin
          if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), GetPluginPath(), GetSimbaPath()]) then
            Argument := '';

          FCompiler.pushTokenizer(TLapeTokenizerString.Create(#39 + Argument + #39));
        end;

      'IFHASLIB':
        begin
          if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), GetPluginPath(), GetSimbaPath()]) then
            FCompiler.pushConditional((not InIgnore) and True, Sender.DocPos)
          else
            FCompiler.pushConditional((not InIgnore) and False, Sender.DocPos);
        end;

      'IFHASFILE':
        begin
          FCompiler.pushConditional((not InIgnore) and FindFile(Argument, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), GetIncludePath(), GetSimbaPath()]), Sender.DocPos);
        end;

      'ERROR':
        begin
          raise Exception.Create('User defined error: "' + Argument + '"');
        end;
    end;
  except
    on E: Exception do
      raise lpException.Create(E.Message, Sender.DocPos);
  end;
end;

procedure TSimbaScript.DoTerminate;
var
  PID: UInt32;
begin
  {$IFDEF WINDOWS}
  GetWindowThreadProcessId(GetConsoleWindow(), PID);
  if (PID = GetCurrentProcessID()) then
  begin
    WriteLn('Press enter to exit');

    ReadLn();
  end;
  {$ENDIF}

  inherited DoTerminate();
end;

procedure TSimbaScript.Execute;
var
  Tokenizer: TSimbaScript_Tokenzier;
begin
  FState := bTrue;

  TThread.ExecuteInThread(@DoStateThread);

  try
    if (FSimbaCommunicationServer <> '') then
      FSimbaCommunication := TSimbaCommunicationClient.Create(FSimbaCommunicationServer);

    Tokenizer := TSimbaScript_Tokenzier.Create(FScriptFile, FScriptName);
    if ExtractFileExt(FScriptFile) = '.tmp' then // Delete temp script file
      SysUtils.DeleteFile(FScriptFile);

    FClient := TClient.Create(GetPluginPath());
    FClient.MOCR.FontPath := GetFontPath();
    FClient.IOManager.SetTarget(StrToIntDef(Target, 0));

    FCompiler := TSimbaScript_Compiler.Create(Tokenizer);
    FCompiler.OnFindFile := @HandleFindFile;
    FCompiler.OnHint := @HandleHint;
    FCompiler.OnHandleDirective := @HandleDirective;
    FCompiler.Debugging := FDebugging;

    FStartTime := GetTickCount64();

    if FCompiler.Compile() then
    begin
      WriteLn(Format('Succesfully compiled in %d milliseconds.', [GetTickCount64() - FStartTime]));
      Flush(Output);
      if FCompileOnly then
        Exit;

      if FDebugging then
        FDebugger := TSimbaScript_Debugger.Create(Self);

      FStartTime := GetTickCount64();

      try
        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FState);
      finally
        FTerminated := True;

        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, [], PCodePos(FCompiler['_OnTerminate'].Ptr)^);
      end;

      if (GetTickCount64() - FStartTime < 10000) then
        WriteLn(Format('Succesfully executed in %d milliseconds.', [GetTickCount64() - FStartTime]))
      else
        WriteLn(Format('Succesfully executed in %s.', [TimeStamp(GetTickCount64() - FStartTime)]));
    end;
  except
    on E: Exception do
      HandleException(E);
  end;

  Flush(Output);
end;

procedure TSimbaScript.DoStateThread;
var
  Stream: THandleStream;
  Value: UInt8;
begin
  Stream := THandleStream.Create(StdInputHandle);
  while Stream.Read(Value, SizeOf(Int32)) = SizeOf(Int32) do
  begin
    Self.State := TInitBool(Value);
    if Self.State = bFalse then
      FUserTerminated := True;
  end;

  Stream.Free();
end;

procedure TSimbaScript.SetState(Value: TInitBool);
var
  Method: TSimbaMethod;
begin
  FState := Value;

  if (FSimbaCommunication <> nil) then
  begin
    Method := TSimbaMethod_ScriptStateChanged.Create(Ord(FState));

    try
      FSimbaCommunication.Invoke(Method);
    finally
      Method.Free();
    end;
  end;
end;

procedure TSimbaScript.Invoke(Method: TSimbaMethod);
begin
  if (FSimbaCommunication = nil) then
    raise Exception.Create(Method.ClassName + ' not available when running headless');

  FSimbaCommunication.Invoke(Method);
end;

constructor TSimbaScript.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
end;

destructor TSimbaScript.Destroy;
begin
  try
    if (FDebugger <> nil) then
      FDebugger.Free();
    if (FSimbaCommunication <> nil) then
      FSimbaCommunication.Free();
    if (FCompiler <> nil) then
      FCompiler.Free();
  except
  end;

  inherited Destroy();
end;

end.

