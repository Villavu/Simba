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
    FScript: String;

    FAppPath: String;
    FDataPath: String;
    FIncludePath: String;
    FFontPath: String;
    FPluginPath: String;
    FScriptPath: String;

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
    procedure HandleException(Ex: Exception);

    function HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure Execute; override;

    procedure SetState(Value: TInitBool);
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

    property AppPath: String read FAppPath write FAppPath;
    property DataPath: String read FDataPath write FDataPath;
    property FontPath: String read FFontPath write FFontPath;
    property PluginPath: String read FPluginPath write FPluginPath;
    property IncludePath: String read FIncludePath write FIncludePath;
    property ScriptPath: String read FScriptPath write FScriptPath;

    procedure Invoke(Method: TSimbaMethod);

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  SimbaScript: TSimbaScript;

implementation

uses
  fileutil,
  simba.files, simba.script_plugin, simba.misc, simba.script_debugger;

procedure TSimbaScript.HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  WriteLn(Hint);
end;

procedure TSimbaScript.HandleException(Ex: Exception);
var
  Method: TSimbaMethod;
begin
  WriteLn(Ex.Message);

  if (Ex is lpException) and (FSimbaCommunication <> nil) then
    with Ex as lpException do
    begin
      Method := TSimbaMethod_ScriptError.Create(DocPos.Line, DocPos.Col, DocPos.FileName);

      try
        Self.Invoke(Method);
      finally
        Method.Free();
      end;
    end;
end;

function TSimbaScript.HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, '', [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), FIncludePath, FAppPath])) then
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
          if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath, FAppPath]) then
            raise Exception.Create('Plugin "' + Argument + '" not found');

          Plugin := TSimbaScriptPlugin.Create(Argument);
          Plugin.Import(FCompiler);
        end;

      'LIBPATH':
        begin
          if not FindPlugin(Argument, [ExtractFileDir(Sender.Tokenizer.FileName), FPluginPath, FAppPath]) then
            Argument := '';

          FCompiler.pushTokenizer(TLapeTokenizerString.Create(#39 + Argument + #39));
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

procedure TSimbaScript.Execute;
begin
  FState := bTrue;

  TThread.ExecuteInThread(@DoStateThread);

  try
    if (FSimbaCommunicationServer <> '') then
      FSimbaCommunication := TSimbaCommunicationClient.Create(FSimbaCommunicationServer);

    FScript := ReadFileToString(FScriptFile);
    if (FScriptName <> '') then
    begin
      DeleteFile(FScriptFile);

      FScriptFile := FScriptName;
    end;

    FClient := TClient.Create();
    FClient.MOCR.FontPath := FFontPath;
    FClient.IOManager.SetTarget(StrToIntDef(Target, 0));

    FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerFile.Create(FScript, FScriptFile));
    FCompiler.OnFindFile := @HandleFindFile;
    FCompiler.OnHint := @HandleHint;
    FCompiler.OnHandleDirective := @HandleDirective;
    FCompiler.Debugging := FDebugging;

    FStartTime := GetTickCount64();

    if FCompiler.Compile() then
    begin
      WriteLn(Format('Succesfully compiled in %d milliseconds.', [GetTickCount64() - FStartTime]));
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

  if FSimbaCommunication <> nil then
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

initialization
  Randomize();

end.

