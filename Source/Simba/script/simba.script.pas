unit simba.script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lptypes, lpvartypes, lpcompiler, lpparser, lpinterpreter, lpmessages,
  simba.script_compiler, simba.client, simba.script_ipc;

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
    FSimbaMethods: TSimbaMethodClient;

    procedure HandleSimbaState;
    procedure HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
    procedure HandleException(Ex: Exception);

    function HandleFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function HandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure Execute; override;

    function GetHeadless: Boolean;

    procedure SetState(Value: TInitBool);
    procedure SetScriptFile(Value: String);
    procedure SetTarget(Value: String);
    procedure SetSimbaMethods(Value: String);
  public
    property CompileOnly: Boolean read FCompileOnly write FCompileOnly;

    property SimbaMethods: String write SetSimbaMethods;
    property Headless: Boolean read GetHeadless;

    property Client: TClient read FClient;
    property State: TInitBool read FState write SetState;
    property StartTime: UInt64 read FStartTime;
    property Target: String read FTarget write FTarget;

    property ScriptFile: String read FScriptFile write SetScriptFile;
    property ScriptName: String read FScriptName write FScriptName;

    property AppPath: String read FAppPath write FAppPath;
    property DataPath: String read FDataPath write FDataPath;
    property FontPath: String read FFontPath write FFontPath;
    property PluginPath: String read FPluginPath write FPluginPath;
    property IncludePath: String read FIncludePath write FIncludePath;
    property ScriptPath: String read FScriptPath write FScriptPath;

    procedure _Write(S: String);
    procedure _WriteLn(S: String);

    procedure Invoke(Method: TSimbaMethod);

    constructor Create; reintroduce;
  end;

var
  SimbaScript: TSimbaScript;

implementation

uses
  fileutil,
  simba.files, simba.script_plugin, simba.misc;

function TSimbaScript.GetHeadless: Boolean;
begin
  Result := FSimbaMethods = nil;
end;

procedure TSimbaScript.SetSimbaMethods(Value: String);
begin
  if (Value <> '') then
    FSimbaMethods := TSimbaMethodClient.Create(Value);
end;

procedure TSimbaScript.HandleHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  WriteLn(Hint);
end;

procedure TSimbaScript.HandleException(Ex: Exception);
var
  Method: TSimbaMethod;
begin
  WriteLn(Ex.Message);

  if (Ex is lpException) and (not Headless) then
    with Ex as lpException do
    begin
      Method := TSimbaMethod_ScriptError.Create(GetProcessID(), DocPos.Line, DocPos.Col, DocPos.FileName);

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
  try
    TThread.ExecuteInThread(@HandleSimbaState);

    FClient := TClient.Create();
    FClient.MOCR.FontPath := FFontPath;
    FClient.WriteLnProc := @_WriteLn;
    FClient.IOManager.SetTarget(StrToIntDef(Target, 0));

    if (FScriptName <> '') then
      FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptName))
    else
      FCompiler := TSimbaScript_Compiler.Create(TLapeTokenizerString.Create(FScript, FScriptFile));

    FCompiler.OnFindFile := @HandleFindFile;
    FCompiler.OnHint := @HandleHint;
    FCompiler.OnHandleDirective := @HandleDirective;

    FStartTime := GetTickCount64();

    if (not FCompiler.Compile()) then
      raise Exception.Create('Compiling failed');

    WriteLn(Format('Succesfully compiled in %d milliseconds.', [GetTickCount64() - FStartTime]));

    if FCompileOnly then
      Exit;

    FStartTime := GetTickCount64();
    FState := bTrue;

    RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FState);

    if (GetTickCount64() - FStartTime < 10000) then
      WriteLn(Format('Succesfully executed in %d milliseconds.', [GetTickCount64() - FStartTime]))
    else
      WriteLn(Format('Succesfully executed in %s.', [TimeStamp(GetTickCount64() - FStartTime)]));
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TSimbaScript.HandleSimbaState;
var
  Stream: THandleStream;
  Value: UInt8;
begin
  Stream := THandleStream.Create(StdInputHandle);
  while Stream.Read(Value, SizeOf(Int32)) = SizeOf(Int32) do
    Self.State := TInitBool(Value);

  Stream.Free();
end;

procedure TSimbaScript.SetState(Value: TInitBool);
var
  Method: TSimbaMethod;
begin
  FState := Value;

  Method := TSimbaMethod_ScriptStateChanged.Create(GetProcessID(), Ord(FState));
  try
    Self.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure TSimbaScript.SetScriptFile(Value: String);
begin
  if (FScriptFile = Value) then
    Exit;

  FScriptFile := Value;
  if FileExists(FScriptFile) then
    FScript := ReadFileToString(FScriptFile);

  if FScriptFile.EndsWith('.tmp') and (FScriptName <> '') then
    DeleteFile(FScriptFile);
end;

procedure TSimbaScript.SetTarget(Value: String);
begin
  FClient.IOManager.SetTarget(StrToIntDef(Value, 0));
end;

procedure TSimbaScript._Write(S: String);
begin
  FWriteBuffer := FWriteBuffer + S;
end;

procedure TSimbaScript._WriteLn(S: String);
begin
  WriteLn(FWriteBuffer + S);
  Flush(Output);

  FWriteBuffer := '';
end;

procedure TSimbaScript.Invoke(Method: TSimbaMethod);
begin
  try
    FSimbaMethods.Invoke(Method);
  except
    on E: Exception do
    begin
      WriteLn('Exception executing Simba method: ', Method.ClassName);
      WriteLn(E.Message);
    end;
  end;
end;

constructor TSimbaScript.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
end;

end.

