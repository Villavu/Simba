unit script_thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lpparser, lpcompiler, lptypes, lpvartypes, lpmessages, lpinterpreter,
  Client, Settings, SettingsSandbox, Files, script_plugins;

type
  PErrorData = ^TErrorData;
  TErrorData = record
    Line, Col: Int32;
    Error: String;
    FilePath: String;
  end;

  EMMLScriptOptions = set of (soCompileOnly, soWriteTimeStamp);
  EMMLScriptState = (ssRun, ssPause, ssStop);

  EMMLScriptTerminateOptions = set of (stoTerminated, stoUserTerminated);

  PMMLScriptThread = ^TMMLScriptThread;
  TMMLScriptThread = class(TThread)
  protected
    FCompiler: TLapeCompiler;
    FOutputBuffer: String;
    FOutput: TStrings;
    FRunning: TInitBool;
    FStartTime: UInt64;
    FClient: TClient;
    FOptions: EMMLScriptOptions;
    FSettings: TMMLSettingsSandbox;
    FUsedPlugins: TMPluginsList;
    FTerminateOptions: EMMLScriptTerminateOptions;

    procedure SetState(Value: EMMLScriptState);

    procedure OnHint(Sender: TLapeCompilerBase; Hint: lpString);

    function OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
    function OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

    procedure Flush;
    procedure HandleException(e: Exception);
    procedure CallTerminateMethod(Method: String);

    function Import: Boolean;
    function Compile: Boolean;

    procedure Execute; override;
  public
    Error: record Data: PErrorData; Callback: procedure() of object; end; // set by framescript
    AppPath, DocPath, ScriptPath, ScriptFile, IncludePath, PluginPath, FontPath: String; // set by TSimbaForm.InitializeTMThread

    property Options: EMMLScriptOptions read FOptions write FOptions;
    property Output: TStrings read FOutput write FOutput;
    property State: EMMLScriptState write SetState;
    property Client: TClient read FClient;
    property Settings: TMMLSettingsSandbox read FSettings;
    property StartTime: UInt64 read FStartTime;
    property TerminateOptions: EMMLScriptTerminateOptions read FTerminateOptions write FTerminateOptions;

    procedure Write(constref S: String);
    procedure WriteLn; overload;
    procedure WriteLn(constref S: String); overload;

    procedure SetSettings(From: TMMLSettings);
    procedure SetFonts(Path: String);

    function Kill: Boolean;

    constructor Create(constref Script, FilePath: String);
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF LINUX} pthreads, {$ENDIF}
  script_imports, fpexprpars, mmisc;

procedure TMMLScriptThread.SetState(Value: EMMLScriptState);
begin
  case Value of
    ssRun: FRunning := bTrue;
    ssStop: FRunning := bFalse;
    ssPause: FRunning := bUnknown;
  end;
end;

procedure TMMLScriptThread.OnHint(Sender: TLapeCompilerBase; Hint: lpString);
begin
  WriteLn(Hint);
end;

procedure TMMLScriptThread.Flush;
begin
  if (FOutput <> nil) and (FOutputBuffer <> '') then
    FOutput.Add(FOutputBuffer);

  FOutputBuffer := '';
end;

procedure TMMLScriptThread.HandleException(e: Exception);
begin
  if (Error.Callback <> nil) and (Error.Data <> nil) then
  begin
    Self.Error.Data^ := Default(TErrorData);

    if (e is lpException) then
    begin
      with (e as lpException) do
      begin
        Self.Error.Data^.Line := DocPos.Line;
        Self.Error.Data^.Col := DocPos.Col;
        Self.Error.Data^.FilePath := DocPos.FileName;
        Self.Error.Data^.Error := Message;
      end;
    end else
      Self.Error.Data^.Error := 'ERROR: ' + e.ClassName + ' :: ' + e.Message;

    Synchronize(Error.Callback);
  end;
end;

procedure TMMLScriptThread.CallTerminateMethod(Method: String);
begin
  FTerminateOptions := FTerminateOptions + [stoTerminated];

  RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, nil, TCodePos(FCompiler.getGlobalVar(Method).Ptr^));

  Flush();
end;

function TMMLScriptThread.OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), IncludePath, ScriptPath])) then
    FileName := '';
end;

type
  TLapeCompiler_Helper = class helper for TLapeCompiler
  public
    procedure pushConditional(AEval: Boolean; ADocPos: TDocPos);
  end;

procedure TLapeCompiler_Helper.pushConditional(AEval: Boolean; ADocPos: TDocPos);
begin
  inherited pushConditional(AEval, ADocPos);
end;

function TMMLScriptThread.OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  Arguments: TStringArray;
  Parser: TFPExpressionParser;
  Plugin: TMPlugin;
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
              Sender.pushConditional((not InIgnore) and Parser.AsBoolean, Sender.DocPos);
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

            Plugin := Plugins.Get(Sender.Tokenizer.FileName, Argument, True);
            for i := 0 to Plugin.Declarations.Count - 1 do
              Plugin.Declarations[i].Import(Sender);

            FUsedPlugins.Add(Plugin);
          end;

        'IFHASLIB':
          begin
            try
              Plugin := Plugins.Get(Sender.Tokenizer.FileName, Argument, False);
            except
              Plugin := nil;
            end;

            Sender.pushConditional((not InIgnore) and (Plugin <> nil), Sender.DocPos);
          end;

        'IFHASFILE':
          begin
            Sender.pushConditional((not InIgnore) and FindFile(Argument, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), IncludePath, ScriptPath]), Sender.DocPos);
          end;
      end;
    except
      on e: Exception do
        raise lpException.Create(e.Message, Sender.DocPos);
    end;

    Exit(True);
  end;

  Exit(False);
end;

function TMMLScriptThread.Import: Boolean;
var
  i: Int32;
begin
  Result := False;

  FCompiler.StartImporting();

  try
    for i := 0 to ScriptImports.Count - 1 do
      ScriptImports.Import(ScriptImports.Keys[i], FCompiler, Self);

    Result := True;
  except
    on e: Exception do
      HandleException(e);
  end;

  FCompiler.EndImporting();
end;

function TMMLScriptThread.Compile: Boolean;
var
  T: UInt64;
begin
  Result := False;

  try
    T := GetTickCount64();

    if FCompiler.Compile() then
    begin
      Self.Write('Compiled successfully in ' + IntToStr(GetTickCount64() - T) + ' ms.');
      Self.WriteLn();

      Result := True;
    end;
  except
    on e: Exception do
      HandleException(e);
  end;
end;

procedure TMMLScriptThread.Execute;
begin
  {$IFDEF LINUX}
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, nil);
  {$ENDIF}

  FRunning := bTrue;

  try
    if Self.Import() and Self.Compile() then
    begin
      if (soCompileOnly in FOptions) then
        Exit;

      FStartTime := GetTickCount64();

      try
        RunCode(FCompiler.Emitter.Code, FCompiler.Emitter.CodeLen, FRunning);

        CallTerminateMethod('__OnTerminate');
      except
        on e: Exception do
        begin
          CallTerminateMethod('__OnTerminate_Always');

          HandleException(e);
        end;
      end;

      if (GetTickCount64() - FStartTime <= 60000) then
        WriteLn('Successfully executed in ' + IntToStr(GetTickCount64() - FStartTime) + ' ms.')
      else
        WriteLn('Successfully executed in ' + TimeStamp(GetTickCount64() - FStartTime) + '.');
    end;
  finally
    Terminate();
  end;
end;

constructor TMMLScriptThread.Create(constref Script, FilePath: String);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FCompiler := TLapeCompiler.Create(TLapeTokenizerString.Create(Script, FilePath));
  FCompiler.OnFindFile := @OnFindFile;
  FCompiler.OnHandleDirective := @OnHandleDirective;
  FCompiler.OnHint := @OnHint;
  FCompiler['Move'].Name := 'MemMove';

  FClient := TClient.Create();
  FClient.WriteLnProc := @Self.WriteLn;

  FOutput := nil;
  FOutputBuffer := '';

  FUsedPlugins := TMPluginsList.Create(False);

  FTerminateOptions := [];
end;

destructor TMMLScriptThread.Destroy;
var
  i: Int32;
begin
  inherited Destroy();

  if (FUsedPlugins <> nil) then
  begin
    for i := 0 to FUsedPlugins.Count - 1 do
      FUsedPlugins[i].RefCount := FUsedPlugins[i].RefCount - 1;

    FreeAndNil(FUsedPlugins);
  end;

  if (FClient <> nil) then
    FreeAndNil(FClient);
  if (FSettings <> nil) Then
    FreeAndNil(FSettings);
  if (FCompiler <> nil) then
    FreeAndNil(FCompiler);
end;

procedure TMMLScriptThread.Write(constref S: String);
begin
  FOutputBuffer := FOutputBuffer + S;
end;

procedure TMMLScriptThread.WriteLn;
begin
  if (soWriteTimeStamp in FOptions) then
    FOutputBuffer := '[' + TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(GetTickCount64() - FStartTime))) + ']: ' + FOutputBuffer;

  Synchronize(@Flush);
end;

procedure TMMLScriptThread.WriteLn(constref S: String);
begin
  Write(S);
  WriteLn();
end;

{$IFDEF WINDOWS}
function TMMLScriptThread.Kill: Boolean;
begin
  CallTerminateMethod('__OnTerminate_Always');

  if (KillThread(Handle) = 0) and (WaitForThreadTerminate(Handle, 2500) = 0) then
  begin
    OnTerminate(Self);

    if (FClient <> nil) then
      FreeAndNil(FClient);
    if (FSettings <> nil) Then
      FreeAndNil(FSettings);
    if (FCompiler <> nil) then
      FreeAndNil(FCompiler);

    Exit(True);
  end;

  Exit(False);
end;
{$ENDIF}

{$IFDEF LINUX}
(*
  For this to work `thread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, nil);` must be called on the script thread and
  `pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, nil);` must called on simba initialization.
*)
function TMMLScriptThread.Kill: Boolean;
const
  ESRCH = 3;
var
  T: UInt64;
begin
  pthread_detach(Handle);

  if (pthread_cancel(Handle) = 0) then
  begin
    T := GetTickCount64() + 2500;

    while (T > GetTickCount64()) do
    begin
      if (pthread_kill(Handle, 0) = ESRCH) then
      begin
        OnTerminate(Self);

        if (FClient <> nil) then
          FreeAndNil(FClient);
        if (FSettings <> nil) Then
          FreeAndNil(FSettings);
        if (FCompiler <> nil) then
          FreeAndNil(FCompiler);

        Exit(True);
      end;

      Sleep(1);
    end;
  end;

  Exit(False);
end;
{$ENDIF}

procedure TMMLScriptThread.SetSettings(From: TMMLSettings);
begin
  FSettings := TMMLSettingsSandbox.Create(From);
  FSettings.Prefix := 'Scripts/';
end;

procedure TMMLScriptThread.SetFonts(Path: String);
var
  Directory: String;
begin
  FClient.MOCR.SetPath(Path);
  for Directory in GetDirectories(Path) do
    FCompiler.addGlobalVar(Directory, Directory).isConstant := True;
end;

end.

