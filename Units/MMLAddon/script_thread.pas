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
  script_imports, fpexprpars, stringutil;

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

  RunCode(FCompiler.Emitter.Code, nil, TCodePos(FCompiler.getGlobalVar(Method).Ptr^));

  Flush();
end;

function TMMLScriptThread.OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), IncludePath, ScriptPath])) then
    FileName := '';
end;

type
   __TLapeCompiler = class(TLapeCompiler); // blasphemy!

function TMMLScriptThread.OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;

  function EvalExpression(Op, Left, Right: String): Boolean;
  var
    Parser: TFPExpressionParser;
  begin
    Parser := TFPExpressionParser.Create(nil);

    try
      Parser.Expression := Left + Trim(Op) + Right; // Simple :>

      Result := Parser.AsBoolean;
    finally
      Parser.Free();
    end;
  end;

  function EvalVersion(Op: String; Left, Right: array of Integer): Boolean; overload;
  begin
    case Trim(Op) of
      '=':  Result := (Left[0] = Right[0]) and (Left[1] = Right[1]) and (Left[2] = Right[2]);

      '<>': Result := (Left[0] <> Right[0]) or (Left[1] <> Right[1]) or (Left[2] <> Right[2]);

      '>':  Result := ((Left[0] > Right[0])) or
                      ((Left[0] >= Right[0]) and (Left[1] > Right[1])) or
                      ((Left[0] >= Right[0]) and (Left[1] >= Right[1]) and (Left[2] > Right[2]));

      '<':  Result := ((Left[0] < Right[0])) or
                      ((Left[0] <= Right[0]) and (Left[1] < Right[1])) or
                      ((Left[0] <= Right[0]) and (Left[1] <= Right[1]) and (Left[2] < Right[2]));
    end;
  end;

  function EvalVersion(Op: String; Left, Right: TStringArray): Boolean; overload;
  begin
    if (Length(Left) <> 3) or (Length(Right) <> 3) then
      raise Exception.Create('');

    Result := EvalVersion(Op, [StrToInt(Left[0]), StrToInt(Left[1]), StrToInt(Left[2])], [StrToInt(Right[0]), StrToInt(Right[1]), StrToInt(Right[2])]);
  end;

var
  Plugin: TMPlugin;
  i: Int32;
  lpe: lpException;
  Eval: Boolean;
  Op: String;
  Args: TStringArray;
begin
  if (UpperCase(Directive) = 'LOADLIB') then
  begin
    if InPeek or InIgnore or (Argument = '') then
      Exit(True);

    try
      Plugin := Plugins.Get(Sender.Tokenizer.FileName, Argument, True);
      for i := 0 to Plugin.Declarations.Count - 1 do
        Plugin.Declarations[i].Import(Sender);

      FUsedPlugins.Add(Plugin);
    except
      on e: Exception do
      begin
        lpe := lpException.Create('', Sender.DocPos); // raise a lape exception so we get the docpos when exception is handled.
        lpe.Message := e.Message;

        raise lpe;
      end;
    end;

    Exit(True);
  end;

  if (UpperCase(Directive) = 'IFHASLIB') then
  begin
    try
      Plugin := Plugins.Get(Sender.Tokenizer.FileName, Argument, False);
    except
      Plugin := nil;
    end;

    with __TLapeCompiler(Sender) do
      pushConditional((not InIgnore) and (Plugin <> nil), Sender.DocPos);

    Exit(True);
  end;

  if (UpperCase(Directive) = 'IFVALUE') or (UpperCase(Directive) = 'IFVERSION') then
  begin
    Eval := False;

    if Pos(' = ', Argument) > 0 then Op := ' = ' else
    if Pos(' <> ', Argument) > 0 then Op := ' <> ' else
    if Pos(' > ', Argument) > 0 then Op := ' > ' else
    if Pos(' < ', Argument) > 0 then Op := ' < ' else Op := '';

    if (Op <> '') then
    begin
      Args := Explode(Op, Argument);

      if (Length(Args) = 2) then
      begin
        Args[0] := Trim(Sender.Defines.Values[Args[0]]);
        Args[1] := Trim(Args[1]);

        if (Args[0] <> '') and (Args[1] <> '') then
        begin
          case UpperCase(Directive) of
            'IFVALUE': // {$IFVALUE SIMBAMAJOR = 1300}
              try
                Eval := EvalExpression(Op, Args[0], Args[1]);
              except
                Exit(False);
              end;

            'IFVERSION': // {$IFVERSION SRL > 1.2.2}
              try
                Eval := EvalVersion(Op, Explode('.', Args[0]), Explode('.', Args[1]));
              except
                Exit(False);
              end;
          end;

          with __TLapeCompiler(Sender) do
             pushConditional((not InIgnore) and Eval, Sender.DocPos);

          Exit(True);
        end;
      end;
    end;
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
      Self.Write('Compiled succesfully in ' + IntToStr(GetTickCount64() - T) + ' ms.');
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
        RunCode(FCompiler.Emitter.Code, FRunning);

        CallTerminateMethod('__OnTerminate');
      except
        on e: Exception do
        begin
          CallTerminateMethod('__OnTerminate_Always');

          HandleException(e);
        end;
      end;

      if (GetTickCount64() - FStartTime <= 60000) then
        WriteLn('Succesfully executed in ' + IntToStr(GetTickCount64() - FStartTime) + ' ms.')
      else
        WriteLn('Succesfully executed in ' + TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(GetTickCount64() - FStartTime))) + '.');
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

