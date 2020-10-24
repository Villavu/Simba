unit simba.script_import_system;

{$mode objfpc}{$H+}
{$macro ON}

interface

{$i import_uses.inc}

procedure Lape_Import_System(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  forms, lazutf8, lpvartypes,
  simba.mufasabase;

procedure Lape_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript._Write(PString(Params^[0])^);
end;

procedure Lape_WriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript._WriteLn('');
end;

procedure Lape_GetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariableUTF8(PString(Params^[0])^);
end;

procedure Lape_GetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IFDEF DARWIN}
  PPtrUInt(Result)^ := 0;
  {$ELSE}
  PPtrUInt(Result)^ := GetCurrentThreadID();
  {$ENDIF}
end;

procedure Lape_Wait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

type
  TSync = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSync.Execute;
type
  TSyncProcedure = procedure; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(PPointer(Params^[0])^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_Sync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := TSync.Create();
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
end;

type
  TSyncObject = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSyncObject.Execute;
type
  TSyncProcedure = procedure of object; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(Params^[0]^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_SyncObject(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSyncObject;
begin
  Sync := TSyncObject.Create();
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
end;

procedure Lape_PauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bUnknown;
end;

procedure Lape_TerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bFalse;
end;

procedure Lape_IsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated;
end;

procedure Lape_IsUserTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated and SimbaScript.UserTerminated;
end;

procedure Lape_EnterMethod(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
//var
//  Event: TSimbaScript_DebuggerEvent;
begin
  {
  with ScriptInstance do
  begin
    _DebuggingIndent += 1;
    _DebuggingMethod := PUInt16(Params^[0])^;

    Event.Method := _DebuggingMethod;
    Event.Indent := _DebuggingIndent;

    FDebuggerThread.Queue(Event);
  end;
  }
end;

procedure Lape_LeaveMethod(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
//var
//  Event: TSimbaScript_DebuggerEvent;
begin
  {
  with ScriptInstance do
  begin
    Event.Method := PUInt16(Params^[0])^;
    Event.Indent := _DebuggingIndent;

    if Event.Method <> _DebuggingMethod then
      FDebuggerThread.Queue(Event);

    _DebuggingIndent -= 1;
  end; }
end;

procedure Lape_AddOnTerminate_String(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  TProcedureArray = array of procedure;
var
  Global: TLapeGlobalVar;
begin
  Global := SimbaScript.Compiler.Globals[PString(Params^[0])^];

  if (Global <> nil) and (Global.VarType is TLapeType_Method) then
  begin
     Insert(
       TProcedure(PPointer(Global.Ptr)^),
       TProcedureArray(SimbaScript.Compiler.Globals['_OnTerminateProcedures'].Ptr^),
       High(Integer)
     );
  end else
    raise Exception.Create('AddOnTerminate: Global not found');
end;

procedure Lape_AddOnTerminate_Procedure(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  TProcedureArray = array of procedure;
var
  Method: Pointer;
  Declarations: TLapeDeclarationList;
  Global: TLapeGlobalVar;
  I: Int32;
begin
  Method := PPointer(Params^[0])^;

  Declarations := SimbaScript.Compiler.GlobalDeclarations;

  for I := 0 to Declarations.Count - 1 do
    if (Declarations[I] is TLapeGlobalVar) then
    begin
      Global := TLapeGlobalVar(Declarations[I]);
      if (Global.Ptr <> nil) and (PPointer(Global.Ptr^) = Method) then
      begin
        Insert(
           TProcedure(Method),
           TProcedureArray(SimbaScript.Compiler.Globals['_OnTerminateProcedures'].Ptr^),
           High(Integer)
         );

        Exit;
      end;
    end;

  raise Exception.Create('AddOnTerminate: Global not found');
end;

procedure Lape_AddOnTerminate_ProcedureOfObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  TProcedureArray = array of procedure of object;
var
  Method: TMethod;
  Declarations: TLapeDeclarationList;
  Global: TLapeGlobalVar;
  I: Int32;
begin
  Method := PMethod(Params^[0])^;

  Declarations := SimbaScript.Compiler.GlobalDeclarations;

  for I := 0 to Declarations.Count - 1 do
    if (Declarations[I] is TLapeGlobalVar) then
    begin
      Global := TLapeGlobalVar(Declarations[I]);

      if (Global.Ptr <> nil) and (Global.Ptr = Method.Data) then
      begin
        Insert(
           TProcedureOfObject(Method),
           TProcedureArray(SimbaScript.Compiler.Globals['_OnTerminateProcedureOfObjects'].Ptr^),
           High(Integer)
         );

        Exit;
      end;
    end;

  raise Exception.Create('AddOnTerminate: Global not found');
end;

procedure Lape_Import_System(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'System';

    addBaseDefine('SIMBA' + Format('%d', [SimbaVersion]));
    addBaseDefine('SIMBAMAJOR' + Format('%d', [SimbaMajor]));

    addBaseDefine('FPCFULLVERSION' + Format('%d', [FPC_FULLVERSION]));
    addBaseDefine('FPCVERSION' + Format('%d', [FPC_VERSION]));
    addBaseDefine('FPCRELEASE' + Format('%d', [FPC_RELEASE]));
    addBaseDefine('FPCPATCH' + Format('%d', [FPC_PATCH]));

    {$IFDEF CPU32}
    addBaseDefine('CPU32');
    addBaseDefine('CPU386');
    {$ENDIF}
    {$IFDEF CPU64}
    addBaseDefine('CPU64');
    {$ENDIF}
    {$IFDEF CPUAARCH64}
    addBaseDefine('CPUAARCH64');
    {$ENDIF}
    {$IFDEF WINDOWS}
    addBaseDefine('WINDOWS');
    {$ENDIF}
    {$IFDEF WIN32}
    addBaseDefine('WIN32');
    {$ENDIF}
    {$IFDEF WIN64}
    addBaseDefine('WIN64');
    {$ENDIF}
    {$IFDEF LINUX}
    addBaseDefine('LINUX');
    {$ENDIF}
    {$IFDEF DARWIN}
    addBaseDefine('DARWIN');
    {$ENDIF}

    if (SimbaScript <> nil) then
    begin
      if SimbaScript.Headless then
        addBaseDefine('SIMBAHEADLESS');

      addGlobalVar('TClient', @SimbaScript.Client, 'Client');

      addGlobalConst(SimbaScript.ScriptFile, 'ScriptFile');
      addGlobalConst(SimbaScript.ScriptPath, 'ScriptPath');
      addGlobalConst(SimbaScript.IncludePath, 'IncludePath');
      addGlobalConst(SimbaScript.PluginPath, 'PluginPath');
      addGlobalConst(SimbaScript.FontPath, 'FontPath');
      addGlobalConst(SimbaScript.AppPath, 'AppPath');
    end else
    begin
      addDelayedCode(
        'var Client: TClient;'    + LineEnding +
        'const ScriptFile  = "";' + LineEnding +
        'const ScriptPath  = "";' + LineEnding +
        'const IncludePath = "";' + LineEnding +
        'const PluginPath  = "";' + LineEnding +
        'const FontPath    = "";' + LineEnding +
        'const AppPath     = "";' + LineEnding
      );
    end;

    addGlobalConst(LineEnding, 'LineEnding');

    addGlobalFunc('procedure _Write(S: string); override;', @Lape_Write);
    addGlobalFunc('procedure _WriteLn; override;', @Lape_WriteLn);

    addGlobalConst(TThread.ProcessorCount, 'ProcessorCount');

    {$IFDEF DARWIN}
    addGlobalConst(0, 'MainThreadID');
    {$ELSE}
    addGlobalConst(MainThreadID, 'MainThreadID');
    {$ENDIF}

    addGlobalFunc('function GetCurrentThreadID: PtrUInt;', @Lape_GetCurrentThreadID);
    addGlobalFunc('function GetEnvironmentVariable(const Name: String): String;', @Lape_GetEnvironmentVariable);
    addGlobalFunc('procedure Wait(Milliseconds: UInt32);', @Lape_Wait);

    {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}
    addGlobalType('procedure()', 'TSyncMethod', FFI_CDECL);
    addGlobalType('procedure() of object', 'TSyncObjectMethod', FFI_CDECL);
    {$ELSE}
    addGlobalType('procedure()', 'TSyncMethod', FFI_DEFAULT_ABI);
    addGlobalType('procedure() of object', 'TSyncObjectMethod', FFI_DEFAULT_ABI);
    {$ENDIF}

    addGlobalFunc('procedure Sync(Method: TSyncMethod); overload;', @Lape_Sync);
    addGlobalFunc('procedure Sync(Method: TSyncObjectMethod); overload;', @Lape_SyncObject);

    addGlobalFunc('procedure TerminateScript;', @Lape_TerminateScript);
    addGlobalFunc('procedure PauseScript;', @Lape_PauseScript);
    addGlobalFunc('function IsTerminated: Boolean;', @Lape_IsTerminated);
    addGlobalFunc('function IsUserTerminated: Boolean;', @Lape_IsUserTerminated);

    addGlobalFunc('procedure _EnterMethod(constref Index: Int32); override;', @Lape_EnterMethod);
    addGlobalFunc('procedure _LeaveMethod(constref Index: Int32); override;', @Lape_LeaveMethod);

    addDelayedCode(
      'var _OnTerminateProcedures: array of procedure;'                   + LineEnding +
      'var _OnTerminateProcedureOfObjects: array of procedure of object;' + LineEnding +
      ''                                                                  + LineEnding +
      'procedure _OnTerminate;'                                           + LineEnding +
      'var I: Int32;'                                                     + LineEnding +
      'begin'                                                             + LineEnding +
      '  for I := 0 to High(_OnTerminateProcedures) do'                   + LineEnding +
      '    _OnTerminateProcedures[I]();'                                  + LineEnding +
      '  for I := 0 to High(_OnTerminateProcedureOfObjects) do'           + LineEnding +
      '    _OnTerminateProcedureOfObjects[I]();'                          + LineEnding +
      'end;'
    );

    addGlobalFunc('procedure AddOnTerminate(Proc: String); overload;', @Lape_AddOnTerminate_String);
    addGlobalFunc('procedure AddOnTerminate(Proc: procedure); overload;', @Lape_AddOnTerminate_Procedure);
    addGlobalFunc('procedure AddOnTerminate(Proc: procedure of object); overload;', @Lape_AddOnTerminate_ProcedureOfObject);

    addDelayedCode(
      'procedure MemMove(constref Src; var Dst; Size: SizeInt);'                + LineEnding +
      'begin'                                                                   + LineEnding +
      '  Move(Src, Dst, Size);'                                                 + LineEnding +
      'end;', 'MemMove', False);
  end;
end;

end.

