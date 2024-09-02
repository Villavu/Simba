{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.import_threading;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.vartype_string, simba.script_compiler, simba.threading;

procedure ImportThreading(Compiler: TSimbaScript_Compiler);

implementation

{$WARN 4046 ERROR} // stop compiling on creating a class with an abstract method

uses
  syncobjs,
  lptypes, lpmessages, lpvartypes, lpinterpreter;

type
  PSimbaLock = ^TSimbaLock;
  TSimbaLock = class(TObject)
  protected
    FCriticalSection: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    function TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
  end;

constructor TSimbaLock.Create;
begin
  inherited Create();

  FCriticalSection := TCriticalSection.Create();
end;

destructor TSimbaLock.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy();
end;

function TSimbaLock.TryEnter: Boolean;
begin
  Result := FCriticalSection.TryEnter();
end;

procedure TSimbaLock.Enter;
begin
  FCriticalSection.Enter();
end;

procedure TSimbaLock.Leave;
begin
  FCriticalSection.Leave();
end;

type
  TPointerArray = array of Pointer;

  PSimbaThreadBase = ^TSimbaThreadBase;
  TSimbaThreadBase = class(TThread)
  protected
    FCodeRunner: TLapeCodeRunner;
    FMethod: TMethod;
    FTerminateMethod: TMethod;
    FName: String;

    procedure Invoke(Method: TMethod; Params: array of Pointer);

    procedure DoMethod; virtual; abstract;
    procedure DoTerminateMethod; virtual; abstract;

    procedure DoTerminate; override;
    procedure Execute; override;

    function GetName: String;
    procedure SetName(Value: String);
  public
    constructor Create(Emitter: TLapeCodeEmitter; Method, TerminateMethod: TMethod); reintroduce;
    constructor Create(Emitter: TLapeCodeEmitter; Method: TMethod); reintroduce;
    destructor Destroy; override;

    function WaitForTerminate(Timeout: Int32): Boolean;

    property Name: String read GetName write SetName;
  end;

  TSimbaThread = class(TSimbaThreadBase)
  protected
    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  end;

  TSimbaThreadEx = class(TSimbaThreadBase)
  protected
    FParams: TPointerArray;

    function getParamsAsParam: Pointer;

    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  end;

  TSimbaThreadSchedule = class(TSimbaThreadBase)
  protected
    FInterval: Integer;
    FTerminateLock: TWaitableLock;

    procedure TerminatedSet; override;

    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  end;

  TSimbaThreadScheduleEx = class(TSimbaThreadBase)
  protected
    FInterval: Integer;
    FTerminateLock: TWaitableLock;
    FParams: TPointerArray;

    function getParamsAsParam: Pointer;
    procedure TerminatedSet; override;
    procedure DoMethod; override;
    procedure DoTerminateMethod; override;
  end;

function TSimbaThreadScheduleEx.getParamsAsParam: Pointer;
begin
  Result := Pointer(FParams);
  // inc ref count
  Inc(PSizeInt(Result - SizeOf(SizeInt) * 2)^);
end;

procedure TSimbaThreadScheduleEx.TerminatedSet;
begin
  inherited TerminatedSet;

  FTerminateLock.Unlock();
end;

procedure TSimbaThreadScheduleEx.DoMethod;
begin
  FTerminateLock.Lock();

  while not Terminated do
  begin
    Invoke(FMethod, [getParamsAsParam()]);

    FTerminateLock.WaitLocked(FInterval);
  end;
end;

procedure TSimbaThreadScheduleEx.DoTerminateMethod;
begin
  { nothing }
end;

procedure TSimbaThreadSchedule.TerminatedSet;
begin
  inherited TerminatedSet();

  FTerminateLock.Unlock();
end;

procedure TSimbaThreadSchedule.DoMethod;
begin
  FTerminateLock.Lock();

  while not Terminated do
  begin
    Invoke(FMethod, []);

    FTerminateLock.WaitLocked(FInterval);
  end;
end;

procedure TSimbaThreadSchedule.DoTerminateMethod;
begin
  { nothing }
end;

function TSimbaThreadEx.getParamsAsParam: Pointer;
begin
  Result := Pointer(FParams);
  // inc ref count
  Inc(PSizeInt(Result - SizeOf(SizeInt) * 2)^);
end;

procedure TSimbaThreadEx.DoMethod;
begin
  Invoke(FMethod, [getParamsAsParam()]);
end;

procedure TSimbaThreadEx.DoTerminateMethod;
begin
  Invoke(FTerminateMethod, [Self, getParamsAsParam()]);
end;

procedure TSimbaThread.DoMethod;
begin
  Invoke(FMethod, []);
end;

procedure TSimbaThread.DoTerminateMethod;
begin
  Invoke(FTerminateMethod, [Self]);
end;

procedure TSimbaThreadBase.Invoke(Method: TMethod; Params: array of Pointer);
var
  VarStack: TByteArray = nil;
  I: Integer;
begin
  SetLength(VarStack, SizeOf(Pointer) + (Length(Params) * SizeOf(Pointer)));
  PPointer(@VarStack[0])^ := Method.Data;
  for I := 0 to High(Params) do
    PPointer(@VarStack[SizeOf(Pointer) * (I+1)])^ := Params[I];

  FCodeRunner.Run(TCodePos(Method.Code), VarStack);
end;

procedure TSimbaThreadBase.DoTerminate;
begin
  if Assigned(FTerminateMethod.Code) then
    DoTerminateMethod();
end;

procedure TSimbaThreadBase.Execute;
begin
  if Assigned(FMethod.Code) then
    DoMethod();
end;

function TSimbaThreadBase.GetName: String;
begin
  Result := FName;
end;

procedure TSimbaThreadBase.SetName(Value: String);
begin
  FName := Value;
  NameThreadForDebugging(FName, ThreadID);
end;

constructor TSimbaThreadBase.Create(Emitter: TLapeCodeEmitter; Method, TerminateMethod: TMethod);
begin
  inherited Create(True, DefaultStackSize div 2);

  FCodeRunner := TLapeCodeRunner.Create(Emitter);
  FMethod := Method;
  FTerminateMethod := TerminateMethod;
end;

constructor TSimbaThreadBase.Create(Emitter: TLapeCodeEmitter; Method: TMethod);
begin
  Create(Emitter, Method, Default(TMethod));
end;

destructor TSimbaThreadBase.Destroy;
begin
  FreeAndNil(FCodeRunner);

  inherited Destroy();
end;

function TSimbaThreadBase.WaitForTerminate(Timeout: Int32): Boolean;
begin
  Result := WaitForThreadTerminate(ThreadID, Timeout) = 0;
end;

(*
Threading
=========
Multithreading methods.
*)

procedure _LapeCreateThread(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThread(Result^) := TSimbaThread.Create(TLapeCodeEmitter(Params^[0]^), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThread(Result^).Start();
end;

procedure _LapeCreateThreadEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadEx(Result^) := TSimbaThreadEx.Create(TLapeCodeEmitter(Params^[0]^), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThreadEx(Result^).FParams := TPointerArray(Params^[3]^);
  TSimbaThreadEx(Result^).Start();
end;

procedure _LapeCreateThreadAnon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThread(Result^) := TSimbaThread.Create(TLapeCodeEmitter(Params^[0]^), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThread(Result^).FreeOnTerminate := True;
  TSimbaThread(Result^).Start();
end;

procedure _LapeCreateThreadAnonEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadEx(Result^) := TSimbaThreadEx.Create(TLapeCodeEmitter(Params^[0]^), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThreadEx(Result^).FParams := TPointerArray(Params^[3]^);
  TSimbaThreadEx(Result^).FreeOnTerminate := True;
  TSimbaThreadEx(Result^).Start();
end;

procedure _LapeCreateThreadSchedule(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadSchedule(Result^) := TSimbaThreadSchedule.Create(TLapeCodeEmitter(Params^[0]^), PMethod(Params^[1])^);
  TSimbaThreadSchedule(Result^).FInterval := PInteger(Params^[2])^;
  TSimbaThreadSchedule(Result^).Name := PString(Params^[3])^;
  TSimbaThreadSchedule(Result^).Start();
end;

procedure _LapeCreateThreadScheduleEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadScheduleEx(Result^) := TSimbaThreadScheduleEx.Create(TLapeCodeEmitter(Params^[0]^), PMethod(Params^[1])^);
  TSimbaThreadScheduleEx(Result^).FParams := Copy(TPointerArray(Params^[2]^));
  TSimbaThreadScheduleEx(Result^).FInterval := PInteger(Params^[3])^;
  TSimbaThreadScheduleEx(Result^).Name := PString(Params^[4])^;
  TSimbaThreadScheduleEx(Result^).Start();
end;

(*
TThread.Name
------------
```
property TThread.Name: String;
```
*)
procedure _LapeThread_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaThreadBase(Params^[0])^.Name;
end;

(*
TThread.Name
------------
```
property TThread.Name(Value: String);
```
*)
procedure _LapeThread_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaThreadBase(Params^[0])^.Name := PString(Params^[1])^;
end;

(*
TThread.Running
---------------
```
property TThread.Running: Boolean;
```
*)
procedure _LapeThread_Running_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := not PSimbaThreadBase(Params^[0])^.Finished;
end;

(*
TThread.ThreadID
----------------
```
property TThread.ThreadID: UInt64;
```
*)
procedure _LapeThread_ThreadID_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt64(Result)^ := UInt64(PSimbaThreadBase(Params^[0])^.ThreadID);
end;

(*
TThread.IsTerminated
--------------------
```
property TThread.IsTerminated: Boolean;
```
*)
procedure _LapeThread_IsTerminated_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaThreadBase(Params^[0])^.Terminated;
end;

(*
TThread.FatalException
----------------------
```
property TThread.FatalException: String;
```
*)
procedure _LapeThread_FatalException_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with PSimbaThreadBase(Params^[0])^ do
  begin
    if (FatalException is lpException) then
    begin
      PString(Result)^ := lpException(FatalException).Error;

      // todo: fix lape adding "Runtime error:" to `Error`
      if PString(Result)^.StartsWith('Runtime error: "') then
        PString(Result)^ := PString(Result)^.CopyRange(Length('Runtime error: "') + 1, Length(PString(Result)^) - 1);
    end else if (FatalException is Exception) then
      PString(Result)^ := Exception(FatalException).Message
    else
      PString(Result)^ := '';
  end;
end;

(*
TThread.Terminate
-----------------
```
procedure TThread.Terminate;
```
*)
procedure _LapeThread_Terminate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaThreadBase(Params^[0])^.Terminate();
end;

(*
TThread.WaitForTerminate
------------------------
```
procedure TThread.WaitForTerminate;
```
*)
procedure _LapeThread_WaitForTerminate1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaThreadBase(Params^[0])^.WaitFor();
end;

(*
TThread.WaitForTerminate
------------------------
```
function TThread.WaitForTerminate(Timeout: Int32): Boolean;
```
*)
procedure _LapeThread_WaitForTerminate2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaThreadBase(Params^[0])^.WaitForTerminate(PInteger(Params^[1])^);
end;

(*
TThread.Free
------------
```
procedure TThread.Free;
```
*)
procedure _LapeThread_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaThreadBase(Params^[0])^.Free();
end;

(*
TLock.Create
------------
```
function TLock.Create: TLock; static;
```
*)
procedure _LapeLock_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaLock(Result)^ := TSimbaLock.Create();
end;

(*
TLock.TryEnter
--------------
```
function TLock.TryEnter: Boolean;
```
*)
procedure _LapeLock_TryEnter(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaLock(Params^[0])^.TryEnter();
end;

(*
TLock.Enter
-----------
```
procedure TLock.Enter;
```
*)
procedure _LapeLock_Enter(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaLock(Params^[0])^.Enter();
end;

(*
TLock.Leave
-----------
```
procedure TLock.Leave;
```
*)
procedure _LapeLock_Leave(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaLock(Params^[0])^.Leave();
end;

(*
TLock.Free
----------
```
procedure TLock.Free;
```
*)
procedure _LapeLock_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaLock(Params^[0])^.Free();
end;

(*
CurrentThread
-------------
```
function CurrentThread: TThread;
```
*)
procedure _LapeCurrentThread(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if (TThread.CurrentThread is TSimbaThreadBase) then
    TThread(Result^) := TThread.CurrentThread
  else
    TThread(Result^) := nil;
end;

(*
TThread.Create
--------------
```
function TThread.Create(Method: procedure of object): TThread; static;
function TThread.Create(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object): TThread; static;

```
*)

(*
TThread.CreateEx
----------------
```
function TThread.CreateEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray): TThread; static;
function TThread.CreateEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread; static;
```
*)

(*
RunInThread
-----------
```
procedure RunInThread(Method: procedure of object);
procedure RunInThread(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object);
```
*)

(*
RunInThreadEx
-------------
```
procedure RunInThreadEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray);
procedure RunInThreadEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray);
```
*)

procedure ImportThreading(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := '!Threading';

    addGlobalVar(
      'record'                    + LineEnding +
       '  CoreCount: Int32;'      + LineEnding +
       '  ThreadCount: Int32;'    + LineEnding +
       '  PhysicalMemory: Int32;' + LineEnding +
       'end;',
       @SimbaCPUInfo,
       'CPUInfo'
    ).isConstant := True;

    ImportingSection := 'Threading';

    addGlobalType('strict Pointer', 'TThread');
    addGlobalFunc('property TThread.Name: String', @_LapeThread_Name_Read);
    addGlobalFunc('property TThread.Name(Value: String)', @_LapeThread_Name_Write);
    addGlobalFunc('property TThread.Running: Boolean', @_LapeThread_Running_Read);
    addGlobalFunc('property TThread.ThreadID: UInt64', @_LapeThread_ThreadID_Read);
    addGlobalFunc('property TThread.FatalException: String', @_LapeThread_FatalException_Read);
    addGlobalFunc('property TThread.IsTerminated: Boolean', @_LapeThread_IsTerminated_Read);
    addGlobalFunc('procedure TThread.Terminate;', @_LapeThread_Terminate);
    addGlobalFunc('procedure TThread.WaitForTerminate; overload', @_LapeThread_WaitForTerminate1);
    addGlobalFunc('function TThread.WaitForTerminate(Timeout: Int32): Boolean; overload', @_LapeThread_WaitForTerminate2);
    addGlobalFunc('procedure TThread.Free;', @_LapeThread_Free);

    addGlobalType('strict Pointer', 'TLock');
    addGlobalFunc('function TLock.Create: TLock; static;', @_LapeLock_Create);
    addGlobalFunc('function TLock.TryEnter: Boolean;', @_LapeLock_TryEnter);
    addGlobalFunc('procedure TLock.Enter;', @_LapeLock_Enter);
    addGlobalFunc('procedure TLock.Leave;', @_LapeLock_Leave);
    addGlobalFunc('procedure TLock.Free;', @_LapeLock_Free);

    addGlobalFunc('function CurrentThread: TThread', @_LapeCurrentThread);

    ImportingSection := '!Threading';

    addGlobalVar(Emitter, '_CodeEmitter').isConstant := True;
    addGlobalFunc('function _CreateThread(Emitter: Pointer; Method: procedure of object; OnTerminate: procedure(Thread: TThread) of object): TThread;', @_LapeCreateThread);
    addGlobalFunc('function _CreateThreadEx(Emitter: Pointer; Method: procedure(Params: TPointerArray) of object; OnTerminate: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread;', @_LapeCreateThreadEx);
    addGlobalFunc('function _CreateThreadAnon(Emitter: Pointer; Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object): TThread;', @_LapeCreateThreadAnon);
    addGlobalFunc('function _CreateThreadAnonEx(Emitter: Pointer; Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread;', @_LapeCreateThreadAnonEx);
    addGlobalFunc('function _CreateThreadSchedule(Emitter: Pointer; Method: procedure of object; Interval: Integer; Name: String): TThread', @_LapeCreateThreadSchedule);
    addGlobalFunc('function _CreateThreadScheduleEx(Emitter: Pointer; Method: procedure(Params: TPointerArray) of object; Params: TPointerArray; Interval: Integer; Name: String): TThread', @_LapeCreateThreadScheduleEx);

    addGlobalVar('array of TThread', nil, '_ScheduleThreads');
    with addGlobalVar('TLock', nil, '_ScheduleLock') do
      PPointer(Ptr)^ := TSimbaLock.Create();

    addGlobalFunc(
      'procedure _ScheduleEvery(Name: String; Method: procedure of object; Interval: Integer);', [
      'begin',
      '  _ScheduleLock.Enter();',
      '  try',
      '    if (not IsScriptMethod(Method)) then',
      '      raise "Script method expected";',
      '    _ScheduleThreads += _CreateThreadSchedule(_CodeEmitter, Method, Interval, Name);',
      '  finally',
      '    _ScheduleLock.Leave();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure _ScheduleEveryEx(Name: String; Method: procedure(Params: TPointerArray) of object; Params: TPointerArray; Interval: Integer);', [
      'begin',
      '  _ScheduleLock.Enter();',
      '  try',
      '    if (not IsScriptMethod(Method)) then',
      '      raise "Script method expected";',
      '    _ScheduleThreads += _CreateThreadScheduleEx(_CodeEmitter, Method, Params, Interval, Name);',
      '  finally',
      '    _ScheduleLock.Leave();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure _ScheduleStop(Name: String);', [
      'var I: Int32;',
      'begin',
      '  _ScheduleLock.Enter();',
      '  try',
      '    for I := 0 to High(_ScheduleThreads) do',
      '      if (_ScheduleThreads[i].Name = Name) then',
      '      begin',
      '        _ScheduleThreads[i].Terminate();',
      '        _ScheduleThreads[i].WaitForTerminate();',
      '        _ScheduleThreads[I].Free();',
      '      end;',
      '  finally',
      '    _ScheduleLock.Leave();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'function _ScheduleNames: TStringArray;', [
      'var I: Integer;',
      'begin',
      '  _ScheduleLock.Enter();',
      '  try',
      '    for I := 0 to High(_ScheduleThreads) do',
      '      if _ScheduleThreads[I].Running then',
      '        Result += _ScheduleThreads[I].Name;',
      '  finally',
      '    _ScheduleLock.Leave();',
      '  end;',
      'end;'
    ]);

    ImportingSection := 'Threading';

    addGlobalFunc(
      'function TThread.Create(Method: procedure of object): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThread(_CodeEmitter, Method, nil);',
      'end;'
    ]);

    addGlobalFunc(
      'function TThread.Create(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) or (not IsScriptMethod(OnTerminateMethod)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThread(_CodeEmitter, Method, OnTerminateMethod);',
      'end;'
    ]);

    addGlobalFunc(
      'function TThread.CreateEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThreadEx(_CodeEmitter, Method, nil, Params);',
      'end;'
    ]);

    addGlobalFunc(
      'function TThread.CreateEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) or (not IsScriptMethod(OnTerminateMethod)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThreadEx(_CodeEmitter, Method, OnTerminateMethod, Params);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure RunInThread(Method: procedure of object); overload;', [
      'begin',
      '  _CreateThreadAnon(_CodeEmitter, Method, nil);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure RunInThread(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object); overload;', [
      'begin',
      '  _CreateThreadAnon(_CodeEmitter, Method, nil);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure RunInThreadEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray); overload;', [
      'begin',
      '  _CreateThreadAnonEx(_CodeEmitter, Method, nil, Params);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure RunInThreadEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray); overload;', [
      'begin',
      '  _CreateThreadAnonEx(_CodeEmitter, Method, OnTerminateMethod, Params);',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

end.

