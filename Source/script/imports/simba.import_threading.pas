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
  simba.base, simba.script, simba.vartype_string, simba.script_threading;

procedure ImportThreading(Script: TSimbaScript);

implementation

uses
  lptypes, lpvartypes, lpmessages,
  simba.threading;

type
  PSimbaThread = ^TSimbaThread;
  PSimbaLock = ^TSimbaLock;

(*
Threading
=========
Multithreading methods.
*)

procedure _LapeCreateThread(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThread(Result^) := TSimbaThread.Create(TLapeCodeEmitter(Params^[0]), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThread(Result^).Start();
end;

procedure _LapeCreateThreadEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadEx(Result^) := TSimbaThreadEx.Create(TLapeCodeEmitter(Params^[0]), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThreadEx(Result^).Params := Copy(TPointerArray(Params^[3]^));
  TSimbaThreadEx(Result^).Start();
end;

procedure _LapeCreateThreadAnon(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThread(Result^) := TSimbaThread.Create(TLapeCodeEmitter(Params^[0]), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThread(Result^).FreeOnTerminate := True;
  TSimbaThread(Result^).Start();
end;

procedure _LapeCreateThreadAnonEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadEx(Result^) := TSimbaThreadEx.Create(TLapeCodeEmitter(Params^[0]), PMethod(Params^[1])^, PMethod(Params^[2])^);
  TSimbaThreadEx(Result^).Params := Copy(TPointerArray(Params^[3]^));
  TSimbaThreadEx(Result^).FreeOnTerminate := True;
  TSimbaThreadEx(Result^).Start();
end;

procedure _LapeCreateThreadSchedule(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadSchedule(Result^) := TSimbaThreadSchedule.Create(TLapeCodeEmitter(Params^[0]), PMethod(Params^[1])^);
  TSimbaThreadSchedule(Result^).Interval := PInteger(Params^[2])^;
  TSimbaThreadSchedule(Result^).Name := PString(Params^[3])^;
  TSimbaThreadSchedule(Result^).Start();
end;

procedure _LapeCreateThreadScheduleEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaThreadScheduleEx(Result^) := TSimbaThreadScheduleEx.Create(TLapeCodeEmitter(Params^[0]), PMethod(Params^[1])^);
  TSimbaThreadScheduleEx(Result^).Params := Copy(TPointerArray(Params^[2]^));
  TSimbaThreadScheduleEx(Result^).Interval := PInteger(Params^[3])^;
  TSimbaThreadScheduleEx(Result^).Name := PString(Params^[4])^;
  TSimbaThreadScheduleEx(Result^).Start();
end;

(*
TThread.Name
------------
```
property TThread.Name: String;
property TThread.Name(Value: String);
```
*)
procedure _LapeThread_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaThread(Params^[0])^.Name;
end;

procedure _LapeThread_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaThread(Params^[0])^.Name := PString(Params^[1])^;
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
  PBoolean(Result)^ := not PSimbaThread(Params^[0])^.Finished;
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
  PUInt64(Result)^ := UInt64(PSimbaThread(Params^[0])^.ThreadID);
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
  PBoolean(Result)^ := PSimbaThread(Params^[0])^.Terminated;
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
  with PSimbaThread(Params^[0])^ do
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
  PSimbaThread(Params^[0])^.Terminate();
end;

(*
TThread.WaitForTerminate
------------------------
```
procedure TThread.WaitForTerminate;
function TThread.WaitForTerminate(Timeout: Int32): Boolean;
```
*)
procedure _LapeThread_WaitForTerminate1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaThread(Params^[0])^.WaitFor();
end;

procedure _LapeThread_WaitForTerminate2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaThread(Params^[0])^.WaitForTerminate(PInteger(Params^[1])^);
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
  PSimbaThread(Params^[0])^.Free();
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
Returns the current thread instance.

```{note}
This will return nil if called outside of a script created thread.
```
*)
procedure _LapeCurrentThread(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if (TThread.CurrentThread is TSimbaThread) then
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

procedure ImportThreading(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Threading';

    addGlobalVar(
      'record'                    + LineEnding +
       '  CoreCount: Int32;'      + LineEnding +
       '  ThreadCount: Int32;'    + LineEnding +
       '  PhysicalMemory: Int32;' + LineEnding +
       '  PCoreCount: Int32;'     + LineEnding +
       'end;',
       @SimbaCPUInfo,
       'CPUInfo'
    ).isConstant := True;

    addGlobalType('type Pointer', 'TThread');
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

    addGlobalType('type TBaseClass', 'TLock');
    addGlobalFunc('function TLock.Create: TLock; static;', @_LapeLock_Create);
    addGlobalFunc('function TLock.TryEnter: Boolean;', @_LapeLock_TryEnter);
    addGlobalFunc('procedure TLock.Enter;', @_LapeLock_Enter);
    addGlobalFunc('procedure TLock.Leave;', @_LapeLock_Leave);
    addGlobalFunc('procedure TLock.Free;', @_LapeLock_Free);

    addGlobalFunc('function CurrentThread: TThread', @_LapeCurrentThread);

    DumpSection := '!Hidden';

    addGlobalMethod('function _CreateThread(Method: procedure of object; OnTerminate: procedure(Thread: TThread) of object): TThread;', @_LapeCreateThread, Script.Compiler.Emitter);
    addGlobalMethod('function _CreateThreadEx(Method: procedure(Params: TPointerArray) of object; OnTerminate: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread;', @_LapeCreateThreadEx, Script.Compiler.Emitter);
    addGlobalMethod('function _CreateThreadAnon(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object): TThread;', @_LapeCreateThreadAnon, Script.Compiler.Emitter);
    addGlobalMethod('function _CreateThreadAnonEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread;', @_LapeCreateThreadAnonEx, Script.Compiler.Emitter);
    addGlobalMethod('function _CreateThreadSchedule(Method: procedure of object; Interval: Integer; Name: String): TThread', @_LapeCreateThreadSchedule, Script.Compiler.Emitter);
    addGlobalMethod('function _CreateThreadScheduleEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray; Interval: Integer; Name: String): TThread', @_LapeCreateThreadScheduleEx, Script.Compiler.Emitter);

    addGlobalVar('array of TThread', nil, '_ScheduleThreads');
    with addGlobalVar('TLock', nil, '_ScheduleLock') do
      PPointer(Ptr)^ := TSimbaLock.Create(True);

    addGlobalFunc(
      'procedure _ScheduleEvery(Name: String; Method: procedure of object; Interval: Integer);', [
      'begin',
      '  _ScheduleLock.Enter();',
      '  try',
      '    if (not IsScriptMethod(Method)) then',
      '      raise "Script method expected";',
      '    _ScheduleThreads += _CreateThreadSchedule(Method, Interval, Name);',
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
      '    _ScheduleThreads += _CreateThreadScheduleEx(Method, Params, Interval, Name);',
      '  finally',
      '    _ScheduleLock.Leave();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure _ScheduleStop(Name: String);', [
      'var I: Integer;',
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

    DumpSection := 'Threading';

    addGlobalFunc(
      'function TThread.Create(Method: procedure of object): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThread(Method, nil);',
      'end;'
    ]);

    addGlobalFunc(
      'function TThread.Create(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) or (not IsScriptMethod(OnTerminateMethod)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThread(Method, OnTerminateMethod);',
      'end;'
    ]);

    addGlobalFunc(
      'function TThread.CreateEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThreadEx(Method, nil, Params);',
      'end;'
    ]);

    addGlobalFunc(
      'function TThread.CreateEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray): TThread; static; overload;', [
      'begin',
      '  if (not IsScriptMethod(Method)) or (not IsScriptMethod(OnTerminateMethod)) then',
      '    raise "Script method expected";',
      '  Result := _CreateThreadEx(Method, OnTerminateMethod, Params);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure RunInThread(Method: procedure of object); overload;', [
      'begin',
      '  _CreateThreadAnon(Method, nil);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure RunInThread(Method: procedure of object; OnTerminateMethod: procedure(Thread: TThread) of object); overload;', [
      'begin',
      '  _CreateThreadAnon(Method, OnTerminateMethod);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure RunInThreadEx(Method: procedure(Params: TPointerArray) of object; Params: TPointerArray); overload;', [
      'begin',
      '  _CreateThreadAnonEx(Method, nil, Params);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure RunInThreadEx(Method: procedure(Params: TPointerArray) of object; OnTerminateMethod: procedure(Thread: TThread; Params: TPointerArray) of object; Params: TPointerArray); overload;', [
      'begin',
      '  _CreateThreadAnonEx(Method, OnTerminateMethod, Params);',
      'end;'
    ]);

    DumpSection := '';
  end;
end;

end.

