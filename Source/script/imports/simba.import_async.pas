unit simba.import_async;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.baseclass, simba.script;

procedure ImportASync(Script: TSimbaScript);

implementation

uses
  lptypes, ffi,
  simba.script_objectutil,
  simba.target,
  simba.target_asyncmovemouse,
  simba.http_async,
  simba.fs_async;

(*
ASync
=====
High level functions that run a task such as an HTTP request which run in the background on another thread.

```
procedure ThisIsCalledWhenFinished(constref Result: TASyncHTTPResult);
begin
  WriteLn(Result.Response);
  WriteLn(Result.Data);
end;

begin
  ASync.HTTPGet('httpbin.org/get', @ThisIsCalledWhenFinished);

  Sleep(5000); // give some time to complete
end;
```
*)

(*
ASync.HTTPGet
-------------
```
procedure ASync.HTTPGet(URL: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;
procedure ASync.HTTPGet(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;
```
*)
procedure _LapeASyncHTTP_Get1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, [], TASyncHTTPFinishEvent(Params^[1]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

procedure _LapeASyncHTTP_Get2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, PStringArray(Params^[1])^, TASyncHTTPFinishEvent(Params^[2]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

(*
ASync.HTTPGetFile
-----------------
```
procedure ASync.HTTPGetFile(URL: String; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;
procedure ASync.HTTPGetFile(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;

```
*)
procedure _LapeASyncHTTP_GetFile1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.GetFile(PString(Params^[0])^, [], PString(Params^[1])^, TASyncHTTPFinishEvent(Params^[2]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

procedure _LapeASyncHTTP_GetFile2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.GetFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^,TASyncHTTPFinishEvent(Params^[3]^), TASyncHTTPProgressEvent(Params^[4]^));
end;


(*
ASync.HTTPPost
--------------
```
procedure ASync.HTTPPost(URL, Data: String; OnFinish: TASyncHTTPFinishEvent); static;
procedure ASync.HTTPPost(URL: String; RequestHeaders: TStringArray; Data: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent); static;
```
*)
procedure _LapeASyncHTTP_Post1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, [], PString(Params^[1])^, TASyncHTTPFinishEvent(Params^[2]^));
end;

procedure _LapeASyncHTTP_Post2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, TASyncHTTPFinishEvent(Params^[3]^));
end;

(*
TASyncMouse.Construct
---------------------
```
function TASyncMouse.Construct(Target: TTarget): TASyncMouse; static;
```
Construct a ASyncMouse for a target.
The `new` keyword is used like so:
```
m := new TASyncMouse(Target);
```
*)
procedure _LapeASyncMouse_Construct(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectASyncMouse(Result)^^ := TASyncMouse.Create(PLapeObjectTarget(Params^[0])^^);
end;

procedure _LapeASyncMouse_Destroy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  LapeObjectDestroy(PLapeObject(Params^[0]));
end;

(*
TASyncMouse.Move
----------------
```
procedure TASyncMouse.Move(Dest: TPoint; Accuracy: Single = 1);
```
*)
procedure _LapeASyncMouse_Move(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectASyncMouse(Params^[0])^^.Move(PPoint(Params^[1])^, PSingle(Params^[2])^);
end;

(*
TASyncMouse.MouseChangeDest
---------------------------
```
property TASyncMouse.Destination(Value: TPoint);
property TASyncMouse.Destination: TPoint;
```
*)
procedure _LapeASyncMouse_Destination_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectASyncMouse(Params^[0])^^.Destination := PPoint(Params^[1])^;
end;

procedure _LapeASyncMouse_Destination_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PLapeObjectASyncMouse(Params^[0])^^.Destination;
end;

(*
TASyncMouse.IsMoving
--------------------
```
property TASyncMouse.IsMoving: Boolean;
```
*)
procedure _LapeASyncMouse_IsMoving_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectASyncMouse(Params^[0])^^.IsMoving;
end;

(*
TASyncMouse.Wait
----------------
```
function TASyncMouse.Wait(Timeout: Integer = -1): Boolean
```
*)
procedure _LapeASyncMouse_Wait(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectASyncMouse(Params^[0])^^.Wait(PInteger(Params^[1])^);
end;

(*
TASyncMouse.Stop
----------------
```
procedure TASyncMouse.Stop;
```
*)
procedure _LapeASyncMouse_Stop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObjectASyncMouse(Params^[0])^^.Stop();
end;

(*
ASync.FileUnzip
---------------
```
procedure ASync.FileUnzip(ZipFile, DestPath: String; OnFinish: TASyncUnzipFinishEvent; OnProgress: TASyncUnzipProgressEvent = nil); static;
```
*)
procedure _LapeASyncUnZip_Unzip(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncUnzip.Unzip(PString(Params^[0])^, PString(Params^[1])^, TASyncUnzipFinishEvent(Params^[2]^), TASyncUnzipProgressEvent(Params^[3]^));
end;

(*
ASync.Schedules
---------------
```
function ASync.Schedules: TStringArray; static;
```
Returns all running schedules.
*)

(*
ASync.ScheduleEvery
-------------------
```
function ASync.ScheduleEvery(Name: String; Method: procedure of object; Interval: Integer); static;
```
Schedule a method to be called every `interval` (in milliseconds).
*)

(*
ASync.ScheduleEvery
-------------------
```
function ASync.ScheduleEvery(Name: String; Method: procedure(Params: TPointerArray) of object; Params: TPointerArray; Interval: Integer); static;
```
ScheduleEvery with passing parameters to the method (as TPointerArray).
*)

(*
ASync.ScheduleStop
------------------
```
procedure ASync.ScheduleStop(Name: String); static;
```
Stop a scheduled method.
*)

procedure ImportASync(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'ASync';

    LapeObjectImport(Script.Compiler, 'TASyncMouse');
    addGlobalFunc('function TASyncMouse.Construct(Target: TTarget): TASyncMouse; static;', @_LapeASyncMouse_Construct);
    addGlobalFunc('procedure TASyncMouse.Destroy;', @_LapeASyncMouse_Destroy);
    addGlobalFunc('property TASyncMouse.Destination: TPoint;', @_LapeASyncMouse_Destination_Read);
    addGlobalFunc('property TASyncMouse.Destination(Value: TPoint);', @_LapeASyncMouse_Destination_Write);
    addGlobalFunc('property TASyncMouse.IsMoving: Boolean', @_LapeASyncMouse_IsMoving_Read);
    addGlobalFunc('function TASyncMouse.Wait(Timeout: Integer = -1): Boolean;', @_LapeASyncMouse_Wait);
    addGlobalFunc('procedure TASyncMouse.Stop;', @_LapeASyncMouse_Stop);
    addGlobalFunc('procedure TASyncMouse.Move(Dest: TPoint; Accuracy: Single = 0.5); overload;', @_LapeASyncMouse_Move);

    // namespace
    addGlobalType('record end;', 'ASync');

    addGlobalType([
      'record',
      '  URL: String;',
      '  Response: EHTTPStatus;',
      '  Data: String;',
      '  Headers: TStringArray;',
      '  Exception: String;',
      '  TimeUsed: Double;',
      'end;'],
      'TASyncHTTPResult');

    addGlobalType('procedure(constref Result: TASyncHTTPResult) of object', 'TASyncHTTPFinishEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(URL: String; Position, Size: Int64) of object', 'TASyncHTTPProgressEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('procedure ASync.HTTPGet(URL: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static; overload;', @_LapeASyncHTTP_Get1);
    addGlobalFunc('procedure ASync.HTTPGet(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static; overload;', @_LapeASyncHTTP_Get2);
    addGlobalFunc('procedure ASync.HTTPGetFile(URL: String; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static; overload;', @_LapeASyncHTTP_GetFile1);
    addGlobalFunc('procedure ASync.HTTPGetFile(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static; overload;', @_LapeASyncHTTP_GetFile2);
    addGlobalFunc('procedure ASync.HTTPPost(URL, Data: String; OnFinish: TASyncHTTPFinishEvent = nil); static; overload;', @_LapeASyncHTTP_Post1);
    addGlobalFunc('procedure ASync.HTTPPost(URL: String; RequestHeaders: TStringArray; Data: String; OnFinish: TASyncHTTPFinishEvent = nil); static; overload;', @_LapeASyncHTTP_Post2);

    addGlobalType([
      'record',
      '  ZipFile: String;',
      '  DestPath: String;',
      '  Success: Boolean;',
      '  Exception: String;',
      '  TimeUsed: Double;',
      'end;'],
      'TASyncUnzipResult');

    addGlobalType('procedure(constref Result: TASyncUnzipResult) of object', 'TASyncUnzipFinishEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Position, Total: Int64) of object', 'TASyncUnzipProgressEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('procedure ASync.FileUnZip(ZipFile, DestPath: String; OnFinish: TASyncUnzipFinishEvent; OnProgress: TASyncUnzipProgressEvent = nil); static;', @_LapeASyncUnZip_Unzip);

    addGlobalFunc(
      'function ASync.Schedules: TStringArray; static;', [
      'begin',
      '  Result := _ScheduleNames();',
      'end;'
    ]);
    addGlobalFunc(
      'procedure ASync.ScheduleEvery(Name: String; Method: procedure of object; Interval: Integer); static; overload;', [
      'begin',
      '  _ScheduleEvery(Name, @Method, Interval);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure ASync.ScheduleEvery(Name: String; Method: procedure(Params: TPointerArray) of object; Params: TPointerArray; Interval: Integer); static; overload;', [
      'begin',
      '  _ScheduleEveryEx(Name, @Method, Params, Interval);',
      'end;'
    ]);
    addGlobalFunc(
      'procedure ASync.ScheduleStop(Name: String); static;', [
      'begin',
      '  _ScheduleStop(Name);',
      'end;'
    ]);

    DumpSection := '';
  end;
end;

end.
