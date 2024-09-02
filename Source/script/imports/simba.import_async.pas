unit simba.import_async;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportASync(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi,
  simba.target, simba.http_async, simba.input_async, simba.fs_async;

(*
ASync
=====
Things that run in the background.

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
```
*)
procedure _LapeASyncHTTP_Get1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, [], TASyncHTTPFinishEvent(Params^[1]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

(*
ASync.HTTPGet
-------------
```
procedure ASync.HTTPGet(URL: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;
```
*)
procedure _LapeASyncHTTP_Get2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, PStringArray(Params^[1])^, TASyncHTTPFinishEvent(Params^[2]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

(*
ASync.HTTPGetFile
-----------------
```
procedure ASync.HTTPGetFile(URL: String; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;
```
*)
procedure _LapeASyncHTTP_GetFile1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.GetFile(PString(Params^[0])^, [], PString(Params^[1])^, TASyncHTTPFinishEvent(Params^[2]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

(*
ASync.HTTPGetFile
-----------------
```
procedure ASync.HTTPGetFile(URL: String; RequestHeaders: TStringArray; DestFile: String; OnFinish: TASyncHTTPFinishEvent; OnProgress: TASyncHTTPProgressEvent = nil); static;
```
*)
procedure _LapeASyncHTTP_GetFile2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.GetFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^,TASyncHTTPFinishEvent(Params^[3]^), TASyncHTTPProgressEvent(Params^[4]^));
end;


(*
ASync.HTTPPost
--------------
```
procedure ASync.HTTPPost(URL, Data: String; OnFinish: TASyncHTTPFinishEvent); static;
```
*)
procedure _LapeASyncHTTP_Post1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, [], PString(Params^[1])^, TASyncHTTPFinishEvent(Params^[2]^));
end;

(*
ASync.HTTPPost
--------------
```
procedure ASync.HTTPPost(URL: String; RequestHeaders: TStringArray; Data: String; RequestHeaders: TStringArray; OnFinish: TASyncHTTPFinishEvent); static;
```
*)
procedure _LapeASyncHTTP_Post2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^, TASyncHTTPFinishEvent(Params^[3]^));
end;

(*
ASync.MouseMove
---------------
```
procedure ASync.MouseMove(Target: TTarget; Dest: TPoint; Accuracy: Double = 1);
```
*)
procedure _LapeASyncMouse_Move(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.Move(PSimbaTarget(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^);
end;

(*
ASync.MouseChangeDest
---------------------
```
procedure ASync.MouseChangeDest(Dest: TPoint);
```
*)
procedure _LapeASyncMouse_MouseChangeDest(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.ChangeDest(PPoint(Params^[0])^);
end;

(*
ASync.MouseMoving
-----------------
```
function ASync.MouseMoving: Boolean;
```
*)
procedure _LapeASyncMouse_IsMoving(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ASyncMouse.IsMoving();
end;

(*
ASync.MouseWaitMoving
---------------------
```
procedure ASync.MouseWaitMoving;
```
*)
procedure _LapeASyncMouse_WaitMoving(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.WaitMoving();
end;

(*
ASync.MouseStop
---------------
```
procedure ASync.MouseStop;
```
*)
procedure _LapeASyncMouse_Stop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.Stop();
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

procedure ImportASync(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'ASync';

    // namespace
    addGlobalType('record end;', 'ASync');

    addGlobalFunc('procedure ASync.MouseMove(constref Target: TTarget; Dest: TPoint; Accuracy: Double = 1); static; overload;', @_LapeASyncMouse_Move);
    addGlobalFunc('procedure ASync.MouseMove(Dest: TPoint; Accuracy: Double = 1); static; overload;', [
                  'begin',
                  '  ASync.MouseMove(System.Target, Dest, Accuracy);',
                  'end;'
                 ]);
    addGlobalFunc('procedure ASync.MouseChangeDest(Dest: TPoint); static;', @_LapeASyncMouse_MouseChangeDest);
    addGlobalFunc('function ASync.MouseMoving: Boolean; static;', @_LapeASyncMouse_IsMoving);
    addGlobalFunc('procedure ASync.MouseWaitMoving; static;', @_LapeASyncMouse_WaitMoving);
    addGlobalFunc('procedure ASync.MouseStop; static;', @_LapeASyncMouse_Stop);

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

    ImportingSection := '';
  end;
end;

end.