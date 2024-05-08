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
*)

(*
ASyncHTTP.Get
-------------
> procedure ASyncHTTP.Get(URL: String; OnFinished: TASyncHTTPFinishedEvent; OnProgress: TASyncHTTPProgressEvent); static;
*)
procedure _LapeASyncHTTP_Get1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, TASyncHTTPFinishedEvent(Params^[1]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

(*
ASyncHTTP.Get
-------------
> procedure ASyncHTTP.Get(URL: String; DestFile: String; OnFinished: TASyncHTTPFinishedEvent; OnProgress: TASyncHTTPProgressEvent); static;
*)
procedure _LapeASyncHTTP_Get2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, PString(Params^[1])^, TASyncHTTPFinishedEvent(Params^[2]^), TASyncHTTPProgressEvent(Params^[3]^));
end;

(*
ASyncHTTP.Post
-------------
> procedure ASyncHTTP.Post(URL, PostData: String; OnFinished: TASyncHTTPFinishedEvent); static;
*)
procedure _LapeASyncHTTP_Post1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, PString(Params^[1])^, TASyncHTTPFinishedEvent(Params^[2]^));
end;

(*
ASyncHTTP.Post
-------------
> procedure ASyncHTTP.Post(URL, PostData: String; Headers: TStringArray; OnFinished: TASyncHTTPFinishedEvent); static;
*)
procedure _LapeASyncHTTP_Post2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, TASyncHTTPFinishedEvent(Params^[3]^));
end;

(*
ASyncMouse.Move
---------------
> procedure ASyncMouse.Move(Target: TTarget; Dest: TPoint; Accuracy: Double = 1);
*)
procedure _LapeASyncMouse_Move(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.Move(PSimbaTarget(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^);
end;

(*
ASyncMouse.ChangeDest
---------------------
> procedure TASyncMouse.ChangeDest(Dest: TPoint);
*)
procedure _LapeASyncMouse_ChangeDest(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.ChangeDest(PPoint(Params^[0])^);
end;

(*
ASyncMouse.IsMoving
-------------------
> function TASyncMouse.IsMoving: Boolean;
*)
procedure _LapeASyncMouse_IsMoving(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := ASyncMouse.IsMoving();
end;

(*
ASyncMouse.WaitMoving
---------------------
> procedure TASyncMouse.WaitMoving;
*)
procedure _LapeASyncMouse_WaitMoving(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.WaitMoving();
end;

(*
ASyncMouse.Stop
---------------
> procedure TASyncMouse.Stop;
*)
procedure _LapeASyncMouse_Stop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.Stop();
end;

(*
ASyncUnZip.Unzip
----------------
> procedure ASyncUnZip.Unzip(ZipFile, DestPath: String; OnFinished: TASyncUnzipFinishedEvent; OnProgress: TASyncUnzipProgressEvent = nil); static;
*)
procedure _LapeASyncUnZip_Unzip(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncUnzip.Unzip(PString(Params^[0])^, PString(Params^[1])^, TASyncUnzipFinishedEvent(Params^[2]^), TASyncUnzipProgressEvent(Params^[3]^));
end;

procedure ImportASync(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    // Empty "namespaces"
    addGlobalType('record end;', 'ASyncMouse');
    addGlobalType('record end;', 'ASyncHTTPClient');
    addGlobalType('record end;', 'ASyncUnZip');

    ImportingSection := 'ASync';
    addGlobalFunc('procedure ASyncMouse.Move(constref Target: TTarget; Dest: TPoint; Accuracy: Double = 1); static; overload;', @_LapeASyncMouse_Move);
    addGlobalFunc('procedure ASyncMouse.ChangeDest(Dest: TPoint); static;', @_LapeASyncMouse_ChangeDest);
    addGlobalFunc('function ASyncMouse.IsMoving: Boolean; static;', @_LapeASyncMouse_IsMoving);
    addGlobalFunc('procedure ASyncMouse.WaitMoving; static;', @_LapeASyncMouse_WaitMoving);
    addGlobalFunc('procedure ASyncMouse.Stop; static;', @_LapeASyncMouse_Stop);

    addGlobalFunc(
      'procedure ASyncMouse.Move(Dest: TPoint; Accuracy: Double = 1); static; overload;', [
      'begin',
      '  ASyncMouse.Move(System.Target, Dest, Accuracy);',
      'end;'
    ]);

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

    addGlobalType('procedure(constref Result: TASyncHTTPResult) of object', 'TASyncHTTPFinishedEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(URL, ContentType: String; Position, Size: Int64) of object', 'TASyncHTTPProgressEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('procedure ASyncHTTPClient.Get(URL: String; OnFetched: TASyncHTTPFinishedEvent; OnProgress: TASyncHTTPProgressEvent = nil); static; overload;', @_LapeASyncHTTP_Get1);
    addGlobalFunc('procedure ASyncHTTPClient.Get(URL, DestFile: String; OnFetched: TASyncHTTPFinishedEvent; OnProgress: TASyncHTTPProgressEvent = nil); static; overload;', @_LapeASyncHTTP_Get2);
    addGlobalFunc('procedure ASyncHTTPClient.Post(URL, PostData: String); static; overload;', @_LapeASyncHTTP_Post1);
    addGlobalFunc('procedure ASyncHTTPClient.Post(URL, PostData: String; Headers: TStringArray); static; overload;', @_LapeASyncHTTP_Post2);

    addGlobalType([
      'record',
      '  ZipFile: String;',
      '  DestPath: String;',
      '  Success: Boolean;',
      '  Exception: String;',
      '  TimeUsed: Double;',
      'end;'],
      'TASyncUnzipResult');

    addGlobalType('procedure(constref Result: TASyncUnzipResult) of object', 'TASyncUnzipFinishedEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Position, Total: Int64) of object', 'TASyncUnzipProgressEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('procedure ASyncUnZip.Unzip(ZipFile, DestPath: String; OnFinished: TASyncUnzipFinishedEvent; OnProgress: TASyncUnzipProgressEvent = nil); static;', @_LapeASyncUnZip_Unzip);

    ImportingSection := '';
  end;
end;

end.
