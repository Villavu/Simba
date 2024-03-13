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
  simba.input, simba.http_async, simba.input_async;

procedure _LapeASyncHTTP_Get1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, TASyncHTTPFinishedEvent(Params^[1]^));
end;

procedure _LapeASyncHTTP_Get2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Get(PString(Params^[0])^, PString(Params^[1])^, TASyncHTTPFinishedEvent(Params^[2]^));
end;

procedure _LapeASyncHTTP_Post1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, PString(Params^[1])^, TASyncHTTPFinishedEvent(Params^[2]^));
end;

procedure _LapeASyncHTTP_Post2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncHTTP.Post(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^, TASyncHTTPFinishedEvent(Params^[3]^));
end;

(*
ASyncMouse.Move
---------------
> procedure ASyncMouse.Move(Input: TSimbaInput; Dest: TPoint; Accuracy: Double = 1);
*)
procedure _LapeASyncMouse_Move(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  ASyncMouse.Move(PSimbaInput(Params^[0])^, PPoint(Params^[1])^, PDouble(Params^[2])^);
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

procedure ImportASync(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    // Empty "namespaces"
    addGlobalType('record end;', 'ASyncMouse');
    addGlobalType('record end;', 'ASyncHTTPClient');

    ImportingSection := 'ASync';
    addGlobalFunc('procedure ASyncMouse.Move(Input: TInput; Dest: TPoint; Accuracy: Double = 1); static;', @_LapeASyncMouse_Move);
    addGlobalFunc('procedure ASyncMouse.ChangeDest(Dest: TPoint); static;', @_LapeASyncMouse_ChangeDest);
    addGlobalFunc('function ASyncMouse.IsMoving: Boolean; static;', @_LapeASyncMouse_IsMoving);
    addGlobalFunc('procedure ASyncMouse.WaitMoving; static;', @_LapeASyncMouse_WaitMoving);
    addGlobalFunc('procedure ASyncMouse.Stop; static;', @_LapeASyncMouse_Stop);

    addGlobalFunc(
      'procedure ASyncMouse.Move(Input: TInput; Dest: TPoint; Accuracy: Double = 1); static; override;', [
      'begin',
      '  if Input.Target.IsDefault() then',
      '  try',
      '    Input.Target := System.Target;',
      '    {$IFDECL Result}Result:={$ENDIF}inherited();',
      '  finally',
      '    Input.Target := [];',
      '  end else',
      '    {$IFDECL Result}Result:={$ENDIF}inherited();',
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
    addGlobalFunc('procedure ASyncHTTPClient.Get(URL: String; OnFetched: TASyncHTTPFinishedEvent); static; overload;', @_LapeASyncHTTP_Get1);
    addGlobalFunc('procedure ASyncHTTPClient.Get(URL, DestFile: String; OnFetched: TASyncHTTPFinishedEvent); static; overload;', @_LapeASyncHTTP_Get2);
    addGlobalFunc('procedure ASyncHTTPClient.Post(URL, PostData: String); static; overload;', @_LapeASyncHTTP_Post1);
    addGlobalFunc('procedure ASyncHTTPClient.Post(URL, PostData: String; Headers: TStringArray); static; overload;', @_LapeASyncHTTP_Post2);

    ImportingSection := '';
  end;
end;

end.
