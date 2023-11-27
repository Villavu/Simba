unit simba.import_misc;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportMisc(Compiler: TSimbaScript_Compiler);

implementation

uses
  clipbrd, lptypes,
  simba.nativeinterface,
  simba.settings, simba.compress, simba.encoding;

(*
Misc
====
Miscellaneous methods that dont go in any other sections.
*)

(*
ClearSimbaOutput
~~~~~~~~~~~~~~~~
> procedure ClearSimbaOutput;

Clear the scripts output box in Simba.
*)
procedure ClearSimbaOutput(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaDebugLn([EDebugLn.CLEAR], '');
end;

(*
SetSimbaSetting
~~~~~~~~~~~~~~~
> function SetSimbaSetting(Name: String; DefValue: String = ''): String;
*)
procedure _LapeGetSimpleSetting(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SimbaSettings.GetSimpleSetting(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
GetSimbaSetting
~~~~~~~~~~~~~~~
> procedure GetSimbaSetting(Name, Value: String);
*)
procedure _LapeSetSimpleSetting(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaSettings.SetSimpleSetting(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
PlaySound
~~~~~~~~~
> procedure PlaySound(Sound: String);
*)
procedure _LapePlaySound(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PlaySound(PString(Params^[0])^);
end;

(*
StopSound
~~~~~~~~~
> procedure StopSound;
*)
procedure _LapeStopSound(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.StopSound();
end;

(*
Simba
~~~~~
> procedure Simba;
*)
procedure _LapeSimba(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  WriteLn(DecompressString(Base64Decode(
    '9AoAAHicldU7b+0gDADgvdL9D+AisSDOTrpUvWvGuzNVOkMn9v72a942j6T1dJrEX7AxqRAXYaS9up3iz8suVxoMKe+' +
    'NC6LGnbEhiCCfTzPfJ5cmgidj5J9MsezSQAyApGHGR17N9SpGoBj1tkuRkJHoAk3WeMfTC66GWbaTFtMAwZDPRjh73U4uCKGnRTh3NMK0mAjiXxA975iERASl' +
    'QjfcRLBVS963TKCQDb0m8Brwwv1IKAWkErcipPNAC5+JdPmY62hE/O3L8yE+T4k4PpGwi2aiEIn25zcqKMQ1a6bgNtGN4kJqJ1tYeqFwrMNDcCFvKjMsWXLOK' +
    'N19toPbBN2PmacG9BogFoW7CQD00JTHdZlLml1yQZiv8zzBxGlQzxoxlx+Gdjo8JQDMV8w/0UmCctC/PGZDIKKPFMIGOM8M5IlUyuMel05IwY3hiHoMTLJYdg' +
    'RKvhJxsGt5wzKI8PApjpQTQmj5CkIRIO6S3REPXZjD1kyNGxABm60IxLkdu8HqQOaRmt0TcTVVFHzCdq2oX6ae2CMRuo/bWuhdHfMhfSI8PTE3xIjAuIRu7An' +
    'hv0kN+e38+1GMPYH/hq1PcyKsywdWvI1n9Y4YXzsLydgSphI4G7i/AexYRTW2RJmBPqFqTcgtUW7T6dgQlwIDfrsIsyDCphcbot5eDPgviZ8Yt0S4Ne4Iuoy/H' +
    '+//1sR/NLyhCQ==')));
end;

(*
SetClipBoard
~~~~~~~~~~~~
> procedure SetClipBoard(Data: string);

Sets the systems clipboard string.
*)
procedure _LapeSetClipBoard(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  try
    Clipboard.AsText := PString(Params^[0])^;
  except
  end;
end;

(*
GetClipBoard
~~~~~~~~~~~~
> function GetClipBoard: String;

Returns the systems clipboard string.
*)
procedure _LapeGetClipBoard(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  try
    PString(Result)^ := Clipboard.AsText;
  except
  end;
end;

(*
SetSimbaTitle
~~~~~~~~~~~~~~
> procedure SetSimbaTitle(S: String);
*)

(*
GetSimbaPID
~~~~~~~~~~~
> function GetSimbaPID: TProcessID;

Returns the Simba's PID this script is running in.
*)

(*
GetSimbaTargetPID
~~~~~~~~~~~~~~~~~~
> function GetSimbaTargetPID: TProcessID;

Returns the current Simba target PID (what is selected with the crosshair)
*)

(*
GetSimbaTargetWindow
~~~~~~~~~~~~~~~~~~~~
> function GetSimbaTargetWindow: TWindowHandle;

Returns the current Simba target window (what is selected with the crosshair)
*)

procedure ImportMisc(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Misc';

    addGlobalFunc(
      'procedure SetSimbaTitle(S: String);', [
      'begin',
      '  _SimbaScript.SetSimbaTitle(S);',
      'end;'
    ]);

    addGlobalFunc(
      'function GetSimbaPID: TProcessID;', [
      'begin',
      '  Result := _SimbaScript.GetSimbaPID();',
      'end;'
    ]);
    addGlobalFunc(
      'function GetSimbaTargetPID: TProcessID;', [
      'begin',
      '  Result := _SimbaScript.GetSimbaTargetPID();',
      'end;'
    ]);
    addGlobalFunc(
      'function GetSimbaTargetWindow: TWindowHandle;', [
      'begin',
      '  Result := _SimbaScript.GetSimbaTargetWindow();',
      'end;'
    ]);
    addGlobalFunc('procedure ClearSimbaOutput', @ClearSimbaOutput);
    addGlobalFunc('function SetSimbaSetting(Name: String; DefValue: String = ""): String', @_LapeGetSimpleSetting);
    addGlobalFunc('procedure GetSimbaSetting(Name, Value: String);', @_LapeSetSimpleSetting);
    addGlobalFunc('procedure PlaySound(Sound: String)', @_LapePlaySound);
    addGlobalFunc('procedure StopSound', @_LapeStopSound);
    addGlobalFunc('procedure Simba', @_LapeSimba);
    addGlobalFunc('procedure SetClipBoard(Data: string)', @_LapeSetClipBoard);
    addGlobalFunc('function GetClipBoard: String', @_LapeGetClipBoard);

    ImportingSection := '';
  end;
end;

end.

