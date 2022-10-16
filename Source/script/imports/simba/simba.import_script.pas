unit simba.import_script;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.process, simba.scriptthread;

(*
Script
======
Script related methods
*)

(*
AddOnTerminate
~~~~~~~~~~~~~~
procedure AddOnTerminate(Proc: procedure);
procedure AddOnTerminate(Proc: procedure of object);

Adds a procedure to be called when the script is terminated.
*)

(*
AddOnUserTerminate
~~~~~~~~~~~~~~~~~~
procedure AddOnUserTerminate(Proc: procedure);
procedure AddOnUserTerminate(Proc: procedure of object);

Adds a procedure to be called when the script is terminated with the stop button being clicked.
*)

(*
AddOnPause
~~~~~~~~~~
procedure AddOnPause(Proc: procedure);
procedure AddOnPause(Proc: procedure of object);

Adds a procedure to be called when the script is paused.
*)

(*
AddOnResume
~~~~~~~~~~~
procedure AddOnResume(Proc: procedure);
procedure AddOnResume(Proc: procedure of object);

Adds a procedure to be called when the script is resumed from pause.
*)

(*
AddOnResume
~~~~~~~~~~~
procedure AddOnResume(Proc: procedure);
procedure AddOnResume(Proc: procedure of object);

Adds a procedure to be called when the script is resumed from pause.
*)

(*
TerminateScript
~~~~~~~~~~~~~~~
procedure TerminateScript;
procedure TerminateScript(Reason: String);

Instantly terminates the script!
*)

(*
PauseScript
~~~~~~~~~~~
procedure PauseScript;

Programmatically pauses the script. The only way for the script to resumed is by the user clicking the play button.
*)
procedure _LapePauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.State := ESimbaScriptState.STATE_PAUSED;
end;

(*
GetScriptPID
~~~~~~~~~~~~
function GetScriptPID: TProcessID;

Returns the process ID of the running script.
*)
procedure _LapeGetScriptPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := GetProcessID();
end;

(*
GetScriptParameters
~~~~~~~~~~~~~~~~~~~
function GetScriptParameters: TStringArray;

Returns all command line parameters passed to the script.
*)
procedure _LapeGetScriptParameters(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := SimbaProcess.GetScriptParameters();
end;

(*
GetScriptParameter
~~~~~~~~~~~~~~~~~~
function GetScriptParameter(Name: String): String;

Returns a command line parameter value passed to the script.
Parameters should be passed as a key-pair value: `Name=Value`
*)
procedure _LapeGetScriptParameter(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaProcess.GetScriptParameter(PString(Params^[0])^);
end;

(*
RunScript
~~~~~~~~~
function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus;

Runs a simba script and **will wait** until the script has finished.

 - The script output will be returned in the `Output` parameter.
 - Returns the exit status of the scripts process.
*)
procedure _LapeRunScript(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessExitStatus(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

(*
RunScript
~~~~~~~~~
function RunScript(Script: String; Parameters: TStringArray): TProcessID;

Runs a simba script and instantly returns the scripts PID.

- The script output will be printed normally.
- The script PID can be used with process methods.

Example::

  PID := RunScript('script.simba', []);
  while IsProcessRunning(PID) do
    Sleep(100);
  WriteLn('Script finished!');
*)
procedure _LapeRunScriptEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScript(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

(*
RunScriptOutputToFile
~~~~~~~~~~~~~~~~~~~~~
function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID;

- The script output will be redirected to the file `OutputFileName`
- The script PID can be used with process methods.

Example::

  PID := RunScriptOutputToFile('script.simba', [], 'output.txt');
  while IsProcessRunning(PID) do
    Sleep(100);
  WriteLn('Script finished!');
*)
procedure _LapeRunScriptOutputToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PProcessID(Result)^ := SimbaProcess.RunScriptOutputToFile(PString(Params^[0])^, PStringArray(Params^[1])^, PString(Params^[2])^);
end;

procedure ImportScript(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Script';

    addGlobalVar('', 'ScriptFile').isConstant := True;
    addGlobalVar('', 'ScriptName').isConstant := True;

    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunScript);
    addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray): TProcessID; overload', @_LapeRunScriptEx);
    addGlobalFunc('function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID', @_LapeRunScriptOutputToFile);

    addGlobalFunc('function GetScriptPID: TProcessID', @_LapeGetScriptPID);
    addGlobalFunc('function GetScriptParameters: TStringArray', @_LapeGetScriptParameters);
    addGlobalFunc('function GetScriptParameter(Name: String): String', @_LapeGetScriptParameter);

    addGlobalFunc('procedure PauseScript', @_LapePauseScript);

    addGlobalFunc(
      'procedure TerminateScript; overload;', [
      'begin',
      '  Halt();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure TerminateScript(Reason: String); overload;', [
      'begin',
      '  WriteLn("Script Terminated: " + Reason);',
      '  Halt();',
      'end;'
    ]);

    ImportingSection := '';

    addDelayedCode([
      'var',
      '  _TerminateEvents, _UserTerminateEvents, _PauseEvents, _ResumeEvents: array of record',
      '    Proc: procedure;',
      '    Method: procedure of object;',
      '  end;',
      '',
      'procedure _CallOnTerminate;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_TerminateEvents) do',
      '  begin',
      '    if Assigned(_TerminateEvents[I].Proc) then _TerminateEvents[I].Proc();',
      '    if Assigned(_TerminateEvents[I].Method) then _TerminateEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnUserTerminate;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_UserTerminateEvents) do',
      '  begin',
      '    if Assigned(_UserTerminateEvents[I].Proc) then _UserTerminateEvents[I].Proc();',
      '    if Assigned(_UserTerminateEvents[I].Method) then _UserTerminateEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnPause;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_PauseEvents) do',
      '  begin',
      '    if Assigned(_PauseEvents[I].Proc) then _PauseEvents[I].Proc();',
      '    if Assigned(_PauseEvents[I].Method) then _PauseEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnResume;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_ResumeEvents) do',
      '  begin',
      '    if Assigned(_ResumeEvents[I].Proc) then _ResumeEvents[I].Proc();',
      '    if Assigned(_ResumeEvents[I].Method) then _ResumeEvents[I].Method();',
      '  end;',
      'end;'
    ], '!Events');

    addDelayedCode([
      'procedure AddOnTerminate(Proc: procedure); overload;',
      'begin',
      '  _TerminateEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnTerminate(Proc: procedure of object); overload;',
      'begin',
      '  _TerminateEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnUserTerminate(Proc: procedure); overload;',
      'begin',
      '  _UserTerminateEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnUserTerminate(Proc: procedure of object); overload;',
      'begin',
      '  _UserTerminateEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnPause(Proc: procedure); overload;',
      'begin',
      '  _PauseEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnPause(Proc: procedure of object); overload;',
      'begin',
      '  _PauseEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnResume(Proc: procedure); overload;',
      'begin',
      '  _ResumeEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnResume(Proc: procedure of object); overload;',
      'begin',
      '  _ResumeEvents += [,Proc];',
      'end;'
    ]);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportScript);

end.

