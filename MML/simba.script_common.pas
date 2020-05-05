unit simba.script_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  ESimbaMethod = (
    SIMBA_METHOD_DEBUG_IMAGE,
    SIMBA_METHOD_SCRIPT_ERROR,
    SIMBA_METHOD_CLEAR_DEBUG,
    SIMBA_METHOD_DEBUG_IMAGE_DISPLAY,
    SIMBA_METHOD_DEBUG_IMAGE_GET,
    SIMBA_METHOD_DEBUG_IMAGE_CLEAR,
    SIMBA_METHOD_GET_PID,
    SIMBA_METHOD_DEBUG_IMAGE_DRAW,
    SIMBA_METHOD_BALLOON_HINT,
    SIMBA_METHOD_DISGUISE,
    SIMBA_METHOD_STATUS,
    SIMBA_METHOD_GET_TARGET_PID,
    SIMBA_METHOD_GET_TARGET_WINDOW,
    SIMBA_METHOD_SCRIPT_STATE_CHANGED,
    SIMBA_METHOD_DEBUGGER_METHOD,
    SIMBA_METHOD_DEBUGGER_EVENT
  );

  PSimbaScript_DebuggerEvent = ^TSimbaScript_DebuggerEvent;
  TSimbaScript_DebuggerEvent = packed record
    Method: Int16;
    Indent: Int16;
  end;

  TSimbaScript_DebuggerEventArray = array of TSimbaScript_DebuggerEvent;

  TSimbaMethod_ScriptError = packed record
    Message: array[1..2048] of Char;
    FileName: ShortString;
    Line: Int32;
    Column: Int32;
  end;

implementation

end.

