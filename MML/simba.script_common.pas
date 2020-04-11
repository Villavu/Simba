unit simba.script_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  ESimbaMethod = (SIMBA_METHOD_DEBUG_IMAGE,
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
                  SIMBA_METHOD_GET_TARGET_WINDOW);

  TSimbaMethod_ScriptError = packed record
    Message: array[1..2048] of Char;
    FileName: ShortString;
    Line: Int32;
    Column: Int32;
  end;

type
  ESimbaScriptState = (SCRIPT_STOPPING, SCRIPT_PAUSED, SCRIPT_RUNNING);

implementation

end.

