unit simba.import_mouse;

{$i simba.inc}

interface

uses
  classes, sysutils,
  lptypes, ffi,
  simba.script_compiler;

implementation

uses
  simba.mufasatypes, simba.scriptthread;

procedure _LapeMoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.MoveMouse(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ScrollMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeGetMousePos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.GetMousePos(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeHoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.HoldMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PClickType(Params^[2])^);
end;

procedure _LapeReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ReleaseMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PClickType(Params^[2])^);
end;

procedure _LapeClickMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ClickMouse(PInteger(Params^[0])^, PInteger(Params^[1])^, PClickType(Params^[2])^);
end;

procedure _LapeIsMouseButtonDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.IsMouseButtonDown(PClickType(Params^[0])^);
end;

procedure ImportMouse(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Mouse');

    addGlobalFunc('procedure MoveMouse(X, Y: Integer)', @_LapeMoveMouse);
    addGlobalFunc('procedure ScrollMouse(X, Y: Integer; Clicks: Integer)', @_LapeScrollMouse);
    addGlobalFunc('procedure GetMousePos(var X, Y: Integer)', @_LapeGetMousePos);
    addGlobalFunc('procedure HoldMouse(X, Y: Integer; ClickType: Integer)', @_LapeHoldMouse);
    addGlobalFunc('procedure ReleaseMouse(X, Y: Integer; ClickType: Integer)', @_LapeReleaseMouse);
    addGlobalFunc('procedure ClickMouse(X, Y: Integer; ClickType: Integer)', @_LapeClickMouse);
    addGlobalFunc('function IsMouseButtonDown(Button: Integer): boolean', @_LapeIsMouseButtonDown);

    addGlobalVar(Integer(MOUSE_RIGHT),   'MOUSE_RIGHT').isConstant := True;
    addGlobalVar(Integer(MOUSE_LEFT),    'MOUSE_LEFT').isConstant := True;
    addGlobalVar(Integer(MOUSE_MIDDLE),  'MOUSE_MIDDLE').isConstant := True;
    addGlobalVar(Integer(MOUSE_EXTRA_1), 'MOUSE_EXTRA_1').isConstant := True;
    addGlobalVar(Integer(MOUSE_EXTRA_2), 'MOUSE_EXTRA_2').isConstant := True;

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportMouse);

end.

