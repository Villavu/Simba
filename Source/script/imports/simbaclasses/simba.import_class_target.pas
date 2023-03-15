unit simba.import_class_target;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.target;

procedure _LapeTarget_GetTargetDimensions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.GetTargetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_GetTargetPosition(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.GetTargetPosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_GetColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PTarget(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_CopyData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PTarget(Params^[0])^.CopyData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_ReturnData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRetData(Result)^ := PTarget(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_FreeReturnData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //PTarget(Params^[0])^.FreeReturnData();
end;

procedure _LapeTarget_ActivateClient(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.ActivateClient();
end;

procedure _LapeTarget_TargetValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.TargetValid();
end;

procedure _LapeTarget_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.MouseSetClientArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_MouseResetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.MouseResetClientArea();
end;

procedure _LapeTarget_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.ImageSetClientArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeTarget_ImageResetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.ImageResetClientArea();
end;

procedure _LapeTarget_GetMousePosition(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.GetMousePosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_MoveMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.MoveMouse(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeTarget_ScrollMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.ScrollMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeTarget_HoldMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.HoldMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeTarget_ReleaseMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.ReleaseMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeTarget_IsMouseButtonHeld(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.IsMouseButtonHeld(PClickType(Params^[1])^);
end;

procedure _LapeTarget_SendString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.SendString(PString(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeTarget_HoldKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.HoldKey(PInteger(Params^[1])^);
end;

procedure _LapeTarget_ReleaseKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.ReleaseKey(PInteger(Params^[1])^);
end;

procedure _LapeTarget_IsKeyHeld(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.IsKeyHeld(PInteger(Params^[1])^);
end;

procedure _LapeTarget_GetKeyCode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PTarget(Params^[0])^.GetKeyCode(PChar(Params^[1])^);
end;

procedure _LapeTarget_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^ := TTarget.Create();
end;

procedure _LapeTarget_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Params^[0])^.Free();
end;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TTarget');
    addGlobalType('record Ptr: PRGB32; IncPtrWith: integer; RowLen: integer; end', 'TRetData');
    addGlobalType('UInt32', 'TClickType');
    addGlobalFunc('procedure TTarget.GetTargetDimensions(out w, h: integer);', @_LapeTarget_GetTargetDimensions);
    addGlobalFunc('procedure TTarget.GetTargetPosition(out left, top: integer);', @_LapeTarget_GetTargetPosition);
    addGlobalFunc('function TTarget.GetColor(x,y : integer): TColor;', @_LapeTarget_GetColor);
    addGlobalFunc('function TTarget.CopyData(X, Y, Width, Height: Integer): PRGB32;', @_LapeTarget_CopyData);
    addGlobalFunc('function TTarget.ReturnData(xs, ys, width, height: Integer): TRetData;', @_LapeTarget_ReturnData);
    addGlobalFunc('procedure TTarget.FreeReturnData;', @_LapeTarget_FreeReturnData);
    addGlobalFunc('procedure TTarget.ActivateClient;', @_LapeTarget_ActivateClient);
    addGlobalFunc('function TTarget.TargetValid: boolean;', @_LapeTarget_TargetValid);
    addGlobalFunc('function TTarget.MouseSetClientArea(x1, y1, x2, y2: integer): boolean;', @_LapeTarget_MouseSetClientArea);
    addGlobalFunc('procedure TTarget.MouseResetClientArea;', @_LapeTarget_MouseResetClientArea);
    addGlobalFunc('function TTarget.ImageSetClientArea(x1, y1, x2, y2: integer): boolean;', @_LapeTarget_ImageSetClientArea);
    addGlobalFunc('procedure TTarget.ImageResetClientArea;', @_LapeTarget_ImageResetClientArea);
    addGlobalFunc('procedure TTarget.GetMousePosition(out x,y: integer);', @_LapeTarget_GetMousePosition);
    addGlobalFunc('procedure TTarget.MoveMouse(x,y: integer);', @_LapeTarget_MoveMouse);
    addGlobalFunc('procedure TTarget.ScrollMouse(x,y : integer; Lines : integer);', @_LapeTarget_ScrollMouse);
    addGlobalFunc('procedure TTarget.HoldMouse(x,y: integer; button: TClickType);', @_LapeTarget_HoldMouse);
    addGlobalFunc('procedure TTarget.ReleaseMouse(x,y: integer; button: TClickType);', @_LapeTarget_ReleaseMouse);
    addGlobalFunc('function TTarget.IsMouseButtonHeld(button : TClickType): boolean;', @_LapeTarget_IsMouseButtonHeld);
    addGlobalFunc('procedure TTarget.SendString(str: string; keywait, keymodwait: integer);', @_LapeTarget_SendString);
    addGlobalFunc('procedure TTarget.HoldKey(key: integer);', @_LapeTarget_HoldKey);
    addGlobalFunc('procedure TTarget.ReleaseKey(key: integer);', @_LapeTarget_ReleaseKey);
    addGlobalFunc('function TTarget.IsKeyHeld(key: integer): boolean;', @_LapeTarget_IsKeyHeld);
    addGlobalFunc('function TTarget.GetKeyCode(C : char): integer;', @_LapeTarget_GetKeyCode);
    addGlobalFunc('procedure TTarget.Init()', @_LapeTarget_Init);
    // addGlobalFunc('procedure TTarget.Free;', @_LapeTarget_Free);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportTarget);

end.

