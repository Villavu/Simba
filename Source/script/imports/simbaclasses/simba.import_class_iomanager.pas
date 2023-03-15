unit simba.import_class_iomanager;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, graphics, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.bitmap, simba.iomanager, simba.target, simba.target_exported;

type
  PNotifyEvent = ^TNotifyEvent;

procedure _LapeIOManager_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^ := TIOManager.Create();
end;

procedure _LapeIOManager_SetTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIOManager_SetTargetEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeIOManager_SetTargetExEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeIOManager_TargetValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.TargetValid();
end;

procedure _LapeIOManager_GetColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PIOManager(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_ReturnMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PIOManager(Params^[0])^.ReturnMatrix(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_ReturnData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRetData(Result)^ := PIOManager(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_CopyData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PIOManager(Params^[0])^.CopyData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_FreeReturnData(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  //PIOManager(Params^[0])^.FreeReturnData();
end;

procedure _LapeIOManager_GetWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.GetWidth();
end;

procedure _LapeIOManager_GetHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.GetHeight();
end;

procedure _LapeIOManager_GetDimensions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.GetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_GetPosition(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.GetPosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_ActivateClient(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.ActivateClient();
end;

procedure _LapeIOManager_IsFrozen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.IsFrozen();
end;

procedure _LapeIOManager_Freeze(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.Freeze();
end;

procedure _LapeIOManager_UnFreeze(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.UnFreeze();
end;

procedure _LapeIOManager_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.MouseSetClientArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_MouseResetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.MouseResetClientArea();
end;

procedure _LapeIOManager_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.ImageSetClientArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeIOManager_ImageResetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.ImageResetClientArea();
end;

procedure _LapeIOManager_GetMousePos(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.GetMousePos(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_MoveMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.MoveMouse(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIOManager_ScrollMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.ScrollMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIOManager_HoldMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.HoldMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeIOManager_ReleaseMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.ReleaseMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeIOManager_ClickMouse(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.ClickMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

procedure _LapeIOManager_IsMouseButtonDown(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.IsMouseButtonDown(PClickType(Params^[1])^);
end;

procedure _LapeIOManager_KeyUp(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.KeyUp(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_KeyDown(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.KeyDown(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_PressKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.PressKey(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_SendText(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.SendText(PString(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIOManager_SendTextEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.SendTextEx(PString(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIOManager_isKeyDown(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.isKeyDown(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_GetKeyCode(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.GetKeyCode(Pchar(Params^[1])^);
end;

procedure _LapeIOManager_GetImageTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.GetImageTarget();
end;

procedure _LapeIOManager_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.GetKeyMouseTarget();
end;

procedure _LapeIOManager_ExportImageTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget_Exported(Result)^ := PIOManager(Params^[0])^.ExportImageTarget();
end;

procedure _LapeIOManager_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget_Exported(Result)^ := PIOManager(Params^[0])^.ExportKeyMouseTarget();
end;

procedure _LapeIOManager_GetImageTargetEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.GetImageTarget(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_GetKeyMouseTargetEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.GetKeyMouseTarget(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_SetImageTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.SetImageTarget(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_SetKeyMouseTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.SetKeyMouseTarget(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_FreeTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.FreeTarget(PInteger(Params^[1])^);
end;

procedure _LapeIOManager_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.Free();
end;

procedure _LapeIOManager_SetTargetHandle(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PWindowHandle(Params^[1])^);
end;

procedure _LapeIOManager_AddHandlerInvalidTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PIOManager(Params^[0])^.AddHandlerInvalidTarget(PNotifyEvent(Params^[1])^);
end;

procedure _LapeIOManager_RemoveHandlerInvalidTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.RemoveHandlerInvalidTarget(PNotifyEvent(Params^[1])^);
end;

procedure _LapeIOManager_AutoActivate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Params^[0])^.AutoActivate := PBoolean(Params^[1])^;
end;

procedure _LapeIOManager_AutoActivate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.AutoActivate;
end;

procedure ImportIOManager(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TIOManager');

    addGlobalType('record func1, func2, func3, func4, func5, func6, func7, func8, func9, func10, func11, func12, func13, func14, func15, func16: Pointer; end', 'TTarget_Exported');
    addGlobalFunc('function TIOManager.SetTarget(WindowHandle: PtrUInt): Integer; overload', @_LapeIOManager_SetTargetHandle);
    addGlobalFunc('function TIOManager.SetTarget(Data: PRGB32; Width, Height: Integer): Integer; overload', @_LapeIOManager_SetTarget);
    addGlobalFunc('function TIOManager.SetTarget(bmp : TMufasaBitmap): Integer; overload', @_LapeIOManager_SetTargetEx);
    addGlobalFunc('function TIOManager.SetTarget(name, initargs: string): Integer; overload', @_LapeIOManager_SetTargetExEx);
    addGlobalFunc('function TIOManager.TargetValid: Boolean;', @_LapeIOManager_TargetValid);
    addGlobalFunc('function TIOManager.GetColor(x,y : Integer): TColor;', @_LapeIOManager_GetColor);
    addGlobalFunc('function TIOManager.CopyData(X, Y, Width, Height: Integer): PRGB32;', @_LapeIOManager_CopyData);
    addGlobalFunc('function TIOManager.ReturnMatrix(X, Y, Width, Height: Integer): TIntegerMatrix;', @_LapeIOManager_ReturnMatrix);
    addGlobalFunc('function TIOManager.ReturnData(X, Y, Width, Height: Integer): TRetData;', @_LapeIOManager_ReturnData);
    addGlobalFunc('procedure TIOManager.FreeReturnData;', @_LapeIOManager_FreeReturnData);
    addGlobalFunc('function TIOManager.GetWidth: Boolean;', @_LapeIOManager_GetWidth);
    addGlobalFunc('function TIOManager.GetHeight: Boolean;', @_LapeIOManager_GetHeight);
    addGlobalFunc('procedure TIOManager.GetDimensions(out W, H: Integer);', @_LapeIOManager_GetDimensions);
    addGlobalFunc('procedure TIOManager.GetPosition(var Left, Top: Integer);', @_LapeIOManager_GetPosition);
    addGlobalFunc('procedure TIOManager.ActivateClient;', @_LapeIOManager_ActivateClient);
    addGlobalFunc('function TIOManager.IsFrozen: Boolean;', @_LapeIOManager_IsFrozen);
    addGlobalFunc('procedure TIOManager.Freeze;', @_LapeIOManager_Freeze);
    addGlobalFunc('procedure TIOManager.UnFreeze;', @_LapeIOManager_UnFreeze);
    addGlobalFunc('function TIOManager.MouseSetClientArea(x1, y1, x2, y2: Integer): boolean;', @_LapeIOManager_MouseSetClientArea);
    addGlobalFunc('procedure TIOManager.MouseResetClientArea;', @_LapeIOManager_MouseResetClientArea);
    addGlobalFunc('function TIOManager.ImageSetClientArea(x1, y1, x2, y2: Integer): boolean;', @_LapeIOManager_ImageSetClientArea);
    addGlobalFunc('procedure TIOManager.ImageResetClientArea;', @_LapeIOManager_ImageResetClientArea);
    addGlobalFunc('procedure TIOManager.GetMousePos(var X, Y: Integer);', @_LapeIOManager_GetMousePos);
    addGlobalFunc('procedure TIOManager.MoveMouse(X, Y: Integer);', @_LapeIOManager_MoveMouse);
    addGlobalFunc('procedure TIOManager.ScrollMouse(X, Y: Integer; Lines : Integer);', @_LapeIOManager_ScrollMouse);
    addGlobalFunc('procedure TIOManager.HoldMouse(X, Y: Integer; button: TClickType);', @_LapeIOManager_HoldMouse);
    addGlobalFunc('procedure TIOManager.ReleaseMouse(X, Y: Integer; button: TClickType);', @_LapeIOManager_ReleaseMouse);
    addGlobalFunc('procedure TIOManager.ClickMouse(X, Y: Integer; button: TClickType);', @_LapeIOManager_ClickMouse);
    addGlobalFunc('function TIOManager.IsMouseButtonDown(button : TClickType): boolean;', @_LapeIOManager_IsMouseButtonDown);
    addGlobalFunc('procedure TIOManager.KeyUp(key: Integer);', @_LapeIOManager_KeyUp);
    addGlobalFunc('procedure TIOManager.KeyDown(key: Integer);', @_LapeIOManager_KeyDown);
    addGlobalFunc('procedure TIOManager.PressKey(key: Integer);', @_LapeIOManager_PressKey);
    addGlobalFunc('procedure TIOManager.SendText(Text: string; KeyWait, KeyModWait: Integer);', @_LapeIOManager_SendText);
    addGlobalFunc('procedure TIOManager.SendTextEx(Text: string; MinKeyWait, MaxKeyWait: Integer);', @_LapeIOManager_SendTextEx);
    addGlobalFunc('function TIOManager.IsKeyDown(key: Integer): Boolean;', @_LapeIOManager_isKeyDown);
    addGlobalFunc('function TIOManager.GetKeyCode(c : char): Integer;', @_LapeIOManager_GetKeyCode);
    addGlobalFunc('function TIOManager.GetImageTarget: TTarget;', @_LapeIOManager_GetImageTarget);
    addGlobalFunc('function TIOManager.GetKeyMouseTarget: TTarget;', @_LapeIOManager_GetKeyMouseTarget);
    addGlobalFunc('function TIOManager.ExportImageTarget: TTarget_Exported;', @_LapeIOManager_ExportImageTarget);
    addGlobalFunc('function TIOManager.ExportKeyMouseTarget: TTarget_Exported;', @_LapeIOManager_ExportKeyMouseTarget);
    addGlobalFunc('procedure TIOManager.GetImageTarget(var idx: Integer); overload', @_LapeIOManager_GetImageTargetEx);
    addGlobalFunc('procedure TIOManager.GetKeyMouseTarget(var idx: Integer); overload', @_LapeIOManager_GetKeyMouseTargetEx);
    addGlobalFunc('procedure TIOManager.SetImageTarget(idx: Integer);', @_LapeIOManager_SetImageTarget);
    addGlobalFunc('procedure TIOManager.SetKeyMouseTarget(idx: Integer);', @_LapeIOManager_SetKeyMouseTarget);
    addGlobalFunc('procedure TIOManager.FreeTarget(idx: Integer);', @_LapeIOManager_FreeTarget);
    addGlobalFunc('function TIOManager.AddHandlerInvalidTarget(Handler: TNotifyEvent): TNotifyEvent;', @_LapeIOManager_AddHandlerInvalidTarget);
    addGlobalFunc('procedure TIOManager.RemoveHandlerInvalidTarget(Handler: TNotifyEvent);', @_LapeIOManager_RemoveHandlerInvalidTarget);
    addClassVar('TIOManager', 'AutoActivate', 'Boolean', @_LapeIOManager_AutoActivate_Read, @_LapeIOManager_AutoActivate_Write);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportIOManager);

end.

