unit simba.script_import_ttarget;
//Depends: TTarget, TObject, integer, Integer, TClickType, string, char

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_TTarget(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.target, graphics, simba.iomanager;

type
  PColor = ^TColor;
  PRetData = ^TRetData;
  PClickType = ^TClickType;

//procedure GetTargetDimensions(out w, h: integer); virtual;
procedure TTarget_GetTargetDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.GetTargetDimensions(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure GetTargetPosition(out left, top: integer); virtual;
procedure TTarget_GetTargetPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.GetTargetPosition(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function GetColor(x,y : integer) : TColor; virtual;
procedure TTarget_GetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PTarget(Params^[0])^.GetColor(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function CopyData(X, Y, Width, Height: Integer): PRGB32; virtual;
procedure TTarget_CopyData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PTarget(Params^[0])^.CopyData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//function ReturnData(xs, ys, width, height: Integer): TRetData; virtual;
procedure TTarget_ReturnData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRetData(Result)^ := PTarget(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure FreeReturnData; virtual;
procedure TTarget_FreeReturnData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.FreeReturnData();
end;

//procedure ActivateClient; virtual;
procedure TTarget_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ActivateClient();
end;

//function TargetValid: boolean; virtual;
procedure TTarget_TargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.TargetValid();
end;

//function MouseSetClientArea(x1, y1, x2, y2: integer): boolean; virtual;
procedure TTarget_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.MouseSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure MouseResetClientArea; virtual;
procedure TTarget_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.MouseResetClientArea();
end;

//function ImageSetClientArea(x1, y1, x2, y2: integer): boolean; virtual;
procedure TTarget_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.ImageSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure ImageResetClientArea; virtual;
procedure TTarget_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ImageResetClientArea();
end;

//procedure GetMousePosition(out x,y: integer); virtual;
procedure TTarget_GetMousePosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.GetMousePosition(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure MoveMouse(x,y: integer); virtual;
procedure TTarget_MoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.MoveMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure ScrollMouse(x,y : integer; Lines : integer); virtual;
procedure TTarget_ScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ScrollMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//procedure HoldMouse(x,y: integer; button: TClickType); virtual;
procedure TTarget_HoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.HoldMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PClickType(Params^[3])^);
end;

//procedure ReleaseMouse(x,y: integer; button: TClickType); virtual;
procedure TTarget_ReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ReleaseMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PClickType(Params^[3])^);
end;

//function  IsMouseButtonHeld( button : TClickType) : boolean ; virtual;
procedure TTarget_IsMouseButtonHeld(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.IsMouseButtonHeld(PClickType(Params^[1])^);
end;

//procedure SendString(str: string; keywait, keymodwait: integer); virtual;
procedure TTarget_SendString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.SendString(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//procedure HoldKey(key: integer); virtual;
procedure TTarget_HoldKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.HoldKey(Pinteger(Params^[1])^);
end;

//procedure ReleaseKey(key: integer); virtual;
procedure TTarget_ReleaseKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.ReleaseKey(Pinteger(Params^[1])^);
end;

//function IsKeyHeld(key: integer): boolean; virtual;
procedure TTarget_IsKeyHeld(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PTarget(Params^[0])^.IsKeyHeld(Pinteger(Params^[1])^);
end;

//function GetKeyCode(C : char) : integer; virtual;
procedure TTarget_GetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PTarget(Params^[0])^.GetKeyCode(Pchar(Params^[1])^);
end;

procedure TTarget_GetHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PTarget(Params^[0])^.Handle;
end;

procedure TTarget_SetHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.Handle := PPtrUInt(Params^[1])^;
end;

//constructor Create();
procedure TTarget_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^ := TTarget.Create();
end;

//procedure Free();
procedure TTarget_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.Free();
end;

procedure TTarget_AddHandlerInvalidTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := PTarget(Params^[0])^.AddHandlerInvalidTarget(TNotifyEvent(Params^[1]^));
end;

procedure TTarget_RemoveHandlerInvalidTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.RemoveHandlerInvalidTarget(PInt32(Params^[1])^);
end;

procedure TTarget_SetAutoFocus(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Params^[0])^.AutoFocus := PBoolean(Params^[1])^;
end;

procedure TTarget_GetAutoFocus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PTarget(Params^[0])^.AutoFocus;
end;

procedure Lape_Import_TTarget(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TTarget');

    addGlobalType('record Ptr: PRGB32; IncPtrWith: integer; RowLen: integer; end;', 'TRetData');
    addGlobalType('UInt32', 'TClickType');

    addGlobalFunc('procedure TTarget.GetTargetDimensions(out w, h: integer); constref;', @TTarget_GetTargetDimensions);
    addGlobalFunc('procedure TTarget.GetTargetPosition(out left, top: integer); constref;', @TTarget_GetTargetPosition);
    addGlobalFunc('function TTarget.GetColor(x,y : integer): TColor; constref;', @TTarget_GetColor);
    addGlobalFunc('function TTarget.CopyData(X, Y, Width, Height: Integer): PRGB32; constref;', @TTarget_CopyData);
    addGlobalFunc('function TTarget.ReturnData(xs, ys, width, height: Integer): TRetData; constref;', @TTarget_ReturnData);
    addGlobalFunc('procedure TTarget.FreeReturnData(); constref;', @TTarget_FreeReturnData);
    addGlobalFunc('procedure TTarget.ActivateClient(); constref;', @TTarget_ActivateClient);
    addGlobalFunc('function TTarget.TargetValid(): boolean; constref;', @TTarget_TargetValid);
    addGlobalFunc('function TTarget.MouseSetClientArea(x1, y1, x2, y2: integer): boolean; constref;', @TTarget_MouseSetClientArea);
    addGlobalFunc('procedure TTarget.MouseResetClientArea(); constref;', @TTarget_MouseResetClientArea);
    addGlobalFunc('function TTarget.ImageSetClientArea(x1, y1, x2, y2: integer): boolean; constref;', @TTarget_ImageSetClientArea);
    addGlobalFunc('procedure TTarget.ImageResetClientArea(); constref;', @TTarget_ImageResetClientArea);
    addGlobalFunc('procedure TTarget.GetMousePosition(out x,y: integer); constref;', @TTarget_GetMousePosition);
    addGlobalFunc('procedure TTarget.MoveMouse(x,y: integer); constref;', @TTarget_MoveMouse);
    addGlobalFunc('procedure TTarget.ScrollMouse(x,y : integer; Lines : integer); constref;', @TTarget_ScrollMouse);
    addGlobalFunc('procedure TTarget.HoldMouse(x,y: integer; button: TClickType); constref;', @TTarget_HoldMouse);
    addGlobalFunc('procedure TTarget.ReleaseMouse(x,y: integer; button: TClickType); constref;', @TTarget_ReleaseMouse);
    addGlobalFunc('function TTarget.IsMouseButtonHeld( button : TClickType): boolean; constref;', @TTarget_IsMouseButtonHeld);
    addGlobalFunc('procedure TTarget.SendString(str: string; keywait, keymodwait: integer); constref;', @TTarget_SendString);
    addGlobalFunc('procedure TTarget.HoldKey(key: integer); constref;', @TTarget_HoldKey);
    addGlobalFunc('procedure TTarget.ReleaseKey(key: integer); constref;', @TTarget_ReleaseKey);
    addGlobalFunc('function TTarget.IsKeyHeld(key: integer): boolean; constref;', @TTarget_IsKeyHeld);
    addGlobalFunc('function TTarget.GetKeyCode(C : char): integer; constref;', @TTarget_GetKeyCode);
    addGlobalFunc('function TTarget.GetHandle(): PtrUInt; constref;', @TTarget_GetHandle);
    addGlobalFunc('procedure TTarget.SetHandle(Value: PtrUInt); constref', @TTarget_SetHandle);
    addGlobalFunc('function TTarget.AddHandlerInvalidTarget(Handler: TNotifyEvent): Int32; constref;', @TTarget_AddHandlerInvalidTarget);
    addGlobalFunc('procedure TTarget.RemoveHandlerInvalidTarget(Index: Int32); constref;', @TTarget_RemoveHandlerInvalidTarget);
    addGlobalFunc('procedure TTarget.SetAutoFocus(Value: Boolean); constref;', @TTarget_SetAutoFocus);
    addGlobalFunc('function TTarget.GetAutoFocus: Boolean; constref;', @TTarget_GetAutoFocus);
    addGlobalFunc('procedure TTarget.Init(); override;', @TTarget_Init);
   // addGlobalFunc('procedure TTarget.Free(); constref;', @TTarget_Free);
  end;
end;


end.

