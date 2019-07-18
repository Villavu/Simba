unit lptiomanager;
//Depends: TIOManager, TObject, string, PRGB32, TPoint, TMufasaBitmap, integer, Integer, boolean, TClickType, Word, char, Boolean, TTarget

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, script_imports;

procedure Register_TIOManager(Compiler: TLapeCompiler);

implementation

uses
  simba.target, simba.iomanager, simba.target_exported, bitmaps, graphics, mufasatypes;

type
  PIOManager = ^TIOManager;
  PTarget = ^TTarget;
  PTarget_Exported = ^TTarget_Exported;
  PColor = ^TColor;
  PRetData = ^TRetData;
  PClickType = ^TClickType;

//constructor Create;
procedure TIOManager_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^ := TIOManager.Create();
end;

//constructor Create(plugin_dir: string);
procedure TIOManager_InitEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^ := TIOManager.Create(PlpString(Params^[1])^);
end;

//function SetTarget(ArrPtr: PRGB32; Size: TPoint): integer; overload;
procedure TIOManager_SetTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PPRGB32(Params^[1])^, PPoint(Params^[2])^);
end;

//function SetTarget(bmp : TMufasaBitmap) : integer; overload;
procedure TIOManager_SetTargetEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PMufasaBitmap(Params^[1])^);
end;

//function SetTarget(name, initargs: string): integer; overload;
procedure TIOManager_SetTargetExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function TargetValid: Boolean;
procedure TIOManager_TargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.TargetValid();
end;

//function GetColor(x,y : integer) : TColor;
procedure TIOManager_GetColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PIOManager(Params^[0])^.GetColor(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function ReturnData(xs, ys, width, height: Integer): TRetData;
procedure TIOManager_ReturnData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRetData(Result)^ := PIOManager(Params^[0])^.ReturnData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//function CopyData(X, Y, Width, Height: Integer): PRGB32;
procedure TIOManager_CopyData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPRGB32(Result)^ := PIOManager(Params^[0])^.CopyData(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure FreeReturnData;
procedure TIOManager_FreeReturnData(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.FreeReturnData();
end;

//procedure GetDimensions(out W, H: Integer);
procedure TIOManager_GetDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure GetPosition(var Left, Top: Integer);
procedure TIOManager_GetPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetPosition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure ActivateClient;
procedure TIOManager_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ActivateClient();
end;

//function IsFrozen: boolean;
procedure TIOManager_IsFrozen(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.IsFrozen();
end;

//procedure SetFrozen(makefrozen: boolean);
procedure TIOManager_SetFrozen(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetFrozen(Pboolean(Params^[1])^);
end;

//function MouseSetClientArea(x1, y1, x2, y2: integer): boolean;
procedure TIOManager_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.MouseSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure MouseResetClientArea;
procedure TIOManager_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.MouseResetClientArea();
end;

//function ImageSetClientArea(x1, y1, x2, y2: integer): boolean;
procedure TIOManager_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.ImageSetClientArea(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure ImageResetClientArea;
procedure TIOManager_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ImageResetClientArea();
end;

//procedure GetMousePos(var X, Y: Integer);
procedure TIOManager_GetMousePos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetMousePos(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure MoveMouse(X, Y: Integer);
procedure TIOManager_MoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.MoveMouse(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure ScrollMouse(x,y : integer; Lines : integer);
procedure TIOManager_ScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ScrollMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//procedure HoldMouse(x,y : integer; button: TClickType);
procedure TIOManager_HoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.HoldMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PClickType(Params^[3])^);
end;

//procedure ReleaseMouse(x,y : integer; button: TClickType);
procedure TIOManager_ReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ReleaseMouse(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PClickType(Params^[3])^);
end;

//procedure ClickMouse(X, Y: Integer; button: TClickType);
procedure TIOManager_ClickMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.ClickMouse(PInteger(Params^[1])^, PInteger(Params^[2])^, PClickType(Params^[3])^);
end;

//function  IsMouseButtonDown( button : TClickType) : boolean;
procedure TIOManager_IsMouseButtonDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PIOManager(Params^[0])^.IsMouseButtonDown(PClickType(Params^[1])^);
end;

//procedure KeyUp(key: Word);
procedure TIOManager_KeyUp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.KeyUp(PWord(Params^[1])^);
end;

//procedure KeyDown(key: Word);
procedure TIOManager_KeyDown(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.KeyDown(PWord(Params^[1])^);
end;

//procedure PressKey(key: Word);
procedure TIOManager_PressKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.PressKey(PWord(Params^[1])^);
end;

//procedure SendText(text: string; keywait, keymodwait: integer);
procedure TIOManager_SendText(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SendText(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//function isKeyDown(key: Word): Boolean;
procedure TIOManager_isKeyDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PIOManager(Params^[0])^.isKeyDown(PWord(Params^[1])^);
end;

//function GetKeyCode(c : char) : integer;
procedure TIOManager_GetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.GetKeyCode(Pchar(Params^[1])^);
end;

//function GetImageTarget: TTarget; overload;
procedure TIOManager_GetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.GetImageTarget();
end;

//function GetKeyMouseTarget: TTarget; overload;
procedure TIOManager_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget(Result)^ := PIOManager(Params^[0])^.GetKeyMouseTarget();
end;

//function ExportImageTarget  : TTarget_Exported; overload;
procedure TIOManager_ExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := PIOManager(Params^[0])^.ExportImageTarget();
end;

//function ExportKeyMouseTarget  : TTarget_Exported; overload;
procedure TIOManager_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := PIOManager(Params^[0])^.ExportKeyMouseTarget();
end;

//procedure GetImageTarget(var idx: integer); overload;
procedure TIOManager_GetImageTargetEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetImageTarget(Pinteger(Params^[1])^);
end;

//procedure GetKeyMouseTarget(var idx: integer); overload;
procedure TIOManager_GetKeyMouseTargetEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.GetKeyMouseTarget(Pinteger(Params^[1])^);
end;

//procedure SetImageTarget(idx: integer);
procedure TIOManager_SetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetImageTarget(Pinteger(Params^[1])^);
end;

//procedure SetKeyMouseTarget(idx: integer);
procedure TIOManager_SetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.SetKeyMouseTarget(Pinteger(Params^[1])^);
end;

//procedure FreeTarget(idx: integer);
procedure TIOManager_FreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.FreeTarget(Pinteger(Params^[1])^);
end;

//procedure Free();
procedure TIOManager_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Params^[0])^.Free();
end;

//function SetTarget(target: TNativeWindow): integer; overload;
procedure TIOManager_SetTargetHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PIOManager(Params^[0])^.SetTarget(PPtrUInt(Params^[1])^);
end;

procedure Register_TIOManager(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TIOManager');

    addGlobalType('PtrUInt', 'TNativeWindow');
    addGlobalType('record func1, func2, func3, func4, func5, func6, func7, func8, func9, func10, func11, func12, func13, func14, func15, func16: Pointer; end', 'TTarget_Exported');
    addGlobalType('record Title: WideString; Handle: UInt32; PID: UInt32; Width, Height: Int32; end', 'TSysProc');
    addGlobalType('array of TSysProc', 'TSysProcArr');

    addGlobalFunc('function TIOManager.SetTarget(target: TNativeWindow): integer; constref; overload;', @TIOManager_SetTargetHandle);
    addGlobalFunc('function TIOManager.SetTarget(ArrPtr: PRGB32; Size: TPoint): integer; constref; overload;', @TIOManager_SetTarget);
    addGlobalFunc('function TIOManager.SetTarget(bmp : TMufasaBitmap): integer; constref; overload;', @TIOManager_SetTargetEx);
    addGlobalFunc('function TIOManager.SetTarget(name, initargs: string): integer; constref; overload;', @TIOManager_SetTargetExEx);
    addGlobalFunc('function TIOManager.TargetValid(): Boolean; constref;', @TIOManager_TargetValid);
    addGlobalFunc('function TIOManager.GetColor(x,y : integer): TColor; constref;', @TIOManager_GetColor);
    addGlobalFunc('function TIOManager.CopyData(X, Y, Width, Height: Integer): PRGB32; constref;', @TIOManager_CopyData);
    addGlobalFunc('function TIOManager.ReturnData(xs, ys, width, height: Integer): TRetData; constref;', @TIOManager_ReturnData);
    addGlobalFunc('procedure TIOManager.FreeReturnData(); constref;', @TIOManager_FreeReturnData);
    addGlobalFunc('procedure TIOManager.GetDimensions(out W, H: Integer); constref;', @TIOManager_GetDimensions);
    addGlobalFunc('procedure TIOManager.GetPosition(var Left, Top: Integer); constref;', @TIOManager_GetPosition);
    addGlobalFunc('procedure TIOManager.ActivateClient(); constref;', @TIOManager_ActivateClient);
    addGlobalFunc('function TIOManager.IsFrozen(): boolean; constref;', @TIOManager_IsFrozen);
    addGlobalFunc('procedure TIOManager.SetFrozen(makefrozen: boolean); constref;', @TIOManager_SetFrozen);
    addGlobalFunc('function TIOManager.MouseSetClientArea(x1, y1, x2, y2: integer): boolean; constref;', @TIOManager_MouseSetClientArea);
    addGlobalFunc('procedure TIOManager.MouseResetClientArea(); constref;', @TIOManager_MouseResetClientArea);
    addGlobalFunc('function TIOManager.ImageSetClientArea(x1, y1, x2, y2: integer): boolean; constref;', @TIOManager_ImageSetClientArea);
    addGlobalFunc('procedure TIOManager.ImageResetClientArea(); constref;', @TIOManager_ImageResetClientArea);
    addGlobalFunc('procedure TIOManager.GetMousePos(var X, Y: Integer); constref;', @TIOManager_GetMousePos);
    addGlobalFunc('procedure TIOManager.MoveMouse(X, Y: Integer); constref;', @TIOManager_MoveMouse);
    addGlobalFunc('procedure TIOManager.ScrollMouse(x,y : integer; Lines : integer); constref;', @TIOManager_ScrollMouse);
    addGlobalFunc('procedure TIOManager.HoldMouse(x,y : integer; button: TClickType); constref;', @TIOManager_HoldMouse);
    addGlobalFunc('procedure TIOManager.ReleaseMouse(x,y : integer; button: TClickType); constref;', @TIOManager_ReleaseMouse);
    addGlobalFunc('procedure TIOManager.ClickMouse(X, Y: Integer; button: TClickType); constref;', @TIOManager_ClickMouse);
    addGlobalFunc('function TIOManager.IsMouseButtonDown( button : TClickType): boolean; constref;', @TIOManager_IsMouseButtonDown);
    addGlobalFunc('procedure TIOManager.KeyUp(key: Word); constref;', @TIOManager_KeyUp);
    addGlobalFunc('procedure TIOManager.KeyDown(key: Word); constref;', @TIOManager_KeyDown);
    addGlobalFunc('procedure TIOManager.PressKey(key: Word); constref;', @TIOManager_PressKey);
    addGlobalFunc('procedure TIOManager.SendText(text: string; keywait, keymodwait: integer); constref;', @TIOManager_SendText);
    addGlobalFunc('function TIOManager.isKeyDown(key: Word): Boolean; constref;', @TIOManager_isKeyDown);
    addGlobalFunc('function TIOManager.GetKeyCode(c : char): integer; constref;', @TIOManager_GetKeyCode);
    addGlobalFunc('function TIOManager.GetImageTarget(): TTarget; constref;', @TIOManager_GetImageTarget);
    addGlobalFunc('function TIOManager.GetKeyMouseTarget(): TTarget; constref;', @TIOManager_GetKeyMouseTarget);
    addGlobalFunc('function TIOManager.ExportImageTarget(): TTarget_Exported; constref;', @TIOManager_ExportImageTarget);
    addGlobalFunc('function TIOManager.ExportKeyMouseTarget(): TTarget_Exported; constref;', @TIOManager_ExportKeyMouseTarget);
    addGlobalFunc('procedure TIOManager.GetImageTarget(var idx: integer); constref; overload;', @TIOManager_GetImageTargetEx);
    addGlobalFunc('procedure TIOManager.GetKeyMouseTarget(var idx: integer); constref; overload;', @TIOManager_GetKeyMouseTargetEx);
    addGlobalFunc('procedure TIOManager.SetImageTarget(idx: integer); constref;', @TIOManager_SetImageTarget);
    addGlobalFunc('procedure TIOManager.SetKeyMouseTarget(idx: integer); constref;', @TIOManager_SetKeyMouseTarget);
    addGlobalFunc('procedure TIOManager.FreeTarget(idx: integer); constref;', @TIOManager_FreeTarget);
    addGlobalFunc('procedure TIOManager.Free(); constref;', @TIOManager_Free);
  end;
end;

end.

