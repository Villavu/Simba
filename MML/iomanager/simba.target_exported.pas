unit simba.target_exported;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,
  simba.target, simba.mufasatypes;

type
  PTarget_Exported = ^TTarget_Exported;
  TTarget_Exported = record
    Target: Pointer;

    GetTargetDimensions: procedure(Target: Pointer; var Width, Height: Int32); stdcall;
    GetTargetPosition: procedure(Target: Pointer; var Top, Left: Int32); stdcall;
    GetColor: function(Target: Pointer; X, Y: Int32) : Int32; stdcall;
    ReturnData: function(Target: Pointer; X, Y, Width, Height: Int32): TRetData; stdcall;
    FreeReturnData: procedure(Target: Pointer); stdcall;

    GetMousePosition: procedure(Target: Pointer; var X, Y: Int32); stdcall;
    MoveMouse: procedure(Target: Pointer; X, Y: Int32); stdcall;
    ScrollMouse: procedure(Target: Pointer; X, Y: Int32; Lines : Int32); stdcall;
    HoldMouse: procedure(Target: Pointer; X, Y: Int32; Left: Boolean); stdcall;
    ReleaseMouse: procedure(Target: Pointer; X, Y: Int32; left: Boolean); stdcall;
    IsMouseButtonHeld: function(Target: Pointer; Left : Boolean) : Boolean;stdcall;

    SendString: procedure(Target: Pointer; Text: PChar; KeyWait, KeyModWait: Int32); stdcall;
    HoldKey: procedure(Target: Pointer; key: Int32); stdcall;
    ReleaseKey: procedure(Target: Pointer; key: Int32); stdcall;
    IsKeyHeld: function(Target: Pointer; key: Int32): Boolean; stdcall;
    GetKeyCode: function(target : pointer; Character: Char): Int32; stdcall;
    CopyData: function(Target: Pointer; X, Y, Width, Height: Int32): PRGB32; stdcall;
  end;

  TTarget_Helper = class helper for TTarget
  public
    function ExportImageTarget: TTarget_Exported;
    function ExportKeyMouseTarget: TTarget_Exported;
  end;

implementation

procedure TTarget_Exported_GetTargetDimensions(Target: Pointer; var Width, Height: Int32); stdcall;
begin
  TTarget(Target).GetTargetDimensions(Width, Height);
end;

procedure TTarget_Exported_GetTargetPosition(Target: Pointer; var Left, Top: Int32); stdcall;
begin
  TTarget(Target).GetTargetPosition(Left, Top);
end;

function TTarget_Exported_GetColor(Target: Pointer; X, Y: Int32): Int32; stdcall;
begin
  Result := TTarget(Target).GetColor(X, Y);
end;

function TTarget_Exported_ReturnData(Target: Pointer; X, Y, Width, Height: Int32): TRetData; stdcall;
begin
  Result := TTarget(Target).ReturnData(X, Y, Width, Height);
end;

procedure TTarget_Exported_FreeReturnData(Target: Pointer); stdcall;
begin
  TTarget(Target).FreeReturnData;
end;

procedure TTarget_Exported_GetMousePosition(Target: Pointer; var X, Y: Int32); stdcall;
begin
  TTarget(Target).GetMousePosition(X, Y);
end;

procedure TTarget_Exported_MoveMouse(Target: Pointer; X, Y: Int32); stdcall;
begin
  TTarget(Target).MoveMouse(X, Y);
end;

procedure TTarget_Exported_ScrollMouse(Target: Pointer; X, Y: Int32; Lines: Int32); stdcall;
begin
  TTarget(Target).ScrollMouse(X, Y,lines);
end;

procedure TTarget_Exported_HoldMouse(Target: Pointer; X, Y: Int32; Left: Boolean); stdcall;
begin
  if Left then
    TTarget(Target).HoldMouse(X, Y, MOUSE_LEFT)
  else
    TTarget(Target).HoldMouse(X, Y, MOUSE_RIGHT);
end;

procedure TTarget_Exported_ReleaseMouse(Target: Pointer; X, Y: Int32; Left: Boolean); stdcall;
begin
  if Left then
    TTarget(Target).ReleaseMouse(X, Y, MOUSE_LEFT)
  else
    TTarget(Target).ReleaseMouse(X, Y, MOUSE_RIGHT);
end;

function TTarget_Exported_IsMouseButtonHeld(Target: Pointer; Left: Boolean): Boolean; stdcall;
begin
  if Left then
    Result := TTarget(Target).IsMouseButtonHeld(MOUSE_LEFT)
  else
    Result := TTarget(Target).IsMouseButtonHeld(MOUSE_RIGHT);
end;

procedure TTarget_Exported_SendString(Target: Pointer; Text: PChar; KeyWait, KeyModWait: Int32); stdcall;
begin
  TTarget(Target).SendString(Text, KeyWait, KeyModWait);
end;

procedure TTarget_Exported_HoldKey(Target: Pointer; Key: Int32); stdcall;
begin
  TTarget(Target).HoldKey(Key);
end;

procedure TTarget_Exported_ReleaseKey(Target: Pointer; Key: Int32); stdcall;
begin
  TTarget(Target).ReleaseKey(Key);
end;

function TTarget_Exported_IsKeyHeld(Target: Pointer; Key: Int32): Boolean; stdcall;
begin
  Result := TTarget(Target).IsKeyHeld(Key);
end;

function TTarget_Exported_GetKeyCode(Target: Pointer; Character: Char): Int32; stdcall;
begin
  Result := TTarget(Target).GetKeyCode(Character);
end;

function TTarget_Exported_CopyData(Target: Pointer; X, Y, Width, Height: Int32): PRGB32; stdcall;
begin
  Result := TTarget(Target).CopyData(X, Y, Width, Height);
end;

function TTarget_Helper.ExportImageTarget: TTarget_Exported;
begin
  Result := Default(TTarget_Exported);

  with Result do
  begin
    Target := Self;

    GetTargetDimensions := @TTarget_Exported_GetTargetDimensions;
    GetTargetPosition := @TTarget_Exported_GetTargetPosition;
    GetColor := @TTarget_Exported_GetColor;
    ReturnData := @TTarget_Exported_ReturnData;
    FreeReturnData := @TTarget_Exported_FreeReturnData;
    CopyData := @TTarget_Exported_CopyData;
  end;
end;

function TTarget_Helper.ExportKeyMouseTarget: TTarget_Exported;
begin
  Result := Default(TTarget_Exported);

  with Result do
  begin
    Target := Self;

    GetMousePosition := @TTarget_Exported_GetMousePosition;
    MoveMouse := @TTarget_Exported_MoveMouse;
    ScrollMouse := @TTarget_Exported_ScrollMouse;
    HoldMouse := @TTarget_Exported_HoldMouse;
    ReleaseMouse := @TTarget_Exported_ReleaseMouse;
    IsMouseButtonHeld := @TTarget_Exported_IsMouseButtonHeld;

    SendString := @TTarget_Exported_SendString;
    HoldKey := @TTarget_Exported_HoldKey;
    ReleaseKey := @TTarget_Exported_ReleaseKey;
    IsKeyHeld := @TTarget_Exported_IsKeyHeld;
    GetKeyCode := @TTarget_Exported_GetKeyCode;
  end;
end;

end.

