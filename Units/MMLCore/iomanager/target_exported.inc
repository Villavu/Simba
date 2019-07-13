{%MainUnit simba.target}

{
  Exports the function pointers of the TTarget. This for calling simba's target methods
  from a external plugin.
}

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
