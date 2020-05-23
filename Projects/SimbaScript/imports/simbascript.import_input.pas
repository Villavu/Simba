unit simbascript.import_input;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

implementation

procedure Lape_MoveMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.MoveMouse(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_ScrollMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.ScrollMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_GetMousePos(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.GetMousePos(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_HoldMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.HoldMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_ReleaseMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.ReleaseMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_ClickMouse(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.ClickMouse(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_IsMouseButtonDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := Script.Client.IOManager.IsMouseButtonDown(PInt32(Params^[0])^);
end;

procedure Lape_KeyDown(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.KeyDown(PUInt16(Params^[0])^);
end;

procedure Lape_KeyUp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.KeyUp(PUInt16(Params^[0])^);
end;

procedure Lape_SendKeys(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.SendText(PString(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_PressKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.PressKey(PUInt16(Params^[0])^);
end;

procedure Lape_isKeyDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := Script.Client.IOManager.isKeyDown(PUInt16(Params^[0])^);
end;

procedure Lape_GetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.IOManager.GetKeyCode(PChar(Params^[0])^);
end;

procedure Lape_Import_Input(Compiler: TScriptCompiler);
var
  i: Int32;
begin
  with Compiler do
  begin
    Section := 'Input';

    addGlobalConst(ps_mouse_right, 'MOUSE_RIGHT');
    addGlobalConst(ps_mouse_left, 'MOUSE_LEFT');
    addGlobalConst(ps_mouse_middle, 'MOUSE_MIDDLE');

    addGlobalFunc('procedure MoveMouse(X, Y: Int32);', @Lape_MoveMouse);
    addGlobalFunc('procedure ScrollMouse(X, Y: Int32; Clicks: Int32);', @Lape_ScrollMouse);
    addGlobalFunc('procedure GetMousePos(var X, Y: Int32);', @Lape_GetMousePos);
    addGlobalFunc('procedure HoldMouse(X, Y: Int32; clickType: Int32);', @Lape_HoldMouse);
    addGlobalFunc('procedure ReleaseMouse(X, Y: Int32; clickType: Int32);', @Lape_ReleaseMouse);
    addGlobalFunc('procedure ClickMouse(X, Y: Int32; clickType: Int32);', @Lape_ClickMouse);
    addGlobalFunc('function IsMouseButtonDown(Button: Int32): boolean', @Lape_IsMouseButtonDown);

    for i := 0 to High(VirtualKeys) do
      addGlobalConst(VirtualKeys[i].Key, Format('VK_%S', [VirtualKeys[i].Str]));

    addGlobalFunc('procedure KeyDown(Key: UInt16);', @Lape_KeyDown);
    addGlobalFunc('procedure KeyUp(Key: UInt16);', @Lape_KeyUp);
    addGlobalFunc('procedure SendKeys(const S: String; KeyWait, KeyModWait: Int32);', @Lape_SendKeys);
    addGlobalFunc('procedure PressKey(Key: UInt16);', @Lape_PressKey);
    addGlobalFunc('function isKeyDown(Key: UInt16): Boolean', @Lape_isKeyDown);
    addGlobalFunc('function GetKeyCode(C: Char): Int32', @Lape_GetKeyCode);
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_Input);

end.
