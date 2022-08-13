unit simba.import_keyboard;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.scriptthread;

procedure _LapeKeyDown(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.KeyDown(PInteger(Params^[0])^);
end;

procedure _LapeKeyUp(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.KeyUp(PInteger(Params^[0])^);
end;

procedure _LapeSendKeys(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.SendText(PString(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSendKeysEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.SendTextEx(PString(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePressKey(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.PressKey(PInteger(Params^[0])^);
end;

procedure _LapeIsKeyDown(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.IsKeyDown(PInteger(Params^[0])^);
end;

procedure _LapeGetKeyCode(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.GetKeyCode(PChar(Params^[0])^);
end;

procedure ImportKeyboard(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Keyboard');

    addGlobalFunc('procedure KeyDown(Key: Integer)', @_LapeKeyDown);
    addGlobalFunc('procedure KeyUp(Key: Integer)', @_LapeKeyUp);
    addGlobalFunc('procedure SendKeys(const S: String; KeyWait, KeyModWait: Integer)', @_LapeSendKeys);
    addGlobalFunc('procedure SendKeysEx(const S: String; MinKeyWait, MaxKeyWait: Integer)', @_LapeSendKeysEx);
    addGlobalFunc('procedure PressKey(Key: Integer)', @_LapePressKey);
    addGlobalFunc('function IsKeyDown(Key: Integer): Boolean', @_LapeIsKeyDown);
    addGlobalFunc('function GetKeyCode(C: Char): Integer', @_LapeGetKeyCode);

    pushSection('Virtual Keys');
    addGlobalVar(Integer(0), 'VK_UNKNOWN').isConstant := True;
    addGlobalVar(Integer(1), 'VK_LBUTTON').isConstant := True;
    addGlobalVar(Integer(2), 'VK_RBUTTON').isConstant := True;
    addGlobalVar(Integer(3), 'VK_CANCEL').isConstant := True;
    addGlobalVar(Integer(4), 'VK_MBUTTON').isConstant := True;
    addGlobalVar(Integer(5), 'VK_XBUTTON1').isConstant := True;
    addGlobalVar(Integer(6), 'VK_XBUTTON2').isConstant := True;
    addGlobalVar(Integer(8), 'VK_BACK').isConstant := True;
    addGlobalVar(Integer(9), 'VK_TAB').isConstant := True;
    addGlobalVar(Integer(12), 'VK_CLEAR').isConstant := True;
    addGlobalVar(Integer(13), 'VK_RETURN').isConstant := True;
    addGlobalVar(Integer(16), 'VK_SHIFT').isConstant := True;
    addGlobalVar(Integer(17), 'VK_CONTROL').isConstant := True;
    addGlobalVar(Integer(18), 'VK_MENU').isConstant := True;
    addGlobalVar(Integer(19), 'VK_PAUSE').isConstant := True;
    addGlobalVar(Integer(20), 'VK_CAPITAL').isConstant := True;
    addGlobalVar(Integer(27), 'VK_ESCAPE').isConstant := True;
    addGlobalVar(Integer(32), 'VK_SPACE').isConstant := True;
    addGlobalVar(Integer(33), 'VK_PRIOR').isConstant := True;
    addGlobalVar(Integer(34), 'VK_NEXT').isConstant := True;
    addGlobalVar(Integer(35), 'VK_END').isConstant := True;
    addGlobalVar(Integer(36), 'VK_HOME').isConstant := True;
    addGlobalVar(Integer(37), 'VK_LEFT').isConstant := True;
    addGlobalVar(Integer(38), 'VK_UP').isConstant := True;
    addGlobalVar(Integer(39), 'VK_RIGHT').isConstant := True;
    addGlobalVar(Integer(40), 'VK_DOWN').isConstant := True;
    addGlobalVar(Integer(41), 'VK_SELECT').isConstant := True;
    addGlobalVar(Integer(42), 'VK_PRINT').isConstant := True;
    addGlobalVar(Integer(43), 'VK_EXECUTE').isConstant := True;
    addGlobalVar(Integer(44), 'VK_SNAPSHOT').isConstant := True;
    addGlobalVar(Integer(45), 'VK_INSERT').isConstant := True;
    addGlobalVar(Integer(46), 'VK_DELETE').isConstant := True;
    addGlobalVar(Integer(47), 'VK_HELP').isConstant := True;
    addGlobalVar(Integer(48), 'VK_0').isConstant := True;
    addGlobalVar(Integer(49), 'VK_1').isConstant := True;
    addGlobalVar(Integer(50), 'VK_2').isConstant := True;
    addGlobalVar(Integer(51), 'VK_3').isConstant := True;
    addGlobalVar(Integer(52), 'VK_4').isConstant := True;
    addGlobalVar(Integer(53), 'VK_5').isConstant := True;
    addGlobalVar(Integer(54), 'VK_6').isConstant := True;
    addGlobalVar(Integer(55), 'VK_7').isConstant := True;
    addGlobalVar(Integer(56), 'VK_8').isConstant := True;
    addGlobalVar(Integer(57), 'VK_9').isConstant := True;
    addGlobalVar(Integer(65), 'VK_A').isConstant := True;
    addGlobalVar(Integer(66), 'VK_B').isConstant := True;
    addGlobalVar(Integer(67), 'VK_C').isConstant := True;
    addGlobalVar(Integer(68), 'VK_D').isConstant := True;
    addGlobalVar(Integer(69), 'VK_E').isConstant := True;
    addGlobalVar(Integer(70), 'VK_F').isConstant := True;
    addGlobalVar(Integer(71), 'VK_G').isConstant := True;
    addGlobalVar(Integer(72), 'VK_H').isConstant := True;
    addGlobalVar(Integer(73), 'VK_I').isConstant := True;
    addGlobalVar(Integer(74), 'VK_J').isConstant := True;
    addGlobalVar(Integer(75), 'VK_K').isConstant := True;
    addGlobalVar(Integer(76), 'VK_L').isConstant := True;
    addGlobalVar(Integer(77), 'VK_M').isConstant := True;
    addGlobalVar(Integer(78), 'VK_N').isConstant := True;
    addGlobalVar(Integer(79), 'VK_O').isConstant := True;
    addGlobalVar(Integer(80), 'VK_P').isConstant := True;
    addGlobalVar(Integer(81), 'VK_Q').isConstant := True;
    addGlobalVar(Integer(82), 'VK_R').isConstant := True;
    addGlobalVar(Integer(83), 'VK_S').isConstant := True;
    addGlobalVar(Integer(84), 'VK_T').isConstant := True;
    addGlobalVar(Integer(85), 'VK_U').isConstant := True;
    addGlobalVar(Integer(86), 'VK_V').isConstant := True;
    addGlobalVar(Integer(87), 'VK_W').isConstant := True;
    addGlobalVar(Integer(88), 'VK_X').isConstant := True;
    addGlobalVar(Integer(89), 'VK_Y').isConstant := True;
    addGlobalVar(Integer(90), 'VK_Z').isConstant := True;
    addGlobalVar(Integer(91), 'VK_LWIN').isConstant := True;
    addGlobalVar(Integer(92), 'VK_RWIN').isConstant := True;
    addGlobalVar(Integer(93), 'VK_APPS').isConstant := True;
    addGlobalVar(Integer(95), 'VK_SLEEP').isConstant := True;
    addGlobalVar(Integer(96), 'VK_NUMPAD0').isConstant := True;
    addGlobalVar(Integer(97), 'VK_NUMPAD1').isConstant := True;
    addGlobalVar(Integer(98), 'VK_NUMPAD2').isConstant := True;
    addGlobalVar(Integer(99), 'VK_NUMPAD3').isConstant := True;
    addGlobalVar(Integer(100), 'VK_NUMPAD4').isConstant := True;
    addGlobalVar(Integer(101), 'VK_NUMPAD5').isConstant := True;
    addGlobalVar(Integer(102), 'VK_NUMPAD6').isConstant := True;
    addGlobalVar(Integer(103), 'VK_NUMPAD7').isConstant := True;
    addGlobalVar(Integer(104), 'VK_NUMPAD8').isConstant := True;
    addGlobalVar(Integer(105), 'VK_NUMPAD9').isConstant := True;
    addGlobalVar(Integer(106), 'VK_MULTIPLY').isConstant := True;
    addGlobalVar(Integer(107), 'VK_ADD').isConstant := True;
    addGlobalVar(Integer(108), 'VK_SEPARATOR').isConstant := True;
    addGlobalVar(Integer(109), 'VK_SUBTRACT').isConstant := True;
    addGlobalVar(Integer(110), 'VK_DECIMAL').isConstant := True;
    addGlobalVar(Integer(111), 'VK_DIVIDE').isConstant := True;
    addGlobalVar(Integer(112), 'VK_F1').isConstant := True;
    addGlobalVar(Integer(113), 'VK_F2').isConstant := True;
    addGlobalVar(Integer(114), 'VK_F3').isConstant := True;
    addGlobalVar(Integer(115), 'VK_F4').isConstant := True;
    addGlobalVar(Integer(116), 'VK_F5').isConstant := True;
    addGlobalVar(Integer(117), 'VK_F6').isConstant := True;
    addGlobalVar(Integer(118), 'VK_F7').isConstant := True;
    addGlobalVar(Integer(119), 'VK_F8').isConstant := True;
    addGlobalVar(Integer(120), 'VK_F9').isConstant := True;
    addGlobalVar(Integer(121), 'VK_F10').isConstant := True;
    addGlobalVar(Integer(122), 'VK_F11').isConstant := True;
    addGlobalVar(Integer(123), 'VK_F12').isConstant := True;
    addGlobalVar(Integer(124), 'VK_F13').isConstant := True;
    addGlobalVar(Integer(125), 'VK_F14').isConstant := True;
    addGlobalVar(Integer(126), 'VK_F15').isConstant := True;
    addGlobalVar(Integer(127), 'VK_F16').isConstant := True;
    addGlobalVar(Integer(128), 'VK_F17').isConstant := True;
    addGlobalVar(Integer(129), 'VK_F18').isConstant := True;
    addGlobalVar(Integer(130), 'VK_F19').isConstant := True;
    addGlobalVar(Integer(131), 'VK_F20').isConstant := True;
    addGlobalVar(Integer(132), 'VK_F21').isConstant := True;
    addGlobalVar(Integer(133), 'VK_F22').isConstant := True;
    addGlobalVar(Integer(134), 'VK_F23').isConstant := True;
    addGlobalVar(Integer(135), 'VK_F24').isConstant := True;
    addGlobalVar(Integer(144), 'VK_NUMLOCK').isConstant := True;
    addGlobalVar(Integer(145), 'VK_SCROLL').isConstant := True;
    addGlobalVar(Integer(160), 'VK_LSHIFT').isConstant := True;
    addGlobalVar(Integer(161), 'VK_RSHIFT').isConstant := True;
    addGlobalVar(Integer(162), 'VK_LCONTROL').isConstant := True;
    addGlobalVar(Integer(163), 'VK_RCONTROL').isConstant := True;
    addGlobalVar(Integer(164), 'VK_LMENU').isConstant := True;
    addGlobalVar(Integer(165), 'VK_RMENU').isConstant := True;
    addGlobalVar(Integer(166), 'VK_BROWSER_BACK').isConstant := True;
    addGlobalVar(Integer(167), 'VK_BROWSER_FORWARD').isConstant := True;
    addGlobalVar(Integer(168), 'VK_BROWSER_REFRESH').isConstant := True;
    addGlobalVar(Integer(169), 'VK_BROWSER_STOP').isConstant := True;
    addGlobalVar(Integer(170), 'VK_BROWSER_SEARCH').isConstant := True;
    addGlobalVar(Integer(171), 'VK_BROWSER_FAVORITES').isConstant := True;
    addGlobalVar(Integer(172), 'VK_BROWSER_HOME').isConstant := True;
    addGlobalVar(Integer(173), 'VK_VOLUME_MUTE').isConstant := True;
    addGlobalVar(Integer(174), 'VK_VOLUME_DOWN').isConstant := True;
    addGlobalVar(Integer(175), 'VK_VOLUME_UP').isConstant := True;
    addGlobalVar(Integer(176), 'VK_MEDIA_NEXT_TRACK').isConstant := True;
    addGlobalVar(Integer(177), 'VK_MEDIA_PREV_TRACK').isConstant := True;
    addGlobalVar(Integer(178), 'VK_MEDIA_STOP').isConstant := True;
    addGlobalVar(Integer(179), 'VK_MEDIA_PLAY_PAUSE').isConstant := True;
    addGlobalVar(Integer(250), 'VK_PLAY').isConstant := True;
    addGlobalVar(Integer(251), 'VK_ZOOM').isConstant := True;

    popSection();

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportKeyboard);

end.

