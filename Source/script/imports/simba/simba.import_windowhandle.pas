unit simba.import_windowhandle;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.helpers_windowhandle;

procedure _LapeWindowHandle_Activate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.Activate();
end;

procedure _LapeWindowHandle_IsVaild(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.IsValid();
end;

procedure _LapeWindowHandle_IsActive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.IsActive();
end;

procedure _LapeWindowHandle_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PWindowHandle(Params^[0])^.IsVisible();
end;

procedure _LapeWindowHandle_GetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := PWindowHandle(Params^[0])^.GetPID();
end;

procedure _LapeWindowHandle_GetRootWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandle(Result)^ := PWindowHandle(Params^[0])^.GetRootWindow();
end;

procedure _LapeWindowHandle_GetTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PWindowHandle(Params^[0])^.GetTitle();
end;

procedure _LapeWindowHandle_GetClassName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PWindowHandle(Params^[0])^.GetClassName();
end;

procedure _LapeWindowHandle_GetTitleW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := PWindowHandle(Params^[0])^.GetTitleW();
end;

procedure _LapeWindowHandle_GetClassNameW(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := PWindowHandle(Params^[0])^.GetClassNameW();
end;

procedure _LapeWindowHandle_GetBounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PWindowHandle(Params^[0])^.GetBounds();
end;

procedure _LapeWindowHandle_GetChildren(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandleArray(Result)^ := PWindowHandle(Params^[0])^.GetChildren(PBoolean(Params^[1])^);
end;

procedure _LapeWindowHandle_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandle(Params^[0])^.SetBounds(PBox(Params^[1])^);
end;

procedure _LapeWindowHandle_Kill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandle(Params^[0])^.Kill();
end;

procedure _LapeGetTopWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandleArray(Result)^ := GetTopWindows();
end;

procedure _LapeGetVisibleWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandleArray(Result)^ := GetVisibleWindows();
end;

procedure _LapeGetWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandleArray(Result)^ := GetWindows();
end;

procedure _LapeGetActiveWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandle(Result)^ := GetActiveWindow();
end;

procedure _LapeGetDesktopWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandle(Result)^ := GetDesktopWindow();
end;

procedure _LapeGetWindowAtCursor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandle(Result)^ := GetWindowAtCursor();
end;

procedure _LapeFindWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindWindow(PString(Params^[0])^, PWindowHandle(Params^[1])^);
end;

procedure _LapeFindWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandleArray(Result)^ := FindWindows(PString(Params^[0])^);
end;

procedure _LapeFindChildWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindChildWindow(PString(Params^[0])^, PString(Params^[1])^, PWindowHandle(Params^[2])^);
end;

procedure _LapeFindChildWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWindowHandleArray(Result)^ := FindChildWindows(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure ImportWindowHandle(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Window Handle');

    addGlobalType('type PtrUInt', 'TWindowHandle');
    addGlobalType('array of TWindowHandle', 'TWindowHandleArray');

    addGlobalFunc('function TWindowHandle.Activate: Boolean;', @_LapeWindowHandle_Activate);
    addGlobalFunc('function TWindowHandle.IsVaild: Boolean;', @_LapeWindowHandle_IsVaild);
    addGlobalFunc('function TWindowHandle.IsActive: Boolean;', @_LapeWindowHandle_IsActive);
    addGlobalFunc('function TWindowHandle.IsVisible: Boolean;', @_LapeWindowHandle_IsVisible);
    addGlobalFunc('function TWindowHandle.GetPID: UInt32;', @_LapeWindowHandle_GetPID);
    addGlobalFunc('function TWindowHandle.GetRootWindow: TWindowHandle; ', @_LapeWindowHandle_GetRootWindow);
    addGlobalFunc('function TWindowHandle.GetClassName: String;', @_LapeWindowHandle_GetClassName);
    addGlobalFunc('function TWindowHandle.GetTitle: String;', @_LapeWindowHandle_GetTitle);
    addGlobalFunc('function TWindowHandle.GetClassNameW: WideString;', @_LapeWindowHandle_GetClassNameW);
    addGlobalFunc('function TWindowHandle.GetTitleW: WideString;', @_LapeWindowHandle_GetTitleW);
    addGlobalFunc('function TWindowHandle.GetBounds: TBox;', @_LapeWindowHandle_GetBounds);
    addGlobalFunc('function TWindowHandle.GetChildren(Recursive: Boolean = True): TWindowHandleArray;', @_LapeWindowHandle_GetChildren);
    addGlobalFunc('procedure TWindowHandle.SetBounds(Bounds: TBox);', @_LapeWindowHandle_SetBounds);
    addGlobalFunc('procedure TWindowHandle.Kill;', @_LapeWindowHandle_Kill);
    addGlobalFunc('function GetTopWindows: TWindowHandleArray', @_LapeGetTopWindows);
    addGlobalFunc('function GetVisibleWindows: TWindowHandleArray', @_LapeGetVisibleWindows);
    addGlobalFunc('function GetWindows: TWindowHandleArray', @_LapeGetWindows);
    addGlobalFunc('function GetActiveWindow: TWindowHandle', @_LapeGetActiveWindow);
    addGlobalFunc('function GetDesktopWindow: TWindowHandle', @_LapeGetDesktopWindow);
    addGlobalFunc('function GetWindowAtCursor: TWindowHandle', @_LapeGetWindowAtCursor);
    addGlobalFunc('function FindWindow(Title: String; out Window: TWindowHandle): Boolean; overload', @_LapeFindWindow);
    addGlobalFunc('function FindWindows(Title: String): TWindowHandleArray; overload', @_LapeFindWindows);
    addGlobalFunc('function FindChildWindow(Title: String; ClassName: String; out Child: TWindowHandle): Boolean; overload', @_LapeFindChildWindow);
    addGlobalFunc('function FindChildWindows(Title: String; ClassName: String): TWindowHandleArray; overload', @_LapeFindChildWindows);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportWindowHandle);

end.

