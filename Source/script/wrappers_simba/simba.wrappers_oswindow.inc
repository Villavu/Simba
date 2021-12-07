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

