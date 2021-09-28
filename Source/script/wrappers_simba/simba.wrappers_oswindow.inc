procedure _LapeOSWindow_Activate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.Activate();
end;

procedure _LapeOSWindow_IsVaild(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsValid();
end;

procedure _LapeOSWindow_IsActive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsActive();
end;

procedure _LapeOSWindow_IsActiveEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsActive(PInt32(Params^[1])^);
end;

procedure _LapeOSWindow_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[0])^.IsVisible();
end;

procedure _LapeOSWindow_GetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := POSWindow(Params^[0])^.GetPID();
end;

procedure _LapeOSWindow_GetRootWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := POSWindow(Params^[0])^.GetRootWindow();
end;

procedure _LapeOSWindow_GetTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := POSWindow(Params^[0])^.GetTitle();
end;

procedure _LapeOSWindow_GetClassName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := POSWindow(Params^[0])^.GetClassName();
end;

procedure _LapeOSWindow_GetBounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := POSWindow(Params^[0])^.GetBounds();
end;

procedure _LapeOSWindow_GetChildren(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindow(Params^[0])^.GetChildren(PBoolean(Params^[1])^);
end;

procedure _LapeOSWindow_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Params^[0])^.SetBounds(PBox(Params^[1])^);
end;

procedure _LapeOSWindow_Kill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Params^[0])^.Kill();
end;

procedure _LapeOSWindowArray_GetByTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[0])^.GetByTitle(PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure _LapeOSWindowArray_GetByTitleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[0])^.GetByTitle(PString(Params^[1])^);
end;

procedure _LapeOSWindowArray_GetByClass(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[0])^.GetByClass(PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure _LapeOSWindowArray_GetByClassEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[0])^.GetByClass(PString(Params^[1])^);
end;

procedure _LapeOSWindowArray_GetByTitleAndClass(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[0])^.GetByTitleAndClass(PString(Params^[1])^, PString(Params^[2])^, POSWindow(Params^[3])^);
end;

procedure _LapeOSWindowArray_GetByTitleAndClassEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[0])^.GetByTitleAndClass(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeOSWindowArray_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := POSWindowArray(Params^[0])^.ToString();
end;

procedure _LapeGetTopWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetTopWindows();
end;

procedure _LapeGetVisibleWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetVisibleWindows();
end;

procedure _LapeGetWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetWindows();
end;

procedure _LapeGetActiveWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetActiveWindow();
end;

procedure _LapeGetDesktopWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetDesktopWindow();
end;

procedure _LapeFindWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := FindWindow(PString(Params^[0])^);
end;

procedure _LapeFindWindowEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindWindow(PString(Params^[0])^, POSWindow(Params^[1])^);
end;

procedure _LapeFindChildWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := FindChildWindow(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeFindChildWindowEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindChildWindow(PString(Params^[0])^, PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure _LapeGetWindowAtCursor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetWindowAtCursor();
end;

