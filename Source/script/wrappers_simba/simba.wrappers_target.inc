procedure _LapeSetDesktopAsClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetDesktop();
end;

procedure _LapeSetTargetWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(PWindowHandle(Params^[0])^);
end;

procedure _LapeSetTargetArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(PRGB32(PPtrUInt(Params^[0])^), Point(PInteger(Params^[1])^, PInteger(Params^[2])^));
end;

procedure _LapeSetTargetBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScriptThread.Script.Client do
    PInteger(Result)^ := IOManager.SetTarget(MBitmaps[PInteger(Params^[0])^]);
end;

procedure _LapeSetEIOSTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure _LapeMouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.MouseSetClientArea(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.MouseResetClientArea();
end;

procedure _LapeImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.ImageSetClientArea(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ImageResetClientArea();
end;

procedure _LapeSetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   SimbaScriptThread.Script.Client.IOManager.SetImageTarget(PInteger(Params^[0])^);
end;

procedure _LapeSetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.SetKeyMouseTarget(PInteger(Params^[0])^);
end;

procedure _LapeGetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.GetImageTarget(PInteger(Result)^);
end;

procedure _LapeGetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.GetKeyMouseTarget(PInteger(Result)^);
end;

procedure _LapeExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := SimbaScriptThread.Script.Client.IOManager.ExportImageTarget();
end;

procedure _LapeExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := SimbaScriptThread.Script.Client.IOManager.ExportKeyMouseTarget();
end;

procedure _LapeFreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.FreeTarget(PInteger(Params^[0])^);
end;

procedure _LapeGetClientDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.GetDimensions(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeGetClientPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.GetPosition(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.Freeze();
end;

procedure _LapeUnFreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.UnFreeze();
end;

procedure _LapeActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.ActivateClient();
end;

procedure _LapeIsTargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.TargetValid();
end;

procedure _LapeSaveScreenshot(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  BMP: TMufasaBitmap;
  W, H: Int32;
begin
  with SimbaScriptThread.Script.Client do
  begin
    IOManager.GetDimensions(W, H);

    BMP := TMufasaBitmap.Create();
    BMP.CopyClientToBitmap(IOManager, True, 0, 0, W-1, H-1);

    try
      BMP.SaveToFile(PString(Params^[0])^);
    finally
      BMP.Free();
    end;
  end;
end;

procedure _LapeGetTargetWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if SimbaScriptThread.Script.Client.IOManager.GetImageTarget() is TWindowTarget then
    PWindowHandle(Result)^ := TWindowTarget(SimbaScriptThread.Script.Client.IOManager.GetImageTarget()).WindowHandle;
end;

procedure _LapeGetTargetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if SimbaScriptThread.Script.Client.IOManager.GetImageTarget() is TWindowTarget then
    PInteger(Result)^ := TWindowTarget(SimbaScriptThread.Script.Client.IOManager.GetImageTarget()).WindowHandle.GetPID();
end;

procedure _LapeAddHandlerInvalidTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := SimbaScriptThread.Script.Client.IOManager.AddHandlerInvalidTarget(PNotifyEvent(Params^[0])^);
end;

procedure _LapeRemoveHandlerInvalidTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.RemoveHandlerInvalidTarget(PNotifyEvent(Params^[0])^);
end;

procedure _LapeSetAutoActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.AutoActivate := PBoolean(Params^[0])^;
end;

procedure _LapeGetAutoActivateClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.AutoActivate;
end;



