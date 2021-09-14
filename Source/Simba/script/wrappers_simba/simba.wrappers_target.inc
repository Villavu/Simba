procedure _LapeSetDesktopAsClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetDesktop();
end;

procedure _LapeSetTargetArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.IOManager.SetTarget(PRGB32(PPtrUInt(Params^[0])^), Point(PInt32(Params^[1])^, PInt32(Params^[2])^));
end;

procedure _LapeSetTargetBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := IOManager.SetTarget(MBitmaps[PInt32(Params^[0])^]);
end;

procedure _LapeSetEIOSTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.IOManager.SetTarget(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure _LapeMouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.IOManager.MouseSetClientArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeMouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.MouseResetClientArea();
end;

procedure _LapeImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.IOManager.ImageSetClientArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure _LapeImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ImageResetClientArea();
end;

procedure _LapeSetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   SimbaScript.Client.IOManager.SetImageTarget(PInt32(Params^[0])^);
end;

procedure _LapeSetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetKeyMouseTarget(PInt32(Params^[0])^);
end;

procedure _LapeGetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetImageTarget(PInt32(Result)^);
end;

procedure _LapeGetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetKeyMouseTarget(PInt32(Result)^);
end;

procedure _LapeExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := SimbaScript.Client.IOManager.ExportImageTarget();
end;

procedure _LapeExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := SimbaScript.Client.IOManager.ExportKeyMouseTarget();
end;

procedure _LapeFreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.FreeTarget(PInt32(Params^[0])^);
end;

procedure _LapeGetClientDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetDimensions(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeGetClientPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetPosition(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure _LapeFreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetFrozen(True);
end;

procedure _LapeUnfreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetFrozen(False);
end;

procedure _LapeActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ActivateClient();
end;

procedure _LapeIsTargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.IOManager.TargetValid();
end;

procedure _LapeGetNativeWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := SimbaScript.Client.IOManager.GetImageTarget.Handle;
end;

procedure _LapeSaveScreenshot(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  BMP: TMufasaBitmap;
  W, H: Int32;
begin
  with SimbaScript.Client do
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
  POSWindow(Result)^ := SimbaScript.Client.IOManager.GetImageTarget().Handle;
end;

procedure _LapeGetTargetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := TOSWindow(SimbaScript.Client.IOManager.GetImageTarget().Handle).GetPID();
end;

