procedure _LapePlaySound(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaNativeInterface.PlaySound(PString(Params^[0])^);
end;

procedure _LapeStopSound(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaNativeInterface.StopSound();
end;

procedure _LapeSetSupressExceptions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MFinder.WarnOnly := PBoolean(Params^[0])^;
end;

procedure _LapeSimba(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  WriteLn(DecompressString(Base64Decode(
    '9AoAAHicldU7b+0gDADgvdL9D+AisSDOTrpUvWvGuzNVOkMn9v72a942j6T1dJrEX7AxqRAXYaS9up3iz8suVxoMKe+' +
    'NC6LGnbEhiCCfTzPfJ5cmgidj5J9MsezSQAyApGHGR17N9SpGoBj1tkuRkJHoAk3WeMfTC66GWbaTFtMAwZDPRjh73U4uCKGnRTh3NMK0mAjiXxA975iERASl' +
    'QjfcRLBVS963TKCQDb0m8Brwwv1IKAWkErcipPNAC5+JdPmY62hE/O3L8yE+T4k4PpGwi2aiEIn25zcqKMQ1a6bgNtGN4kJqJ1tYeqFwrMNDcCFvKjMsWXLOK' +
    'N19toPbBN2PmacG9BogFoW7CQD00JTHdZlLml1yQZiv8zzBxGlQzxoxlx+Gdjo8JQDMV8w/0UmCctC/PGZDIKKPFMIGOM8M5IlUyuMel05IwY3hiHoMTLJYdg' +
    'RKvhJxsGt5wzKI8PApjpQTQmj5CkIRIO6S3REPXZjD1kyNGxABm60IxLkdu8HqQOaRmt0TcTVVFHzCdq2oX6ae2CMRuo/bWuhdHfMhfSI8PTE3xIjAuIRu7An' +
    'hv0kN+e38+1GMPYH/hq1PcyKsywdWvI1n9Y4YXzsLydgSphI4G7i/AexYRTW2RJmBPqFqTcgtUW7T6dgQlwIDfrsIsyDCphcbot5eDPgviZ8Yt0S4Ne4Iuoy/H' +
    '+//1sR/NLyhCQ==')));
end;

procedure _LapeSetClipBoard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  try
    Clipboard.AsText := PString(Params^[0])^;
  except
  end;
end;

procedure _LapeGetClipBoard(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  try
    PString(Result)^ := Clipboard.AsText;
  except
  end;
end;

procedure _LapeStatus(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_Status.Create(PString(Params^[0])^);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeShowBalloonHint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ShowBalloonHint.Create(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, TBalloonFlags(Params^[3]^));

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeClearDebug(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ClearDebug.Create();

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeDisguise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_Disguse.Create(PString(Params^[0])^);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ShowBitmap.Create(PMufasaBitmap(Params^[0])^);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeShowBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  with SimbaScriptThread.Script.Client do
    Bitmap := MBitmaps[PInt32(Params^[0])^];

  Method := TSimbaMethod_ShowBitmap.Create(Bitmap);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeDisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_DisplayDebugImage.Create(PInt32(Params^[0])^, PInt32(Params^[1])^);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeDrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_UpdateDebugImage.Create(PMufasaBitmap(Params^[0])^);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeDrawBitmapDebugImgEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  with SimbaScriptThread.Script.Client do
    Bitmap := MBitmaps[PInt32(Params^[0])^];

  Method := TSimbaMethod_UpdateDebugImage.Create(Bitmap);

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ClearDebugImage.Create();

  try
    SimbaScriptThread.Script.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure _LapeGetSimbaPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_GetSimbaPID.Create();

  try
    SimbaScriptThread.Script.Invoke(Method);

    Method.Result.Read(Result^, SizeOf(PtrUint));
  finally
    Method.Free();
  end;
end;

procedure _LapeGetSimbaTargetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_GetSimbaTargetPID.Create();

  try
    SimbaScriptThread.Script.Invoke(Method);

    Method.Result.Read(Result^, SizeOf(PtrUInt));
  finally
    Method.Free();
  end;
end;

procedure _LapeGetSimbaTargetWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_GetSimbaTargetWindow.Create();

  try
    SimbaScriptThread.Script.Invoke(Method);

    Method.Result.Read(Result^, SizeOf(PtrUInt));
  finally
    Method.Free();
  end;
end;

