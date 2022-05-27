unit simba.import_target;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.bitmap,
  simba.helpers_windowhandle,  simba.target_exported, simba.target_window;

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
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(PMufasaBitmap(Params^[0])^);
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
begin
  with SimbaScriptThread.Script.Client do
  begin
    BMP := TMufasaBitmap.CreateFromClient(SimbaScriptThread.Script.Client);
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
  TNotifyEvent(Result^) := SimbaScriptThread.Script.Client.IOManager.AddHandlerInvalidTarget(TNotifyEvent(Params^[0]^));
end;

procedure _LapeRemoveHandlerInvalidTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.RemoveHandlerInvalidTarget(TNotifyEvent(Params^[0]^));
end;

procedure _LapeSetAutoActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.IOManager.AutoActivate := PBoolean(Params^[0])^;
end;

procedure _LapeGetAutoActivateClient(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.AutoActivate;
end;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Target');

    addGlobalFunc('function SetDesktopAsClient: Integer', @_LapeSetDesktopAsClient);
    addGlobalFunc('function SetTargetWindow(Window: TWindowHandle): Integer', @_LapeSetTargetWindow);
    addGlobalFunc('function SetTargetArray(P: PtrUInt; W, H: Integer): Integer', @_LapeSetTargetArray);
    addGlobalFunc('function SetTargetBitmap(bitmap: Integer): Integer', @_LapeSetTargetBitmap);
    addGlobalFunc('function SetEIOSTarget(Plugin, Args: String): Integer', @_LapeSetEIOSTarget);
    addGlobalFunc('function MouseSetClientArea(x1, y1, x2, y2: Integer): Boolean', @_LapeMouseSetClientArea);
    addGlobalFunc('procedure MouseResetClientArea', @_LapeMouseResetClientArea);
    addGlobalFunc('function ImageSetClientArea(x1, y1, x2, y2: Integer): Boolean', @_LapeImageSetClientArea);
    addGlobalFunc('procedure ImageResetClientArea', @_LapeImageResetClientArea);
    addGlobalFunc('procedure SetImageTarget(idx: Integer)', @_LapeSetImageTarget);
    addGlobalFunc('procedure SetKeyMouseTarget(idx: Integer)', @_LapeSetKeyMouseTarget);
    addGlobalFunc('function GetImageTarget: Integer', @_LapeGetImageTarget);
    addGlobalFunc('function GetKeyMouseTarget: Integer', @_LapeGetKeyMouseTarget);
    addGlobalFunc('function ExportImageTarget: TTarget_Exported', @_LapeExportImageTarget);
    addGlobalFunc('function ExportKeyMouseTarget: TTarget_Exported', @_LapeExportKeyMouseTarget);
    addGlobalFunc('procedure FreeTarget(idx: Integer)', @_LapeFreeTarget);
    addGlobalFunc('procedure GetClientDimensions(var W, H: Integer)', @_LapeGetClientDimensions);
    addGlobalFunc('procedure GetClientPosition(var Left, Top: Integer)', @_LapeGetClientPosition);
    addGlobalFunc('procedure Freeze', @_LapeFreeze);
    addGlobalFunc('procedure Unfreeze', @_LapeUnfreeze);
    addGlobalFunc('procedure ActivateClient', @_LapeActivateClient);
    addGlobalFunc('function IsTargetValid: Boolean', @_LapeIsTargetValid);
    addGlobalFunc('procedure SaveScreenshot(FileName: string)', @_LapeSaveScreenshot);
    addGlobalFunc('function GetTargetWindow: TWindowHandle', @_LapeGetTargetWindow);
    addGlobalFunc('function GetTargetPID: Integer', @_LapeGetTargetPID);
    addGlobalFunc('function AddHandlerInvalidTarget(Handler: TNotifyEvent): TNotifyEvent;', @_LapeAddHandlerInvalidTarget);
    addGlobalFunc('procedure RemoveHandlerInvalidTarget(Handler: TNotifyEvent);', @_LapeRemoveHandlerInvalidTarget);
    addGlobalFunc('procedure SetAutoActivateClient(Value: Boolean);', @_LapeSetAutoActivateClient);
    addGlobalFunc('function GetAutoActivateClient: Boolean;', @_LapeGetAutoActivateClient);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportTarget);

end.

