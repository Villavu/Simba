unit simba.import_target;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.bitmap,
  simba.windowhandle, simba.target_exported, simba.target_window;

procedure _LapeSetDesktopAsClient(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetDesktop();
end;

procedure _LapeSetTargetWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(PWindowHandle(Params^[0])^);
end;

procedure _LapeSetTargetData(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(PPointer(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSetTargetBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(PMufasaBitmap(Params^[0])^);
end;

procedure _LapeSetEIOSTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.SetTarget(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure _LapeMouseSetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.MouseSetClientArea(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMouseResetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.MouseResetClientArea();
end;

procedure _LapeImageSetClientArea(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.ImageSetClientArea(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeImageResetClientArea(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.ImageResetClientArea();
end;

procedure _LapeSetImageTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
   SimbaScriptThread.Script.Client.IOManager.SetImageTarget(PInteger(Params^[0])^);
end;

procedure _LapeSetKeyMouseTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.SetKeyMouseTarget(PInteger(Params^[0])^);
end;

procedure _LapeGetImageTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.GetImageTarget(PInteger(Result)^);
end;

procedure _LapeGetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.GetKeyMouseTarget(PInteger(Result)^);
end;

procedure _LapeExportImageTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget_Exported(Result)^ := SimbaScriptThread.Script.Client.IOManager.ExportImageTarget();
end;

procedure _LapeExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTarget_Exported(Result)^ := SimbaScriptThread.Script.Client.IOManager.ExportKeyMouseTarget();
end;

procedure _LapeFreeTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.FreeTarget(PInteger(Params^[0])^);
end;

procedure _LapeGetClientDimensions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.GetDimensions(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeGetClientPosition(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.GetPosition(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeFreeze(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.Freeze();
end;

procedure _LapeUnFreeze(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.UnFreeze();
end;

procedure _LapeActivateClient(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.ActivateClient();
end;

procedure _LapeIsTargetValid(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.TargetValid();
end;

procedure _LapeGetTargetWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if SimbaScriptThread.Script.Client.IOManager.GetImageTarget() is TWindowTarget then
    PWindowHandle(Result)^ := TWindowTarget(SimbaScriptThread.Script.Client.IOManager.GetImageTarget()).WindowHandle;
end;

procedure _LapeGetTargetPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  if SimbaScriptThread.Script.Client.IOManager.GetImageTarget() is TWindowTarget then
    PInteger(Result)^ := TWindowTarget(SimbaScriptThread.Script.Client.IOManager.GetImageTarget()).WindowHandle.GetPID();
end;

procedure _LapeAddHandlerInvalidTarget(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TNotifyEvent(Result^) := SimbaScriptThread.Script.Client.IOManager.AddHandlerInvalidTarget(TNotifyEvent(Params^[0]^));
end;

procedure _LapeRemoveHandlerInvalidTarget(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.RemoveHandlerInvalidTarget(TNotifyEvent(Params^[0]^));
end;

procedure _LapeSetAutoActivateClient(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.IOManager.AutoActivate := PBoolean(Params^[0])^;
end;

procedure _LapeGetAutoActivateClient(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.IOManager.AutoActivate;
end;

procedure _LapeGetClientWidth(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.GetWidth();
end;

procedure _LapeGetClientHeight(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SimbaScriptThread.Script.Client.IOManager.GetHeight();
end;

procedure ImportTarget(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Target';

    addGlobalFunc('function SetDesktopAsClient: Integer', @_LapeSetDesktopAsClient);
    addGlobalFunc('function SetTargetWindow(Window: TWindowHandle): Integer', @_LapeSetTargetWindow);
    addGlobalFunc('function SetTargetData(Data: PRGB32; W, H: Integer): Integer', @_LapeSetTargetData);
    addGlobalFunc('function SetTargetBitmap(Bitmap: TMufasaBitmap): Integer', @_LapeSetTargetBitmap);
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
    addGlobalFunc('function GetTargetWindow: TWindowHandle', @_LapeGetTargetWindow);
    addGlobalFunc('function GetTargetPID: Integer', @_LapeGetTargetPID);
    addGlobalFunc('function AddHandlerInvalidTarget(Handler: TNotifyEvent): TNotifyEvent;', @_LapeAddHandlerInvalidTarget);
    addGlobalFunc('procedure RemoveHandlerInvalidTarget(Handler: TNotifyEvent);', @_LapeRemoveHandlerInvalidTarget);
    addGlobalFunc('procedure SetAutoActivateClient(Value: Boolean);', @_LapeSetAutoActivateClient);
    addGlobalFunc('function GetAutoActivateClient: Boolean;', @_LapeGetAutoActivateClient);

    addGlobalFunc('function GetClientWidth: Integer', @_LapeGetClientWidth);
    addGlobalFunc('function GetClientHeight: Integer', @_LapeGetClientHeight);

    addGlobalFunc(
      'function SaveScreenshot: String; overload;', [
      'var',
      '  FileName: String;',
      '  T: UInt64;',
      'begin',
      '  with TMufasaBitmap.CreateFromClient() do',
      '  try',
      '    FileName := ScreenshotPath + ExtractFileName(ScriptFile) + #32 + FormatDateTime("dd-mm hh-mm-ss", Now()) + ".png";',
      '    if SaveToFile(FileName) then',
      '      Exit(FileName);',
      '',
      '    // File not available! Try for a little with milliseconds (zzz)',
      '    T := GetTickCount() + 1000;',
      '    while (GetTickCount() < T) do',
      '    begin',
      '      FileName := ScreenshotPath + ExtractFileName(ScriptFile) + #32 + FormatDateTime("dd-mm hh-mm-ss-zzz", Now()) + ".png";',
      '      if SaveToFile(FileName) then',
      '        Exit(FileName);',
      '',
      '      Sleep(50);',
      '    end;',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'function SaveScreenshot(FileName: String): String; overload;', [
      'begin',
      '  if (ExtractFileExt(FileName) = "") then',
      '    FileName := FileName + ".png";',
      '',
      '  with TMufasaBitmap.CreateFromClient() do',
      '  try',
      '    if SaveToFile(FileName) then',
      '      Result := FileName;',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportTarget);

end.

