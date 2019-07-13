unit script_import_target;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, mufasatypes, simba.iomanager, bitmaps,
  simba.target;

procedure Lape_GetProcessID(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeUInt(Result)^ := GetProcessID();
end;

procedure Lape_SetTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.SetTargetEx(PSysProc(Params^[1])^);
end;

procedure Lape_SetDesktopAsClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.SetDesktop();
end;

procedure Lape_SetTargetArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.SetTarget(PRGB32(PPtrUInt(Params^[1])^), Point(PInt32(Params^[2])^, PInt32(Params^[3])^));
end;

procedure Lape_SetTargetBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with TMMLScriptThread(Params^[0]).Client do
    PInt32(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.SetTarget(MBitmaps[PInt32(Params^[1])^]);
end;

procedure Lape_SetEIOSTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.SetTarget(Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.MouseSetClientArea(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.MouseResetClientArea();
end;

procedure Lape_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.ImageSetClientArea(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
end;

procedure Lape_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.ImageResetClientArea();
end;

procedure Lape_SetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   TMMLScriptThread(Params^[0]).Client.IOManager.SetImageTarget(PInt32(Params^[1])^);
end;

procedure Lape_SetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.SetKeyMouseTarget(PInt32(Params^[1])^);
end;

procedure Lape_GetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.GetImageTarget(PInt32(Result)^);
end;

procedure Lape_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.GetKeyMouseTarget(PInt32(Result)^);
end;

procedure Lape_ExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.ExportImageTarget();
end;

procedure Lape_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.ExportKeyMouseTarget();
end;

procedure Lape_FreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.FreeTarget(PInt32(Params^[1])^);
end;

procedure Lape_GetClientDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.GetDimensions(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_GetClientPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.GetPosition(PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_Freeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.SetFrozen(True);
end;

procedure Lape_Unfreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.SetFrozen(False);
end;

procedure Lape_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.ActivateClient();
end;

procedure Lape_IsTargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.TargetValid();
end;

procedure Lape_GetNativeWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := TMMLScriptThread(Params^[0]).Client.IOManager.GetImageTarget.Handle;
end;

procedure Lape_SaveScreenshot(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  BMP: TMufasaBitmap;
  W, H: Int32;
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    IOManager.GetDimensions(W, H);

    BMP := TMufasaBitmap.Create();
    BMP.CopyClientToBitmap(IOManager, True, 0, 0, W-1, H-1);

    try
      BMP.SaveToFile(PString(Params^[1])^);
    finally
      BMP.Free();
    end;
  end;
end;

procedure Lape_SetAutoFocus(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.IOManager.AutoFocus := PBoolean(Params^[1])^;
end;

procedure Lape_Import_Window(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('function GetProcessID: SizeUInt', @Lape_GetProcessID, Data);
    addGlobalMethod('procedure SetTarget(Proc: TSysProc);', @Lape_SetTarget, Data);
    addGlobalMethod('procedure SetDesktopAsClient;', @Lape_SetDesktopAsClient, Data);
    addGlobalMethod('function SetTargetArray(P: PtrUInt; W, H: Int32): Int32', @Lape_SetTargetArray, Data);
    addGlobalMethod('function SetTargetBitmap(bitmap: Int32): Int32', @Lape_SetTargetBitmap, Data);
    addGlobalMethod('function SetEIOSTarget(name, args: String): Int32', @Lape_SetEIOSTarget, Data);
    addGlobalMethod('function MouseSetClientArea(x1, y1, x2, y2: Int32): Boolean;', @Lape_MouseSetClientArea, Data);
    addGlobalMethod('procedure MouseResetClientArea;', @Lape_MouseResetClientArea, Data);
    addGlobalMethod('function ImageSetClientArea(x1, y1, x2, y2: Int32): Boolean;', @Lape_ImageSetClientArea, Data);
    addGlobalMethod('procedure ImageResetClientArea;', @Lape_ImageResetClientArea, Data);
    addGlobalMethod('procedure SetImageTarget(idx: Int32);', @Lape_SetImageTarget, Data);
    addGlobalMethod('procedure SetKeyMouseTarget(idx: Int32);', @Lape_SetKeyMouseTarget, Data);
    addGlobalMethod('function GetImageTarget: Int32', @Lape_GetImageTarget, Data);
    addGlobalMethod('function GetKeyMouseTarget: Int32', @Lape_GetKeyMouseTarget, Data);
    addGlobalMethod('function ExportImageTarget: TTarget_Exported', @Lape_ExportImageTarget, Data);
    addGlobalMethod('function ExportKeyMouseTarget: TTarget_Exported', @Lape_ExportKeyMouseTarget, Data);
    addGlobalMethod('procedure FreeTarget(idx: Int32);', @Lape_FreeTarget, Data);
    addGlobalMethod('procedure GetClientDimensions(var W, H: Int32);', @Lape_GetClientDimensions, Data);
    addGlobalMethod('procedure GetClientPosition(var Left, Top: Int32);', @Lape_GetClientPosition, Data);
    addGlobalMethod('procedure Freeze;', @Lape_Freeze, Data);
    addGlobalMethod('procedure Unfreeze;', @Lape_Unfreeze, Data);
    addGlobalMethod('procedure ActivateClient;', @Lape_ActivateClient, Data);
    addGlobalMethod('function IsTargetValid: Boolean', @Lape_IsTargetValid, Data);
    addGlobalMethod('function GetNativeWindow: PtrUInt;', @Lape_GetNativeWindow, Data);
    addGlobalMethod('procedure SaveScreenshot(FileName: string);', @Lape_SaveScreenshot, Data);
    addGlobalMethod('procedure SetAutoFocus(Value: Boolean)', @Lape_SetAutoFocus, Data);
  end;
end;

initialization
  ScriptImports.Add('Target', @Lape_Import_Window);

end.

