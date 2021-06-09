unit simba.script_import_target;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Target(Compiler: TSimbaScript_Compiler);

implementation

uses
  simba.iomanager, simba.bitmap, simba.target, simba.target_exported, simba.oswindow;

procedure Lape_SetTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetTargetEx(PSysProc(Params^[0])^);
end;

procedure Lape_SetDesktopAsClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetDesktop();
end;

procedure Lape_SetTargetArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.IOManager.SetTarget(PRGB32(PPtrUInt(Params^[0])^), Point(PInt32(Params^[1])^, PInt32(Params^[2])^));
end;

procedure Lape_SetTargetBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := IOManager.SetTarget(MBitmaps[PInt32(Params^[0])^]);
end;

procedure Lape_SetEIOSTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.IOManager.SetTarget(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.IOManager.MouseSetClientArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.MouseResetClientArea();
end;

procedure Lape_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.IOManager.ImageSetClientArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ImageResetClientArea();
end;

procedure Lape_SetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   SimbaScript.Client.IOManager.SetImageTarget(PInt32(Params^[0])^);
end;

procedure Lape_SetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetKeyMouseTarget(PInt32(Params^[0])^);
end;

procedure Lape_GetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetImageTarget(PInt32(Result)^);
end;

procedure Lape_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetKeyMouseTarget(PInt32(Result)^);
end;

procedure Lape_ExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := SimbaScript.Client.IOManager.ExportImageTarget();
end;

procedure Lape_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := SimbaScript.Client.IOManager.ExportKeyMouseTarget();
end;

procedure Lape_FreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.FreeTarget(PInt32(Params^[0])^);
end;

procedure Lape_GetClientDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetDimensions(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_GetClientPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.GetPosition(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_Freeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetFrozen(True);
end;

procedure Lape_Unfreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.SetFrozen(False);
end;

procedure Lape_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.IOManager.ActivateClient();
end;

procedure Lape_IsTargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.IOManager.TargetValid();
end;

procedure Lape_GetNativeWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := SimbaScript.Client.IOManager.GetImageTarget.Handle;
end;

procedure Lape_SaveScreenshot(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
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

procedure Lape_GetTargetWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := SimbaScript.Client.IOManager.GetImageTarget().Handle;
end;

procedure Lape_GetTargetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := TOSWindow(SimbaScript.Client.IOManager.GetImageTarget().Handle).GetPID();
end;

procedure Lape_Import_Target(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'Target';

    addGlobalFunc('procedure SetTarget(Proc: TSysProc);', @Lape_SetTarget);
    addGlobalFunc('procedure SetDesktopAsClient;', @Lape_SetDesktopAsClient);
    addGlobalFunc('function SetTargetArray(P: PtrUInt; W, H: Int32): Int32', @Lape_SetTargetArray);
    addGlobalFunc('function SetTargetBitmap(bitmap: Int32): Int32', @Lape_SetTargetBitmap);
    addGlobalFunc('function SetEIOSTarget(name, args: String): Int32', @Lape_SetEIOSTarget);
    addGlobalFunc('function MouseSetClientArea(x1, y1, x2, y2: Int32): Boolean;', @Lape_MouseSetClientArea);
    addGlobalFunc('procedure MouseResetClientArea;', @Lape_MouseResetClientArea);
    addGlobalFunc('function ImageSetClientArea(x1, y1, x2, y2: Int32): Boolean;', @Lape_ImageSetClientArea);
    addGlobalFunc('procedure ImageResetClientArea;', @Lape_ImageResetClientArea);
    addGlobalFunc('procedure SetImageTarget(idx: Int32);', @Lape_SetImageTarget);
    addGlobalFunc('procedure SetKeyMouseTarget(idx: Int32);', @Lape_SetKeyMouseTarget);
    addGlobalFunc('function GetImageTarget: Int32', @Lape_GetImageTarget);
    addGlobalFunc('function GetKeyMouseTarget: Int32', @Lape_GetKeyMouseTarget);
    addGlobalFunc('function ExportImageTarget: TTarget_Exported', @Lape_ExportImageTarget);
    addGlobalFunc('function ExportKeyMouseTarget: TTarget_Exported', @Lape_ExportKeyMouseTarget);
    addGlobalFunc('procedure FreeTarget(idx: Int32);', @Lape_FreeTarget);
    addGlobalFunc('procedure GetClientDimensions(var W, H: Int32);', @Lape_GetClientDimensions);
    addGlobalFunc('procedure GetClientPosition(var Left, Top: Int32);', @Lape_GetClientPosition);
    addGlobalFunc('procedure Freeze;', @Lape_Freeze);
    addGlobalFunc('procedure Unfreeze;', @Lape_Unfreeze);
    addGlobalFunc('procedure ActivateClient;', @Lape_ActivateClient);
    addGlobalFunc('function IsTargetValid: Boolean', @Lape_IsTargetValid);
    addGlobalFunc('function GetNativeWindow: PtrUInt;', @Lape_GetNativeWindow);
    addGlobalFunc('procedure SaveScreenshot(FileName: string);', @Lape_SaveScreenshot);
    addGlobalFunc('function GetTargetWindow: TOSWindow;', @Lape_GetTargetWindow);
    addGlobalFunc('function GetTargetPID: UInt32;', @Lape_GetTargetPID);
  end;
end;

end.
