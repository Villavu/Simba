unit simbascript.import_target;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

implementation

uses
  simba.iomanager, simba.bitmap, simba.target, simba.target_exported;

procedure Lape_GetProcessID(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSizeUInt(Result)^ := GetProcessID();
end;

procedure Lape_SetTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.SetTargetEx(PSysProc(Params^[0])^);
end;

procedure Lape_SetDesktopAsClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.SetDesktop();
end;

procedure Lape_SetTargetArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.IOManager.SetTarget(PRGB32(PPtrUInt(Params^[0])^), Point(PInt32(Params^[1])^, PInt32(Params^[2])^));
end;

procedure Lape_SetTargetBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with Script.Client do
    PInt32(Result)^ := Script.Client.IOManager.SetTarget(MBitmaps[PInt32(Params^[0])^]);
end;

procedure Lape_SetEIOSTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := Script.Client.IOManager.SetTarget(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_MouseSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := Script.Client.IOManager.MouseSetClientArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_MouseResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.MouseResetClientArea();
end;

procedure Lape_ImageSetClientArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := Script.Client.IOManager.ImageSetClientArea(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Lape_ImageResetClientArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.ImageResetClientArea();
end;

procedure Lape_SetImageTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
   Script.Client.IOManager.SetImageTarget(PInt32(Params^[0])^);
end;

procedure Lape_SetKeyMouseTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.SetKeyMouseTarget(PInt32(Params^[0])^);
end;

procedure Lape_GetImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.GetImageTarget(PInt32(Result)^);
end;

procedure Lape_GetKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.GetKeyMouseTarget(PInt32(Result)^);
end;

procedure Lape_ExportImageTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := Script.Client.IOManager.ExportImageTarget();
end;

procedure Lape_ExportKeyMouseTarget(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTarget_Exported(Result)^ := Script.Client.IOManager.ExportKeyMouseTarget();
end;

procedure Lape_FreeTarget(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.FreeTarget(PInt32(Params^[0])^);
end;

procedure Lape_GetClientDimensions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.GetDimensions(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_GetClientPosition(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.GetPosition(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Lape_Freeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.SetFrozen(True);
end;

procedure Lape_Unfreeze(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.SetFrozen(False);
end;

procedure Lape_ActivateClient(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Script.Client.IOManager.ActivateClient();
end;

procedure Lape_IsTargetValid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := Script.Client.IOManager.TargetValid();
end;

procedure Lape_GetNativeWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := Script.Client.IOManager.GetImageTarget.Handle;
end;

procedure Lape_SaveScreenshot(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  BMP: TMufasaBitmap;
  W, H: Int32;
begin
  with Script.Client do
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

procedure Lape_Import_Window(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    Section := 'Target';

    addGlobalFunc('function GetProcessID: SizeUInt', @Lape_GetProcessID);
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
  end;
end;

initialization
  RegisterScriptImport(@Lape_Import_Window);

end.
