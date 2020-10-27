unit simba.script_import_simba;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Simba(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  extctrls,
  simba.bitmap, simba.script_communication;

procedure Lape_Status(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_Status.Create(PString(Params^[0])^);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_ShowBalloonHint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ShowBalloonHint.Create(PString(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^, TBalloonFlags(Params^[3]^));

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_ClearDebug(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ClearDebug.Create();

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_Disguise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_Disguse.Create(PString(Params^[0])^);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_ShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ShowBitmap.Create(PMufasaBitmap(Params^[0])^);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_ShowBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  with SimbaScript.Client do
    Bitmap := MBitmaps[PInt32(Params^[0])^];

  Method := TSimbaMethod_ShowBitmap.Create(Bitmap);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_DisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_DisplayDebugImage.Create(PInt32(Params^[0])^, PInt32(Params^[1])^);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_DrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_UpdateDebugImage.Create(PMufasaBitmap(Params^[0])^);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_DrawBitmapDebugImgEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  with SimbaScript.Client do
    Bitmap := MBitmaps[PInt32(Params^[0])^];

  Method := TSimbaMethod_UpdateDebugImage.Create(Bitmap);

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_ClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_ClearDebugImage.Create();

  try
    SimbaScript.Invoke(Method);
  finally
    Method.Free();
  end;
end;

procedure Lape_GetSimbaPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_GetSimbaPID.Create();

  try
    SimbaScript.Invoke(Method);

    Method.Result.Read(Result^, SizeOf(PtrUint));
  finally
    Method.Free();
  end;
end;

procedure Lape_GetSimbaTargetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_GetSimbaTargetPID.Create();

  try
    SimbaScript.Invoke(Method);

    Method.Result.Read(Result^, SizeOf(PtrUInt));
  finally
    Method.Free();
  end;
end;

procedure Lape_GetSimbaTargetWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod_GetSimbaTargetWindow.Create();

  try
    SimbaScript.Invoke(Method);

    Method.Result.Read(Result^, SizeOf(PtrUInt));
  finally
    Method.Free();
  end;
end;

procedure Lape_Import_Simba(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Simba';

    addGlobalType('(bfNone, bfInfo, bfWarning, bfError)', 'TBalloonFlags');

    addGlobalFunc('procedure Status(const Status: String);', @Lape_Status);
    addGlobalFunc('procedure ShowBalloonHint(const Title, Hint: String; const Timeout: Int32; const Flags: TBalloonFlags);', @Lape_ShowBalloonHint);
    addGlobalFunc('procedure ClearDebug;', @Lape_ClearDebug);
    addGlobalFunc('procedure Disguise(const Caption: String);', @Lape_Disguise);
    addGlobalFunc('procedure ShowBitmap(Bitmap: TMufasaBitmap); overload;', @Lape_ShowBitmap);
    addGlobalFunc('procedure ShowBitmap(Bitmap: Int32); overload;', @Lape_ShowBitmapEx);
    addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap); overload;', @Lape_DrawBitmapDebugImg);
    addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: Int32); overload;', @Lape_DrawBitmapDebugImgEx);
    addGlobalFunc('procedure DisplayDebugImgWindow(W, H: Int32);', @Lape_DisplayDebugImgWindow);
    addGlobalFunc('procedure ClearDebugImg;', @Lape_ClearDebugImg);
    addGlobalFunc('function GetSimbaPID: PtrUInt;', @Lape_GetSimbaPID);
    addGlobalFunc('function GetSimbaTargetPID: PtrUInt;', @Lape_GetSimbaTargetPID);
    addGlobalFunc('function GetSimbaTargetWindow: TOSWindow;', @Lape_GetSimbaTargetWindow);
  end;
end;

end.



