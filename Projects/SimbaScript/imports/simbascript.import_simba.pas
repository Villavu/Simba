unit simbascript.import_simba;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Simba(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  extctrls, simba.bitmap, simba.script_common, simba.oswindow;

procedure Lape_Status(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
  Status: ShortString;
begin
  Status := PString(Params^[1])^;

  Method := TSimbaMethod.Create(SIMBA_METHOD_STATUS);
  Method.Params.Write(Status, SizeOf(ShortString));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_ShowBalloonHint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
  Title, Hint: ShortString;
begin
  Title := PString(Params^[1])^;
  Hint := PString(Params^[2])^;

  Method := TSimbaMethod.Create(SIMBA_METHOD_BALLOON_HINT);
  Method.Params.Write(Title, SizeOf(ShortString));
  Method.Params.Write(Hint, SizeOf(ShortString));
  Method.Params.Write(PInt32(Params^[3])^, SizeOf(Int32));
  Method.Params.Write(PInt32(Params^[4])^, SizeOf(Int32));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_ClearDebug(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod.Create(SIMBA_METHOD_CLEAR_DEBUG);
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_Disguise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
  Title: ShortString;
begin
  Title := PString(Params^[1])^;

  Method := TSimbaMethod.Create(SIMBA_METHOD_DISGUISE);
  Method.Params.Write(Title, SizeOf(ShortString));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_ShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  Bitmap := PMufasaBitmap(Params^[1])^;

  Method := TSimbaMethod.Create(SIMBA_METHOD_DEBUG_IMAGE);
  Method.Params.Write(Bitmap.Width, SizeOf(Int32));
  Method.Params.Write(Bitmap.Height, SizeOf(Int32));
  Method.Params.Write(Bitmap.FData^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_ShowBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  with TSimbaScript(Params^[0]).Client do
    Bitmap := MBitmaps[PInt32(Params^[1])^];

  Method := TSimbaMethod.Create(SIMBA_METHOD_DEBUG_IMAGE);
  Method.Params.Write(Bitmap.Width, SizeOf(Int32));
  Method.Params.Write(Bitmap.Height, SizeOf(Int32));
  Method.Params.Write(Bitmap.FData^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_DisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod.Create(SIMBA_METHOD_DEBUG_IMAGE_DISPLAY);
  Method.Params.Write(PInt32(Params^[1])^, SizeOf(Int32));
  Method.Params.Write(PInt32(Params^[2])^, SizeOf(Int32));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_DrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Method: TSimbaMethod;
begin
  with TSimbaScript(Params^[0]).Client do
    Bitmap := MBitmaps[PInt32(Params^[1])^];

  Method := TSimbaMethod.Create(SIMBA_METHOD_DEBUG_IMAGE_DRAW);
  Method.Params.Write(Bitmap.Width, SizeOf(Int32));
  Method.Params.Write(Bitmap.Height, SizeOf(Int32));
  Method.Params.Write(Bitmap.FData^, Bitmap.Width * Bitmap.Height * SizeOf(TRGB32));
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_GetDebugBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Bitmap: TMufasaBitmap;
  Width, Height: Int32;
  Method: TSimbaMethod;
begin
  with TSimbaScript(Params^[0]).Client do
    Bitmap := MBitmaps[MBitmaps.CreateBMP(0, 0)];

  Method := TSimbaMethod.Create(SIMBA_METHOD_DEBUG_IMAGE_GET);
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Result.Read(Width, SizeOf(Int32));
  Method.Result.Read(Height, SizeOf(Int32));

  Bitmap.LoadFromMemory(PRGB32(Method.Result.Memory + Method.Result.Position), Width, Height);

  Method.Free();

  PInt32(Result)^ := Bitmap.Index;
end;

procedure Lape_ClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod.Create(SIMBA_METHOD_DEBUG_IMAGE_CLEAR);
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Free();
end;

procedure Lape_GetSimbaPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod.Create(SIMBA_METHOD_GET_PID);
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Result.Read(PPtrUInt(Result)^, SizeOf(PtrUInt));
  Method.Free();
end;

procedure Lape_GetSimbaTargetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod.Create(SIMBA_METHOD_GET_TARGET_PID);
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Result.Read(PUInt32(Result)^, SizeOf(UInt32));
  Method.Free();
end;

procedure Lape_GetSimbaTargetWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Method: TSimbaMethod;
begin
  Method := TSimbaMethod.Create(SIMBA_METHOD_GET_TARGET_WINDOW);
  Method.Invoke(TSimbaScript(Params^[0]));
  Method.Result.Read(POSWindow(Result)^, SizeOf(TOSWindow));
  Method.Free();
end;

procedure Lape_Import_Simba(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Simba';

    addGlobalType('(bfNone, bfInfo, bfWarning, bfError)', 'TBalloonFlags');

    addGlobalMethod('procedure Status(const Status: String);', @Lape_Status, Data);
    addGlobalMethod('procedure ShowBalloonHint(const Title, Hint: String; const Timeout: Int32; const Flags: TBalloonFlags);', @Lape_ShowBalloonHint, Data);
    addGlobalMethod('procedure ClearDebug;', @Lape_ClearDebug, Data);
    addGlobalMethod('procedure Disguise(const Caption: String);', @Lape_Disguise, Data);
    addGlobalMethod('procedure ShowBitmap(BMP: TMufasaBitmap); overload;', @Lape_ShowBitmap, Data);
    addGlobalMethod('procedure ShowBitmap(BMP: Int32); overload;', @Lape_ShowBitmapEx, Data);
    addGlobalMethod('procedure DisplayDebugImgWindow(W, H: Int32);', @Lape_DisplayDebugImgWindow, Data);
    addGlobalMethod('procedure DrawBitmapDebugImg(BMP: Int32);', @Lape_DrawBitmapDebugImg, Data);
    addGlobalMethod('function GetDebugBitmap: Int32;', @Lape_GetDebugBitmap, Data);
    addGlobalMethod('procedure ClearDebugImg;', @Lape_ClearDebugImg, Data);
    addGlobalMethod('function GetSimbaPID: PtrUInt;', @Lape_GetSimbaPID, Data);
    addGlobalMethod('function GetSimbaTargetPID: UInt32;', @Lape_GetSimbaTargetPID, Data);
    addGlobalMethod('function GetSimbaTargetWindow: TOSWindow;', @Lape_GetSimbaTargetWindow, Data);
  end;
end;

end.

