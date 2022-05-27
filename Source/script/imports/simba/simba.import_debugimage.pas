unit simba.import_debugimage;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.scriptthread, simba.bitmap;

procedure _LapeShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowBitmap requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Show(PMufasaBitmap(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeDrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DrawBitmapDebugImg requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Draw(PMufasaBitmap(Params^[0])^);
end;

procedure _LapeDisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DisplayDebugImgWindow requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Display(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ClearDebugImage requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Clear();
end;

procedure _LapeSetDebugImgMaxSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('SetDebugImgMaxSize requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_SetMaxSize(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeHideDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('HideDebugImg requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Hide();
end;

procedure ImportDebugImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Debug Image');

    addGlobalFunc('procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True); override', @_LapeShowBitmap);
    addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap); overload', @_LapeDrawBitmapDebugImg);
    addGlobalFunc('procedure DisplayDebugImgWindow(Width, Height: Integer)', @_LapeDisplayDebugImgWindow);
    addGlobalFunc('procedure SetDebugImgMaxSize(MaxWidth, MaxHeight: Integer)', @_LapeSetDebugImgMaxSize);
    addGlobalFunc('procedure HideDebugImg', @_LapeHideDebugImg);
    addGlobalFunc('procedure ClearDebugImg', @_LapeClearDebugImg);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDebugImage);

end.

