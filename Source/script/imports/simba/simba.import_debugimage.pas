unit simba.import_debugimage;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.scriptthread, simba.bitmap;

(*
Debug Image
===========
| The debug image is a simba sided window which can be drawn on to visually debug.
| As the window is simba sided the image will remain after the script terminates.
*)

(*
Show
~~~~
procedure Show(Matrix: TIntegerMatrix);
*)

(*
Show
~~~~
procedure Show(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
*)

(*
Show
~~~~
procedure Show(Boxes: TBoxArray; Filled: Boolean = False);
*)

(*
Show
~~~~
procedure Show(Box: TBox; Filled: Boolean = False);
*)

(*
Show
~~~~
procedure Show(TPA: TPointArray; Color: Integer = $0000FF);
*)

(*
Show
~~~~
procedure Show(ATPA: T2DPointArray; Color: Integer = $0000FF);
*)

(*
ShowOnClient
~~~~~~~~~~~~
procedure ShowOnClient(Boxes: TBoxArray; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
procedure ShowOnClient(Box: TBox; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
procedure ShowOnClient(TPA: TPointArray; Color: Integer = $0000FF);
*)

(*
ShowOnClient
~~~~~~~~~~~~
procedure ShowOnClient(ATPA: T2DPointArray; Color: Integer = $0000FF);
*)

(*
ShowBitmap
~~~~~~~~~~
procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True); override;
*)
procedure _LapeShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowBitmap requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Show(PMufasaBitmap(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
DrawBitmapDebugImg
~~~~~~~~~~~~~~~~~~
procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap);
*)
procedure _LapeDrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DrawBitmapDebugImg requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Draw(PMufasaBitmap(Params^[0])^);
end;

(*
DisplayDebugImgWindow
~~~~~~~~~~~~~~~~~~~~~
procedure DisplayDebugImgWindow(Width, Height: Integer);
*)
procedure _LapeDisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DisplayDebugImgWindow requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Display(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
ClearDebugImg
~~~~~~~~~~~~~
procedure ClearDebugImg;
*)
procedure _LapeClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ClearDebugImage requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_Clear();
end;

(*
SetDebugImgMaxSize
~~~~~~~~~~~~~~~~~~
procedure SetDebugImgMaxSize(MaxWidth, MaxHeight: Integer);
*)
procedure _LapeSetDebugImgMaxSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('SetDebugImgMaxSize requires Simba communication');

  with SimbaScriptThread.Script.Client do
    SimbaScriptThread.Script.SimbaCommunication.DebugImage_SetMaxSize(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
HideDebugImg
~~~~~~~~~~~~
procedure HideDebugImg;
*)
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
    pushSection('https://villavu.github.io/Simba/debug image.html');

    addGlobalFunc('procedure ShowBitmap(Bitmap: TMufasaBitmap; EnsureVisible: Boolean = True); override', @_LapeShowBitmap);
    addGlobalFunc('procedure DrawBitmapDebugImg(Bitmap: TMufasaBitmap); overload', @_LapeDrawBitmapDebugImg);
    addGlobalFunc('procedure DisplayDebugImgWindow(Width, Height: Integer)', @_LapeDisplayDebugImgWindow);
    addGlobalFunc('procedure SetDebugImgMaxSize(MaxWidth, MaxHeight: Integer)', @_LapeSetDebugImgMaxSize);
    addGlobalFunc('procedure HideDebugImg', @_LapeHideDebugImg);
    addGlobalFunc('procedure ClearDebugImg', @_LapeClearDebugImg);

    addGlobalFunc(
      'procedure Show(Matrix: TIntegerMatrix); overload;', [
      'begin',
      '  with TMufasaBitmap.Create(Matrix.Width(), Matrix.Height()) do',
      '  try',
      '    DrawMatrix(Matrix);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Matrix: TSingleMatrix; ColorMapID: Integer = 0); overload;', [
      'begin',
      '  with TMufasaBitmap.Create(Matrix.Width(), Matrix.Height()) do',
      '  try',
      '    DrawMatrix(Matrix, ColorMapID);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Boxes: TBoxArray; Filled: Boolean = False); overload;', [
      'begin',
      '  with Boxes.Merge() do',
      '    with TMufasaBitmap.Create(X1+X2+1, Y1+Y2+1) do',
      '    try',
      '      DrawBoxArray(Boxes, Filled);',
      '      Show();',
      '    finally',
      '      Free();',
      '    end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Box: TBox; Filled: Boolean = False); overload;', [
      'begin',
      '  Show(TBoxArray([Box]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(TPA: TPointArray; Color: Integer = $0000FF); overload;', [
      'begin',
      '  with GetTPABounds(TPA) do',
      '    with TMufasaBitmap.Create(X1+X2+1, Y1+Y2+1) do',
      '    try',
      '      DrawTPA(TPA, Color);',
      '      Show();',
      '    finally',
      '      Free();',
      '    end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(ATPA: T2DPointArray; Color: Integer = $0000FF); overload;', [
      'begin',
      '  with GetATPABounds(ATPA) do',
      '    with TMufasaBitmap.Create(X1+X2+1, Y1+Y2+1) do',
      '    try',
      '      DrawATPA(ATPA);',
      '      Show();',
      '    finally',
      '      Free();',
      '    end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(Boxes: TBoxArray; Filled: Boolean = False); overload;', [
      'begin',
      '  with TMufasaBitmap.CreateFromClient() do',
      '  try',
      '    DrawBoxArray(Boxes, Filled);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(Box: TBox; Filled: Boolean = False); overload;', [
      'begin',
      '  ShowOnClient(TBoxArray([Box]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(TPA: TPointArray; Color: Integer = $0000FF); overload;', [
      'begin',
      '  with TMufasaBitmap.CreateFromClient() do',
      '  try',
      '    DrawTPA(TPA, Color);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(ATPA: T2DPointArray; Color: Integer = $0000FF); overload;', [
      'begin',
      '  with TMufasaBitmap.CreateFromClient() do',
      '  try',
      '    DrawATPA(ATPA);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDebugImage);

end.

