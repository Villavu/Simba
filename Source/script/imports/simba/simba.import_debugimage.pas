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
procedure Show(Quads: TQuadArray; Filled: Boolean = False);
*)

(*
Show
~~~~
procedure Show(Quad: TQuad; Filled: Boolean = False);
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
procedure ShowOnClient(Quads: TQuadArray; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
procedure ShowOnClient(Quad: TQuad; Filled: Boolean = False);
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
Show
~~~~
procedure Show(Bitmap: TSimbaImage; EnsureVisible: Boolean = True);
*)
procedure _LapeShowBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowBitmap requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Show(PSimbaImage(Params^[0])^, PBoolean(Params^[1])^);
end;

(*
UpdateDebugImage
~~~~~~~~~~~~~~~~
procedure UpdateDebugImage(Bitmap: TSimbaImage);
*)
procedure _LapeUpdateDebugImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('DrawBitmapDebugImg requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Update(PSimbaImage(Params^[0])^);
end;

(*
ShowDebugImage
~~~~~~~~~~~~~~
procedure ShowDebugImage(Width, Height: Integer);
*)
procedure _LapeShowDebugImage1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowDebugImage requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Display(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
ShowDebugImage
~~~~~~~~~~~~~~
procedure ShowDebugImage(X, Y,Width, Height: Integer);
*)
procedure _LapeShowDebugImage2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('ShowDebugImage requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Display(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
SetDebugImageMaxSize
~~~~~~~~~~~~~~~~~~~~
procedure SetDebugImageMaxSize(MaxWidth, MaxHeight: Integer);
*)
procedure _LapeSetDebugImageMaxSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('SetDebugImgMaxSize requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_SetMaxSize(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

(*
HideDebugImage
~~~~~~~~~~~~~~
procedure HideDebugImage;
*)
procedure _LapeHideDebugImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('HideDebugImage requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.DebugImage_Hide();
end;

procedure ImportDebugImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Debug Image';

    addGlobalFunc('procedure SetDebugImageMaxSize(MaxWidth, MaxHeight: Integer)', @_LapeSetDebugImageMaxSize);
    addGlobalFunc('procedure ShowDebugImage(Width, Height: Integer); overload;', @_LapeShowDebugImage1);
    addGlobalFunc('procedure ShowDebugImage(X, Y, Width, Height: Integer); overload;', @_LapeShowDebugImage2);
    addGlobalFunc('procedure HideDebugImage;', @_LapeHideDebugImage);
    addGlobalFunc('procedure UpdateDebugImage(Bitmap: TSimbaImage);', @_LapeUpdateDebugImage);
    addGlobalFunc('procedure Show(Bitmap: TSimbaImage; EnsureVisible: Boolean = True);', @_LapeShowBitmap);

    ImportingSection := 'TSimbaImage';

    addGlobalFunc(
      'procedure TSimbaImage.Show(EnsureVisible: Boolean = True);', [
      'begin',
      '  Show(Self, EnsureVisible);',
      'end;'
    ]);

    ImportingSection := 'Debug Image';

    addGlobalFunc(
      'procedure Show(Matrix: TIntegerMatrix); overload;', [
      'begin',
      '  with TSimbaImage.Create(Matrix.Width(), Matrix.Height()) do',
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
      '  with TSimbaImage.Create(Matrix.Width(), Matrix.Height()) do',
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
      '    with TSimbaImage.Create(X1+X2+1, Y1+Y2+1) do',
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
      '  with TPA.Bounds() do',
      '    with TSimbaImage.Create(X1+X2+1, Y1+Y2+1) do',
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
      '  with ATPA.Bounds() do',
      '    with TSimbaImage.Create(X1+X2+1, Y1+Y2+1) do',
      '    try',
      '      DrawATPA(ATPA);',
      '      Show();',
      '    finally',
      '      Free();',
      '    end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Quads: TQuadArray; Filled: Boolean = False); overload;', [
      'var',
      '  Boxes: TBoxArray;',
      '  Quad: TQuad;',
      'begin',
      '  for Quad in Quads do',
      '    Boxes += Quad.Bounds();',
      '',
      '  with Boxes.Merge() do',
      '    with TSimbaImage.Create(X1+X2+1, Y1+Y2+1) do',
      '    try',
      '      DrawQuadArray(Quads, Filled);',
      '      Show();',
      '    finally',
      '      Free();',
      '    end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Quad: TQuad; Filled: Boolean = False); overload;', [
      'begin',
      '  Show(TQuadArray([Quad]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(Quads: TQuadArray; Filled: Boolean = False); overload;', [
      'begin',
      '  with TSimbaImage.CreateFromTarget() do',
      '  try',
      '    DrawQuadArray(Quads, Filled);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(Quad: TQuad; Filled: Boolean = False); overload;', [
      'begin',
      '  ShowOnClient(TQuadArray([Quad]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnClient(Boxes: TBoxArray; Filled: Boolean = False); overload;', [
      'begin',
      '  with TSimbaImage.CreateFromTarget() do',
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
      '  with TSimbaImage.CreateFromTarget() do',
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
      '  with TSimbaImage.CreateFromTarget() do',
      '  try',
      '    DrawATPA(ATPA);',
      '    Show();',
      '  finally',
      '    Free();',
      '  end;',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDebugImage);

end.

