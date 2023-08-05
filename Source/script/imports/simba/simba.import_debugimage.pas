unit simba.import_debugimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportDebugImage(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes;

(*
Debug Image
===========
| The debug image is a simba sided window which can be drawn on to visually debug.
| As the window is simba sided the image will remain after the script terminates.
*)

(*
Show
~~~~
> procedure Show(Matrix: TIntegerMatrix);
*)

(*
Show
~~~~
> procedure Show(Matrix: TSingleMatrix; ColorMapID: Integer = 0);
*)

(*
Show
~~~~
> procedure Show(Boxes: TBoxArray; Filled: Boolean = False);
*)

(*
Show
~~~~
> procedure Show(Box: TBox; Filled: Boolean = False);
*)

(*
Show
~~~~
> procedure Show(Quads: TQuadArray; Filled: Boolean = False);
*)

(*
Show
~~~~
> procedure Show(Quad: TQuad; Filled: Boolean = False);
*)

(*
Show
~~~~
> procedure Show(TPA: TPointArray; Color: Integer = $0000FF);
*)

(*
Show
~~~~
> procedure Show(ATPA: T2DPointArray; Color: Integer = $0000FF);
*)

(*
ShowOnClient
~~~~~~~~~~~~
> procedure ShowOnClient(Boxes: TBoxArray; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
> procedure ShowOnClient(Box: TBox; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
> procedure ShowOnClient(Quads: TQuadArray; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
> procedure ShowOnClient(Quad: TQuad; Filled: Boolean = False);
*)

(*
ShowOnClient
~~~~~~~~~~~~
> procedure ShowOnClient(TPA: TPointArray; Color: Integer = $0000FF);
*)

(*
ShowOnClient
~~~~~~~~~~~~
> procedure ShowOnClient(ATPA: T2DPointArray; Color: Integer = $0000FF);
*)

(*
Show
~~~~
> procedure Show(Bitmap: TSimbaImage; EnsureVisible: Boolean = True);
*)

(*
DebugImageUpdate
~~~~~~~~~~~~~~~~
> procedure DebugImageUpdate(Bitmap: TSimbaImage);
*)

(*
DebugImageDisplay
~~~~~~~~~~~~~~~~~
> procedure DebugImageDisplay(Width, Height: Integer);
*)

(*
DebugImageDisplay
~~~~~~~~~~~~~~~~~
> procedure DebugImageDisplay(X, Y,Width, Height: Integer);
*)

(*
DebugImageClose
~~~~~~~~~~~~~~~
> procedure DebugImageClose;
*)

(*
DebugImageSetMaxSize
~~~~~~~~~~~~~~~~~~~~
> procedure DebugImageSetMaxSize(MaxWidth, MaxHeight: Integer);
*)

(*
DebugImageShow
~~~~~~~~~~~~~~
> procedure DebugImageShow(Image: TImage; EnsureVisible: Boolean = True);
*)

procedure ImportDebugImage(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Debug Image';

    addGlobalFunc(
      'procedure DebugImageSetMaxSize(MaxWidth, MaxHeight: Integer);', [
      'begin',
      '  _SimbaScript.DebugImage_SetMaxSize(MaxWidth, MaxHeight);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure DebugImageDisplay(Width, Height: Integer); overload;', [
      'begin',
      '  _SimbaScript.DebugImage_Display(Width, Height);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure DebugImageDisplay(X, Y, Width, Height: Integer); overload;', [
      'begin',
      '  _SimbaScript.DebugImage_Display(X, Y, Width, Height);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure DebugImageClose;', [
      'begin',
      '  _SimbaScript.DebugImage_Hide();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure DebugImageUpdate(Image: TImage);', [
      'begin',
      '  _SimbaScript.DebugImage_Update(Image);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure DebugImageShow(Image: TImage; EnsureVisible: Boolean = True);', [
      'begin',
      '  _SimbaScript.DebugImage_Show(Image, EnsureVisible);',
      'end;'
    ]);

    ImportingSection := 'Image';

    addGlobalFunc(
      'procedure TImage.Show(EnsureVisible: Boolean = True);', [
      'begin',
      '  _SimbaScript.DebugImage_Show(Self, EnsureVisible);',
      'end;'
    ]);

    ImportingSection := 'Debug Image';

    addGlobalFunc(
      'procedure Show(Matrix: TIntegerMatrix); overload;', [
      'begin',
      '  with TImage.Create(Matrix.Width(), Matrix.Height()) do',
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
      '  with TImage.Create(Matrix.Width(), Matrix.Height()) do',
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
      '    with TImage.Create(X1+X2+1, Y1+Y2+1) do',
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
      '    with TImage.Create(X1+X2+1, Y1+Y2+1) do',
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
      '    with TImage.Create(X1+X2+1, Y1+Y2+1) do',
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
      '    with TImage.Create(X1+X2+1, Y1+Y2+1) do',
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
      '  with TImage.CreateFromTarget() do',
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
      '  with TImage.CreateFromTarget() do',
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
      '  with TImage.CreateFromTarget() do',
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
      '  with TImage.CreateFromTarget() do',
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

end.
