unit simba.import_debugimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportDebugImage(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.script_objectutil,
  simba.script_communication;

(*
Debug Image
===========
| The debug image is a simba sided window which can be drawn on to visually debug.
| As the window is simba sided the image will remain after the script terminates.
*)

(*
Show
----
```
procedure Show(Matrix: TIntegerMatrix);
```
*)

(*
Show
----
```
procedure Show(Matrix: TSingleMatrix; ColorMapType: Integer = 0; EnsureVisible: Boolean = True);
```
*)

(*
Show
----
```
procedure Show(Boxes: TBoxArray; Filled: Boolean = False);
```
*)

(*
Show
----
```
procedure Show(Box: TBox; Filled: Boolean = False);
```
*)

(*
Show
----
```
procedure Show(TPA: TPointArray);
```
*)

(*
Show
----
```
procedure Show(ATPA: T2DPointArray);
```
*)

(*
Show
----
```
procedure Show(Quads: TQuadArray; Filled: Boolean = False);
```
*)

(*
Show
----
```
procedure Show(Quad: TQuad; Filled: Boolean = False);
```
*)

(*
ShowOnTarget
------------
```
procedure ShowOnTarget(Boxes: TBoxArray; Filled: Boolean = False);
```
*)

(*
ShowOnTarget
------------
```
procedure ShowOnTarget(Box: TBox; Filled: Boolean = False);
```
*)

(*
ShowOnTarget
------------
```
procedure ShowOnTarget(TPA: TPointArray);
```
*)

(*
ShowOnTarget
------------
```
procedure ShowOnTarget(ATPA: T2DPointArray);
```
*)

(*
ShowOnTarget
------------
```
procedure ShowOnTarget(Quads: TQuadArray; Filled: Boolean = False);
```
*)

(*
ShowOnTarget
------------
```
procedure ShowOnTarget(Quad: TQuad; Filled: Boolean = False);
```
*)

function Communication(const Params: PParamArray): TSimbaScriptCommunication;
begin
  Result := TSimbaScript(Params^[0]).SimbaCommunication;
  if (Result = nil) then
    SimbaException('DebugImage requires Simba communication');
end;

(*
DebugImageUpdate
----------------
```
procedure DebugImageUpdate(Image: TImage);
```
*)
procedure _LapeDebugImage_Update(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugImage_Update(PLapeObjectImage(Params^[1])^^, False, False);
end;

(*
DebugImageShow
--------------
```
procedure DebugImageShow(Image: TImage; EnsureVisible: Boolean = True);
```
*)
procedure _LapeDebugImage_Show(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugImage_Update(PLapeObjectImage(Params^[1])^^, True, PBoolean(Params^[2])^);
end;

(*
DebugImageDisplay
-----------------
```
procedure DebugImageDisplay(Width, Height: Integer);
```
*)
procedure _LapeDebugImage_Display1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugImage_Display(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
DebugImageDisplay
-----------------
```
procedure DebugImageDisplay(X, Y, Width, Height: Integer);
```
*)
procedure _LapeDebugImage_Display2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugImage_Display(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

(*
DebugImageSetMaxSize
--------------------
```
procedure DebugImageSetMaxSize(MaxWidth, MaxHeight: Integer);
```
*)
procedure _LapeDebugImage_SetMaxSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugImage_SetMaxSize(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
DebugImageClose
---------------
```
procedure DebugImageClose;
```
*)
procedure _LapeDebugImage_Close(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugImage_Close();
end;

(*
DebugMatrixUpdate
-----------------
```
procedure DebugMatrixUpdate(Matrix: TSingleMatrix; ColorMapType: Integer = 0);
```
*)
procedure _LapeDebugMatrix_Update(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugMatrix_Update(TSingleMatrix(Params^[1]^), PInteger(Params^[2])^, False, False);
end;

(*
DebugMatrixShow
---------------
```
procedure DebugMatrixShow(Matrix: TSingleMatrix; ColorMapType: Integer = 0; EnsureVisible: Boolean = True);
```
*)
procedure _LapeDebugMatrix_Show(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  Communication(Params).DebugMatrix_Update(TSingleMatrix(Params^[1]^), PInteger(Params^[2])^, True, PBoolean(Params^[3])^);
end;

procedure ImportDebugImage(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Debug Image';

    addGlobalMethod('procedure DebugImageUpdate(Image: TImage)', @_LapeDebugImage_Update, Script);
    addGlobalMethod('procedure DebugImageShow(Image: TImage; EnsureVisible: Boolean = True)', @_LapeDebugImage_Show, Script);
    addGlobalMethod('procedure DebugImageDisplay(Width, Height: Integer); overload', @_LapeDebugImage_Display1, Script);
    addGlobalMethod('procedure DebugImageDisplay(X, Y, Width, Height: Integer); overload', @_LapeDebugImage_Display2, Script);
    addGlobalMethod('procedure DebugImageSetMaxSize(MaxWidth, MaxHeight: Integer);', @_LapeDebugImage_SetMaxSize, Script);
    addGlobalMethod('procedure DebugImageClose', @_LapeDebugImage_Close, Script);

    addGlobalMethod('procedure DebugMatrixUpdate(Matrix: TSingleMatrix; ColorMapType: Integer = 0)', @_LapeDebugMatrix_Update, Script);
    addGlobalMethod('procedure DebugMatrixShow(Matrix: TSingleMatrix; ColorMapType: Integer = 0; EnsureVisible: Boolean = True)', @_LapeDebugMatrix_Show, Script);

    DumpSection := 'Image';

    addGlobalFunc(
      'procedure TImage.Show(EnsureVisible: Boolean = True);', [
      'begin',
      '  DebugImageShow(Self, EnsureVisible);',
      'end;'
    ]);

    DumpSection := 'Debug Image';

    addGlobalFunc(
      'procedure Show(Matrix: TIntegerMatrix); overload;', [
      'var img: TImage;',
      'begin',
      '  img := new TImage();',
      '  img.FromMatrix(Matrix);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Matrix: TSingleMatrix; ColorMapType: Integer = 0; EnsureVisible: Boolean = True); overload;', [
      'begin',
      '  DebugMatrixShow(Matrix, ColorMapType, EnsureVisible);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Boxes: TBoxArray; Filled: Boolean = False); overload;', [
      'var img: TImage;',
      'begin',
      '  with Boxes.Merge() do',
      '    img := new TImage(X1+Width, Y1+Height);',
      '  img.Canvas.DrawFilled := Filled;',
      '  img.Canvas.DrawBoxArray(Boxes);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Box: TBox; Filled: Boolean = False); overload;', [
      'begin',
      '  Show(TBoxArray([Box]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(TPA: TPointArray); overload;', [
      'var img: TImage;',
      'begin',
      '  with TPA.Bounds() do',
      '    img := new TImage(X1+Width, Y1+Height);',
      '  img.Canvas.DrawTPA(TPA);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(ATPA: T2DPointArray); overload;', [
      'var img: TImage;',
      'begin',
      '  with ATPA.Bounds() do',
      '    img := new TImage(X1+Width, Y1+Height);',
      '  img.Canvas.DrawATPA(ATPA);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Quads: TQuadArray; Filled: Boolean = False); overload;', [
      'var img: TImage;',
      'begin',
      '  with Quads.Merge().Bounds do',
      '    img := new TImage(X1+Width, Y1+Height);',
      '  img.Canvas.DrawFilled := Filled;',
      '  img.Canvas.DrawQuadArray(Quads);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure Show(Quad: TQuad; Filled: Boolean = False); overload;', [
      'begin',
      '  Show(TQuadArray([Quad]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnTarget(Boxes: TBoxArray; Filled: Boolean = False); overload;', [
      'var img: TImage;',
      'begin',
      '  img := Target.GetImage();',
      '  img.Canvas.DrawFilled := Filled;',
      '  img.Canvas.DrawBoxArray(Boxes);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnTarget(Box: TBox; Filled: Boolean = False); overload;', [
      'begin',
      '  ShowOnTarget(TBoxArray([Box]), Filled);',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnTarget(TPA: TPointArray); overload;', [
      'var img: TImage;',
      'begin',
      '  img := Target.GetImage();',
      '  img.Canvas.DrawTPA(TPA);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnTarget(ATPA: T2DPointArray); overload;', [
      'var img: TImage;',
      'begin',
      '  img := Target.GetImage();',
      '  img.Canvas.DrawATPA(ATPA);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnTarget(Quads: TQuadArray; Filled: Boolean = False); overload;', [
      'var img: TImage;',
      'begin',
      '  img := Target.GetImage();',
      '  img.Canvas.DrawFilled := Filled;',
      '  img.Canvas.DrawQuadArray(Quads);',
      '  img.Show();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure ShowOnTarget(Quad: TQuad; Filled: Boolean = False); overload;', [
      'begin',
      '  ShowOnTarget(TQuadArray([Quad]), Filled);',
      'end;'
    ]);

    DumpSection := '';
  end;
end;

end.
