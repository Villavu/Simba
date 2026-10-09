unit simba.import_externalcanvas;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportExternalCanvas(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.canvas_external, simba.script_objectutil;

type
  PSimbaExternalCanvas = ^TSimbaExternalCanvas;

(*
External Canvas
===============
TExternalCanvas is a `TCanvas` that draws on memory it does not own, such as the overlay of a target plugin.
Everything a `TCanvas` has works on it, and every call is locked so it is safe to draw from more than one thread.

```
var
  Canvas: TExternalCanvas;
begin
  Canvas := TExternalCanvas.Create();
  Canvas.SetMemory(Data, Width, Height);
  Canvas.DrawColor := Colors.RED;
  Canvas.DrawBox([10, 10, 50, 50]);
  Canvas.Free();
end;
```
*)

(*
TExternalCanvas.Create
----------------------
```
function TExternalCanvas.Create: TExternalCanvas; static;
```

A canvas with no memory yet: see `SetMemory`. Its `DefaultPixel` is transparent black, so `Clear` leaves nothing showing.

It is not an object like `TImage`: it must be freed with `Free`.
*)
procedure _LapeExternalCanvas_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := TSimbaExternalCanvas.Create();
end;

(*
TExternalCanvas.SetMemory
-------------------------
```
procedure TExternalCanvas.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);
```

The pixels to draw on: AWidth * AHeight of BGRA. They are used where they are, not copied, and must stay valid while the canvas draws.
*)
procedure _LapeExternalCanvas_SetMemory(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.SetMemory(PPointer(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

(*
TExternalCanvas.UserData
------------------------
```
property TExternalCanvas.UserData: Pointer;
property TExternalCanvas.UserData(Value: Pointer);
```

A pointer of your own to keep with the canvas. The canvas never uses it.
*)
procedure _LapeExternalCanvas_UserData_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PSimbaExternalCanvas(Params^[0])^.UserData
end;

procedure _LapeExternalCanvas_UserData_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.UserData := PPointer(Params^[1])^;
end;

(*
TExternalCanvas.DrawImage
-------------------------
```
procedure TExternalCanvas.DrawImage(Image: TImage; Position: TPoint);
```

Draws an image with its top left at the position. Transparent pixels of the image are not drawn.
*)
procedure _LapeExternalCanvas_DrawImage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaExternalCanvas(Params^[0])^.DrawImage(PLapeObjectImage(Params^[1])^^, PPoint(Params^[2])^);
end;

procedure ImportExternalCanvas(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    // a TCanvas: every inherited method dispatches to TSimbaExternalCanvas's locked override
    addGlobalType('type TCanvas', 'TExternalCanvas');

    addGlobalFunc('function TExternalCanvas.Create: TExternalCanvas; static;', @_LapeExternalCanvas_Create);
    addGlobalFunc('procedure TExternalCanvas.SetMemory(Data: PColorBGRA; AWidth, AHeight: Integer);', @_LapeExternalCanvas_SetMemory);
    addGlobalFunc('procedure TExternalCanvas.DrawImage(Image: TImage; Position: TPoint);', @_LapeExternalCanvas_DrawImage);

    addProperty('TExternalCanvas', 'UserData', 'Pointer', @_LapeExternalCanvas_UserData_Read, @_LapeExternalCanvas_UserData_Write);
  end;
end;

end.

