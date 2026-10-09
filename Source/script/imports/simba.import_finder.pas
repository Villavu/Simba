unit simba.import_finder;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script, simba.script_objectutil;

procedure ImportFinder(Script: TSimbaScript);

implementation

uses
  lptypes,
  simba.colormath, simba.dtm, simba.misc, simba.image, simba.finder, simba.finder_pixels;

(*
Finder
======
TFinder finds things in pixels: colors, images, DTMs.

There are two of them:

- `TImage.Finder` finds in an image.
- `TTarget` is a TFinder, so every method here is on `Target` too and finds in what the target shows.

```
WriteLn(Target.FindColor($0000FF, 5));
WriteLn(img.Finder.FindColor($0000FF, 5));
```

A `Bounds` of `[-1,-1,-1,-1]`, the default, is everything. Points come back relative to the image or target, not to `Bounds`.
*)

procedure _LapeFinder_Destroy(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  LapeObjectDestroy(PLapeObject(Params^[0]));
end;

(*
TImage.Finder
-------------
```
property TImage.Finder: TFinder;
```

Everything that finds in the image lives here. It reads the image's own pixels, so it sees what is drawn after it was taken.

```
var
  img: TImage;
begin
  img := Target.GetImage();
  WriteLn(img.Finder.CountColor($0000FF, 5));
end;
```
*)
procedure _LapeImage_Finder_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLapeObject(Result)^ := LapeObjectAlloc(PLapeObjectImage(Params^[0])^^.Finder, False); // the image owns it
end;

(*
TFinder.MatchColor
------------------
```
function TFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix;
```

How far each pixel is from `Color`, from 0 (an exact match) up to 100. It is the number the other finders compare with a tolerance.
*)
procedure _LapeFinder_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PLapeObjectFinder(Params^[0])^^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindColor
-----------------
```
function TFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
function TFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```

Every pixel within the tolerance of the color. Tolerance is 0..100: 0 is an exact match.
*)
procedure _LapeFinder_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeFinder_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.CountColor
------------------
```
function TFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer;
function TFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer;
```

How many pixels `FindColor` would find.
*)
procedure _LapeFinder_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectFinder(Params^[0])^^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeFinder_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectFinder(Params^[0])^^.CountColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.HasColor
----------------
```
function TFinder.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
function TFinder.HasColor(Color: TColorTolerance; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
```

True if at least `MinCount` pixels match. It stops looking once it has that many.
*)
procedure _LapeFinder_HasColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectFinder(Params^[0])^^.HasColor(PColor(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeFinder_HasColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLapeObjectFinder(Params^[0])^^.HasColor(PColorTolerance(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.GetColor
----------------
```
function TFinder.GetColor(P: TPoint): TColor;
```

The color at a point, or -1 if it is out of bounds.
*)
procedure _LapeFinder_GetColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PLapeObjectFinder(Params^[0])^^.GetColor(PPoint(Params^[1])^);
end;

(*
TFinder.GetColors
-----------------
```
function TFinder.GetColors(Points: TPointArray): TColorArray;
```

The color at each point.
*)
procedure _LapeFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PLapeObjectFinder(Params^[0])^^.GetColors(PPointArray(Params^[1])^);
end;

(*
TFinder.GetColorsMatrix
-----------------------
```
function TFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix;
```

The colors of a box as a matrix.
*)
procedure _LapeFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PLapeObjectFinder(Params^[0])^^.GetColorsMatrix(PBox(Params^[1])^);
end;

(*
TFinder.FindImage
-----------------
```
function TFinder.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
function TFinder.FindImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```

The top left of the first place the image is found, or `[-1,-1]`. Transparent pixels of the image match anything.
*)
procedure _LapeFinder_FindImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with PLapeObjectImage(Params^[1])^^ do
    PPoint(Result)^ := PLapeObjectFinder(Params^[0])^^.FindImage(Data, Width, Height, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeFinder_FindImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with PLapeObjectImage(Params^[1])^^ do
    PPoint(Result)^ := PLapeObjectFinder(Params^[0])^^.FindImage(Data, Width, Height, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.FindImageEx
-------------------
```
function TFinder.FindImageEx(Image: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
function TFinder.FindImageEx(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```

Every place the image is found, up to `MaxToFind` (-1 is all).
*)
procedure _LapeFinder_FindImageEx1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with PLapeObjectImage(Params^[1])^^ do
    PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindImageEx(Data, Width, Height, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeFinder_FindImageEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with PLapeObjectImage(Params^[1])^^ do
    PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindImageEx(Data, Width, Height, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.FindTemplate
--------------------
```
function TFinder.FindTemplate(Templ: TImage; out Match: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```

The best place for the template by template matching, and how good a match it is in `Match`.
*)
procedure _LapeFinder_FindTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with PLapeObjectImage(Params^[1])^^ do
    PPoint(Result)^ := PLapeObjectFinder(Params^[0])^^.FindTemplate(Data, Width, Height, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindDTM
---------------
```
function TFinder.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```

The first place the DTM is found, or `[-1,-1]`.
*)
procedure _LapeFinder_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PLapeObjectFinder(Params^[0])^^.FindDTM(PDTM(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.FindDTMEx
-----------------
```
function TFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```

Every place the DTM is found, up to `MaxToFind` (-1 is all).
*)
procedure _LapeFinder_FindDTMEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindDTMEx(PDTM(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindDTMRotated
----------------------
```
function TFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
```

`FindDTM` with the DTM turned from `StartDegrees` to `EndDegrees`, `Step` at a time. `FoundDegrees` is the angle it was found at.
*)
procedure _LapeFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PLapeObjectFinder(Params^[0])^^.FindDTMRotated(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.FindDTMRotatedEx
------------------------
```
function TFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```

`FindDTMEx` with the DTM turned. `FoundDegrees` has the angle of each point found.
*)
procedure _LapeFinder_FindDTMRotatedEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindDTMRotatedEx(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PInteger(Params^[6])^, PBox(Params^[7])^);
end;

(*
TFinder.FindEdges
-----------------
```
function TFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
function TFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
```

The pixels whose color differs from the one to their right or below by more than `MinDiff`.
*)
procedure _LapeFinder_FindEdges1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindEdges(PSingle(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeFinder_FindEdges2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PLapeObjectFinder(Params^[0])^^.FindEdges(PSingle(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.GetBrightness
---------------------
```
function TFinder.GetBrightness(Algo: EBrightnessAlgo; Bounds: TBox = [-1,-1,-1,-1]): Integer;
```

The brightness of an area.

`Algo` can be either of:
 - `EBrightnessAlgo.MIN`
 - `EBrightnessAlgo.MAX`
 - `EBrightnessAlgo.MEAN`
*)
procedure _LapeFinder_GetBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PLapeObjectFinder(Params^[0])^^.GetBrightness(EBrightnessAlgo(Params^[1]^), PBox(Params^[2])^);
end;

procedure ImportFinder(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Finder';

    LapeObjectImport(Script.Compiler, 'TFinder');

    addGlobalType(specialize GetEnumDecl<EBrightnessAlgo>(True, True), 'EBrightnessAlgo');

    addGlobalFunc('procedure TFinder.Destroy;', @_LapeFinder_Destroy);

    addProperty('TImage', 'Finder', 'TFinder', @_LapeImage_Finder_Read);

    addGlobalFunc('function TFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix;', @_LapeFinder_MatchColor);

    addGlobalFunc('function TFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor1);
    addGlobalFunc('function TFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor2);

    addGlobalFunc('function TFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor1);
    addGlobalFunc('function TFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor2);

    addGlobalFunc('function TFinder.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload', @_LapeFinder_HasColor1);
    addGlobalFunc('function TFinder.HasColor(Color: TColorTolerance; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload;', @_LapeFinder_HasColor2);

    addGlobalFunc('function TFinder.GetColor(P: TPoint): TColor', @_LapeFinder_GetColor);
    addGlobalFunc('function TFinder.GetColors(Points: TPointArray): TColorArray', @_LapeFinder_GetColors);
    addGlobalFunc('function TFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix', @_LapeFinder_GetColorsMatrix);

    addGlobalFunc('function TFinder.FindImageEx(Image: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindImageEx1);
    addGlobalFunc('function TFinder.FindImageEx(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindImageEx2);
    addGlobalFunc('function TFinder.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_FindImage1);
    addGlobalFunc('function TFinder.FindImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_FindImage2);
    addGlobalFunc('function TFinder.FindTemplate(Templ: TImage; out Match: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindTemplate);

    addGlobalFunc('function TFinder.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindDTM);
    addGlobalFunc('function TFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeFinder_FindDTMEx);
    addGlobalFunc('function TFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindDTMRotated);
    addGlobalFunc('function TFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeFinder_FindDTMRotatedEx);

    addGlobalFunc('function TFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload;', @_LapeFinder_FindEdges1);
    addGlobalFunc('function TFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload;', @_LapeFinder_FindEdges2);

    addGlobalFunc('function TFinder.GetBrightness(Algo: EBrightnessAlgo; Bounds: TBox = [-1,-1,-1,-1]): Integer;', @_LapeFinder_GetBrightness);

    DumpSection := '';
  end;
end;

end.
