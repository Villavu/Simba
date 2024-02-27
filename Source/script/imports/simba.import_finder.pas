{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Wrappers & Imports to import into lape.
}
unit simba.import_finder;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Graphics,
  simba.base, simba.script_compiler;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes,
  simba.finder, simba.image, simba.dtm,
  simba.colormath, simba.target, simba.finder_color, simba.finder_image;

(*
Finder
======
Find Colors,Images,DTM and more on a target.

- If the `TFinder.Target` field is set, the global "Target" variable will be used. Which by default is set to Simba's target selection.
- There is a pre-defined variable `Finder` to use.

Example:

```
  WriteLn Finder.CountColor($0000FF, 10); // Count a red color with 10 tolerance.
```
*)

(*
TFinder.FindEdges
-----------------
> function TFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindEdges1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.FindEdges
-----------------
> function TFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindEdges2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindDTMEx
-----------------
> function TFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindDTMEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTMEx(PDTM(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindDTMRotatedEx
------------------------
> function TFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindDTMRotatedEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTMRotatedEx(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PInteger(Params^[6])^, PBox(Params^[7])^);
end;

(*
TFinder.FindDTM
---------------
> function TFinder.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindDTM(PDTM(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.FindDTMRotated
----------------------
> function TFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindDTMRotated(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.FindImageEx
-------------------
> function TFinder.FindImageEx(Image: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindImageEx1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindImageEx(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindImageEx
-------------------
> function TFinder.FindImageEx(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindImageEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindImageEx(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.FindImage
-----------------
> function TFinder.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.HasImage
----------------
> function TFinder.HasImage(Image: TSimbaImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox  = [-1,-1,-1,-1]): Boolean;
*)
procedure _LapeFinder_HasImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaFinder(Params^[0])^.HasImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.HasImage
----------------
> function TFinder.HasImage(Image: TSimbaImage; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox  = [-1,-1,-1,-1]): Boolean;
*)
procedure _LapeFinder_HasImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaFinder(Params^[0])^.HasImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindImage
-----------------
> function TFinder.FindImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.MatchColor
------------------
> function TFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix;
*)
procedure _LapeFinder_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaFinder(Params^[0])^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindColor
-----------------
> function TFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindColor
-----------------
> function TFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.FindColor
-----------------
> function TFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.CountColor
------------------
> function TFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.CountColor
------------------
> function TFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.CountColor
------------------
> function TFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_CountColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.HasColor
----------------
> function TFinder.HasColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
*)
procedure _LapeFinder_HasColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaFinder(Params^[0])^.HasColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.HasColor
----------------
> function TFinder.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
*)
procedure _LapeFinder_HasColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaFinder(Params^[0])^.HasColor(PColor(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.HasColor
----------------
> function TFinder.HasColor(Color: TColorTolerance; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload;
*)
procedure _LapeFinder_HasColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaFinder(Params^[0])^.HasColor(PColorTolerance(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.GetColor
----------------
> function TFinder.GetColor(X, Y: Integer): TColor;
*)
procedure _LapeFinder_GetColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TFinder.GetColor
----------------
> function TFinder.GetColor(P: TPoint): TColor;
*)
procedure _LapeFinder_GetColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PPoint(Params^[1])^.X, PPoint(Params^[1])^.Y);
end;

(*
TFinder.GetColors
-----------------
> function TFinder.GetColors(Points: TPointArray): TColorArray;
*)
procedure _LapeFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColorArray(Result)^ := PSimbaFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

(*
TFinder.GetColorsMatrix
-----------------------
> function TFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix;
*)
procedure _LapeFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaFinder(Params^[0])^.GetColorsMatrix(PBox(Params^[1])^);
end;

(*
TFinder.GetPixelDifference
--------------------------
> function TFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_GetPixelDifference1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.GetPixelDifference
--------------------------
> function TFinder.GetPixelDifference(WaitTime, Tolerance: Single; Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_GetPixelDifference2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.GetPixelDifferenceTPA
-----------------------------
> function TFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_GetPixelDifferenceTPA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.GetPixelDifferenceTPA
-----------------------------
> function TFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Single; Area: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_GetPixelDifferenceTPA2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.AverageBrightness
-------------------------
> function TFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_AverageBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.AverageBrightness(PBox(Params^[1])^);
end;

(*
TFinder.PeakBrightness
----------------------
> function TFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_PeakBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.PeakBrightness(PBox(Params^[1])^);
end;

(*
TFinder.FindTemplate
--------------------
> function TFinder.FindTemplate(Image: TImage; out Match: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindTemplate(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.HasTemplate
--------------------
> function TFinder.HasTemplate(Image: TImage; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): Boolean;
*)
procedure _LapeFinder_HasTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaFinder(Params^[0])^.HasTemplate(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);

  procedure addFinderMethod(Header: lpString; Addr: Pointer);
  begin
    Compiler.addGlobalFunc(Header, Addr);
    Compiler.addGlobalFuncOverride(Header, [
      'begin',
      '  if Self.Target.IsDefault() then',
      '  try',
      '    Self.Target := System.Target;',
      '    {$IFDECL Result}Result:={$ENDIF}inherited();',
      '  finally',
      '    Self.Target := [];',
      '  end else',
      '    {$IFDECL Result}Result:={$ENDIF}inherited();',
      'end;'
    ]);
  end;

begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

    addGlobalVar('record Enabled: Boolean; SliceWidth, SliceHeight: Integer; end;', @ColorFinderMultithreadOpts, 'ColorFinderMultithreadOpts');
    addGlobalVar('record Enabled: Boolean; SliceWidth, SliceHeight: Integer; end;', @BitmapFinderMultithreadOpts, 'BitmapFinderMultithreadOpts');

    addGlobalType([
      'record',
      '  Color: TColor;',
      '  Tolerance: Single;',
      '  ColorSpace: EColorSpace;',
      '  Multipliers: TChannelMultipliers;',
      'end;'],
      'TColorTolerance'
    );

    addGlobalFunc(
      'function ColorTolerance(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TColorTolerance;', [
      'begin',
      '  Result := [Color, Tolerance, ColorSpace, Multipliers];',
      'end;']
    );

    addGlobalType([
      'packed record',
      '  Target: TTarget;',
      'end;'],
      'TFinder'
    );

    with addGlobalVar('TFinder', '[]', 'Finder') do
    begin
      if (VarType.Size <> SizeOf(TSimbaFinder)) then
        SimbaException('SizeOf(TSimbaFinder) is wrong!');

      Used := duTrue;
    end;

    addFinderMethod('function TFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindEdges1);
    addFinderMethod('function TFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindEdges2);

    addFinderMethod('function TFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeFinder_FindDTMEx);
    addFinderMethod('function TFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeFinder_FindDTMRotatedEx);

    addFinderMethod('function TFinder.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindDTM);
    addFinderMethod('function TFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindDTMRotated);

    addFinderMethod('function TFinder.FindImageEx(Image: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindImageEx1);
    addFinderMethod('function TFinder.FindImageEx(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindImageEx2);
    addFinderMethod('function TFinder.FindImage(Image: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_FindImage1);
    addFinderMethod('function TFinder.FindImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_FindImage2);

    addFinderMethod('function TFinder.HasImage(Image: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_HasImage1);
    addFinderMethod('function TFinder.HasImage(Image: TImage; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_HasImage2);

    addFinderMethod('function TFinder.FindTemplate(Image: TImage; out Match: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindTemplate);
    addFinderMethod('function TFinder.HasTemplate(Image: TImage; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): Boolean', @_LapeFinder_HasTemplate);

    addFinderMethod('function TFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix', @_LapeFinder_MatchColor);

    addFinderMethod('function TFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor1);
    addFinderMethod('function TFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor2);
    addFinderMethod('function TFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor3);

    addFinderMethod('function TFinder.HasColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload', @_LapeFinder_HasColor1);
    addFinderMethod('function TFinder.HasColor(Color: TColor; Tolerance: Single; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload', @_LapeFinder_HasColor2);
    addFinderMethod('function TFinder.HasColor(Color: TColorTolerance; MinCount: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): Boolean; overload;', @_LapeFinder_HasColor3);

    addFinderMethod('function TFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor1);
    addFinderMethod('function TFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor2);
    addFinderMethod('function TFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor3);

    addFinderMethod('function TFinder.GetColor(X, Y: Integer): TColor; overload', @_LapeFinder_GetColor1);
    addFinderMethod('function TFinder.GetColor(P: TPoint): TColor; overload', @_LapeFinder_GetColor2);
    addFinderMethod('function TFinder.GetColors(Points: TPointArray): TColorArray', @_LapeFinder_GetColors);
    addFinderMethod('function TFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix', @_LapeFinder_GetColorsMatrix);

    addFinderMethod('function TFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeFinder_GetPixelDifference1);
    addFinderMethod('function TFinder.GetPixelDifference(WaitTime: Integer; Tolerance: Single; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeFinder_GetPixelDifference2);
    addFinderMethod('function TFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_GetPixelDifferenceTPA1);
    addFinderMethod('function TFinder.GetPixelDifferenceTPA(WaitTime: Integer; Tolerance: Single; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_GetPixelDifferenceTPA2);

    addFinderMethod('function TFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeFinder_AverageBrightness);
    addFinderMethod('function TFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeFinder_PeakBrightness);

    ImportingSection := 'Image';

    addGlobalFunc(
      'function TImage.Finder: TFinder;', [
      'begin',
      '  Result.Target.SetImage(Self);',
      'end;'
      ]
    );

    ImportingSection := '';
  end;
end;

end.
