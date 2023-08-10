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
  simba.mufasatypes, simba.script_compiler;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes,
  simba.finder, simba.bitmap, simba.dtm,
  simba.colormath, simba.target, simba.finder_color, simba.finder_bitmap;

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
~~~~~~~~~~~~~~~~~
> function TFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindEdges1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.FindEdges
~~~~~~~~~~~~~~~~~
> function TFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindEdges2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindDTMEx
~~~~~~~~~~~~~~~~~
> function TFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindDTMEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTMEx(PDTM(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindDTMRotatedEx
~~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindDTMRotatedEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTMRotatedEx(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PInteger(Params^[6])^, PBox(Params^[7])^);
end;

(*
TFinder.FindDTM
~~~~~~~~~~~~~~~
> function TFinder.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindDTM(PDTM(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.FindDTMRotated
~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindDTMRotated(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.FindImageEx
~~~~~~~~~~~~~~~~~~~
> function TFinder.FindImageEx(Bitmap: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindImageEx1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindImageEx(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindImageEx
~~~~~~~~~~~~~~~~~~~
> function TFinder.FindImageEx(Bitmap: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindImageEx2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindImageEx(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TFinder.FindImage
~~~~~~~~~~~~~~~~~
> function TFinder.FindImage(Bitmap: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindImage1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindImage
~~~~~~~~~~~~~~~~~
> function TFinder.FindImage(Bitmap: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindImage2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindImage(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.MatchColor
~~~~~~~~~~~~~~~~~~
> function TFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix;
*)
procedure _LapeFinder_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaFinder(Params^[0])^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TFinder.FindColor
~~~~~~~~~~~~~~~~~
> function TFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.FindColor
~~~~~~~~~~~~~~~~~
> function TFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.FindColor
~~~~~~~~~~~~~~~~~
> function TFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_FindColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.CountColor
~~~~~~~~~~~~~~~~~~
> function TFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.CountColor
~~~~~~~~~~~~~~~~~~
> function TFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TFinder.CountColor
~~~~~~~~~~~~~~~~~~
> function TFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_CountColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.GetColor
~~~~~~~~~~~~~~~~
> function TFinder.GetColor(X, Y: Integer): TColor;
*)
procedure _LapeFinder_GetColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TFinder.GetColor
~~~~~~~~~~~~~~~~
> function TFinder.GetColor(P: TPoint): TColor;
*)
procedure _LapeFinder_GetColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PPoint(Params^[1])^.X, PPoint(Params^[1])^.Y);
end;

(*
TFinder.GetColors
~~~~~~~~~~~~~~~~~
> function TFinder.GetColors(Points: TPointArray): TIntegerArray;
*)
procedure _LapeFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PSimbaFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

(*
TFinder.GetColorsMatrix
~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix;
*)
procedure _LapeFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaFinder(Params^[0])^.GetColorsMatrix(PBox(Params^[1])^);
end;

(*
TFinder.GetPixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_GetPixelDifference1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.GetPixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.GetPixelDifference(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_GetPixelDifference2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.GetPixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_GetPixelDifferenceTPA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TFinder.GetPixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeFinder_GetPixelDifferenceTPA2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TFinder.AverageBrightness
~~~~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_AverageBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.AverageBrightness(PBox(Params^[1])^);
end;

(*
TFinder.PeakBrightness
~~~~~~~~~~~~~~~~~~~~~~
> function TFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeFinder_PeakBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.PeakBrightness(PBox(Params^[1])^);
end;

(*
TFinder.FindTemplate
~~~~~~~~~~~~~~~~~~~~
> function TFinder.FindTemplate(Bitmap: TImage; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeFinder_FindTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindTemplate(PSimbaImage(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);

  // If target field is default value, use global target variable.
  function GetOverrideBody(Name, Params: lpString; isFunction: Boolean): String;
  begin
    Result := 'begin'                                                                                                 + LineEnding +
              '  if Self.Target.IsDefault() then'                                                                     + LineEnding +
              '    ' + BoolToStr(isFunction, 'Result := ', '') + 'Self.GetGlobalFinder().' + Name + '(' + Params + ')' + LineEnding +
              '  else'                                                                                                + LineEnding +
              '    ' + BoolToStr(isFunction, 'Result := ', '') + 'inherited();'                                       + LineEnding +
              'end;';
  end;

  procedure addInputMethod(Header: lpString; Addr: Pointer);
  begin
    Compiler.addGlobalFunc(Header, Addr);
    Compiler.addOverrideMethod(Header, @GetOverrideBody);
  end;

begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

    addGlobalVar(ltBoolean, @ColorFinderMT_Enabled,     'ColorFinderMT_Enabled');
    addGlobalVar(ltInt32,   @ColorFinderMT_SliceHeight, 'ColorFinderMT_SliceHeight');
    addGlobalVar(ltInt32,   @ColorFinderMT_SliceWidth,  'ColorFinderMT_SliceWidth');

    addGlobalVar(ltBoolean, @BitmapFinderMT_Enabled,     'BitmapFinderMT_Enabled');
    addGlobalVar(ltInt32,   @BitmapFinderMT_SliceHeight, 'BitmapFinderMT_SliceHeight');
    addGlobalVar(ltInt32,   @BitmapFinderMT_SliceWidth,  'BitmapFinderMT_SliceWidth');

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
    if (getGlobalType('TFinder').Size <> SizeOf(TSimbaFinder)) then
      raise Exception.Create('SizeOf(TSimbaFinder) is wrong!');

    with addGlobalVar('TFinder', '[]', 'Finder') do
      Used := duTrue;

    addGlobalFunc(
      'function TFinder.GetGlobalFinder: TFinder;', [
      'begin',
      '  Result := Self;',
      '  Result.Target := System.Target;',
      'end;'
    ]);

    addInputMethod('function TFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindEdges1);
    addInputMethod('function TFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindEdges2);

    addInputMethod('function TFinder.FindDTMEx(DTM: TDTM; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeFinder_FindDTMEx);
    addInputMethod('function TFinder.FindDTMRotatedEx(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeFinder_FindDTMRotatedEx);

    addInputMethod('function TFinder.FindDTM(DTM: TDTM; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindDTM);
    addInputMethod('function TFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindDTMRotated);

    addInputMethod('function TFinder.FindImageEx(Bitmap: TImage; Tolerance: Single; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindImageEx1);
    addInputMethod('function TFinder.FindImageEx(Bitmap: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer = -1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindImageEx2);
    addInputMethod('function TFinder.FindImage(Bitmap: TImage; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_FindImage1);
    addInputMethod('function TFinder.FindImage(Bitmap: TImage; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPoint; overload', @_LapeFinder_FindImage2);

    addInputMethod('function TFinder.FindTemplate(Bitmap: TImage; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeFinder_FindTemplate);

    addInputMethod('function TFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix', @_LapeFinder_MatchColor);

    addInputMethod('function TFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor1);
    addInputMethod('function TFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor2);
    addInputMethod('function TFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_FindColor3);

    addInputMethod('function TFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor1);
    addInputMethod('function TFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor2);
    addInputMethod('function TFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeFinder_CountColor3);

    addInputMethod('function TFinder.GetColor(X, Y: Integer): TColor; overload', @_LapeFinder_GetColor1);
    addInputMethod('function TFinder.GetColor(P: TPoint): TColor; overload', @_LapeFinder_GetColor2);
    addInputMethod('function TFinder.GetColors(Points: TPointArray): TIntegerArray', @_LapeFinder_GetColors);
    addInputMethod('function TFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix', @_LapeFinder_GetColorsMatrix);

    addInputMethod('function TFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeFinder_GetPixelDifference1);
    addInputMethod('function TFinder.GetPixelDifference(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeFinder_GetPixelDifference2);
    addInputMethod('function TFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_GetPixelDifferenceTPA1);
    addInputMethod('function TFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeFinder_GetPixelDifferenceTPA2);

    addInputMethod('function TFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeFinder_AverageBrightness);
    addInputMethod('function TFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeFinder_PeakBrightness);

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
