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
  Classes, SysUtils, Graphics;

implementation

uses
  lptypes, lpvartypes,
  simba.script_compiler, simba.mufasatypes, simba.finder, simba.bitmap, simba.dtm,
  simba.colormath, simba.colormath_distance, simba.bitmap_finders, simba.target, simba.finder_color;

(*
Finder
======
Find Colors,Images,DTM and more on a target.

- If the `TSimbaFinder.Target` field is set, the global "Target" variable will be used. Which by default is set to Simba's target selection.
- There is a pre-defined variable `Finder` to use.

Example:

```
  WriteLn Finder.CountColor($0000FF, 10); // Count a red color with 10 tolerance.
```
*)

(*
TSimbaFinder.FindEdges
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindEdges1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaFinder.FindEdges
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindEdges2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TSimbaFinder.FindDTM
~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindDTM(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTM(PDTM(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TSimbaFinder.FindDTMRotated
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTMRotated(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PInteger(Params^[6])^, PBox(Params^[7])^);
end;

(*
TSimbaFinder.FindBitmap
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindBitmap1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

(*
TSimbaFinder.FindBitmap
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindBitmap2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

(*
TSimbaFinder.MatchColor
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix;
*)
procedure _LapeSimbaFinder_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaFinder(Params^[0])^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

(*
TSimbaFinder.FindColor
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TSimbaFinder.FindColor
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TSimbaFinder.FindColor
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_FindColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaFinder.CountColor
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

(*
TSimbaFinder.CountColor
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

(*
TSimbaFinder.CountColor
~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_CountColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaFinder.GetColor
~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetColor(X, Y: Integer): TColor;
*)
procedure _LapeSimbaFinder_GetColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

(*
TSimbaFinder.GetColor
~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetColor(P: TPoint): TColor;
*)
procedure _LapeSimbaFinder_GetColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PPoint(Params^[1])^.X, PPoint(Params^[1])^.Y);
end;

(*
TSimbaFinder.GetColors
~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetColors(Points: TPointArray): TIntegerArray;
*)
procedure _LapeSimbaFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PSimbaFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

(*
TSimbaFinder.GetColorsMatrix
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix;
*)
procedure _LapeSimbaFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaFinder(Params^[0])^.GetColorsMatrix(PBox(Params^[1])^);
end;

(*
TSimbaFinder.GetPixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_GetPixelDifference1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaFinder.GetPixelDifference
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetPixelDifference(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_GetPixelDifference2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TSimbaFinder.GetPixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_GetPixelDifferenceTPA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

(*
TSimbaFinder.GetPixelDifferenceTPA
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray;
*)
procedure _LapeSimbaFinder_GetPixelDifferenceTPA2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

(*
TSimbaFinder.AverageBrightness
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_AverageBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.AverageBrightness(PBox(Params^[1])^);
end;

(*
TSimbaFinder.PeakBrightness
~~~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer;
*)
procedure _LapeSimbaFinder_PeakBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.PeakBrightness(PBox(Params^[1])^);
end;

(*
TSimbaFinder.FindTemplate
~~~~~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.FindTemplate(Bitmap: TMufasaBitmap; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint;
*)
procedure _LapeSimbaFinder_FindTemplate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaFinder(Params^[0])^.FindTemplate(PMufasaBitmap(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeMufasaBitmap_Finder(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Result)^ := PMufasaBitmap(Params^[0])^.Finder;
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

    addGlobalVar(ltBoolean, @ColorFinderMT_Enabled, 'ColorFinderMT_Enabled');
    addGlobalVar(ltInt32, @ColorFinderMT_SliceHeight, 'ColorFinderMT_SliceHeight');
    addGlobalVar(ltInt32, @ColorFinderMT_SliceWidth, 'ColorFinderMT_SliceWidth');

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
      '  Target: TSimbaTarget;',
      '  {%CODETOOLS OFF}',
      '  InternalData: array[0..' + IntToStr(SizeOf(TSimbaFinder) - SizeOf(TSimbaTarget) - 1)  + '] of Byte;',
      '  {%CODETOOLS ON}',
      'end;'],
      'TSimbaFinder'
    );
    if (getGlobalType('TSimbaFinder').Size <> SizeOf(TSimbaFinder)) then
      raise Exception.Create('SizeOf(TSimbaFinder) is wrong!');

    with addGlobalVar('TSimbaFinder', '[]', 'Finder') do
      Used := duTrue;

    addGlobalFunc(
      'function TSimbaFinder.GetGlobalFinder: TSimbaFinder;', [
      'begin',
      '  Result := Self;',
      '  Result.Target := System.Target;',
      'end;'
    ]);

    addInputMethod('function TSimbaFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindEdges1);
    addInputMethod('function TSimbaFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindEdges2);

    addInputMethod('function TSimbaFinder.FindDTM(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindDTM);
    addInputMethod('function TSimbaFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindDTMRotated);

    addInputMethod('function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindBitmap1);
    addInputMethod('function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindBitmap2);
    addInputMethod('function TSimbaFinder.FindTemplate(Bitmap: TMufasaBitmap; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeSimbaFinder_FindTemplate);

    addInputMethod('function TSimbaFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix', @_LapeSimbaFinder_MatchColor);

    addInputMethod('function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor1);
    addInputMethod('function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor2);
    addInputMethod('function TSimbaFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor3);

    addInputMethod('function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor1);
    addInputMethod('function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor2);
    addInputMethod('function TSimbaFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor3);

    addInputMethod('function TSimbaFinder.GetColor(X, Y: Integer): TColor; overload', @_LapeSimbaFinder_GetColor1);
    addInputMethod('function TSimbaFinder.GetColor(P: TPoint): TColor; overload', @_LapeSimbaFinder_GetColor2);
    addInputMethod('function TSimbaFinder.GetColors(Points: TPointArray): TIntegerArray', @_LapeSimbaFinder_GetColors);
    addInputMethod('function TSimbaFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix', @_LapeSimbaFinder_GetColorsMatrix);

    addInputMethod('function TSimbaFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_GetPixelDifference1);
    addInputMethod('function TSimbaFinder.GetPixelDifference(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_GetPixelDifference2);
    addInputMethod('function TSimbaFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_GetPixelDifferenceTPA1);
    addInputMethod('function TSimbaFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_GetPixelDifferenceTPA2);

    addInputMethod('function TSimbaFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeSimbaFinder_AverageBrightness);
    addInputMethod('function TSimbaFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeSimbaFinder_PeakBrightness);

    ImportingSection := 'TMufasaBitmap';

    addGlobalFunc('function TMufasaBitmap.Finder: TSimbaFinder', @_LapeMufasaBitmap_Finder);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

