{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Wrappers & Imports to import into lape.
}
unit simba.import_finder;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, Graphics,
  lptypes,
  simba.script_compiler, simba.mufasatypes, simba.finder, simba.bitmap, simba.dtm,
  simba.colormath, simba.colormath_distance, simba.bitmap_finders;

(*
Finder
======
Find Colors,Images,DTM on a target.
*)

(*
TSimbaFinder.SetTargetDesktop
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaFinder.SetTargetDesktop;
*)
procedure _LapeSimbaFinder_SetTargetDesktop(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTargetDesktop();
end;

(*
TSimbaFinder.SetTargetBitmap
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaFinder.SetTargetBitmap(Bitmap: TMufasaBitmap);
*)
procedure _LapeSimbaFinder_SetTargetBitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTargetBitmap(PMufasaBitmap(Params^[1])^);
end;

(*
TSimbaFinder.SetTargetWindow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaFinder.SetTargetWindow(Window: TWindowHandle);
*)
procedure _LapeSimbaFinder_SetTargetWindow(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTargetWindow(PWindowHandle(Params^[1])^);
end;

(*
TSimbaFinder.SetTargetEIOS
~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaFinder.SetTargetEIOS(Plugin, Args: String);
*)
procedure _LapeSimbaFinder_SetTargetEIOS(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTargetEIOS(PString(Params^[1])^, PString(Params^[2])^);
end;

(*
TSimbaFinder.GetTargetDimensions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
procedure TSimbaFinder.GetTargetDimensions(out Width, Height: Integer);
*)
procedure _LapeSimbaFinder_GetTargetDimensions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.GetTargetDimensions(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

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

(*
TSimbaFinder.GetImage
~~~~~~~~~~~~~~~~~~~~~
function TSimbaFinder.GetImage(Area: TBox = [-1,-1,-1,-1]): TMufasaBitmap;
*)
procedure _LapeSimbaFinder_GetImage(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMufasaBitmap(Result)^ := PSimbaFinder(Params^[0])^.GetImage(PBox(Params^[1])^);
end;

procedure _LapeMufasaBitmap_Finder(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Result)^ := PMufasaBitmap(Params^[0])^.Finder;
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

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
      '  InternalData: array[0..' + IntToStr(SizeOf(TSimbaFinder) - 1)  + '] of Byte;',
      'end;'],
      'TSimbaFinder'
    );

    with addGlobalVar('TSimbaFinder', '[]', 'Finder') do
      Used := duTrue;

    addGlobalFunc('procedure TSimbaFinder.SetTargetDesktop', @_LapeSimbaFinder_SetTargetDesktop);
    addGlobalFunc('procedure TSimbaFinder.SetTargetBitmap(Bitmap: TMufasaBitmap)', @_LapeSimbaFinder_SetTargetBitmap);
    addGlobalFunc('procedure TSimbaFinder.SetTargetWindow(Window: TWindowHandle)', @_LapeSimbaFinder_SetTargetWindow);
    addGlobalFunc('procedure TSimbaFinder.SetTargetEIOS(Plugin, Args: String)', @_LapeSimbaFinder_SetTargetEIOS);

    addGlobalFunc('procedure TSimbaFinder.GetTargetDimensions(out Width, Height: Integer)', @_LapeSimbaFinder_GetTargetDimensions);

    addGlobalFunc('function TSimbaFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindEdges1);
    addGlobalFunc('function TSimbaFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindEdges2);

    addGlobalFunc('function TSimbaFinder.FindDTM(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindDTM);
    addGlobalFunc('function TSimbaFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindDTMRotated);

    addGlobalFunc('function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindBitmap1);
    addGlobalFunc('function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindBitmap2);

    addGlobalFunc('function TSimbaFinder.FindTemplate(Bitmap: TMufasaBitmap; MinMatch: Single; Bounds: TBox = [-1,-1,-1,-1]): TPoint', @_LapeSimbaFinder_FindTemplate);

    addGlobalFunc('function TSimbaFinder.MatchColor(Color: TColor; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TSingleMatrix', @_LapeSimbaFinder_MatchColor);

    addGlobalFunc('function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor1);
    addGlobalFunc('function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor2);
    addGlobalFunc('function TSimbaFinder.FindColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor3);

    addGlobalFunc('function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor1);
    addGlobalFunc('function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor2);
    addGlobalFunc('function TSimbaFinder.CountColor(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor3);

    addGlobalFunc('function TSimbaFinder.GetColor(X, Y: Integer): TColor; overload', @_LapeSimbaFinder_GetColor1);
    addGlobalFunc('function TSimbaFinder.GetColor(P: TPoint): TColor; overload', @_LapeSimbaFinder_GetColor2);
    addGlobalFunc('function TSimbaFinder.GetColors(Points: TPointArray): TIntegerArray', @_LapeSimbaFinder_GetColors);
    addGlobalFunc('function TSimbaFinder.GetColorsMatrix(Bounds: TBox = [-1,-1,-1,-1]): TIntegerMatrix', @_LapeSimbaFinder_GetColorsMatrix);

    addGlobalFunc('function TSimbaFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_GetPixelDifference1);
    addGlobalFunc('function TSimbaFinder.GetPixelDifference(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_GetPixelDifference2);
    addGlobalFunc('function TSimbaFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_GetPixelDifferenceTPA1);
    addGlobalFunc('function TSimbaFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_GetPixelDifferenceTPA2);

    addGlobalFunc('function TSimbaFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeSimbaFinder_AverageBrightness);
    addGlobalFunc('function TSimbaFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeSimbaFinder_PeakBrightness);

    addGlobalFunc('function TSimbaFinder.GetImage(Area: TBox = [-1,-1,-1,-1]): TMufasaBitmap', @_LapeSimbaFinder_GetImage);

    ImportingSection := 'TMufasaBitmap';

    addGlobalFunc('function TMufasaBitmap.Finder: TSimbaFinder', @_LapeMufasaBitmap_Finder);
    addGlobalFunc(
      'function TMufasaBitmap.CreateFromFinder(Area: TBox = [-1,-1,-1,-1]): TMufasaBitmap; static; override;', [
      'begin',
      '  Result := Finder.GetImage(Area);',
      'end;'
    ]);
    addGlobalFunc(
      'function TMufasaBitmap.DrawFinder(P: TPoint; Area: TBox = [-1,-1,-1,-1]): TMufasaBitmap;', [
      'var',
      '  Image: TMufasaBitmap := TMufasaBitmap.CreateFromFinder(Area);',
      'begin',
      '  try',
      '    Self.DrawBitmap(Image, P);',
      '  finally',
      '    Image.Free();',
      '  end;',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

