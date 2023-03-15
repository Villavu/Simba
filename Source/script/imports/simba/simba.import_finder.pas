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
  classes, sysutils, Graphics, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.finder_test, simba.bitmap;

procedure _LapeSimbaFinder_SetTarget_Bitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTarget(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeSimbaFinder_SetTarget_Window(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTarget(PWindowHandle(Params^[1])^);
end;

procedure _LapeSimbaFinder_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_FindColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeSimbaFinder_FindColorEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColorEx(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_CountColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeSimbaFinder_CountColorEx(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColorEx(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_GetColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PSimbaFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

procedure _LapeSimbaFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaFinder(Params^[0])^.GetColorsMatrix(PBox(Params^[1])^);
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

    addGlobalType('array [0..2] of Single', 'TChannelMultipliers');

    addGlobalType([
      'record',
      '  ColorSpace: EColorSpace;',
      '  Multipliers: TChannelMultipliers;',
      'end;'],
      'TColorSpace'
    );

    addGlobalType([
      'record',
      '  Color: Integer;',
      '  Tolerance: Single;',
      '  ColorSpace: EColorSpace;',
      '  Multipliers: TChannelMultipliers;',
      'end;'],
      'TColorTolerance'
    );

    addGlobalFunc(
      'function ColorSpace(AColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TColorSpace;', [
      'begin',
      '  Result := [AColorSpace, Multipliers];',
      'end;']
    );

    addGlobalFunc(
      'function ColorTolerance(Color: TColor; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers): TColorTolerance;', [
      'begin',
      '  Result := [Color, Tolerance, ColorSpace, Multipliers];',
      'end;']
    );

    addGlobalType([
      'record',
      '  Data: array[0..' + IntToStr(SizeOf(TSimbaFinder))  + '] of Byte;',
      'end;'],
      'TSimbaFinder'
    );

    addGlobalVar(getGlobalType('TSimbaFinder').NewGlobalVarP(), 'Finder');

    addGlobalFunc('procedure TSimbaFinder.SetTarget(Bitmap: TMufasaBitmap); overload', @_LapeSimbaFinder_SetTarget_Bitmap);
    addGlobalFunc('procedure TSimbaFinder.SetTarget(Window: TWindowHandle); overload', @_LapeSimbaFinder_SetTarget_Window);

    addGlobalFunc('function TSimbaFinder.FindColor(Color: TColor; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor1);
    addGlobalFunc('function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor2);
    addGlobalFunc('function TSimbaFinder.FindColor(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindColor3);
    addGlobalFunc('function TSimbaFinder.FindColorEx(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindColorEx);

    addGlobalFunc('function TSimbaFinder.CountColor(Color: TColor; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_CountColor1);
    addGlobalFunc('function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload;', @_LapeSimbaFinder_CountColor2);
    addGlobalFunc('function TSimbaFinder.CountColor(Color: TColor; Tolerance: Single; ColorSpace: TColorSpace; Bounds: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_CountColor3);
    addGlobalFunc('function TSimbaFinder.CountColorEx(Color: TColorTolerance; Bounds: TBox = [-1,-1,-1,-1]): Integer;', @_LapeSimbaFinder_CountColorEx);

    addGlobalFunc('function TSimbaFinder.GetColor(X, Y: Integer): TColor', @_LapeSimbaFinder_GetColor);
    addGlobalFunc('function TSimbaFinder.GetColors(Points: TPointArray): TIntegerArray', @_LapeSimbaFinder_GetColors);
    addGlobalFunc('function TSimbaFinder.GetColorsMatrix(Bounds: TBox): TIntegerMatrix', @_LapeSimbaFinder_GetColorsMatrix);

    {

    // FindDTMs
    addGlobalFunc(
      'function FindDTMs(DTM: TDTM; out FoundPoints: TPointArray; X1, Y1, X2, Y2: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMs(DTM, FoundPoints, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTMs(DTM: TDTM; out FoundPoints: TPointArray; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMs(DTM, FoundPoints, Area);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTMs(DTM: TDTM; out FoundPoints: TPointArray): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMs(DTM, FoundPoints, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );

    // FindDTM
    addGlobalFunc(
      'function FindDTM(DTM: TDTM; out X, Y: Integer; X1, Y1, X2, Y2: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTM(DTM, X, Y, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTM(DTM: TDTM; out X, Y: Integer; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTM(DTM, X, Y, Area);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTM(DTM: TDTM; out X, Y: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTM(DTM, X, Y, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );

    // FindDTMsRotated
    addGlobalFunc(
      'function FindDTMsRotated(DTM: TDTM; out FoundPoints: TPointArray; X1, Y1, X2, Y2: Integer; StartDegree, EndDegree, Step: Double; out FoundDegrees: TDoubleArray): Boolean;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMsRotated(DTM, FoundPoints, TBox([X1, Y1, X2, Y2]), StartDegree, EndDegree, Step, FoundDegrees);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTMsRotated(DTM: TDTM; out FoundPoints: TPointArray; Area: TBox; StartDegree, EndDegree, Step: Double; out FoundDegrees: TDoubleArray): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMsRotated(DTM, FoundPoints, Area, StartDegree, EndDegree, Step, FoundDegrees);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTMsRotated(DTM: TDTM; out FoundPoints: TPointArray; StartDegree, EndDegree, Step: Double; out FoundDegrees: TDoubleArray): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMsRotated(DTM, FoundPoints, TBox([-1, -1, -1, -1]), StartDegree, EndDegree, Step, FoundDegrees);',
      'end;'
      ]
    );

    // FindDTMRotated
    addGlobalFunc(
      'function FindDTMRotated(DTM: TDTM; out X, Y: Integer; X1, Y1, X2, Y2: Integer; StartDegree, EndDegree, Step: Double; out FoundDegree: Double): Boolean;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMRotated(DTM, X, Y, TBox([X1, Y1, X2, Y2]), StartDegree, EndDegree, Step, FoundDegree);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTMRotated(DTM: TDTM; out X, Y: Integer; Area: TBox; StartDegree, EndDegree, Step: Double; out FoundDegree: Double): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMRotated(DTM, X, Y, Area, StartDegree, EndDegree, Step, FoundDegree);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindDTMRotated(DTM: TDTM; out X, Y: Integer; StartDegree, EndDegree, Step: Double; out FoundDegree: Double): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindDTMRotated(DTM, X, Y, TBox([-1, -1, -1, -1]), StartDegree, EndDegree, Step, FoundDegree);',
      'end;'
      ]
    );

    // FindBitmaps
    addGlobalFunc(
      'function FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; X1, Y1, X2, Y2: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmaps(Bitmap, Points, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmaps(Bitmap, Points, Area);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmaps(Bitmap: TMufasaBitmap; out Points: TPointArray): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmaps(Bitmap, Points, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );

    // FindBitmap
    addGlobalFunc(
      'function FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; X1, Y1, X2, Y2: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmap(Bitmap, X, Y, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmap(Bitmap, X, Y, Area);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmap(Bitmap: TMufasaBitmap; out X, Y: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmap(Bitmap, X, Y, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );

    // FindBitmapsTolerance
    addGlobalFunc(
      'function FindBitmapsTolerance(Bitmap: TMufasaBitmap; out Points: TPointArray; X1, Y1, X2, Y2: Integer; Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmapsTolerance(Bitmap, Points, TBox([X1, Y1, X2, Y2]), Tolerance);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmapsTolerance(Bitmap: TMufasaBitmap; out Points: TPointArray; Area: TBox; Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmapsTolerance(Bitmap, Points, Area, Tolerance);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmapsTolerance(Bitmap: TMufasaBitmap; out Points: TPointArray; Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmapsTolerance(Bitmap, Points, TBox([-1, -1, -1, -1]), Tolerance);',
      'end;'
      ]
    );

    // FindBitmapTolerance
    addGlobalFunc(
      'function FindBitmapTolerance(Bitmap: TMufasaBitmap; out X, Y: Integer; X1, Y1, X2, Y2: Integer; Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmapTolerance(Bitmap, X, Y, TBox([X1, Y1, X2, Y2]), Tolerance);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmapTolerance(Bitmap: TMufasaBitmap; out X, Y: Integer; Area: TBox; Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmapTolerance(Bitmap, X, Y, Area, Tolerance);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindBitmapTolerance(Bitmap: TMufasaBitmap; out X, Y: Integer; Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindBitmapTolerance(Bitmap, X, Y, TBox([-1, -1, -1, -1]), Tolerance);',
      'end;'
      ]
    );

    // FindTemplate
    addGlobalFunc(
      'function FindTemplate(Bitmap: TMufasaBitmap; out X, Y: Integer; Formula: ETMFormula; Area: TBox; MinMatch: Extended): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindTemplate(Bitmap, X, Y, Formula, Area, MinMatch);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindTemplate(Bitmap: TMufasaBitmap; out X,Y: Integer; Formula: ETMFormula; MinMatch: Extended): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindTemplate(Bitmap, X, Y, Formula, TBox([-1, -1, -1, -1]), MinMatch);',
      'end;'
      ]
    );

    // FindTemplateEx
    addGlobalFunc(
      'function FindTemplateEx(Bitmap: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; Area: TBox; MinMatch: Extended): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindTemplateEx(Bitmap, TPA, Formula, Area, MinMatch);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindTemplateEx(Bitmap: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; MinMatch: Extended): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindTemplateEx(Bitmap, TPA, Formula, TBox([-1, -1, -1, -1]), MinMatch);',
      'end;'
      ]
    );

    // GetColor
    addGlobalFunc(
      'function GetColor(X, Y: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().GetColor(X, Y);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function GetColor(Point: TPoint): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().GetColor(Point.X, Point.Y);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function GetColors(Points: TPointArray): TIntegerArray;', [
      'begin',
      '  Result := Client.GetMFinder().GetColors(Points);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function GetColorsMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix;', [
      'begin',
      '  Result := Client.GetMFinder().GetColorsMatrix(X1, Y1, X2, Y2);',
      'end;'
      ]
    );

    // SimilarColors
    addGlobalFunc(
      'function SimilarColors(Color1, Color2, Tolerance: Integer): Boolean;', [
      'begin',
      '  Result := Client.GetMFinder().SimilarColors(Color1, Color2, Tolerance);',
      'end;'
      ]
    );

    // AverageBrightness
    addGlobalFunc(
      'function AverageBrightness(Area: TBox): Integer;', [
      'begin',
      '  Result := Client.GetMFinder().AverageBrightness(Area);',
      'end;'
      ]
    );
    // PeakBrightness
    addGlobalFunc(
      'function PeakBrightness(Area: TBox): Integer;', [
      'begin',
      '  Result := Client.GetMFinder().PeakBrightness(Area);',
      'end;'
      ]
    );

    // GetPixelDifference
    addGlobalFunc(
      'function GetPixelDifference(Area: TBox; WaitTime: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().GetPixelDifference(Area, WaitTime);',
      'end;'
      ]
    );
    // GetPixelDifference
    addGlobalFunc(
      'function GetPixelDifference(Area: TBox; Tolerance, WaitTime: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().GetPixelDifference(Area, Tolerance, WaitTime);',
      'end;'
      ]
    );

    // GetPixelDifference
    addGlobalFunc(
      'function GetPixelDifferenceTPA(Area: TBox; WaitTime: Integer): TPointArray; overload;', [
      'begin',
      '  Result := Client.GetMFinder().GetPixelDifferenceTPA(Area, WaitTime);',
      'end;'
      ]
    );
    // GetPixelDifference
    addGlobalFunc(
      'function GetPixelDifferenceTPA(Area: TBox; Tolerance, WaitTime: Integer): TPointArray; overload;', [
      'begin',
      '  Result := Client.GetMFinder().GetPixelDifferenceTPA(Area, Tolerance, WaitTime);',
      'end;'
      ]
    );
    }

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

