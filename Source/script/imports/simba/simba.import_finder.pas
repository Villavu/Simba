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
  simba.script_compiler, simba.mufasatypes, simba.finder, simba.bitmap, simba.dtm, simba.colormath_conversion;

procedure _LapeSimbaFinder_SetTarget_Bitmap(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTarget(PMufasaBitmap(Params^[1])^);
end;

procedure _LapeSimbaFinder_SetTarget_Window(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaFinder(Params^[0])^.SetTarget(PWindowHandle(Params^[1])^);
end;

procedure _LapeSimbaFinder_FindEdges1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_FindEdges2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindEdges(PSingle(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeSimbaFinder_FindDTM(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTM(PDTM(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindDTMRotated(PDTM(Params^[1])^, PDouble(Params^[2])^, PDouble(Params^[3])^, PDouble(Params^[4])^, PDoubleArray(Params^[5])^, PInteger(Params^[6])^, PBox(Params^[7])^);
end;

procedure _LapeSimbaFinder_FindBitmap1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PSingle(Params^[2])^, PInteger(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeSimbaFinder_FindBitmap2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PInteger(Params^[5])^, PBox(Params^[6])^);
end;

procedure _LapeSimbaFinder_MatchColor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSingleMatrix(Result)^ := PSimbaFinder(Params^[0])^.MatchColor(PColor(Params^[1])^, PColorSpace(Params^[2])^, PChannelMultipliers(Params^[3])^, PBox(Params^[4])^);
end;

procedure _LapeSimbaFinder_FindColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_FindColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

procedure _LapeSimbaFinder_FindColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.FindColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_CountColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_CountColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColor(Params^[1])^, PSingle(Params^[2])^, PColorSpace(Params^[3])^, PChannelMultipliers(Params^[4])^, PBox(Params^[5])^);
end;

procedure _LapeSimbaFinder_CountColor3(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.CountColor(PColorTolerance(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_GetColor1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeSimbaFinder_GetColor2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PColor(Result)^ := PSimbaFinder(Params^[0])^.GetColor(PPoint(Params^[1])^.X, PPoint(Params^[1])^.Y);
end;

procedure _LapeSimbaFinder_GetColors(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PSimbaFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

procedure _LapeSimbaFinder_GetColorsMatrix(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerMatrix(Result)^ := PSimbaFinder(Params^[0])^.GetColorsMatrix(PBox(Params^[1])^);
end;

procedure _LapeSimbaFinder_GetPixelDifference1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_GetPixelDifference2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifference(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_GetPixelDifferenceTPA1(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapeSimbaFinder_GetPixelDifferenceTPA2(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaFinder(Params^[0])^.GetPixelDifferenceTPA(PInteger(Params^[1])^, PInteger(Params^[2])^, PBox(Params^[3])^);
end;

procedure _LapeSimbaFinder_AverageBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.AverageBrightness(PBox(Params^[1])^);
end;

procedure _LapeSimbaFinder_PeakBrightness(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaFinder(Params^[0])^.PeakBrightness(PBox(Params^[1])^);
end;

procedure ImportFinder(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

    addGlobalType('array [0..2] of Single', 'TChannelMultipliers');

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
      'record',
      '  InternalData: array[0..' + IntToStr(SizeOf(TSimbaFinder))  + '] of Byte;',
      'end;'],
      'TSimbaFinder'
    );

    with addGlobalVar('TSimbaFinder', '[]', 'Finder') do
      Used := duTrue;

    addGlobalFunc('procedure TSimbaFinder.SetTarget(Bitmap: TMufasaBitmap); overload', @_LapeSimbaFinder_SetTarget_Bitmap);
    addGlobalFunc('procedure TSimbaFinder.SetTarget(Window: TWindowHandle); overload', @_LapeSimbaFinder_SetTarget_Window);

    addGlobalFunc('function TSimbaFinder.FindEdges(MinDiff: Single; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindEdges1);
    addGlobalFunc('function TSimbaFinder.FindEdges(MinDiff: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindEdges2);

    addGlobalFunc('function TSimbaFinder.FindDTM(DTM: TDTM; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindDTM);
    addGlobalFunc('function TSimbaFinder.FindDTMRotated(DTM: TDTM; StartDegrees, EndDegrees: Double; Step: Double; out FoundDegrees: TDoubleArray; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray', @_LapeSimbaFinder_FindDTMRotated);

    addGlobalFunc('function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; MaxToFind: Integer = 1; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindBitmap1);
    addGlobalFunc('function TSimbaFinder.FindBitmap(Bitmap: TMufasaBitmap; Tolerance: Single; ColorSpace: EColorSpace; Multipliers: TChannelMultipliers; MaxToFind: Integer; Bounds: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_FindBitmap2);

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
    addGlobalFunc('function TSimbaFinder.GetColorsMatrix(Bounds: TBox): TIntegerMatrix', @_LapeSimbaFinder_GetColorsMatrix);

    addGlobalFunc('function TSimbaFinder.GetPixelDifference(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_GetPixelDifference1);
    addGlobalFunc('function TSimbaFinder.GetPixelDifference(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): Integer; overload', @_LapeSimbaFinder_GetPixelDifference2);
    addGlobalFunc('function TSimbaFinder.GetPixelDifferenceTPA(WaitTime: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_GetPixelDifferenceTPA1);
    addGlobalFunc('function TSimbaFinder.GetPixelDifferenceTPA(WaitTime, Tolerance: Integer; Area: TBox = [-1,-1,-1,-1]): TPointArray; overload', @_LapeSimbaFinder_GetPixelDifferenceTPA2);

    addGlobalFunc('function TSimbaFinder.AverageBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeSimbaFinder_AverageBrightness);
    addGlobalFunc('function TSimbaFinder.PeakBrightness(Area: TBox = [-1,-1,-1,-1]): Integer', @_LapeSimbaFinder_PeakBrightness);

    {
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
    )
    }

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

