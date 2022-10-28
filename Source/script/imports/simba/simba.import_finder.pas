unit simba.import_finder;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes;


procedure ImportFinder(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Finder';

    addGlobalFunc(
      'procedure SetColorToleranceSpeed(CTS: Integer);', [
      'begin',
      '  Client.GetMFinder().SetToleranceSpeed(CTS);',
      'end;'
      ]
    );

    addGlobalFunc(
      'function GetToleranceSpeed: Integer;', [
      'begin',
      '  Result := Client.GetMFinder().GetToleranceSpeed();',
      'end;'
      ]
    );

    addGlobalFunc(
      'procedure SetToleranceSpeed2Modifiers(HueMod, SatMod: Extended);', [
      'begin',
      '  Client.GetMFinder().SetToleranceSpeed2Modifiers(HueMod, SatMod);',
      'end;'
      ]
    );

    addGlobalFunc(
      'procedure GetToleranceSpeed2Modifiers(out HueMod, SatMod: Extended);', [
      'begin',
      '  Client.GetMFinder().GetToleranceSpeed2Modifiers(HueMod, SatMod);',
      'end;'
      ]
    );

    addGlobalFunc(
      'procedure SetToleranceSpeed3Modifier(Modifier: Extended);', [
      'begin',
      '  Client.GetMFinder().SetToleranceSpeed3Modifier(Modifier);',
      'end;'
      ]
    );

    addGlobalFunc(
      'function GetToleranceSpeed3Modifier: Extended;', [
      'begin',
      '  Result := Client.GetMFinder().GetToleranceSpeed3Modifier();',
      'end;'
      ]
    );

    // FindColor
    addGlobalFunc(
      'function FindColor(out X, Y: Integer; Color, X1, Y1, X2, Y2: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColor(X, Y, Color, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColor(out X, Y: Integer; Color: Integer; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColor(X, Y, Color, Area);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColor(out X, Y: Integer; Color: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColor(X, Y, Color, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );

    // FindColorTolerance
    addGlobalFunc(
      'function FindColorTolerance(out X, Y: Integer; Color, X1, Y1, X2, Y2, Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColorTolerance(X, Y, Color, Tolerance, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColorTolerance(out X, Y: Integer; Color, Tolerance: Integer; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColorTolerance(X, Y, Color, Tolerance, Area);',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColorTolerance(out X, Y: Integer; Color, Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColorTolerance(X, Y, Color, Tolerance, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );

    // FindColorsTolerance
    addGlobalFunc(
      'function FindColorsTolerance(out Points: TPointArray; Color, X1, Y1, X2, Y2, Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColorsTolerance(Points, Color, Tolerance, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColorsTolerance(Points, Color, Tolerance, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColorsTolerance(out Points: TPointArray; Color, Tolerance: Integer; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColorsTolerance(Points, Color, Tolerance, Area);',
      'end;'
      ]
    );

    // FindColors
    addGlobalFunc(
      'function FindColors(out Points: TPointArray; Color, X1, Y1, X2, Y2: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColors(Points, Color, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColors(out Points: TPointArray; Color: Integer): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColors(Points, Color, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function FindColors(out Points: TPointArray; Color: Integer; Area: TBox): Boolean; overload;', [
      'begin',
      '  Result := Client.GetMFinder().FindColors(Points, Color, Area);',
      'end;'
      ]
    );

    // CountColorsTolerance
    addGlobalFunc(
      'function CountColorTolerance(Color, X1, Y1, X2, Y2, Tolerance: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().CountColorTolerance(Color, Tolerance, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function CountColorTolerance(Color, Tolerance: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().CountColorTolerance(Color, Tolerance, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function CountColorTolerance(Color, Tolerance: Integer; Area: TBox): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().CountColorTolerance(Color, Tolerance, Area);',
      'end;'
      ]
    );

    // CountColors
    addGlobalFunc(
      'function CountColor(Color, X1, Y1, X2, Y2: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().CountColor(Color, TBox([X1, Y1, X2, Y2]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function CountColor(Color: Integer): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().CountColor(Color, TBox([-1, -1, -1, -1]));',
      'end;'
      ]
    );
    addGlobalFunc(
      'function CountColor(Color: Integer; Area: TBox): Integer; overload;', [
      'begin',
      '  Result := Client.GetMFinder().CountColor(Color, Area);',
      'end;'
      ]
    );

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

    addGlobalFunc(
      'function GetColor(X, Y: Integer): Integer;', [
      'begin',
      '  Result := Client.GetMFinder().GetColor(X, Y);',
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

    addGlobalFunc(
      'function SimilarColors(Color1, Color2, Tolerance: Integer): Boolean;', [
      'begin',
      '  Result := Client.GetMFinder().SimilarColors(Color1, Color2, Tolerance);',
      'end;'
      ]
    );

    {
    addGlobalFunc('function GetColor(X, Y: Integer): Integer', @_LapeGetColor);
    addGlobalFunc('function GetColors(const Coords: TPointArray): TIntegerArray', @_LapeGetColors);
    addGlobalFunc('function GetColorsMatrix(X1, Y1, X2, Y2: Integer): TIntegerMatrix', @_LapeGetColorsMatrix);
    addGlobalFunc('procedure SetColorToleranceSpeed(CTS: Integer)', @_LapeSetColorToleranceSpeed);
    addGlobalFunc('function GetToleranceSpeed: Integer', @_LapeGetToleranceSpeed);
    addGlobalFunc('procedure SetToleranceSpeed2Modifiers(Hue, Sat: Extended)', @_LapeSetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure GetToleranceSpeed2Modifiers(var Hue, Sat: Extended)', @_LapeGetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure SetToleranceSpeed3Modifier(Modifier: Extended)', @_LapeSetToleranceSpeed3Modifier);
    addGlobalFunc('function GetToleranceSpeed3Modifier: Extended', @_LapeGetToleranceSpeed3Modifier);
    addGlobalFunc('function SimilarColors(Color1, Color2, Tol: Integer): Boolean', @_LapeSimilarColors);

    addGlobalFunc('function FindTemplate(Templ: TMufasaBitmap; out X, Y: Integer; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @_LapeFindTemplate);
    addGlobalFunc('function FindTemplateEx(Templ: TMufasaBitmap; out TPA: TPointArray; Formula: ETMFormula; xs,ys,xe,ye: Integer; MinMatch: Extended; DynamicAdjust: Boolean = True): Boolean', @_LapeFindTemplateEx);
}
    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFinder);

end.

