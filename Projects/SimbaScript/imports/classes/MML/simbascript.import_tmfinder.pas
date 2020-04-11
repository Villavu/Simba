unit simbascript.import_tmfinder;
//Depends: TMFinder, TObject, boolean, integer, Integer, TPointArray, TMask, TMufasaBitmap, Boolean, Extended, TMDTM, T2DExtendedArray, array of integer, TPRGB32Array

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_TMFinder(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.finder, simba.bitmap, simba.dtm;

type
  PMFinder = ^TMFinder;
  PObject = ^TObject;
  PMDTM = ^TMDTM;
  PCTSInfoArray = ^TCTSInfoArray;
  PCTSInfo2DArray = ^TCTSInfo2DArray;

//Read: WarnOnly : boolean;
procedure Lape_TMFinder_WarnOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFinder(Params^[0])^.WarnOnly;
end;

//Write: WarnOnly : boolean;
procedure Lape_TMFinder_WarnOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.WarnOnly := Pboolean(Params^[1])^;
end;

//procedure DefaultOperations(var xs,ys,xe,ye : integer);
procedure Lape_TMFinder_DefaultOperations(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.DefaultOperations(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//function CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer;
procedure Lape_TMFinder_CountColorTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.CountColorTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

//function CountColor(Color, xs, ys, xe, ye: Integer): Integer;
procedure Lape_TMFinder_CountColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.CountColor(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^);
end;

//function SimilarColors(Color1,Color2,Tolerance : Integer) : boolean;
procedure Lape_TMFinder_SimilarColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFinder(Params^[0])^.SimilarColors(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//function FindColor(out x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean;
procedure Lape_TMFinder_FindColor(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColor(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean;
procedure Lape_TMFinder_FindColorSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColorSpiral(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye,Tol: Integer): Boolean;
procedure Lape_TMFinder_FindColorSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColorSpiralTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//function FindColorTolerance(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean;
procedure Lape_TMFinder_FindColorTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColorTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//function FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean;
procedure Lape_TMFinder_FindColorsTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColorsTolerance(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindColorsSpiralTolerance(x, y: Integer; out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tol: Integer) : boolean;
procedure Lape_TMFinder_FindColorsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMFinder(Params^[0])^.FindColorsSpiralTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PPointArray(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PInteger(Params^[9])^);
end;

//function FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean;
procedure Lape_TMFinder_FindColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColors(PPointArray(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^);
end;

//function FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea: Integer): Boolean;
procedure Lape_TMFinder_FindColoredArea(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColoredArea(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//function FindColoredAreaTolerance(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea, tol: Integer): Boolean;
procedure Lape_TMFinder_FindColoredAreaTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindColoredAreaTolerance(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PInteger(Params^[9])^);
end;

//function FindMaskTolerance(const mask: TMask; out x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean;
procedure Lape_TMFinder_FindMaskTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindMaskTolerance(PMask(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PInteger(Params^[9])^);
end;

//procedure CheckMask(const Mask : TMask);
procedure Lape_TMFinder_CheckMask(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.CheckMask(PMask(Params^[1])^);
end;

//function FindBitmap(bitmap: TMufasaBitmap; out x, y: Integer): Boolean;
procedure Lape_TMFinder_FindBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmap(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//function FindBitmapIn(bitmap: TMufasaBitmap; out x, y: Integer;  xs, ys, xe, ye: Integer): Boolean;
procedure Lape_TMFinder_FindBitmapIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapIn(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean;
procedure Lape_TMFinder_FindBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapToleranceIn(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^);
end;

//function FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean;
procedure Lape_TMFinder_FindBitmapSpiral(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapSpiral(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean;
procedure Lape_TMFinder_FindBitmapSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapSpiralTolerance(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, Pinteger(Params^[7])^, Pinteger(Params^[8])^);
end;

//function FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x, y: Integer; out Points : TPointArray; xs, ys, xe, ye,tolerance: Integer; maxToFind: Integer = 0): Boolean;
procedure Lape_TMFinder_FindBitmapsSpiralTolerance(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindBitmapsSpiralTolerance(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PPointArray(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PInteger(Params^[9])^, PInteger(Params^[10])^);
end;

//function FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; out accuracy: Extended): Boolean;
procedure Lape_TMFinder_FindDeformedBitmapToleranceIn(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDeformedBitmapToleranceIn(PMufasaBitmap(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PInteger(Params^[8])^, PInteger(Params^[9])^, PBoolean(Params^[10])^, PExtended(Params^[11])^);
end;

//function FindDTM(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
procedure Lape_TMFinder_FindDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTM(PMDTM(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindDTMs(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2 : integer; maxToFind: Integer = 0): Boolean;
procedure Lape_TMFinder_FindDTMs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTMs(PMDTM(Params^[1])^, PPointArray(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, Pinteger(Params^[5])^, Pinteger(Params^[6])^, PInteger(Params^[7])^);
end;

//function FindDTMRotated(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended; Alternating : boolean): Boolean;
procedure Lape_TMFinder_FindDTMRotated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTMRotated(PMDTM(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PInteger(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, PExtended(Params^[10])^, PExtended(Params^[11])^, Pboolean(Params^[12])^);
end;

//function FindDTMsRotated(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray;Alternating : boolean; maxToFind: Integer = 0): Boolean;
procedure Lape_TMFinder_FindDTMsRotated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFinder(Params^[0])^.FindDTMsRotated(PMDTM(Params^[1])^, PPointArray(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PInteger(Params^[5])^, PInteger(Params^[6])^, PExtended(Params^[7])^, PExtended(Params^[8])^, PExtended(Params^[9])^, P2DExtendedArray(Params^[10])^, Pboolean(Params^[11])^, PInteger(Params^[12])^);
end;

//function GetColors(const Coords: TPointArray): TIntegerArray;
procedure Lape_TMFinder_GetColors(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := PMFinder(Params^[0])^.GetColors(PPointArray(Params^[1])^);
end;

//procedure SetToleranceSpeed(nCTS: Integer);
procedure Lape_TMFinder_SetToleranceSpeed(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.SetToleranceSpeed(PInteger(Params^[1])^);
end;

//function GetToleranceSpeed: Integer;
procedure Lape_TMFinder_GetToleranceSpeed(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFinder(Params^[0])^.GetToleranceSpeed();
end;

//procedure SetToleranceSpeed2Modifiers(const nHue, nSat: Extended);
procedure Lape_TMFinder_SetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.SetToleranceSpeed2Modifiers(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

//procedure GetToleranceSpeed2Modifiers(out hMod, sMod: Extended);
procedure Lape_TMFinder_GetToleranceSpeed2Modifiers(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.GetToleranceSpeed2Modifiers(PExtended(Params^[1])^, PExtended(Params^[2])^);
end;

//procedure SetToleranceSpeed3Modifier(modifier: Extended);
procedure Lape_TMFinder_SetToleranceSpeed3Modifier(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.SetToleranceSpeed3Modifier(PExtended(Params^[1])^);
end;

//function GetToleranceSpeed3Modifier: Extended;
procedure Lape_TMFinder_GetToleranceSpeed3Modifier(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := PMFinder(Params^[0])^.GetToleranceSpeed3Modifier();
end;

//function Create_CTSInfo(Color, Tolerance: Integer): Pointer; overload;
procedure Lape_TMFinder_Create_CTSInfo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PMFinder(Params^[0])^.Create_CTSInfo(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function Create_CTSInfo(R, G, B, Tolerance: Integer): Pointer; overload;
procedure Lape_TMFinder_Create_CTSInfoEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PMFinder(Params^[0])^.Create_CTSInfo(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//function Create_CTSInfoArray(color, tolerance: array of integer): TCTSInfoArray;
procedure Lape_TMFinder_Create_CTSInfoArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCTSInfoArray(Result)^ := PMFinder(Params^[0])^.Create_CTSInfoArray(PIntegerArray(Params^[1])^, PIntegerArray(Params^[2])^);
end;

//function Create_CTSInfo2DArray(w, h: integer; data: TPRGB32Array; Tolerance: Integer): TCTSInfo2DArray;
procedure Lape_TMFinder_Create_CTSInfo2DArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCTSInfo2DArray(Result)^ := PMFinder(Params^[0])^.Create_CTSInfo2DArray(Pinteger(Params^[1])^, Pinteger(Params^[2])^, PPRGB32Array(Params^[3])^, PInteger(Params^[4])^);
end;

//constructor Create(aClient: TObject);
procedure Lape_TMFinder_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^ := TMFinder.Create(PObject(Params^[1])^);
end;

//procedure Free();
procedure Lape_TMFinder_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Params^[0])^.Free();
end;

procedure Lape_Import_TMFinder(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMFinder');

    addGlobalType('Pointer', 'TCTSInfo');
    addGlobalType('array of TCTSInfo', 'TCTSInfoArray');
    addGlobalType('array of TCTSInfoArray', 'TCTSInfo2DArray');

    addClassVar('TMFinder', 'WarnOnly', 'boolean', @Lape_TMFinder_WarnOnly_Read, @Lape_TMFinder_WarnOnly_Write);
    addGlobalFunc('procedure TMFinder.DefaultOperations(var xs,ys,xe,ye : integer); constref;', @Lape_TMFinder_DefaultOperations);
    addGlobalFunc('function TMFinder.CountColorTolerance(Color, xs, ys, xe, ye, Tolerance: Integer): Integer; constref;', @Lape_TMFinder_CountColorTolerance);
    addGlobalFunc('function TMFinder.CountColor(Color, xs, ys, xe, ye: Integer): Integer; constref;', @Lape_TMFinder_CountColor);
    addGlobalFunc('function TMFinder.SimilarColors(Color1,Color2,Tolerance : Integer): boolean; constref;', @Lape_TMFinder_SimilarColors);
    addGlobalFunc('function TMFinder.FindColor(out x, y: Integer; Color, xs, ys, xe, ye: Integer): Boolean; constref;', @Lape_TMFinder_FindColor);
    addGlobalFunc('function TMFinder.FindColorSpiral(var x, y: Integer; color, xs, ys, xe, ye: Integer): Boolean; constref;', @Lape_TMFinder_FindColorSpiral);
    addGlobalFunc('function TMFinder.FindColorSpiralTolerance(var x, y: Integer; color, xs, ys, xe, ye,Tol: Integer): Boolean; constref;', @Lape_TMFinder_FindColorSpiralTolerance);
    addGlobalFunc('function TMFinder.FindColorTolerance(out x, y: Integer; Color, xs, ys, xe, ye, tol: Integer): Boolean; constref;', @Lape_TMFinder_FindColorTolerance);
    addGlobalFunc('function TMFinder.FindColorsTolerance(out Points: TPointArray; Color, xs, ys, xe, ye, Tol: Integer): Boolean; constref;', @Lape_TMFinder_FindColorsTolerance);
    addGlobalFunc('function TMFinder.FindColorsSpiralTolerance(x, y: Integer; out Points: TPointArray; color, xs, ys, xe, ye: Integer; Tol: Integer): boolean; constref;', @Lape_TMFinder_FindColorsSpiralTolerance);
    addGlobalFunc('function TMFinder.FindColors(var TPA: TPointArray; Color, xs, ys, xe, ye: Integer): Boolean; constref;', @Lape_TMFinder_FindColors);
    addGlobalFunc('function TMFinder.FindColoredArea(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea: Integer): Boolean; constref;', @Lape_TMFinder_FindColoredArea);
    addGlobalFunc('function TMFinder.FindColoredAreaTolerance(var x, y: Integer; color, xs, ys, xe, ye: Integer; MinArea, tol: Integer): Boolean; constref;', @Lape_TMFinder_FindColoredAreaTolerance);
    addGlobalFunc('function TMFinder.FindMaskTolerance(const mask: TMask; out x, y: Integer; xs, ys, xe, ye: Integer; Tolerance, ContourTolerance: Integer): Boolean; constref;', @Lape_TMFinder_FindMaskTolerance);
    addGlobalFunc('procedure TMFinder.CheckMask(const Mask : TMask); constref;', @Lape_TMFinder_CheckMask);
    addGlobalFunc('function TMFinder.FindBitmap(bitmap: TMufasaBitmap; out x, y: Integer): Boolean; constref;', @Lape_TMFinder_FindBitmap);
    addGlobalFunc('function TMFinder.FindBitmapIn(bitmap: TMufasaBitmap; out x, y: Integer;  xs, ys, xe, ye: Integer): Boolean; constref;', @Lape_TMFinder_FindBitmapIn);
    addGlobalFunc('function TMFinder.FindBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer): Boolean; constref;', @Lape_TMFinder_FindBitmapToleranceIn);
    addGlobalFunc('function TMFinder.FindBitmapSpiral(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye: Integer): Boolean; constref;', @Lape_TMFinder_FindBitmapSpiral);
    addGlobalFunc('function TMFinder.FindBitmapSpiralTolerance(bitmap: TMufasaBitmap; var x, y: Integer; xs, ys, xe, ye,tolerance : integer): Boolean; constref;', @Lape_TMFinder_FindBitmapSpiralTolerance);
    addGlobalFunc('function TMFinder.FindBitmapsSpiralTolerance(bitmap: TMufasaBitmap; x, y: Integer; out Points : TPointArray; xs, ys, xe, ye,tolerance: Integer; maxToFind: Integer = 0): Boolean; constref;', @Lape_TMFinder_FindBitmapsSpiralTolerance);
    addGlobalFunc('function TMFinder.FindDeformedBitmapToleranceIn(bitmap: TMufasaBitmap; out x, y: Integer; xs, ys, xe, ye: Integer; tolerance: Integer; Range: Integer; AllowPartialAccuracy: Boolean; out accuracy: Extended): Boolean; constref;', @Lape_TMFinder_FindDeformedBitmapToleranceIn);
    addGlobalFunc('function TMFinder.FindDTM(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer): Boolean; constref;', @Lape_TMFinder_FindDTM);
    addGlobalFunc('function TMFinder.FindDTMs(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2 : integer; maxToFind: Integer = 0): Boolean; constref;', @Lape_TMFinder_FindDTMs);
    addGlobalFunc('function TMFinder.FindDTMRotated(DTM: TMDTM; out x, y: Integer; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: Extended; Alternating : boolean): Boolean; constref;', @Lape_TMFinder_FindDTMRotated);
    addGlobalFunc('function TMFinder.FindDTMsRotated(DTM: TMDTM; out Points: TPointArray; x1, y1, x2, y2: Integer; sAngle, eAngle, aStep: Extended; out aFound: T2DExtendedArray;Alternating : boolean; maxToFind: Integer = 0): Boolean; constref;', @Lape_TMFinder_FindDTMsRotated);
    addGlobalFunc('function TMFinder.GetColors(const Coords: TPointArray): TIntegerArray; constref;', @Lape_TMFinder_GetColors);
    addGlobalFunc('procedure TMFinder.SetToleranceSpeed(nCTS: Integer); constref;', @Lape_TMFinder_SetToleranceSpeed);
    addGlobalFunc('function TMFinder.GetToleranceSpeed(): Integer; constref;', @Lape_TMFinder_GetToleranceSpeed);
    addGlobalFunc('procedure TMFinder.SetToleranceSpeed2Modifiers(const nHue, nSat: Extended); constref;', @Lape_TMFinder_SetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure TMFinder.GetToleranceSpeed2Modifiers(out hMod, sMod: Extended); constref;', @Lape_TMFinder_GetToleranceSpeed2Modifiers);
    addGlobalFunc('procedure TMFinder.SetToleranceSpeed3Modifier(modifier: Extended); constref;', @Lape_TMFinder_SetToleranceSpeed3Modifier);
    addGlobalFunc('function TMFinder.GetToleranceSpeed3Modifier(): Extended; constref;', @Lape_TMFinder_GetToleranceSpeed3Modifier);
    addGlobalFunc('function TMFinder.Create_CTSInfo(Color, Tolerance: Integer): Pointer; constref;', @Lape_TMFinder_Create_CTSInfo);
    addGlobalFunc('function TMFinder.Create_CTSInfo(R, G, B, Tolerance: Integer): Pointer; constref; overload;', @Lape_TMFinder_Create_CTSInfoEx);
    addGlobalFunc('function TMFinder.Create_CTSInfoArray(color, tolerance: array of integer): TCTSInfoArray; constref;', @Lape_TMFinder_Create_CTSInfoArray);
    addGlobalFunc('function TMFinder.Create_CTSInfo2DArray(w, h: integer; data: TPRGB32Array; Tolerance: Integer): TCTSInfo2DArray; constref;', @Lape_TMFinder_Create_CTSInfo2DArray);
    addGlobalFunc('procedure TMFinder.Init(aClient: TObject);', @Lape_TMFinder_Init);
    //addGlobalFunc('procedure TMFinder.Free(); constref;', @Lape_TMFinder_Free);
  end;
end;

end.

