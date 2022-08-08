unit simba.import_point;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.mufasatypes, simba.script_compiler;

procedure _LapePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Point(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapePoint_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := TPoint.Create(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapePoint_InPolygon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InPolygon(PPointArray(Params^[1])^);
end;

procedure _LapePoint_InCircle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InCircle(PPoint(Params^[1])^, PDouble(Params^[2])^);
end;

procedure _LapePoint_InTriangle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InTriangle(PPoint(Params^[1])^, PPoint(Params^[2])^, PPoint(Params^[3])^);
end;

procedure _LapePoint_InBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PPoint(Params^[0])^.InBox(PBox(Params^[1])^);
end;

procedure _LapePoint_DistanceTo(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PPoint(Params^[0])^.DistanceTo(PPoint(Params^[1])^);
end;

procedure _LapePoint_Rotate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Rotate(PDouble(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapePoint_RotateFast(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.RotateFast(PInteger(Params^[1])^, PPoint(Params^[2])^);
end;

procedure _LapePoint_Magnitude(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PPoint(Params^[0])^.Magnitude();
end;

procedure _LapePoint_AngleBetween(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PDouble(Result)^ := PPoint(Params^[0])^.AngleBetween(PPoint(Params^[1])^);
end;

procedure _LapePoint_Offset1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePoint_Offset2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure _LapePoint_Random1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePoint_Random2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInteger(Params^[1])^);
end;

procedure ImportPoint(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Point');

    addGlobalFunc('function Point(X, Y: Integer): TPoint', @_LapePoint);

    addGlobalFunc('function TPoint.Create(X, Y: Integer): TPoint; static;', @_LapePoint_Create);
    addGlobalFunc('function TPoint.InTriangle(A, B, C: TPoint): Boolean;', @_LapePoint_InTriangle);
    addGlobalFunc('function TPoint.InPolygon(Poly: TPointArray): Boolean;', @_LapePoint_InPolygon);
    addGlobalFunc('function TPoint.InCircle(Center: TPoint; Radius: Double): Boolean;', @_LapePoint_InCircle);
    addGlobalFunc('function TPoint.InBox(Box: TBox): Boolean;', @_LapePoint_InBox);
    addGlobalFunc('function TPoint.DistanceTo(Other: TPoint): Double;', @_LapePoint_DistanceTo);
    addGlobalFunc('function TPoint.Rotate(Radians: Double; Center: TPoint): TPoint;', @_LapePoint_Rotate);
    addGlobalFunc('function TPoint.RotateFast(Degrees: Integer; Center: TPoint): TPoint;', @_LapePoint_RotateFast);
    addGlobalFunc('function TPoint.Magnitude: Double;', @_LapePoint_Magnitude);
    addGlobalFunc('function TPoint.AngleBetween(Other: TPoint): Double;', @_LapePoint_AngleBetween);
    addGlobalFunc('function TPoint.Offset(X, Y: Integer): TPoint; overload;', @_LapePoint_Offset1);
    addGlobalFunc('function TPoint.Offset(P: TPoint): TPoint; overload;', @_LapePoint_Offset2);
    addGlobalFunc('function TPoint.Random(Min, Max: Integer): TPoint; overload;', @_LapePoint_Random1);
    addGlobalFunc('function TPoint.Random(Value: Integer): TPoint; overload;', @_LapePoint_Random2);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportPoint);

end.

