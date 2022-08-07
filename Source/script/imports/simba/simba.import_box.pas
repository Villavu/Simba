unit simba.import_box;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.mufasatypes, simba.script_compiler;

procedure _LapeBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeBoxEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_Create1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := TBox.Create(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeBox_Create2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := TBox.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_EqualDimensions(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.EqualDimensions(PBox(Params^[1])^);
end;

procedure _LapeBox_Area(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Area();
end;

procedure _LapeBox_Expand1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

procedure _LapeBox_Expand2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_Contains1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PBox(Params^[1])^);
end;

procedure _LapeBox_Contains2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PPoint(Params^[1])^);
end;

procedure _LapeBox_Contains3(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBox(Params^[0])^.Contains(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_Partition(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBox(Params^[0])^.Partition(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_Offset1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBox_Offset2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure _LapeBox_Combine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBox(Params^[0])^.Combine(PBox(Params^[1])^);
end;

procedure _LapeBox_Invert(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBox(Params^[0])^.Invert(PBox(Params^[1])^);
end;

procedure _LapeBox_NearestEdge(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.NearestEdge(PPoint(Params^[1])^);
end;

procedure _LapeBox_Intersect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.Intersect(PPoint(Params^[1])^);
end;

procedure _LapeBox_Clip(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Params^[0])^.Clip(PBox(Params^[1])^);
end;

procedure _LapeBox_Normalize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Params^[0])^.Normalize();
end;

procedure _LapeBox_Width(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Width;
end;

procedure _LapeBox_Height(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBox(Params^[0])^.Height;
end;

procedure _LapeBox_Center(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PBox(Params^[0])^.Center;
end;

procedure _LapeBoxArray_Create(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := TBoxArray.Create(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^, PPoint(Params^[5])^);
end;

procedure _LapeBoxArray_Pack(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Pack();
end;

procedure _LapeBoxArray_SortFrom(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortFrom(PPoint(Params^[1])^);
end;

procedure _LapeBoxArray_SortByX(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByX(PBoolean(Params^[1])^);
end;

procedure _LapeBoxArray_SortByY(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByY(PBoolean(Params^[1])^);
end;

procedure _LapeBoxArray_SortByWidth(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByWidth(PBoolean(Params^[1])^);
end;

procedure _LapeBoxArray_SortByHeight(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByHeight(PBoolean(Params^[1])^);
end;

procedure _LapeBoxArray_SortByArea(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Params^[0])^.SortByArea(PBoolean(Params^[1])^);
end;

procedure _LapeBoxArray_Merge(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PBoxArray(Params^[0])^.Merge();
end;

procedure _LapeBoxArray_Centers(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PBoxArray(Params^[0])^.Centers();
end;

procedure _LapeBoxArray_Offset1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure _LapeBoxArray_Offset2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Offset(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeBoxArray_Expand1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^);
end;

procedure _LapeBoxArray_Expand2(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoxArray(Result)^ := PBoxArray(Params^[0])^.Expand(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure ImportBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Box');

    addGlobalFunc('function Box(X1, Y1, X2, Y2: Integer): TBox; overload', @_LapeBox);
    addGlobalFunc('function Box(Mid: TPoint; XRad, YRad: Integer): TBox; overload', @_LapeBoxEx);

    addGlobalFunc('function TBox.Create(X1, Y1, X2, Y2: Integer): TBox; static; overload;', @_LapeBox_Create1);
    addGlobalFunc('function TBox.Create(Center: TPoint; XRad, YRad: Integer): TBox; static; overload;', @_LapeBox_Create2);

    addGlobalFunc('function TBox.EqualDimensions(Other: TBox): Boolean;', @_LapeBox_EqualDimensions);
    addGlobalFunc('function TBox.Area: Integer;', @_LapeBox_Area);
    addGlobalFunc('function TBox.Expand(SizeMod: Integer): TBox; overload;', @_LapeBox_Expand1);
    addGlobalFunc('function TBox.Expand(WidMod, HeiMod: Integer): TBox; overload;', @_LapeBox_Expand2);
    addGlobalFunc('function TBox.Contains(Other: TBox): Boolean; overload;', @_LapeBox_Contains1);
    addGlobalFunc('function TBox.Contains(Other: TPoint): Boolean; overload;', @_LapeBox_Contains2);
    addGlobalFunc('function TBox.Contains(X, Y: Integer): Boolean; overload;', @_LapeBox_Contains3);
    addGlobalFunc('function TBox.Partition(Rows, Cols: Int32): TBoxArray;', @_LapeBox_Partition);
    addGlobalFunc('function TBox.Offset(X, Y: Integer): TBox; overload;', @_LapeBox_Offset1);
    addGlobalFunc('function TBox.Offset(P: TPoint): TBox; overload;', @_LapeBox_Offset2);
    addGlobalFunc('function TBox.Combine(Other: TBox): TBox;', @_LapeBox_Combine);
    addGlobalFunc('function TBox.Invert(Area: TBox): TBoxArray;', @_LapeBox_Invert);

    addGlobalFunc('function TBox.NearestEdge(P: TPoint): TPoint;', @_LapeBox_NearestEdge);
    addGlobalFunc('function TBox.Intersect(P: TPoint): TPoint;', @_LapeBox_Intersect);

    addGlobalFunc('procedure TBox.Clip(Other: TBox);', @_LapeBox_Clip);
    addGlobalFunc('procedure TBox.Normalize;', @_LapeBox_Normalize);

    addGlobalFunc('function TBox.Width: Integer;', @_LapeBox_Width);
    addGlobalFunc('function TBox.Height: Integer;', @_LapeBox_Height);
    addGlobalFunc('function TBox.Center: TPoint;', @_LapeBox_Center);

    addGlobalFunc('function TBoxArray.Create(Start: TPoint; Columns, Rows, Width, Height: Int32; Spacing: TPoint): TBoxArray; static;', @_LapeBoxArray_Create);
    addGlobalFunc('function TBoxArray.Pack: TBoxArray;', @_LapeBoxArray_Pack);

    addGlobalFunc('procedure TBoxArray.SortFrom(From: TPoint);', @_LapeBoxArray_SortFrom);
    addGlobalFunc('procedure TBoxArray.SortByX(LowToHigh: Boolean);', @_LapeBoxArray_SortByX);
    addGlobalFunc('procedure TBoxArray.SortByY(LowToHigh: Boolean);', @_LapeBoxArray_SortByY);
    addGlobalFunc('procedure TBoxArray.SortByWidth(LowToHigh: Boolean);', @_LapeBoxArray_SortByWidth);
    addGlobalFunc('procedure TBoxArray.SortByHeight(LowToHigh: Boolean);', @_LapeBoxArray_SortByHeight);
    addGlobalFunc('procedure TBoxArray.SortByArea(LowToHigh: Boolean);', @_LapeBoxArray_SortByArea);

    addGlobalFunc('function TBoxArray.Merge: TBox;', @_LapeBoxArray_Merge);
    addGlobalFunc('function TBoxArray.Centers: TPointArray;', @_LapeBoxArray_Centers);
    addGlobalFunc('function TBoxArray.Offset(P: TPoint): TBoxArray; overload;', @_LapeBoxArray_Offset1);
    addGlobalFunc('function TBoxArray.Offset(X, Y: Integer): TBoxArray; overload;', @_LapeBoxArray_Offset2);

    addGlobalFunc('function TBoxArray.Expand(SizeMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand1);
    addGlobalFunc('function TBoxArray.Expand(WidMod, HeiMod: Integer): TBoxArray; overload;', @_LapeBoxArray_Expand2);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportBox);

end.

