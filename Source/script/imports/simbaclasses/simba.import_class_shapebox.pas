unit simba.import_class_shapebox;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, controls, extctrls, comctrls, graphics, lptypes, ffi,
  simba.script_compiler, simba.mufasatypes, simba.imagebox,
  simba.shapebox;

type
  PComponent = ^TComponent;
  PPanel = ^TPanel;

procedure _LapeSimbaShapeBox_PointCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.PointCount;
end;

procedure _LapeSimbaShapeBox_PointName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PSimbaShapeBox(Params^[0])^.PointName[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_Point_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PSimbaShapeBox(Params^[0])^.Point[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_PathCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.PathCount;
end;

procedure _LapeSimbaShapeBox_PathName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PSimbaShapeBox(Params^[0])^.PathName[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_Path_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSimbaShapeBox(Params^[0])^.Path[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_PolyCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.PolyCount;
end;

procedure _LapeSimbaShapeBox_PolyName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PSimbaShapeBox(Params^[0])^.PolyName[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_Poly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := PSimbaShapeBox(Params^[0])^.Poly[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_BoxCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.BoxCount;
end;

procedure _LapeSimbaShapeBox_BoxName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PSimbaShapeBox(Params^[0])^.BoxName[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_Box_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PSimbaShapeBox(Params^[0])^.Box[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaImageBox(Params^[0])^ := TSimbaShapeBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_LeftPanel(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPanel(Result)^ := PSimbaShapeBox(Params^[0])^.LeftPanel;
end;

procedure _LapeSimbaShapeBox_NewPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.NewPoint();
end;

procedure _LapeSimbaShapeBox_NewBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.NewBox();
end;

procedure _LapeSimbaShapeBox_NewPoly(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.NewPoly();
end;

procedure _LapeSimbaShapeBox_NewPath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.NewPath();
end;

procedure _LapeSimbaShapeBox_DeletePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.DeletePoint(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_DeletePath(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.DeletePath(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_DeleteBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.DeleteBox(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_DeletePoly(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.DeletePoly(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_AddPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.AddPoint(PPoint(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_AddBox(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.AddBox(PBox(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_AddPoly(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.AddPoly(PPointArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_AddPath(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSimbaShapeBox(Params^[0])^.AddPath(PPointArray(Params^[1])^, PString(Params^[2])^);
end;

procedure ImportSimbaShapeBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TSimbaShapeBox', 'TSimbaImageBox');

    addClassVar('TSimbaShapeBox', 'PointCount', 'Integer', @_LapeSimbaShapeBox_PointCount_Read);
    addClassVar('TSimbaShapeBox', 'PointName', 'String', @_LapeSimbaShapeBox_PointName_Read, nil, True);
    addClassVar('TSimbaShapeBox', 'Point', 'TPoint', @_LapeSimbaShapeBox_Point_Read, nil, True);

    addClassVar('TSimbaShapeBox', 'PathCount', 'Integer', @_LapeSimbaShapeBox_PathCount_Read);
    addClassVar('TSimbaShapeBox', 'PathName', 'String', @_LapeSimbaShapeBox_PathName_Read, nil, True);
    addClassVar('TSimbaShapeBox', 'Path', 'TPointArray', @_LapeSimbaShapeBox_Path_Read, nil, True);

    addClassVar('TSimbaShapeBox', 'PolyCount', 'Integer', @_LapeSimbaShapeBox_PolyCount_Read);
    addClassVar('TSimbaShapeBox', 'PolyName', 'String', @_LapeSimbaShapeBox_PolyName_Read, nil, True);
    addClassVar('TSimbaShapeBox', 'Poly', 'TPointArray', @_LapeSimbaShapeBox_Poly_Read, nil, True);

    addClassVar('TSimbaShapeBox', 'BoxCount', 'Integer', @_LapeSimbaShapeBox_BoxCount_Read);
    addClassVar('TSimbaShapeBox', 'BoxName', 'String', @_LapeSimbaShapeBox_BoxName_Read, nil, True);
    addClassVar('TSimbaShapeBox', 'Box', 'TBox', @_LapeSimbaShapeBox_Box_Read, nil, True);

    addGlobalFunc('procedure TSimbaShapeBox.Init(Owner: TComponent); override', @_LapeSimbaShapeBox_Init);

    addGlobalFunc('procedure TSimbaShapeBox.SaveToFile(FileName: String);', @_LapeSimbaShapeBox_SaveToFile);
    addGlobalFunc('procedure TSimbaShapeBox.LoadFromFile(FileName: String);', @_LapeSimbaShapeBox_LoadFromFile);

    addGlobalFunc('function TSimbaShapeBox.NewPoint: Integer;', @_LapeSimbaShapeBox_NewPoint);
    addGlobalFunc('function TSimbaShapeBox.NewBox: Integer;', @_LapeSimbaShapeBox_NewBox);
    addGlobalFunc('function TSimbaShapeBox.NewPoly: Integer;', @_LapeSimbaShapeBox_NewPoly);
    addGlobalFunc('function TSimbaShapeBox.NewPath: Integer;', @_LapeSimbaShapeBox_NewPath);

    addGlobalFunc('procedure TSimbaShapeBox.DeletePoint(Index: Integer);', @_LapeSimbaShapeBox_DeletePoint);
    addGlobalFunc('procedure TSimbaShapeBox.DeleteBox(Index: Integer);', @_LapeSimbaShapeBox_DeleteBox);
    addGlobalFunc('procedure TSimbaShapeBox.DeletePoly(Index: Integer);', @_LapeSimbaShapeBox_DeletePoly);
    addGlobalFunc('procedure TSimbaShapeBox.DeletePath(Index: Integer);', @_LapeSimbaShapeBox_DeletePath);

    addGlobalFunc('procedure TSimbaShapeBox.AddPoint(Point: TPoint; AName: String = "");', @_LapeSimbaShapeBox_AddPoint);
    addGlobalFunc('procedure TSimbaShapeBox.AddBox(Box: TBox; AName: String = "");', @_LapeSimbaShapeBox_AddBox);
    addGlobalFunc('procedure TSimbaShapeBox.AddPoly(Poly: TPointArray; AName: String = "");', @_LapeSimbaShapeBox_AddPoly);
    addGlobalFunc('procedure TSimbaShapeBox.AddPath(Path: TPointArray; AName: String = "");', @_LapeSimbaShapeBox_AddPath);

    addGlobalFunc('function TSimbaShapeBox.LeftPanel: TPanel;', @_LapeSimbaShapeBox_LeftPanel);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportSimbaShapeBox);

end.

