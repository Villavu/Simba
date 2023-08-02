unit simba.import_class_shapebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportSimbaShapeBox(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, stdctrls, extctrls, comctrls, graphics, lptypes, ffi,
  simba.imagebox, simba.shapebox;

type
  PComponent = ^TComponent;
  PPanel = ^TPanel;
  PButton = ^TButton;

procedure _LapeSimbaShapeBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageBox(Result)^ := TSimbaShapeBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeSimbaShapeBox_SaveToFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_PointButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.PointButton;
end;

procedure _LapeSimbaShapeBox_BoxButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.BoxButton;
end;

procedure _LapeSimbaShapeBox_PathButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.PathButton;
end;

procedure _LapeSimbaShapeBox_PrintButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.PrintButton;
end;

procedure _LapeSimbaShapeBox_PolyButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.PolyButton;
end;

procedure _LapeSimbaShapeBox_NameButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.NameButton;
end;

procedure _LapeSimbaShapeBox_DeleteButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.DeleteButton;
end;

procedure _LapeSimbaShapeBox_DeleteAllButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.DeleteAllButton;
end;

procedure _LapeSimbaShapeBox_Panel(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanel(Result)^ := PSimbaShapeBox(Params^[0])^.Panel;
end;

procedure _LapeSimbaShapeBox_ManualAddPoint1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddPoint(PPoint(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_ManualAddPoint2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddPoint(PPoint(Params^[1])^, PString(Params^[2])^, Params^[3]^);
end;

procedure _LapeSimbaShapeBox_ManualAddBox1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddBox(PBox(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_ManualAddBox2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddBox(PBox(Params^[1])^, PString(Params^[2])^, Params^[3]^);
end;

procedure _LapeSimbaShapeBox_ManualAddPoly1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddPoly(PPointArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_ManualAddPoly2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddPoly(PPointArray(Params^[1])^, PString(Params^[2])^, Params^[3]^);
end;

procedure _LapeSimbaShapeBox_ManualAddPath1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddPath(PPointArray(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeSimbaShapeBox_ManualAddPath2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.ManualAddPath(PPointArray(Params^[1])^, PString(Params^[2])^, Params^[3]^);
end;

procedure _LapeSimbaShapeBox_GetShape(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PShapeBoxShape(Result)^ := PSimbaShapeBox(Params^[0])^.Shape[PInteger(Params^[1])^];
end;

procedure _LapeSimbaShapeBox_GetShapeIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.ShapeIndex;
end;

procedure _LapeSimbaShapeBox_GetShapeCount(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.ShapeCount;
end;

procedure _LapeSimbaShapeBox_UserDataSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.UserDataSize;
end;

procedure _LapeSimbaShapeBox_UserDataSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.UserDataSize := PInteger(Params^[1])^;
end;

procedure _LapeSimbaShapeBox_QueryNameOnNew_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaShapeBox(Params^[0])^.QueryNameOnNew;
end;

procedure _LapeSimbaShapeBox_QueryNameOnNew_WRite(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.QueryNameOnNew := PBoolean(Params^[1])^;
end;

procedure _LapeSimbaShapeBox_DeleteShape(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.DeleteShape(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_DeleteAllShapes(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.DeleteAllShapes();
end;

procedure ImportSimbaShapeBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TSimbaShapeBox', 'TSimbaImageBox');

    addGlobalType([
      'record',
        'Name: String;',
        'Index: Integer;',
        'UserData: Pointer;',
        '',
        'IsBox: Boolean;',
        'IsPoint: Boolean;',
        'IsPoly: Boolean;',
        'IsPath: Boolean;',
        '',
        'Box: TBox;',
        'Point: TPoint;',
        'Path: TPointArray;',
        'Poly: TPointArray;',
      'end;'
    ], 'TShapeBoxShape');

    addClassConstructor('TSimbaShapeBox', '(Owner: TLazComponent)', @_LapeSimbaShapeBox_Create);

    addGlobalFunc('function TSimbaShapeBox.GetShape(Index: Integer): TShapeBoxShape', @_LapeSimbaShapeBox_GetShape);
    addGlobalFunc('function TSimbaShapeBox.ShapeCount: Integer', @_LapeSimbaShapeBox_GetShapeCount);
    addGlobalFunc('function TSimbaShapeBox.ShapeIndex: Integer', @_LapeSimbaShapeBox_GetShapeIndex);

    addClassVar('TSimbaShapeBox', 'UserDataSize', 'Integer', @_LapeSimbaShapeBox_UserDataSize_Read, @_LapeSimbaShapeBox_UserDataSize_Write);
    addClassVar('TSimbaShapeBox', 'QueryNameOnNew', 'Boolean', @_LapeSimbaShapeBox_QueryNameOnNew_Read, @_LapeSimbaShapeBox_QueryNameOnNew_Write);

    addGlobalFunc('procedure TSimbaShapeBox.DeleteShape(Index: Integer);', @_LapeSimbaShapeBox_DeleteShape);
    addGlobalFunc('procedure TSimbaShapeBox.DeleteAllShapes;', @_LapeSimbaShapeBox_DeleteAllShapes);

    addGlobalFunc('procedure TSimbaShapeBox.SaveToFile(FileName: String);', @_LapeSimbaShapeBox_SaveToFile);
    addGlobalFunc('procedure TSimbaShapeBox.LoadFromFile(FileName: String);', @_LapeSimbaShapeBox_LoadFromFile);

    addGlobalFunc('function TSimbaShapeBox.PointButton: TLazButton;', @_LapeSimbaShapeBox_PointButton);
    addGlobalFunc('function TSimbaShapeBox.BoxButton: TLazButton;', @_LapeSimbaShapeBox_BoxButton);
    addGlobalFunc('function TSimbaShapeBox.PolyButton: TLazButton;', @_LapeSimbaShapeBox_PolyButton);
    addGlobalFunc('function TSimbaShapeBox.PathButton: TLazButton;', @_LapeSimbaShapeBox_PathButton);
    addGlobalFunc('function TSimbaShapeBox.PrintButton: TLazButton;', @_LapeSimbaShapeBox_PrintButton);
    addGlobalFunc('function TSimbaShapeBox.NameButton: TLazButton;', @_LapeSimbaShapeBox_NameButton);
    addGlobalFunc('function TSimbaShapeBox.DeleteButton: TLazButton;', @_LapeSimbaShapeBox_DeleteButton);
    addGlobalFunc('function TSimbaShapeBox.DeleteAllButton: TLazButton;', @_LapeSimbaShapeBox_DeleteAllButton);

    addGlobalFunc('procedure TSimbaShapeBox.ManualAddPoint(Point: TPoint; AName: String = ""); overload', @_LapeSimbaShapeBox_ManualAddPoint1);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddPoint(Point: TPoint; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddPoint2);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddBox(Box: TBox; AName: String = "");', @_LapeSimbaShapeBox_ManualAddBox1);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddBox(Point: TBox; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddBox2);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddPoly(Poly: TPointArray; AName: String = "");', @_LapeSimbaShapeBox_ManualAddPoly1);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddPoly(Point: TPointArray; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddPoly2);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddPath(Path: TPointArray; AName: String = "");', @_LapeSimbaShapeBox_ManualAddPath1);
    addGlobalFunc('procedure TSimbaShapeBox.ManualAddPath(Point: TPointArray; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddPath2);

    addGlobalFunc('function TSimbaShapeBox.Panel: TLazPanel;', @_LapeSimbaShapeBox_Panel);
  end;
end;

end.

