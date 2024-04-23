unit simba.import_shapebox;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportSimbaShapeBox(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, stdctrls, extctrls, comctrls, graphics, lptypes, ffi,
  simba.component_imagebox, simba.component_shapebox;

type
  PComponent = ^TComponent;
  PPanel = ^TPanel;
  PButton = ^TButton;
  PNotifyEvent = ^TNotifyEvent;

procedure _LapeSimbaShapeBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Result)^ := TSimbaShapeBox.Create(PComponent(Params^[0])^, PInteger(Params^[1])^);
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

procedure _LapeSimbaShapeBox_CopyButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := PSimbaShapeBox(Params^[0])^.CopyButton;
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

procedure _LapeSimbaShapeBox_GetCount(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.Count;
end;

procedure _LapeSimbaShapeBox_QueryName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaShapeBox(Params^[0])^.QueryName;
end;

procedure _LapeSimbaShapeBox_QueryName_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.QueryName := PBoolean(Params^[1])^;
end;

procedure _LapeSimbaShapeBox_CopyShape(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.CopyShape(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_DeleteShape(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.DeleteShape(PInteger(Params^[1])^);
end;

procedure _LapeSimbaShapeBox_DeleteAllShapes(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.DeleteAllShapes();
end;

procedure _LapeSimbaShapeBox_BeginUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.BeginUpdate();
end;

procedure _LapeSimbaShapeBox_EndUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.EndUpdate();
end;

procedure _LapeSimbaShapeBox_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSimbaShapeBox(Params^[0])^.OnSelectionChange;
end;

procedure _LapeSimbaShapeBox_OnSelectionChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.OnSelectionChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSimbaShapeBox_HasSelection(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaShapeBox(Params^[0])^.HasSelection;
end;

procedure _LapeSimbaShapeBox_GetSelectedShape(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PShapeBoxShape(Result)^ := PSimbaShapeBox(Params^[0])^.SelectedShape;
end;

procedure _LapeSimbaShapeBox_SelectedIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaShapeBox(Params^[0])^.SelectedIndex;
end;

procedure _LapeSimbaShapeBox_SelectedIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaShapeBox(Params^[0])^.SelectedIndex := PInteger(Params^[1])^;
end;

procedure ImportSimbaShapeBox(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TShapeBox', 'TImageBox');

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

    addClassConstructor('TShapeBox', '(Owner: TLazComponent; UserDataSize: Integer = 0)', @_LapeSimbaShapeBox_Create);

    addProperty('TShapeBox', 'OnSelectionChange', 'TLazNotifyEvent', @_LapeSimbaShapeBox_OnSelectionChange_Read, @_LapeSimbaShapeBox_OnSelectionChange_Write);
    addProperty('TShapeBox', 'QueryName', 'Boolean', @_LapeSimbaShapeBox_QueryName_Read, @_LapeSimbaShapeBox_QueryName_Write);
    addProperty('TShapeBox', 'SelectedIndex', 'Integer', @_LapeSimbaShapeBox_SelectedIndex_Read, @_LapeSimbaShapeBox_SelectedIndex_Write);

    addGlobalFunc('function TShapeBox.HasSelection: Boolean', @_LapeSimbaShapeBox_HasSelection);
    addGlobalFunc('function TShapeBox.GetSelectedShape: TShapeBoxShape', @_LapeSimbaShapeBox_GetSelectedShape);
    addGlobalFunc('function TShapeBox.GetShape(Index: Integer): TShapeBoxShape', @_LapeSimbaShapeBox_GetShape);
    addGlobalFunc('function TShapeBox.GetCount: Integer', @_LapeSimbaShapeBox_GetCount);

    addGlobalFunc('function TShapeBox.CopyShape(Index: Integer): Integer;', @_LapeSimbaShapeBox_CopyShape);
    addGlobalFunc('procedure TShapeBox.DeleteShape(Index: Integer);', @_LapeSimbaShapeBox_DeleteShape);
    addGlobalFunc('procedure TShapeBox.DeleteAllShapes;', @_LapeSimbaShapeBox_DeleteAllShapes);

    addGlobalFunc('procedure TShapeBox.BeginUpdate;', @_LapeSimbaShapeBox_BeginUpdate);
    addGlobalFunc('procedure TShapeBox.EndUpdate;', @_LapeSimbaShapeBox_EndUpdate);

    addGlobalFunc('procedure TShapeBox.SaveToFile(FileName: String);', @_LapeSimbaShapeBox_SaveToFile);
    addGlobalFunc('procedure TShapeBox.LoadFromFile(FileName: String);', @_LapeSimbaShapeBox_LoadFromFile);

    addGlobalFunc('function TShapeBox.PointButton: TLazButton;', @_LapeSimbaShapeBox_PointButton);
    addGlobalFunc('function TShapeBox.BoxButton: TLazButton;', @_LapeSimbaShapeBox_BoxButton);
    addGlobalFunc('function TShapeBox.PolyButton: TLazButton;', @_LapeSimbaShapeBox_PolyButton);
    addGlobalFunc('function TShapeBox.PathButton: TLazButton;', @_LapeSimbaShapeBox_PathButton);
    addGlobalFunc('function TShapeBox.PrintButton: TLazButton;', @_LapeSimbaShapeBox_PrintButton);
    addGlobalFunc('function TShapeBox.NameButton: TLazButton;', @_LapeSimbaShapeBox_NameButton);
    addGlobalFunc('function TShapeBox.CopyButton: TLazButton;', @_LapeSimbaShapeBox_CopyButton);
    addGlobalFunc('function TShapeBox.DeleteButton: TLazButton;', @_LapeSimbaShapeBox_DeleteButton);
    addGlobalFunc('function TShapeBox.DeleteAllButton: TLazButton;', @_LapeSimbaShapeBox_DeleteAllButton);

    addGlobalFunc('procedure TShapeBox.ManualAddPoint(Point: TPoint; AName: String = ""); overload', @_LapeSimbaShapeBox_ManualAddPoint1);
    addGlobalFunc('procedure TShapeBox.ManualAddPoint(Point: TPoint; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddPoint2);
    addGlobalFunc('procedure TShapeBox.ManualAddBox(Box: TBox; AName: String = "");', @_LapeSimbaShapeBox_ManualAddBox1);
    addGlobalFunc('procedure TShapeBox.ManualAddBox(Point: TBox; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddBox2);
    addGlobalFunc('procedure TShapeBox.ManualAddPoly(Poly: TPointArray; AName: String = "");', @_LapeSimbaShapeBox_ManualAddPoly1);
    addGlobalFunc('procedure TShapeBox.ManualAddPoly(Point: TPointArray; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddPoly2);
    addGlobalFunc('procedure TShapeBox.ManualAddPath(Path: TPointArray; AName: String = "");', @_LapeSimbaShapeBox_ManualAddPath1);
    addGlobalFunc('procedure TShapeBox.ManualAddPath(Point: TPointArray; AName: String; constref UserData); overload; ', @_LapeSimbaShapeBox_ManualAddPath2);

    addGlobalFunc('function TShapeBox.Panel: TLazPanel;', @_LapeSimbaShapeBox_Panel);
  end;
end;

end.

