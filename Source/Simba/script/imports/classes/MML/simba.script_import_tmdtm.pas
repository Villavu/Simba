unit simba.script_import_tmdtm;
//Depends: TObject

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_TMDTM(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.dtm;

type
  PMDTM = ^TMDTM;
  PPMDTMPoint = ^PMDTMPoint;
  PMDTMPointArray = ^TMDTMPointArray;

//Read: Name : string;
procedure TMDTM_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMDTM(Params^[0])^.Name;
end;

//Write: Name : string;
procedure TMDTM_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Name := PlpString(Params^[1])^;
end;

//Read: Index : integer;
procedure TMDTM_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.Index;
end;

//Write: Index : integer;
procedure TMDTM_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Index := Pinteger(Params^[1])^;
end;

//function ToString : string;
procedure TMDTM_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMDTM(Params^[0])^.ToString();
end;

//function SaveToFile(const FileName : string) : boolean;
procedure TMDTM_SaveToFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//function LoadFromString(const s : string) : boolean;
procedure TMDTM_LoadFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.LoadFromString(PlpString(Params^[1])^);
end;

//procedure Normalize;
procedure TMDTM_Normalize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Normalize();
end;

//function Valid : boolean;
procedure TMDTM_Valid(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.Valid();
end;

//procedure DeletePoint(Point : integer);
procedure TMDTM_DeletePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.DeletePoint(Pinteger(Params^[1])^);
end;

//procedure SwapPoint(p1, p2: integer);
procedure TMDTM_SwapPoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.SwapPoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure MovePoint(fromIndex, toIndex: integer);
procedure TMDTM_MovePoint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.MovePoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function AddPoint(Point: TMDTMPoint): integer;
procedure TMDTM_AddPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.AddPoint(PMDTMPoint(Params^[1])^);
end;

//Read: property PPoints : PMDTMPoint read GetPointerPoints;
procedure TMDTM_PPoints_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPMDTMPoint(Result)^ := PMDTM(Params^[0])^.PPoints;
end;

//Read: property Count : integer read FLen write SetPointCount;
procedure TMDTM_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.Count;
end;

//Write: property Count : integer read FLen write SetPointCount;
procedure TMDTM_Count_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Count := Pinteger(Params^[1])^;
end;

//Read: property Points : TMDTMPointArray read FPoints;
procedure TMDTM_Points_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMPointArray(Result)^ := PMDTM(Params^[0])^.Points;
end;

//constructor Create();
procedure TMDTM_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^ := TMDTM.Create();
end;

//procedure Free();
procedure TMDTM_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Params^[0])^.Free();
end;

procedure Lape_Import_TMDTM(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMDTM');

    addGlobalType('record x, y, c, t, asz: integer; bp: boolean; end;', 'TMDTMPoint');
    addGlobalType('^TMDTMPoint', 'PMDTMPoint');
    addGlobalType('array of TMDTMPoint;', 'TMDTMPointArray');

    addClassVar('TMDTM', 'Name', 'string', @TMDTM_Name_Read, @TMDTM_Name_Write);
    addClassVar('TMDTM', 'Index', 'integer', @TMDTM_Index_Read, @TMDTM_Index_Write);
    addGlobalFunc('function TMDTM.ToString(): string; constref;', @TMDTM_ToString);
    addGlobalFunc('function TMDTM.SaveToFile(const FileName : string): boolean; constref;', @TMDTM_SaveToFile);
    addGlobalFunc('function TMDTM.LoadFromString(const s : string): boolean; constref;', @TMDTM_LoadFromString);
    addGlobalFunc('procedure TMDTM.Normalize(); constref;', @TMDTM_Normalize);
    addGlobalFunc('function TMDTM.Valid(): boolean; constref;', @TMDTM_Valid);
    addGlobalFunc('procedure TMDTM.DeletePoint(Point : integer); constref;', @TMDTM_DeletePoint);
    addGlobalFunc('procedure TMDTM.SwapPoint(p1, p2: integer); constref;', @TMDTM_SwapPoint);
    addGlobalFunc('procedure TMDTM.MovePoint(fromIndex, toIndex: integer); constref;', @TMDTM_MovePoint);
    addGlobalFunc('function TMDTM.AddPoint(Point: TMDTMPoint): integer; constref;', @TMDTM_AddPoint);
    addClassVar('TMDTM', 'PPoints', 'PMDTMPoint', @TMDTM_PPoints_Read);
    addClassVar('TMDTM', 'Count', 'integer', @TMDTM_Count_Read, @TMDTM_Count_Write);
    addClassVar('TMDTM', 'Points', 'TMDTMPointArray', @TMDTM_Points_Read);
    addGlobalFunc('procedure TMDTM.Init(); override;', @TMDTM_Init);
    //addGlobalFunc('procedure TMDTM.Free(); constref;', @TMDTM_Free);
  end;
end;

end.

