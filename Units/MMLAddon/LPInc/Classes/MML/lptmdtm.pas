unit lpTMDTM;
//Depends: TObject

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMDTM(Compiler: TLapeCompiler);

implementation

uses
  dtm, MufasaTypes;

type
  PMDTM = ^TMDTM;
  PPMDTMPoint = ^PMDTMPoint;
  PMDTMPointArray = ^TMDTMPointArray;

//Read: Name : string;
procedure TMDTM_Name_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMDTM(Params^[0])^.Name;
end;

//Write: Name : string;
procedure TMDTM_Name_Write(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.Name := PlpString(Params^[1])^;
end;

//Read: Index : integer;
procedure TMDTM_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.Index;
end;

//Write: Index : integer;
procedure TMDTM_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.Index := Pinteger(Params^[1])^;
end;

//function ToString : string;
procedure TMDTM_ToString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMDTM(Params^[0])^.ToString();
end;

//function SaveToFile(const FileName : string) : boolean;
procedure TMDTM_SaveToFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//function LoadFromString(const s : string) : boolean;
procedure TMDTM_LoadFromString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.LoadFromString(PlpString(Params^[1])^);
end;

//procedure Normalize;
procedure TMDTM_Normalize(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.Normalize();
end;

//function Valid : boolean;
procedure TMDTM_Valid(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMDTM(Params^[0])^.Valid();
end;

//procedure DeletePoint(Point : integer);
procedure TMDTM_DeletePoint(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.DeletePoint(Pinteger(Params^[1])^);
end;

//procedure SwapPoint(p1, p2: integer);
procedure TMDTM_SwapPoint(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.SwapPoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure MovePoint(fromIndex, toIndex: integer);
procedure TMDTM_MovePoint(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.MovePoint(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function AddPoint(Point: TMDTMPoint): integer;
procedure TMDTM_AddPoint(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.AddPoint(PMDTMPoint(Params^[1])^);
end;

//Read: property PPoints : PMDTMPoint read GetPointerPoints;
procedure TMDTM_PPoints_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPMDTMPoint(Result)^ := PMDTM(Params^[0])^.PPoints;
end;

//Read: property Count : integer read FLen write SetPointCount;
procedure TMDTM_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMDTM(Params^[0])^.Count;
end;

//Write: property Count : integer read FLen write SetPointCount;
procedure TMDTM_Count_Write(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.Count := Pinteger(Params^[1])^;
end;

//Read: property Points : TMDTMPointArray read FPoints;
procedure TMDTM_Points_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMDTMPointArray(Result)^ := PMDTM(Params^[0])^.Points;
end;

//constructor Create();
procedure TMDTM_Init(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^ := TMDTM.Create();
end;

//procedure Free();
procedure TMDTM_Free(const Params: PParamArray); lape_extdecl
begin
  PMDTM(Params^[0])^.Free();
end;

procedure Register_TMDTM(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMDTM', 'TObject');

    addGlobalType('record x, y, c, t, asz: integer; bp: boolean; end;', 'TMDTMPoint');
    addGlobalType('^TMDTMPoint', 'PMDTMPoint');
    addGlobalType('array of TMDTMPoint;', 'TMDTMPointArray');

    addClassVar(Compiler, 'TMDTM', 'Name', 'string', @TMDTM_Name_Read, @TMDTM_Name_Write);
    addClassVar(Compiler, 'TMDTM', 'Index', 'integer', @TMDTM_Index_Read, @TMDTM_Index_Write);
    addGlobalFunc('function TMDTM.ToString(): string;', @TMDTM_ToString);
    addGlobalFunc('function TMDTM.SaveToFile(const FileName : string): boolean;', @TMDTM_SaveToFile);
    addGlobalFunc('function TMDTM.LoadFromString(const s : string): boolean;', @TMDTM_LoadFromString);
    addGlobalFunc('procedure TMDTM.Normalize();', @TMDTM_Normalize);
    addGlobalFunc('function TMDTM.Valid(): boolean;', @TMDTM_Valid);
    addGlobalFunc('procedure TMDTM.DeletePoint(Point : integer);', @TMDTM_DeletePoint);
    addGlobalFunc('procedure TMDTM.SwapPoint(p1, p2: integer);', @TMDTM_SwapPoint);
    addGlobalFunc('procedure TMDTM.MovePoint(fromIndex, toIndex: integer);', @TMDTM_MovePoint);
    addGlobalFunc('function TMDTM.AddPoint(Point: TMDTMPoint): integer;', @TMDTM_AddPoint);
    addClassVar(Compiler, 'TMDTM', 'PPoints', 'PMDTMPoint', @TMDTM_PPoints_Read, nil);
    addClassVar(Compiler, 'TMDTM', 'Count', 'integer', @TMDTM_Count_Read, @TMDTM_Count_Write);
    addClassVar(Compiler, 'TMDTM', 'Points', 'TMDTMPointArray', @TMDTM_Points_Read, nil);
    addGlobalFunc('procedure TMDTM.Init();', @TMDTM_Init);
    addGlobalFunc('procedure TMDTM.Free();', @TMDTM_Free);
  end;
end;

end.

