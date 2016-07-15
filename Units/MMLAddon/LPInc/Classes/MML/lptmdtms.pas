unit lpTMDTMS;
//Depends: TObject, TMDTM

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMDTMS(Compiler: TLapeCompiler);

implementation

uses
  dtm, MufasaTypes;

type
  PMDTMS = ^TMDTMS;
  PMDTM = ^TMDTM;
  PSDTM = ^TSDTM;
  PObject = ^TObject;

//function AddDTM(const d: TSDTM): Integer;overload;
procedure TMDTMS_AddDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.AddDTM(PSDTM(Params^[1])^);
end;

//function AddDTM(const d: TMDTM): Integer;overload;
procedure TMDTMS_AddDTMEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.AddDTM(PMDTM(Params^[1])^);
end;

//function ExistsDTM(index : integer) : boolean;
procedure TMDTMS_ExistsDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMDTMS(Params^[0])^.ExistsDTM(Pinteger(Params^[1])^);
end;

//function GetDTM(index: Integer) :TMDTM;
procedure TMDTMS_GetDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMDTM(Result)^ := PMDTMS(Params^[0])^.GetDTM(PInteger(Params^[1])^);
end;

//procedure FreeDTM(DTM: Integer);
procedure TMDTMS_FreeDTM(const Params: PParamArray); lape_extdecl
begin
  PMDTMS(Params^[0])^.FreeDTM(PInteger(Params^[1])^);
end;

//function StringToDTM(const S: String): Integer;
procedure TMDTMS_StringToDTM(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.StringToDTM(PlpString(Params^[1])^);
end;

//constructor Create(Owner: TObject);
procedure TMDTMS_Init(const Params: PParamArray); lape_extdecl
begin
  PMDTMS(Params^[0])^ := TMDTMS.Create(PObject(Params^[1])^);
end;

//procedure Free();
procedure TMDTMS_Free(const Params: PParamArray); lape_extdecl
begin
  PMDTMS(Params^[0])^.Free();
end;

procedure Register_TMDTMS(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMDTMS');

    addGlobalType('record x, y, Color, Tolerance, AreaSize, AreaShape: UInt32; end;', 'TSDTMPointDef');
    addGlobalType('array of TSDTMPointDef;', 'TSDTMPointDefArray');
    addGlobalType('record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end;', 'TSDTM');
	
    addGlobalFunc('function TMDTMS.AddDTM(const d: TSDTM): Integer; constref;', @TMDTMS_AddDTM);
    addGlobalFunc('function TMDTMS.AddDTM(const d: TMDTM): Integer; constref; overload;', @TMDTMS_AddDTMEx);
    addGlobalFunc('function TMDTMS.ExistsDTM(index : integer): boolean; constref;', @TMDTMS_ExistsDTM);
    addGlobalFunc('function TMDTMS.GetDTM(index: Integer): TMDTM; constref;', @TMDTMS_GetDTM);
    addGlobalFunc('procedure TMDTMS.FreeDTM(DTM: Integer); constref;', @TMDTMS_FreeDTM);
    addGlobalFunc('function TMDTMS.StringToDTM(const S: String): Integer; constref;', @TMDTMS_StringToDTM);
    addGlobalFunc('procedure TMDTMS.Init(Owner: TObject);', @TMDTMS_Init);
    addGlobalFunc('procedure TMDTMS.Free();', @TMDTMS_Free);
  end;
end;

end.

