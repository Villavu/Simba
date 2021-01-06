unit simba.script_import_dtm;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_DTM(Compiler: TSimbaScript_Compiler);

implementation

uses
  simba.dtm, simba.dtmutil;

procedure Lape_SetDTMName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MDTMs[PInt32(Params^[0])^].Name := PString(Params^[0])^;
end;

procedure Lape_DTMFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MDTMs.StringToDTM(PString(Params^[0])^);
end;

procedure Lape_FreeDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.Client.MDTMs.FreeDTM(PInt32(Params^[0])^);
end;

procedure Lape_GetDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SimbaScript.Client.MDTMs.GetDTM(PInt32(Params^[0])^);
end;

procedure Lape_AddTSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MDTMs.AddDTM(PSDTM(Params^[0])^);
end;

procedure Lape_AddDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScript.Client.MDTMs.AddDTM(PMDTM(Params^[0])^);
end;

procedure Lape_MDTMToSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSDTM(Result)^ := MDTMToSDTM(PMDTM(Params^[0])^);
end;

procedure Lape_SDTMToMDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SDTMToMDTM(PSDTM(Params^[0])^);
end;

procedure Lape_CreateDTMPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMPoint(Result)^ := CreateDTMPoint(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, Pboolean(Params^[5])^);
end;

procedure Lape_DTMExists(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Client.MDTMs.ExistsDTM(PInt32(Params^[0])^);
end;

procedure Lape_Import_DTM(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'DTM';

    addGlobalFunc('procedure SetDTMName(DTM: Int32; Name: String);', @Lape_SetDTMName);
    addGlobalFunc('function DTMFromString(S: String): Int32', @Lape_DTMFromString);
    addGlobalFunc('procedure FreeDTM(DTM: Int32);', @Lape_FreeDTM);
    addGlobalFunc('function GetDTM(Index: Int32): TMDTM', @Lape_GetDTM);
    addGlobalFunc('function AddSDTM(d: TSDTM): Int32', @Lape_AddTSDTM);
    addGlobalFunc('function AddDTM(d: TMDTM): Int32', @Lape_AddDTM);
    addGlobalFunc('function DTMExists(Index : Int32): Boolean', @Lape_DTMExists);
    addGlobalFunc('function MDTMToSDTM(DTM: TMDTM): TSDTM', @Lape_MDTMToSDTM);
    addGlobalFunc('function SDTMToMDTM(DTM: TSDTM): TMDTM', @Lape_SDTMToMDTM);
    addGlobalFunc('function CreateDTMPoint(x,y,c,t,asz: Int32; bp: Boolean): TMDTMPoint', @Lape_CreateDTMPoint);
  end;
end;

end.

