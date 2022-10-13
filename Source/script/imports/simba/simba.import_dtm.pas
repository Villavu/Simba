unit simba.import_dtm;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.dtm, simba.dtmutil;

procedure _LapeSetDTMName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MDTMs[PInt32(Params^[0])^].Name := PString(Params^[0])^;
end;

procedure _LapeDTMFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MDTMs.StringToDTM(PString(Params^[0])^);
end;

procedure _LapeFreeDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.Client.MDTMs.FreeDTM(PInt32(Params^[0])^);
end;

procedure _LapeGetDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SimbaScriptThread.Script.Client.MDTMs.GetDTM(PInt32(Params^[0])^);
end;

procedure _LapeAddTSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MDTMs.AddDTM(PSDTM(Params^[0])^);
end;

procedure _LapeAddDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := SimbaScriptThread.Script.Client.MDTMs.AddDTM(PMDTM(Params^[0])^);
end;

procedure _LapeMDTMToSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSDTM(Result)^ := MDTMToSDTM(PMDTM(Params^[0])^);
end;

procedure _LapeSDTMToMDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SDTMToMDTM(PSDTM(Params^[0])^);
end;

procedure _LapeCreateDTMPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMPoint(Result)^ := CreateDTMPoint(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, Pboolean(Params^[5])^);
end;

procedure _LapeDTMExists(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScriptThread.Script.Client.MDTMs.ExistsDTM(PInt32(Params^[0])^);
end;

procedure ImportDTM(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'DTM';

    addGlobalFunc('procedure SetDTMName(DTM: Int32; Name: String)', @_LapeSetDTMName);
    addGlobalFunc('function DTMFromString(S: String): Int32', @_LapeDTMFromString);
    addGlobalFunc('procedure FreeDTM(DTM: Int32)', @_LapeFreeDTM);
    addGlobalFunc('function GetDTM(Index: Int32): TMDTM', @_LapeGetDTM);
    addGlobalFunc('function AddSDTM(d: TSDTM): Int32', @_LapeAddTSDTM);
    addGlobalFunc('function AddDTM(d: TMDTM): Int32', @_LapeAddDTM);
    addGlobalFunc('function DTMExists(Index : Int32): Boolean', @_LapeDTMExists);
    addGlobalFunc('function MDTMToSDTM(DTM: TMDTM): TSDTM', @_LapeMDTMToSDTM);
    addGlobalFunc('function SDTMToMDTM(DTM: TSDTM): TMDTM', @_LapeSDTMToMDTM);
    addGlobalFunc('function CreateDTMPoint(x,y,c,t,asz: Int32; bp: Boolean): TMDTMPoint', @_LapeCreateDTMPoint);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDTM);

end.

