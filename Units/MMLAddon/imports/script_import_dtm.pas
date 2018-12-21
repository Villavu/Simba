unit script_import_dtm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, mufasatypes, dtm, dtmutil;

procedure Lape_SetDTMName(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.MDTMs[PInt32(Params^[1])^].Name := PString(Params^[1])^;
end;

procedure Lape_DTMFromString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TMMLScriptThread(Params^[0]).Client.MDTMs.StringToDTM(PString(Params^[1])^);
end;

procedure Lape_FreeDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TMMLScriptThread(Params^[0]).Client.MDTMs.FreeDTM(PInt32(Params^[1])^);
end;

procedure Lape_GetDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := TMMLScriptThread(Params^[0]).Client.MDTMs.GetDTM(PInt32(Params^[1])^);
end;

procedure Lape_AddTSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TMMLScriptThread(Params^[0]).Client.MDTMs.AddDTM(PSDTM(Params^[1])^);
end;

procedure Lape_AddDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInt32(Result)^ := TMMLScriptThread(Params^[0]).Client.MDTMs.AddDTM(PMDTM(Params^[1])^);
end;

procedure Lape_MDTMToSDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSDTM(Result)^ := MDTMToSDTM(PMDTM(Params^[1])^);
end;

procedure Lape_SDTMToMDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := SDTMToMDTM(PSDTM(Params^[1])^);
end;

procedure Lape_CreateDTMPoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMPoint(Result)^ := CreateDTMPoint(PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PInt32(Params^[5])^, Pboolean(Params^[6])^);
end;

procedure Lape_DTMExists(const Params : PParamArray; const Result : Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := TMMLScriptThread(Params^[0]).Client.MDTMs.ExistsDTM(PInt32(Params^[1])^);
end;

procedure Lape_Import_DTM(Compiler: TLapeCompiler; Data: Pointer);
begin
  with Compiler do
  begin
    addGlobalMethod('procedure SetDTMName(DTM: Int32; Name: String);', @Lape_SetDTMName, Data);
    addGlobalMethod('function DTMFromString(S: String): Int32', @Lape_DTMFromString, Data);
    addGlobalMethod('procedure FreeDTM(DTM: Int32);', @Lape_FreeDTM, Data);
    addGlobalMethod('function GetDTM(Index: Int32): TMDTM', @Lape_GetDTM, Data);
    addGlobalMethod('function AddSDTM(d: TSDTM): Int32', @Lape_AddTSDTM, Data);
    addGlobalMethod('function AddDTM(d: TMDTM): Int32', @Lape_AddDTM, Data);
    addGlobalMethod('function DTMExists(Index : Int32): Boolean', @Lape_DTMExists, Data);
    addGlobalMethod('function MDTMToSDTM(DTM: TMDTM): TSDTM', @Lape_MDTMToSDTM, Data);
    addGlobalMethod('function SDTMToMDTM(DTM: TSDTM): TMDTM', @Lape_SDTMToMDTM, Data);
    addGlobalMethod('function CreateDTMPoint(x,y,c,t,asz: Int32; bp: Boolean): TMDTMPoint', @Lape_CreateDTMPoint, Data);
  end;
end;

initialization
  ScriptImports.Add('DTM', @Lape_Import_DTM);

end.

