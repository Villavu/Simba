unit simba.import_class_dtms;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.mufasatypes, simba.dtm;

type
  PObject = ^TObject;

procedure _LapeMDTMS_AddDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.AddDTM(PSDTM(Params^[1])^);
end;

procedure _LapeMDTMS_AddDTMEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.AddDTM(PMDTM(Params^[1])^);
end;

procedure _LapeMDTMS_ExistsDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMDTMS(Params^[0])^.ExistsDTM(Pinteger(Params^[1])^);
end;

procedure _LapeMDTMS_GetDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTM(Result)^ := PMDTMS(Params^[0])^.GetDTM(PInteger(Params^[1])^);
end;

procedure _LapeMDTMS_FreeDTM(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Params^[0])^.FreeDTM(PInteger(Params^[1])^);
end;

procedure _LapeMDTMS_StringToDTM(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMDTMS(Params^[0])^.StringToDTM(PlpString(Params^[1])^);
end;

procedure _LapeMDTMS_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Params^[0])^ := TMDTMS.Create(PObject(Params^[1])^);
end;

procedure _LapeMDTMS_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Params^[0])^.Free();
end;

procedure ImportDTMs(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Classes');

    addClass('TMDTMS');
    addGlobalType('record x, y, Color, Tolerance, AreaSize, AreaShape: UInt32; end', 'TSDTMPointDef');
    addGlobalType('array of TSDTMPointDef', 'TSDTMPointDefArray');
    addGlobalType('record MainPoint: TSDTMPointDef; SubPoints: TSDTMPointDefArray; end', 'TSDTM');
    addGlobalFunc('function TMDTMS.AddDTM(const d: TSDTM): Integer;', @_LapeMDTMS_AddDTM);
    addGlobalFunc('function TMDTMS.AddDTM(const d: TMDTM): Integer; overload', @_LapeMDTMS_AddDTMEx);
    addGlobalFunc('function TMDTMS.ExistsDTM(index : integer): boolean;', @_LapeMDTMS_ExistsDTM);
    addGlobalFunc('function TMDTMS.GetDTM(index: Integer): TMDTM;', @_LapeMDTMS_GetDTM);
    addGlobalFunc('procedure TMDTMS.FreeDTM(DTM: Integer);', @_LapeMDTMS_FreeDTM);
    addGlobalFunc('function TMDTMS.StringToDTM(const S: String): Integer;', @_LapeMDTMS_StringToDTM);
    addGlobalFunc('procedure TMDTMS.Init(Owner: TObject)', @_LapeMDTMS_Init);
    //addGlobalFunc('procedure TMDTMS.Free;', @_LapeMDTMS_Free);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportDTMs);

end.

