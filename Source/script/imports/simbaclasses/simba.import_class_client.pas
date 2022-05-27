unit simba.import_class_client;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.client, simba.files, simba.ocr, simba.finder,
  simba.dtm, simba.iomanager;

procedure _LapeClient_IOManager_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIOManager(Result)^ := PClient(Params^[0])^.IOManager;
end;

procedure _LapeClient_IOManager_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.IOManager := PIOManager(Params^[1])^;
end;

procedure _LapeClient_MFiles_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Result)^ := PClient(Params^[0])^.MFiles;
end;

procedure _LapeClient_MFiles_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MFiles := PMFiles(Params^[1])^;
end;

procedure _LapeClient_MFinder_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFinder(Result)^ := PClient(Params^[0])^.MFinder;
end;

procedure _LapeClient_MFinder_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MFinder := PMFinder(Params^[1])^;
end;

procedure _LapeClient_MDTMs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMDTMS(Result)^ := PClient(Params^[0])^.MDTMs;
end;

procedure _LapeClient_MDTMs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MDTMs := PMDTMS(Params^[1])^;
end;

procedure _LapeClient_MOCR_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMOCR(Result)^ := PClient(Params^[0])^.MOCR;
end;

procedure _LapeClient_MOCR_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.MOCR := PMOCR(Params^[1])^;
end;

procedure _LapeClient_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^ := TClient.Create(PString(Params^[1])^, PIOManager(Params^[2])^);
end;

procedure _LapeClient_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PClient(Params^[0])^.Free();
end;

procedure ImportClient(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Classes');

    addClassVar('TClient', 'IOManager', 'TIOManager', @_LapeClient_IOManager_Read, @_LapeClient_IOManager_Write);
    addClassVar('TClient', 'MFiles', 'TMFiles', @_LapeClient_MFiles_Read, @_LapeClient_MFiles_Write);
    addClassVar('TClient', 'MFinder', 'TMFinder', @_LapeClient_MFinder_Read, @_LapeClient_MFinder_Write);
    addClassVar('TClient', 'MDTMs', 'TMDTMS', @_LapeClient_MDTMs_Read, @_LapeClient_MDTMs_Write);
    addClassVar('TClient', 'MOCR', 'TMOCR', @_LapeClient_MOCR_Read, @_LapeClient_MOCR_Write);
    addGlobalFunc('procedure TClient.Init(const plugin_dir: string = ""; const UseIOManager: TIOManager = nil)', @_LapeClient_Init);
    //addGlobalFunc('procedure TClient.Free;', @_LapeClient_Free);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportClient);

end.

