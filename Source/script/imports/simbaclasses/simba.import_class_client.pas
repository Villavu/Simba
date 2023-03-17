unit simba.import_class_client;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.client, simba.finder,
  simba.iomanager;

procedure _LapeClient_IOManager_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIOManager(Result)^ := PClient(Params^[0])^.IOManager;
end;

procedure _LapeClient_IOManager_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PClient(Params^[0])^.IOManager := PIOManager(Params^[1])^;
end;

procedure _LapeClient_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PClient(Params^[0])^ := TClient.Create(PString(Params^[1])^, PIOManager(Params^[2])^);
end;

procedure _LapeClient_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PClient(Params^[0])^.Free();
end;

procedure ImportClient(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClassVar('TClient', 'IOManager', 'TIOManager', @_LapeClient_IOManager_Read, @_LapeClient_IOManager_Write);
    addGlobalFunc('procedure TClient.Init(const plugin_dir: string = ""; const UseIOManager: TIOManager = nil)', @_LapeClient_Init);
    //addGlobalFunc('procedure TClient.Free;', @_LapeClient_Free);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportClient);

end.

