unit lpTClient;
//Depends: TClient, TObject, TIOManager, TMFiles, TMFinder, TMBitmaps, TMDTMS, TMOCR, TWritelnProc, string

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TClient(Compiler: TLapeCompiler);

implementation

uses
  client,
  iomanager
  {$IFDEF WINDOWS}, os_windows{$ENDIF}{$IFDEF LINUX}, os_linux{$ENDIF}
  , files,
  bitmaps,
  dtm,
  ocr,
  finder,
  MufasaTypes;

type
  PClient = ^TClient;
  PIOManager = ^TIOManager;
  PMFiles = ^TMFiles;
  PMFinder = ^TMFinder;
  PMBitmaps = ^TMBitmaps;
  //PMDTM = ^TMDTM;
  PMDTMS = ^TMDTMS;
  PMOCR = ^TMOCR;
  PWriteLnProc = ^TWriteLnProc;

//Read: IOManager: TIOManager;
procedure TClient_IOManager_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PIOManager(Result)^ := PClient(Params^[0])^.IOManager;
end;

//Write: IOManager: TIOManager;
procedure TClient_IOManager_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.IOManager := PIOManager(Params^[1])^;
end;

//Read: MFiles: TMFiles;
procedure TClient_MFiles_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMFiles(Result)^ := PClient(Params^[0])^.MFiles;
end;

//Write: MFiles: TMFiles;
procedure TClient_MFiles_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.MFiles := PMFiles(Params^[1])^;
end;

//Read: MFinder: TMFinder;
procedure TClient_MFinder_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMFinder(Result)^ := PClient(Params^[0])^.MFinder;
end;

//Write: MFinder: TMFinder;
procedure TClient_MFinder_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.MFinder := PMFinder(Params^[1])^;
end;

//Read: MBitmaps : TMBitmaps;
procedure TClient_MBitmaps_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMBitmaps(Result)^ := PClient(Params^[0])^.MBitmaps;
end;

//Write: MBitmaps : TMBitmaps;
procedure TClient_MBitmaps_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.MBitmaps := PMBitmaps(Params^[1])^;
end;

//Read: MDTMs: TMDTMS;
procedure TClient_MDTMs_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMDTMS(Result)^ := PClient(Params^[0])^.MDTMs;
end;

//Write: MDTMs: TMDTMS;
procedure TClient_MDTMs_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.MDTMs := PMDTMS(Params^[1])^;
end;

//Read: MOCR: TMOCR;
procedure TClient_MOCR_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMOCR(Result)^ := PClient(Params^[0])^.MOCR;
end;

//Write: MOCR: TMOCR;
procedure TClient_MOCR_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.MOCR := PMOCR(Params^[1])^;
end;

//Read: WritelnProc : TWritelnProc;
procedure TClient_WritelnProc_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWritelnProc(Result)^ := PClient(Params^[0])^.WritelnProc;
end;

//Write: WritelnProc : TWritelnProc;
procedure TClient_WritelnProc_Write(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.WritelnProc := PWritelnProc(Params^[1])^;
end;

//procedure WriteLn(s : string);
procedure TClient_WriteLn(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.WriteLn(PlpString(Params^[1])^);
end;

//constructor Create(const plugin_dir: string = ''; const UseIOManager : TIOManager = nil);
procedure TClient_Init(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^ := TClient.Create(PlpString(Params^[1])^, PIOManager(Params^[2])^);
end;

//procedure Free();
procedure TClient_Free(const Params: PParamArray); lape_extdecl
begin
  PClient(Params^[0])^.Free();
end;

procedure Register_TClient(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TClient', 'TObject');

    addGlobalType('procedure(s: string)', 'TWriteLnProc');

    addClassVar(Compiler, 'TClient', 'IOManager', 'TIOManager', @TClient_IOManager_Read, @TClient_IOManager_Write);
    addClassVar(Compiler, 'TClient', 'MFiles', 'TMFiles', @TClient_MFiles_Read, @TClient_MFiles_Write);
    addClassVar(Compiler, 'TClient', 'MFinder', 'TMFinder', @TClient_MFinder_Read, @TClient_MFinder_Write);
    addClassVar(Compiler, 'TClient', 'MBitmaps', 'TMBitmaps', @TClient_MBitmaps_Read, @TClient_MBitmaps_Write);
    addClassVar(Compiler, 'TClient', 'MDTMs', 'TMDTMS', @TClient_MDTMs_Read, @TClient_MDTMs_Write);
    addClassVar(Compiler, 'TClient', 'MOCR', 'TMOCR', @TClient_MOCR_Read, @TClient_MOCR_Write);
    addClassVar(Compiler, 'TClient', 'WriteLnProc', 'TWriteLnProc', @TClient_WritelnProc_Read, @TClient_WritelnProc_Write);
    addGlobalFunc('procedure TClient.WriteLn(s : string);', @TClient_WriteLn);
    addGlobalFunc('procedure TClient.Init(const plugin_dir: string = ''''; const UseIOManager: TIOManager = nil);', @TClient_Init);
    addGlobalFunc('procedure TClient.Free();', @TClient_Free);
  end;
end;

end.

