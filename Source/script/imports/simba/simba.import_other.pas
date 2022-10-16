unit simba.import_other;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, clipbrd, extctrls, lptypes,
  simba.script_compiler, simba.nativeinterface, simba.scriptthread, simba.stringutil,
  simba.settings, simba.mufasatypes;

procedure _LapePlaySound(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PlaySound(PString(Params^[0])^);
end;

procedure _LapeStopSound(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.StopSound();
end;

procedure _LapeSetSupressExceptions(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaScriptThread.Script.Client.MFinder.WarnOnly := PBoolean(Params^[0])^;
end;

procedure _LapeSimba(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  WriteLn(DecompressString(Base64Decode(
    '9AoAAHicldU7b+0gDADgvdL9D+AisSDOTrpUvWvGuzNVOkMn9v72a942j6T1dJrEX7AxqRAXYaS9up3iz8suVxoMKe+' +
    'NC6LGnbEhiCCfTzPfJ5cmgidj5J9MsezSQAyApGHGR17N9SpGoBj1tkuRkJHoAk3WeMfTC66GWbaTFtMAwZDPRjh73U4uCKGnRTh3NMK0mAjiXxA975iERASl' +
    'QjfcRLBVS963TKCQDb0m8Brwwv1IKAWkErcipPNAC5+JdPmY62hE/O3L8yE+T4k4PpGwi2aiEIn25zcqKMQ1a6bgNtGN4kJqJ1tYeqFwrMNDcCFvKjMsWXLOK' +
    'N19toPbBN2PmacG9BogFoW7CQD00JTHdZlLml1yQZiv8zzBxGlQzxoxlx+Gdjo8JQDMV8w/0UmCctC/PGZDIKKPFMIGOM8M5IlUyuMel05IwY3hiHoMTLJYdg' +
    'RKvhJxsGt5wzKI8PApjpQTQmj5CkIRIO6S3REPXZjD1kyNGxABm60IxLkdu8HqQOaRmt0TcTVVFHzCdq2oX6ae2CMRuo/bWuhdHfMhfSI8PTE3xIjAuIRu7An' +
    'hv0kN+e38+1GMPYH/hq1PcyKsywdWvI1n9Y4YXzsLydgSphI4G7i/AexYRTW2RJmBPqFqTcgtUW7T6dgQlwIDfrsIsyDCphcbot5eDPgviZ8Yt0S4Ne4Iuoy/H' +
    '+//1sR/NLyhCQ==')));
end;

procedure _LapeSetClipBoard(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  try
    Clipboard.AsText := PString(Params^[0])^;
  except
  end;
end;

procedure _LapeGetClipBoard(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  try
    PString(Result)^ := Clipboard.AsText;
  except
  end;
end;

procedure _LapeStatus(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  if (SimbaScriptThread.Script.SimbaCommunication = nil) then
    raise Exception.Create('Status requires Simba communication');

  SimbaScriptThread.Script.SimbaCommunication.Status(PString(Params^[0])^);
end;

procedure ImportOther(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Other';

    addGlobalFunc('procedure PlaySound(Sound: String)', @_LapePlaySound);
    addGlobalFunc('procedure StopSound', @_LapeStopSound);
    addGlobalFunc('procedure SetSupressExceptions(Supress: Boolean)', @_LapeSetSupressExceptions);
    addGlobalFunc('procedure Simba', @_LapeSimba);
    addGlobalFunc('procedure SetClipBoard(Data: string)', @_LapeSetClipBoard);
    addGlobalFunc('function GetClipBoard: String', @_LapeGetClipBoard);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportOther);

end.

