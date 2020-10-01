unit simbascript.import_other;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Other(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.stringutil, lclintf, clipbrd
  {$IFDEF WINDOWS},
  mmsystem
  {$ENDIF};

procedure Lape_PlaySound(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IFDEF WINDOWS}
  sndPlaySound(PChar(PString(Params^[0])^), SND_ASYNC or SND_NODEFAULT);
  {$ENDIF}
end;

procedure Lape_StopSound(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  {$IFDEF WINDOWS}
  sndPlaySound(nil, 0);
  {$ENDIF}
end;

procedure Lape_SetSupressExceptions(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ScriptInstance.Client.MFinder.WarnOnly := System.PBoolean(Params^[0])^;
end;

procedure Lape_HakunaMatata(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  OpenURL('https://www.youtube.com/watch?v=xB5ceAruYrI');
end;

procedure Lape_Simba(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ScriptInstance.WriteLn(DecompressString(Base64Decode(
    '9AoAAHicldU7b+0gDADgvdL9D+AisSDOTrpUvWvGuzNVOkMn9v72a942j6T1dJrEX7AxqRAXYaS9up3iz8suVxoMKe+' +
    'NC6LGnbEhiCCfTzPfJ5cmgidj5J9MsezSQAyApGHGR17N9SpGoBj1tkuRkJHoAk3WeMfTC66GWbaTFtMAwZDPRjh73U4uCKGnRTh3NMK0mAjiXxA975iERASl' +
    'QjfcRLBVS963TKCQDb0m8Brwwv1IKAWkErcipPNAC5+JdPmY62hE/O3L8yE+T4k4PpGwi2aiEIn25zcqKMQ1a6bgNtGN4kJqJ1tYeqFwrMNDcCFvKjMsWXLOK' +
    'N19toPbBN2PmacG9BogFoW7CQD00JTHdZlLml1yQZiv8zzBxGlQzxoxlx+Gdjo8JQDMV8w/0UmCctC/PGZDIKKPFMIGOM8M5IlUyuMel05IwY3hiHoMTLJYdg' +
    'RKvhJxsGt5wzKI8PApjpQTQmj5CkIRIO6S3REPXZjD1kyNGxABm60IxLkdu8HqQOaRmt0TcTVVFHzCdq2oX6ae2CMRuo/bWuhdHfMhfSI8PTE3xIjAuIRu7An' +
    'hv0kN+e38+1GMPYH/hq1PcyKsywdWvI1n9Y4YXzsLydgSphI4G7i/AexYRTW2RJmBPqFqTcgtUW7T6dgQlwIDfrsIsyDCphcbot5eDPgviZ8Yt0S4Ne4Iuoy/H' +
    '+//1sR/NLyhCQ==')));
end;

procedure Lape_SetClipBoard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  try
    Clipboard.AsText := PString(Params^[0])^;
  except
  end;
end;

procedure Lape_GetClipBoard(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  try
    PString(Result)^ := Clipboard.AsText;
  except
  end;
end;

procedure Lape_Import_Other(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Other';

    addGlobalFunc('procedure PlaySound(Sound: String);', @Lape_PlaySound);
    addGlobalFunc('procedure StopSound;', @Lape_StopSound);
    addGlobalFunc('procedure SetSupressExceptions(Supress: Boolean);', @Lape_SetSupressExceptions);
    addGlobalFunc('procedure HakunaMatata;', @Lape_HakunaMatata);
    addGlobalFunc('procedure Simba;', @Lape_Simba);
    addGlobalFunc('procedure SetClipBoard(Data: string);', @Lape_SetClipBoard);
    addGlobalFunc('function GetClipBoard: String', @Lape_GetClipBoard);
  end;
end;

end.

