unit simba.script_import_system;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

{$i import_uses.inc}

procedure Lape_Import_System(Compiler: TSimbaScript_Compiler);

implementation

uses
  forms, lazutf8, lpvartypes,
  simba.files;

procedure Lape_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Write(PString(Params^[0])^);
end;

procedure Lape_WriteLn(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  WriteLn('');

  Flush(Output);
end;

procedure Lape_GetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariableUTF8(PString(Params^[0])^);
end;

procedure Lape_GetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(GetCurrentThreadID());
end;

procedure Lape_GetMainThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(MainThreadID);
end;

procedure Lape_GetProcessorCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := TThread.ProcessorCount;
end;

procedure Lape_Wait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

type
  TSync = object
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSync.Execute;
type
  TSyncProcedure = procedure of object; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(Params^[0]^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_Sync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := Default(TSync);
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);
end;

procedure Lape_PauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bUnknown;
end;

procedure Lape_TerminateScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScript.State := bFalse;
end;

procedure Lape_IsTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated;
end;

procedure Lape_IsUserTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := SimbaScript.Terminated and SimbaScript.UserTerminated;
end;

procedure Lape_Import_System(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    Section := 'System';

    addBaseDefine('SIMBA' + Format('%d', [SIMBA_VERSION]));
    addBaseDefine('SIMBAMAJOR' + Format('%d', [SIMBA_MAJOR]));

    addBaseDefine('FPCFULLVERSION' + Format('%d', [FPC_FULLVERSION]));
    addBaseDefine('FPCVERSION' + Format('%d', [FPC_VERSION]));
    addBaseDefine('FPCRELEASE' + Format('%d', [FPC_RELEASE]));
    addBaseDefine('FPCPATCH' + Format('%d', [FPC_PATCH]));

    {$IFDEF CPU32}
    addBaseDefine('CPU32');
    addBaseDefine('CPU386');
    {$ENDIF}
    {$IFDEF CPU64}
    addBaseDefine('CPU64');
    {$ENDIF}
    {$IFDEF CPUAARCH64}
    addBaseDefine('CPUAARCH64');
    {$ENDIF}
    {$IFDEF WINDOWS}
    addBaseDefine('WINDOWS');
    {$ENDIF}
    {$IFDEF WIN32}
    addBaseDefine('WIN32');
    {$ENDIF}
    {$IFDEF WIN64}
    addBaseDefine('WIN64');
    {$ENDIF}
    {$IFDEF LINUX}
    addBaseDefine('LINUX');
    {$ENDIF}
    {$IFDEF DARWIN}
    addBaseDefine('DARWIN');
    {$ENDIF}

    if (SimbaScript <> nil) then
    begin
      if (SimbaScript.SimbaCommunicationServer = '') then
        addBaseDefine('SIMBAHEADLESS');

      addGlobalVar('TClient', @SimbaScript.Client, 'Client');
      addGlobalConst(SimbaScript.ScriptFile, 'ScriptFile');
    end else
    begin
      addDelayedCode(
        'const Client: TClient = nil;' + LineEnding +
        'const ScriptFile: String = "";'
      );
    end;

    addGlobalConst(GetIncludePath(), 'IncludePath');
    addGlobalConst(GetPluginPath(), 'PluginPath');
    addGlobalConst(GetFontPath(), 'FontPath');
    addGlobalConst(GetSimbaPath(), 'AppPath');
    addGlobalConst(GetScriptPath(), 'ScriptPath');

    addGlobalConst(LineEnding, 'LineEnding');

    addGlobalFunc('procedure _Write(S: String); override;', @Lape_Write);
    addGlobalFunc('procedure _WriteLn; override;', @Lape_WriteLn);

    addGlobalFunc('function GetProcessorCount: Int32;', @Lape_GetProcessorCount);
    addGlobalFunc('function GetMainThreadID: PtrUInt;', @Lape_GetMainThreadID);
    addGlobalFunc('function GetCurrentThreadID: PtrUInt;', @Lape_GetCurrentThreadID);

    addGlobalFunc('function GetEnvironmentVariable(const Name: String): String;', @Lape_GetEnvironmentVariable);
    addGlobalFunc('procedure Wait(Milliseconds: UInt32);', @Lape_Wait);

    addGlobalType('procedure() of object', 'TSyncMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
    addGlobalFunc('procedure Sync(Method: TSyncMethod);', @Lape_Sync);

    addGlobalFunc('procedure TerminateScript;', @Lape_TerminateScript);
    addGlobalFunc('procedure PauseScript;', @Lape_PauseScript);
    addGlobalFunc('function IsTerminated: Boolean;', @Lape_IsTerminated);
    addGlobalFunc('function IsUserTerminated: Boolean;', @Lape_IsUserTerminated);

    addDelayedCode(
      'procedure MemMove(constref Src; var Dst; Size: SizeInt);'                 + LineEnding +
      'begin'                                                                    + LineEnding +
      '  Move(Src, Dst, Size);'                                                  + LineEnding +
      'end;'
    );
  end;
end;

end.

