unit simba.import_system;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, ffi,
  simba.script_compiler, simba.mufasatypes, simba.scriptthread, simba.nativeinterface, simba.math, simba.files;

procedure _LapeGetEnvironmentVariable(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := GetEnvironmentVariable(PString(Params^[0])^);
end;

procedure _LapeGetCurrentThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(GetCurrentThreadID());
end;

procedure _LapeGetMainThreadID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := PtrUInt(MainThreadID);
end;

procedure _LapeGetProcessorCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := TThread.ProcessorCount;
end;

procedure _LapeWait(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure _LapePreciseSleep(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaNativeInterface.PreciseSleep(PUInt32(Params^[0])^);
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

procedure _LapeSync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := Default(TSync);
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);
end;

procedure _LapePauseScript(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SimbaScriptThread.Script.State := ESimbaScriptState.STATE_PAUSED;
end;

procedure _LapePoint(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := Point(PInteger(Params^[0])^, PInteger(Params^[1])^);
end;

procedure _LapeBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeBoxEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := Box(PPoint(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeIntToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := IntToBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeIntInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := IntInBox(PInteger(Params^[0])^, PInteger(Params^[1])^, PBox(Params^[2])^);
end;

procedure _LapePointToBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := PointToBox(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure _LapePointInBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PointInBox(PPoint(Params^[0])^, PBox(Params^[1])^);
end;

procedure _LapeMiddleBox(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := MiddleBox(PBox(Params^[0])^);
end;

procedure ImportSystem(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('System');

    addBaseDefine('SIMBA' + Format('%d', [SIMBA_VERSION]));
    addBaseDefine('SIMBAMAJOR' + Format('%d', [SIMBA_MAJOR]));
    addBaseDefine('FPC' + Format('%d', [FPC_FULLVERSION]));

    {$IF DEFINED(CPU32)}
    addBaseDefine('CPU32');
    {$ELSEIF DEFINED(CPUAARCH64)}
    addBaseDefine('CPUAARCH64');
    {$ELSEIF DEFINED(CPU64)}
    addBaseDefine('CPU64');
    {$ENDIF}

    {$IF DEFINED(WINDOWS)}
    addBaseDefine('WINDOWS');
    {$ELSEIF DEFINED(DARWIN)}
    addBaseDefine('DARWIN');
    {$ELSEIF DEFINED(LINUX)}
    addBaseDefine('LINUX');
    {$ENDIF}

    addGlobalType('array of String', 'TStringArray');
    addGlobalType('array of TStringArray', 'T2DStringArray');
    addGlobalType('array of Integer', 'TIntegerArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of T2DIntegerArray', 'T3DIntegerArray');
    addGlobalType('array of Int64', 'TInt64Array');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of Single', 'TSingleArray');
    addGlobalType('array of Double', 'TDoubleArray');
    addGlobalType('array of Extended', 'TExtendedArray');
    addGlobalType('array of TExtendedArray', 'T2DExtendedArray');
    addGlobalType('array of Boolean', 'TBooleanArray');
    addGlobalType('array of Variant', 'TVariantArray');

    addGlobalType('record X1, Y1, X2, Y2: Integer; end', 'TBox');
    addGlobalType('array of TBox', 'TBoxArray');

    addGlobalType('record X, Y: Integer; end', 'TPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record X, Y: Single; end', 'TPointF');
    addGlobalType('Integer', 'TColor');

    addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');

    addGlobalFunc('function Box(X1, Y1, X2, Y2: Integer): TBox; overload', @_LapeBox);
    addGlobalFunc('function Box(Mid: TPoint; XRad, YRad: Integer): TBox; overload', @_LapeBoxEx);
    addGlobalFunc('function Point(X, Y: Integer): TPoint', @_LapePoint);
    addGlobalFunc('function IntToBox(X1, Y1, X2, Y2: Integer): TBox', @_LapeIntToBox);
    addGlobalFunc('function IntInBox(X, Y: Integer; Box: TBox): Boolean', @_LapeIntInBox);
    addGlobalFunc('function PointToBox(PT1, PT2: TPoint): TBox', @_LapePointToBox);
    addGlobalFunc('function PointInBox(PT: TPoint; Box: TBox): Boolean', @_LapePointInBox);
    addGlobalFunc('function MiddleBox(B: TBox): TPoint', @_LapeMiddleBox);

    addGlobalVar('', 'ScriptFile').isConstant := True;
    addGlobalVar(GetIncludePath(), 'IncludePath').isConstant := True;
    addGlobalVar(GetPluginPath(), 'PluginPath').isConstant := True;
    addGlobalVar(GetFontPath(), 'FontPath').isConstant := True;
    addGlobalVar(GetSimbaPath(), 'AppPath').isConstant := True;
    addGlobalVar(GetScriptPath(), 'ScriptPath').isConstant := True;

    addGlobalFunc('function GetProcessorCount: Integer', @_LapeGetProcessorCount);
    addGlobalFunc('function GetMainThreadID: PtrUInt', @_LapeGetMainThreadID);
    addGlobalFunc('function GetCurrentThreadID: PtrUInt', @_LapeGetCurrentThreadID);
    addGlobalFunc('function GetEnvironmentVariable(const Name: String): String', @_LapeGetEnvironmentVariable);

    addGlobalType('procedure() of object', 'TSyncMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
    addGlobalFunc('procedure Sync(Method: TSyncMethod)', @_LapeSync);

    addDelayedCode('procedure TerminateScript; begin Halt(); end;');
    addGlobalFunc('procedure PauseScript', @_LapePauseScript);

    addDelayedCode(
      'procedure MemMove(constref Src; var Dst; Size: SizeInt);' + LineEnding +
      'begin'                                                    + LineEnding +
      '  Move(Src, Dst, Size);'                                  + LineEnding +
      'end;'
    );

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportSystem);

end.

