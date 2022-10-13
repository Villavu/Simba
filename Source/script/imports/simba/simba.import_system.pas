unit simba.import_system;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, ffi,
  simba.script_compiler, simba.mufasatypes, simba.nativeinterface;

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

procedure _LapeGetThreadCount(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPtrUInt(Result)^ := TThread.ProcessorCount;
end;

procedure ImportSystem(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'System';

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
    addGlobalType('array of Integer', 'TIntegerArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of T2DIntegerArray', 'T3DIntegerArray');
    addGlobalType('array of Int64', 'TInt64Array');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of Single', 'TSingleArray');
    addGlobalType('array of Double', 'TDoubleArray');
    addGlobalType('array of Extended', 'TExtendedArray');
    addGlobalType('array of Boolean', 'TBooleanArray');
    addGlobalType('array of Variant', 'TVariantArray');

    addGlobalType('record X, Y: Integer; end', 'TPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record Top: TPoint; Right: TPoint; Bottom: TPoint; Left: TPoint; end;', 'TQuad');
    addGlobalType('array of TQuad', 'TQuadArray');

    addGlobalType('record X1, Y1, X2, Y2: Integer; end', 'TBox');
    addGlobalType('array of TBox', 'TBoxArray');

    addGlobalType('record X, Y: Single; end', 'TPointF');
    addGlobalType('Integer', 'TColor');

    addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');

    addGlobalFunc('function GetThreadCount: Integer', @_LapeGetThreadCount);
    addGlobalFunc('function GetMainThreadID: PtrUInt', @_LapeGetMainThreadID);
    addGlobalFunc('function GetCurrentThreadID: PtrUInt', @_LapeGetCurrentThreadID);
    addGlobalFunc('function GetEnvironmentVariable(const Name: String): String', @_LapeGetEnvironmentVariable);

    addGlobalType('procedure() of object', 'TSyncMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
    addGlobalFunc('procedure Sync(Method: TSyncMethod)', @_LapeSync);

    addGlobalFunc(
      'procedure MemMove(constref Src; var Dst; Size: SizeInt); deprecated;', [
      'begin',
      '  Move(Src, Dst, Size);',
      'end;'
    ]);

    ImportingSection := '';
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportSystem);

end.

