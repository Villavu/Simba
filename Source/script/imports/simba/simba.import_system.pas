unit simba.import_system;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportSystem(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics,
  lptypes, lpparser, ffi,
  simba.nativeinterface;

procedure _LapeGetCurrentThreadID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPtrUInt(Result)^ := PtrUInt(GetCurrentThreadID());
end;

procedure _LapeGetMainThreadID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPtrUInt(Result)^ := PtrUInt(MainThreadID);
end;

procedure _LapePreciseSleep(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
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

procedure _LapeSync(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
var
  Sync: TSync;
begin
  Sync := Default(TSync);
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);
end;

procedure _LapeGetThreadCount(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPtrUInt(Result)^ := TThread.ProcessorCount;
end;

procedure _LapeGetEnvVar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := GetEnvironmentVariable(PString(Params^[0])^);
end;

procedure _LapeGetEnvVars(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  function _GetEnvVars: TStringArray;
  var
    Count, I: Integer;
  begin
    Count := 0;

    SetLength(Result, GetEnvironmentVariableCount() + 1);
    for I := 1 to GetEnvironmentVariableCount() do
      if (GetEnvironmentString(I) <> '') then
      begin
        Result[Count] := GetEnvironmentString(I);
        Inc(Count);
      end;
    SetLength(Result, Count);
  end;

begin
  PStringArray(Result)^ := _GetEnvVars();
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

    {$IF SizeOf(TColor) = 4}
    addGlobalType('type Int32', 'TColor');
    {$ELSE}
    addGlobalType('type Int64', 'TColor');
    {$ENDIF}
    addGlobalType('array of TColor', 'TColorArray');

    addGlobalType('array of TStringArray', 'T2DStringArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of Int64', 'TInt64Array');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of Variant', 'TVariantArray');

    addGlobalType('record X, Y: Integer; end', 'TPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record Top: TPoint; Right: TPoint; Bottom: TPoint; Left: TPoint; end;', 'TQuad');
    addGlobalType('array of TQuad', 'TQuadArray');

    addGlobalType('record X1, Y1, X2, Y2: Integer; end', 'TBox');
    addGlobalType('array of TBox', 'TBoxArray');

    addGlobalType('record X, Y: Single; end', 'TPointF');

    addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');

    addGlobalFunc('function GetThreadCount: Integer', @_LapeGetThreadCount);
    addGlobalFunc('function GetMainThreadID: PtrUInt', @_LapeGetMainThreadID);
    addGlobalFunc('function GetCurrentThreadID: PtrUInt', @_LapeGetCurrentThreadID);

    addGlobalType('procedure() of object', 'TSyncMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
    addGlobalFunc('procedure Sync(Method: TSyncMethod)', @_LapeSync);

    addGlobalFunc(
      'procedure MemMove(constref Src; var Dst; Size: SizeInt); deprecated "Use Move";', [
      'begin',
      '  Move(Src, Dst, Size);',
      'end;'
    ]);

    addGlobalFunc('function GetEnvVar(Name: String): String', @_LapeGetEnvVar);
    addGlobalFunc('function GetEnvVars: TStringArray', @_LapeGetEnvVars);

    ImportingSection := '';
  end;
end;

end.

