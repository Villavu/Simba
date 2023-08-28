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
  lptypes, lpvartypes, lpparser, ffi,
  simba.nativeinterface, simba.env, simba.threading;

procedure _LapePreciseSleep(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PreciseSleep(PUInt32(Params^[0])^);
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

    addGlobalType(getBaseType(DetermineIntType(SizeOf(TColor), False)).createCopy(), 'TColor');
    addGlobalType('array of TColor', 'TColorArray');

    addGlobalType('array of TStringArray', 'T2DStringArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of Int64', 'TInt64Array');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of Variant', 'TVariantArray');

    addGlobalType('record X, Y: Integer; end', 'TPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record Top, Right, Bottom, Left: TPoint; end;', 'TQuad');
    addGlobalType('array of TQuad', 'TQuadArray');

    addGlobalType('record X1, Y1, X2, Y2: Integer; end', 'TBox');
    addGlobalType('array of TBox', 'TBoxArray');

    addGlobalType('record X, Y: Single; end', 'TPointF');

    addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');

    addGlobalFunc('function GetEnvVar(Name: String): String', @_LapeGetEnvVar);
    addGlobalFunc('function GetEnvVars: TStringArray', @_LapeGetEnvVars);

    addGlobalVar('', 'SCRIPT_FILE').isConstant := True; // Filled in later
    addGlobalVar(GetTickCount64(), 'SCRIPT_START_TIME').isConstant := True;
    addGlobalVar(SimbaEnv.IncludesPath, 'INCLUDES_DIR').isConstant := True;
    addGlobalVar(SimbaEnv.PluginsPath, 'PLUGINS_DIR').isConstant := True;
    addGlobalVar(SimbaEnv.SimbaPath, 'SIMBA_DIR').isConstant := True;
    addGlobalVar(SimbaEnv.ScriptsPath, 'SCRIPTS_DIR').isConstant := True;
    addGlobalVar(SimbaEnv.DataPath, 'SIMBA_DATA_DIR').isConstant := True;
    addGlobalVar(SimbaEnv.ScreenshotsPath, 'SCREENSHOTS_DIR').isConstant := True;

    ImportingSection := '';
  end;
end;

end.

