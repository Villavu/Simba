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

addGlobalFunc('function Box(const X1, Y1, X2, Y2: Integer): TBox', @_LapeBox);
addGlobalFunc('function Point(const X, Y: Integer): TPoint', @_LapePoint);
addGlobalFunc('function IntToBox(x1, y1, x2, y2: Integer): TBox', @_LapeIntToBox);
addGlobalFunc('function IntInBox(x, y: Integer; Box: TBox): Boolean', @_LapeIntInBox);
addGlobalFunc('function PointToBox(PT1, PT2: TPoint): TBox', @_LapePointToBox);
addGlobalFunc('function PointInBox(PT: TPoint; Box: TBox): Boolean', @_LapePointInBox);
addGlobalFunc('function MiddleBox(b: TBox): TPoint', @_LapeMiddleBox);

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

addGlobalFunc('procedure TerminateScript', @_LapeTerminateScript);
addGlobalFunc('procedure PauseScript', @_LapePauseScript);

addDelayedCode(
  'procedure MemMove(constref Src; var Dst; Size: SizeInt);' + LineEnding +
  'begin'                                                    + LineEnding +
  '  Move(Src, Dst, Size);'                                  + LineEnding +
  'end;'
);

popSection();

