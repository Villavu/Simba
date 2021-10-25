addBaseDefine('SIMBA_DIRECTORY',   #39 + GetSimbaPath()   + #39);
addBaseDefine('DATA_DIRECTORY',    #39 + GetDataPath()    + #39);
addBaseDefine('INCLUDE_DIRECTORY', #39 + GetIncludePath() + #39);
addBaseDefine('PLUGIN_DIRECTORY',  #39 + GetPluginPath()  + #39);
addBaseDefine('FONT_DIRECTORY',    #39 + GetFontPath()    + #39);
addBaseDefine('SCRIPT_DIRECTORY',  #39 + GetScriptPath()  + #39);
addBaseDefine('PACKAGE_DIRECTORY', #39 + GetPackagePath() + #39);

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
  {$IFDEF CPUAARCH64}
  addBaseDefine('CPUAARCH64');
  {$ELSE}
  addBaseDefine('CPU64');
  {$ENDIF}
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

addGlobalType('array of String', 'TStringArray');
addGlobalType('array of TStringArray', 'T2DStringArray');
addGlobalType('array of Integer', 'TIntegerArray');
addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
addGlobalType('array of T2DIntegerArray', 'T3DIntegerArray');
addGlobalType('array of Byte', 'TByteArray');
addGlobalType('array of TByteArray', 'T2DByteArray');
addGlobalType('array of Single', 'TSingleArray');
addGlobalType('array of TSingleArray', 'T2DSingleArray');
addGlobalType('array of Double', 'TDoubleArray');
addGlobalType('array of TDoubleArray', 'T2DDoubleArray');
addGlobalType('array of Extended', 'TExtendedArray');
addGlobalType('array of TExtendedArray', 'T2DExtendedArray');
addGlobalType('array of Boolean', 'TBooleanArray');
addGlobalType('array of TBooleanArray', 'T2DBooleanArray');
addGlobalType('array of Variant', 'TVariantArray');
addGlobalType('array of Char', 'TCharArray');

addGlobalType('record X1, Y1, X2, Y2: Integer; end', 'TBox');
addGlobalType('array of TBox', 'TBoxArray');
addGlobalType('array of TBoxArray', 'T2DBoxArray');

addGlobalType('record X, Y: Integer; end', 'TPoint');
addGlobalType('array of TPoint', 'TPointArray');
addGlobalType('array of TPointArray', 'T2DPointArray');

addGlobalType('record X, Y: Single; end', 'TPointF');
addGlobalType('Integer', 'TColor');

addGlobalType('record Top, Right, Btm, Left: TPoint; end', 'TRectangle');

addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');

addGlobalFunc('function Point(const X, Y: Integer): TPoint', @_LapePoint);
addGlobalFunc('function IntToBox(x1, y1, x2, y2: Integer): TBox', @_LapeIntToBox);
addGlobalFunc('function IntInBox(x, y: Integer; Box: TBox): Boolean', @_LapeIntInBox);
addGlobalFunc('function PointToBox(PT1, PT2: TPoint): TBox', @_LapePointToBox);
addGlobalFunc('function PointInBox(PT: TPoint; Box: TBox): Boolean', @_LapePointInBox);
addGlobalFunc('function MiddleBox(b: TBox): TPoint', @_LapeMiddleBox);

addGlobalVar(GetIncludePath(), 'IncludePath').isConstant := True;
addGlobalVar(GetPluginPath(), 'PluginPath').isConstant := True;
addGlobalVar(GetFontPath(), 'FontPath').isConstant := True;
addGlobalVar(GetSimbaPath(), 'AppPath').isConstant := True;
addGlobalVar(GetScriptPath(), 'ScriptPath').isConstant := True;

addGlobalFunc('function GetProcessorCount: Integer', @_LapeGetProcessorCount);
addGlobalFunc('function GetMainThreadID: PtrUInt', @_LapeGetMainThreadID);
addGlobalFunc('function GetCurrentThreadID: PtrUInt', @_LapeGetCurrentThreadID);

addGlobalFunc('function GetEnvironmentVariable(const Name: String): String', @_LapeGetEnvironmentVariable);
addGlobalFunc('procedure Wait(Milliseconds: UInt32)', @_LapeWait);

addGlobalType('procedure() of object', 'TSyncMethod', {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}FFI_CDECL{$ELSE}FFI_DEFAULT_ABI{$ENDIF});
addGlobalFunc('procedure Sync(Method: TSyncMethod)', @_LapeSync);

addGlobalFunc('procedure TerminateScript', @_LapeTerminateScript);
addGlobalFunc('procedure PauseScript', @_LapePauseScript);
addGlobalFunc('function IsTerminated: Boolean', @_LapeIsTerminated);
addGlobalFunc('function IsUserTerminated: Boolean', @_LapeIsUserTerminated);

addDelayedCode(
  'procedure MemMove(constref Src; var Dst; Size: SizeInt);' + LineEnding +
  'begin'                                                    + LineEnding +
  '  Move(Src, Dst, Size);'                                  + LineEnding +
  'end;'
);

FSection := ''; // hide from codeinsight

addGlobalFunc('procedure _Write(S: String); override', @_LapeWrite);
addGlobalFunc('procedure _WriteLn; override', @_LapeWriteLn);

addGlobalFunc('procedure _Sort(var a: TIntegerArray); overload;', @_LapeSortTIA);

addGlobalFunc('function _Unique(const a: TPointArray): TPointArray; overload', @_LapeUniqueTPA);
addGlobalFunc('function _Unique(const a: TIntegerArray): TIntegerArray; overload', @_LapeUniqueTIA);

addGlobalFunc('function _IndicesOf(const Item: TPoint; const a: TPointArray): TIntegerArray; overload', @_LapeIndicesOfTPA);
addGlobalFunc('function _IndicesOf(const Item: Integer; const a: TIntegerArray): TIntegerArray; overload', @_LapeIndicesOfTIA);
addGlobalFunc('function _IndicesOf(const Item: String; const a: TStringArray): TIntegerArray; overload', @_LapeIndicesOfTSA);

addGlobalFunc('function _IndexOf(const Item: TPoint; const a: TPointArray): Integer; overload', @_LapeIndexOfTPA);
addGlobalFunc('function _IndexOf(const Item: Integer; const a: TIntegerArray): Integer; overload', @_LapeIndexOfTIA);
addGlobalFunc('function _IndexOf(const Item: String; const a: TStringArray): Integer; overload', @_LapeIndexOfTSA);
