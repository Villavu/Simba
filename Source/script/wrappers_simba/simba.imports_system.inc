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

addGlobalType(
  'record'               + LineEnding +
  '  Position: Integer;' + LineEnding +
  '  Length: Integer;'   + LineEnding +
  '  Match: String;'     + LineEnding +
  'end',
  'TRegExprMatch');

addGlobalType('array of TRegExprMatch', 'TRegExprMatchArray');

addGlobalType('record Top, Right, Btm, Left: TPoint; end', 'TRectangle');
addGlobalType('array of TRectangle', 'TRectangleArray');

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

FSection := 'System.TRectangle';

addGlobalFunc('function TRectangle.ToTPA: TPointArray; constref', @_LapeRectangle_ToTPA);
addGlobalFunc('function TRectangle.Bounds: TBox; constref', @_LapeRectangle_Bounds);
addGlobalFunc('function TRectangle.Middle: TPoint; constref', @_LapeRectangle_Middle);
addGlobalFunc('function TRectangle.Offset(X, Y: Integer): TBox; constref; overload', @_LapeRectangle_Offset);
addGlobalFunc('function TRectangle.Offset(P: TPoint): TRectangle; constref; overload', @_LapeRectangle_OffsetPoint);
addGlobalFunc('function TRectangle.Expand(Amount: Integer): TRectangle; constref', @_LapeRectangle_Expand);
addGlobalFunc('function TRectangle.Contains(X, Y: Integer): Boolean; constref; overload', @_LapeRectangle_Contains);
addGlobalFunc('function TRectangle.Contains(P: TPoint): Boolean; constref; overload', @_LapeRectangle_ContainsPoint);
addGlobalFunc('function TRectangle.Contains(B: TBox): Boolean; constref; overload', @_LapeRectangle_ContainsBox);
addGlobalFunc('function TRectangle.LongSideLen: Integer; constref', @_LapeRectangle_LongSideLen);
addGlobalFunc('function TRectangle.ShortSideLen: Integer; constref', @_LapeRectangle_ShortSideLen);
addGlobalFunc('function TRectangle.Area: Integer; constref', @_LapeRectangle_Area);
addGlobalFunc('function TRectangle.Rotate(Radians: Double): TRectangle; constref', @_LapeRectangle_Rotate);
addGlobalFunc('function TRectangle.RotateFast(Degrees: Integer): TRectangle; constref', @_LapeRectangle_RotateFast);
addGlobalFunc('procedure TRectangle.Normalize;', @_LapeRectangle_Normalize);

FSection := 'System.TBox';

addGlobalFunc('function TBox.ToRectangle: TRectangle; constref', @_LapeBox_ToRectangle);
addGlobalFunc('function TBox.Middle: TPoint; constref', @_LapeBox_Middle);
addGlobalFunc('function TBox.Width: Integer; constref', @_LapeBox_Width);
addGlobalFunc('function TBox.Height: Integer; constref', @_LapeBox_Height);
addGlobalFunc('function TBox.Area: Integer; constref', @_LapeBox_Area);
addGlobalFunc('function TBox.Offset(X, Y: Integer): TBox; constref; overload;', @_LapeBox_Offset);
addGlobalFunc('function TBox.Offset(P: TPoint): TBox; constref; overload;', @_LapeBox_OffsetPoint);
addGlobalFunc('function TBox.Contains(X, Y: Integer): Boolean; constref; overload', @_LapeBox_Contains);
addGlobalFunc('function TBox.Contains(P: TPoint): Boolean; constref; overload', @_LapeBox_ContainsPoint);
addGlobalFunc('function TBox.Contains(Box: TBox): Boolean; constref; overload', @_LapeBox_ContainsBox);
addGlobalFunc('function TBox.Combine(Other: TBox): TBox; constref', @_LapeBox_Combine);
addGlobalFunc('function TBox.Expand(Size: Integer): TBox; constref; overload', @_LapeBox_Expand);
addGlobalFunc('function TBox.Expand(Size: Integer; MaxBounds: TBox): TBox; constref; overload', @_LapeBox_ExpandEx);
addGlobalFunc('function TBox.Expand(Width, Height: Integer): TBox; constref; overload', @_LapeBox_ExpandWidthHeight);
addGlobalFunc('function TBox.Expand(Width, Height: Integer; MaxBounds: TBox): TBox; constref; overload', @_LapeBox_ExpandWidthHeightEx);
addGlobalFunc('procedure TBox.Clip(Box: TBox); constref', @_LapeBox_Clip);
addGlobalFunc('procedure TBox.Normalize; constref', @_LapeBox_Normalize);

FSection := 'System.TPoint';

addGlobalFunc('function TPoint.InBox(Box: TBox): Boolean; constref', @_LapePoint_InBox);
addGlobalFunc('function TPoint.InRect(Rect: TRectangle): Boolean; constref', @_LapePoint_InRect);
addGlobalFunc('function TPoint.InCircle(Center: TPoint; Radius: Integer): Boolean; constref', @_LapePoint_InCircle);
addGlobalFunc('function TPoint.DistanceTo(Other: TPoint): Integer; constref', @_LapePoint_DistanceTo);
addGlobalFunc('function TPoint.AngleBetween(Other: TPoint): Double; constref', @_LapePoint_AngleBetween);
addGlobalFunc('function TPoint.Offset(P: TPoint): TPoint; constref; overload', @_LapePoint_OffsetPoint);
addGlobalFunc('function TPoint.Offset(X, Y: Integer): TPoint; constref; overload', @_LapePoint_Offset);
addGlobalFunc('function TPoint.Rotate(Radians: Double; Center: TPoint): TPoint; constref', @_LapePoint_Rotate);
addGlobalFunc('function TPoint.RotateFast(Degrees: Integer; Center: TPoint): TPoint; constref', @_LapePoint_RotateFast);
addGlobalFunc('function TPoint.Scale(Radians: Double; Radius: Integer): TPoint; constref', @_LapePoint_Scale);

FSection := 'System.String';

addGlobalFunc('function String.NumberChars: String; static;', @_LapeString_NumberChars);
addGlobalFunc('function String.AlphaChars: String; static;', @_LapeString_AlphaChars);
addGlobalFunc('function String.Before(const Value: String): String; constref', @_LapeString_Before);
addGlobalFunc('function String.After(const Value: String): String; constref', @_LapeString_After);
addGlobalFunc('function String.StartsWith(const Value: String; CaseSenstive: Boolean = True): Boolean; constref', @_LapeString_StartsWith);
addGlobalFunc('function String.EndsWith(const Value: String; CaseSenstive: Boolean = True): Boolean; constref', @_LapeString_EndsWith);
addGlobalFunc('function String.Upper: String; constref', @_LapeString_Upper);
addGlobalFunc('function String.Lower: String; constref', @_LapeString_Lower);
addGlobalFunc('function String.Split(const Separator: String): TStringArray; constref', @_LapeString_Split);
addGlobalFunc('function String.PadLeft(Count: Integer; PaddingChar: Char = #32): string; constref', @_LapeString_PadLeft);
addGlobalFunc('function String.PadRight(Count: Integer; PaddingChar: Char = #32): string; constref', @_LapeString_PadRight);
addGlobalFunc('function String.Replace(const OldValue, NewValue: string): string; constref; overload', @_LapeString_Replace);
addGlobalFunc('function String.Replace(const OldValue, NewValue: string; ReplaceFlags: TReplaceFlags): string; constref; overload', @_LapeString_ReplaceEx);
addGlobalFunc('function String.Trim: string; constref; overload', @_LapeString_Trim);
addGlobalFunc('function String.Trim(const TrimChars: array of Char): string; constref; overload', @_LapeString_TrimEx);
addGlobalFunc('function String.TrimLeft: string; constref; overload', @_LapeString_TrimLeft);
addGlobalFunc('function String.TrimLeft(const TrimChars: array of Char): string; constref; overload', @_LapeString_TrimLeftEx);
addGlobalFunc('function String.TrimRight: string; constref; overload', @_LapeString_TrimRight);
addGlobalFunc('function String.TrimRight(const TrimChars: array of Char): string; constref; overload', @_LapeString_TrimRightEx);
addGlobalFunc('function String.RegExprSplit(const Pattern: String): TStringArray; constref', @_LapeString_RegExprSplit);
addGlobalFunc('function String.RegExprFind(const Pattern: String): TRegExprMatch; constref', @_LapeString_RegExprFind);
addGlobalFunc('function String.RegExprFindAll(const Pattern: String): TRegExprMatchArray; constref', @_LapeString_RegExprFindAll);
addGlobalFunc('function String.RegExprExists(const Pattern: String): Boolean; constref', @_LapeString_RegExprExists);
addGlobalFunc('function String.Copy: String; constref; overload', @_LapeString_Copy);
addGlobalFunc('function String.Copy(const StartIndex, Count: Integer): String; constref; overload', @_LapeString_CopyEx);
addGlobalFunc('function String.Copy(const StartIndex: Integer): String; constref; overload', @_LapeString_CopyExEx);
addGlobalFunc('function String.CopyRange(const StartIndex, EndIndex: Integer): String; constref', @_LapeString_CopyRange);
addGlobalFunc('procedure String.Delete(const StartIndex, Count: Integer)', @_LapeString_Delete);
addGlobalFunc('procedure String.DeleteRange(const StartIndex, EndIndex: Integer)', @_LapeString_DeleteRange);
addGlobalFunc('procedure String.Insert(const Value: String; Index: Integer)', @_LapeString_Insert);
addGlobalFunc('function String.IndexOf(const Value: String): Integer; constref; overload', @_LapeString_IndexOf);
addGlobalFunc('function String.IndexOf(const Value: String; Offset: Integer): Integer; constref; overload', @_LapeString_IndexOfOffset);
addGlobalFunc('function String.LastIndexOf(const Value: String): Integer; constref; overload', @_LapeString_LastIndexOf);
addGlobalFunc('function String.LastIndexOf(const Value: String; Offset: Integer): Integer; constref; overload', @_LapeString_LastIndexOfOffset);
addGlobalFunc('function String.IndexOfAny(Values: TStringArray): Integer; constref; overload', @_LapeString_IndexOfAny);
addGlobalFunc('function String.IndexOfAny(Values: TStringArray; Offset: Integer): Integer; constref; overload', @_LapeString_IndexOfAnyOffset);
addGlobalFunc('function String.Contains(const Value: String; CaseSenstive: Boolean = True): Boolean; constref', @_LapeString_Contains);
addGlobalFunc('function String.ContainsAny(const Values: TStringArray; CaseSenstive: Boolean = True): Boolean; constref', @_LapeString_ContainsAny);
addGlobalFunc('function String.IndicesOf(const Value: String): TIntegerArray; constref; overload', @_LapeString_IndicesOf);
addGlobalFunc('function String.IndicesOf(const Value: String; Offset: Integer): TIntegerArray; constref; overload', @_LapeString_IndicesOfOffset);
addGlobalFunc('function String.Between(const S1, S2: String): String; constref', @_LapeString_Between);
addGlobalFunc('function String.BetweenAll(const S1, S2: String): TStringArray; constref', @_LapeString_BetweenAll);
addGlobalFunc('function String.Extract(const Characters: String): String; constref', @_LapeString_Extract);
addGlobalFunc('function String.ExtractNumber(Default: Int64 = -1): Int64; constref; overload', @_LapeString_ExtractNumber);
addGlobalFunc('function String.ExtractNumber(const NumberChars: String; Default: Int64 = -1): Int64; constref; overload', @_LapeString_ExtractNumberEx);

// hide from codeinsight
// native wrappers are for speed

FSection := '';

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
