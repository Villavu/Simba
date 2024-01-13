unit simba.import_system;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportSystem(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics,
  lptypes, lpvartypes, lpparser,
  simba.nativeinterface, simba.env;

(*
System
======
Base methods and types.
*)

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

(*
GetMem
~~~~~~
> function GetMem(i: SizeInt): Pointer;
*)

(*
AllocMem
~~~~~~~~
> function AllocMem(i: SizeInt): Pointer;
*)

(*
FreeMem
~~~~~~~
> procedure FreeMem(p: Pointer);
*)

(*
ReallocMem
~~~~~~~~~~
> procedure ReallocMem(var p: Pointer; s: SizeInt);
*)

(*
FillMem
~~~~~~~
> procedure FillMem(var p; s: SizeInt; b: UInt8 = 0);
*)

(*
Move
~~~~
> procedure Move(constref Src; var Dst; s: SizeInt);
*)

(*
CompareMem
~~~~~~~~~~
> function CompareMem(constref p1, p2; Length: SizeInt): EvalBool;
*)

(*
Assigned
~~~~~~~~
> function Assigned(constref p): EvalBool;
*)

(*
Delete
~~~~~~
> procedure Delete(A: array; Index: Int32; Count: Int32 = Length(A));
*)

(*
Insert
~~~~~~
> procedure Insert(Item: Anything; A: array; Index: Int32);
*)

(*
Copy
~~~~
> procedure Copy(A: array; Index: Int32 = 0; Count: Int32 = Length(A));
*)

(*
SetLength
~~~~~~~~~
> procedure SetLength(A: array; Length: Int32);
*)

(*
Low
~~~
> function Low(A: array): Int32;
*)

(*
High
~~~~
> function High(A: array): Int32;
*)

(*
Length
~~~~~~
> function Length(A: array): Int32;
*)

(*
WriteLn
~~~~~~~
> procedure WriteLn(Args: Anything);
*)

(*
Write
~~~~~
> procedure Write(Args: Anything);
*)

(*
Swap
~~~~
> procedure Swap(var A, B: Anything);
*)

(*
SizeOf
~~~~~~
> function SizeOf(A: Anything): Int32;
*)

(*
ToString
~~~~~~~~
> function ToString(A: Anything): String;
*)

(*
ToStr
~~~~~
> function ToStr(A: Anything): String;
*)

(*
Inc
~~~
> function Inc(var X: Ordinal; Amount: SizeInt = 1): Ordinal;
*)

(*
Dec
~~~
> function Dec(var X: Ordinal; Amount: SizeInt = 1): Ordinal;
*)

(*
Ord
~~~
> function Ord(X: Ordinal): Int32;
*)

(*
SleepUntil
~~~~~~~~~~
> function SleepUntil(Condition: BoolExpr; Interval, Timeout: Int32): Boolean;
*)

(*
Default
~~~~~~~
> function Default(T: AnyType): AnyType;
*)

(*
Sort
~~~~
> procedure Sort(var A: array);
> procedure Sort(var A: array; Weights: array of Ordinal; LowToHigh: Boolean);
> procedure Sort(var A: array; CompareFunc: function(constref L, R: Anything): Int32);
*)

(*
Sorted
~~~~~~
> function Sorted(const A: array): array; overload;
> function Sorted(const A: array; CompareFunc: function(constref L, R: Anything): Int32): array;
> function Sorted(const A: array; Weights: array of Ordinal; LowToHigh: Boolean): array;
*)

(*
Unique
~~~~~~
> function Unique(const A: array): array;
*)

(*
Reverse
~~~~~~~
> procedure Reverse(var A: array);
*)

(*
Reversed
~~~~~~~~
> function Reversed(const A: array): array;
*)

(*
IndexOf
~~~~~~~
> function IndexOf(const Item: T; const A: array): Integer;
*)

(*
IndicesOf
~~~~~~~~~
> function IndicesOf(const Item: T; const A: array): TIntegerArray;
*)

(*
Contains
~~~~~~~~
> function Contains(const Item: T; const A: array): Boolean;
*)

(*
GetCallerAddress
~~~~~~~~~~~~~~~~
> function GetCallerAddress: Pointer;
*)

(*
GetCallerName
~~~~~~~~~~~~~
> function GetCallerName: String;
*)

(*
GetCallerLocation
~~~~~~~~~~~~~~~~~
> function GetCallerLocation: Pointer;
*)

(*
GetCallerLocationStr
~~~~~~~~~~~~~~~~~~~~
> function GetCallerLocationStr: String;
*)

(*
GetExceptionLocation
~~~~~~~~~~~~~~~~~~~~
> function GetExceptionLocation: Pointer;
*)

(*
GetExceptionLocationStr
~~~~~~~~~~~~~~~~~~~~~~~
> function GetExceptionLocationStr: String;
*)

(*
GetExceptionMessage
~~~~~~~~~~~~~~~~~~~
> function GetExceptionMessage: String;
*)

(*
GetScriptMethodName
~~~~~~~~~~~~~~~~~~~
> function GetScriptMethodName(Address: Pointer): String;
*)

(*
DumpCallStack
~~~~~~~~~~~~~
> function DumpCallStack(Start: Integer = 0): String;
*)

(*
LoadLibrary
~~~~~~~~~~~
> function LoadLibrary(const Name: string): TLibHandle;
*)

(*
GetProcAddress
~~~~~~~~~~~~~~
> function GetProcAddress(Lib: TlibHandle; const ProcName: string): ConstPointer;
*)

(*
FreeLibrary
~~~~~~~~~~~
> function FreeLibrary(Lib: TLibHandle): EvalBool;
*)

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

