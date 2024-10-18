unit simba.import_base;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportBase(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics, Variants,
  lptypes, lpvartypes, lpparser, ffi,
  simba.nativeinterface, simba.env, simba.baseclass, simba.vartype_ordarray;

(*
Base
====
Base methods and types.
*)

(*
GetMem
------
```
function GetMem(i: SizeInt): Pointer;
```
*)

(*
AllocMem
--------
```
function AllocMem(i: SizeInt): Pointer;
```
*)

(*
FreeMem
-------
```
procedure FreeMem(p: Pointer);
```
*)

(*
ReallocMem
----------
```
procedure ReallocMem(var p: Pointer; s: SizeInt);
```
*)

(*
FillMem
-------
```
procedure FillMem(var p; s: SizeInt; b: UInt8 = 0);
```
*)

(*
Move
----
```
procedure Move(constref Src; var Dst; s: SizeInt);
```
*)

(*
CompareMem
----------
```
function CompareMem(constref p1, p2; Length: SizeInt): EvalBool;
```
*)

(*
Assigned
--------
```
function Assigned(constref p): EvalBool;
```
*)

(*
Delete
------
```
procedure Delete(A: array; Index: Int32; Count: Int32 = Length(A));
```
*)

(*
Insert
------
```
procedure Insert(Item: Anything; A: array; Index: Int32);
```
*)

(*
Copy
----
```
procedure Copy(A: array; Index: Int32 = 0; Count: Int32 = Length(A));
```
*)

(*
SetLength
---------
```
procedure SetLength(A: array; Length: Int32);
```
*)

(*
Low
---
```
function Low(A: array): Int32;
```
*)

(*
High
----
```
function High(A: array): Int32;
```
*)

(*
Length
------
```
function Length(A: array): Int32;
```
*)

(*
WriteLn
-------
```
procedure WriteLn(Args: Anything);
```
*)

(*
Write
-----
```
procedure Write(Args: Anything);
```
*)

(*
Swap
----
```
procedure Swap(var A, B: Anything);
```
*)

(*
SizeOf
------
```
function SizeOf(A: Anything): Int32;
```
*)

(*
ToString
--------
```
function ToString(A: Anything): String;
```
*)

(*
ToStr
-----
```
function ToStr(A: Anything): String;
```
*)

(*
Inc
---
```
function Inc(var X: Ordinal; Amount: SizeInt = 1): Ordinal;
```
*)

(*
Dec
---
```
function Dec(var X: Ordinal; Amount: SizeInt = 1): Ordinal;
```
*)

(*
Ord
---
```
function Ord(X: Ordinal): Int32;
```
*)

(*
SleepUntil
----------
```
function SleepUntil(Condition: BoolExpr; Interval, Timeout: Int32): Boolean;
```
*)

(*
Default
-------
```
function Default(T: AnyType): AnyType;
```
*)

(*
Sort
----
```
procedure Sort(var A: array);
```
```
procedure Sort(var A: array; Weights: array of Ordinal; LowToHigh: Boolean);
```
```
procedure Sort(var A: array; CompareFunc: function(constref L, R: Anything): Int32);
```
*)

(*
Sorted
------
```
function Sorted(const A: array): array; overload;
```
```
function Sorted(const A: array; CompareFunc: function(constref L, R: Anything): Int32): array;
```
```
function Sorted(const A: array; Weights: array of Ordinal; LowToHigh: Boolean): array;
```
*)

(*
Unique
------
```
function Unique(const A: array): array;
```
*)

(*
Reverse
-------
```
procedure Reverse(var A: array);
```
*)

(*
Reversed
--------
```
function Reversed(const A: array): array;
```
*)

(*
IndexOf
-------
```
function IndexOf(const Item: T; const A: array): Integer;
```
*)

(*
IndicesOf
---------
```
function IndicesOf(const Item: T; const A: array): TIntegerArray;
```
*)

(*
Contains
--------
```
function Contains(const Item: T; const A: array): Boolean;
```
*)

(*
GetCallerAddress
----------------
```
function GetCallerAddress: Pointer;
```
*)

(*
GetCallerName
-------------
```
function GetCallerName: String;
```
*)

(*
GetCallerLocation
-----------------
```
function GetCallerLocation: Pointer;
```
*)

(*
GetCallerLocationStr
--------------------
```
function GetCallerLocationStr: String;
```
*)

(*
GetExceptionLocation
--------------------
```
function GetExceptionLocation: Pointer;
```
*)

(*
GetExceptionLocationStr
-----------------------
```
function GetExceptionLocationStr: String;
```
*)

(*
GetExceptionMessage
-------------------
```
function GetExceptionMessage: String;
```
*)

(*
GetScriptMethodName
-------------------
```
function GetScriptMethodName(Address: Pointer): String;
```
*)

(*
DumpCallStack
-------------
```
function DumpCallStack(Start: Integer = 0): String;
```
*)

(*
Variant.VarType
---------------
```
function Variant.VarType: EVariantVarType;
```

Returns the variants var type.

Example:

```
  if (v.VarType = EVariantVarType.Int32) then
    WriteLn('Variant contains a Int32');
```
*)
procedure _LapeVariantVarType(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariantType(Result)^ := PVariant(Params^[0])^.VarType;
end;

(*
Variant.IsNumeric
-----------------
```
function Variant.IsNumeric: Boolean;
```

Is integer or float?
*)
procedure _LapeVariantIsNumeric(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsNumeric(PVariant(Params^[0])^);
end;

(*
Variant.IsString
----------------
```
function Variant.IsString: Boolean;
```
*)
procedure _LapeVariantIsString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsStr(PVariant(Params^[0])^);
end;

(*
Variant.IsInteger
-----------------
```
function Variant.IsInteger: Boolean;
```
*)
procedure _LapeVariantIsInteger(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsOrdinal(PVariant(Params^[0])^);
end;

(*
Variant.IsFloat
---------------
```
function Variant.IsFloat: Boolean;
```
*)
procedure _LapeVariantIsFloat(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsFloat(PVariant(Params^[0])^);
end;

(*
Variant.IsBoolean
-----------------
```
function Variant.IsBoolean: Boolean;
```
*)
procedure _LapeVariantIsBoolean(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsBool(PVariant(Params^[0])^);
end;

(*
Variant.IsVariant
-----------------
```
function Variant.IsVariant: Boolean;
```

The variant holds another variant!
*)
procedure _LapeVariantIsVariant(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsType(PVariant(Params^[0])^, varVariant);
end;

(*
Variant.IsAssigned
------------------
```
function Variant.IsAssigned: Boolean;
```

Example:

```
  if v.IsAssigned() then
    WriteLn('Variant HAS been assigned to')
  else
    WriteLn('The variant has NOT been assigned to');
```
*)
procedure _LapeVariantIsAssigned(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := not VarIsClear(PVariant(Params^[0])^);
end;

(*
Variant.IsNull
--------------
```
function Variant.IsNull: Boolean;
```
*)
procedure _LapeVariantIsNull(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := VarIsNull(PVariant(Params^[0])^);
end;

(*
Variant.NULL
------------
```
function Variant.NULL: Variant; static;
```

Static method that returns a null variant variable.

Example:

```
v := Variant.NULL;
WriteLn(v.IsNull());
```
*)
procedure _LapeVariantNULL(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := Null;
end;

procedure _LapeBaseClass_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := TSimbaBaseClass(Params^[0]^).Name;
end;

procedure _LapeBaseClass_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaBaseClass(Params^[0]^).Name := PString(Params^[1])^;
end;

procedure _LapeBaseClass_FreeOnTerminate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := TSimbaBaseClass(Params^[0]^).FreeOnTerminate;
end;

procedure _LapeBaseClass_FreeOnTerminate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaBaseClass(Params^[0]^).FreeOnTerminate := PBoolean(Params^[1])^;
end;

procedure _LapeByteArray_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PByteArray(Params^[0])^.ToString();
end;

procedure _LapeByteArray_FromString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PByteArray(Params^[0])^.FromString(PString(Params^[1])^);
end;

procedure ImportBase(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Base';

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

    addGlobalType(getBaseType(DetermineIntType(SizeOf(Byte), False)).createCopy(), 'Byte');
    addGlobalType(getBaseType(DetermineIntType(SizeOf(Integer), True)).createCopy(), 'Integer');

    addGlobalType('Int32', 'TColor');
    addGlobalType('array of TColor', 'TColorArray');

    addGlobalType('array of TStringArray', 'T2DStringArray');
    addGlobalType('array of TIntegerArray', 'T2DIntegerArray');
    addGlobalType('array of Int64', 'TInt64Array');
    addGlobalType('array of Byte', 'TByteArray');
    addGlobalType('array of Variant', 'TVariantArray');
    addGlobalType('array of Pointer', 'TPointerArray');

    addGlobalType('record X, Y: Integer; end', 'TPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('record A, B, C: TPoint; end;', 'TTriangle');
    addGlobalType('array of TTriangle', 'TTriangleArray');

    addGlobalType('record Top, Right, Bottom, Left: TPoint; end;', 'TQuad');
    addGlobalType('array of TQuad', 'TQuadArray');

    addGlobalType('record X1, Y1, X2, Y2: Integer; end', 'TBox');
    addGlobalType('array of TBox', 'TBoxArray');

    addGlobalType('record Width, Height: Integer; end', 'TSize');

    addGlobalType('(__LT__, __GT__, __EQ__, __LE__, __GE__, __NE__)', 'EComparator');

    addGlobalType('enum(Unknown, Unassigned, Null, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Single, Double, DateTime, Currency, Boolean, Variant, AString, UString, WString)', 'EVariantVarType');

    addGlobalFunc('function Variant.VarType: EVariantVarType;', @_LapeVariantVarType);

    addGlobalFunc('function Variant.IsNumeric: Boolean;', @_LapeVariantIsNumeric);
    addGlobalFunc('function Variant.IsInteger: Boolean;', @_LapeVariantIsInteger);
    addGlobalFunc('function Variant.IsFloat: Boolean;', @_LapeVariantIsFloat);
    addGlobalFunc('function Variant.IsString: Boolean;', @_LapeVariantIsString);
    addGlobalFunc('function Variant.IsBoolean: Boolean;', @_LapeVariantIsBoolean);
    addGlobalFunc('function Variant.IsVariant: Boolean;', @_LapeVariantIsVariant);
    addGlobalFunc('function Variant.IsAssigned: Boolean;', @_LapeVariantIsAssigned);
    addGlobalFunc('function Variant.IsNull: Boolean;', @_LapeVariantIsNull);

    addGlobalFunc('function Variant.NULL: Variant; static;', @_LapeVariantNULL);

    addGlobalFunc('function TByteArray.ToString: String;', @_LapeByteArray_ToString);
    addGlobalFunc('procedure TByteArray.FromString(Str: String);', @_LapeByteArray_FromString);

    ImportingSection := '';

    addClass('TBaseClass', 'Pointer');
    addProperty('TBaseClass', 'Name', 'String', @_LapeBaseClass_Name_Read, @_LapeBaseClass_Name_Write);
    addProperty('TBaseClass', 'FreeOnTerminate', 'Boolean', @_LapeBaseClass_FreeOnTerminate_Read, @_LapeBaseClass_FreeOnTerminate_Write);
  end;
end;

end.
