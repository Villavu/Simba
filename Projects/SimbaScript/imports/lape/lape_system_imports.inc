'{$DEFINE Lape}' + LineEnding +
'{$DEFINE Sesquipedalian}' + LineEnding +
'procedure Delete(A: array of Anything; Index: Int32; Count: Int32 = Length(A)); begin end;' + LineEnding +
'procedure Insert(Item: Anything; A: array of Anything; Index: Int32); begin end;' + LineEnding +
'procedure Copy(A: array of Anything; Index: Int32 = 0; Count: Int32 = Length(A)); overload; begin end;' + LineEnding +
'procedure Copy(S: String; Index: Int32 = 1; Count: Int32 = Length(S)); overload; begin end;' + LineEnding +
'procedure SetLength(A: Array of Anything; Length: Int32); overload; begin end;' + LineEnding +
'procedure SetLength(S: String; Length: Int32); overload; begin end;' + LineEnding +
'function Low(A: array of Anything): Int32; begin end;' + LineEnding +
'function Low(A: String): Int32; begin end;' + LineEnding +
'function High(A: array of Anything): Int32; begin end;' + LineEnding +
'function High(A: String): Int32; begin end;' + LineEnding +
'function Length(A: array of Anything): Int32; overload; begin end;' + LineEnding +
'function Length(S: String): Int32; overload; begin end;' + LineEnding +
'procedure WriteLn(Args: Anything); begin end;' + LineEnding +
'procedure Write(Args: Anything); begin end;' + LineEnding +
'procedure Swap(var A, B: Anything); begin end;' + LineEnding +
'function SizeOf(A: Anything): Int32; begin end;' + LineEnding +
'function ToString(A: Anything): String; begin end;' + LineEnding +
'function ToStr(A: Anything): String; begin end;' + LineEnding +
'function GetMem(i: SizeInt): Pointer; begin end;' + LineEnding +
'function AllocMem(i: SizeInt): Pointer; begin end;' + LineEnding +
'procedure FreeMem(p: Pointer); begin end;' + LineEnding +
'procedure ReallocMem(var p: Pointer; s: SizeInt); begin end;' + LineEnding +
'procedure FillMem(var p; s: SizeInt; b: UInt8 = 0); begin end;' + LineEnding +
'function CompareMem(constref p1, p2; Length: SizeInt): EvalBool; begin end;' + LineEnding +
'function Assigned(constref p): EvalBool; begin end;' + LineEnding +
'procedure RaiseException(Ex: string); overload; begin end;' + LineEnding +
'procedure UniqueString(var Str: AnsiString); overload; begin end;' + LineEnding +
'procedure UniqueString(var Str: WideString); overload; begin end;' + LineEnding +
'procedure UniqueString(var Str: UnicodeString); overload; begin end;' + LineEnding +
'function Random(min, max: Int64): Int64; overload; begin end;' + LineEnding +
'function Random(min, max: Extended): Extended; overload; begin end;' + LineEnding +
'function Random(l: Int64): Int64; overload; begin end;' + LineEnding +
'function Random: Extended; overload; begin end;' + LineEnding +
'procedure Randomize; begin end;' + LineEnding +
'var RandSeed: Int32;' + LineEnding +
'procedure Sleep(Milliseconds: UInt32); begin end;' + LineEnding +
'function GetExceptionMessage: ShortString; begin end;' + LineEnding +
'function Inc(var X: Ordinal): Ordinal; overload; begin end;' + LineEnding +
'function Dec(var X: Ordinal): Ordinal; overload; begin end;' + LineEnding +
'function Inc(var X: Ordinal; Amount: SizeInt): Ordinal; overload; begin end;' + LineEnding +
'function Dec(var X: Ordinal; Amount: SizeInt): Ordinal; overload; begin end;' + LineEnding +
'function Ord(X: Ordinal): Int32; begin end;' + LineEnding +
'function WaitUntil(Condition: Expression; Interval, Timeout: Int32): Boolean; begin end;';

