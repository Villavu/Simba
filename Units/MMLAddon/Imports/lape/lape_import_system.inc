'{$DEFINE Lape}' + LineEnding +
'{$DEFINE Sesquipedalian}' + LineEnding +
'{$DEFINE Lape_CaseInsensitive}' + LineEnding +
'{$DEFINE Lape_PascalLabels}' + LineEnding +
'{$DEFINE Lape_Ansi}' + LineEnding +
'type UInt8 = UInt8;' + LineEnding +
'type Int8 = Int8;' + LineEnding +
'type UInt16 = UInt16;' + LineEnding +
'type Int16 = Int16;' + LineEnding +
'type UInt32 = UInt32;' + LineEnding +
'type Int32 = Int32;' + LineEnding +
'type UInt64 = UInt64;' + LineEnding +
'type Int64 = Int64;' + LineEnding +
(* Breaks parser
'type Currency = Currency;' + LineEnding +
'type Single = Single;' + LineEnding +
'type Double = Double;' + LineEnding +
'type Extended = Extended;' + LineEnding +
'type Boolean = Boolean;' + LineEnding +
'type ByteBool = ByteBool;' + LineEnding +
'type WordBool = WordBool;' + LineEnding +
'type LongBool = LongBool;' + LineEnding +
'type AnsiChar = AnsiChar;' + LineEnding +
'type WideChar = WideChar;' + LineEnding +
'type Pointer = Pointer;' + LineEnding +
'type ConstPointer = ConstPointer;' + LineEnding +
'type AnsiString = AnsiString;' + LineEnding +
'type ShortString = ShortString;' + LineEnding +
'type UnicodeString = UnicodeString;' + LineEnding +
'type String = AnsiString;' + LineEnding +
'type Char = Char;' + LineEnding +
'type EvalBool = LongBool;' + LineEnding +
*)
'type TMethod = packed record Method, Self: Pointer; end;' + LineEnding +
{$IFDEF CPU64}
'type SizeInt = Int64;' + LineEnding +
'type SizeUInt = UInt64;' + LineEnding +
'type NativeInt = Int64;' + LineEnding +
'type NativeUInt = UInt64;' + LineEnding +
'type PtrInt = Int64;' + LineEnding +
'type PtrUInt = UInt64;' + LineEnding +
{$ELSE}
'type SizeInt = Int32;' + LineEnding +
'type SizeUInt = UInt32;' + LineEnding +
'type NativeInt = Int32;' + LineEnding +
'type NativeUInt = UInt32;' + LineEnding +
'type PtrInt = Int32;' + LineEnding +
'type PtrUInt = UInt32;' + LineEnding +
{$ENDIF}
(* Breaks parser
'type Byte = UInt8;' + LineEnding +
'type ShortInt = Int8;' + LineEnding +
'type Word = UInt16;' + LineEnding +
'type SmallInt = Int16;' + LineEnding +
'type LongWord = UInt32;' + LineEnding +
'type LongInt = Int32;' + LineEnding +
'type Cardinal = UInt32;' + LineEnding +
'type PChar = PChar;' + LineEnding +
*)
'var True: Boolean;' + LineEnding +
'var False: Boolean;' + LineEnding +
(* Breaks parser
'var nil: Pointer;' + LineEnding +
*)
'procedure Delete(A: array of Anything; Index: Int32; Count: Int32 = Length(A)); begin end;' + LineEnding +
'procedure Insert(Item: Anything; A: array of Anything; Index: Int32); begin end;' + LineEnding +
'procedure Copy(A: array of Anything; Index: Int32 = 0; Count: Int32 = Length(A)); overload; begin end;' + LineEnding +
'procedure Copy(S: String; Index: Int32 = 1; Count: Int32 = Length(S)); overload; begin end;' + LineEnding +
'procedure SetLength(A: Array of Anything; Length: Int32); overload; begin end;' + LineEnding +
'procedure SetLength(S: String; Length: Int32); overload; begin end;' + LineEnding +
'function Length(A: array of Anything): Int32; overload; begin end;' + LineEnding +
'function Length(S: String): Int32; overload; begin end;' + LineEnding +
'procedure WriteLn(Args: Anything); begin end;' + LineEnding +
'procedure Write(Args: Anything); begin end;' + LineEnding +
'procedure Swap(var A, B: Anything); begin end;' + LineEnding +
'function SizeOf(A: Anything): Int32; begin end;' + LineEnding +
'function ToString(A: Anything): String; begin end;' + LineEnding +
'function GetMem(i: SizeInt): Pointer; begin end;' + LineEnding +
'function AllocMem(i: SizeInt): Pointer; begin end;' + LineEnding +
'procedure FreeMem(p: Pointer); begin end;' + LineEnding +
'procedure ReallocMem(var p: Pointer; s: SizeInt); begin end;' + LineEnding +
'procedure FillMem(var p; s: SizeInt; b: UInt8 = 0); begin end;' + LineEnding +
'procedure MemMove(constref Src; var Dst; s: SizeInt); begin end;' + LineEnding +
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
'function GetExceptionMessage: ShortString; begin end;';

