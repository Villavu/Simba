'type UInt8 = UInt8;' + LineEnding +
'type Int8 = Int8;' + LineEnding +
'type UInt16 = UInt16;' + LineEnding +
'type Int16 = Int16;' + LineEnding +
'type UInt32 = UInt32;' + LineEnding +
'type Int32 = Int32;' + LineEnding +
'type UInt64 = UInt64;' + LineEnding +
'type Int64 = Int64;' + LineEnding +
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
'type Byte = UInt8;' + LineEnding +
'type ShortInt = Int8;' + LineEnding +
'type Word = UInt16;' + LineEnding +
'type SmallInt = Int16;' + LineEnding +
'type LongWord = UInt32;' + LineEnding +
'type LongInt = Int32;' + LineEnding +
'type Cardinal = UInt32;' + LineEnding +
'type PChar = PChar;' + LineEnding +
'type TMethod = packed record Method, Self: Pointer; end;' + LineEnding +
'var True: Boolean;' + LineEnding +
'var False: Boolean;';
