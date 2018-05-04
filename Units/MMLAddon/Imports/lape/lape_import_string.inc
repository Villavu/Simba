'type TTextLineBreakStyle = (tlbsLF, tlbsCRLF, tlbsCR);' + LineEnding +
'type TReplaceFlag = (rfReplaceAll, rfIgnoreCase);' + LineEnding +
'type TReplaceFlags = set of TReplaceFlag;' + LineEnding +
'function UpperCase(s: string): string; begin end;' + LineEnding +
'function LowerCase(s: string): string; begin end;' + LineEnding +
'function UpCase(c: AnsiChar): AnsiChar; overload; begin end;' + LineEnding +
'function UpCase(c: WideChar): WideChar; overload; begin end;' + LineEnding +
'function CompareStr(s1, s2: string): Int32; begin end;' + LineEnding +
'function CompareText(s1, s2: string): Int32; begin end;' + LineEnding +
'function SameText(s1, s2: string): EvalBool; begin end;' + LineEnding +
'function AnsiUpperCase(s: string): string; begin end;' + LineEnding +
'function AnsiLowerCase(s: string): string; begin end;' + LineEnding +
'function AnsiCompareStr(s1, s2: string): Int32; begin end;' + LineEnding +
'function AnsiCompareText(s1, s2: string): Int32; begin end;' + LineEnding +
'function AnsiSameText(s1,s2: string): EvalBool; begin end;' + LineEnding +
'function AnsiSameStr(s1,s2: string): EvalBool; begin end;' + LineEnding +
'function Trim(s: string): string; begin end;' + LineEnding +
'function TrimLeft(s: string): string; begin end;' + LineEnding +
'function TrimRight(s: string): string; begin end;' + LineEnding +
'function PadL(s: string; Len: SizeInt; c: Char = ' +  #32 + '): string; begin end;' + LineEnding +
'function PadR(s: string; Len: SizeInt; c: Char = ' +  #32 + '): string; begin end;' + LineEnding +
'function QuotedStr(s: string): string; begin end;' + LineEnding +
'function AnsiQuotedStr(s: string; Quote: Char): string; begin end;' + LineEnding +
'function AnsiDequotedStr(s: string; AQuote: Char): string; begin end;' + LineEnding +
'function WrapText(Line, BreakStr: string; BreakChars: set of AnsiChar; MaxCol: Int32): string; begin end;' + LineEnding +
'function AdjustLineBreaks(s: string; Style: TTextLineBreakStyle): string; begin end;' + LineEnding +
'function IntToHex(Value: Int64; Digits: Int32 = 1): string; overload; begin end;' + LineEnding +
'function IntToHex(Value: UInt64; Digits: Int32 = 1): string; overload; begin end;' + LineEnding +
'function IntToStr(i: Int64): string; overload; begin end;' + LineEnding +
'function IntToStr(i: UInt64): string; overload; begin end;' + LineEnding +
'function StrToInt(s: string): Int32; begin end;' + LineEnding +
'function StrToIntDef(s: string; Def: Int32): Int32; begin end;' + LineEnding +
'function StrToInt64(s: string): Int64; begin end;' + LineEnding +
'function StrToInt64Def(s: string; Def: Int64): Int64; begin end;' + LineEnding +
'function StrToUInt64(s: string): UInt64; begin end;' + LineEnding +
'function StrToUInt64Def(s: string; Def: UInt64): UInt64; begin end;' + LineEnding +
'function FloatToStr(f: Extended): string; begin end;' + LineEnding +
'function StrToFloat(s: string): Extended; begin end;' + LineEnding +
'function StrToFloatDef(s: string; Def: Extended): Extended; begin end;' + LineEnding +
'function CurrToStr(Value: Currency): string; begin end;' + LineEnding +
'function StrToCurr(s: string): Currency; begin end;' + LineEnding +
'function StrToCurrDef(s: string; Def: Currency): Currency; begin end;' + LineEnding +
'function StrToBool(s: string): EvalBool; begin end;' + LineEnding +
'function BoolToStr(B: EvalBool; TrueStr: string = ' + #39 + 'True' + #39 + '; FalseStr: string = ' + #39 + 'False' + #39 +'): string; begin end;' + LineEnding +
'function StrToBoolDef(s: string; Default: EvalBool): EvalBool; begin end;' + LineEnding +
'function Format(Fmt: string; Args: array of Variant): string; begin end;' + LineEnding +
'function FormatFloat(Format: string; Value: Extended): string; begin end;' + LineEnding +
'function FormatCurr(Format: string; Value: Currency): string; begin end;' + LineEnding +
'function LastDelimiter(Delimiters, s: string): SizeInt; begin end;' + LineEnding +
'function StringReplace(S, OldPattern, NewPattern: string; Flags: TReplaceFlags): string; begin end;' + LineEnding +
'Function IsDelimiter(Delimiters, s: string; Index: SizeInt): EvalBool; begin end;' + LineEnding +
'function Pos(Substr: string; Source: string): SizeInt; begin end;' + LineEnding +
'function StringOfChar(c: Char; l: SizeInt): string; begin end;' + LineEnding;
