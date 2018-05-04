'var Null: Variant;' + LineEnding +
'var Unassigned: Variant;' + LineEnding +
'type HRESULT = Int32;' + LineEnding +
'type TVarType = UInt16;' + LineEnding +
'type TVariantRelationship = (vrEqual, vrLessThan, vrGreaterThan, vrNotEqual);' + LineEnding +
'var VarEmpty: Int32 = 0;' + LineEnding +
'var VarNull: Int32 = 1;' + LineEnding +
'var VarSmallInt: Int32 = 2;' + LineEnding +
'var VarInteger: Int32 = 3;' + LineEnding +
'var VarSingle: Int32 = 4;' + LineEnding +
'var VarDouble: Int32 = 5;' + LineEnding +
'var VarDate: Int32 = 7;' + LineEnding +
'var VarCurrency: Int32 = 6;' + LineEnding +
'var VarOleStr: Int32 = 8;' + LineEnding +
'var VarDispatch: Int32 = 9;' + LineEnding +
'var VarError: Int32 = 10;' + LineEnding +
'var VarBoolean: Int32 = 11;' + LineEnding +
'var VarVariant: Int32 = 12;' + LineEnding +
'var VarUnknown: Int32 = 13;' + LineEnding +
'var VarShortInt: Int32 = 16;' + LineEnding +
'var VarByte: Int32 = 17;' + LineEnding +
'var VarWord: Int32 = 18;' + LineEnding +
'var VarLongWord: Int32 = 19;' + LineEnding +
'var VarInt64: Int32 = 20;' + LineEnding +
'var VarStrArg: Int32 = 72;' + LineEnding +
'var VarString: Int32 = 256;' + LineEnding +
'var VarAny: Int32 = 257;' + LineEnding +
'var VarUString: Int32 = 258;' + LineEnding +
'var VarUInt64: Int32 = 21;' + LineEnding +
'var VarTypeMask: Int32 = 4095;' + LineEnding +
'var VarArray: Int32 = 8192;' + LineEnding +
'var VarByRef: Int32 = 16384;' + LineEnding +
'function VarType(const V: Variant): TVarType; begin end;' + LineEnding +
'function VarAsType(const V: Variant; aVarType: TVarType): Variant; begin end;' + LineEnding +
'function VarIsByRef(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsEmpty(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsNull(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsClear(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsError(const V: Variant; out AResult: HRESULT): EvalBool; begin end;' + LineEnding +
'function VarAsError(AResult: HRESULT): Variant; begin end;' + LineEnding +
'function VarIsCustom(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsOrdinal(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsFloat(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsNumeric(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsStr(const V: Variant): EvalBool; begin end;' + LineEnding +
'function VarIsArray(const A: Variant; AResolveByRef: EvalBool = True): EvalBool; begin end;' + LineEnding +
'function VarToStr(const V: Variant): string; begin end;' + LineEnding +
'function VarToStrDef(const V: Variant; ADefault: string): string; begin end;' + LineEnding +
'function VarToWideStr(const V: Variant): WideString; begin end;' + LineEnding +
'function VarToWideStrDef(const V: Variant; ADefault: WideString): WideString; begin end;' + LineEnding +
'function VarToUnicodeStr(const V: Variant): UnicodeString; begin end;' + LineEnding +
'function VarToUnicodeStrDef(const V: Variant; ADefault: UnicodeString): UnicodeString; begin end;' + LineEnding +
'function VarToDateTime(const V: Variant): TDateTime; begin end;' + LineEnding +
'function VarFromDateTime(DateTime: TDateTime): Variant; begin end;' + LineEnding +
'function VarInRange(const AValue, AMin, AMax: Variant): EvalBool; begin end;' + LineEnding +
'function VarEnsureRange(const AValue, AMin, AMax: Variant): Variant; begin end;' + LineEnding +
'function VarSameValue(const A, B: Variant): EvalBool; begin end;' + LineEnding +
'function VarCompareValue(const A, B: Variant): TVariantRelationship; begin end;' + LineEnding +
'function VarTypeIsValidArrayType(aVarType: TVarType): EvalBool; begin end;' + LineEnding +
'function VarTypeIsValidElementType(aVarType: TVarType): EvalBool; begin end;' + LineEnding +
'function VarArrayCreate(Bounds: array of SizeInt; aVarType: TVarType): Variant; begin end;' + LineEnding +
'function VarArrayOf(Values: array of Variant): Variant; begin end;' + LineEnding +
'procedure VarArrayRedim(var A: Variant; HighBound: SizeInt); begin end;' + LineEnding +
'function VarArrayAsPSafeArray(const A: Variant): Pointer; begin end;' + LineEnding +
'procedure VarCopyNoInd(var Dest: Variant; const Source: Variant); begin end;' + LineEnding +
'function VarArrayDimCount(const A: Variant): SizeInt; begin end;' + LineEnding +
'function VarArrayLowBound(const A: Variant; Dim: SizeInt): SizeInt; begin end;' + LineEnding +
'function VarArrayHighBound(const A: Variant; Dim: SizeInt): SizeInt; begin end;' + LineEnding +
'function VarArrayLock(const A: Variant): Pointer; begin end;' + LineEnding +
'procedure VarArrayUnlock(const A: Variant); begin end;' + LineEnding +
'function VarArrayRef(const A: Variant): Variant; begin end;' + LineEnding +
'function VarArrayGet(const A: Variant; Indices: array of Int32): Variant; begin end;' + LineEnding +
'procedure VarArraySet(var A: Variant; const Value: Variant; Indices: array of Int32); begin end;' + LineEnding;
