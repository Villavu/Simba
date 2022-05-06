{$scopedenums on}

procedure _LapeVarType(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
type
  PVarType = ^EVarType;
  EVarType = (
    VarUnknown,
    VarUnassigned,
    VarNull,
    VarInt8,
    VarInt16,
    VarInt32,
    VarInt64,
    VarUInt8,
    VarUInt16,
    VarUInt32,
    VarUInt64,
    VarSingle,
    VarDouble,
    VarDate,
    VarCurrency,
    VarBoolean,
    VarVariant,
    VarString,
    VarUnicodeString
  );

  function GetVarType(const V: Variant): EVarType;
  begin
    case TVarData(V).VType of
      varEmpty:         Result := EVarType.VarUnassigned;
      varNull:          Result := EVarType.VarNull;
      varShortInt:      Result := EVarType.VarInt8;
      varSmallInt:      Result := EVarType.VarInt16;
      varInteger:       Result := EVarType.VarInt32;
      VarInt64:         Result := EVarType.VarInt64;
      varByte:          Result := EVarType.VarUInt8;
      varWord:          Result := EVarType.VarUInt16;
      varLongWord:      Result := EVarType.VarUInt32;
      VarUInt64:        Result := EVarType.VarUInt64;
      VarSingle:        Result := EVarType.VarSingle;
      VarDouble:        Result := EVarType.VarDouble;
      VarDate:          Result := EVarType.VarDate;
      VarCurrency:      Result := EVarType.VarCurrency;
      VarBoolean:       Result := EVarType.VarBoolean;
      VarVariant:       Result := EVarType.VarVariant;
      VarString:        Result := EVarType.VarString;
      varOleStr:        Result := EVarType.VarUnicodeString;
      else
        Result := EVarType.VarUnknown;
    end;
  end;

begin
  PVarType(Result)^ := GetVarType(PVariant(Params^[0])^);
end;

{$scopedenums off}

