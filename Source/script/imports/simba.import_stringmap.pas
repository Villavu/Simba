unit simba.import_stringmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportStringMap(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes,
  simba.container_stringmap;

procedure _LapeStringMap_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaStringMap(Params^[0])^.ToString();
end;

procedure _LapeStringMap_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaStringMap(Params^[0])^.Count;
end;

procedure _LapeStringMap_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaStringMap(Params^[0])^.Add(PString(Params^[1])^, PVariant(Params^[2])^);
end;

procedure _LapeStringMap_Get(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := PSimbaStringMap(Params^[0])^.Get(PString(Params^[1])^);
end;

procedure _LapeStringMap_GetAll(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariantArray(Result)^ := PSimbaStringMap(Params^[0])^.GetAll(PString(Params^[1])^);
end;

procedure _LapeStringMap_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaStringMap(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringMap_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PSimbaStringMap(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeStringMap_GetValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := PSimbaStringMap(Params^[0])^.Value[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Value[PInteger(Params^[1])^] := PVariant(Params^[2])^;
end;

procedure _LapeStringMap_GetPair(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaStringMap.TPair(Result^) := PSimbaStringMap(Params^[0])^.Pair[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_GetKey(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaStringMap(Params^[0])^.Key[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_SetKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Key[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringMap_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaStringMap(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeStringMap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Clear();
end;

procedure _LapeStringMap_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringMap_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeStringMap_DeleteAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.DeleteAll(PString(Params^[1])^);
end;

procedure _LapeStringMap_Load(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Load(PString(Params^[1])^, EVariantType(Params^[2]^), PString(Params^[3])^);
end;

procedure _LapeStringMap_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Save(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringMap_SetSorted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringMap_SetCaseSens(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.CaseSens := PBoolean(Params^[1])^;
end;

procedure _LapeStringMap_GetSorted(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaStringMap(Params^[0])^.Sorted;
end;

procedure _LapeStringMap_GetCaseSens(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaStringMap(Params^[0])^.CaseSens;
end;

procedure ImportStringMap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('record Key: String; Value: Variant; end;', 'TStringMapPair');

    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FItems: array of record KeyHash: UInt32; Key: String; Value: Variant; end;',
      '  FCount: Integer;',
      '  FSorted: Boolean;',
      '  FCaseSens: Boolean;',
      '  {%CODETOOLS ON}',
      'end;'],
      'TStringMap'
    );
    if (getGlobalType('TStringMap').Size <> SizeOf(TSimbaStringMap)) then
      SimbaException('SizeOf(TStringMap) is wrong!');

    addGlobalFunc('function TStringMap.ToString: String', @_LapeStringMap_ToString);
    addGlobalFunc('function TStringMap.Add(Key: String; Value: Variant): Integer;', @_LapeStringMap_Add);
    addGlobalFunc('function TStringMap.Get(Key: String): Variant;', @_LapeStringMap_Get);
    addGlobalFunc('function TStringMap.GetAll(Key: String): TVariantArray;', @_LapeStringMap_GetAll);
    addGlobalFunc('function TStringMap.IndexOf(Key: String): Integer;', @_LapeStringMap_IndexOf);
    addGlobalFunc('function TStringMap.IndicesOf(Key: String): TIntegerArray;', @_LapeStringMap_IndicesOf);
    addGlobalFunc('procedure TStringMap.Clear', @_LapeStringMap_Clear);
    addGlobalFunc('procedure TStringMap.Delete(Index: Integer); overload', @_LapeStringMap_Delete1);
    addGlobalFunc('procedure TStringMap.Delete(Key: String); overload', @_LapeStringMap_Delete2);
    addGlobalFunc('procedure TStringMap.DeleteAll(Key: String)', @_LapeStringMap_DeleteAll);
    addGlobalFunc('function TStringMap.Exists(Key: String): Boolean;', @_LapeStringMap_Exists);
    addGlobalFunc('procedure TStringMap.Load(FileName: String; VarType: EVariantVarType; Sep: String = "=")', @_LapeStringMap_Load);
    addGlobalFunc('procedure TStringMap.Save(FileName: String; Sep: String = "=");', @_LapeStringMap_Save);

    addProperty('TStringMap', 'Sorted', 'Boolean', @_LapeStringMap_GetSorted, @_LapeStringMap_SetSorted);
    addProperty('TStringMap', 'CaseSens', 'Boolean', @_LapeStringMap_GetCaseSens, @_LapeStringMap_SetCaseSens);
    addProperty('TStringMap', 'Count', 'Integer', @_LapeStringMap_Count);
    addPropertyIndexed('TStringMap', 'Pair', 'Index: Integer', 'TStringMapPair', @_LapeStringMap_GetPair);
    addPropertyIndexed('TStringMap', 'Key', 'Index: Integer', 'String', @_LapeStringMap_GetKey, @_LapeStringMap_SetKey);
    addPropertyIndexed('TStringMap', 'Value', 'Index: Integer', 'Variant', @_LapeStringMap_GetValue, @_LapeStringMap_SetValue);

    addDelayedCode([
      'function ToString(constref Param: TStringMap): String; override;',
      'begin',
      '  Result := Param.ToString();',
      'end;'
    ]);

    // Image map, just wrap above
    addDelayedCode([
      'type',
      '  TImageMap = record',
      '    {%CODETOOLS OFF}',
      '    StringMap: TStringMap;',
      '    {%CODETOOLS ON}',
      '  end;',
      '',
      'property TImageMap.Count: Integer;',
      'begin',
      '  Result := Self.StringMap.Count;',
      'end;',
      '',
      'function TImageMap.Add(Key: String; Value: TImage): Integer;',
      'begin',
      '  Result := Self.StringMap.Add(Key, PtrUInt(Value));',
      'end;',
      '',
      'function TImageMap.IndexOf(Key: String): Integer;',
      'begin',
      '  Result := Self.StringMap.IndexOf(Key);',
      'end;',
      '',
      'function TImageMap.IndicesOf(Key: String): TIntegerArray;',
      'begin',
      '  Result := Self.StringMap.IndicesOf(Key);',
      'end;',
      '',
      'procedure TImageMap.Clear;',
      'begin',
      '  Self.StringMap.Clear();',
      'end;',
      '',
      'procedure TImageMap.Delete(Index: Integer); overload;',
      'begin',
      '  Self.StringMap.Delete(Index);',
      'end;',
      '',
      'procedure TImageMap.Delete(Key: String); overload;',
      'begin',
      '  Self.StringMap.Delete(Key);',
      'end;',
      '',
      'procedure TImageMap.DeleteAll(Key: String);',
      'begin',
      '  Self.StringMap.DeleteAll(Key);',
      'end;',
      '',
      'function TImageMap.Exists(Key: String): Boolean;',
      'begin',
      '  Result := Self.StringMap.Exists(Key);',
      'end;',
      '',
      'function TImageMap.Get(Key: String): TImage;',
      'begin',
      '  Result := TImage(PtrUInt(Self.StringMap.Get(Key)));',
      'end;',
      '',
      'function TImageMap.GetAll(Key: String): TImageArray;',
      'var',
      '  Arr: TVariantArray := Self.StringMap.GetAll(Key);',
      '  I: Integer;',
      'begin',
      '  SetLength(Result, Length(Arr));',
      '  for I := 0 to High(Arr) do',
      '    Result[I] := TImage(PtrUInt(Arr[I]));',
      'end;',
      '',
      'property TImageMap.Sorted: Boolean;',
      'begin',
      '  Result := Self.StringMap.Sorted;',
      'end;',
      '',
      'property TImageMap.Sorted(Value: Boolean);',
      'begin',
      '  Self.StringMap.Sorted := Value;',
      'end;',
      '',
      'property TImageMap.CaseSens: Boolean;',
      'begin',
      '  Result := Self.StringMap.CaseSens;',
      'end;',
      '',
      'property TImageMap.CaseSens(Value: Boolean);',
      'begin',
      '  Self.StringMap.CaseSens := Value;',
      'end;',
      '',
      'property TImageMap.Key(Index: Integer): String;',
      'begin',
      '  Result := Self.StringMap.Key[Index];',
      'end;',
      '',
      'property TImageMap.Key(Index: Integer; Value: String);',
      'begin',
      '  Self.StringMap.Key[Index] := Value;',
      'end;',
      '',
      'property TImageMap.Value(Index: Integer): TImage;',
      'begin',
      '  Result := TImage(PtrUInt(Self.StringMap.Value[Index]));',
      'end;',
      '',
      'property TImageMap.Value(Index: Integer; NewValue: TImage);',
      'begin',
      '  Self.StringMap.Value[Index] := PtrUInt(NewValue);',
      'end;',
      '',
      'function ToString(constref Param: TImageMap): String; override;',
      'begin',
      '  Result := Param.StringMap.ToString();',
      'end;'
    ], 'ImageMap');
  end;
end;

end.

