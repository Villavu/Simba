unit simba.import_stringmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportStringMap(Compiler: TSimbaScript_Compiler);
procedure ImportStringPointerMap(Compiler: TSimbaScript_Compiler);
procedure ImportStringIntMap(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes,
  simba.stringmap;

procedure _LapeStringMap_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringMap(Params^[0])^.ToString();
end;

procedure _LapeStringMap_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringMap(Params^[0])^.Count;
end;

procedure _LapeStringMap_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringMap(Params^[0])^.Add(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringMap_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringMap(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringMap_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PStringMap(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeStringMap_GetValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringMap(Params^[0])^.Value[PString(Params^[1])^];
end;

procedure _LapeStringMap_GetValues(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PStringMap(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStringMap_GetPair(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TStringMap.PPair(Result)^ := PStringMap(Params^[0])^.Pair[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_GetKey(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringMap(Params^[0])^.Key[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_SetKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Key[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringMap_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Value[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringMap_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringMap(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeStringMap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Clear();
end;

procedure _LapeStringMap_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringMap_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeStringMap_DeleteAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.DeleteAll(PString(Params^[1])^);
end;

procedure _LapeStringMap_Load(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Load(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringMap_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Save(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringMap_SetSorted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringMap_SetCaseSens(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringMap(Params^[0])^.CaseSens := PBoolean(Params^[1])^;
end;

procedure _LapeStringMap_GetSorted(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringMap(Params^[0])^.Sorted;
end;

procedure _LapeStringMap_GetCaseSens(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringMap(Params^[0])^.CaseSens;
end;

procedure ImportStringMap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('record Key, Value: String; end;', 'TStringPair');

    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FItems: array of record KeyHash: UInt32; Key, Value: String; end;',
      '  FCount: Integer;',
      '  FSorted: Boolean;',
      '  FCaseSens: Boolean;',
      '  {%CODETOOLS ON}',
      '  InvalidVal: String;',
      'end;'],
      'TStringMap'
    );
    if (getGlobalType('TStringMap').Size <> SizeOf(TStringMap)) then
      SimbaException('SizeOf(TStringMap) is wrong!');

    addGlobalFunc('function ToString(constref Param: TStringMap): String; override;', @_LapeStringMap_ToString);

    addGlobalFunc('function TStringMap.Count: Integer', @_LapeStringMap_Count);
    addGlobalFunc('function TStringMap.Add(Key, Value: String): Integer;', @_LapeStringMap_Add);
    addGlobalFunc('function TStringMap.IndexOf(Key: String): Integer;', @_LapeStringMap_IndexOf);
    addGlobalFunc('function TStringMap.IndicesOf(Key: String): TIntegerArray;', @_LapeStringMap_IndicesOf);
    addGlobalFunc('procedure TStringMap.Clear', @_LapeStringMap_Clear);
    addGlobalFunc('procedure TStringMap.Delete(Index: Integer); overload', @_LapeStringMap_Delete1);
    addGlobalFunc('procedure TStringMap.Delete(Key: String); overload', @_LapeStringMap_Delete2);
    addGlobalFunc('procedure TStringMap.DeleteAll(Key: String)', @_LapeStringMap_DeleteAll);
    addGlobalFunc('function TStringMap.Exists(Key: String): Boolean;', @_LapeStringMap_Exists);
    addGlobalFunc('procedure TStringMap.Load(FileName: String; Sep: String = "=")', @_LapeStringMap_Load);
    addGlobalFunc('procedure TStringMap.Save(FileName: String; Sep: String = "=");', @_LapeStringMap_Save);

    addGlobalFunc('function TStringMap.GetValues(Key: String): TStringArray', @_LapeStringMap_GetValues);
    addGlobalFunc('function TStringMap.GetValue(Key: String): String', @_LapeStringMap_GetValue);
    addGlobalFunc('procedure TStringMap.SetValue(Key: String; Value: String)', @_LapeStringMap_SetValue);

    addClassVar('TStringMap', 'Pair', 'TStringPair', @_LapeStringMap_GetPair, nil, True);
    addClassVar('TStringMap', 'Key', 'String', @_LapeStringMap_GetKey, @_LapeStringMap_SetKey, True);
    addClassVar('TStringMap', 'Sorted', 'Boolean', @_LapeStringMap_GetSorted, @_LapeStringMap_SetSorted);
    addClassVar('TStringMap', 'CaseSens', 'Boolean', @_LapeStringMap_GetCaseSens, @_LapeStringMap_SetCaseSens);
  end;
end;

procedure _LapeStringPointerMap_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringPointerMap(Params^[0])^.ToString();
end;

procedure _LapeStringPointerMap_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringPointerMap(Params^[0])^.Count;
end;

procedure _LapeStringPointerMap_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringPointerMap(Params^[0])^.Add(PString(Params^[1])^, PPointer(Params^[2])^);
end;

procedure _LapeStringPointerMap_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringPointerMap(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringPointerMap_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PStringPointerMap(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeStringPointerMap_GetValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PStringPointerMap(Params^[0])^.Value[PString(Params^[1])^];
end;

procedure _LapeStringPointerMap_GetValues(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
type
  TPointerArray = array of Pointer;
  PPointerArray = ^TPointerArray;
begin
  PPointerArray(Result)^ := PStringPointerMap(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStringPointerMap_GetPair(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TStringPointerMap.PPair(Result)^ := PStringPointerMap(Params^[0])^.Pair[PInteger(Params^[1])^];
end;

procedure _LapeStringPointerMap_GetKey(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringPointerMap(Params^[0])^.Key[PInteger(Params^[1])^];
end;

procedure _LapeStringPointerMap_SetKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Key[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringPointerMap_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Value[PString(Params^[1])^] := PPointer(Params^[2])^;
end;

procedure _LapeStringPointerMap_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringPointerMap(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeStringPointerMap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Clear();
end;

procedure _LapeStringPointerMap_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringPointerMap_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeStringPointerMap_DeleteAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.DeleteAll(PString(Params^[1])^);
end;

procedure _LapeStringPointerMap_Load(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Load(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringPointerMap_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Save(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringPointerMap_SetSorted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringPointerMap_SetCaseSens(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPointerMap(Params^[0])^.CaseSens := PBoolean(Params^[1])^;
end;

procedure _LapeStringPointerMap_GetSorted(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringPointerMap(Params^[0])^.Sorted;
end;

procedure _LapeStringPointerMap_GetCaseSens(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringPointerMap(Params^[0])^.CaseSens;
end;

procedure ImportStringPointerMap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('record Key: String; Value: Pointer; end;', 'TStringPointerPair');

    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FItems: array of record KeyHash: UInt32; Key: String; Value: Pointer; end;',
      '  FCount: Integer;',
      '  FSorted: Boolean;',
      '  FCaseSens: Boolean;',
      '  {%CODETOOLS ON}',
      '  InvalidVal: Pointer;',
      'end;'],
      'TStringPointerMap'
    );
    if (getGlobalType('TStringPointerMap').Size <> SizeOf(TStringPointerMap)) then
      SimbaException('SizeOf(TStringPointerMap) is wrong!');

    addGlobalFunc('function ToString(constref Param: TStringPointerMap): String; override;', @_LapeStringPointerMap_ToString);

    addGlobalFunc('function TStringPointerMap.Count: Integer', @_LapeStringPointerMap_Count);
    addGlobalFunc('function TStringPointerMap.Add(Key: String; Value: Pointer): Integer;', @_LapeStringPointerMap_Add);
    addGlobalFunc('function TStringPointerMap.IndexOf(Key: String): Integer;', @_LapeStringPointerMap_IndexOf);
    addGlobalFunc('function TStringPointerMap.IndicesOf(Key: String): TIntegerArray;', @_LapeStringPointerMap_IndicesOf);
    addGlobalFunc('procedure TStringPointerMap.Clear', @_LapeStringPointerMap_Clear);
    addGlobalFunc('procedure TStringPointerMap.Delete(Index: Integer); overload', @_LapeStringPointerMap_Delete1);
    addGlobalFunc('procedure TStringPointerMap.Delete(Key: String); overload', @_LapeStringPointerMap_Delete2);
    addGlobalFunc('procedure TStringPointerMap.DeleteAll(Key: String)', @_LapeStringPointerMap_DeleteAll);
    addGlobalFunc('function TStringPointerMap.Exists(Key: String): Boolean;', @_LapeStringPointerMap_Exists);
    addGlobalFunc('procedure TStringPointerMap.Load(FileName: String; Sep: String = "=")', @_LapeStringPointerMap_Load);
    addGlobalFunc('procedure TStringPointerMap.Save(FileName: String; Sep: String = "=");', @_LapeStringPointerMap_Save);

    addGlobalFunc('function TStringPointerMap.GetValues(Key: String): array of Pointer', @_LapeStringPointerMap_GetValues);
    addGlobalFunc('function TStringPointerMap.GetValue(Key: String): Pointer', @_LapeStringPointerMap_GetValue);
    addGlobalFunc('procedure TStringPointerMap.SetValue(Key: String; Value: Pointer)', @_LapeStringPointerMap_SetValue);

    addClassVar('TStringPointerMap', 'Pair', 'TStringPointerPair', @_LapeStringPointerMap_GetPair, nil, True);
    addClassVar('TStringPointerMap', 'Key', 'String', @_LapeStringPointerMap_GetKey, @_LapeStringPointerMap_SetKey, True);
    addClassVar('TStringPointerMap', 'Sorted', 'Boolean', @_LapeStringPointerMap_GetSorted, @_LapeStringPointerMap_SetSorted);
    addClassVar('TStringPointerMap', 'CaseSens', 'Boolean', @_LapeStringPointerMap_GetCaseSens, @_LapeStringPointerMap_SetCaseSens);
  end;
end;

procedure _LapeStringIntMap_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringIntMap(Params^[0])^.ToString();
end;

procedure _LapeStringIntMap_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringIntMap(Params^[0])^.Count;
end;

procedure _LapeStringIntMap_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringIntMap(Params^[0])^.Add(PString(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeStringIntMap_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringIntMap(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringIntMap_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PStringIntMap(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeStringIntMap_GetValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PStringIntMap(Params^[0])^.Value[PString(Params^[1])^];
end;

procedure _LapeStringIntMap_GetValues(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64Array(Result)^ := PStringIntMap(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStringIntMap_GetPair(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TStringIntMap.PPair(Result)^ := PStringIntMap(Params^[0])^.Pair[PInteger(Params^[1])^];
end;

procedure _LapeStringIntMap_GetKey(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringIntMap(Params^[0])^.Key[PInteger(Params^[1])^];
end;

procedure _LapeStringIntMap_SetKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Key[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringIntMap_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Value[PString(Params^[1])^] := PInt64(Params^[2])^;
end;

procedure _LapeStringIntMap_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringIntMap(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeStringIntMap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Clear();
end;

procedure _LapeStringIntMap_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringIntMap_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeStringIntMap_DeleteAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.DeleteAll(PString(Params^[1])^);
end;

procedure _LapeStringIntMap_Load(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Load(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringIntMap_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Save(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringIntMap_SetSorted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringIntMap_SetCaseSens(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringIntMap(Params^[0])^.CaseSens := PBoolean(Params^[1])^;
end;

procedure _LapeStringIntMap_GetSorted(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringIntMap(Params^[0])^.Sorted;
end;

procedure _LapeStringIntMap_GetCaseSens(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringIntMap(Params^[0])^.CaseSens;
end;

procedure ImportStringIntMap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('record Key: String; Value: Int64; end;', 'TStringIntPair');

    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FItems: array of record KeyHash: UInt32; Key: String; Value: Int64; end;',
      '  FCount: Integer;',
      '  FSorted: Boolean;',
      '  FCaseSens: Boolean;',
      '  {%CODETOOLS ON}',
      '  InvalidVal: Int64;',
      'end;'],
      'TStringIntMap'
    );
    if (getGlobalType('TStringIntMap').Size <> SizeOf(TStringIntMap)) then
      SimbaException('SizeOf(TStringIntMap) is wrong!');

    addGlobalFunc('function ToString(constref Param: TStringIntMap): String; override;', @_LapeStringIntMap_ToString);

    addGlobalFunc('function TStringIntMap.Count: Integer', @_LapeStringIntMap_Count);
    addGlobalFunc('function TStringIntMap.Add(Key: String; Value: Int64): Integer;', @_LapeStringIntMap_Add);
    addGlobalFunc('function TStringIntMap.IndexOf(Key: String): Integer;', @_LapeStringIntMap_IndexOf);
    addGlobalFunc('function TStringIntMap.IndicesOf(Key: String): TIntegerArray;', @_LapeStringIntMap_IndicesOf);
    addGlobalFunc('procedure TStringIntMap.Clear', @_LapeStringIntMap_Clear);
    addGlobalFunc('procedure TStringIntMap.Delete(Index: Integer); overload', @_LapeStringIntMap_Delete1);
    addGlobalFunc('procedure TStringIntMap.Delete(Key: String); overload', @_LapeStringIntMap_Delete2);
    addGlobalFunc('procedure TStringIntMap.DeleteAll(Key: String)', @_LapeStringIntMap_DeleteAll);
    addGlobalFunc('function TStringIntMap.Exists(Key: String): Boolean;', @_LapeStringIntMap_Exists);
    addGlobalFunc('procedure TStringIntMap.Load(FileName: String; Sep: String = "=")', @_LapeStringIntMap_Load);
    addGlobalFunc('procedure TStringIntMap.Save(FileName: String; Sep: String = "=");', @_LapeStringIntMap_Save);

    addGlobalFunc('function TStringIntMap.GetValues(Key: String): TInt64Array', @_LapeStringIntMap_GetValues);
    addGlobalFunc('function TStringIntMap.GetValue(Key: String): Int64', @_LapeStringIntMap_GetValue);
    addGlobalFunc('procedure TStringIntMap.SetValue(Key: String; Value: Int64)', @_LapeStringIntMap_SetValue);

    addClassVar('TStringIntMap', 'Pair', 'TStringIntPair', @_LapeStringIntMap_GetPair, nil, True);
    addClassVar('TStringIntMap', 'Key', 'String', @_LapeStringIntMap_GetKey, @_LapeStringIntMap_SetKey, True);
    addClassVar('TStringIntMap', 'Sorted', 'Boolean', @_LapeStringIntMap_GetSorted, @_LapeStringIntMap_SetSorted);
    addClassVar('TStringIntMap', 'CaseSens', 'Boolean', @_LapeStringIntMap_GetCaseSens, @_LapeStringIntMap_SetCaseSens);
  end;
end;

end.

