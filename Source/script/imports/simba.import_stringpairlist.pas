unit simba.import_stringpairlist;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportStringPairList(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes,
  simba.stringpairlist;

procedure _LapeStringPairList_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringPairList(Params^[0])^.ToString();
end;

procedure _LapeStringPairList_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringPairList(Params^[0])^.Count;
end;

procedure _LapeStringPairList_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringPairList(Params^[0])^.Add(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringPairList_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringPairList(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringPairList_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PStringPairList(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeStringPairList_GetValue(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringPairList(Params^[0])^.Value[PString(Params^[1])^];
end;

procedure _LapeStringPairList_GetValues(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PStringPairList(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStringPairList_GetPair(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPair(Result)^ := PStringPairList(Params^[0])^.Pair[PInteger(Params^[1])^];
end;

procedure _LapeStringPairList_GetKey(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringPairList(Params^[0])^.Key[PInteger(Params^[1])^];
end;

procedure _LapeStringPairList_SetKey(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Key[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringPairList_SetValue(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Value[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringPairList_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringPairList(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeStringPairList_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Clear();
end;

procedure _LapeStringPairList_Delete1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringPairList_Delete2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeStringPairList_DeleteAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.DeleteAll(PString(Params^[1])^);
end;

procedure _LapeStringPairList_Load(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Load(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringPairList_Save(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Save(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringPairList_SetSorted(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringPairList_SetCaseSens(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringPairList(Params^[0])^.CaseSens := PBoolean(Params^[1])^;
end;

procedure _LapeStringPairList_GetSorted(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringPairList(Params^[0])^.Sorted;
end;

procedure _LapeStringPairList_GetCaseSens(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringPairList(Params^[0])^.CaseSens;
end;

procedure ImportStringPairList(Compiler: TSimbaScript_Compiler);
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
      'end;'],
      'TStringPairList'
    );
    if (getGlobalType('TStringPairList').Size <> SizeOf(TStringPairList)) then
      SimbaException('SizeOf(TStringPairList) is wrong!');

    addGlobalFunc('function ToString(constref Param: TStringPairList): String; override;', @_LapeStringPairList_ToString);

    addGlobalFunc('function TStringPairList.Count: Integer', @_LapeStringPairList_Count);
    addGlobalFunc('function TStringPairList.Add(Key, Value: String): Integer;', @_LapeStringPairList_Add);
    addGlobalFunc('function TStringPairList.IndexOf(Key: String): Integer;', @_LapeStringPairList_IndexOf);
    addGlobalFunc('function TStringPairList.IndicesOf(Key: String): TIntegerArray;', @_LapeStringPairList_IndicesOf);
    addGlobalFunc('procedure TStringPairList.Clear', @_LapeStringPairList_Clear);
    addGlobalFunc('procedure TStringPairList.Delete(Index: Integer); overload', @_LapeStringPairList_Delete1);
    addGlobalFunc('procedure TStringPairList.Delete(Key: String); overload', @_LapeStringPairList_Delete2);
    addGlobalFunc('procedure TStringPairList.DeleteAll(Key: String)', @_LapeStringPairList_DeleteAll);
    addGlobalFunc('function TStringPairList.Exists(Key: String): Boolean;', @_LapeStringPairList_Exists);
    addGlobalFunc('procedure TStringPairList.Load(FileName: String; Sep: String = "=")', @_LapeStringPairList_Load);
    addGlobalFunc('procedure TStringPairList.Save(FileName: String; Sep: String = "=");', @_LapeStringPairList_Save);

    addGlobalFunc('function TStringPairList.GetValues(Key: String): TStringArray', @_LapeStringPairList_GetValues);
    addGlobalFunc('function TStringPairList.GetValue(Key: String): String', @_LapeStringPairList_GetValue);
    addGlobalFunc('procedure TStringPairList.SetValue(Key: String; Value: String)', @_LapeStringPairList_SetValue);

    addClassVar('TStringPairList', 'Pair', 'TStringPair', @_LapeStringPairList_GetPair, nil, True);
    addClassVar('TStringPairList', 'Key', 'String', @_LapeStringPairList_GetKey, @_LapeStringPairList_SetKey, True);
    addClassVar('TStringPairList', 'Sorted', 'Boolean', @_LapeStringPairList_GetSorted, @_LapeStringPairList_SetSorted);
    addClassVar('TStringPairList', 'CaseSens', 'Boolean', @_LapeStringPairList_GetCaseSens, @_LapeStringPairList_SetCaseSens);
  end;
end;

end.

