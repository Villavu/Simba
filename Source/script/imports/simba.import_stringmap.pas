unit simba.import_stringmap;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportStringMap(Compiler: TSimbaScript_Compiler);
procedure ImportImageMap(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, lpvartypes,
  simba.container_stringmap, simba.container_imagemap, simba.image;

type
  PSimbaImageMap = ^TSimbaImageMap;
  PSimbaStringMap = ^TSimbaStringMap;

procedure _LapeStringMap_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaStringMap(Params^[0])^.ToString();
end;

procedure _LapeStringMap_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaStringMap(Params^[0])^.Count;
end;

procedure _LapeStringMap_KeyFromIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.KeyFromIndex[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStringMap_KeyFromIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaStringMap(Params^[0])^.KeyFromIndex[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_ValueFromIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.ValueFromIndex[PInteger(Params^[1])^] := PVariant(Params^[2])^;
end;

procedure _LapeStringMap_ValueFromIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := PSimbaStringMap(Params^[0])^.ValueFromIndex[PInteger(Params^[1])^];
end;

procedure _LapeStringMap_Value_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Value[PString(Params^[1])^] := PVariant(Params^[2])^;
end;

procedure _LapeStringMap_Value_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariant(Result)^ := PSimbaStringMap(Params^[0])^.Value[PString(Params^[1])^];
end;

procedure _LapeStringMap_Values_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PVariantArray(Result)^ := PSimbaStringMap(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStringMap_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaStringMap(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringMap_IndicesOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PIntegerArray(Result)^ := PSimbaStringMap(Params^[0])^.IndicesOf(PString(Params^[1])^);
end;

procedure _LapeStringMap_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaStringMap(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeStringMap_Add(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.Add(PString(Params^[1])^, PVariant(Params^[2])^);
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

procedure _LapeStringMap_AllowDuplicates_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaStringMap(Params^[0])^.AllowDuplicates;
end;

procedure _LapeStringMap_AllowDuplicates_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaStringMap(Params^[0])^.AllowDuplicates := PBoolean(Params^[1])^;
end;

procedure _LapeImageMap_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaImageMap(Params^[0])^.Count;
end;

procedure _LapeImageMap_Image_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImage(Result)^ := PSimbaImageMap(Params^[0])^.Image[PString(Params^[1])^];
end;

procedure _LapeImageMap_Image_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageMap(Params^[0])^.Image[PString(Params^[1])^] := PSimbaImage(Params^[2])^;
end;

procedure _LapeImageMap_Images_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageArray(Result)^ := PSimbaImageMap(Params^[0])^.Images[PString(Params^[1])^];
end;

procedure _LapeImageMap_Exists(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSimbaImageMap(Params^[0])^.Exists(PString(Params^[1])^);
end;

procedure _LapeImageMap_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageMap(Params^[0])^.Clear();
end;

procedure _LapeImageMap_Delete(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageMap(Params^[0])^.Delete(PString(Params^[1])^);
end;

procedure _LapeImageMap_Add(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaImageMap(Params^[0])^.Add(PString(Params^[1])^, PSimbaImage(Params^[2])^);
end;

procedure _LapeImageMap_ToString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PSimbaImageMap(Params^[0])^.ToString();
end;

procedure ImportStringMap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FItems: array of record KeyHash: UInt32; Key: String; Value: Variant; end;',
      '  FCount: Integer;',
      '  FSorted: Boolean;',
      '  FCaseSens: Boolean;',
      '  FAllowDuplicate: Boolean;',
      '  {%CODETOOLS ON}',
      'end;'],
      'TStringMap'
    );
    if (getGlobalType('TStringMap').Size <> SizeOf(TSimbaStringMap)) then
      SimbaException('SizeOf(TStringMap) is wrong!');

    addGlobalFunc('function TStringMap.ToString: String', @_LapeStringMap_ToString);
    addGlobalFunc('function TStringMap.IndexOf(Key: String): Integer;', @_LapeStringMap_IndexOf);
    addGlobalFunc('function TStringMap.IndicesOf(Key: String): TIntegerArray;', @_LapeStringMap_IndicesOf);
    addGlobalFunc('procedure TStringMap.Add(Key: String; Value: Variant);', @_LapeStringMap_Add);
    addGlobalFunc('procedure TStringMap.Clear', @_LapeStringMap_Clear);
    addGlobalFunc('procedure TStringMap.Delete(Index: Integer); overload', @_LapeStringMap_Delete1);
    addGlobalFunc('procedure TStringMap.Delete(Key: String); overload', @_LapeStringMap_Delete2);
    addGlobalFunc('function TStringMap.Exists(Key: String): Boolean;', @_LapeStringMap_Exists);
    addGlobalFunc('procedure TStringMap.Load(FileName: String; VarType: EVariantVarType; Sep: String = "=")', @_LapeStringMap_Load);
    addGlobalFunc('procedure TStringMap.Save(FileName: String; Sep: String = "=");', @_LapeStringMap_Save);

    addProperty('TStringMap', 'AllowDuplicates', 'Boolean', @_LapeStringMap_AllowDuplicates_Read, @_LapeStringMap_AllowDuplicates_Write);
    addProperty('TStringMap', 'Sorted', 'Boolean', @_LapeStringMap_GetSorted, @_LapeStringMap_SetSorted);
    addProperty('TStringMap', 'CaseSens', 'Boolean', @_LapeStringMap_GetCaseSens, @_LapeStringMap_SetCaseSens);
    addProperty('TStringMap', 'Count', 'Integer', @_LapeStringMap_Count);

    addPropertyIndexed('TStringMap', 'KeyFromIndex', 'Index: Integer', 'String', @_LapeStringMap_KeyFromIndex_Read, @_LapeStringMap_KeyFromIndex_Write);
    addPropertyIndexed('TStringMap', 'ValueFromIndex', 'Index: Integer', 'Variant', @_LapeStringMap_ValueFromIndex_Read, @_LapeStringMap_ValueFromIndex_Write);
    addPropertyIndexed('TStringMap', 'Value', 'Key: String', 'Variant', @_LapeStringMap_Value_Read, @_LapeStringMap_Value_Write);
    addPropertyIndexed('TStringMap', 'Values', 'Key: String', 'TVariantArray', @_LapeStringMap_Values_Read);

    addDelayedCode([
      'function ToString(constref Param: TStringMap): String; override;',
      'begin',
      '  Result := Param.ToString();',
      'end;'
    ]);
  end;
end;

procedure ImportImageMap(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  FItems: array of record KeyHash: UInt32; Key: String; Value: TImage; end;',
      '  FCount: Integer;',
      '  {%CODETOOLS ON}',
      'end;'],
      'TImageMap'
    );
    if (getGlobalType('TImageMap').Size <> SizeOf(TSimbaImageMap)) then
      SimbaException('SizeOf(TImageMap) is wrong!');

    addGlobalFunc('function TImageMap.ToString: String', @_LapeImageMap_ToString);
    addGlobalFunc('procedure TImageMap.Add(Key: String; Img: TImage);', @_LapeImageMap_Add);
    addGlobalFunc('procedure TImageMap.Clear;', @_LapeImageMap_Clear);
    addGlobalFunc('procedure TImageMap.Delete(Key: String);', @_LapeImageMap_Delete);
    addGlobalFunc('function TImageMap.Exists(Key: String): Boolean;', @_LapeImageMap_Exists);

    addProperty('TImageMap', 'Count', 'Integer', @_LapeImageMap_Count);
    addPropertyIndexed('TImageMap', 'Image', 'Key: String', 'TImage', @_LapeImageMap_Image_Read, @_LapeImageMap_Image_Write);
    addPropertyIndexed('TImageMap', 'Images', 'Key: String', 'TImageArray', @_LapeImageMap_Images_Read);

    addDelayedCode([
      'function ToString(constref Param: TImageMap): String; override;',
      'begin',
      '  Result := Param.ToString();',
      'end;'
    ]);
  end;
end;

end.

