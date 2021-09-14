type
  PObject = ^TObject;

procedure _LapeObject_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Params^[0])^ := TObject.Create();
end;

procedure _LapeObject_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Params^[0])^.Free();
end;

procedure _LapeObject_ClassName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PObject(Params^[0])^.ClassName;
end;

