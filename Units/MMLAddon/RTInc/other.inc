procedure RUTIS_ps_Writeln(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_Writeln(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_SetScriptProp(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_SetScriptProp(PSP_Property(Params^[0].Data)^, PVariantArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetScriptProp(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_GetScriptProp(PSP_Property(Params^[0].Data)^, PVariantArray(PPointer(Params^[1].Data)^)^);
end;

procedure RUTIS_ps_Wait(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_Wait(PInteger(Params^[0].Data)^);
end;
