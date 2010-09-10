{procedure RUTIS_MakeString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := MakeString(PPSVariantIFC(Params^[0].Data)^);
end;}

procedure RUTIS_writeln_(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  psWriteln(PString(Params^[0].Data)^);
end;

{procedure RUTIS_swap_(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := swap_(PPSExec(Params^[0].Data)^, PPSExternalProcRec(Params^[1].Data)^, PPSStack(Params^[2].Data)^, PPSStack(Params^[3].Data)^);
end;

procedure RUTIS_ToStr_(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ToStr_(PPSExec(Params^[0].Data)^, PPSExternalProcRec(Params^[1].Data)^, PPSStack(Params^[2].Data)^, PPSStack(Params^[3].Data)^);
end;}

procedure RUTIS_ps_SetScriptProp(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_SetScriptProp(PSP_Property(Params^[0].Data)^, PVariantArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetScriptProp(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_GetScriptProp(PSP_Property(Params^[0].Data)^, PVariantArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_Wait(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_Wait(PInteger(Params^[0].Data)^);
end;
