procedure RUTIS_ps_round(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_round(Pextended(Params^[0].Data)^);
end;

procedure RUTIS_ps_iAbs(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_iAbs(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_ceil(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_ceil(Pextended(Params^[0].Data)^);
end;

procedure RUTIS_ps_pow(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pextended(Result^.Data)^ := ps_pow(Pextended(Params^[0].Data)^, Pextended(Params^[1].Data)^);
end;

procedure RUTIS_ps_Max(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_Max(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_Min(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_Min(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_MinE(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pextended(Result^.Data)^ := ps_MinE(Pextended(Params^[0].Data)^, Pextended(Params^[1].Data)^);
end;

procedure RUTIS_ps_MaxE(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pextended(Result^.Data)^ := ps_MaxE(Pextended(Params^[0].Data)^, Pextended(Params^[1].Data)^);
end;

procedure RUTIS_ps_Sqr(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pextended(Result^.Data)^ := ps_Sqr(Pextended(Params^[0].Data)^);
end;

procedure RUTIS_ps_Point(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PPoint(Result^.Data)^ := ps_Point(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_Distance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_Distance(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pinteger(Params^[3].Data)^);
end;

procedure RUTIS_ps_Hypot(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PExtended(Result^.Data)^ := ps_Hypot(PExtended(Params^[0].Data)^, PExtended(Params^[1].Data)^);
end;

procedure RUTIS_ps_RandomRange(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_RandomRange(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_ArcTan2(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pextended(Result^.Data)^ := ps_ArcTan2(Pextended(Params^[0].Data)^, Pextended(Params^[1].Data)^);
end;

procedure RUTIS_ps_IncEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_IncEx(Pinteger(PPointer(Params^[0].Data)^)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_DecEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_DecEx(Pinteger(PPointer(Params^[0].Data)^)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_Factorial(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInt64(Result^.Data)^ := ps_Factorial(Plongword(Params^[0].Data)^);
end;
