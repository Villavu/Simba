procedure RUTIS_ps_GetColor(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PColor(Result^.Data)^ := ps_GetColor(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetColorsWrap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_GetColorsWrap(PPointArray(Params^[0].Data)^, PIntegerArray(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetColors(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PIntegerArray(Result^.Data)^ := ps_GetColors(PPointArray(Params^[0].Data)^);
end;

procedure RUTIS_ps_findcolor(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_findcolor(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pinteger(Params^[3].Data)^, Pinteger(Params^[4].Data)^, Pinteger(Params^[5].Data)^, Pinteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_findcolortoleranceOptimised(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_findcolortoleranceOptimised(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pinteger(Params^[3].Data)^, Pinteger(Params^[4].Data)^, Pinteger(Params^[5].Data)^, Pinteger(Params^[6].Data)^, Pinteger(Params^[7].Data)^);
end;

procedure RUTIS_ps_findcolortolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_findcolortolerance(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^, Pinteger(Params^[3].Data)^, Pinteger(Params^[4].Data)^, Pinteger(Params^[5].Data)^, Pinteger(Params^[6].Data)^, Pinteger(Params^[7].Data)^);
end;

procedure RUTIS_ps_FindColors(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColors(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^);
end;

procedure RUTIS_ps_SetColorToleranceSpeed(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetColorToleranceSpeed(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetToleranceSpeed(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_GetToleranceSpeed();
end;

procedure RUTIS_ps_SetToleranceSpeed2Modifiers(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetToleranceSpeed2Modifiers(PExtended(Params^[0].Data)^, PExtended(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetToleranceSpeed2Modifiers(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_GetToleranceSpeed2Modifiers(PExtended(Params^[0].Data)^, PExtended(Params^[1].Data)^);
end;

procedure RUTIS_ps_SimilarColors(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_SimilarColors(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_CountColor(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_CountColor(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^);
end;

procedure RUTIS_ps_CountColorTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_CountColorTolerance(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^);
end;

procedure RUTIS_ps_FindColorsToleranceOptimised(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColorsToleranceOptimised(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FindColorsTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColorsTolerance(PPointArray(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FindColorSpiral(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColorSpiral(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^);
end;

procedure RUTIS_ps_FindColorSpiralTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColorSpiralTolerance(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^);
end;

procedure RUTIS_ps_FindColorsSpiralTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_FindColorsSpiralTolerance(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PPointArray(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^, PInteger(Params^[8].Data)^);
end;

procedure RUTIS_ps_FindColoredArea(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColoredArea(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^);
end;

procedure RUTIS_ps_FindColoredAreaTolerance(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FindColoredAreaTolerance(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^, PInteger(Params^[3].Data)^, PInteger(Params^[4].Data)^, PInteger(Params^[5].Data)^, PInteger(Params^[6].Data)^, PInteger(Params^[7].Data)^, PInteger(Params^[8].Data)^);
end;
