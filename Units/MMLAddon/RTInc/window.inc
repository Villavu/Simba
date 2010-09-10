procedure RUTIS_ps_SetDesktopAsClient(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetDesktopAsClient();
end;

procedure RUTIS_ps_SetTargetArray(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_SetTargetArray(PInteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^, Pinteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_SetTargetBitmap(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_SetTargetBitmap(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_SetEIOSTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_SetEIOSTarget(Pstring(Params^[0].Data)^, PVariant(Params^[1].Data)^);
end;

procedure RUTIS_ps_SetImageTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetImageTarget(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_SetKeyMouseTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_SetKeyMouseTarget(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetImageTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_GetImageTarget();
end;

procedure RUTIS_ps_GetKeyMouseTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pinteger(Result^.Data)^ := ps_GetKeyMouseTarget();
end;

procedure RUTIS_ps_ExportImageTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PTarget_Exported(Result^.Data)^ := ps_ExportImageTarget();
end;

procedure RUTIS_ps_ExportKeyMouseTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PTarget_Exported(Result^.Data)^ := ps_ExportKeyMouseTarget();
end;

procedure RUTIS_ps_FreeTarget(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_FreeTarget(Pinteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetClientDimensions(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_GetClientDimensions(Pinteger(Params^[0].Data)^, Pinteger(Params^[1].Data)^);
end;

procedure RUTIS_ps_Freeze(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_Freeze();
end;

procedure RUTIS_ps_Unfreeze(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_Unfreeze();
end;

procedure RUTIS_ps_ActivateClient(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_ActivateClient();
end;

procedure RUTIS_ps_IsTargetValid(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_IsTargetValid();
end;
