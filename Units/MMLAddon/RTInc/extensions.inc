procedure RUTIS_ext_UnTar(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ext_UnTar(Pstring(Params^[0].Data)^, PStringArray(Params^[1].Data)^);
end;

procedure RUTIS_ext_UnTarEx(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ext_UnTarEx(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^, Pboolean(Params^[2].Data)^);
end;

procedure RUTIS_ext_DecompressBZip2(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ext_DecompressBZip2(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^, PCardinal(Params^[2].Data)^);
end;

procedure RUTIS_ext_GetPage(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pstring(Result^.Data)^ := ext_GetPage(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ext_MessageDlg(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ext_MessageDlg(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^, PMsgDlgType(Params^[2].Data)^, PMsgDlgButtons(Params^[3].Data)^, PLongint(Params^[4].Data)^);
end;

procedure RUTIS_ext_SDTMToMDTM(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PMDTM(Result^.Data)^ := ext_SDTMToMDTM(PSDTM(Params^[0].Data)^);
end;

procedure RUTIS_ext_InputQuery(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ext_InputQuery(PString(Params^[0].Data)^, PString(Params^[1].Data)^, PString(Params^[2].Data)^);
end;
