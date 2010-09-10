procedure RUTIS_ps_CreateFile(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_CreateFile(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_OpenFile(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_OpenFile(Pstring(Params^[0].Data)^, PBoolean(Params^[1].Data)^);
end;

procedure RUTIS_ps_RewriteFile(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_RewriteFile(Pstring(Params^[0].Data)^, PBoolean(Params^[1].Data)^);
end;

procedure RUTIS_ps_AppendFile(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_AppendFile(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_CloseFile(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_CloseFile(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_EndOfFile(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_EndOfFile(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_FileSize(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PLongInt(Result^.Data)^ := ps_FileSize(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_ReadFileString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_ReadFileString(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_WriteFileString(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_WriteFileString(PInteger(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_SetFileCharPointer(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_SetFileCharPointer(PInteger(Params^[0].Data)^, PInteger(Params^[1].Data)^, PInteger(Params^[2].Data)^);
end;

procedure RUTIS_ps_FilePointerPos(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PInteger(Result^.Data)^ := ps_FilePointerPos(PInteger(Params^[0].Data)^);
end;

procedure RUTIS_ps_FileExists(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_FileExists(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_DirectoryExists(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PBoolean(Result^.Data)^ := ps_DirectoryExists(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_CreateDirectory(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_CreateDirectory(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_ForceDirectores(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  Pboolean(Result^.Data)^ := ps_ForceDirectores(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_GetFiles(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PStringArray(Result^.Data)^ := ps_GetFiles(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^);
end;

procedure RUTIS_ps_GetDirectories(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  PStringArray(Result^.Data)^ := ps_GetDirectories(Pstring(Params^[0].Data)^);
end;

procedure RUTIS_ps_WriteINI(Params: PRutisParamInfoArray; Result: PRutisParamInfo);
begin
  ps_WriteINI(Pstring(Params^[0].Data)^, Pstring(Params^[1].Data)^, Pstring(Params^[2].Data)^, Pstring(Params^[3].Data)^);
end;
