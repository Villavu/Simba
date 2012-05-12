procedure Lape_CreateFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_CreateFile(Pstring(Params^[0])^);
end;

procedure Lape_OpenFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_OpenFile(Pstring(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_RewriteFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_RewriteFile(Pstring(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure Lape_AppendFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_AppendFile(Pstring(Params^[0])^);
end;

procedure Lape_CloseFile(const Params: PParamArray); lape_extdecl
begin
  ps_CloseFile(PInteger(Params^[0])^);
end;

procedure Lape_EndOfFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_EndOfFile(PInteger(Params^[0])^);
end;

procedure Lape_FileSize(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := ps_FileSize(PInteger(Params^[0])^);
end;

procedure Lape_ReadFileString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_ReadFileString(PInteger(Params^[0])^, Pstring(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_WriteFileString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_WriteFileString(PInteger(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_SetFileCharPointer(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_SetFileCharPointer(PInteger(Params^[0])^, PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure Lape_FilePointerPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := ps_FilePointerPos(PInteger(Params^[0])^);
end;

procedure Lape_FileExists(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_FileExists(Pstring(Params^[0])^);
end;

procedure Lape_DirectoryExists(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_DirectoryExists(Pstring(Params^[0])^);
end;

procedure Lape_CreateDirectory(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_CreateDirectory(Pstring(Params^[0])^);
end;

procedure Lape_ForceDirectories(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := ps_ForceDirectories(Pstring(Params^[0])^);
end;

procedure Lape_GetFiles(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStringArray(Result)^ := ps_GetFiles(Pstring(Params^[0])^, Pstring(Params^[1])^);
end;

procedure Lape_GetDirectories(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStringArray(Result)^ := ps_GetDirectories(Pstring(Params^[0])^);
end;

procedure Lape_DeleteFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := ps_DeleteFile(Pstring(Params^[0])^);
end;

procedure Lape_WriteINI(const Params: PParamArray); lape_extdecl
begin
  ps_WriteINI(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^, Pstring(Params^[3])^);
end;

procedure Lape_ReadINI(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_ReadINI(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_DeleteINI(const Params: PParamArray); lape_extdecl
begin
  ps_DeleteINI(Pstring(Params^[0])^, Pstring(Params^[1])^, Pstring(Params^[2])^);
end;

procedure Lape_ExtractFileExt(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pstring(Result)^ := ps_ExtractFileExt(Pstring(Params^[0])^);
end;
