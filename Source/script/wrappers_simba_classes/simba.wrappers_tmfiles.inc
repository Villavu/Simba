procedure _LapeMFiles_CreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.CreateFile(PString(Params^[1])^);
end;

procedure _LapeMFiles_OpenFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.OpenFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMFiles_RewriteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.RewriteFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMFiles_AppendFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.AppendFile(PString(Params^[1])^);
end;

procedure _LapeMFiles_DeleteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.DeleteFile(PString(Params^[1])^);
end;

procedure _LapeMFiles_RenameFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.RenameFile(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMFiles_CloseFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.CloseFile(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_WriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.WriteINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^, PString(Params^[4])^);
end;

procedure _LapeMFiles_ReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMFiles(Params^[0])^.ReadINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeMFiles_DeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.DeleteINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeMFiles_EndOfFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.EndOfFile(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_FileSizeMuf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMFiles(Params^[0])^.FileSizeMuf(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_ReadFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.ReadFileString(PInteger(Params^[1])^, PString(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMFiles_WriteFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.WriteFileString(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMFiles_SetFileCharPointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.SetFileCharPointer(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMFiles_FilePointerPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.FilePointerPos(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^ := TMFiles.Create(PObject(Params^[1])^);
end;

procedure _LapeMFiles_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.Free();
end;

