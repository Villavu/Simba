procedure _LapeCreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.CreateFile(PString(Params^[0])^);
end;

procedure _LapeOpenFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.OpenFile(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeRewriteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.RewriteFile(PString(Params^[0])^, PBoolean(Params^[1])^);
end;

procedure _LapeAppendFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.AppendFile(PString(Params^[0])^);
end;

procedure _LapeCloseFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MFiles.CloseFile(PInt32(Params^[0])^);
end;

procedure _LapeEndOfFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.EndOfFile(PInt32(Params^[0])^);
end;

procedure _LapeFileSize(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.FileSizeMuf(PInt32(Params^[0])^);
end;

procedure _LapeReadFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.ReadFileString(PInt32(Params^[0])^, PString(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeWriteFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PBoolean(Result)^ := MFiles.WriteFileString(PInt32(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeSetFileCharPointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.SetFileCharPointer(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^);
end;

procedure _LapeFilePointerPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PInt32(Result)^ := MFiles.FilePointerPos(PInt32(Params^[0])^);
end;

procedure _LapeGetFiles(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetFiles(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeGetDirectories(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := GetDirectories(PString(Params^[0])^);
end;

procedure _LapeWriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MFiles.WriteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    PString(Result)^ := MFiles.ReadINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeDeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  with SimbaScript.Client do
    MFiles.DeleteINI(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeUnZipFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  UnZipFile(PString(Params^[0])^, PString(Params^[1])^);
end;

procedure _LapeUnZipOneFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := UnZipOneFile(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeZipFiles(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  ZipFiles(PString(Params^[0])^, PStringArray(Params^[1])^);
end;

procedure _LapeFindFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  List: TStringList;
  Files: ^TStringArray absolute Result;
  i: Int32;
begin
  List := TStringList.Create();

  try
    with TListFileSearcher.Create(List) do
    try
      Search(PString(Params^[0])^, PString(Params^[1])^, PBoolean(Params^[2])^, PBoolean(Params^[3])^);
    finally
      Free();
    end;

    SetLength(Files^, List.Count);
    for i := 0 to List.Count - 1 do
      Files^[i] := List[i];
  finally
    List.Free();
  end;
end;

