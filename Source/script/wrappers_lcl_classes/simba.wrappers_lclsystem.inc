type
  PCollection = ^TCollection;
  PCollectionItem = ^TCollectionItem;
  PComponent = ^TComponent;
  PComponentName = ^TComponentName;
  PComponentState = ^TComponentState;
  PCustomMemoryStream = ^TCustomMemoryStream;
  PFileStream = ^TFileStream;
  PHandle = ^THandle;
  PHandleStream = ^THandleStream;
  PMemoryStream = ^TMemoryStream;
  PNotifyEvent = ^TNotifyEvent;
  PPersistent = ^TPersistent;
  PSeekOrigin = ^TSeekOrigin;
  PStream = ^TStream;
  PStringList = ^TStringList;
  PStringListSortCompare = ^TStringListSortCompare;
  PStrings = ^TStrings;
  PStringStream = ^TStringStream;

procedure _LapePersistent_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPersistent(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapePersistent_GetNamePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PPersistent(Params^[0])^.GetNamePath();
end;

procedure _LapePersistent_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPersistent(Params^[0])^ := TPersistent.Create();
end;

procedure _LapePersistent_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPersistent(Params^[0])^.Free();
end;

procedure _LapeStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStream(Params^[0])^.Seek(PInteger(Params^[1])^, PWord(Params^[2])^);
end;

procedure _LapeStream_ReadBuffer(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.ReadBuffer(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_WriteBuffer(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteBuffer(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_CopyFrom(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PStream(Params^[0])^.CopyFrom(PStream(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeStream_ReadComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PStream(Params^[0])^.ReadComponent(PComponent(Params^[1])^);
end;

procedure _LapeStream_ReadComponentRes(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PStream(Params^[0])^.ReadComponentRes(PComponent(Params^[1])^);
end;

procedure _LapeStream_WriteComponent(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteComponent(PComponent(Params^[1])^);
end;

procedure _LapeStream_WriteComponentRes(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteComponentRes(PString(Params^[1])^, PComponent(Params^[2])^);
end;

procedure _LapeStream_WriteDescendent(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteDescendent(PComponent(Params^[1])^, PComponent(Params^[2])^);
end;

procedure _LapeStream_WriteDescendentRes(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteDescendentRes(PString(Params^[1])^, PComponent(Params^[2])^, PComponent(Params^[3])^);
end;

procedure _LapeStream_WriteResourceHeader(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteResourceHeader(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_FixupResourceHeader(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.FixupResourceHeader(PInteger(Params^[1])^);
end;

procedure _LapeStream_ReadResHeader(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.ReadResHeader();
end;

procedure _LapeStream_ReadByte(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  pbyte(Result)^ := PStream(Params^[0])^.ReadByte();
end;

procedure _LapeStream_ReadDWord(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := PStream(Params^[0])^.ReadDWord();
end;

procedure _LapeStream_ReadAnsiString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStream(Params^[0])^.ReadAnsiString();
end;

procedure _LapeStream_WriteByte(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteByte(pbyte(Params^[1])^);
end;

procedure _LapeStream_WriteWord(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteWord(PWord(Params^[1])^);
end;

procedure _LapeStream_WriteDWord(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteDWord(PCardinal(Params^[1])^);
end;

procedure _LapeStream_WriteAnsiString(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteAnsiString(PString(Params^[1])^);
end;

procedure _LapeStream_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PStream(Params^[0])^.Position;
end;

procedure _LapeStream_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.Position := PInt64(Params^[1])^;
end;

procedure _LapeStream_Size_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PStream(Params^[0])^.Size;
end;

procedure _LapeStream_Size_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.Size := PInt64(Params^[1])^;
end;

procedure _LapeStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^ := TStream.Create();
end;

procedure _LapeStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.Free();
end;

procedure _LapeHandleStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandleStream(Params^[0])^ := THandleStream.Create(PHandle(Params^[1])^);
end;

procedure _LapeHandleStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PHandleStream(Params^[0])^.Read(Ppointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeHandleStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PHandleStream(Params^[0])^.Write(Ppointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeHandleStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PHandleStream(Params^[0])^.Seek(PInt64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeHandleStream_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PHandleStream(Params^[0])^.Handle;
end;

procedure _LapeHandleStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandleStream(Params^[0])^.Free();
end;

procedure _LapeFileStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileStream(Params^[0])^ := TFileStream.Create(PString(Params^[1])^, PWord(Params^[2])^);
end;

procedure _LapeFileStream_InitEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileStream(Params^[0])^ := TFileStream.Create(PString(Params^[1])^, PWord(Params^[2])^, PCardinal(Params^[3])^);
end;

procedure _LapeFileStream_FileName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileStream(Params^[0])^.FileName;
end;

procedure _LapeFileStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileStream(Params^[0])^.Free();
end;

procedure _LapeCustomMemoryStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomMemoryStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomMemoryStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInt64(Result)^ := PCustomMemoryStream(Params^[0])^.Seek(PInteger(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeCustomMemoryStream_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeCustomMemoryStream_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeCustomMemoryStream_Memory_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPointer(Result)^ := PCustomMemoryStream(Params^[0])^.Memory;
end;

procedure _LapeCustomMemoryStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^ := TCustomMemoryStream.Create();
end;

procedure _LapeCustomMemoryStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemoryStream(Params^[0])^.Free();
end;

procedure _LapeMemoryStream_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.Clear();
end;

procedure _LapeMemoryStream_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeMemoryStream_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeMemoryStream_SetSize(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.SetSize(PPtrInt(Params^[1])^);
end;

procedure _LapeMemoryStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMemoryStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMemoryStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^ := TMemoryStream.Create();
end;

procedure _LapeMemoryStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoryStream(Params^[0])^.Free();
end;

procedure _LapeStringStream_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringStream(Params^[0])^ := TStringStream.Create(PString(Params^[1])^);
end;

procedure _LapeStringStream_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringStream_ReadString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStringStream(Params^[0])^.ReadString(PInteger(Params^[1])^);
end;

procedure _LapeStringStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringStream(Params^[0])^.Seek(PInteger(Params^[1])^, PWord(Params^[2])^);
end;

procedure _LapeStringStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringStream_WriteString(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringStream(Params^[0])^.WriteString(PString(Params^[1])^);
end;

procedure _LapeStringStream_DataString_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStringStream(Params^[0])^.DataString;
end;

procedure _LapeStringStream_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringStream(Params^[0])^.Free();
end;

procedure _LapeStrings_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Add(PString(Params^[1])^);
end;

procedure _LapeStrings_AddObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.AddObject(PString(Params^[1])^, PObject(Params^[2])^);
end;

procedure _LapeStrings_Append(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Append(PString(Params^[1])^);
end;

procedure _LapeStrings_AddStrings(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.AddStrings(PStrings(Params^[1])^);
end;

procedure _LapeStrings_AddStringsArray(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.AddStrings(PStringArray(Params^[1])^);
end;

procedure _LapeStrings_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeStrings_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.BeginUpdate();
end;

procedure _LapeStrings_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Clear();
end;

procedure _LapeStrings_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStrings_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.EndUpdate();
end;

procedure _LapeStrings_Equals(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStrings(Params^[0])^.Equals(PStrings(Params^[1])^);
end;

procedure _LapeStrings_Exchange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStrings_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStrings_IndexOfName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfName(PString(Params^[1])^);
end;

procedure _LapeStrings_IndexOfObject(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfObject(PObject(Params^[1])^);
end;

procedure _LapeStrings_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Insert(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStrings_InsertObject(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.InsertObject(PInteger(Params^[1])^, PString(Params^[2])^, PObject(Params^[3])^);
end;

procedure _LapeStrings_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeStrings_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeStrings_Move(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Move(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStrings_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeStrings_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeStrings_ToStringArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringArray(Result)^ := PStrings(Params^[0])^.ToStringArray;
end;

procedure _LapeStrings_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Count;
end;

procedure _LapeStrings_Objects_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PObject(Result)^ := PStrings(Params^[0])^.Objects[PInteger(Params^[1])^];
end;

procedure _LapeStrings_Objects_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Objects[PInteger(Params^[1])^] := PObject(Params^[2])^;
end;

procedure _LapeStrings_Strings_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStrings(Params^[0])^.Strings[PInteger(Params^[1])^];
end;

procedure _LapeStrings_Strings_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Strings[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStrings_Values_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStrings(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStrings_Values_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Values[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStrings_Names_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStrings(Params^[0])^.Names[PInteger(Params^[1])^];
end;

procedure _LapeStrings_ValueFromIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStrings(Params^[0])^.ValueFromIndex[PInteger(Params^[1])^];
end;

procedure _LapeStrings_ValueFromIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.ValueFromIndex[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStrings_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PStrings(Params^[0])^.Text;
end;

procedure _LapeStrings_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Text := PString(Params^[1])^;
end;

procedure _LapeStrings_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^ := TStrings.Create();
end;

procedure _LapeStrings_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Params^[0])^.Free();
end;

procedure _LapeStringList_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringList(Params^[0])^.Add(PString(Params^[1])^);
end;

procedure _LapeStringList_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Clear();
end;

procedure _LapeStringList_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringList_Exchange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringList_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Find(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringList_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStringList(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringList_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Insert(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringList_Sort(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Sort();
end;

procedure _LapeStringList_CustomSort(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.CustomSort(PStringListSortCompare(Params^[1])^);
end;

procedure _LapeStringList_Sorted_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Sorted;
end;

procedure _LapeStringList_Sorted_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringList_CaseSensitive_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.CaseSensitive;
end;

procedure _LapeStringList_CaseSensitive_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.CaseSensitive := PBoolean(Params^[1])^;
end;

procedure _LapeStringList_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChange;
end;

procedure _LapeStringList_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeStringList_OnChanging_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChanging;
end;

procedure _LapeStringList_OnChanging_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure _LapeStringList_OwnsObjects_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PStringList(Params^[0])^.OwnsObjects;
end;

procedure _LapeStringList_OwnsObjects_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.OwnsObjects := Pboolean(Params^[1])^;
end;

procedure _LapeStringList_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^ := TStringList.Create();
end;

procedure _LapeStringList_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStringList(Params^[0])^.Free();
end;

procedure _LapeComponent_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^ := TComponent.Create(PComponent(Params^[1])^);
end;

procedure _LapeComponent_DestroyComponents(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.DestroyComponents();
end;

procedure _LapeComponent_Destroying(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Destroying();
end;

procedure _LapeComponent_FindComponent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PComponent(Params^[0])^.FindComponent(PString(Params^[1])^);
end;

procedure _LapeComponent_InsertComponent(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.InsertComponent(PComponent(Params^[1])^);
end;

procedure _LapeComponent_RemoveComponent(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.RemoveComponent(PComponent(Params^[1])^);
end;

procedure _LapeComponent_Components(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PComponent(Params^[0])^.Components[PInteger(Params^[1])^];
end;

procedure _LapeComponent_ComponentCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentCount;
end;

procedure _LapeComponent_ComponentIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentIndex;
end;

procedure _LapeComponent_ComponentIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.ComponentIndex := PInteger(Params^[1])^;
end;

procedure _LapeComponent_ComponentState_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponentState(Result)^ := PComponent(Params^[0])^.ComponentState;
end;

procedure _LapeComponent_Owner_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PComponent(Params^[0])^.Owner;
end;

procedure _LapeComponent_Name_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponentName(Result)^ := PComponent(Params^[0])^.Name;
end;

procedure _LapeComponent_Name_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Name := PComponentName(Params^[1])^;
end;

procedure _LapeComponent_Tag_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPtrInt(Result)^ := PComponent(Params^[0])^.Tag;
end;

procedure _LapeComponent_Tag_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Tag := PPtrInt(Params^[1])^;
end;

procedure _LapeComponent_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Params^[0])^.Free();
end;

procedure _LapeCollectionItem_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^ := TCollectionItem.Create(PCollection(Params^[1])^);
end;

procedure _LapeCollectionItem_GetNamePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PCollectionItem(Params^[0])^.GetNamePath();
end;

procedure _LapeCollectionItem_Collection_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Result)^ := PCollectionItem(Params^[0])^.Collection;
end;

procedure _LapeCollectionItem_Collection_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.Collection := PCollection(Params^[1])^;
end;

procedure _LapeCollectionItem_ID_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCollectionItem(Params^[0])^.ID;
end;

procedure _LapeCollectionItem_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCollectionItem(Params^[0])^.Index;
end;

procedure _LapeCollectionItem_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.Index := PInteger(Params^[1])^;
end;

procedure _LapeCollectionItem_DisplayName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PCollectionItem(Params^[0])^.DisplayName;
end;

procedure _LapeCollectionItem_DisplayName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.DisplayName := PString(Params^[1])^;
end;

procedure _LapeCollectionItem_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Params^[0])^.Free();
end;

procedure _LapeCollection_Owner(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPersistent(Result)^ := PCollection(Params^[0])^.Owner();
end;

procedure _LapeCollection_Add(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Add();
end;

procedure _LapeCollection_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeCollection_BeginUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.BeginUpdate();
end;

procedure _LapeCollection_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Clear();
end;

procedure _LapeCollection_EndUpdate(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.EndUpdate();
end;

procedure _LapeCollection_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeCollection_GetNamePath(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PCollection(Params^[0])^.GetNamePath();
end;

procedure _LapeCollection_Insert(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Insert(PInteger(Params^[1])^);
end;

procedure _LapeCollection_FindItemID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.FindItemID(PInteger(Params^[1])^);
end;

procedure _LapeCollection_Exchange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCollection_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCollection(Params^[0])^.Count;
end;

procedure _LapeCollection_Items_Index_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollectionItem(Result)^ := PCollection(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeCollection_Items_Index_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Items[PInteger(Params^[1])^] := PCollectionItem(Params^[2])^;
end;

procedure _LapeCollection_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCollection(Params^[0])^.Free();
end;

