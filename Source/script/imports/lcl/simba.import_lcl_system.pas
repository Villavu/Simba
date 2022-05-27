unit simba.import_lcl_system;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes, ffi,
  simba.script_compiler;

type
  PObject = ^TObject;
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

procedure _LapeObject_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Params^[0])^ := TObject.Create();
end;

procedure _LapeObject_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PObject(Params^[0])^.Free();
end;

procedure _LapeObject_ClassName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PObject(Params^[0])^.ClassName;
end;

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
  PInteger(Result)^ := PStream(Params^[0])^.Read(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_Write(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStream(Params^[0])^.Write(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_Seek(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PStream(Params^[0])^.Seek(PInteger(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeStream_ReadBuffer(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.ReadBuffer(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_WriteBuffer(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStream(Params^[0])^.WriteBuffer(PPointer(Params^[1])^, PInteger(Params^[2])^);
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

procedure ImportLCLSystem(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TObject', 'Pointer');

    addGlobalFunc('procedure TObject.Init;', @_LapeObject_Init);
    addGlobalFunc('procedure TObject.Free;', @_LapeObject_Free);
    addGlobalFunc('function TObject.ClassName: String;', @_LapeObject_ClassName);

    addGlobalType('procedure(Sender: TObject) of object', 'TNotifyEvent', FFI_DEFAULT_ABI);
    addGlobalType('UInt32', 'THandle');
    addGlobalType('String', 'TComponentName');
    addGlobalType('String', 'TCaption');
    addGlobalType('(soBeginning, soCurrent, soEnd)', 'TSeekOrigin');

    addClass('TPersistent');
    addGlobalFunc('procedure TPersistent.Assign(Source: TPersistent);', @_LapePersistent_Assign);
    addGlobalFunc('function TPersistent.GetNamePath: String;', @_LapePersistent_GetNamePath);
    addGlobalFunc('procedure TPersistent.Init; override', @_LapePersistent_Init);
    addGlobalFunc('procedure TPersistent.Free;', @_LapePersistent_Free);

    addClass('TCollection');
    addClass('TCollectionItem', 'TPersistent');
    addClassVar('TCollectionItem', 'Collection', 'TCollection', @_LapeCollectionItem_Collection_Read, @_LapeCollectionItem_Collection_Write);
    addClassVar('TCollectionItem', 'ID', 'Integer', @_LapeCollectionItem_ID_Read, nil);
    addClassVar('TCollectionItem', 'Index', 'Integer', @_LapeCollectionItem_Index_Read, @_LapeCollectionItem_Index_Write);
    addClassVar('TCollectionItem', 'DisplayName', 'String', @_LapeCollectionItem_DisplayName_Read, @_LapeCollectionItem_DisplayName_Write);
    addGlobalFunc('procedure TCollectionItem.Init(ACollection: TCollection)', @_LapeCollectionItem_Init);

    addGlobalFunc('function TCollection.Owner: TPersistent;', @_LapeCollection_Owner);
    addGlobalFunc('function TCollection.Add: TCollectionItem;', @_LapeCollection_Add);
    addGlobalFunc('procedure TCollection.BeginUpdate;', @_LapeCollection_BeginUpdate);
    addGlobalFunc('procedure TCollection.Clear;', @_LapeCollection_Clear);
    addGlobalFunc('procedure TCollection.EndUpdate;', @_LapeCollection_EndUpdate);
    addGlobalFunc('procedure TCollection.Delete(Index: Integer);', @_LapeCollection_Delete);
    addGlobalFunc('function TCollection.Insert(Index: Integer): TCollectionItem;', @_LapeCollection_Insert);
    addGlobalFunc('function TCollection.FindItemID(ID: Integer): TCollectionItem;', @_LapeCollection_FindItemID);
    addGlobalFunc('procedure TCollection.Exchange(Const Index1, index2: Integer);', @_LapeCollection_Exchange);
    addClassVar('TCollection', 'Count', 'Integer', @_LapeCollection_Count_Read, nil);
    addClassVar('TCollection', 'Items', 'TCollectionItem', @_LapeCollection_Items_Index_Read, @_LapeCollection_Items_Index_Write, True);

    addClass('TComponent', 'TPersistent');
    addGlobalType('(csLoading, csReading, csWriting, csDestroying,csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,csInline, csDesignInstance)', 'TComponentState');
    addGlobalFunc('procedure TComponent.Init(AOwner: TComponent); overload', @_LapeComponent_Init);
    addGlobalFunc('procedure TComponent.DestroyComponents;', @_LapeComponent_DestroyComponents);
    addGlobalFunc('procedure TComponent.Destroying;', @_LapeComponent_Destroying);
    addGlobalFunc('function TComponent.FindComponent(const AName: String): TComponent;', @_LapeComponent_FindComponent);
    addGlobalFunc('procedure TComponent.InsertComponent(AComponent: TComponent);', @_LapeComponent_InsertComponent);
    addGlobalFunc('procedure TComponent.RemoveComponent(AComponent: TComponent);', @_LapeComponent_RemoveComponent);
    addGlobalFunc('function TComponent.GetComponent(index: Integer): TComponent;', @_LapeComponent_Components);
    addClassVar('TComponent', 'ComponentCount', 'Integer', @_LapeComponent_ComponentCount_Read);
    addClassVar('TComponent', 'ComponentIndex', 'Integer', @_LapeComponent_ComponentIndex_Read, @_LapeComponent_ComponentIndex_Write);
    addClassVar('TComponent', 'ComponentState', 'TComponentState', @_LapeComponent_ComponentState_Read);
    addClassVar('TComponent', 'Owner', 'TComponent', @_LapeComponent_Owner_Read);
    addClassVar('TComponent', 'Name', 'TComponentName', @_LapeComponent_Name_Read, @_LapeComponent_Name_Write);
    addClassVar('TComponent', 'Tag', 'PtrInt', @_LapeComponent_Tag_Read, @_LapeComponent_Tag_Write);

    addClass('TStream');
    addGlobalFunc('function TStream.Read(var Buffer; Count: Integer): Integer;', @_LapeStream_Read);
    addGlobalFunc('function TStream.Write(constref Buffer; Count: Integer): Integer;', @_LapeStream_Write);
    addGlobalFunc('function TStream.Seek(Offset: Integer; Origin: TSeekOrigin): Integer;', @_LapeStream_Seek);
    addGlobalFunc('procedure TStream.ReadBuffer(var Buffer; Count: Integer);', @_LapeStream_ReadBuffer);
    addGlobalFunc('procedure TStream.WriteBuffer(constref Buffer; Count: Integer);', @_LapeStream_WriteBuffer);
    addGlobalFunc('function TStream.CopyFrom(Source: TStream; Count: Int64): Int64;', @_LapeStream_CopyFrom);
    addGlobalFunc('function TStream.ReadComponent(Instance: TComponent): TComponent;', @_LapeStream_ReadComponent);
    addGlobalFunc('function TStream.ReadComponentRes(Instance: TComponent): TComponent;', @_LapeStream_ReadComponentRes);
    addGlobalFunc('procedure TStream.WriteComponent(Instance: TComponent);', @_LapeStream_WriteComponent);
    addGlobalFunc('procedure TStream.WriteComponentRes(const ResName: String; Instance: TComponent);', @_LapeStream_WriteComponentRes);
    addGlobalFunc('procedure TStream.WriteDescendent(Instance, Ancestor: TComponent);', @_LapeStream_WriteDescendent);
    addGlobalFunc('procedure TStream.WriteDescendentRes(const ResName: String; Instance, Ancestor: TComponent);', @_LapeStream_WriteDescendentRes);
    addGlobalFunc('procedure TStream.WriteResourceHeader(const ResName: String; var FixupInfo: Integer);', @_LapeStream_WriteResourceHeader);
    addGlobalFunc('procedure TStream.FixupResourceHeader(FixupInfo: Integer);', @_LapeStream_FixupResourceHeader);
    addGlobalFunc('procedure TStream.ReadResHeader;', @_LapeStream_ReadResHeader);
    addGlobalFunc('function TStream.ReadByte: Byte;', @_LapeStream_ReadByte);
    addGlobalFunc('function TStream.ReadDWord: UInt32;', @_LapeStream_ReadDWord);
    addGlobalFunc('function TStream.ReadAnsiString: String;', @_LapeStream_ReadAnsiString);
    addGlobalFunc('procedure TStream.WriteByte(b: Byte);', @_LapeStream_WriteByte);
    addGlobalFunc('procedure TStream.WriteWord(w: Int16);', @_LapeStream_WriteWord);
    addGlobalFunc('procedure TStream.WriteDWord(d: UInt32);', @_LapeStream_WriteDWord);
    addGlobalFunc('procedure TStream.WriteAnsiString(const S: String);', @_LapeStream_WriteAnsiString);
    addClassVar('TStream', 'Position', 'Integer', @_LapeStream_Position_Read, @_LapeStream_Position_Write);
    addClassVar('TStream', 'Size', 'Integer', @_LapeStream_Size_Read, @_LapeStream_Size_Write);
    addGlobalFunc('procedure TStream.Init', @_LapeStream_Init);
    addGlobalFunc('procedure TStream.Free;', @_LapeStream_Free);

    addClass('THandleStream', 'TStream');
    addClassVar('THandleStream', 'Handle', 'THandle', @_LapeHandleStream_Handle_Read);
    addGlobalFunc('procedure THandleStream.Init(AHandle: THandle)', @_LapeHandleStream_Init);

    addClass('TFileStream', 'THandleStream');
    addClassVar('TFileStream', 'FileName', 'String', @_LapeFileStream_FileName_Read);
    addGlobalFunc('procedure TFileStream.Init(const AFileName: String; Mode: Int16)', @_LapeFileStream_Init);

    addClass('TCustomMemoryStream', 'TStream');
    addGlobalFunc('function TCustomMemoryStream.Seek(const Offset: Integer; Origin: TSeekOrigin): Int64;', @_LapeCustomMemoryStream_Seek);
    addGlobalFunc('procedure TCustomMemoryStream.SaveToStream(Stream: TStream);', @_LapeCustomMemoryStream_SaveToStream);
    addGlobalFunc('procedure TCustomMemoryStream.SaveToFile(const FileName: String);', @_LapeCustomMemoryStream_SaveToFile);
    addClassVar('TCustomMemoryStream', 'Memory', 'Pointer', @_LapeCustomMemoryStream_Memory_Read);
    addGlobalFunc('procedure TCustomMemoryStream.Init; override', @_LapeCustomMemoryStream_Init);

    addClass('TMemoryStream', 'TCustomMemoryStream');
    addGlobalFunc('procedure TMemoryStream.Clear;', @_LapeMemoryStream_Clear);
    addGlobalFunc('procedure TMemoryStream.LoadFromStream(Stream: TStream);', @_LapeMemoryStream_LoadFromStream);
    addGlobalFunc('procedure TMemoryStream.LoadFromFile(const FileName: String);', @_LapeMemoryStream_LoadFromFile);
    addGlobalFunc('procedure TMemoryStream.SetSize(NewSize: PtrInt);', @_LapeMemoryStream_SetSize);
    addGlobalFunc('procedure TMemoryStream.Init; override', @_LapeMemoryStream_Init);

    addClass('TStringStream', 'TStream');
    addClassVar('TStringStream', 'DataString', 'String', @_LapeStringStream_DataString_Read);
    addGlobalFunc('procedure TStringStream.Init(const AString: String)', @_LapeStringStream_Init);
    addGlobalFunc('function TStringStream.ReadString(Count: Integer): String;', @_LapeStringStream_ReadString);
    addGlobalFunc('procedure TStringStream.WriteString(const AString: String);', @_LapeStringStream_WriteString);

    addClass('TStrings', 'TPersistent');
    addGlobalFunc('function TStrings.Add(const S: String): Integer;', @_LapeStrings_Add);
    addGlobalFunc('function TStrings.AddObject(const S: String; AObject: TObject): Integer;', @_LapeStrings_AddObject);
    addGlobalFunc('procedure TStrings.Append(const S: String);', @_LapeStrings_Append);
    addGlobalFunc('procedure TStrings.AddStrings(const TheStrings: TStrings); overload', @_LapeStrings_AddStrings);
    addGlobalFunc('procedure TStrings.AddStrings(const TheStrings: TStringArray); overload', @_LapeStrings_AddStringsArray);
    addGlobalFunc('procedure TStrings.BeginUpdate;', @_LapeStrings_BeginUpdate);
    addGlobalFunc('procedure TStrings.Clear;', @_LapeStrings_Clear);
    addGlobalFunc('procedure TStrings.Delete(Index: Integer);', @_LapeStrings_Delete);
    addGlobalFunc('procedure TStrings.EndUpdate;', @_LapeStrings_EndUpdate);
    addGlobalFunc('function TStrings.Equals(TheStrings: TStrings): Boolean;', @_LapeStrings_Equals);
    addGlobalFunc('procedure TStrings.Exchange(Index1, Index2: Integer);', @_LapeStrings_Exchange);
    addGlobalFunc('function TStrings.IndexOf(const S: String): Integer;', @_LapeStrings_IndexOf);
    addGlobalFunc('function TStrings.IndexOfName(const Name: String): Integer;', @_LapeStrings_IndexOfName);
    addGlobalFunc('function TStrings.IndexOfObject(AObject: TObject): Integer;', @_LapeStrings_IndexOfObject);
    addGlobalFunc('procedure TStrings.Insert(Index: Integer; const S: String);', @_LapeStrings_Insert);
    addGlobalFunc('procedure TStrings.InsertObject(Index: Integer; const S: String; AObject: TObject);', @_LapeStrings_InsertObject);
    addGlobalFunc('procedure TStrings.LoadFromFile(const FileName: String);', @_LapeStrings_LoadFromFile);
    addGlobalFunc('procedure TStrings.LoadFromStream(Stream: TStream);', @_LapeStrings_LoadFromStream);
    addGlobalFunc('procedure TStrings.Move(FromIndex, ToIndex: Integer);', @_LapeStrings_Move);
    addGlobalFunc('procedure TStrings.SaveToFile(const FileName: String);', @_LapeStrings_SaveToFile);
    addGlobalFunc('procedure TStrings.SaveToStream(Stream: TStream);', @_LapeStrings_SaveToStream);
    addGlobalFunc('function TStrings.ToStringArray: TStringArray;', @_LapeStrings_ToStringArray);
    addClassVar('TStrings', 'Count', 'Integer', @_LapeStrings_Count_Read);
    addClassVar('TStrings', 'Objects', 'TObject', @_LapeStrings_Objects_Read, @_LapeStrings_Objects_Write, True);
    addClassVar('TStrings', 'Values', 'String', @_LapeStrings_Values_Read, @_LapeStrings_Values_Write, True, 'String');
    addClassVar('TStrings', 'Strings', 'String', @_LapeStrings_Strings_Read, @_LapeStrings_Strings_Write, True);
    addClassVar('TStrings', 'Names', 'String', @_LapeStrings_Names_Read, nil, True);
    addClassVar('TStrings', 'ValueFromIndex', 'String', @_LapeStrings_ValueFromIndex_Read, @_LapeStrings_ValueFromIndex_Write, True);
    addClassVar('TStrings', 'Text', 'String', @_LapeStrings_Text_Read, @_LapeStrings_Text_Write);
    addGlobalFunc('procedure TStrings.Init; override', @_LapeStrings_Init);

    addClass('TStringList', 'TStrings');

    addGlobalType('function(List: TStringList; Index1, Index2: Integer): Integer', 'TStringListSortCompare', FFI_DEFAULT_ABI);
    addClassVar('TStringList', 'Sorted', 'Boolean', @_LapeStringList_Sorted_Read, @_LapeStringList_Sorted_Write);
    addClassVar('TStringList', 'CaseSensitive', 'Boolean', @_LapeStringList_CaseSensitive_Read, @_LapeStringList_CaseSensitive_Write);
    addClassVar('TStringList', 'OnChange', 'TNotifyEvent', @_LapeStringList_OnChange_Read, @_LapeStringList_OnChange_Write);
    addClassVar('TStringList', 'OnChanging', 'TNotifyEvent', @_LapeStringList_OnChanging_Read, @_LapeStringList_OnChanging_Write);
    addClassVar('TStringList', 'OwnsObjects', 'boolean', @_LapeStringList_OwnsObjects_Read, @_LapeStringList_OwnsObjects_Write);

    addGlobalFunc('procedure TStringList.Init; override', @_LapeStringList_Init);
    addGlobalFunc('function TStringList.Find(const S: String; Out Index: Integer): Boolean;', @_LapeStringList_Find);
    addGlobalFunc('procedure TStringList.Sort;', @_LapeStringList_Sort);
    addGlobalFunc('procedure TStringList.CustomSort(CompareFn: TStringListSortCompare);', @_LapeStringList_CustomSort);
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportLCLSystem);

end.

