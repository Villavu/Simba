unit simba.import_lcl_system;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportLCLSystem(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi;

type
  PObject = ^TObject;
  PComponent = ^TComponent;
  PComponentName = ^TComponentName;
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

procedure _LapeObject_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PObject(Params^[0])^.Free();
end;

procedure _LapeObject_ClassName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PObject(Params^[0])^.ClassName;
end;

procedure _LapeStream_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStream(Params^[0])^.Read(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStream(Params^[0])^.Write(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_Seek(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStream(Params^[0])^.Seek(PInteger(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeStream_ReadBuffer(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.ReadBuffer(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_WriteBuffer(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.WriteBuffer(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStream_CopyFrom(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PStream(Params^[0])^.CopyFrom(PStream(Params^[1])^, PInt64(Params^[2])^);
end;

procedure _LapeStream_ReadByte(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  pbyte(Result)^ := PStream(Params^[0])^.ReadByte();
end;

procedure _LapeStream_ReadDWord(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PStream(Params^[0])^.ReadDWord();
end;

procedure _LapeStream_ReadAnsiString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStream(Params^[0])^.ReadAnsiString();
end;

procedure _LapeStream_WriteByte(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.WriteByte(pbyte(Params^[1])^);
end;

procedure _LapeStream_WriteWord(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.WriteWord(PWord(Params^[1])^);
end;

procedure _LapeStream_WriteDWord(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.WriteDWord(PCardinal(Params^[1])^);
end;

procedure _LapeStream_WriteAnsiString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.WriteAnsiString(PString(Params^[1])^);
end;

procedure _LapeStream_Position_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PStream(Params^[0])^.Position;
end;

procedure _LapeStream_Position_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.Position := PInt64(Params^[1])^;
end;

procedure _LapeStream_Size_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PStream(Params^[0])^.Size;
end;

procedure _LapeStream_Size_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Params^[0])^.Size := PInt64(Params^[1])^;
end;

procedure _LapeStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStream(Result)^ := TStream.Create();
end;

procedure _LapeHandleStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandleStream(Result)^ := THandleStream.Create(PHandle(Params^[0])^);
end;

procedure _LapeHandleStream_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PHandleStream(Params^[0])^.Read(Ppointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeHandleStream_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PHandleStream(Params^[0])^.Write(Ppointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeHandleStream_Seek(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PHandleStream(Params^[0])^.Seek(PInt64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeHandleStream_Handle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PHandleStream(Params^[0])^.Handle;
end;

procedure _LapeFileStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFileStream(Result)^ := TFileStream.Create(PString(Params^[0])^, PWord(Params^[1])^);
end;

procedure _LapeFileStream_FileName_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PFileStream(Params^[0])^.FileName;
end;

procedure _LapeCustomMemoryStream_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomMemoryStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomMemoryStream_Seek(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInt64(Result)^ := PCustomMemoryStream(Params^[0])^.Seek(PInteger(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeCustomMemoryStream_SaveToStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemoryStream(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeCustomMemoryStream_SaveToFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemoryStream(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeCustomMemoryStream_Memory_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointer(Result)^ := PCustomMemoryStream(Params^[0])^.Memory;
end;

procedure _LapeCustomMemoryStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemoryStream(Result)^ := TCustomMemoryStream.Create();
end;

procedure _LapeMemoryStream_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoryStream(Params^[0])^.Clear();
end;

procedure _LapeMemoryStream_LoadFromStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoryStream(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeMemoryStream_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoryStream(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeMemoryStream_SetSize(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoryStream(Params^[0])^.SetSize(PPtrInt(Params^[1])^);
end;

procedure _LapeMemoryStream_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMemoryStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeMemoryStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoryStream(Result)^ := TMemoryStream.Create();
end;

procedure _LapeStringStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringStream(Result)^ := TStringStream.Create(PString(Params^[0])^);
end;

procedure _LapeStringStream_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringStream_ReadString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringStream(Params^[0])^.ReadString(PInteger(Params^[1])^);
end;

procedure _LapeStringStream_Seek(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringStream(Params^[0])^.Seek(PInteger(Params^[1])^, PWord(Params^[2])^);
end;

procedure _LapeStringStream_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringStream_WriteString(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringStream(Params^[0])^.WriteString(PString(Params^[1])^);
end;

procedure _LapeStringStream_DataString_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStringStream(Params^[0])^.DataString;
end;

procedure _LapeStrings_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Add(PString(Params^[1])^);
end;

procedure _LapeStrings_AddObject(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStrings(Params^[0])^.AddObject(PString(Params^[1])^, PObject(Params^[2])^);
end;

procedure _LapeStrings_Append(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Append(PString(Params^[1])^);
end;

procedure _LapeStrings_AddStrings(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.AddStrings(PStrings(Params^[1])^);
end;

procedure _LapeStrings_AddStringsArray(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.AddStrings(PStringArray(Params^[1])^);
end;

procedure _LapeStrings_Assign(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Assign(PPersistent(Params^[1])^);
end;

procedure _LapeStrings_BeginUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.BeginUpdate();
end;

procedure _LapeStrings_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Clear();
end;

procedure _LapeStrings_Delete(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStrings_EndUpdate(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.EndUpdate();
end;

procedure _LapeStrings_Equals(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStrings(Params^[0])^.Equals(PStrings(Params^[1])^);
end;

procedure _LapeStrings_Exchange(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStrings_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStrings_IndexOfName(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfName(PString(Params^[1])^);
end;

procedure _LapeStrings_IndexOfObject(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStrings(Params^[0])^.IndexOfObject(PObject(Params^[1])^);
end;

procedure _LapeStrings_Insert(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Insert(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStrings_InsertObject(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.InsertObject(PInteger(Params^[1])^, PString(Params^[2])^, PObject(Params^[3])^);
end;

procedure _LapeStrings_LoadFromFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeStrings_LoadFromStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeStrings_Move(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Move(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStrings_SaveToFile(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeStrings_SaveToStream(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeStrings_ToStringArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringArray(Result)^ := PStrings(Params^[0])^.ToStringArray;
end;

procedure _LapeStrings_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStrings(Params^[0])^.Count;
end;

procedure _LapeStrings_Objects_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PObject(Result)^ := PStrings(Params^[0])^.Objects[PInteger(Params^[1])^];
end;

procedure _LapeStrings_Objects_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Objects[PInteger(Params^[1])^] := PObject(Params^[2])^;
end;

procedure _LapeStrings_Strings_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStrings(Params^[0])^.Strings[PInteger(Params^[1])^];
end;

procedure _LapeStrings_Strings_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Strings[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStrings_Values_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStrings(Params^[0])^.Values[PString(Params^[1])^];
end;

procedure _LapeStrings_Values_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Values[PString(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStrings_Names_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStrings(Params^[0])^.Names[PInteger(Params^[1])^];
end;

procedure _LapeStrings_ValueFromIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStrings(Params^[0])^.ValueFromIndex[PInteger(Params^[1])^];
end;

procedure _LapeStrings_ValueFromIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.ValueFromIndex[PInteger(Params^[1])^] := PString(Params^[2])^;
end;

procedure _LapeStrings_Text_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PStrings(Params^[0])^.Text;
end;

procedure _LapeStrings_Text_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Params^[0])^.Text := PString(Params^[1])^;
end;

procedure _LapeStringList_Add(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringList(Params^[0])^.Add(PString(Params^[1])^);
end;

procedure _LapeStringList_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.Clear();
end;

procedure _LapeStringList_Delete(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeStringList_Exchange(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.Exchange(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringList_Find(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Find(PString(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeStringList_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PStringList(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeStringList_Insert(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.Insert(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeStringList_Sort(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.Sort();
end;

procedure _LapeStringList_CustomSort(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.CustomSort(PStringListSortCompare(Params^[1])^);
end;

procedure _LapeStringList_Sorted_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.Sorted;
end;

procedure _LapeStringList_Sorted_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeStringList_CaseSensitive_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PStringList(Params^[0])^.CaseSensitive;
end;

procedure _LapeStringList_CaseSensitive_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.CaseSensitive := PBoolean(Params^[1])^;
end;

procedure _LapeStringList_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChange;
end;

procedure _LapeStringList_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeStringList_OnChanging_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PStringList(Params^[0])^.OnChanging;
end;

procedure _LapeStringList_OnChanging_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.OnChanging := PNotifyEvent(Params^[1])^;
end;

procedure _LapeStringList_OwnsObjects_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PStringList(Params^[0])^.OwnsObjects;
end;

procedure _LapeStringList_OwnsObjects_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Params^[0])^.OwnsObjects := Pboolean(Params^[1])^;
end;

procedure _LapeStringList_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStringList(Result)^ := TStringList.Create();
end;

procedure _LapeComponent_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Result)^ := TComponent.Create(PComponent(Params^[0])^);
end;

procedure _LapeComponent_FindComponent(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Result)^ := PComponent(Params^[0])^.FindComponent(PString(Params^[1])^);
end;

procedure _LapeComponent_InsertComponent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Params^[0])^.InsertComponent(PComponent(Params^[1])^);
end;

procedure _LapeComponent_RemoveComponent(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Params^[0])^.RemoveComponent(PComponent(Params^[1])^);
end;

procedure _LapeComponent_Components(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Result)^ := PComponent(Params^[0])^.Components[PInteger(Params^[1])^];
end;

procedure _LapeComponent_ComponentCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentCount;
end;

procedure _LapeComponent_ComponentIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PComponent(Params^[0])^.ComponentIndex;
end;

procedure _LapeComponent_ComponentIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Params^[0])^.ComponentIndex := PInteger(Params^[1])^;
end;

procedure _LapeComponent_Owner_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Result)^ := PComponent(Params^[0])^.Owner;
end;

procedure _LapeComponent_Name_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponentName(Result)^ := PComponent(Params^[0])^.Name;
end;

procedure _LapeComponent_Name_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Params^[0])^.Name := PComponentName(Params^[1])^;
end;

procedure _LapeComponent_Tag_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPtrInt(Result)^ := PComponent(Params^[0])^.Tag;
end;

procedure _LapeComponent_Tag_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Params^[0])^.Tag := PPtrInt(Params^[1])^;
end;

procedure ImportLCLSystem(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TObject', 'Pointer');

    addGlobalFunc('procedure TObject.Free;', @_LapeObject_Free);
    addGlobalFunc('function TObject.ClassName: String;', @_LapeObject_ClassName);

    addGlobalType('procedure(Sender: TObject) of object', 'TLazNotifyEvent', FFI_DEFAULT_ABI);
    addGlobalType('PtrUInt', 'TLazHandle');
    addGlobalType('(soBeginning, soCurrent, soEnd)', 'TLazSeekOrigin');

    addClass('TLazComponent');
    addClassConstructor('TLazComponent', '(AOwner: TLazComponent)', @_LapeComponent_Create);
    addGlobalFunc('function TLazComponent.FindComponent(const AName: String): TLazComponent;', @_LapeComponent_FindComponent);
    addGlobalFunc('procedure TLazComponent.InsertComponent(AComponent: TLazComponent);', @_LapeComponent_InsertComponent);
    addGlobalFunc('procedure TLazComponent.RemoveComponent(AComponent: TLazComponent);', @_LapeComponent_RemoveComponent);
    addGlobalFunc('function TLazComponent.GetComponent(index: Integer): TLazComponent;', @_LapeComponent_Components);
    addClassVar('TLazComponent', 'ComponentCount', 'Integer', @_LapeComponent_ComponentCount_Read);
    addClassVar('TLazComponent', 'ComponentIndex', 'Integer', @_LapeComponent_ComponentIndex_Read, @_LapeComponent_ComponentIndex_Write);
    addClassVar('TLazComponent', 'Owner', 'TLazComponent', @_LapeComponent_Owner_Read);
    addClassVar('TLazComponent', 'Name', 'String', @_LapeComponent_Name_Read, @_LapeComponent_Name_Write);
    addClassVar('TLazComponent', 'Tag', 'PtrInt', @_LapeComponent_Tag_Read, @_LapeComponent_Tag_Write);

    addClass('TLazStream');
    addGlobalFunc('function TLazStream.Read(var Buffer; Count: Integer): Integer;', @_LapeStream_Read);
    addGlobalFunc('function TLazStream.Write(constref Buffer; Count: Integer): Integer;', @_LapeStream_Write);
    addGlobalFunc('function TLazStream.Seek(Offset: Integer; Origin: TLazSeekOrigin): Integer;', @_LapeStream_Seek);
    addGlobalFunc('procedure TLazStream.ReadBuffer(var Buffer; Count: Integer);', @_LapeStream_ReadBuffer);
    addGlobalFunc('procedure TLazStream.WriteBuffer(constref Buffer; Count: Integer);', @_LapeStream_WriteBuffer);
    addGlobalFunc('function TLazStream.CopyFrom(Source: TLazStream; Count: Int64): Int64;', @_LapeStream_CopyFrom);
    addGlobalFunc('function TLazStream.ReadByte: Byte;', @_LapeStream_ReadByte);
    addGlobalFunc('function TLazStream.ReadDWord: UInt32;', @_LapeStream_ReadDWord);
    addGlobalFunc('function TLazStream.ReadAnsiString: String;', @_LapeStream_ReadAnsiString);
    addGlobalFunc('procedure TLazStream.WriteByte(b: Byte);', @_LapeStream_WriteByte);
    addGlobalFunc('procedure TLazStream.WriteWord(w: Int16);', @_LapeStream_WriteWord);
    addGlobalFunc('procedure TLazStream.WriteDWord(d: UInt32);', @_LapeStream_WriteDWord);
    addGlobalFunc('procedure TLazStream.WriteAnsiString(const S: String);', @_LapeStream_WriteAnsiString);
    addClassVar('TLazStream', 'Position', 'Integer', @_LapeStream_Position_Read, @_LapeStream_Position_Write);
    addClassVar('TLazStream', 'Size', 'Integer', @_LapeStream_Size_Read, @_LapeStream_Size_Write);
    addClassConstructor('TLazStream', '(AOwner: TLazComponent)', @_LapeStream_Create);

    addClass('TLazHandleStream', 'TLazStream');
    addClassVar('TLazHandleStream', 'Handle', 'TLazHandle', @_LapeHandleStream_Handle_Read);
    addClassConstructor('TLazHandleStream', '(AHandle: TLazHandle)', @_LapeHandleStream_Create);

    addClass('TLazFileStream', 'TLazHandleStream');
    addClassVar('TLazFileStream', 'FileName', 'String', @_LapeFileStream_FileName_Read);
    addClassConstructor('TLazFileStream', '(const AFileName: String; Mode: Int16)', @_LapeFileStream_Create);

    addClass('TLazCustomMemoryStream', 'TLazStream');
    addGlobalFunc('function TLazCustomMemoryStream.Seek(const Offset: Integer; Origin: TLazSeekOrigin): Int64;', @_LapeCustomMemoryStream_Seek);
    addGlobalFunc('procedure TLazCustomMemoryStream.SaveToStream(Stream: TLazStream);', @_LapeCustomMemoryStream_SaveToStream);
    addGlobalFunc('procedure TLazCustomMemoryStream.SaveToFile(const FileName: String);', @_LapeCustomMemoryStream_SaveToFile);
    addClassVar('TLazCustomMemoryStream', 'Memory', 'Pointer', @_LapeCustomMemoryStream_Memory_Read);
    addClassConstructor('TLazCustomMemoryStream', '', @_LapeCustomMemoryStream_Create);

    addClass('TLazMemoryStream', 'TLazCustomMemoryStream');
    addGlobalFunc('procedure TLazMemoryStream.Clear;', @_LapeMemoryStream_Clear);
    addGlobalFunc('procedure TLazMemoryStream.LoadFromStream(Stream: TLazStream);', @_LapeMemoryStream_LoadFromStream);
    addGlobalFunc('procedure TLazMemoryStream.LoadFromFile(const FileName: String);', @_LapeMemoryStream_LoadFromFile);
    addGlobalFunc('procedure TLazMemoryStream.SetSize(NewSize: PtrInt);', @_LapeMemoryStream_SetSize);
    addClassConstructor('TLazMemoryStream', '', @_LapeMemoryStream_Create);

    addClass('TLazStringStream', 'TLazStream');
    addClassVar('TLazStringStream', 'DataString', 'String', @_LapeStringStream_DataString_Read);
    addClassConstructor('TLazStringStream', '(const AString: String)', @_LapeStringStream_Create);
    addGlobalFunc('function TLazStringStream.ReadString(Count: Integer): String;', @_LapeStringStream_ReadString);
    addGlobalFunc('procedure TLazStringStream.WriteString(const AString: String);', @_LapeStringStream_WriteString);

    addClass('TLazStrings');
    addGlobalFunc('function TLazStrings.Add(const S: String): Integer;', @_LapeStrings_Add);
    addGlobalFunc('function TLazStrings.AddObject(const S: String; AObject: TObject): Integer;', @_LapeStrings_AddObject);
    addGlobalFunc('procedure TLazStrings.Append(const S: String);', @_LapeStrings_Append);
    addGlobalFunc('procedure TLazStrings.AddStrings(const TheStrings: TLazStrings); overload', @_LapeStrings_AddStrings);
    addGlobalFunc('procedure TLazStrings.AddStrings(const TheStrings: TStringArray); overload', @_LapeStrings_AddStringsArray);
    addGlobalFunc('procedure TLazStrings.BeginUpdate;', @_LapeStrings_BeginUpdate);
    addGlobalFunc('procedure TLazStrings.Clear;', @_LapeStrings_Clear);
    addGlobalFunc('procedure TLazStrings.Delete(Index: Integer);', @_LapeStrings_Delete);
    addGlobalFunc('procedure TLazStrings.EndUpdate;', @_LapeStrings_EndUpdate);
    addGlobalFunc('function TLazStrings.Equals(TheStrings: TLazStrings): Boolean;', @_LapeStrings_Equals);
    addGlobalFunc('procedure TLazStrings.Exchange(Index1, Index2: Integer);', @_LapeStrings_Exchange);
    addGlobalFunc('function TLazStrings.IndexOf(const S: String): Integer;', @_LapeStrings_IndexOf);
    addGlobalFunc('function TLazStrings.IndexOfName(const Name: String): Integer;', @_LapeStrings_IndexOfName);
    addGlobalFunc('function TLazStrings.IndexOfObject(AObject: TObject): Integer;', @_LapeStrings_IndexOfObject);
    addGlobalFunc('procedure TLazStrings.Insert(Index: Integer; const S: String);', @_LapeStrings_Insert);
    addGlobalFunc('procedure TLazStrings.InsertObject(Index: Integer; const S: String; AObject: TObject);', @_LapeStrings_InsertObject);
    addGlobalFunc('procedure TLazStrings.LoadFromFile(const FileName: String);', @_LapeStrings_LoadFromFile);
    addGlobalFunc('procedure TLazStrings.LoadFromStream(Stream: TLazStream);', @_LapeStrings_LoadFromStream);
    addGlobalFunc('procedure TLazStrings.Move(FromIndex, ToIndex: Integer);', @_LapeStrings_Move);
    addGlobalFunc('procedure TLazStrings.SaveToFile(const FileName: String);', @_LapeStrings_SaveToFile);
    addGlobalFunc('procedure TLazStrings.SaveToStream(Stream: TLazStream);', @_LapeStrings_SaveToStream);
    addGlobalFunc('function TLazStrings.ToStringArray: TStringArray;', @_LapeStrings_ToStringArray);
    addClassVar('TLazStrings', 'Count', 'Integer', @_LapeStrings_Count_Read);
    addClassVar('TLazStrings', 'Objects', 'TObject', @_LapeStrings_Objects_Read, @_LapeStrings_Objects_Write, True);
    addClassVar('TLazStrings', 'Values', 'String', @_LapeStrings_Values_Read, @_LapeStrings_Values_Write, True, 'String');
    addClassVar('TLazStrings', 'Strings', 'String', @_LapeStrings_Strings_Read, @_LapeStrings_Strings_Write, True);
    addClassVar('TLazStrings', 'Names', 'String', @_LapeStrings_Names_Read, nil, True);
    addClassVar('TLazStrings', 'ValueFromIndex', 'String', @_LapeStrings_ValueFromIndex_Read, @_LapeStrings_ValueFromIndex_Write, True);
    addClassVar('TLazStrings', 'Text', 'String', @_LapeStrings_Text_Read, @_LapeStrings_Text_Write);

    addClass('TLazStringList', 'TLazStrings');

    addGlobalType('function(List: TLazStringList; Index1, Index2: Integer): Integer', 'TLazStringListSortCompare', FFI_DEFAULT_ABI);
    addClassVar('TLazStringList', 'Sorted', 'Boolean', @_LapeStringList_Sorted_Read, @_LapeStringList_Sorted_Write);
    addClassVar('TLazStringList', 'CaseSensitive', 'Boolean', @_LapeStringList_CaseSensitive_Read, @_LapeStringList_CaseSensitive_Write);
    addClassVar('TLazStringList', 'OnChange', 'TLazNotifyEvent', @_LapeStringList_OnChange_Read, @_LapeStringList_OnChange_Write);
    addClassVar('TLazStringList', 'OnChanging', 'TLazNotifyEvent', @_LapeStringList_OnChanging_Read, @_LapeStringList_OnChanging_Write);
    addClassVar('TLazStringList', 'OwnsObjects', 'boolean', @_LapeStringList_OwnsObjects_Read, @_LapeStringList_OwnsObjects_Write);

    addClassConstructor('TLazStringList', '', @_LapeStringList_Create);
    addGlobalFunc('function TLazStringList.Find(const S: String; Out Index: Integer): Boolean;', @_LapeStringList_Find);
    addGlobalFunc('procedure TLazStringList.Sort;', @_LapeStringList_Sort);
    addGlobalFunc('procedure TLazStringList.CustomSort(CompareFunc: TLazStringListSortCompare);', @_LapeStringList_CustomSort);
  end;
end;

end.

