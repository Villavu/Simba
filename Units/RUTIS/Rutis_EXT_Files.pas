Unit Rutis_EXT_Files;

{$ifdef FPC}
  {$mode objfpc}
  {$H+}
{$endif}

Interface

Uses
  Windows, Classes, SysUtils,
  Rutis_Defs, Rutis_Classes, Rutis_Engine;

Procedure REFiles_CloseFiles;
Procedure RegisterEXTMethods(Engine : TRutisEngine);

//==============================================================================
Implementation
//==============================================================================

Type
  PObject     = ^TObject;
  PFileStream = ^TFileStream;

Var
  REFiles_Streams  : Array Of TFileStream;

//==============================================================================

Procedure REFiles_CloseFiles;
Var i  : Integer;
Begin
  For i := 0 To high(REFiles_Streams) Do
    REFiles_Streams[i].Free;
  SetLength(REFiles_Streams, 0);
End;

//==============================================================================
//==============================================================================

Procedure _StreamCreate(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PPointer(Result^.Data)^ := nil;
  SetLength(REFiles_Streams, length(REFiles_Streams) + 1);
  Try
    REFiles_Streams[high(REFiles_Streams)] := TFileStream.Create(PAnsiString(Params^[0].Data)^, PWord(Params^[1].Data)^); //fmCreate or fmShareDenyWrite
  Except
    REFiles_Streams[high(REFiles_Streams)].Free;
    SetLength(REFiles_Streams, length(REFiles_Streams) - 1);
    exit;
  End;
  PPointer(Result^.Data)^ := REFiles_Streams[high(REFiles_Streams)];
End;

Procedure _StreamClose(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var i  : Integer;
Begin
  For i := 0 To high(REFiles_Streams) Do
    If REFiles_Streams[i] = PPointer(Params^[0].Data)^ Then
    Begin
      REFiles_Streams[i].Free;
      REFiles_Streams[i] := REFiles_Streams[high(REFiles_Streams)];
      SetLength(REFiles_Streams, length(REFiles_Streams) - 1);
      exit;
    End;
End;

Procedure _StreamGetPosition(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PCardinal(Result^.Data)^ := 0;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PCardinal(Result^.Data)^ := PFileStream(Params^[0].Data)^.Position;
End;

Procedure _StreamSetPosition(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Position := PCardinal(Params^[1].Data)^;
End;

Procedure _StreamGetSize(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PCardinal(Result^.Data)^ := 0;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PCardinal(Result^.Data)^ := PFileStream(Params^[0].Data)^.Size;
End;

Procedure _StreamSetSize(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Size := PCardinal(Params^[1].Data)^;
End;

//==============================================================================

Procedure _StreamWriteByte(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := False;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Write(PByte(Params^[1].Data)^, 1);

  PBoolean(Result^.Data)^ := True;
End;

Procedure _StreamWriteWord(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := False;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Write(PWord(Params^[1].Data)^, 2);

  PBoolean(Result^.Data)^ := True;
End;

Procedure _StreamWriteDWord(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := False;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Write(PCardinal(Params^[1].Data)^, 4);

  PBoolean(Result^.Data)^ := True;
End;

Procedure _StreamWriteSingle(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := False;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Write(PCardinal(Params^[1].Data)^, 4);

  PBoolean(Result^.Data)^ := True;
End;

Procedure _StreamWriteString(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Var
  str  : Ansistring;
Begin
  PBoolean(Result^.Data)^ := False;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  str := PAnsiString(Params^[1].Data)^;
  StreamWriteString(str, PFileStream(Params^[0].Data)^);

  PBoolean(Result^.Data)^ := True;
End;

Procedure _StreamWrite(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PBoolean(Result^.Data)^ := False;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Write(PPChar(Params^[1].Data)^^, PInteger(Params^[2].Data)^);

  PBoolean(Result^.Data)^ := True;
End;

//==============================================================================

Procedure _StreamReadByte(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PByte(Result^.Data)^ := 0;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Read(PByte(Result^.Data)^, 1);
End;

Procedure _StreamReadWord(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PWord(Result^.Data)^ := 0;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Read(PWord(Result^.Data)^, 2);
End;

Procedure _StreamReadDWord(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PInteger(Result^.Data)^ := 0;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Read(PInteger(Result^.Data)^, 4);
End;

Procedure _StreamReadString(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PAnsiString(Result^.Data)^ := '';
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PAnsiString(Result^.Data)^ := StreamReadString(PFileStream(Params^[0].Data)^);
End;

Procedure _StreamRead(Params : PRutisParamInfoArray; Result : PRutisParamInfo);
Begin
  PByte(Result^.Data)^ := 0;
  If PObject(Params^[0].Data)^ = nil Then exit;
  If not (PObject(Params^[0].Data)^ is TFileStream) Then exit;

  PFileStream(Params^[0].Data)^.Read(PPChar(Params^[1].Data)^^, PInteger(Params^[2].Data)^);
End;

//==============================================================================
//==============================================================================

Procedure RegisterEXTMethods(Engine : TRutisEngine);
Begin
  Engine.RegExtMethod('StreamCreate',{$IfDef FPC}@{$EndIf}_StreamCreate, ['String', 'Word'], 'Pointer');
  Engine.RegExtMethod('StreamClose',{$IfDef FPC}@{$EndIf}_StreamClose, ['Pointer'], 'Pointer');
  Engine.RegExtMethod('StreamGetPosition',{$IfDef FPC}@{$EndIf}_StreamGetPosition, ['Pointer'], 'Cardinal');
  Engine.RegExtMethod('StreamSetPosition',{$IfDef FPC}@{$EndIf}_StreamSetPosition, ['Pointer', 'Cardinal'], '');
  Engine.RegExtMethod('StreamGetSize',{$IfDef FPC}@{$EndIf}_StreamGetSize, ['Pointer'], 'Cardinal');
  Engine.RegExtMethod('StreamSetSize',{$IfDef FPC}@{$EndIf}_StreamSetSize, ['Pointer', 'Cardinal'], '');

  Engine.RegExtMethod('StreamWriteByte',{$IfDef FPC}@{$EndIf}_StreamWriteByte, ['Pointer', 'Byte'], 'Boolean');
  Engine.RegExtMethod('StreamWriteWord',{$IfDef FPC}@{$EndIf}_StreamWriteWord, ['Pointer', 'Word'], 'Boolean');
  Engine.RegExtMethod('StreamWriteInteger',{$IfDef FPC}@{$EndIf}_StreamWriteDWord, ['Pointer', 'Integer'], 'Boolean');
  Engine.RegExtMethod('StreamWriteCardinal',{$IfDef FPC}@{$EndIf}_StreamWriteDWord, ['Pointer', 'Cardinal'], 'Boolean');
  Engine.RegExtMethod('StreamWriteSingle',{$IfDef FPC}@{$EndIf}_StreamWriteDWord, ['Pointer', 'Single'], 'Boolean');
  Engine.RegExtMethod('StreamWriteString',{$IfDef FPC}@{$EndIf}_StreamWriteString, ['Pointer', 'String'], 'Boolean');
  Engine.RegExtMethod('StreamWrite',{$IfDef FPC}@{$EndIf}_StreamWrite, ['Pointer', 'Pointer', 'Integer'], 'Boolean');

  Engine.RegExtMethod('StreamReadByte',{$IfDef FPC}@{$EndIf}_StreamReadByte, ['Pointer'], 'Byte');
  Engine.RegExtMethod('StreamReadWord',{$IfDef FPC}@{$EndIf}_StreamReadWord, ['Pointer'], 'Word');
  Engine.RegExtMethod('StreamReadInteger',{$IfDef FPC}@{$EndIf}_StreamReadDWord, ['Pointer'], 'Integer');
  Engine.RegExtMethod('StreamReadCardinal',{$IfDef FPC}@{$EndIf}_StreamReadDWord, ['Pointer'], 'Cardinal');
  Engine.RegExtMethod('StreamReadSingle',{$IfDef FPC}@{$EndIf}_StreamReadDWord, ['Pointer'], 'Single');
  Engine.RegExtMethod('StreamReadString',{$IfDef FPC}@{$EndIf}_StreamReadString, ['Pointer'], 'String');
  Engine.RegExtMethod('StreamRead',{$IfDef FPC}@{$EndIf}_StreamRead, ['Pointer', 'Pointer', 'Integer'], 'Boolean');
End;

Initialization
Finalization
  REFiles_CloseFiles;
End.

