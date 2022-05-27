unit simba.import_class_xml;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.xmlparser;

type
  PStream = ^TStream;

procedure _LapeXmlNode_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^ := TXmlNode.Create();
end;

procedure _LapeXmlNode_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PString(Params^[1])^);
end;

procedure _LapeXmlNode_GetParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Parent;
end;

procedure _LapeXmlNode_GetNodeName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PXmlNode(Params^[0])^.NodeName;
end;

procedure _LapeXmlNode_GetText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PXmlNode(Params^[0])^.Text;
end;

procedure _LapeXmlNode_SetNodeName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^.NodeName := PString(Params^[1])^;
end;

procedure _LapeXmlNode_SetParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^.Parent := PXMLNode(Params^[1])^;
end;

procedure _LapeXmlNode_GetChildNodes(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Result)^ := PXmlNode(Params^[0])^.ChildNodes;
end;

procedure _LapeXmlNode_FindEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeXmlNode_FindExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeXmlNode_FindNodes(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNodeList(Result)^ := PXmlNode(Params^[0])^.FindNodes(PString(Params^[1])^);
end;

procedure _LapeXmlNode_HasAttribute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PXmlNode(Params^[0])^.HasAttribute(PString(Params^[1])^);
end;

procedure _LapeXmlNode_AddChild(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.AddChild(PString(Params^[1])^);
end;

procedure _LapeXmlNode_SetText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.SetText(PString(Params^[1])^);
end;

procedure _LapeXmlNode_SetAttribute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.SetAttribute(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeXmlNode_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^.Free();
end;

procedure _LapeXmlNode_GetAttribute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PXmlNode(Params^[0])^[PString(Params^[1])^];
end;

procedure _LapeXMLNodeList_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^ := TXMLNodeList.Create();
end;

procedure _LapeXMLNodeList_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Clear();
end;

procedure _LapeXMLNodeList_Assign(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Assign(PXMLNodeList(Params^[1])^);
end;

procedure _LapeXMLNodeList_Add(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Add(PXMLNode(Params^[1])^);
end;

procedure _LapeXMLNodeList_AddEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Add(PXMLNodeList(Params^[1])^);
end;

procedure _LapeXMLNodeList_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.IndexOf(PXMLNode(Params^[1])^);
end;

procedure _LapeXMLNodeList_IndexOfEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.IndexOf(PString(Params^[1])^);
end;

procedure _LapeXMLNodeList_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Delete(PXMLNode(Params^[1])^);
end;

procedure _LapeXMLNodeList_DeleteEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeXMLNodeList_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.Count;
end;

procedure _LapeXMLNodeList_Item(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNode(Result)^ := PXMLNodeList(Params^[0])^.Item[PInteger(Params^[1])^];
end;

procedure _LapeXMLNodeList_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNodeList(Params^[0])^.Free();
end;

procedure _LapeVerySimpleXml_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^ := TVerySimpleXml.Create();
end;

procedure _LapeVerySimpleXml_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Clear();
end;

procedure _LapeVerySimpleXml_LoadFromFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.LoadFromFile(PString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeVerySimpleXml_LoadFromString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.LoadFromString(PString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeVerySimpleXml_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.SaveToFile(PString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_SaveToString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.SaveToString(PString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_Root_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNode(Result)^ := PVerySimpleXml(Params^[0])^.Root;
end;

procedure _LapeVerySimpleXml_Root_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Root := PXMLNode(Params^[1])^;
end;

procedure _LapeVerySimpleXml_Header_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXMLNode(Result)^ := PVerySimpleXml(Params^[0])^.Header;
end;

procedure _LapeVerySimpleXml_Header_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Header := PXMLNode(Params^[1])^;
end;

procedure _LapeVerySimpleXml_Ident_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PVerySimpleXml(Params^[0])^.Ident;
end;

procedure _LapeVerySimpleXml_Ident_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Ident := PString(Params^[1])^;
end;

procedure _LapeVerySimpleXml_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Free();
end;

procedure ImportXML(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Classes');

    addClass('TXmlNode', 'TObject');
    addClass('TXMLNodeList', 'TObject');
    addGlobalFunc('procedure TXmlNode.Init()', @_LapeXmlNode_Init);
    addGlobalFunc('function TXmlNode.GetChildNodes: TXMLNodeList;', @_LapeXMLNode_GetChildNodes);
    addGlobalFunc('function TXmlNode.GetNodeName: string;', @_LapeXmlNode_GetNodeName);
    addGlobalFunc('function TXmlNode.GetText: string;', @_LapeXmlNode_GetText);
    addGlobalFunc('procedure TXmlNode.SetNodeName(const Name: string);', @_LapeXmlNode_SetNodeName);
    addGlobalFunc('procedure TXmlNode.SetParent(const Node: TXMLNode);', @_LapeXmlNode_SetParent);
    addGlobalFunc('function TXmlNode.GetAttribute(const Name: string): string;', @_LapeXmlNode_GetAttribute);
    addGlobalFunc('function TXmlNode.GetParent: TXMLNode;', @_LapeXmlNode_GetParent);
    addGlobalFunc('function TXmlNode.Find(Name: String): TXmlNode; overload', @_LapeXmlNode_Find);
    addGlobalFunc('function TXmlNode.Find(Name, Attribute: String): TXmlNode; overload', @_LapeXmlNode_FindEx);
    addGlobalFunc('function TXmlNode.Find(Name, Attribute, Value: String): TXmlNode; overload', @_LapeXmlNode_FindExEx);
    addGlobalFunc('function TXmlNode.FindNodes(Name: String): TXmlNodeList;', @_LapeXmlNode_FindNodes);
    addGlobalFunc('function TXmlNode.HasAttribute(const Name: String): Boolean;', @_LapeXmlNode_HasAttribute);
    addGlobalFunc('function TXmlNode.AddChild(const Name: String): TXmlNode;', @_LapeXmlNode_AddChild);
    addGlobalFunc('function TXmlNode.SetText(Value: String): TXmlNode;', @_LapeXmlNode_SetText);
    addGlobalFunc('function TXmlNode.SetAttribute(AttrName: String;Value: String): TXmlNode;', @_LapeXmlNode_SetAttribute);
    //addGlobalFunc('procedure TXmlNode.Free;', @_LapeXmlNode_Free);

    addGlobalFunc('procedure TXMLNodeList.Init()', @_LapeXMLNodeList_Init);
    addGlobalFunc('procedure TXMLNodeList.Clear;', @_LapeXMLNodeList_Clear);
    addGlobalFunc('procedure TXMLNodeList.Assign(Src: TXMLNodeList);', @_LapeXMLNodeList_Assign);
    addGlobalFunc('procedure TXMLNodeList.Add(aXMLNode: TXMLNode); overload', @_LapeXMLNodeList_Add);
    addGlobalFunc('procedure TXMLNodeList.Add(aXMLNodes: TXMLNodeList); overload', @_LapeXMLNodeList_AddEx);
    addGlobalFunc('function TXMLNodeList.IndexOf(aXMLNode: TXMLNode): Integer; overload', @_LapeXMLNodeList_IndexOf);
    addGlobalFunc('function TXMLNodeList.IndexOf(NodeName: string): Integer; overload', @_LapeXMLNodeList_IndexOfEx);
    addGlobalFunc('procedure TXMLNodeList.Delete(aXMLNode: TXMLNode); overload', @_LapeXMLNodeList_Delete);
    addGlobalFunc('procedure TXMLNodeList.Delete(Index: Integer); overload', @_LapeXMLNodeList_DeleteEx);
    addClassVar('TXMLNodeList', 'Count', 'Integer', @_LapeXMLNodeList_Count_Read, nil);
    addGlobalFunc('function TXMLNodeList.Item(Index: Integer): TXMLNode;', @_LapeXMLNodeList_Item);
    //addGlobalFunc('procedure TXMLNodeList.Free;', @_LapeXMLNodeList_Free);

    addClass('TXml', 'TObject');
    addGlobalFunc('procedure TXml.Init()', @_LapeVerySimpleXml_Init);
    addGlobalFunc('procedure TXml.Clear()', @_LapeVerySimpleXml_Clear);
    addGlobalFunc('procedure TXml.LoadFromFile(const FileName: String)', @_LapeVerySimpleXml_LoadFromFile);
    addGlobalFunc('procedure TXml.LoadFromStream(const Stream: TStream)', @_LapeVerySimpleXml_LoadFromStream);
    addGlobalFunc('procedure TXml.LoadFromString(const Str: String)', @_LapeVerySimpleXml_LoadFromString);
    addGlobalFunc('procedure TXml.SaveToStream(const Stream: TStream)', @_LapeVerySimpleXml_SaveToStream);
    addGlobalFunc('procedure TXml.SaveToFile(const FileName: String)', @_LapeVerySimpleXml_SaveToFile);
    addGlobalFunc('procedure TXml.SaveToString(var Str: String)', @_LapeVerySimpleXml_SaveToString);
    addClassVar('TXml', 'Root', 'TXMLNode', @_LapeVerySimpleXml_Root_Read, @_LapeVerySimpleXml_Root_Write);
    addClassVar('TXml', 'Header', 'TXMLNode', @_LapeVerySimpleXml_Header_Read, @_LapeVerySimpleXml_Header_Write);
    addClassVar('TXml', 'Ident', 'string', @_LapeVerySimpleXml_Ident_Read, @_LapeVerySimpleXml_Ident_Write);
    //addGlobalFunc('procedure TXml.Free()', @_LapeVerySimpleXml_Free);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportXML);

end.

