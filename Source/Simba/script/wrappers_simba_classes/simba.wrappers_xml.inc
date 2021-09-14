procedure _LapeXmlNode_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^ := TXmlNode.Create();
end;

procedure _LapeXmlNode_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PlpString(Params^[1])^);
end;

procedure _LapeXmlNode_GetParent(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Parent;
end;

procedure _LapeXmlNode_GetNodeName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PXmlNode(Params^[0])^.NodeName;
end;

procedure _LapeXmlNode_GetText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PXmlNode(Params^[0])^.Text;
end;

procedure _LapeXmlNode_SetNodeName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^.NodeName := PLpString(Params^[1])^;
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
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure _LapeXmlNode_FindExEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PlpString(Params^[1])^, PlpString(Params^[2])^, PlpString(Params^[3])^);
end;

procedure _LapeXmlNode_FindNodes(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNodeList(Result)^ := PXmlNode(Params^[0])^.FindNodes(PlpString(Params^[1])^);
end;

procedure _LapeXmlNode_HasAttribute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PXmlNode(Params^[0])^.HasAttribute(PlpString(Params^[1])^);
end;

procedure _LapeXmlNode_AddChild(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.AddChild(PlpString(Params^[1])^);
end;

procedure _LapeXmlNode_SetText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.SetText(PlpString(Params^[1])^);
end;

procedure _LapeXmlNode_SetAttribute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.SetAttribute(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

procedure _LapeXmlNode_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PXmlNode(Params^[0])^.Free();
end;

procedure _LapeXmlNode_GetAttribute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PXmlNode(Params^[0])^[PlpString(Params^[1])^];
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
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.IndexOf(PlpString(Params^[1])^);
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
  PVerySimpleXml(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_LoadFromStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

procedure _LapeVerySimpleXml_LoadFromString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.LoadFromString(PlpString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_SaveToStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

procedure _LapeVerySimpleXml_SaveToFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

procedure _LapeVerySimpleXml_SaveToString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.SaveToString(PlpString(Params^[1])^);
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
  PlpString(Result)^ := PVerySimpleXml(Params^[0])^.Ident;
end;

procedure _LapeVerySimpleXml_Ident_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Ident := PlpString(Params^[1])^;
end;

procedure _LapeVerySimpleXml_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PVerySimpleXml(Params^[0])^.Free();
end;

