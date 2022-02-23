pushSection('Classes');

addClass('TXmlNode', 'TObject');
addClass('TXMLNodeList', 'TObject');
addGlobalFunc('procedure TXmlNode.Init()', @_LapeXmlNode_Init);
addGlobalFunc('function TXmlNode.GetChildNodes: TXMLNodeList; constref', @_LapeXMLNode_GetChildNodes);
addGlobalFunc('function TXmlNode.GetNodeName: string; constref', @_LapeXmlNode_GetNodeName);
addGlobalFunc('function TXmlNode.GetText: string; constref', @_LapeXmlNode_GetText);
addGlobalFunc('procedure TXmlNode.SetNodeName(const Name: string); constref', @_LapeXmlNode_SetNodeName);
addGlobalFunc('procedure TXmlNode.SetParent(const Node: TXMLNode); constref', @_LapeXmlNode_SetParent);
addGlobalFunc('function TXmlNode.GetAttribute(const Name: string): string; constref', @_LapeXmlNode_GetAttribute);
addGlobalFunc('function TXmlNode.GetParent: TXMLNode; constref', @_LapeXmlNode_GetParent);
addGlobalFunc('function TXmlNode.Find(Name: String): TXmlNode; constref; overload', @_LapeXmlNode_Find);
addGlobalFunc('function TXmlNode.Find(Name, Attribute: String): TXmlNode; constref; overload', @_LapeXmlNode_FindEx);
addGlobalFunc('function TXmlNode.Find(Name, Attribute, Value: String): TXmlNode; constref; overload', @_LapeXmlNode_FindExEx);
addGlobalFunc('function TXmlNode.FindNodes(Name: String): TXmlNodeList; constref', @_LapeXmlNode_FindNodes);
addGlobalFunc('function TXmlNode.HasAttribute(const Name: String): Boolean; constref', @_LapeXmlNode_HasAttribute);
addGlobalFunc('function TXmlNode.AddChild(const Name: String): TXmlNode; constref', @_LapeXmlNode_AddChild);
addGlobalFunc('function TXmlNode.SetText(Value: String): TXmlNode; constref', @_LapeXmlNode_SetText);
addGlobalFunc('function TXmlNode.SetAttribute(AttrName: String;Value: String): TXmlNode; constref', @_LapeXmlNode_SetAttribute);
//addGlobalFunc('procedure TXmlNode.Free; constref', @_LapeXmlNode_Free);

addGlobalFunc('procedure TXMLNodeList.Init()', @_LapeXMLNodeList_Init);
addGlobalFunc('procedure TXMLNodeList.Clear; constref', @_LapeXMLNodeList_Clear);
addGlobalFunc('procedure TXMLNodeList.Assign(Src: TXMLNodeList); constref', @_LapeXMLNodeList_Assign);
addGlobalFunc('procedure TXMLNodeList.Add(aXMLNode: TXMLNode); constref; overload', @_LapeXMLNodeList_Add);
addGlobalFunc('procedure TXMLNodeList.Add(aXMLNodes: TXMLNodeList); constref; overload', @_LapeXMLNodeList_AddEx);
addGlobalFunc('function TXMLNodeList.IndexOf(aXMLNode: TXMLNode): Integer; constref; overload', @_LapeXMLNodeList_IndexOf);
addGlobalFunc('function TXMLNodeList.IndexOf(NodeName: string): Integer; constref; overload', @_LapeXMLNodeList_IndexOfEx);
addGlobalFunc('procedure TXMLNodeList.Delete(aXMLNode: TXMLNode); constref; overload', @_LapeXMLNodeList_Delete);
addGlobalFunc('procedure TXMLNodeList.Delete(Index: Integer); constref; overload', @_LapeXMLNodeList_DeleteEx);
addClassVar('TXMLNodeList', 'Count', 'Integer', @_LapeXMLNodeList_Count_Read, nil);
addGlobalFunc('function TXMLNodeList.Item(Index: Integer): TXMLNode; constref', @_LapeXMLNodeList_Item);
//addGlobalFunc('procedure TXMLNodeList.Free; constref', @_LapeXMLNodeList_Free);

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

