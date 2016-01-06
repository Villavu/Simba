unit lpxml;
//Depends: TXmlNode, TObject, String, String): TXmlNode; virtual

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils,uxml, lpcompiler, lptypes,lpClassHelper;

procedure Register_TXml(Compiler: TLapeCompiler);

implementation

type
  PXmlNode = ^TXmlNode;
  PXMLNodeList = ^TXMLNodeList;
  PVerySimpleXML = ^TVerySimpleXML;
  PStream = ^TStream;

//constructor Create;
procedure TXmlNode_Init(const Params: PParamArray); lape_extdecl
begin
  PXmlNode(Params^[0])^ := TXmlNode.Create();
end;

//function Find(Name: String): TXmlNode; overload;
procedure TXmlNode_Find(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PlpString(Params^[1])^);
end;

//property get
procedure TXmlNode_GetParent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Parent;
end;

procedure TXmlNode_GetNodeName(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PXmlNode(Params^[0])^.NodeName;
end;

procedure TXmlNode_GetText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PXmlNode(Params^[0])^.Text;
end;

procedure TXmlNode_SetNodeName(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Params^[0])^.NodeName := PLpString(Params^[1])^;
end;

procedure TXmlNode_SetParent(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Params^[0])^.Parent := PXMLNode(Params^[1])^;
end;

procedure TXmlNode_GetChildNodes(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXMLNodeList(Result)^ := PXmlNode(Params^[0])^.ChildNodes;
end;

//function Find(Name, Attribute: String): TXmlNode; overload;
procedure TXmlNode_FindEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//function Find(Name, Attribute, Value: String): TXmlNode; overload;
procedure TXmlNode_FindExEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.Find(PlpString(Params^[1])^, PlpString(Params^[2])^, PlpString(Params^[3])^);
end;

//function FindNodes(Name: String): TXmlNodeList;
procedure TXmlNode_FindNodes(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNodeList(Result)^ := PXmlNode(Params^[0])^.FindNodes(PlpString(Params^[1])^);
end;

//function HasAttribute(const Name: String): Boolean;
procedure TXmlNode_HasAttribute(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PXmlNode(Params^[0])^.HasAttribute(PlpString(Params^[1])^);
end;

//function AddChild(const Name: String): TXmlNode; ;
procedure TXmlNode_AddChild(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.AddChild(PlpString(Params^[1])^);
end;

//function SetText(Value: String): TXmlNode;
procedure TXmlNode_SetText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.SetText(PlpString(Params^[1])^);
end;

//function SetAttribute(AttrName: String;Value: String): TXmlNode;
procedure TXmlNode_SetAttribute(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXmlNode(Result)^ := PXmlNode(Params^[0])^.SetAttribute(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure Free();
procedure TXmlNode_Free(const Params: PParamArray); lape_extdecl
begin
  PXmlNode(Params^[0])^.Free();
end;

procedure TXmlNode_GetAttribute(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PXmlNode(Params^[0])^[PlpString(Params^[1])^];
end;

procedure Register_TXmlNode(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin

    addClass('TXmlNode', 'TObject');
    addClass('TXMLNodeList', 'TObject');
    addGlobalFunc('procedure TXmlNode.Init();', @TXmlNode_Init);
    addGlobalFunc('function TXmlNode.GetChildNodes(): TXMLNodeList;', @TXMLNode_GetChildNodes);
    addGlobalFunc('function TXmlNode.GetNodeName(): string;', @TXmlNode_GetNodeName);
    addGlobalFunc('function TXmlNode.GetText(): string;', @TXmlNode_GetText);
    addGlobalFunc('procedure TXmlNode.SetNodeName(const Name: string);', @TXmlNode_SetNodeName);
    addGlobalFunc('procedure TXmlNode.SetParent(const Node: TXMLNode);', @TXmlNode_SetParent);
    addGlobalFunc('function TXmlNode.GetAttribute(const Name: string): string;', @TXmlNode_GetAttribute);
    addGlobalFunc('function TXmlNode.GetParent(): TXMLNode;', @TXmlNode_GetParent);
    addGlobalFunc('function TXmlNode.Find(Name: String): TXmlNode;', @TXmlNode_Find);
    addGlobalFunc('function TXmlNode.Find(Name, Attribute: String): TXmlNode; overload;', @TXmlNode_FindEx);
    addGlobalFunc('function TXmlNode.Find(Name, Attribute, Value: String): TXmlNode; overload;', @TXmlNode_FindExEx);
    addGlobalFunc('function TXmlNode.FindNodes(Name: String): TXmlNodeList;', @TXmlNode_FindNodes);
    addGlobalFunc('function TXmlNode.HasAttribute(const Name: String): Boolean;', @TXmlNode_HasAttribute);
    addGlobalFunc('function TXmlNode.AddChild(const Name: String): TXmlNode;;', @TXmlNode_AddChild);
    addGlobalFunc('function TXmlNode.SetText(Value: String): TXmlNode;', @TXmlNode_SetText);
    addGlobalFunc('function TXmlNode.SetAttribute(AttrName: String;Value: String): TXmlNode;', @TXmlNode_SetAttribute);
    addGlobalFunc('procedure TXmlNode.Free();', @TXmlNode_Free);
  end;
end;

//constructor Create;
procedure TXMLNodeList_Init(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^ := TXMLNodeList.Create();
end;

//procedure Clear;
procedure TXMLNodeList_Clear(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Clear();
end;

//procedure Assign(Src: TXMLNodeList);
procedure TXMLNodeList_Assign(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Assign(PXMLNodeList(Params^[1])^);
end;

//procedure Add(aXMLNode: TXMLNode); overload;
procedure TXMLNodeList_Add(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Add(PXMLNode(Params^[1])^);
end;

//procedure Add(aXMLNodes: TXMLNodeList); overload;
procedure TXMLNodeList_AddEx(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Add(PXMLNodeList(Params^[1])^);
end;

//function IndexOf(aXMLNode: TXMLNode): Integer; overload;
procedure TXMLNodeList_IndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.IndexOf(PXMLNode(Params^[1])^);
end;

//function IndexOf(NodeName: string): Integer; overload;
procedure TXMLNodeList_IndexOfEx(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.IndexOf(PlpString(Params^[1])^);
end;

//procedure Delete(aXMLNode: TXMLNode); overload;
procedure TXMLNodeList_Delete(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Delete(PXMLNode(Params^[1])^);
end;

//procedure Delete(Index: Integer); overload;
procedure TXMLNodeList_DeleteEx(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//Read: property Count: Integer read GetCount;
procedure TXMLNodeList_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PXMLNodeList(Params^[0])^.Count;
end;

//Read: property Item[Index: Integer]: TXMLNode read GetItem; default;
procedure TXMLNodeList_Item(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXMLNode(Result)^ := PXMLNodeList(Params^[0])^.Item[PInteger(Params^[1])^];
end;

//procedure Free();
procedure TXMLNodeList_Free(const Params: PParamArray); lape_extdecl
begin
  PXMLNodeList(Params^[0])^.Free();
end;

procedure Register_TXMLNodeList(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addGlobalFunc('procedure TXMLNodeList.Init();', @TXMLNodeList_Init);
    addGlobalFunc('procedure TXMLNodeList.Clear();', @TXMLNodeList_Clear);
    addGlobalFunc('procedure TXMLNodeList.Assign(Src: TXMLNodeList);', @TXMLNodeList_Assign);
    addGlobalFunc('procedure TXMLNodeList.Add(aXMLNode: TXMLNode);', @TXMLNodeList_Add);
    addGlobalFunc('procedure TXMLNodeList.Add(aXMLNodes: TXMLNodeList); overload;', @TXMLNodeList_AddEx);
    addGlobalFunc('function TXMLNodeList.IndexOf(aXMLNode: TXMLNode): Integer;', @TXMLNodeList_IndexOf);
    addGlobalFunc('function TXMLNodeList.IndexOf(NodeName: string): Integer; overload;', @TXMLNodeList_IndexOfEx);
    addGlobalFunc('procedure TXMLNodeList.Delete(aXMLNode: TXMLNode);', @TXMLNodeList_Delete);
    addGlobalFunc('procedure TXMLNodeList.Delete(Index: Integer); overload;', @TXMLNodeList_DeleteEx);
    addClassVar('TXMLNodeList', 'Count', 'Integer', @TXMLNodeList_Count_Read, nil);
    addGlobalFunc('function TXMLNodeList.Item(Index: Integer): TXMLNode;', @TXMLNodeList_Item);
    addGlobalFunc('procedure TXMLNodeList.Free();', @TXMLNodeList_Free);
  end;
end;

//constructor Create; virtual;
procedure TVerySimpleXml_Init(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^ := TVerySimpleXml.Create();
end;

//procedure Clear; virtual;
procedure TVerySimpleXml_Clear(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.Clear();
end;

//procedure LoadFromFile(const FileName: String);
procedure TVerySimpleXml_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure LoadFromStream(const Stream: TStream);
procedure TVerySimpleXml_LoadFromStream(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//procedure SaveToStream(const Stream: TStream);
procedure TVerySimpleXml_SaveToStream(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//procedure SaveToFile(const FileName: String);
procedure TVerySimpleXml_SaveToFile(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//Read: property Root: TXMLNode read GetRoot write SetRoot;
procedure TVerySimpleXml_Root_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXMLNode(Result)^ := PVerySimpleXml(Params^[0])^.Root;
end;

//Write: property Root: TXMLNode read GetRoot write SetRoot;
procedure TVerySimpleXml_Root_Write(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.Root := PXMLNode(Params^[1])^;
end;

//Read: property Header: TXMLNode read GetHeader write SetHeader;
procedure TVerySimpleXml_Header_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PXMLNode(Result)^ := PVerySimpleXml(Params^[0])^.Header;
end;

//Write: property Header: TXMLNode read GetHeader write SetHeader;
procedure TVerySimpleXml_Header_Write(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.Header := PXMLNode(Params^[1])^;
end;

//Read: property Ident: string read GetIdent write SetIdent;
procedure TVerySimpleXml_Ident_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PVerySimpleXml(Params^[0])^.Ident;
end;

//Write: property Ident: string read GetIdent write SetIdent;
procedure TVerySimpleXml_Ident_Write(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.Ident := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TVerySimpleXml_Free(const Params: PParamArray); lape_extdecl
begin
  PVerySimpleXml(Params^[0])^.Free();
end;

procedure Register_TXml(Compiler: TLapeCompiler);
begin
  Register_TXmlNode(Compiler);
  Register_TXmlNodeList(Compiler);
  with Compiler do
  begin
    addClass('TXml', 'TObject');

    addGlobalFunc('procedure TXml.Init();', @TVerySimpleXml_Init);
    addGlobalFunc('procedure TXml.Clear();', @TVerySimpleXml_Clear);
    addGlobalFunc('procedure TXml.LoadFromFile(const FileName: String);', @TVerySimpleXml_LoadFromFile);
    addGlobalFunc('procedure TXml.LoadFromStream(const Stream: TStream);', @TVerySimpleXml_LoadFromStream);
    addGlobalFunc('procedure TXml.SaveToStream(const Stream: TStream);', @TVerySimpleXml_SaveToStream);
    addGlobalFunc('procedure TXml.SaveToFile(const FileName: String);', @TVerySimpleXml_SaveToFile);
    addClassVar('TXml', 'Root', 'TXMLNode', @TVerySimpleXml_Root_Read, @TVerySimpleXml_Root_Write);
    addClassVar('TXml', 'Header', 'TXMLNode', @TVerySimpleXml_Header_Read, @TVerySimpleXml_Header_Write);
    addClassVar('TXml', 'Ident', 'string', @TVerySimpleXml_Ident_Read, @TVerySimpleXml_Ident_Write);
    addGlobalFunc('procedure TXml.Free();', @TVerySimpleXml_Free);
  end;
end;

end.

