unit sm_srv_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, DateUtils, XMLRead,XMLWrite,Dom,sm_types,sm_utils;

type
  //-------------------------------------------------------------------

  { TScriptStorage }

  { TServerStorage }

  TServerStorage = class(TPackageList)
  public
    procedure LoadFromXmlStream(aFileName: TStream);
    procedure LoadFromXmlFile(aFileName: string);
    procedure SaveLocalXMLRegistry(aFileName: string);
  end;

implementation
//-------------------------------------------------------------------
procedure TServerStorage.LoadFromXmlStream(aFileName: TStream);
//-------------------------------------------------------------------

  procedure DoLoadFiles(aParentNode: TDOMNode; aPackageItem: TPackageItem);
  var
    I: Integer;
    j: Integer;
    oFileItem: TFileItem;
    oNode,oNode1: TDOMNode;
    s,p: string;
    sItem: TSubitem;
  begin
    for I := 0 to aParentNode.ChildNodes.Count - 1 do
    begin
      oFileItem:=aPackageItem.Files.AddItem;

      oNode:=aParentNode.ChildNodes[i];
      oFileItem.FileName:= VarToStr(oNode.Attributes.GetNamedItem('filename').NodeValue);
      oFileItem.Author  := VarToStr(oNode.Attributes.GetNamedItem('author').NodeValue);
      oFileItem.EMail   := VarToStr(oNode.Attributes.GetNamedItem('email').NodeValue);
      oFileItem.Version := StrToFloat(VarToStr(oNode.Attributes.GetNamedItem('version').NodeValue));

      s:=VarToStr(oNode.Attributes.GetNamedItem('date_modify').NodeValue);

      //oFileItem.DateModify := StrToDateTime(s);
        oFileItem.DateModify := SM_StrToDate(s);

      for j := 0 to oNode.ChildNodes.Count - 1 do
      begin
        oNode1:=oNode.ChildNodes[j];

        if LowerCase(oNode1.NodeName)='subfile' then
        begin
          sItem:=oFileItem.SubFiles.AddItem;
          sItem.FileName:=oNode1.Attributes.GetNamedItem('filename').NodeValue;
          sItem.UnpPath:=oNode1.Attributes.GetNamedItem('filepath').NodeValue;
        end else

        if LowerCase(oNode1.NodeName)='description' then
        begin
          oFileItem.Description:=oNode1.TextContent;
        end;

      end;

    end;
  end;


  procedure DoLoadPackages(aParentNode: TDOMNode);
  var
    I: Integer;
    oNode: TDOMNode;
    oPackageItem: TPackageItem;
  begin
    for I := 0 to aParentNode.ChildNodes.Count - 1 do
    begin
      oPackageItem:=AddItem;

      oNode:=aParentNode.ChildNodes[i];
      oPackageItem.Name:= oNode.Attributes.GetNamedItem('name').NodeValue;

      DoLoadFiles(oNode, oPackageItem);
    end;
  end;

var
  oXmlDocument: TXmlDocument;
begin
  oXMLDocument:=TXMLDocument.Create;
  ReadXMLFile(oXmlDocument,aFileName);

  DoLoadPackages (oXmlDocument.DocumentElement);

  FreeAndNil(oXmlDocument);
end;

procedure TServerStorage.LoadFromXmlFile(aFileName: string);
  procedure DoLoadFiles(aParentNode: TDOMNode; aPackageItem: TPackageItem);
   var
     I: Integer;
     j: Integer;
     oFileItem: TFileItem;
     oNode,oNode1: TDOMNode;
     s,p: string;
     sItem: TSubitem;
   begin
     for I := 0 to aParentNode.ChildNodes.Count - 1 do
     begin
       oFileItem:=aPackageItem.Files.AddItem;

       oNode:=aParentNode.ChildNodes[i];
       oFileItem.FileName:= VarToStr(oNode.Attributes.GetNamedItem('filename').NodeValue);
       oFileItem.Author  := VarToStr(oNode.Attributes.GetNamedItem('author').NodeValue);
       oFileItem.EMail   := VarToStr(oNode.Attributes.GetNamedItem('email').NodeValue);
       oFileItem.Version := StrToFloat(VarToStr(oNode.Attributes.GetNamedItem('version').NodeValue));

       s:=VarToStr(oNode.Attributes.GetNamedItem('date_modify').NodeValue);

       //oFileItem.DateModify := StrToDateTime(s);
         oFileItem.DateModify := SM_StrToDate(s);

       for j := 0 to oNode.ChildNodes.Count - 1 do
       begin
         oNode1:=oNode.ChildNodes[j];

         if LowerCase(oNode1.NodeName)='subfile' then
         begin
           sItem:=oFileItem.SubFiles.AddItem;
           sItem.FileName:=oNode1.Attributes.GetNamedItem('filename').NodeValue;
           sItem.UnpPath:=oNode1.Attributes.GetNamedItem('filepath').NodeValue;
         end else

         if LowerCase(oNode1.NodeName)='description' then
         begin
           oFileItem.Description:=oNode1.TextContent;
         end;

       end;

     end;
   end;


   procedure DoLoadPackages(aParentNode: TDOMNode);
   var
     I: Integer;
     oNode: TDOMNode;
     oPackageItem: TPackageItem;
   begin
     for I := 0 to aParentNode.ChildNodes.Count - 1 do
     begin
       oPackageItem:=AddItem;

       oNode:=aParentNode.ChildNodes[i];
       oPackageItem.Name:= oNode.Attributes.GetNamedItem('name').NodeValue;

       DoLoadFiles(oNode, oPackageItem);
     end;
   end;

 var
   oXmlDocument: TXmlDocument;
 begin
   oXMLDocument:=TXMLDocument.Create;
   ReadXMLFile(oXmlDocument,aFileName);

   DoLoadPackages (oXmlDocument.DocumentElement);

   FreeAndNil(oXmlDocument);

 end;

procedure TServerStorage.SaveLocalXMLRegistry(aFileName: string);
var
  oXmlDocument: TXmlDocument;
  vRoot,ParentNode,PackageNode,TempNode,Description,FileItemNode,SubFileNode: TDOMNode;
  i,d,j: integer;
  s: string;
  oFileItem: TFileItem;
begin
  oXmlDocument:=TXmlDocument.Create;
  {oXmlDocument.Encoding:='UTF-8'; // No exists in FPC 2.6.x}
  vRoot:=oXmlDocument.CreateElement('Document');
  oXmlDocument.AppendChild(vroot);
  vRoot:=oXMLDocument.DocumentElement;
  for i:=0 to count -1 do
     begin
       PackageNode:=oXmlDocument.CreateElement('structure');
       TDOMElement(PackageNode).SetAttribute('name',Items[i].Name);
         for d:=0 to Items[i].Files.Count - 1 do
            begin
              oFileItem:=Items[i].Files.Items[d];
              FileItemNode:=oXMLDocument.CreateElement('file');
              TDOMElement(FileItemNode).SetAttribute('filename',oFileItem.FileName);
              TDOMElement(FileItemNode).SetAttribute('author',oFileItem.Author);
              TDOMElement(FileItemNode).SetAttribute('email',oFileItem.EMail);
              TDOMElement(FileItemNode).SetAttribute('version',FloatToStr(oFileItem.Version));
              TDOMElement(FileItemNode).SetAttribute('installed',IntToStr(0));
              TDOMElement(FileItemNode).SetAttribute('update',IntToStr(0));
              s:=DateTimeToStr(oFileItem.DateModify);
              TDOMElement(FileItemNode).SetAttribute('date_modify',s);
                if oFileItem.description<>'' then
                 begin
                   TempNode:=oXMLDocument.CreateElement('description');
                   Description:=oXMLDocument.CreateTextNode(oFileItem.description);
                   TempNode.AppendChild(Description);
                   FileItemNode.AppendChild(TempNode);
                   end;
           {   for j := 0 to oFileItem.SubFiles.Count - 1 do
                begin
                   SubFileNode:=oXMLDocument.CreateElement('subfile');
                   TDOMElement(SubFileNode).SetAttribute('filename',oFileItem.SubFiles[j].FileName);
                   TDOMElement(SubFileNode).SetAttribute('filepath',oFileItem.SubFiles[j].UnpPath);
                   FileItemNode.AppendChild(SubFileNode);
                end; }
             PackageNode.AppendChild(FileItemNode);
            end;
       vRoot.AppendChild(PackageNode);
     end;
  WriteXMLFile (oXmlDocument,aFileName);
  FreeAndNil(oXmlDocument);
end;


begin
  ShortDateFormat:='dd.mm.yyyy';
  ShortTimeFormat:='h:mm';

  {Test; }

end.

