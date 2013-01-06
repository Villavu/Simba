unit sm_client_base;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, DateUtils, XMLRead,XMLWrite,Dom,sm_types,sm_srv_base,sm_utils;

type
  //-------------------------------------------------------------------

  { TClientStorage }

  TClientStorage = class(TPackageList)
  //-------------------------------------------------------------------
  private
    procedure ConvertFileItem(fItem: TFileItem;var fItemEx: TFileItemEx);
    procedure EqualScripts(index: integer;aServer: TFileItemList);
  public
    procedure CheckStorage(aScript: TServerStorage);
    procedure LoadLocalXMLRegistry(aFileName: string);
    procedure UpdateLocalXMLRegistry(aFileName: string);
    procedure CheckUpdates(aScript: TServerStorage);

  end;

implementation

procedure TClientStorage.CheckStorage(aScript: TServerStorage);
var
  I: Integer;
  j: Integer;
  k: Integer;
  res: integer;
  Local: TPackageItem;
  Server: TPackageItem;
  oFileItemEx: TFileItemEx;
  oFileItem: TFileItem;
  sFile: TSubItem;
begin
  //if categories in local storage < server storage
  if Count < aScript.Count then
    begin
      for i:= 0 to aScript.Count - 1 do begin
        server:=aScript.Items[i];
        local:=TPackageItem(FindByName(server.Name));
        if not assigned(local) then
          begin
          Local:=AddItem;
          Local.Name:=aScript.Items[i].Name;
           for j:=0 to aScript.Items[i].Files.Count-1 do
             begin
              oFileItemEx:=Local.Files.AddItemEx;
              oFileItem:= aScript.Items[i].Files[j];
              ConvertFileItem(oFileItem,oFileItemEx);
             end;
          end;
        EqualScripts(i,server.Files);
      end;
    end;
  //if categories in local storage > server storage
   if Count > aScript.Count then
    begin
      for i:= 0 to Count - 1 do begin
        local:=Items[i];
        server:=TPackageItem(FindByName(local.Name));
        if not assigned(server) then
          begin
          self.Delete(i);
          end;
        EqualScripts(i,server.Files);
      end;
    end;
  //checking categories names
   if Count = aScript.Count then
     begin
     for i:= 0 to Count - 1 do begin
       server:=aScript.Items[i];
       local:=Items[i];
        if Assigned(local) then
          if not Eq(local.Name, server.Name) then
            local.Name:=server.Name;
        EqualScripts(i,server.Files);
     end;
     end;
end;

procedure TClientStorage.ConvertFileItem(fItem: TFileItem;
  var fItemEx: TFileItemEx);
var
  i: integer;
  sFile: TSubItem;
begin
  fItemEx.Author:=fItem.Author;
  fItemEx.FileName:=fItem.FileName;
  fItemEx.DateModify:=fItem.DateModify;
  fItemEx.Installed:=0;
  fItemEx.Update:=0;
  fItemEx.Description:=fItem.Description;
  fItemEx.EMail:=fItem.EMail;
  fItemEx.Version:=fItem.Version;
  { for i:=0 to fItem.SubFiles.Count-1 do
     begin
       sFile:=fItemEx.SubFiles.AddItem;
       sFile.FileName:=fItem.SubFiles[i].FileName;
       sFile.UnpPath:=fItem.SubFiles[i].UnpPath;
     end; }
end;

procedure TClientStorage.EqualScripts(index: integer;aServer: TFileItemList);
var
  I: Integer;
  Local: TFileItemEx;
  Server: TFileItem;
begin
  if items[index].Files.Count < aServer.Count then
    begin
      for i:= 0 to aServer.Count - 1 do begin
        server:=aServer.Items[i];
        local:=TFileItemEx(items[index].Files.FindByName(server.FileName));
        if not assigned(local) then
          begin
          Local:=items[i].Files.AddItemEx;
          ConvertFileItem(server,local);
             end;
          end;
      end;
  if items[index].Files.Count > aServer.Count then
    begin
      for i:= 0 to items[index].Files.Count - 1 do begin
        local:=items[index].Files.ItemsEx[i];
        server:=TFileItem(aServer.FindByName(local.FileName));
        if not assigned(server) then
          begin
          self.Delete(i);
          end;
      end;
    end;
   if items[index].Files.Count = aServer.Count then
     begin
     for i:= 0 to aServer.Count-1 do begin
       server:=aServer.Items[i];
       local:=items[index].Files.ItemsEx[i];
        if Assigned(local) then
          if not Eq(local.FileName, server.FileName) then
            begin
            local.FileName:=server.FileName;
            local.Description:=server.Description;
     end;
     end;

     end;
end;

procedure TClientStorage.LoadLocalXMLRegistry(aFileName: string);
  procedure DoLoadFiles(aParentNode: TDOMNode; aPackageItem: TPackageItem);
  var
    I: Integer;
    j: Integer;
    oFileItem: TFileItemEx;
    oNode,oNode1: TDOMNode;
    s: string;
    sItem: TSubitem;
  begin
    for I := 0 to aParentNode.ChildNodes.Count - 1 do
    begin
      oFileItem:=aPackageItem.Files.AddItemEx;

      oNode:=aParentNode.ChildNodes[i];


      oFileItem.FileName:= VarToStr(oNode.Attributes.GetNamedItem('filename').NodeValue);
      oFileItem.Author  := VarToStr(oNode.Attributes.GetNamedItem('author').NodeValue);
      oFileItem.EMail   := VarToStr(oNode.Attributes.GetNamedItem('email').NodeValue);
      oFileItem.Version := StrToFloat(VarToStr(oNode.Attributes.GetNamedItem('version').NodeValue));
      oFileItem.Installed:=StrToInt(VarToStr(oNode.Attributes.GetNamedItem('installed').NodeValue));
      oFileItem.Update:=StrToInt(VarToStr(oNode.Attributes.GetNamedItem('update').NodeValue));

      s:=VarToStr(oNode.Attributes.GetNamedItem('date_modify').NodeValue);

      oFileItem.DateModify := SM_StrToDate(s);

      for j := 0 to oNode.ChildNodes.Count - 1 do
      begin
        oNode1:=oNode.ChildNodes[j];

      {  if LowerCase(oNode1.NodeName)='subfile' then
        begin
          sItem:=oFileItem.SubFiles.AddItem;
          sItem.FileName:=oNode1.Attributes.GetNamedItem('filename').NodeValue;
          sItem.UnpPath:=oNode1.Attributes.GetNamedItem('filepath').NodeValue;
        end else  }

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
  ReadXMLFile(oXmlDocument,aFileName);

  DoLoadPackages (oXmlDocument.DocumentElement);

  FreeAndNil(oXmlDocument);
end;

procedure TClientStorage.UpdateLocalXMLRegistry(aFileName: string);
var
  oXmlDocument: TXmlDocument;
  vRoot,PackageNode,TempNode,Description,FileItemNode,SubFileNode: TDOMNode;
  i,d,j: integer;
  s: string;
  oFileItem: TFileItemEx;
begin
  DeleteFile(aFileName);
 // CheckStorage(aScript);
  oXmlDocument:=TXmlDocument.Create;
  {oXmlDocument.Encoding:='UTF-8'; // No Encoding with FPC 2.6.x}
  vRoot:=oXmlDocument.CreateElement('Document');
  oXmlDocument.AppendChild(vroot);
  vRoot:=oXMLDocument.DocumentElement;
  for i:=0 to count -1 do
     begin
       PackageNode:=oXmlDocument.CreateElement('structure');
       TDOMElement(PackageNode).SetAttribute('name',Items[i].Name);
         for d:=0 to Items[i].Files.Count - 1 do
            begin
              oFileItem:=Items[i].Files.ItemsEx[d];
              FileItemNode:=oXMLDocument.CreateElement('file');
              TDOMElement(FileItemNode).SetAttribute('filename',oFileItem.FileName);
              TDOMElement(FileItemNode).SetAttribute('author',oFileItem.Author);
              TDOMElement(FileItemNode).SetAttribute('email',oFileItem.EMail);
              TDOMElement(FileItemNode).SetAttribute('version',FloatToStr(oFileItem.Version));
              TDOMElement(FileItemNode).SetAttribute('installed',IntToStr(oFileItem.Installed));
              TDOMElement(FileItemNode).SetAttribute('update',IntToStr(oFileItem.Update));

              s:=DateTimeToStr(oFileItem.DateModify);
              TDOMElement(FileItemNode).SetAttribute('date_modify',s);
                if oFileItem.description<>'' then
                 begin
                   TempNode:=oXMLDocument.CreateElement('description');
                   Description:=oXMLDocument.CreateTextNode(oFileItem.description);
                   TempNode.AppendChild(Description);
                   FileItemNode.AppendChild(TempNode);
                   end;
             { for j := 0 to oFileItem.SubFiles.Count - 1 do
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

procedure TClientStorage.CheckUpdates(aScript: TServerStorage);
var
  i: integer;
  k: integer;
  local,server: TPackageItem;
  oFileItem: TFileItem;
  oFIleItemEx: TFileItemEx;
begin
  for i:=0 to count -1 do
    begin
      local:=items[i];
      server:=aScript.items[i];
       for k:=0 to local.files.count -1 do
         begin
           oFileItem:= server.files[k];
           oFileItemEx:= local.files.itemsex[k];
            if (oFileItemEx.version<oFileItem.version) and (oFileItemEx.installed > 0) then
              begin
                oFileItemEx.update:=1;
              end;
         end;
    end;

end;


begin
  ShortDateFormat:='dd.mm.yyyy';
  ShortTimeFormat:='h:mm';

  {Test; }

end.


