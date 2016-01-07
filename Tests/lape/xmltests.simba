program new;

Procedure ShowStream(WhatTest: string;ss: TStringStream);
begin
 WriteLn(WhatTest + ' : '+ ss.getDataString());
end;

procedure XMLGenSaveTest(ss: TStringStream);
var
 Xml: TXml;
 Node, Child, SubChild: TXmlNode;
begin
 XML.Init;
 try
  Xml.GetHeader().SetAttribute('encoding','utf-8');

  Xml.GetRoot().SetNodeName('catalog');
  Xml.GetRoot().AddChild('book').SetAttribute('id', 'bk101').
    AddChild('author').SetText('Gambardella, Matthew').GetParent().
    AddChild('title').SetText('XML Developer''s Guide').GetParent().
    AddChild('description').SetText('An in-depth look at creating XML applications.');

  Xml.GetRoot().AddChild('book').SetAttribute('id', 'bk102').SetAttribute('lang','en').
    AddChild('author').SetText('Ralls, Kim').GetParent().
    AddChild('title').SetText('Midnight Rain').GetParent().
    AddChild('description').SetText('A former architect battles corporate zombies, '+
      'an evil sorceress, and her own childhood to become queen of the world.');

  Node := Xml.GetRoot().AddChild('book');
  Node.SetAttribute('id','bk103');

  Child := Node.AddChild('author');
  Child.SetText('Corets, Eva');

  Child := Node.AddChild('title');
  Child.SetText('Maeve Ascendant');

  Child := Node.AddChild('description');
  Child.SetText('After the "collapse" of a <nanotechnology> society in England, ' +
  'the young survivors lay the foundation for a new society.');

  Child := Node.AddChild('keywords');
  SubChild := Child.AddChild('keyword');
  SubChild.SetText('fantasy');
  SubChild := Child.AddChild('keyword');
  SubChild.SetText('technology');
  SubChild := Child.AddChild('keyword');
  SubChild.SetText('england');
  XML.SaveToStream(ss);
  ShowStream('Generated xml',ss);
 finally
  if assigned(XML) then
   XML.Free;
 end;
end;

Procedure XMLSimpleReadTest(ss: TStringStream);
var
 Node: TXMLNode;
 XML: TXML;
 mss: TStringStream;
begin
 XML.Init;
 try
  XML.LoadFromStream(ss);
  mss.Init('');
  XML.SaveToStream(mss);
  ShowStream('Readed xml',mss);
  WriteLn('Node description is: ' + Xml.GetRoot().Find('book', 'id', 'bk103').Find('description').GetText());
 finally
  if Assigned(XML) then
   XML.Free;
  if Assigned(mss) then
   mss.Free;
 end;
end;

procedure NodesReadTest(ss: TStringStream);
var
 XML : TXML;
 Node: TXMLNode;
 i, len: integer;
begin
 XML.Init;
 try
  XML.LoadFromStream(ss);
  Len:= XML.getRoot().GetChildNodes().getCount();
  WriteLn('len = '+ IntToStr(len));
  for i := 0 to len - 1 do
   begin
    Node:=XML.getRoot().GetChildNodes().Item(i);
    WriteLn('Node name = ' + Node.GetNodeName + ' id :' + Node.getAttribute('id'));
   end;
 finally
  if Assigned(XML) then
   XML.Free;
 end;
end;


var
 ss: TStringStream;
begin
 ss.Init('');
 try
   XMLGenSaveTest(ss);
   XMLSimpleReadTest(ss);
   NodesReadTest(ss);
 finally
  ss.Free;
 end;
end.
