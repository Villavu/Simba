unit sm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, StdCtrls,sm_srv_base,sm_client_base, sm_types, sm_web,sm_settings, sm_control;

type

  { TSManager }

  TSManager = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    btns: TImageList;
    ListBox1: TListBox;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    PageControl1: TPageControl;
    ManagerPopup: TPopupMenu;
    SMPanel: TPanel;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    TreeView1: TTreeView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure UpdateClick(Sender: TObject);
    //simba integration
    procedure SetOptions(SimbaPath,ServerUrl,LocalStoragePath,DescFileName: string;FirstRun: boolean);
  private
    procedure LoadPackageToListView(aPackageItem: TPackageItem;idx: integer);
    procedure LoadToTreeView;
    procedure UpdateFileData(aFileItem: TFileItem);
    procedure UpdateStats();
    procedure GetFromServer();
    procedure ReloadFromServer();
    { private declarations }
  public
    { public declarations }
  end; 

var
  SManager: TSManager;
  Repository: TServerStorage;
  Local: TClientStorage;
  Loader: TDownloader;
  ManagerPopup: TPopupMenu;
  Index: integer;//current selected category index
  option: TOption;//just test options system

implementation

{$R *.lfm}
uses MufasaBase;
{ TSManager }


procedure TSManager.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TSManager.ListView1Click(Sender: TObject);
begin
  if not assigned(ListView1.Selected) then exit;
  UpdateFileData (TFileItem(ListView1.Selected.Data));
end;

procedure TSManager.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  oFileItem: TFileItemEx;
begin
  if not assigned(ListView1.Selected) then exit;
  if (button=mbRight)  then
   begin
     oFileItem:=Local.Items[index].Files.ItemsEx[ListView1.Selected.Index];
     case oFileItem.Installed of
       0:ManagerPopup.Items[0].Caption:='Install';
       1:ManagerPopup.Items[0].Caption:='Uninstall';
    end;
     case oFileItem.Update of
       0:ManagerPopup.Items[1].Caption:='Force update';
       1:ManagerPopup.Items[1].Caption:='Update';
    end;
     ManagerPopup.PopUp;
   end;
end;

procedure TSManager.MenuItem2Click(Sender: TObject);
begin
 ToolButton1Click(Sender);
end;

procedure TSManager.ToolButton1Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TSManager.ToolButton3Click(Sender: TObject);
begin
  if (index < 0) or (index > TreeView1.Items.Count) then exit;
  ReloadFromServer;
  Local.CheckStorage(Repository);
  Local.CheckUpdates(Repository);
  LoadPackageToListView(Repository.Items[index],Index);
  UpdateStats;
end;

procedure TSManager.ToolButton4Click(Sender: TObject);
var
  i,j: integer;
  ExItem: TFileItemEx;
begin
  Local.CheckStorage(Repository);
  for i:=0 to local.Count-1 do
    begin
      for j:=0 to local.Items[i].Files.Count-1 do
        begin
          ExItem:=local.Items[i].Files.ItemsEx[j];
          if (ExItem.Update > 0) and (ExItem.Installed > 0) then
           if UpdateScript(Repository.Items[i].Files[j],option) then
             begin
             ExItem.Update:=0;
             ExItem.Version:=Repository.Items[i].Files[j].Version;
             ExItem.DateModify:=Repository.Items[i].Files[j].DateModify;
             ExItem.Author:=Repository.Items[i].Files[j].Author;
             ExItem.Email:=Repository.Items[i].Files[j].Email;
              Local.UpdateLocalXMLRegistry(option.XMLStorage);
              UpdateStats;
              LoadPackageToListView(Repository.Items[i],i);
             end;
        end;
    end;
   Local.UpdateLocalXMLRegistry(option.XMLStorage);
end;

procedure TSManager.TreeView1Click(Sender: TObject);
begin
    if not assigned(TreeView1.Selected) then exit;
    index:=TreeView1.Selected.Index;
    LoadPackageToListView(TPackageItem(TreeView1.Selected.Data),Index);
    UpdateFileData (TFileItem(ListView1.Items[0].Data));
end;

procedure TSManager.InstallClick(Sender: TObject);
var
  loc: TFileItemEx;
   rep: TFileItem;
begin
   rep:=Repository.Items[index].Files.Items[ListView1.Selected.Index];
   loc:= Local.Items[index].Files.ItemsEx[ListView1.Selected.Index];
   case  loc.Installed of
       0:begin loc.Installed:=1; getScript(rep,option);  end;
       1:begin loc.Installed:=0; RemoveScript(rep,option); end;
    end;
    loc.Version:=rep.Version;
    loc.DateModify:=rep.DateModify;
    loc.Author:=rep.Author;
    loc.EMail:=rep.EMail;
    loc.FileName:=rep.FileName;
   Local.UpdateLocalXMLRegistry(option.XMLStorage);
   LoadPackageToListView(Repository.Items[index],index);
   UpdateStats;
end;

procedure TSManager.UpdateClick(Sender: TObject);
var
  loc: TFileItemEx;
   rep: TFileItem;
begin
   rep:=Repository.Items[index].Files.Items[ListView1.Selected.Index];
   loc:= Local.Items[index].Files.ItemsEx[ListView1.Selected.Index];
   if (loc.Installed < 1) then exit;
   case  loc.update of
       0:begin  getScript(rep,option);  end;
       1:begin loc.update:=0; UpdateScript(rep,option); end;
    end;
    loc.Version:=rep.Version;
    loc.DateModify:=rep.DateModify;
    loc.Author:=rep.Author;
    loc.EMail:=rep.EMail;
    loc.FileName:=rep.FileName;
   Local.UpdateLocalXMLRegistry(option.XMLStorage);
   LoadPackageToListView(Repository.Items[index],index);
   UpdateStats;
end;

Procedure TSManager.SetOptionS(SimbaPath, ServerUrl,
  LocalStoragePath,DescFileName: string; FirstRun: boolean);
begin
  if not FirstRun then
   begin
    index:=0;
    SetOptionsPaths(ServerUrl,LocalStoragePath,SimbaPath,DescFileName,option);
    try
    GetFromServer();
    except
      mDebugLn('Script Manager server was not found');
      exit;
    end;
    Local := TClientStorage.Create();
    Local.LoadLocalXMLRegistry(option.XMLStorage);
    Local.CheckStorage(Repository);
    Local.UpdateLocalXMLRegistry(option.XMLStorage);
    UpdateStats;
    LoadToTreeView;
   end
  else
   begin
    index:=0;
    SetOptionsPaths(ServerUrl,LocalStoragePath,SimbaPath,DescFileName,option);
    try
    GetFromServer();
    except
      mDebugLn('Script Manager server was not found');
      exit;
    end;
    Repository.SaveLocalXMLRegistry(option.XMLStorage);
    Local := TClientStorage.Create();
    Local.LoadLocalXMLRegistry(option.XMLStorage);
    Local.CheckStorage(Repository);
    Local.UpdateLocalXMLRegistry(option.XMLStorage);
    UpdateStats;
    LoadToTreeView;
   end;
end;



procedure TSManager.LoadPackageToListView(aPackageItem: TPackageItem;idx: integer);
var
  I: Integer;
  oListItem: TListItem;
  oFileItem: TFileItemEx;
begin
  ListView1.Items.Clear;
  for I := 0 to aPackageItem.Files.Count - 1 do
  begin
    oListItem:= ListView1.Items.Add;
    oListItem.Data:=aPackageItem.Files[i];
    oListItem.Caption:=aPackageItem.Files[i].FileName;
    oListItem.SubItems.Add(aPackageItem.Files[i].Author);
    oListItem.SubItems.Add(aPackageItem.Files[i].EMail);
    oListItem.SubItems.Add(DateToStr(aPackageItem.Files[i].DateModify));
    oFileItem:=local.items[idx].Files.ItemsEx[i];
    case oFileItem.Installed of
    0:begin oListItem.SubItems.Add(FloatToStr(aPackageItem.Files[i].Version)); oListItem.SubItems.Add('Not installed'); end;
    1:begin
         if oFileItem.update > 0 then
             oListItem.SubItems.Add(FloatToStr(oFileItem.version)+'<'+FloatToStr(aPackageItem.Files[i].Version))
             else
               oListItem.SubItems.Add(FloatToStr(oFileItem.version));
         oListItem.SubItems.Add('Installed');
       end;
    end;
    UpdateFileData(aPackageItem.Files[i]);
    //Items.AddObject(nil, FConfig.Items[i].Name, FConfig.Items[i]);

  end;


  if aPackageItem.Files.Count = 0 then
    UpdateFileData(nil);
 // Exit;
//  if aPackageItem.Files.Count > 0 then
 //   UpdateFileData(TFileItem(ListView1.Items[0].Data));

end;

procedure TSManager.LoadToTreeView;
var
  I: Integer;
  TempNode: TTreeNode;
begin
  TreeView1.Items.Clear;
  for I := 0 to Repository.Count - 1 do
    //TreeView1.Items.AddObject(nil, FConfig.Items[i].Name, FConfig.Items[i]);
  begin
     TempNode:=TreeView1.Items.Add(nil,Repository.Items[i].Name);
     TempNode.Data:=Repository.Items[i];
  end;
  Index:=0;
  LoadPackageToListView(TPackageItem(Repository.Items[0]),0);
end;

procedure TSManager.UpdateFileData(aFileItem: TFileItem);
var
  I: Integer;
  sItem: TSubItem;
begin
  ListBox1.Items.Clear;
  Memo1.Lines.Text:='';

  if not Assigned(aFileItem) then
   Exit;


  Memo1.Lines.Text:=aFileItem.Description;

  for I := 0 to aFileItem.SubFiles.Count - 1 do
  begin
     sItem:=TSubItem(aFileItem.SubFiles[i]);
     ListBox1.Items.Add (sItem.FileName);
  end;

end;

procedure TSManager.UpdateStats();
procedure UpdateStatusBar(category ,scripts, installed, updates: integer);
begin
 with StatusBar1 do
  begin
   Panels[0].Text:='Categories:'+#13+IntToStr(category);
   Panels[1].Text:='Scripts:'+#13+IntToStr(scripts);
   Panels[2].Text:='Installed:'+#13+IntToStr(installed);
   Panels[3].Text:='Available updates:'+#13+IntToStr(updates);
 //  Panels[4].Text:='Memory: '+#13+IntToStr(getlost);
  end;
end;
var
  i,j: integer;
  installed,updates: integer;
  catcount, scriptcount: integer;
  CatItem: TPackageItem;
  ExItem: TFileItemEx;
begin
  Local.CheckStorage(Repository);
  catcount:=Repository.Count;
  scriptcount:=0;
  installed:=0;
  updates:=0;
  for i:=0 to Repository.Count -1 do
   begin
     CatItem:=Repository.Items[i];
     scriptcount:=scriptcount+CatItem.Files.Count;
   end;
  for i:=0 to local.Count-1 do
    begin
      for j:=0 to local.Items[i].Files.Count-1 do
        begin
          ExItem:=local.Items[i].Files.ItemsEx[j];
          if ExItem.Installed > 0 then inc(installed);
          if ExItem.Update > 0 then inc(updates);
        end;
    end;
  UpdateStatusBar(catcount,scriptcount,installed,updates);
end;

procedure TSManager.GetFromServer();
var
  XML: TMemoryStream;
begin
  XML:=TMemoryStream.Create;
  try
  Loader:=Tdownloader.Create(option.XMLSrvDesc+option.SrvFileNAme);
  loader.Download(XML);
  XML.Position:=0;
  Repository := TServerStorage.Create();
  Repository.LoadFromXmlStream(XML);
//  Repository.SaveLocalXMLRegistry(option.XMLStorage);
  finally
    XML.Free;
  end;
end;
{FIXME~!}
procedure TSManager.ReloadFromServer();
var
  XML: TMemoryStream;
begin
  XML:=TMemoryStream.Create;
  try
   //Repository.Clear;   always error. Fix me!
   loader.Download(XML);
   XML.Position:=0;
   Repository := TServerStorage.Create();
   Repository.LoadFromXmlStream(XML);
   Local.CheckStorage(Repository);
  finally
    XML.Free;
  end;

  end;

end.

