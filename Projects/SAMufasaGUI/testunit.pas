unit TestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, SynEdit, SynHighlighterPas, SynMemo,
  //Client,
  MufasaTypes,
  mmlpsthread,
  mmlthread,
  window // for the comp picker and selector
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Mufasa_Image_List: TImageList;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItemScript: TMenuItem;
    MenuItemRun: TMenuItem;
    SynEdit1: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
    ToolBar1: TToolBar;
    TB_Run: TToolButton;
    TB_Pause: TToolButton;
    TB_Stop: TToolButton;
    ToolButton1: TToolButton;
    TB_ReloadPlugins: TToolButton;
    TB_WAT: TToolButton;
    TB_NewTab: TToolButton;
    TB_CloseTab: TToolButton;
    ToolButton4: TToolButton;
    TB_ClearDebug: TToolButton;
    TB_PickColour: TToolButton;
    TB_SelectClient: TToolButton;
    ToolButton8: TToolButton;
    TB_Convert: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemRunClick(Sender: TObject);
  private
    { private declarations }
  public
    Window: TMWindow;
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
   lclintf,plugins;



{ TForm1 }
procedure Run;
Var
  MMLPSThread : TMMLPSThread;

begin
  MMLPSThread := TMMLPSThread.Create(True);
  MMLPSThread.SetPSScript(Form1.SynEdit1.Lines.Text);
  MMLPSThread.SetDebug(Form1.Memo1);
  MMLPSThread.Resume;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin;
  Run;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Window := TMWindow.Create;
  MainDir:= ExtractFileDir(Application.ExeName);
  PluginsGlob := TMPlugins.Create;
  PluginsGlob.PluginDirs.Add(ExpandFileName(MainDir + DS + '..' + DS + '..'+ DS + 'Plugins'+ DS));
//  SynMemo1.sc
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Window.Free;
  PluginsGlob.Free;
end;

procedure TForm1.MenuItemRunClick(Sender: TObject);
begin
  Run;
end;


initialization
  {$I testunit.lrs}

end.

