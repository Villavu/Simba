unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ListView1: TListView;
    SaveButton: TButton;
    LoadButton: TButton;
    TreeView1: TTreeView;
    procedure SaveButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
  private
    { private declarations }

  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation
uses
  settings;

{ TForm1 }

procedure TForm1.SaveButtonClick(Sender: TObject);
var
  sett: TMMLSettings;
  s: TStringArray;
  i: Integer;

begin
  sett := TMMLSettings.Create(TreeView1.Items);
  sett.CreateKey('Settings/FullScreen/wat', true);
  sett.SetKeyValue('Settings/FullScreen/wat', 'TRUE');
  writeln(sett.GetKeyValue('Settings/FullScreen/wat'));

  writeln(sett.GetSetDefaultKeyValue('Settings/FullScreen/wat2', 'OWAT'));

  writeln(sett.GetSetDefaultKeyValue('Settings/Component1/hoiii', 'NO U'));

  writeln(sett.GetSetDefaultKeyValue('Settings/Component2/hoi', 'OK'));
  s := sett.ListKeys('Settings/FullScreen');
  for i := 0 to high(s) do
    writeln(s[i]);


 { ss:='Settings' + '/' + s[0];
  while length(s) > 0 do
  begin
    s := sett.ListKeys(ss);
    if(length(s)>0) then
      ss:=ss +'/' + s[0];
  end;   }

  sett.SaveToXML('settings.xml');
  sett.Free;
  //SaveToXML('settings.xml');
end;

procedure TForm1.LoadButtonClick(Sender: TObject);
var
  s: TMMLSettings;
begin
  if not FileExists('settings.xml') then
  begin
    TreeView1.Items.Clear;
    s := TMMLSettings.Create(TreeView1.Items);
    s.SaveToXML('settings.xml');
    s.Free;
  end;

  TreeView1.Items.Clear;
  s := TMMLSettings.Create(TreeView1.Items);
  s.LoadFromXML('settings.xml');
end;

initialization
  {$I unit1.lrs}

end.

