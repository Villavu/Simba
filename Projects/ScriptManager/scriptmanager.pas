unit scriptmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, settings, MufasaTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    ListView1: TListView;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TSimbaScript = class(TObject)

  public
    Name, Version, Author, Description: String;
    Tags, Files: TStringArray;

  private

  public
    constructor Create(n: TTreeNode);
    destructor Delete;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }
procedure fill(s: TMMLSettings);
var
  i:integer;
  n, nn: TTreeNode;
  ss: TSimbaScript;
begin
  n := s.WalkToNode('Scripts/ScriptList/');
  nn := n.GetFirstChild;
  while nn <> nil do
  begin
    ss := TSimbaScript.Create(nn);
    nn := nn.GetNextSibling;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: TMMLSettings;
begin
  s := TMMLSettings.Create(TreeView1.Items);
  s.LoadFromXML('/scratch/gittest/list.xml');
  fill(s);
  s.Free;
end;

{ TSimbaScript }

constructor TSimbaScript.Create(n: TTreeNode);
begin

end;

destructor TSimbaScript.Delete;
begin

end;

end.

