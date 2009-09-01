unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Client;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
Var
   Client: TClient;
   w,h:integer;
   bmp: TBitmap;

begin
  Client := TClient.Create;
  Client.MWindow.GetDimensions(w, h);
  writeln(inttostr(w) + ' , ' + inttostr(h));

  bmp := Client.MWindow.CopyClientToBitmap(0, 0, w, h);
  //writeln(inttostr(bmp.Width));
  bmp.SaveToFile('/tmp/test.bmp');
  bmp.Free;

  Client.Destroy;
end;

initialization
  {$I unit1.lrs}

end.

