unit TestUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Client, MufasaTypes;

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
   w,h, x, y, xx, yy:integer;
   bmp: TBitmap;
   ptr: PRGB32;
   t:integer;

begin
  Client := TClient.Create;
  Client.MWindow.GetDimensions(w, h);
  writeln(inttostr(w) + ' , ' + inttostr(h));

  bmp := Client.MWindow.CopyClientToBitmap(0, 0, w, h);
  bmp.SaveToFile('/tmp/test.bmp');
  bmp.Free;

  Client.MInput.GetMousePos(x, y);
  writeln(inttostr(x) + ' , ' + inttostr(y));

  Client.MInput.SetMousePos(50, 50);
  Client.MInput.GetMousePos(x, y);
  writeln(inttostr(x) + ' , ' + inttostr(y));

  ptr := Client.MWindow.ReturnData(0, 0, w, h);
  for yy := 0 to h - 1 do
    for xx := 0 to w - 1 do
    begin
      { Do comparison here }
      inc(ptr);
    end;
  Client.MWindow.FreeReturnData;

  Client.Destroy;
  writeln('Test completed successfully');
end;

initialization
  {$I testunit.lrs}

end.

