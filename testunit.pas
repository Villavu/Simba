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

type
    TMyThread = class(TThread)
    private
    protected
      procedure Execute; override;
    public
      Constructor Create(CreateSuspended : boolean);
    end;

  constructor TMyThread.Create(CreateSuspended : boolean);
  begin
    FreeOnTerminate := True;
    inherited Create(CreateSuspended);
  end;

procedure TMyThread.Execute;
Var
   Client: TClient;
   w,h, x, y, xx, yy:integer;
   bmp: TBitmap;
   ptr: PRGB32;

begin
  while (not Terminated)  do
  begin

  Writeln('Creating the client');
  Client := TClient.Create;
  Writeln('Getting the dimensions');
  Client.MWindow.GetDimensions(w, h);
  writeln(inttostr(w) + ' , ' + inttostr(h));
  Writeln('Setting target');
  Client.MWindow.SetTarget(132840,w_window);
  Client.MWindow.ActivateClient;
  Client.MWindow.GetDimensions(w, h);
  Writeln('Copying BMP');
  bmp := Client.MWindow.CopyClientToBitmap(0, 0, w, h);
  bmp.SaveToFile('c:\test.bmp');
  bmp.Free;

 //Sleep(1000);
  Client.MInput.GetMousePos(x, y);
  writeln(inttostr(x) + ' , ' + inttostr(y));

  Client.MInput.SetMousePos(50, 50);
  Client.MInput.GetMousePos(x, y);
  writeln(inttostr(x) + ' , ' + inttostr(y));

  Client.MInput.ClickMouse(60, 60, mouse_Right);

  ptr := Client.MWindow.ReturnData(0, 0, w, h);
  for yy := 0 to h - 1 do
    for xx := 0 to w - 1 do
    begin
      { Do comparison here }
      inc(ptr);
    end;

  Client.MWindow.FreeReturnData;

  Client.MInput.IsMouseButtonDown(mouse_Left);
  Sleep(1000);
  if Client.MInput.IsMouseButtonDown(mouse_Left) then
    writeln('Left mouse is down!');
  if Client.MInput.IsMouseButtonDown(mouse_Right) then
    writeln('Right mouse is down!');
  if Client.MInput.IsMouseButtonDown(mouse_Middle) then
    writeln('Middle mouse is down!');
  Client.Destroy;
  writeln('Test completed successfully');
  break;
  end;
end;


{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
Var
   MyThread: TMyThread;

begin
  MyThread := TMyThread.Create(True);
  MyThread.Resume;
end;

initialization
  {$I testunit.lrs}

end.

