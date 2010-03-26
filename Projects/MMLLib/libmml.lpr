library libmml;

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client;

{$R *.res}

var
  C: TClient;
function test: pchar;
begin
  result := PChar('hello world');
end;

procedure init;
begin
  C:=TCLient.Create('');
end;

function getmousepos: tpoint;
begin
  C.IOManager.GetMousePos(result.x,result.y);
end;



exports
  test,
  init,
  getmousepos;


begin
end.

