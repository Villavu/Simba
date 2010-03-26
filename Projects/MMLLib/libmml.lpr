library libmml;

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client,sysutils,MufasaTypes,dtmutil;

{$R *.res}

type
  PTPoint = ^TPoint;
  PPDTM = ^PDTM;

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

function returnpoints: PTPoint;

begin
  result := AllocMem(sizeof(TPoint) * 2);
  result[0].x := 5;
  result[0].y := 10;
  result[1].x := 20;
  result[1].y := 30;
end;

function printpoints(b: PTPoint; len: integer): boolean;
var i:integer;
begin
  for i := 0 to len - 1 do
    writeln('X, Y: (' + inttostr(b[i].x) + ', ' + inttostr(b[i].y) + ')');
end;

procedure hoi(var i: integer);
begin
  i := i + 1;
end;

function givedtm:PPDTM;
var
  dtm: PPDTM;
begin
  dtm := AllocMem(sizeof(pdtm));
  initdtm(dtm^,2);
  result:=dtm;
  dtm^.n := PChar('wat');
end;

exports
  test,
  init,
  getmousepos,
  returnpoints,
  printpoints,
  hoi,
  givedtm;


begin
end.

