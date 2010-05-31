library libmml;

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client,sysutils,MufasaTypes,dtmutil;

{$R *.res}

type
  PTPoint = ^TPoint;


Const
  RESULT_OK = 0;
  RESULT_ERROR = -1;

  MOUSE_UP = 0;
  MOUSE_DOWN = 1;

var
  C: TClient;
  gr: Pointer;
  last_error: PChar;


function init: integer; cdecl;
begin
  C:=TClient.Create('');
  result:=0;
end;

{ Mouse }

function getMousePos(var t: tpoint): integer; cdecl;

begin
  try
    C.IOManager.GetMousePos(t.x,t.y);
    result := RESULT_OK;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
end;

function setMousePos(var t: tpoint): integer; cdecl;
begin
  try
    C.IOManager.MoveMouse(t.x,t.y);
    result := RESULT_OK;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
end;

function ConvIntClickType(Int : Integer) : TClickType;inline;
begin
  case int of
    0 : result := mouse_Left;
    1 : result := mouse_Right;
    2: result := mouse_Middle;
  end;
end;

function getMouseButtonState(But: Integer): Integer;
begin
  try
    if C.IOManager.IsMouseButtonDown(ConvIntClickType(But)) then
      result := MOUSE_DOWN;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
end;

function setMouseButtonState(But, State, X, Y: Integer): Integer;
begin
  try
    if State = MOUSE_UP then
    begin
      C.IOManager.ReleaseMouse(X, Y, ConvIntClickType(But));
      result := RESULT_OK;
    end else if state = MOUSE_DOWN then
    begin
      C.IOManager.HoldMouse(X, Y, ConvIntClickType(But));
      result := RESULT_OK;
    end;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
end;


function findColor(var x, y: integer; color, x1, y1, x2, y2: integer): boolean;
begin
  C.MFinder.FindColor(x, y, color, x1, y1, x2, y2);
end;

function returnpoints: PTPoint;  cdecl;

begin
  result := AllocMem(sizeof(TPoint) * 2);
  result[0].x := 5;
  result[0].y := 10;
  result[1].x := 20;
  result[1].y := 30;
end;

function printpoints(b: PTPoint; len: integer): boolean;   cdecl;
var i:integer;
begin
  for i := 0 to len - 1 do
    writeln('X, Y: (' + inttostr(b[i].x) + ', ' + inttostr(b[i].y) + ')');
end;

procedure hoi(var i: integer);  cdecl;
begin
  i := i + 1;
end;

{function givedtm:PPDTM;   cdecl;
var
  dtm: PPDTM;
begin
  writeln('Size: ' + inttostr(sizeof(pdtm)));
  writeln('Size: ' + inttostr(sizeof(ptruint)));
  dtm := AllocMem(sizeof(pdtm));
  initdtm(dtm^,2);
  result:=dtm;
  dtm^.n := PChar('wat');
end;

function givedtm2:PDTM; cdecl;

var
  dtm: pdtm;
begin
  initdtm(dtm,2);
  result:=dtm;
  //result.n := PChar('wat');
  //writeln('woohoo');
end;                }

function returnarray: tpointarray;  cdecl;
var
  i:integer;
begin
  setlength(result,5);
  for i := 0 to high(result) do
    result[i] := Point(i * 50, i + 50);
  writeln('res: ' + IntToStr(PtrUInt(result)));
  gr := @result[0];
end;

procedure printarray2(var arr: TPointArray); cdecl;
var i:integer;
begin
  for i := 0 to high(arr) do
    writeln(inttostr(arr[i].x) + ',' + inttostr(arr[i].y));
  setlength(arr,0);
  writeln('GR: ' + inttostr(tpoint(tpointarray(gr)[0]).y));
end;

procedure printarray(arr: PTPoint); cdecl;
var
  i:integer;
  arr2: TPointArray;
begin
  writeln('arr: ' + IntToStr(PtrUInt(@arr[0])));
  setlength(arr2,0);
  arr2 := @arr[0];
  writeln('arr2: ' + IntToStr(PtrUInt(@arr2[0])));
 { for i := 0 to 4 do
    writeln(inttostr(arr[i].x) + ',' + inttostr(arr[i].y));

  writeln(length(arr2));
  for i := 0 to high(arr2) do
    writeln(inttostr(arr2[i].x) + ',' + inttostr(arr2[i].y)); }

  printarray2(arr2);
  writeln(inttostr(length(arr2)));
  writeln(inttostr(arr[0].x) + ',' + inttostr(arr[0].y));
end;

procedure fpc_freemem_(p:pointer); cdecl;
begin
  writeln('free: ' + inttostr(qword(p)));
  freemem(pointer(ptruint(p)));
end;

function fpc_allocmem_(size: ptruint): pointer; cdecl;
begin
  result:=AllocMem(size);
  writeln('alloc: ' + inttostr(qword(result)));
end;

function fpc_reallocmem_(size: ptruint; ptr: pointer): pointer;
begin
  result:=ReAllocMem(ptr, size);
end;


exports
  init,
  { Mouse }
  getMousePos,
  setMousePos,
  getMouseButtonState,
  setMouseButtonState,

  { Finder }
  findColor,

  returnpoints,
  printpoints,
  hoi,
  returnarray,
  printarray,
  fpc_freemem_,
  fpc_allocmem_,
  fpc_reallocmem_;


begin
end.

