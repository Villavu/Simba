library libmml;

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client,sysutils,MufasaTypes,dtmutil;

{$R *.res}

type
  PTPoint = ^TPoint;


Const
  RESULT_OK = 0;
  RESULT_FALSE = 1;
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

function findColor(var x, y: integer; color, x1, y1, x2, y2: integer): integer;
begin
  try
    if C.MFinder.FindColor(x, y, color, x1, y1, x2, y2) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
end;

function findColorTolerance(var x, y: integer; color, tol, x1, y1, x2, y2: integer): integer;
begin
  try
    if C.MFinder.FindColorTolerance(x, y, color, x1, y1, x2, y2, tol) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
end;

function findColors(var ptr: PTPoint; color, x1, y1, x2, y2: integer): integer;
var
  TPA: TPointArray;
begin
  try
    C.MFinder.FindColors(TPA, color, x1, y1, x2, y2);
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
  ptr := AllocMem(sizeof(tpoint) * (length(TPA) + 1));
  PInteger(ptr)[0] := length(TPA);
  Move(TPA[0], ptr[1], length(TPA)*sizeof(tpoint));
end;

function findColorsTolerance(var ptr: PTPoint; color, tol, x1, y1, x2, y2: integer): integer;
var
  TPA: TPointArray;
begin
  try
    C.MFinder.FindColorsTolerance(TPA, color, x1, y1, x2, y2, tol);
  except on e : Exception do
  begin
    result := RESULT_ERROR;
    last_error := PChar(e.Message);
  end;
  end;
  ptr := AllocMem(sizeof(tpoint) * (length(TPA) + 1));
  PInteger(ptr)[0] := length(TPA);
  Move(TPA[0], ptr[1], length(TPA)*sizeof(tpoint));
end;

procedure fpc_freemem_(p:pointer); cdecl;
begin
  freemem(pointer(ptruint(p)));
end;

function fpc_allocmem_(size: ptruint): pointer; cdecl;
begin
  result:= AllocMem(size);
end;

function fpc_reallocmem_(size: ptruint; ptr: pointer): pointer;
begin
  result:= ReAllocMem(ptr, size);
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
  findColors,
  findColorTolerance,
  findColorsTolerance,

  { Mem Management }
  fpc_freemem_,
  fpc_allocmem_,
  fpc_reallocmem_;


begin
end.

