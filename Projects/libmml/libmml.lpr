library libmml;

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client,sysutils,MufasaTypes,dtmutil, dtm;

{$R *.res}

Const
  RESULT_OK = 0;
  RESULT_FALSE = 1;
  RESULT_ERROR = -1;

  MOUSE_UP = 0;
  MOUSE_DOWN = 1;

var
  last_error: String;
  debug: boolean;

 { This must be called on Library load }

function init: integer;  cdecl;
begin
  last_error := '';
  debug := true;
  result := RESULT_OK;
end;

procedure set_last_error(s: string);
begin
  last_error := s;
  if debug then
    writeln('ERROR: ' + s);
end;

{ Validate the TClient. If it is NULL, set last error and return false }
function validate_client(C: TClient): boolean; inline;
begin
  result := Assigned(C);
  if not result then
  begin
    last_error := 'PClient is NULL';
    if debug then
      writeln(last_error);
  end;
end;

{
  Create a TClient.
  You can use multiple, but you'll have to manage them yourself.
}
function create_client: PtrUInt; cdecl;
var
  C: TClient;
begin
  try
    C := TClient.Create('');
    Result := PtrUInt(C);
  except on e : Exception do
    begin
      // FIXME UINT negative
      result := PtrUInt(RESULT_ERROR);
      set_last_error(e.Message);
    end;
  end;
  writeln(format('C: %d, IOManager: %d', [PtrUInt(C), PtrUInt(C.IOManager)]));
end;

{ Destroy a TClient }
function destroy_client(C: TClient): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  C.Free;
end;

{ Set (verbose) debug on/off }
procedure set_debug(v: Boolean); cdecl;
begin
  debug := v;
end;

{ Get debug }
function get_debug: boolean; cdecl;
begin
  exit(debug);
end;

{
  VERY IMPORTANT: If you use get_last_error, you must immediately store the
  resulting string somewhere else. As soon as you do other calls, the last error
  may be reset or assigned a different memory position, making your old
  pointer invalid.
}
function get_last_error: pchar; cdecl;
begin
  exit(@last_error[1]);
end;

{ Turn an array into a pointer. The pointer memory is not managed by FPC, so we can pass
  it along happily. It'll have to be freed by the external control though }
function array_to_ptr(ptr: Pointer; size: PtrUInt; objsize: PtrUInt): Pointer; cdecl;
begin
  result := GetMem(objsize * size);
  Move(ptr^, result^, objsize * size);
end;

{ Free memory previously allocated by libMML }
function free_ptr(ptr: pointer): boolean; cdecl;
begin
  result := Assigned(ptr);
  if not result then
  begin
    set_last_error('TClient is NULL');
    if debug then
      writeln(last_error);
  end else
    FreeMem(ptr);
end;

{ Allocate memory with libMML }
function alloc_mem(size, objsize: PtrUInt): Pointer; cdecl;
begin
  result := GetMem(size * objsize);
end;

{ Reallocate memory with libMML }
function realloc_mem(ptr: Pointer; size, objsize: PtrUInt): Pointer; cdecl;
begin
  result := ReAllocMem(ptr, size*objsize);
end;

{ Mouse }

{ Returns mouse position of client C to point t }
function get_mouse_pos(C: TClient; var t: tpoint): integer; cdecl;

begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    C.IOManager.GetMousePos(t.x,t.y);
    result := RESULT_OK;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

{ Set mouse position of client C to point t }
function set_mouse_pos(C: TClient; var t: tpoint): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    C.IOManager.MoveMouse(t.x,t.y);
    result := RESULT_OK;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;


{ Helper function }
function ConvIntClickType(Int : Integer) : TClickType; inline;
begin
  case int of
    0 : result := mouse_Left;
    1 : result := mouse_Right;
    2: result := mouse_Middle;
  end;
end;

{ Return the state of a mouse button given client C }
function get_mouse_button_state(C: TClient; But: Integer): Integer;  cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    if C.IOManager.IsMouseButtonDown(ConvIntClickType(But)) then
      result := MOUSE_DOWN;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

{ Set the state of a mouse button given client C }
function set_mouse_button_state(C: TClient; But, State, X, Y: Integer): Integer;  cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

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
      set_last_error(e.Message);
    end;
  end;
end;


{ Colour }

function get_color(C: TClient; x, y: integer; var color: integer): integer;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    color := C.IOManager.GetColor(x, y);
  except on e : exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;

  result := RESULT_OK;
end;

{ Find color on client C in area (x1,y1,x2,y2) and return coordinate (if any) in x, y }
function find_color(C: TClient; var x, y: integer; color, x1, y1, x2, y2: integer): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    if C.MFinder.FindColor(x, y, color, x1, y1, x2, y2) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;


function find_color_tolerance(C: TClient; var x, y: integer; color, tol, x1, y1, x2, y2: integer): integer;  cdecl;

begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    if C.MFinder.FindColorTolerance(x, y, color, x1, y1, x2, y2, tol) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

function find_colors(C: TClient; var ptr: PPoint; var len: integer; color, x1, y1, x2, y2: integer): integer;  cdecl;
var
  TPA: TPointArray;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  SetLength(TPA, 0);
  try
    C.MFinder.FindColors(TPA, color, x1, y1, x2, y2);
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;

  len := Length(TPA);
  ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
  result := RESULT_OK;
  setlength(tpa, 0);
end;

function find_colors_tolerance(C: TClient; var ptr: PPoint; var len: Integer;
               color, tol, x1, y1, x2, y2: integer): integer;  cdecl;
var
  TPA: TPointArray;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    C.MFinder.FindColorsTolerance(TPA, color, x1, y1, x2, y2, tol);
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;

  len := Length(TPA);
  ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
  result := RESULT_OK;
end;

{ DTM }

{ FIXME: DTM has not been tested yet! }

{ Create a MDTM}
function create_dtm(PointLen: integer; Points: PMDTMPoint; DTM: TMDTM): integer; cdecl;
var
  i: integer;
begin
  DTM := TMDTM.Create;
  for i := 0 to PointLen - 1 do
    DTM.AddPoint(Points[i]);

  if DTM.Valid then
    exit(RESULT_OK);

  DTM.Free;
  set_last_error('Invalid DTM');
  result := RESULT_ERROR;
end;

{ Delete a MDTM. Don't delete it if it is managed! use remove_dtm instead }
function delete_dtm(C: TClient; DTM: TMDTM): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  if not assigned(DTM) then
  begin
    set_last_error('DTM is NULL');
    exit(RESULT_ERROR);
  end;

  DTM.Free;

  result := RESULT_OK;
end;

{ Add a previously created DTM to the DTM Manager }
function add_dtm(C: TClient; DTM: TMDTM; var index: integer): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  if not assigned(DTM) then
  begin
    set_last_error('DTM is NULL');
    exit(RESULT_ERROR);
  end;

  index := C.MDTMs.AddDTM(DTM);
end;

{ Remove a previously added DTM from the DTM manager. This also frees the DTM }
function remove_dtm(C: TClient; DTMi: integer): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  C.MDTMs.FreeDTM(DTMi);
end;

{ Find a DTM given DTM index i, client C in area x1,y1,x2,y2. Return coord at x, y. }
function find_dtm(C: TClient; DTMi: integer; var x, y: integer; x1, y1, x2, y2: integer): integer; cdecl;
var
  res: boolean;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  try
    res := C.MFinder.FindDTM(C.MDTMs.DTM[DTMi], x, y, x1, y1, x2, y2);
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;

  if res then
    result := RESULT_OK
  else
    result := RESULT_FALSE;
end;

function set_array_target(C: TClient; Arr: PRGB32; Size: TPoint): integer; cdecl;
begin
  if not validate_client(C) then
  begin
    exit(RESULT_ERROR);
  end;

  if not assigned(Arr) then
  begin
    set_last_error('Arr is not assigned');
    exit(RESULT_FALSE);
  end;

  C.IOManager.SetTarget(Arr, Size);

  result := RESULT_OK;
end;

exports

  init,
  create_client,
  destroy_client,
  get_last_error,
  get_debug,
  set_debug,
  alloc_mem,
  realloc_mem,
  free_ptr,

  get_mouse_pos, set_mouse_pos,
  get_mouse_button_state, set_mouse_button_state,

  get_color,

  find_color, find_color_tolerance,

  find_colors, find_colors_tolerance,

  create_dtm, delete_dtm, add_dtm, remove_dtm,

  find_dtm,

  set_array_target;


begin
end.

