library libmml;

{$mode objfpc}{$H+}

uses
  cmem,Classes,interfaces,graphics,client,sysutils,MufasaTypes,dtmutil, dtm;

//{$R *.res}

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
        // FIXME UINT negative
        result := PtrUInt(RESULT_ERROR);
        set_last_error(e.Message);
    end;
    writeln(format('C: %d, IOManager: %d', [PtrUInt(C), PtrUInt(C.IOManager)]));
end;

{ Destroy a TClient }
function destroy_client(C: TClient): integer; cdecl;
begin
    try
        C.Free;
    except on e : Exception do
        result := RESULT_ERROR;
        set_last_error(e.message):
    end;
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
        result := RESULT_ERROR;
        set_last_error(e.Message);
    end;
end;

{ Set mouse position of client C to point t }
function set_mouse_pos(C: TClient; var t: tpoint): integer; cdecl;
begin
    try
        C.IOManager.MoveMouse(t.x,t.y);
        result := RESULT_OK;
    except on e : Exception do
        result := RESULT_ERROR;
        set_last_error(e.Message);
    end;
end;


{ Helper function }
function ConvIntClickType(Int : Integer) : TClickType; inline;
begin
  case int of
    0 : result := mouse_Left;
    1 : result := mouse_Right;
    2 : result := mouse_Middle;
  end;
end;

{ Return the state of a mouse button given client C }
function get_mouse_button_state(C: TClient; But: Integer): Integer;  cdecl;
begin
    try
        if C.IOManager.IsMouseButtonDown(ConvIntClickType(But)) then
            result := MOUSE_DOWN;
        else
            result := MOUSE_UP;
    except on e : Exception do
        result := RESULT_ERROR;
        set_last_error(e.Message);
    end;
end;

{ Set the state of a mouse button given client C }
function set_mouse_button_state(C: TClient; But, State, X, Y: Integer): Integer;  cdecl;
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
        result := RESULT_ERROR;
        set_last_error(e.Message);
    end;
end;


{ Colour }

function get_color(C: TClient; x, y: Integer;
                   out color: Integer): Integer; cdecl;
begin
    try
        color := C.IOManager.GetColor(x, y);
        if color > -1 then
            result := RESULT_OK
        else
            result := RESULT_FALSE;
    except on e : Exception do
        set_last_error(e.message);
        result := RESULT_ERROR;
    end;
end;

{ Find color on client C in area (x1,y1,x2,y2) and return coordinate (if any) in x, y }
function find_color(C: TClient; var x, y: Integer;
                    color, x1, y1, x2, y2: Integer): Integer; cdecl;
begin
    try
        if C.MFinder.FindColor(x, y, color, x1, y1, x2, y2) then
            result := RESULT_OK
        else
            result := RESULT_FALSE;
    except on e : Exception do
        set_last_error(e.Message);
        result := RESULT_ERROR;
    end;
end;

function find_color_tolerance(C: TClient; var x, y: Integer; color: Integer;
                              tol, x1, y1, x2, y2: Integer): Integer;  cdecl;

begin
    try
        if C.MFinder.FindColorTolerance(x, y, color, x1, y1, x2, y2, tol) then
            result := RESULT_OK
        else
            result := RESULT_FALSE;
    except on e : Exception do
        set_last_error(e.Message);
        result := RESULT_ERROR;
    end;
end;

function find_color_tolerance_optimised(C: TClient; var x, y: Integer;
                                        var len: Integer; col: Integer;
                                        x1, y1, x2, y2: Integer;
                                        tol: Integer): Integer; cdecl;
begin
    try
        if C.MFinder.FindColorToleranceOptimised(x, y, col, x1, y1, x2, y2,
                                                 tol) then
            result := RESULT_OK
        else
            result := RESULT_FALSE;
    except on e : Exception do
        set_last_error(e.message);
        result := RESULT_ERROR;
    end;
end;

function find_colors(C: TClient; var ptr: PPoint; var len: Integer;
                     color, x1, y1, x2, y2: Integer): Integer;  cdecl;
var
  TPA: TPointArray;
begin
    setlength(TPA, 0);
    try
        C.MFinder.FindColors(TPA, color, x1, y1, x2, y2);
    except on e : Exception do
        set_last_error(e.Message);
        result := RESULT_ERROR;
    end;
    
    len := Length(TPA);
    if len > 0 then
      result := RESULT_OK
    else
      setlength(tpa, 0);
      exit(RESULT_FALSE);
    
    ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
    setlength(tpa, 0);
end;

function find_colors_tolerance(C: TClient; var ptr: PPoint; var len: Integer;
               color, tol, x1, y1, x2, y2: Integer): Integer;  cdecl;
var
  TPA: TPointArray;
begin
  try
    C.MFinder.FindColorsTolerance(TPA, color, x1, y1, x2, y2, tol);
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;

  len := Length(TPA);
  if len > 0 then
    result := RESULT_OK
  else
    setlength(tpa, 0);
    exit(RESULT_FALSE);

  ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
  setlength(TPA, 0);
end;

function find_colors_tolerance_optimised(C: TClient; var ptr: PPoint;
                                         var len: Integer; col: Integer;
                                         x1, y1, x2, y2: Integer;
                                         tol: Integer): Integer; cdecl;
var
  TPA: TPointArray;
begin
  try
    C.MFinder.FindColorsToleranceOptimised(TPA, col, x1, y1, x2, y2, tol);
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;

  len := Length(TPA);
  if len > 0 then
    result := RESULT_OK
  else
    setlength(tpa, 0);
    exit(RESULT_FALSE);

  ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
  setlength(TPA, 0);
end;

function similar_colors(C: TClient; col1, col2, tol: Integer): Integer; cdecl;
begin
  if C.MFinder.SimilarColors(col1, col2, tol) then
    result := RESULT_OK
  else
    result := RESULT_FALSE;
end;

function count_color(C: TClient; out count: Integer;
                     Color, xs, ys, xe, ye: Integer): Integer; cdecl;
begin
  try
    begin
      count := C.MFinder.CountColor(Color, xs, ys, xe, ye);
      if count > 0 then
        result := RESULT_OK
      else
        result := RESULT_FALSE;
    end;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function count_color_tolerance(C: TClient; out count: Integer; col: Integer;
                               xs, ys, xe, ye, tol: Integer): Integer; cdecl;
begin
  try
    count := C.MFinder.CountColorTolerance(col, xs, ys, xe, ye, tol);
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
  if count > 0 then
    result := RESULT_OK
  else
    result := RESULT_FALSE;
end;

function find_color_spiral(C: TClient; var x, y: Integer;
                           col, xs, ys, xe, ye: Integer): Integer; cdecl;
begin
  try
    if C.MFinder.FindColorSpiral(x, y, col, xs, ys, xe, ye) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_color_spiral_tolerance(C: TClient; var x, y: Integer;
                                     col, xs, ys, xe, ye: Integer;
                                     tol: Integer): Integer; cdecl;
begin
  try
    if C.MFinder.FindColorSpiralTolerance(x, y, col, xs, ys, xe, ye, tol) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_colored_area(C: TClient; var x, y: Integer;
                           col, xs, ys, xe, ye, minA: Integer): Integer; cdecl;
begin
  try
    if C.MFinder.FindColoredArea(x, y, col, xs, ys, xe, ye, minA) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_colored_area_tolerance(C: TClient; var x, y: Integer;
                                     col, xs, ys, xe, ye, minA: Integer;
                                     tol: Integer): Integer; cdecl;
begin
  try
    if C.MFinder.FindColoredAreaTolerance(x, y, col,
                                          xs, ys, xe, ye, minA, tol) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function set_tolerance_speed(C: TClient; nCTS: Integer): Integer; cdecl;
begin
  try
    begin
      C.MFinder.SetToleranceSpeed(nCTS);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_tolerance_speed(C: TClient; out cts: Integer): Integer; cdecl;
begin
  try
    begin
    cts := C.MFinder.GetToleranceSpeed;
    result := RESULT_OK;
    end
  except on e: Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
end;

function set_tolerance_speed_2_modifiers(C: TClient;
                                         nHue, nSat: Extended): Integer; cdecl;
begin
  try
    begin
      C.MFinder.SetToleranceSpeed2Modifiers(nHue, nSat);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_tolerance_speed_2_modifiers(C: TClient; out hueMod: Extended;
                                         out satMod: Extended): Integer; cdecl;
var
  h, s: Extended;
begin
  try:
    begin
      C.MFinder.GetToleranceSpeed2Modifiers(h, s);
      hueMod := h;
      satMod := s;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
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
  if not assigned(DTM) then
  begin
    set_last_error('DTM is NULL');
    exit(RESULT_ERROR);
  end;

  try:
  begin
    index := C.MDTMs.AddDTM(DTM);
    exit(RESULT_OK);
  end;
  except on e : Exception do
  begin
    exit(RESULT_ERROR);
  end;
end;

{ Remove a previously added DTM from the DTM manager. This also frees the DTM }
function remove_dtm(C: TClient; DTMi: integer): integer; cdecl;
begin
  C.MDTMs.FreeDTM(DTMi);
end;

{ Find a DTM given DTM index i, client C in area x1,y1,x2,y2. Return coord at x, y. }
function find_dtm(C: TClient; DTMi: integer; var x, y: integer; x1, y1, x2, y2: integer): integer; cdecl;
var
  res: boolean;
begin
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

{ Find a DTM given DTM index i, client C in area x1,y1,x2,y2. Return coord at x, y. }
function find_dtms(C: TClient; DTMi: integer; ptr: PPoint; x1, y1, x2, y2: integer): integer; cdecl;
var
  res: boolean;
  TPA: TPointArray;
begin
  try
    res := C.MFinder.FindDTMs(C.MDTMs.DTM[DTMi], TPA, x1, y1, x2, y2);
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;

  len := Length(TPA);
  if len > 0 then
    result := RESULT_OK
  else
    setlength(tpa, 0);
    exit(RESULT_FALSE);
    
  ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
  setlength(TPA, 0);
end;

function set_array_target(C: TClient; Arr: PRGB32; Size: TPoint): integer; cdecl;
begin
  if not assigned(Arr) then
  begin
    set_last_error('Arr is not assigned');
    exit(RESULT_FALSE);
  end;

  // FIXME: Catch exceptions.
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
