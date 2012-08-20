
(*
Color Functions
===============





*)

(*
get_color
---------


*)
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
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
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
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
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
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
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
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;

  len := Length(TPA);
  if len > 0 then
    result := RESULT_OK
  else
  begin
    setlength(tpa, 0);
    exit(RESULT_FALSE);
  end;

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
  begin
    setlength(tpa, 0);
    exit(RESULT_FALSE);
  end;

  ptr := array_to_ptr(Pointer(@TPA[0]), len, sizeof(TPoint));
  setlength(TPA, 0);
end;

function similar_colors(C: TClient; col1, col2, tol: Integer): Integer; cdecl;
begin
  try
    if C.MFinder.SimilarColors(col1, col2, tol) then
      result := RESULT_OK
    else
      result := RESULT_FALSE;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_FALSE;
    end;
  end;
end;

function count_color(C: TClient; out count: Integer;
                     Color, xs, ys, xe, ye: Integer): Integer; cdecl;
begin
  try
    count := C.MFinder.CountColor(Color, xs, ys, xe, ye);
    if count > 0 then
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
    if C.MFinder.FindColorSpiralTolerance(x, y, col, xs, ys, xe, ye,
      tol) then
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
    C.MFinder.SetToleranceSpeed(nCTS);
    result := RESULT_OK;
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
    cts := C.MFinder.GetToleranceSpeed;
    result := RESULT_OK;
  except on e: Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end
  end;
end;

function set_tolerance_speed_2_modifiers(C: TClient;
                                         nHue, nSat: Extended): Integer; cdecl;
begin
  try
    C.MFinder.SetToleranceSpeed2Modifiers(nHue, nSat);
    result := RESULT_OK;
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
  try
    C.MFinder.GetToleranceSpeed2Modifiers(h, s);
    hueMod := h;
    satMod := s;
    result := RESULT_OK;
  except on e : Exception do
    begin
      set_last_error(e.message);
      result := RESULT_ERROR;
    end;
  end;
end;

