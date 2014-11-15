(*
Mouse
=====

Mouse functions in libMML.

*)

(*
get_mouse_pos
-------------


Returns mouse position of client C to point t
*)

{$H-}

function get_mouse_pos(C: TClient; var t: tpoint): integer; cdecl;

begin
  if not validate_client(C) then
    exit(RESULT_ERROR);

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

(*
set_mouse_pos
-------------

Set mouse position of client C to point t
*)
function set_mouse_pos(C: TClient; var t: tpoint): integer; cdecl;
begin
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
    2 : result := mouse_Middle;
  end;
end;

{ Return the state of a mouse button given client C }
function get_mouse_button_state(C: TClient; But: Integer): Integer;  cdecl;
begin
  try
    if C.IOManager.IsMouseButtonDown(ConvIntClickType(But)) then
      result := MOUSE_DOWN
    else
      result := MOUSE_UP;
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

function key_down(C: TClient; key: Word): Integer; cdecl;
begin
  try
    begin
      C.IOManager.KeyDown(key);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

function key_up(C: TClient; key: Word): Integer; cdecl;
begin
  try
    begin
      C.IOManager.KeyUp(key);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

function send_keys(C: TClient; s: PChar; keywait, keymodwait: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.SendText(s, keywait, keymodwait);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

function key_press(C: TClient; key: Word): Integer; cdecl;
begin
  try
    begin
      C.IOManager.PressKey(key);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

function is_key_down(C: TClient; key: Word; out test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.isKeyDown(key);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;

function get_key_code(C: TClient; key: char; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.GetKeyCode(key);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      result := RESULT_ERROR;
      set_last_error(e.Message);
    end;
  end;
end;
