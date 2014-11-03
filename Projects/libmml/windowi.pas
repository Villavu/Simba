(*
Window Functions
================

*)

function set_desktop_as_client(C: TClient): Integer; cdecl;
begin
  try
    begin
      C.IOManager.SetDesktop();
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function free_target(C: TClient; idx: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.FreeTarget(idx);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_client_dimensions(C: TClient; var test_w, test_h: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.GetDimensions(test_w, test_h);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_client_position(C: TClient; var test_top, test_left: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.GetPosition(test_top, test_left);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function freeze(C: TClient): Integer; cdecl;
begin
  try
    begin
      C.IOManager.SetFrozen(true);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function unfreeze(C: TClient): Integer; cdecl;
begin
  try
    begin
      C.IOManager.SetFrozen(false);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function activate_client(C: TClient): Integer; cdecl;
begin
  try
    begin
      C.IOManager.ActivateClient();
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function is_target_valid(C: TClient; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.TargetValid;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_native_window(C: TClient; test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.GetImageTarget.GetHandle();
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;
