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

function set_target_array(C: TClient; P, w, h: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.SetTarget(PRGB32(P), classes.Point(w, h));
      result := RESULT_OK; // TODO: Should this check to see if test is valid? > -1?
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function set_target_bitmap(C: TClient; bitmap: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.SetTarget(C.MBitmaps[bitmap]);
      result := RESULT_OK; // TODO: Should this check to see if test is valid? > -1?
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function set_eios_target(C: TClient; name, args: PChar; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.SetTarget(name, args);
      result := RESULT_OK; // TODO: Should this check to see if test is valid? > -1?
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function mouse_set_client_area(C: TClient; x1, y1, x2, y2: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.MouseSetClientArea(x1, y1, x2, y2); // TODO: Should this check to see if test is valid? > -1?
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function mouse_reset_client_area(C: TClient): Integer; cdecl;
begin
  try
    begin
      C.IOManager.MouseResetClientArea();
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function image_set_client_area(C: TClient; x1, y1, x2, y2: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.ImageSetClientArea(x1, y1, x2, y2); // TODO: Should this check to see if test is valid? > -1?
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function image_reset_client_area(C: TClient): Integer; cdecl;
begin
  try
    begin
      C.IOManager.ImageResetClientArea();
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: Why is SetImageTarget a procedure? Why doesn't it just return the id?
function set_image_target(C: TClient; var idx: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.SetImageTarget(idx);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: Why is SetKeyMouseTarget a procedure? Why doesn't it just return the id?
function set_key_mouse_target(C: TClient; var idx: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.SetKeyMouseTarget(idx);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_image_target(C: TClient; var test: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.GetImageTarget(test); // TODO: Should this check to see if test is valid? > -1?
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_key_mouse_target(C: TClient; var test: Integer): Integer; cdecl;
begin
  try
    begin
      C.IOManager.GetKeyMouseTarget(test);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function export_image_target(C: TClient; var test: TTarget_Exported): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.ExportImageTarget; // TODO: Should this check to see if test is valid? > -1?
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function export_key_mouse_target(C: TClient; var test: TTarget_Exported): Integer; cdecl;
begin
  try
    begin
      test := C.IOManager.ExportKeyMouseTarget;
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
