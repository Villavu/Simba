(*
OCR Functions
=============

*)

function rs_get_up_text(C: TClient; var test: String): Integer; cdecl;
begin
  try
    begin
      test := C.MOCR.GetUpTextAt(7, 7, true);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function rs_get_up_text_at_ex(C: TClient; x, y: Integer; shadow: Boolean;
                              fontname: String; var test: String): Integer; cdecl;
begin
  try
    begin
      test := C.MOCR.GetUpTextAtEx(x, y, shadow, fontname);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function rs_get_up_text_at(C: TClient; x, y: Integer; test: String): Integer; cdecl;
begin
  try
    begin
      test := C.MOCR.GetUpTextAt(x, y, true);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

//function rs_bitmap_from_text(C: TClient; text, font: String; test: Integer): Integer; cdecl;
//var
//  bmp: TMufasaBitmap;
//begin
//  try
//    begin
//      bmp := C.MOCR.TextToFontBitmap(text, font);
//      test := C.MBitmaps.AddBMP(bmp);
//      result := RESULT_OK;
//    end;
//  except on e : Exception do
//    begin
//      set_last_error(e.Message);
//      result := RESULT_ERROR;
//    end;
//  end;
//end;

