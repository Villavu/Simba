(*
Bitmap Functions
================

*)

function create_bitmap_string(C: TClient; bmp: Integer; var test: PChar): Integer; cdecl;
var s: string;
begin
  try
    begin
      s := C.MBitmaps[bmp].ToString;
      test := @s[1];
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_mufasa_bitmap(C: TClient; bmp: Integer; var test: TMufasaBitmap): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps[bmp];
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function create_bitmap(C: TClient; w, h: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(w, h);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function free_bitmap(C: TClient; bmp: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps.FreeBMP(bmp);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function save_bitmap(C: TClient; bmp: Integer; path: PChar): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].SaveToFile(path);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_OK;
    end;
  end;
end;

// XXX: Not exported in PS - probably shouldn't be exported here either?
function set_persistent_memory_bitmap(C: TClient; bmp: Integer; mem: PtrUInt; awidth, aheight: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].SetPersistentMemory(mem, awidth, aheight);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: Not exported in PS - probably shouldn't be exported here either?
function reset_persistent_memory_bitmap(C: TClient; bmp: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].ResetPersistentMemory();
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function bitmap_from_string(C: TClient; width, height: Integer; data: PChar; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMPFromString(width, height, data);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function load_bitmap(C: TClient; path: PChar; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMPFromFile(path);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: Fix <= 0
function set_bitmap_size(C: TClient; bmp, newW, newH: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].SetSize(newW, newH);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: Fix <= 0
function stretch_bitmap_resize(C: TClient; bmp, newW, newH: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].StretchResize(newW, newH);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_bitmap_size(C: TClient; bmp: Integer; var bmpW, bmpH: Integer): Integer; cdecl;
begin
  try
    begin
      with C.MBitmaps[bmp] do
      begin
        bmpW := width;
        bmpH := height;
      end;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function set_bitmap_name(C: TClient; bmp: Integer; name: PChar): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].Name := name;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function create_mirrored_bitmap(C: TClient; bmp: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateMirroredBitmap(bmp, MirrorWidth);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function create_mirrored_bitmap_ex(C: TClient; bmp: Integer; MirrorStyle: TBmpMirrorStyle; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateMirroredBitmap(bmp, MirrorStyle);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function fast_get_pixel(C: TClient; bmp, x, y: Integer; var test: TColor): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps[bmp].FastGetPixel(x, y);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function fast_get_pixels(C: TClient; bmp: Integer; TPA: TPointArray; var test: TIntegerArray): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps[bmp].FastGetPixels(TPA);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_bitmap_area_colors(C: TClient; bmp, xs, ys, xe, ye: Integer; var test: T2DIntArray): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps[bmp].GetAreaColors(xs, ys, xe, ye);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// Why TColor?
function fast_set_pixel(C: TClient; bmp, x, y: Integer; color: TColor): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].FastSetPixel(x, y, color);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// TIA instead of TColorArray (or something?)
function fast_set_pixels(C: TClient; bmp: Integer; TPA: TPointArray; Colors: TIntegerArray): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].FastSetPixels(TPA, Colors);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// Why does this take an int for color?
function draw_tpa_bitmap(C: TClient; bmp: Integer; TPA: TPointArray; color: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].DrawTPA(TPA, Color);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function draw_atpa_bitmap(C: TClient; bmp: Integer; atpa: T2DPointArray): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].DrawATPA(atpa);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function draw_atpa_bitmap_ex(C: TClient; bmp: Integer; atpa: T2DPointArray; colors: TIntegerArray): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].DrawATPA(atpa, colors);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// TColor instead of Integer?
function fast_draw_clear(C: TClient; bmp: Integer; color: TColor): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].FastDrawClear(color);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function draw_bitmap(C: TClient; bmp: Integer; dest: TCanvas; x, y: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].DrawToCanvas(x, y, dest);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function fast_draw_transparent(C: TClient; x, y, src, tgt: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[src].FastDrawTransparent(x, y, C.MBitmaps[tgt]);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function set_transparent_color(C: TClient; bmp: Integer; color: TColor): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].SetTransparentColor(color);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function get_transparent_color(C: TClient; bmp: Integer; var test: TColor): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps[bmp].GetTransparentColor;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function fast_replace_color(C: TClient; bmp: Integer; old, new: TColor): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].FastReplaceColor(old, new);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function copy_client_to_bitmap(C: TClient; bmp, xs, ys, xe, ye: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].CopyClientToBitmap(C.IOManager, True, xs, ys, xe, ye);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function bitmap_from_client(C: TClient; xs, ys, xe, ye: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[test].CopyClientToBitmap(C.IOManager, True, xs, ys, xe, ye);
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_bitmap(C: TClient; bmp: Integer; var x, y: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindBitmap(C.MBitmaps[bmp], x, y);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_bitmap_in(C: TClient; bmp: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindBitmapIn(C.MBitmaps[bmp], x, y, xs, ys, xe, ye);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_bitmap_tolerance_in(C: TClient; bmp: Integer; var x, y: Integer; xs, ys, xe, ye, tol: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindBitmapToleranceIn(C.MBitmaps[bmp], x, y, xs, ys, xe, ye, tol);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_bitmap_spiral(C: TClient; bmp: Integer; var x, y: Integer; xs, ys, xe, ye: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindBitmapSpiral(C.MBitmaps[bmp], x, y, xs, ys, xe, ye);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_bitmaps_spiral_tolerance(C: TClient; bmp: Integer; var x, y: Integer; var pts: TPointArray; xs, ys, xe, ye, tol: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindBitmapsSpiralTolerance(C.MBitmaps[bmp], x, y, pts, xs, ys, xe, ye, tol);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_bitmap_spiral_tolerance(C: TClient; bmp: Integer; var x, y: Integer; xs, ys, xe, ye, tol: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindBitmapSpiralTolerance(C.MBitmaps[bmp], x, y, xs, ys, xe, ye, tol);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: Changed angle from an Extended to a Double, as Python's ctypes doesn't
// have a way (that I know of) to support 80-bit datatypes. C99 supports it
// but I don't think Python will. Hopefully 64 bits of data will suffice.
function rotate_bitmap(C: TClient; bmp: Integer; angle: Double; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].RotateBitmap(angle, C.MBitmaps[test]);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function desaturate(C: TClient; bmp: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].Desaturate(C.MBitmaps[test]);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function invert_bitmap(C: TClient; bmp: Integer): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].Invert;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function copy_bitmap(C: TClient; bmp: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CopyBMP(bmp);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// In PS's wrapper, this uses GreyScale(TMufasaBitmap) but uses the Invert
// method with no parameters. I don't think I like the in-place modification
// but I'm not sure which to use. For now I'm copying PS.
function grey_scale_bitmap(C: TClient; bmp: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].GreyScale(C.MBitmaps[test]);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function brightness_bitmap(C: TClient; bmp, br: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].Brightness(C.MBitmaps[test], br);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: co should be Extended but changed to a Double. Check rotate_bitmap above
function contrast_bitmap(C: TClient; bmp: Integer; co: Double; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].Contrast(C.MBitmaps[test], co);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function posterize_bitmap(C: TClient; bmp, po: Integer; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].Posterize(C.MBitmaps[test], po);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function create_mask_from_bitmap(C: TClient; bmp: Integer; var test: TMask): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps[bmp].CreateTMask;
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function find_mask_tolerance(C: TClient; mask: TMask; var x, y: Integer; xs, ys, xe, ye, tol, contourTol: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindMaskTolerance(mask, x, y, xs, ys, xe, ye, tol, contourTol);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: first param has name mask but is being used as a managed bitmap id?
function find_bitmap_mask_tolerance(C: TClient; mask: Integer; var x, y: Integer; xs, ys, xe, ye, tol, contourTol: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MFinder.FindMaskTolerance(C.MBitmaps[mask].CreateTMask, x, y, xs, ys, xe, ye, tol, contourTol);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: acc is Double, should be Extended.
function find_deformed_bitmap_tolerance_in(C: TClient; bmp: Integer; var x, y: Integer; xs, ys, xe, ye, tol, range: Integer; partialAcc: Boolean; var acc: Double; var test: Boolean): Integer; cdecl;
var
  accE: Extended;
begin
  try
    begin
      accE := acc;
      test := C.MFinder.FindDeformedBitmapToleranceIn(C.MBitmaps[bmp], x, y, xs, ys, xe, ye, tol, range, partialAcc, accE);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function rectangle_bitmap(C: TClient; bmp: Integer; box: TBox; col: TColor): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].Rectangle(box, col);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function flood_fill_bitmap(C: TClient; bmp: Integer; startPoint: TPoint; searchCol, replaceCol: TColor): Integer; cdecl;
begin
  try
    begin
      C.MBitmaps[bmp].FloodFill(startPoint, searchCol, replaceCol);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: matrix should be a T2DExtendedArray instead of array of array of Double
// (T2DDoubleArray)

type
  T2DDoubleArray = array of array of Double;
// XXX: THIS IS MOST DEFINITELY BROKEN
// FIXME
function convolute_bitmap(C: TClient; bmp: Integer; matrix: T2DDoubleArray; var test: Integer): Integer; cdecl;
var
  matrixE: T2DExtendedArray;
  i, j: Integer;
begin
  try
    begin
      for i := 0 to High(matrix) do
        for j := 0 to High(matrix) do
          matrixE[i][j] := matrix[i][j];
      test := C.MBitmaps.CreateBMP(0, 0);
      C.MBitmaps[bmp].Convolute(C.MBitmaps[test], matrixE);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function calculate_pixel_shift(C: TClient; bmp1, bmp2: Integer; compareBox: TBox; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := CalculatePixelShift(C.MBitmaps[bmp1], C.MBitmaps[bmp2], compareBox);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function calculate_pixel_shift_tpa(C: TClient; bmp1, bmp2: Integer; pts: TPointArray; var test: Integer): Integer; cdecl;
begin
  try
    begin
      test := CalculatePixelShiftTPA(C.MBitmaps[bmp1], C.MBitmaps[bmp2], pts);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

// XXX: test (output var) is Double - should be Extended
function calculate_pixel_tolerance(C: TClient; bmp1, bmp2: Integer; compareBox: TBox; cts: Integer; var test: Double): Integer; cdecl;
begin
  try
    begin
      test := CalculatePixelTolerance(C.MBitmaps[bmp1], C.MBitmaps[bmp2], compareBox, cts);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function calculate_pixel_tolerance_tpa(C: TClient; bmp1, bmp2: Integer; pts: TPointArray; cts: Integer; var test: Double): Integer; cdecl;
begin
  try
    begin
      test := CalculatePixelToleranceTPA(C.MBitmaps[bmp1], C.MBitmaps[bmp2], pts, cts);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;

function bitmap_exists(C: TClient; id: Integer; var test: Boolean): Integer; cdecl;
begin
  try
    begin
      test := C.MBitmaps.ExistsBMP(id);
      result := RESULT_OK;
    end;
  except on e : Exception do
    begin
      set_last_error(e.Message);
      result := RESULT_ERROR;
    end;
  end;
end;
