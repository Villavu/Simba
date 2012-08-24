(*
DTM Functions
=============


*)

{ Create a MDTM}
function create_dtm(PointLen: integer; Points: PMDTMPoint; DTM: TMDTM): integer;
    cdecl;
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

  try
    index := C.MDTMs.AddDTM(DTM);
    exit(RESULT_OK);
  except on e : Exception do
    result := RESULT_ERROR;
  end;
end;

{ Remove a previously added DTM from the DTM manager. This also frees the DTM }
function remove_dtm(C: TClient; DTMi: integer): integer; cdecl;
begin
  C.MDTMs.FreeDTM(DTMi);
end;

{ Find a DTM given DTM index i, client C in area x1,y1,x2,y2. Return coord at x, y. }
function find_dtm(C: TClient; DTMi: integer; var x, y: integer; x1, y1, x2,
    y2: integer): integer; cdecl;
var
  res: boolean;
begin
  try
    res := C.MFinder.FindDTM(C.MDTMs.DTM[DTMi], x, y, x1, y1, x2, y2);
  except on e : Exception do
    begin;
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
function find_dtms(C: TClient; DTMi: integer; ptr: PPoint; x1, y1, x2, 
    y2: integer): integer; cdecl;
var
  res: boolean;
  len: integer;
  TPA: TPointArray;
begin
  try
    res := C.MFinder.FindDTMs(C.MDTMs.DTM[DTMi], TPA, x1, y1, x2, y2);
  except on e : Exception do
  begin;
    result := RESULT_ERROR;
    set_last_error(e.Message);
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
