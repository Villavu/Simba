function ps_FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
begin
 Result := CurrThread.Client.MDTM.FindDTM(DTM, x, y, x1, y1, x2, y2);
end;

function ps_FindDTMs(DTM: Integer; var p: TPointArray; x1, y1, x2, y2: Integer): Boolean;
begin
 Result := CurrThread.Client.MDTM.FindDTMs(DTM, p, x1, y1, x2, y2);
end;

function ps_FindDTMRotated(DTM: Integer; var x, y: Integer; x1, y1, x2, y2:
                           Integer; sAngle, eAngle, aStep: Extended;
                           var aFound: Extended): Boolean;
begin
 Result := CurrThread.Client.MDTM.FindDTMRotated(DTM, x, y, x1, y1, x2, y2,
                                                 sAngle, eAngle, aStep, aFound);
end;

function ps_FindDTMsRotated(DTM: Integer; var x, y: Integer; x1, y1, x2, y2:
                           Integer; sAngle, eAngle, aStep: Extended;
                           var aFound: Extended): Boolean;
begin
 Result := CurrThread.Client.MDTM.FindDTMRotated(DTM, x, y, x1, y1, x2, y2,
                                  sAngle, eAngle, aStep, aFound);
end;

function ps_FindDTMsRotated(DTM: Integer; var Points: TPointArray; x1,
                                  y1, x2, y2: Integer; sAngle, eAngle,
                                  aStep: Extended; var aFound: T2DExtendedArray)
                                  : Boolean;
begin
  Result := CurrThread.Client.MDTM.FindDTMsRotated(DTM, Points, x1, y1, x2, y2,
                                    sAngle, eAngle, aStep, aFound);
end;

function ps_DTMFromString(DTMString: String): Integer;
var
   dtm: pDTM;
begin
  With CurrThread.Client.MDTM do
  begin
    dtm := StringToDTM(DTMString);
    Result := AddpDTM(dtm);
  end;
end;

procedure ps_FreeDTM(DTM: Integer);
begin
  CurrThread.Client.MDTM.FreeDTM(DTM);
end;

function ps_GetDTM(index: Integer; var dtm: pDTM): Boolean;
begin
 Result := CurrThread.Client.MDTM.GetDTM(index, dtm);
end;

function ps_AddDTM(d: TDTM): Integer;
begin
  Result := CurrThread.Client.MDTM.AddDTM(d);
end;

function ps_AddpDTM(d: pDTM): Integer;
begin
 Result := CurrThread.Client.MDTM.AddpDTM(d);
end;
