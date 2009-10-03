function FindDTM(DTM: Integer; var x, y: Integer; x1, y1, x2, y2: Integer): Boolean;
begin
 Result := CurrThread.Client.MDTM.FindDTM(DTM, x, y, x1, y1, x2, y2);
end;

function ps_StringFromDTM(DTMString: String): Integer;
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
