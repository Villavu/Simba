function rs_GetUpText: String;
begin
  // why the hell does it still not use shadows?
  Result := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;
