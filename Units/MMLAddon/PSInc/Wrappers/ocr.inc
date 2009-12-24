function rs_GetUpText: String;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAtEx(7, 7, true);
end;
