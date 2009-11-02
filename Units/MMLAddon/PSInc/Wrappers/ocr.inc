function rs_GetUpText: String;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAt(9,9);
end;
