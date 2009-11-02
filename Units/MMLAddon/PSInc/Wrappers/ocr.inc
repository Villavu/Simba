function rs_GetUpText: String;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAt(7,7);
end;
