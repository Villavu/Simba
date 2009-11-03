function rs_GetUpText: String;
begin
  Result := CurrThread.Client.MOCR.GetUpTextAt(7,7);
  {writeln('inside: ' + result);}
end;
