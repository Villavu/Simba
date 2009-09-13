procedure GetClientDimensions(var w, h: integer);
begin
  CurrThread.Client.MWindow.GetDimensions(w, h);
end;

procedure Wait(t: Integer);
begin
  Sleep(t);
end;

