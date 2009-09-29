procedure GetClientDimensions(var w, h: integer);
begin
  CurrThread.Client.MWindow.GetDimensions(w, h);
end;

procedure Wait(t: Integer);
begin
  Sleep(t);
end;

function Freeze: boolean;
begin
  result := CurrThread.Client.MWindow.Freeze();
end;

function Unfreeze: boolean;
begin
  result := CurrThread.Client.MWindow.Unfreeze;
end;
