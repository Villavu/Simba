procedure MoveMouse(x, y: integer);
begin
  CurrThread.Client.MInput.SetMousePos(X, Y);
end;

procedure GetMousePos(var x, y: integer);
begin
   CurrThread.Client.MInput.GetMousePos(X, Y);
end; 
