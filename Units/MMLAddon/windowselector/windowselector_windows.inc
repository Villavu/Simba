{&MainUnit windowselector.pas}

uses
  windows;

function TMWindowSelector.Drag: TOSWindow;
var
  Window: TOSWindow;
  Bounds: TBox;
  Point: TPoint;
begin
  Result := 0;

  SetWindowLong(FForm.Handle, GWL_EXSTYLE, GetWindowLong(FForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TRANSPARENT);

  while GetAsyncKeyState(VK_LBUTTON) <> 0 do
  begin
    GetCursorPos(Point);

    Window := WindowFromPoint(Point);

    if Window <> Result then
    begin
      Bounds := Window.GetBounds();
      with Bounds do
        FForm.SetBounds(X1,Y1, X2-X1, Y2-Y1);
      FForm.ShowOnTop();

      Result := Window;
    end;

    Application.ProcessMessages();

    Sleep(25);
  end;

  FLastPick := Result;
  FHasPicked := True;

  FForm.Hide();
end;
