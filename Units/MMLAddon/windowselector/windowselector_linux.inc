{&MainUnit windowselector.pas}

uses
  x, simba.xlib, simba.xlib_helpers;

function TMWindowSelector.Drag: TOSWindow;
type
  TWindowTree = record
    Window: TOSWindow;
    Children: array of TWindowTree;
  end;

  procedure BuildTree(var Tree: TWindowTree; Window: TOSWindow);
  var
    Windows: TOSWindowArray;
    i: Int32;
  begin
    SetLength(Tree.Children, Length(Tree.Children) + 1);
    Tree.Children[High(Tree.Children)].Window := Window;

    Windows := Window.GetChildren(False);
    for i := High(Windows) downto 0 do
      BuildTree(Tree.Children[High(Tree.Children)], Windows[i]);
  end;

  function SearchTree(var Tree: TWindowTree; X, Y: Int32): TOSWindow;
  var
    i: Int32;
    Temp: TOSWindow;
  begin
    Result := 0;

    for i := 0 to High(Tree.Children) do
    begin
      with Tree.Children[i].Window.GetBounds() do
        if (X > X1) and (Y > Y1) and (X < X2) and (Y < Y2) then
          Result := Tree.Children[i].Window;

      Temp := SearchTree(Tree.Children[i], X, Y);
      if Temp <> 0 then
        Result := Temp;
    end;
  end;

var
  Windows: TOSWindowArray;
  Trees: array of TWindowTree;
  Root, Child: TWindow;
  RootX, RootY, X, Y, Mask: Int32;
  Bounds: TBox;
  Window: TOSWindow;
  i: Int32;
begin
  Result := 0;

  Windows := GetTopWindows();

  SetLength(Trees, Length(Windows));
  for i := 0 to High(Windows) do
    BuildTree(Trees[i], Windows[i]);

  repeat
    XQueryPointer(DefaultDisplay, GetDesktopWindow(), @Root, @Child, @RootX, @RootY, @X, @Y, @Mask);

    Window := 0;
    for i := 0 to High(Trees) do
    begin
      Window := SearchTree(Trees[i], X, Y);
      if (Window <> 0) then
        Break;
    end;
    if (Window = 0) then
      Window := GetDesktopWindow();

    if Result <> Window then
    begin
      Bounds := Window.GetBounds();

      with Window.GetRootWindow().GetBounds() do
      begin
        if Bounds.X1 < X1 then Bounds.X1 := X1;
        if Bounds.Y1 < Y1 then Bounds.Y1 := Y1;
        if Bounds.X2 > X2 then Bounds.X2 := X2;
        if Bounds.Y2 > Y2 then Bounds.Y2 := Y2;
      end;

      with GetDesktopWindow().GetBounds() do
      begin
        if Bounds.X1 < X1 then Bounds.X1 := X1;
        if Bounds.Y1 < Y1 then Bounds.Y1 := Y1;
        if Bounds.X2 > X2 then Bounds.X2 := X2;
        if Bounds.Y2 > Y2 then Bounds.Y2 := Y2;
      end;

      with Bounds do
        FForm.SetBounds(X1, Y1, X2-X1-1, Y2-Y1-1);
      FForm.ShowOnTop();

      Result := Window;
    end;

    Sleep(25);

    Application.ProcessMessages();

  until (Mask and Button1Mask) = 0;

  FLastPick := Result;
  FHasPicked := True;

  FForm.Hide();
end;
