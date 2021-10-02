function TOSWindowArray_Helper.GetByTitle(Title: String; out Window: TOSWindow): Boolean;
begin
  for Window in Self do
    if ExecRegExpr(Title, Window.GetTitle()) then
      Exit(True);

  Exit(False);
end;

function TOSWindowArray_Helper.GetByTitle(Title: String): TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in Self do
    if ExecRegExpr(Title, Window.GetTitle()) then
      Result += [Window];
end;

function TOSWindowArray_Helper.GetByClass(ClassName: String; out Window: TOSWindow): Boolean;
begin
  for Window in Self do
    if ExecRegExpr(ClassName, Window.GetClassName()) then
      Exit(True);

  Exit(False);
end;

function TOSWindowArray_Helper.GetByClass(ClassName: String): TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in Self do
    if ExecRegExpr(ClassName, Window.GetClassName()) then
      Result += [Window];
end;

function TOSWindowArray_Helper.GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean;
begin
  for Window in Self do
    if ExecRegExpr(Title, Window.GetTitle()) and ExecRegExpr(ClassName, Window.GetClassName()) then
      Exit(True);

  Exit(False);
end;

function TOSWindowArray_Helper.GetByTitleAndClass(Title, ClassName: String): TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in Self do
    if ExecRegExpr(Title, Window.GetTitle()) and ExecRegExpr(ClassName, Window.GetClassName()) then
      Result += [Window];
end;

function TOSWindowArray_Helper.ToString: String;

  procedure Append(Text: String; Args: array of const);
  begin
    Result := Result + Format(Text, Args) + LineEnding;
  end;

var
  Window: TOSWindow;
begin
  for Window in Self do
    with Window.GetBounds() do
    begin
      Append('Window: %d', [Window]);
      Append(' - Title: %s', [Window.GetTitle()]);
      Append(' - Class: %s', [Window.GetClassName()]);
      Append(' - PID: %d', [Window.GetPID()]);
      Append(' - Root: %d', [Window.GetRootWindow()]);
      Append(' - Visible: %s', [BoolToStr(Window.IsVisible(), True)]);
      Append(' - Bounds: [%d, %d, %d, %d]', [X1, Y1, X2, Y2]);
      Append(' - Children: %d', [Length(Window.GetChildren())]);;
      Append('', []);
    end;
end;

function FindWindow(Title: String): TOSWindowArray;
begin
  Result := GetTopWindows().GetByTitle(Title);
end;

function FindWindow(Title: String; out Window: TOSWindow): Boolean;
begin
  Result := GetTopWindows().GetByTitle(Title, Window);
end;

function FindChildWindow(Title: String; ClassName: String): TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in FindWindow(Title) do
    Result += Window.GetChildren().GetByClass(ClassName);
end;

function FindChildWindow(Title: String; ClassName: String; out Child: TOSWindow): Boolean;
var
  Window: TOSWindow;
begin
  for Window in FindWindow(Title) do
    if Window.GetChildren().GetByClass(ClassName, Child) then
      Exit(True);

  Exit(False);
end;

