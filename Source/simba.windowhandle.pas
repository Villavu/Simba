{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.windowhandle;

{$i simba.inc}

interface

uses
  classes, sysutils,
  simba.mufasatypes;

type
  TWindowHandleHelper = type Helper for TWindowHandle
  public
    function IsValid: Boolean;
    function IsActive: Boolean;
    function IsVisible: Boolean;
    function GetPID: Integer;
    function GetRootWindow: TWindowHandle;
    function GetClassName: String;
    function GetTitle: String;
    function GetClassNameW: WideString;
    function GetTitleW: WideString;
    function GetBounds: TBox;
    function GetChildren(Recursive: Boolean = True): TWindowHandleArray;
    function GetRelativeCursorPos: TPoint;
    procedure SetBounds(Bounds: TBox);
    function Activate: Boolean;
    procedure Kill;
  end;

  function GetVisibleWindows: TWindowHandleArray;
  function GetWindows: TWindowHandleArray;
  function GetTopWindows: TWindowHandleArray;
  function GetActiveWindow: TWindowHandle;
  function GetDesktopWindow: TWindowHandle;
  function GetWindowAtCursor: TWindowHandle;

  function FindWindow(Title: String; out Window: TWindowHandle): Boolean;
  function FindWindows(Title: String): TWindowHandleArray;
  function FindChildWindow(Title: String; ClassName: String; out Child: TWindowHandle): Boolean;
  function FindChildWindows(Title: String; ClassName: String): TWindowHandleArray;

implementation

uses
  simba.nativeinterface;

function TWindowHandleHelper.IsValid: Boolean;
begin
  Result := SimbaNativeInterface.IsWindowValid(Self);
end;

function TWindowHandleHelper.IsActive: Boolean;
begin
  Result := SimbaNativeInterface.IsWindowActive(Self);
end;

function TWindowHandleHelper.IsVisible: Boolean;
begin
  Result := SimbaNativeInterface.IsWindowVisible(Self);
end;

function TWindowHandleHelper.GetPID: Integer;
begin
  Result := SimbaNativeInterface.GetWindowPID(Self);
end;

function TWindowHandleHelper.GetRootWindow: TWindowHandle;
begin
  Result := SimbaNativeInterface.GetRootWindow(Self);
end;

function TWindowHandleHelper.GetClassName: String;
begin
  Result := AnsiString(SimbaNativeInterface.GetWindowClass(Self));
end;

function TWindowHandleHelper.GetTitle: String;
begin
  Result := AnsiString(SimbaNativeInterface.GetWindowTitle(Self));
end;

function TWindowHandleHelper.GetClassNameW: WideString;
begin
  Result := SimbaNativeInterface.GetWindowClass(Self);
end;

function TWindowHandleHelper.GetTitleW: WideString;
begin
  Result := SimbaNativeInterface.GetWindowTitle(Self);
end;

function TWindowHandleHelper.GetBounds: TBox;
begin
  Result := SimbaNativeInterface.GetWindowBounds(Self);
end;

function TWindowHandleHelper.GetChildren(Recursive: Boolean): TWindowHandleArray;
begin
  Result := SimbaNativeInterface.GetWindowChildren(Self, Recursive);
end;

procedure TWindowHandleHelper.SetBounds(Bounds: TBox);
begin
  SimbaNativeInterface.SetWindowBounds(Self, Bounds);
end;

function TWindowHandleHelper.Activate: Boolean;
begin
  Result := SimbaNativeInterface.ActivateWindow(Self);
end;

procedure TWindowHandleHelper.Kill;
begin
  SimbaNativeInterface.TerminateProcess(Self.GetPID());
end;

function TWindowHandleHelper.GetRelativeCursorPos: TPoint;
begin
  Result := SimbaNativeInterface.GetMousePosition(Self);
end;

function GetVisibleWindows: TWindowHandleArray;
begin
  Result := SimbaNativeInterface.GetVisibleWindows();
end;

function GetWindows: TWindowHandleArray;
begin
  Result := SimbaNativeInterface.GetWindows();
end;

function GetActiveWindow: TWindowHandle;
begin
  Result := SimbaNativeInterface.GetActiveWindow();
end;

function GetDesktopWindow: TWindowHandle;
begin
  Result := SimbaNativeInterface.GetDesktopWindow();
end;

function GetWindowAtCursor: TWindowHandle;
begin
  Result := SimbaNativeInterface.GetWindowAtCursor();
end;

function GetTopWindows: TWindowHandleArray;
begin
  Result := SimbaNativeInterface.GetTopWindows();
end;

function FindWindow(Title: String; out Window: TWindowHandle): Boolean;
var
  Windows: TWindowHandleArray;
  I: Integer;
begin
  Windows := GetWindows();
  for I := 0 to High(Windows) do
    if Windows[I].GetTitle().RegExprExists(Title) then
    begin
      Window := Windows[I];

      Result := True;
      Exit;
    end;

  Result := False;
end;

function FindWindows(Title: String): TWindowHandleArray;
var
  Windows: TWindowHandleArray;
  I: Integer;
begin
  Result := Default(TWindowHandleArray);

  Windows := GetWindows();
  for I := 0 to High(Windows) do
    if Windows[I].GetTitle().RegExprExists(Title) then
      Result := Result + [Windows[I]];
end;

function FindChildWindow(Title: String; ClassName: String; out Child: TWindowHandle): Boolean;
var
  Windows, ChildWindows: TWindowHandleArray;
  I, J: Integer;
begin
  Windows := FindWindows(Title);

  for I := 0 to High(Windows) do
  begin
    ChildWindows := Windows[I].GetChildren();
    for J := 0 to High(ChildWindows) do
      if ChildWindows[J].GetClassName().RegExprExists(ClassName) then
      begin
        Child := ChildWindows[J];

        Result := True;
        Exit;
      end;
  end;

  Result := False;
end;

function FindChildWindows(Title: String; ClassName: String): TWindowHandleArray;
var
  Windows, ChildWindows: TWindowHandleArray;
  I, J: Integer;
begin
  Result := Default(TWindowHandleArray);

  Windows := FindWindows(Title);
  for I := 0 to High(Windows) do
  begin
    ChildWindows := Windows[I].GetChildren();
    for J := 0 to High(ChildWindows) do
      if ChildWindows[J].GetClassName().RegExprExists(ClassName) then
        Result := Result + [ChildWindows[J]];
  end;
end;

end.

