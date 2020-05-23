unit simba.oswindow;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  classes, sysutils, regexpr, lcltype,
  simba.mufasatypes;

type
  POSWindow = ^TOSWindow;
  TOSWindow = type HWND;

  POSWindowArray = ^TOSWindowArray;
  TOSWindowArray = array of TOSWindow;

  TOSWindowArray_Helper = type Helper for TOSWindowArray
  public
    function GetByTitle(Title: String; out Window: TOSWindow): Boolean; overload;
    function GetByTitle(Title: String): TOSWindowArray; overload;

    function GetByClass(ClassName: String; out Window: TOSWindow): Boolean; overload;
    function GetByClass(ClassName: String): TOSWindowArray; overload;

    function GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean; overload;
    function GetByTitleAndClass(Title, ClassName: String): TOSWindowArray; overload;

    function ToString: String;
  end;

  TOSWindow_Helper = type Helper for TOSWindow
  public
    function IsValid: Boolean;
    function IsActive: Boolean; overload;
    function IsActive(Time: Int32): Boolean; overload;
    function IsVisible: Boolean;
    function GetPID: UInt32;
    function GetRootWindow: TOSWindow;
    function GetClassName: WideString;
    function GetTitle: WideString;
    function GetBounds(out Bounds: TBox): Boolean; overload;
    function GetBounds: TBox; overload;
    function GetChildren(Recursive: Boolean = True): TOSWindowArray;
    procedure SetBounds(Bounds: TBox);
    function Activate: Boolean;
    procedure Kill;
  end;

  function GetVisibleWindows: TOSWindowArray;
  function GetWindows: TOSWindowArray;
  function GetActiveWindow: TOSWindow;
  function GetDesktopWindow: TOSWindow;
  function GetWindowAtCursor: TOSWindow;
  function GetTopWindows: TOSWindowArray;

  function FindWindow(Title: String): TOSWindowArray;
  function FindWindow(Title: String; out Window: TOSWindow): Boolean;
  function FindChildWindow(Title: String; ClassName: String): TOSWindowArray;
  function FindChildWindow(Title: String; ClassName: String; out Child: TOSWindow): Boolean;

  operator +(Left: TOSWindowArray; Right: TOSWindow): TOSWindowArray;
  operator +(Left: TOSWindowArray; Right: TOSWindowArray): TOSWindowArray;

implementation

{$IFDEF LINUX}
  {$INCLUDE oswindow_linux.inc}
{$ENDIF}

{$IFDEF WINDOWS}
  {$INCLUDE oswindow_windows.inc}
{$ENDIF}

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
      Result += Window;
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
      Result += Window;
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
      Result += Window;
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

operator +(Left: TOSWindowArray; Right: TOSWindow): TOSWindowArray;
var
  Count: Int32;
begin
  Result := Left;

  Count := Length(Result);
  SetLength(Result, Count + 1);
  Result[Count] := Right;
end;

operator +(Left: TOSWindowArray; Right: TOSWindowArray): TOSWindowArray;
begin
  SetLength(Result, Length(Left) + Length(Right));

  if Length(Result) > 0 then
  begin
    if Length(Left) > 0 then
      Move(Left[0], Result[0], Length(Left) * SizeOf(TOSWindow));
    if Length(Right) > 0 then
      Move(Right[0], Result[Length(Left)], Length(Right) * SizeOf(TOSWindow));
  end;
end;

initialization
  {$IFDEF WINDOWS}
  InitDwmLibrary();
  {$ENDIF}

end.


