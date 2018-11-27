unit oswindow;

{$mode objfpc}{$H+}

{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
  classes, sysutils, regexpr, lcltype,
  mufasatypes;

type
  TOSWindow = type HWND;
  TOSWindow_Helper = type Helper for TOSWindow
  public
  type
    TOSWindowArray = array of TOSWindow;
  public
    function IsVaild: Boolean;
    function IsActive: Boolean; overload;
    function IsActive(Time: Int32): Boolean; overload;
    function IsVisible: Boolean;
    function GetPID: UInt32;
    function GetRootWindow: TOSWindow;
    function GetClassName: String;
    function GetTitle: String;
    function GetBounds: TBox;
    function GetChildren: TOSWindowArray;
    procedure SetBounds(Box: TBox);
    function Activate: Boolean;
    procedure Kill;
  end;

  TOSWindowArray = array of TOSWindow;
  TOSWindowArray_Helper = type Helper for TOSWindowArray
  public
    function GetByTitle(Title: String; out Window: TOSWindow): Boolean; overload;
    function GetByTitle(Title: String): TOSWindowArray; overload;

    function GetByClass(ClassName: String; out Window: TOSWindow): Boolean; overload;
    function GetByClass(ClassName: String): TOSWindowArray; overload;

    function GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean; overload;
    function GetByTitleAndClass(Title, ClassName: String): TOSWindowArray; overload;
  end;

  POSWindow = ^TOSWindow;
  POSWindowArray = ^TOSWindowArray;

  function GetVisibleWindows: TOSWindowArray;
  function GetWindows: TOSWindowArray;
  function GetActiveWindow: TOSWindow;
  function GetDesktopWindow: TOSWindow;

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
    begin
      SetLength(Result, Length(Result) + 1);

      Result[High(Result)] := Window;
    end;
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
    begin
      SetLength(Result, Length(Result) + 1);

      Result[High(Result)] := Window;
    end;
end;

function TOSWindowArray_Helper.GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean;
begin
  for Window in Self do
    if ExecRegExpr(Title, Window.GetTitle()) and ExecRegExpr(ClassName, Window.GetClassName()) then
      Exit(True);
end;

function TOSWindowArray_Helper.GetByTitleAndClass(Title, ClassName: String): TOSWindowArray;
var
  Window: TOSWindow;
begin
  SetLength(Result, 0);

  for Window in Self do
    if ExecRegExpr(Title, Window.GetTitle()) and ExecRegExpr(ClassName, Window.GetClassName()) then
    begin
      SetLength(Result, Length(Result) + 1);

      Result[High(Result)] := Window;
    end;
end;

end.


