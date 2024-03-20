{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_windowselector;

{$i simba.inc}
{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

interface

uses
  classes, sysutils, controls, forms, graphics,
  simba.base;

function ShowWindowSelector: TWindowHandle;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  CocoaAll, CocoaWSForms, CocoaUtils,
  {$ENDIF}
  simba.baseclass, simba.vartype_windowhandle, simba.nativeinterface;

type
  TSimbaWindowSelectorBase = class(TSimbaBaseClass)
  protected
    FExcludeWindows: TWindowHandleArray;

    procedure HighlightWindow(Window: TWindowHandle); virtual; abstract;
  public
    function Select: TWindowHandle;
  end;

  // 4 forms to create a box around the window
  TSimbaWindowSelectorSimple = class(TSimbaWindowSelectorBase)
  protected
  const
    BORDER_SIZE = 4;
  protected
    FLeftForm, FRightForm, FTopForm, FBottomForm: TForm;

    procedure HighlightWindow(Window: TWindowHandle); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // overlay a transparent form
  TSimbaWindowSelectorFancy = class(TSimbaWindowSelectorBase)
  protected
    FForm: TForm;

    procedure HighlightWindow(Window: TWindowHandle); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function ShowWindowSelector: TWindowHandle;
begin
  // Linux (xlib) does support transparent windows but display managers may or may not perform.
  {$IF DEFINED(WINDOWS) or DEFINED(DARWIN)}
  with TSimbaWindowSelectorFancy.Create() do
  {$ELSE}
  with TSimbaWindowSelectorSimple.Create() do
  {$ENDIF}
  try
    Result := Select();
  finally
    Free();
  end;
end;

function TSimbaWindowSelectorBase.Select: TWindowHandle;
var
  Window: TWindowHandle;
begin
  Result := 0;

  while SimbaNativeInterface.MousePressed(EMouseButton.LEFT) do
  begin
    Window := GetWindowAtCursor(FExcludeWindows);
    if (Window <> 0) and (Window <> Result) then
    begin
      HighlightWindow(Window);

      Result := Window;
    end;

    Application.ProcessMessages();

    Sleep(25);
  end;
end;

constructor TSimbaWindowSelectorSimple.Create;

  function CreateEdgeForm: TForm;
  begin
    Result := TForm.CreateNew(nil);
    Result.FormStyle := fsSystemStayOnTop;
    Result.Scaled := False;
    Result.BorderStyle := bsNone;
    Result.Color := clGreen;
    Result.ShowInTaskBar := stNever;
    Result.SetBounds(0, 0, 0, 0);
    Result.Show();

    FExcludeWindows := FExcludeWindows + [Result.Handle];
  end;

begin
  inherited Create();

  FLeftForm   := CreateEdgeForm();
  FRightForm  := CreateEdgeForm();
  FTopForm    := CreateEdgeForm();
  FBottomForm := CreateEdgeForm();
end;

destructor TSimbaWindowSelectorSimple.Destroy;
begin
  FreeAndNil(FLeftForm);
  FreeAndNil(FRightForm);
  FreeAndNil(FTopForm);
  FreeAndNil(FBottomForm);

  inherited Destroy();
end;

procedure TSimbaWindowSelectorSimple.HighlightWindow(Window: TWindowHandle);
begin
  with Window.GetBounds() do
  begin
    FLeftForm.SetBounds(X1 - BORDER_SIZE, Y1 - BORDER_SIZE, BORDER_SIZE, Y2 - Y1 + (BORDER_SIZE * 2));
    FRightForm.SetBounds(X2, Y1 - BORDER_SIZE, BORDER_SIZE, Y2 - Y1 + (BORDER_SIZE * 2));
    FTopForm.SetBounds(X1, Y1 - BORDER_SIZE, X2 - X1, BORDER_SIZE);
    FBottomForm.SetBounds(X1, Y2, X2 - X1, BORDER_SIZE);
  end;
end;

procedure TSimbaWindowSelectorFancy.HighlightWindow(Window: TWindowHandle);
begin
  with Window.GetBounds() do
    FForm.SetBounds(X1, Y1, X2-X1, Y2-Y1);
end;

constructor TSimbaWindowSelectorFancy.Create;
begin
  FForm := TForm.CreateNew(nil);
  FForm.FormStyle := fsSystemStayOnTop;
  FForm.BorderStyle := bsNone;
  FForm.Scaled := False;
  FForm.SetBounds(0, 0, 0, 0);
  FForm.Color := clGreen;
  FForm.AlphaBlend := True;
  FForm.AlphaBlendValue := 100;
  FForm.Show();

  {$IFDEF DARWIN}
  with TCocoaWSCustomForm.GetWindowContentFromHandle(FForm) do
  begin
    window.setBackgroundColor(ColorToNSColor(FForm.Color));

    FExcludeWindows := [window.windowNumber];
  end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  SetWindowLong(FForm.Handle, GWL_EXSTYLE, GetWindowLong(FForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TRANSPARENT);

  FExcludeWindows := [FForm.Handle];
  {$ENDIF}
end;

destructor TSimbaWindowSelectorFancy.Destroy;
begin
  FreeAndNil(FForm);

  inherited Destroy();
end;

end.

