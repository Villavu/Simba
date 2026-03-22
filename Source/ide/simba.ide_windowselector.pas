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
  Classes, SysUtils, Controls, Forms, Graphics,
  simba.base,
  simba.ide_events;

type
  TSimbaWindowSelector = class(TComponent)
  protected
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure Pick;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

var
  SimbaWindowSelector: TSimbaWindowSelector;

implementation

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF DARWIN}
  CocoaAll, CocoaWSForms, CocoaUtils,
  {$ENDIF}
  simba.ide_vars,
  simba.vartype_windowhandle,
  simba.vartype_box,
  simba.process,
  simba.dialog,
  simba.nativeinterface,
  simba.initializations;

type
  TWindowHighlighter = class
  protected const
    BORDER_SIZE = 3;
  protected
    FForm: TForm;
    FLeftForm, FRightForm, FTopForm, FBottomForm: TForm;
    FExcludeWindows: TWindowHandleArray;
  public
    constructor Create;
    destructor Destroy; override;

    function GetWindowAtCursor: TWindowHandle;
    procedure Highlight(Window: TWindowHandle);
  end;

constructor TWindowHighlighter.Create;

  function FormToWindowHandle(Form: TForm): TWindowHandle;
  begin
    {$IFDEF DARWIN}
    Result := TCocoaWSCustomForm.GetWindowContentFromHandle(Form).window.windowNumber;
    {$ELSE}
    Result := Form.Handle;
    {$ENDIF}
  end;

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

    FExcludeWindows := FExcludeWindows + [FormToWindowHandle(Result)];
  end;

  function CreateTransparentForm: TForm;
  begin
    Result := TForm.CreateNew(nil);
    Result.FormStyle := fsSystemStayOnTop;
    Result.BorderStyle := bsNone;
    Result.Scaled := False;
    Result.SetBounds(0, 0, 0, 0);
    Result.Color := clGreen;
    Result.AlphaBlend := True;
    Result.AlphaBlendValue := 100;
    Result.Show();

    {$IFDEF DARWIN}
    with TCocoaWSCustomForm.GetWindowContentFromHandle(Result) do
      window.setBackgroundColor(ColorToNSColor(Result.Color));
    {$ENDIF}

    {$IFDEF WINDOWS}
    SetWindowLong(Result.Handle, GWL_EXSTYLE, GetWindowLong(Result.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TRANSPARENT);
    {$ENDIF}

    FExcludeWindows := FExcludeWindows + [FormToWindowHandle(Result)];
  end;

begin
  inherited Create();

  {$IF DEFINED(WINDOWS) or DEFINED(DARWIN)}
  FForm := CreateTransparentForm();
  {$ELSE}
  FLeftForm   := CreateEdgeForm();
  FRightForm  := CreateEdgeForm();
  FTopForm    := CreateEdgeForm();
  FBottomForm := CreateEdgeForm();
  {$ENDIF}
end;

destructor TWindowHighlighter.Destroy;
begin
  FreeAndNil(FForm);
  FreeAndNil(FLeftForm);
  FreeAndNil(FRightForm);
  FreeAndNil(FTopForm);
  FreeAndNil(FBottomForm);

  inherited Destroy();
end;

function TWindowHighlighter.GetWindowAtCursor: TWindowHandle;
begin
  Result := SimbaNativeInterface.GetWindowAtCursor(FExcludeWindows);
end;

procedure TWindowHighlighter.Highlight(Window: TWindowHandle);
begin
  with Window.GetBounds() do
  begin
    if (FForm <> nil)       then FForm.SetBounds(X1, Y1, X2 - X1, Y2 - Y1);
    if (FLeftForm <> nil)   then FLeftForm.SetBounds(X1 - BORDER_SIZE, Y1 - BORDER_SIZE, BORDER_SIZE, Y2 - Y1 + (BORDER_SIZE * 2));
    if (FRightForm <> nil)  then FRightForm.SetBounds(X2, Y1 - BORDER_SIZE, BORDER_SIZE, Y2 - Y1 + (BORDER_SIZE * 2));
    if (FTopForm <> nil)    then FTopForm.SetBounds(X1, Y1 - BORDER_SIZE, X2 - X1, BORDER_SIZE);
    if (FBottomForm <> nil) then FBottomForm.SetBounds(X1, Y2, X2 - X1, BORDER_SIZE);
  end;
end;

constructor TSimbaWindowSelector.Create;
begin
  inherited Create(nil);

  SimbaEvents.Register(Self, @DoSimbaEvent);
end;

destructor TSimbaWindowSelector.Destroy;
begin
  inherited Destroy();
end;

procedure TSimbaWindowSelector.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.TOOLBAR_PICKTARGET:
      Pick();
  end;
end;

procedure TSimbaWindowSelector.Pick;
var
  Selected, WinAtCursor: TWindowHandle;
  Highlighter: TWindowHighlighter;
  Pid: TProcessID;
  Bounds: TBox;
begin
  Selected := 0;
  Highlighter := nil;
  try
    Highlighter := TWindowHighlighter.Create();

    while SimbaNativeInterface.MousePressed(EMouseButton.LEFT) do
    begin
      WinAtCursor := Highlighter.GetWindowAtCursor();
      if (WinAtCursor <> 0) and (WinAtCursor <> Selected) then
      begin
        Highlighter.Highlight(WinAtCursor);
        Selected := WinAtCursor;
      end;

      Application.ProcessMessages();
      Sleep(25);
    end;

    if (Selected <> 0) and Selected.IsValid() then
    begin
      Pid := Selected.GetPID();
      Bounds := Selected.GetBounds();

      DebugLn([EDebugLn.FOCUS], 'Window Selected: %d',  [Selected]);
      DebugLn([EDebugLn.FOCUS], ' - Dimensions: %dx%d', [Bounds.Width - 1, Bounds.Height - 1]);
      DebugLn([EDebugLn.FOCUS], ' - PID: %d (%s)',      [PID, IfThen(IsProcess64Bit(PID), '64 bit', '32 bit')]);
      DebugLn([EDebugLn.FOCUS], ' - Title: "%s"',       [Selected.GetTitle()]);
      DebugLn([EDebugLn.FOCUS], ' - ClassName: "%s"',   [Selected.GetClassName()]);
      DebugLn([EDebugLn.FOCUS], ' - Executable: "%s"',  [GetProcessPath(PID)]);

      SimbaIDEVars.WindowSelection := Selected;
      SimbaIDEVars.ProcessSelection := Pid;
    end;
  except
    on E: Exception do
      ShowErrorDialog('Target Selector', 'Exception occurred while selecting target %s', [E.Message]);
  end;

  if (Highlighter <> nil) then
    Highlighter.Free();
end;

procedure DoCreate;
begin
  SimbaWindowSelector := TSimbaWindowSelector.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaWindowSelector);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaWindowSelector');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaWindowSelector');

end.


