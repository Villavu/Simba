{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Track system mouse position.
}
unit simba.ide_mouselogger;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.vartype_windowhandle, simba.threading;

type
  TSimbaMouseLogger = class(TThread)
  protected
    FWindowHandle: TWindowHandle;
    FWindowHandleChanged: Boolean;
    FIdle: TWaitableLock;

    procedure DoWindowSelected(Sender: TObject);
    procedure DoApplicationMinimized(Sender: TObject);
    procedure DoApplicationRestored(Sender: TObject);
    procedure DoChange(Data: PtrInt);
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    X, Y: Integer;
    HotkeyPressed: Boolean;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Forms,
  simba.ide_events, simba.ide_maintoolbar, simba.nativeinterface;

procedure TSimbaMouseLogger.DoWindowSelected(Sender: TObject);
begin
  FWindowHandle := SimbaMainToolBar.WindowSelection;
  FWindowHandleChanged := True;
end;

procedure TSimbaMouseLogger.DoApplicationMinimized(Sender: TObject);
begin
  FIdle.Lock();
end;

procedure TSimbaMouseLogger.DoApplicationRestored(Sender: TObject);
begin
  FIdle.Unlock();
end;

procedure TSimbaMouseLogger.DoChange(Data: PtrInt);
var
  Point: TSmallPoint absolute Data;
begin
  X := Point.X;
  Y := Point.Y;
  HotkeyPressed := SimbaNativeInterface.KeyPressed(EKeyCode.F1);

  SimbaIDEEvents.Notify(SimbaIDEEvent.MOUSELOGGER_CHANGE, Self);
end;

procedure TSimbaMouseLogger.Execute;
var
  Window: TWindowHandle;
  PrevPoint, Point: TSmallPoint;
  Data: PtrInt absolute Point;
begin
  PrevPoint.X := -1;
  PrevPoint.Y := -1;

  Window := GetDesktopWindow();

  while not Terminated do
  begin
    if FIdle.IsLocked() then // Wait until not minimized
    begin
      FIdle.WaitLocked();
      if Terminated then
        Break;
    end;

    if FWindowHandleChanged then
    begin
      FWindowHandleChanged := False;

      Window := FWindowHandle;
    end;
    if not Window.IsValid() then
      Window := GetDesktopWindow();

    if Window.IsVisible() then
      with Window.GetRelativeCursorPos() do
      begin
        Point.X := Int16(X); // cast to prevent overflows when debugging
        Point.Y := Int16(Y);
      end
    else
    begin
      Point.X := -1;
      Point.Y := -1;
    end;

    if (Point.X <> PrevPoint.X) or (Point.Y <> PrevPoint.Y) then
    begin
      PrevPoint := Point;

      Application.RemoveAsyncCalls(Self);
      Application.QueueAsyncCall(@DoChange, Data);
    end;

    Sleep(300);
  end;
end;

procedure TSimbaMouseLogger.TerminatedSet;
begin
  inherited TerminatedSet();

  FIdle.Unlock();
end;

constructor TSimbaMouseLogger.Create;
begin
  inherited Create(False, 512*512);

  Application.AddOnMinimizeHandler(@DoApplicationMinimized);
  Application.AddOnRestoreHandler(@DoApplicationRestored);

  SimbaIDEEvents.Register(SimbaIDEEvent.WINDOW_SELECTED, @DoWindowSelected);
end;

destructor TSimbaMouseLogger.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  Application.RemoveAllHandlersOfObject(Self);

  SimbaIDEEvents.UnRegister(SimbaIDEEvent.WINDOW_SELECTED, @DoWindowSelected);

  inherited Destroy();
end;

end.

