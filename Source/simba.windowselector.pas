{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.windowselector;

{$i simba.inc}

interface

uses
  classes, sysutils, controls, forms, graphics,
  simba.helpers_windowhandle, simba.mufasatypes, simba.iomanager;

type
  TSimbaWindowSelector = class
  protected
    FLeftForm, FRightForm, FTopForm, FBottomForm: TForm;
    FIOManager: TIOManager;
    FBorderSize: Int32;
  public
    Selected: TWindowHandle;

    constructor Create(BorderSize: Int32 = 4);
    destructor Destroy; override;
  end;

implementation

uses
  math;

constructor TSimbaWindowSelector.Create(BorderSize: Int32);

  function CreateEdgeForm: TForm;
  begin
    Result := TForm.CreateNew(nil);
    Result.Scaled := False;
    Result.BorderStyle := bsNone;
    Result.Color := clGreen;
    Result.ShowInTaskBar := stNever;
  end;

var
  Window: TWindowHandle;
begin
  FBorderSize := BorderSize;

  FIOManager := TIOManager.Create();

  FLeftForm := CreateEdgeForm();
  FRightForm := CreateEdgeForm();
  FTopForm := CreateEdgeForm();
  FBottomForm := CreateEdgeForm();

  while FIOManager.IsMouseButtonDown(MOUSE_LEFT) do
  begin
    Window := GetWindowAtCursor();

    if (Window <> Selected) then
    begin
      with Window.GetBounds() do
      begin
        FLeftForm.SetBounds(X1 - FBorderSize, Y1 - FBorderSize, FBorderSize, Y2 - Y1 + (FBorderSize * 2));
        FLeftForm.ShowOnTop();

        FRightForm.SetBounds(X2, Y1 - FBorderSize, FBorderSize, Y2 - Y1 + (FBorderSize * 2));
        FRightForm.ShowOnTop();

        FTopForm.SetBounds(X1, Y1 - FBorderSize, X2 - X1, FBorderSize);
        FTopForm.ShowOnTop();

        FBottomForm.SetBounds(X1, Y2, X2 - X1, FBorderSize);
        FBottomForm.ShowOnTop();
      end;

      Selected := Window;
    end;

    Application.ProcessMessages();

    Sleep(25);
  end;
end;

destructor TSimbaWindowSelector.Destroy;
begin
  FIOManager.Free();

  FLeftForm.Free();
  FRightForm.Free();
  FTopForm.Free();
  FBottomForm.Free();

  inherited Destroy();
end;

end.

