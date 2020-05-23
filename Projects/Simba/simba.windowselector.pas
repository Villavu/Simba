{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    WindowSelector for the Mufasa Macro Library
}

unit simba.windowselector;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, controls, forms, graphics,
  simba.oswindow, simba.mufasatypes, simba.iomanager;

type
  TSimbaWindowSelector = class
  protected
    FLeftForm, FRightForm, FTopForm, FBottomForm: TForm;
    FIOManager: TIOManager;
    FBorderSize: Int32;
  public
    Selected: TOSWindow;

    constructor Create(BorderSize: Int32 = 4);
    destructor Destroy; override;
  end;

implementation

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
  Window: TOSWindow;
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

