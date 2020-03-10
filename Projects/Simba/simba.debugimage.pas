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

    Image debug window for Mufasa Macro Library
}
unit simba.debugimage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Buttons,
  simba.mufasatypes, simba.imagebox;

type
  TSimbaDebugImageForm = class(TForm)
  protected
    FMouseX, FMouseY: Int32;

    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
    procedure ImageDoubleClick(Sender: TObject);
  public
    ImageBox: TSimbaImageBox;

    procedure SetDimensions(W, H: Int32);

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImageForm;

implementation

uses
  anchordocking, lcltype,
  simba.debugform, simba.main;

procedure TSimbaDebugImageForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
begin
  FMouseX := X;
  FMouseY := Y;
end;

procedure TSimbaDebugImageForm.ImageDoubleClick(Sender: TObject);
begin
  SimbaDebugForm.Add('Debug Image Click: ' + IntToStr(FMouseX) + ', ' + IntToStr(FMouseY));
end;

procedure TSimbaDebugImageForm.SetDimensions(W, H: Int32);
begin
  if (W < 200) then
    W := 200;
  if (H < 200) then
    H := 200;

  if (DockMaster.GetAnchorSite(Self) <> nil) then
  begin
    DockMaster.GetAnchorSite(Self).Width := W;
    DockMaster.GetAnchorSite(Self).Height := H + DockMaster.GetAnchorSite(Self).Header.Height + ImageBox.StatusBar.Height;
  end else
  begin
    Width := W;
    Height := H + ImageBox.StatusBar.Height;
  end;
end;

constructor TSimbaDebugImageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ImageBox := TSimbaImageBox.Create(Self);
  ImageBox.Parent := Self;
  ImageBox.Align := alClient;
  ImageBox.OnMouseMove := @ImageMouseMove;
  ImageBox.OnDblClick := @ImageDoubleClick;
end;

initialization
  {$R *.lfm}

end.

