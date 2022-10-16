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
  classes, sysutils, lresources, forms, controls, dialogs, math,
  simba.imagebox;

type
  TSimbaDebugImageForm = class(TForm)
  protected
    FMouseX, FMouseY: Int32;

    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
    procedure ImageDoubleClick(Sender: TObject);
  public
    ImageBox: TSimbaImageBox;

    procedure SetSize(AWidth, AHeight: Integer; AEnsureVisible: Boolean = True);

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImageForm;

implementation

uses
  simba.debugform, simba.dockinghelpers;

procedure TSimbaDebugImageForm.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Int32);
begin
  FMouseX := X;
  FMouseY := Y;
end;

procedure TSimbaDebugImageForm.ImageDoubleClick(Sender: TObject);
begin
  SimbaDebugForm.Add('Debug Image Click: ' + IntToStr(FMouseX) + ', ' + IntToStr(FMouseY));
end;

procedure TSimbaDebugImageForm.SetSize(AWidth, AHeight: Integer; AEnsureVisible: Boolean);
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  if (Form is TSimbaAnchorDockHostSite) and (TSimbaAnchorDockHostSite(Form).Header <> nil) then
  begin
    AHeight := AHeight + TSimbaAnchorDockHostSite(Form).Header.Height +
                         TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Top +
                         TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Bottom;
  end;

  if (AWidth > Form.Width) then
    Form.Width := AWidth;
  if (AHeight > Form.Height) then
    Form.Height := AHeight;

  if AEnsureVisible then
    Form.EnsureVisible(True);
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

