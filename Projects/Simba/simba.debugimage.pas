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
  published
    procedure ImageDoubleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Int32);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetParent(AParent: TWinControl); override;
  protected
    FControlDown: Boolean;
    FPoints: TPointArray;
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
  simba.debugform, simba.main, simba.iomanager, simba.bitmap;

procedure TSimbaDebugImageForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Manager: TIOManager;
  Bitmap: TMufasaBitmap;
  W, H: Int32;
begin
  if (Key = VK_CONTROL) then
    FControlDown := True;

  if (Key = VK_F5) then
  begin
    Manager := nil;
    Bitmap := nil;

    try
      Manager := TIOManager.Create();
      Manager.SetTarget(SimbaForm.WindowSelection);
      Manager.GetDimensions(W, H);

      Bitmap := TMufasaBitmap.Create();
      Bitmap.CopyClientToBitmap(Manager, True, 0, 0, W-1, H-1);

      ImageBox.Draw(Bitmap.FData, Bitmap.Width, Bitmap.Height);
    except
    end;

    if (Bitmap <> nil) then
      Bitmap.Free();
    if (Manager <> nil) then
      Manager.Free();
  end;
end;

procedure TSimbaDebugImageForm.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and (AParent is TAnchorDockHostSite) then
    AParent.OnKeyDown := @FormKeyDown;

  inherited SetParent(AParent);
end;

procedure TSimbaDebugImageForm.ImageDoubleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Int32);
begin
  if FControlDown then
  begin
    SetLength(FPoints, Length(FPoints) + 1);

    FPoints[High(FPoints)].X := X;
    FPoints[High(FPoints)].Y := Y;
  end else
    SimbaDebugForm.Add('Debug Image Click: ' + IntToStr(X) + ', ' + IntToStr(Y));
end;

procedure TSimbaDebugImageForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  I: Int32;
  S: String;
begin
  if (Key = VK_CONTROL) then
  begin
    if (Length(FPoints) > 0) then
    begin
      S := '[';
      for I := 0 to High(FPoints) do
      begin
        if I > 0 then
          S := S + ', ';
        S := S + '[' + IntToStr(FPoints[I].X) + ', ' + IntToStr(FPoints[I].Y) + ']';
      end;
      S := S + ']';

      SimbaDebugForm.Add('Debug Image Clicks: ' + S);
    end;

    FPoints := nil;
    FControlDown := False;
  end;
end;

procedure TSimbaDebugImageForm.SetDimensions(W, H: Int32);
begin
  if (DockMaster.GetAnchorSite(Self) <> nil) then
  begin
    DockMaster.GetAnchorSite(Self).Width := W;
    DockMaster.GetAnchorSite(Self).Height := H + DockMaster.GetAnchorSite(Self).Header.Height;
  end else
  begin
    Width := W;
    Height := H;
  end;
end;

constructor TSimbaDebugImageForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ImageBox := TSimbaImageBox.Create(Self);
  ImageBox.Parent := Self;
  ImageBox.Align := alClient;
  ImageBox.OnImageDoubleClick := @ImageDoubleClick;
end;

initialization
  {$R *.lfm}

end.

