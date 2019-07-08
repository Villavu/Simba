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
  simba.oswindow, mufasatypes;

type
  TMWindowSelector = class(TObject)
  protected
    FLastPick: TOSWindow;
    FHasPicked: Boolean;
    FForm: TForm;
  public
    constructor Create;
    destructor Destroy; override;

    property HasPicked: Boolean read FHasPicked;
    property LastPick: TOSWindow read FLastPick;

    function Drag: TOSWindow;
  end;


implementation

{$IFDEF WINDOWS}
  {$INCLUDE windowselector_windows.inc}
{$ENDIF}

{$IFDEF LINUX}
  {$INCLUDE windowselector_linux.inc}
{$ENDIF}

constructor TMWindowSelector.Create;
begin
  FLastPick := 0;
  FHasPicked := False;

  FForm := TForm.Create(nil);
  FForm.Scaled := False;
  FForm.BorderStyle := bsNone;
  FForm.Color := clGreen;
  FForm.AlphaBlend := True;
  FForm.AlphaBlendValue := 150;
  FForm.ShowInTaskBar := stNever;
end;

destructor TMWindowSelector.Destroy;
begin
  FForm.Free();

  inherited Destroy();
end;

end.

