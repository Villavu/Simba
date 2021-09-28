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

    about form for the Mufasa Macro Library
}
unit simba.aboutform;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSimbaAboutForm = class(TForm)
    AboutMemo: TMemo;
    ButtonExit: TButton;
    ImageSimba: TImage;
    VersionLabel: TLabel;
    LabelTitle: TLabel;
    procedure ButtonExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VersionLabelClick(Sender: TObject);
    procedure VersionLabelMouseEnter(Sender: TObject);
    procedure VersionLabelMouseLeave(Sender: TObject);
  end; 

var
  SimbaAboutForm: TSimbaAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, LazVersion,
  simba.main;

procedure TSimbaAboutForm.FormShow(Sender: TObject);
begin
  LabelTitle.Caption := Format('Simba %d', [SIMBA_VERSION]);
  if (SIMBA_COMMIT <> '') then
    {%H-}VersionLabel.Caption := Format('(%s)', [SIMBA_COMMIT]);

  AboutMemo.Lines.Clear();
  AboutMemo.Lines.Add('Simba is released under the GPL license.');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('You are currently using version: %d', [SIMBA_VERSION]);
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('SHA1:');
  AboutMemo.Lines.Add('  %s', [SimbaForm.BinaryHash]);
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Built with:');
  AboutMemo.Lines.Add('  FPC %d.%d.%d', [FPC_VERSION, FPC_RELEASE, FPC_PATCH]);
  AboutMemo.Lines.Add('  Lazarus version %s', [LAZ_VERSION]);
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Please report bugs at: http://bugs.villavu.com/');
end;

procedure TSimbaAboutForm.ButtonExitClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaAboutForm.FormCreate(Sender: TObject);
begin
  Width := 500;
  Height := 450;
end;

procedure TSimbaAboutForm.VersionLabelClick(Sender: TObject);
begin
  if (SIMBA_COMMIT <> '') then
    {%H-}OpenURL(SIMBA_COMMIT_URL);
end;

procedure TSimbaAboutForm.VersionLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := True;
end;

procedure TSimbaAboutForm.VersionLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := False;
end;

end.

