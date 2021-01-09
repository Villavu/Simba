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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSimbaAboutForm = class(TForm)
    AboutMemo: TMemo;
    ButtonClose: TButton;
    ImageSimba: TImage;
    LabelTitle: TLabel;
    LabelRevision: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  end; 

var
  SimbaAboutForm: TSimbaAboutForm;

implementation

procedure TSimbaAboutForm.FormCreate(Sender: TObject);
begin
  Self.Caption := Format('About Simba r%d', [SIMBA_VERSION]);
  Self.LabelRevision.Caption := Format('Revision %d', [SIMBA_VERSION]);

  AboutMemo.Lines.Add('Simba is released under the GPL license.');
  AboutMemo.Lines.Add(Format('You are currently using version: %d',[SIMBA_VERSION]));
  AboutMemo.Lines.Add(Format('Compiled with FPC version %d.%d.%d', [FPC_VERSION, FPC_RELEASE, FPC_PATCH]));
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Please report bugs at: http://bugs.villavu.com/');
  AboutMemo.ReadOnly := True;
end;

procedure TSimbaAboutForm.OkButtonClick(Sender: TObject);
begin
  Self.ModalResult := mrOK;
  Self.Hide;
end;

initialization
  {$R *.lfm}

end.

