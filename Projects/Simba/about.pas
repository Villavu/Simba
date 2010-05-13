{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AboutMemo: TMemo;
    ButtonClose: TButton;
    ImageSimba: TImage;
    LabelTitle: TLabel;
    LabelRevision: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation
uses
  SimbaUnit;
{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Self.Caption := format('About Simba r%d', [SimbaUnit.SimbaVersion]);
  Self.LabelRevision.Caption := format('Revision %d', [SimbaUnit.SimbaVersion]);
  AboutMemo.Lines.Add('Simba is released under the GPL license.');
  AboutMemo.Lines.Add(format('You are currently using version: %d',[SimbaUnit.SimbaVersion]));
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Please report bugs at: http://mufasa.villavu.com/mantis/');
end;

procedure TAboutForm.OkButtonClick(Sender: TObject);
begin
  Self.ModalResult:=mrOK;
  Self.Hide;
end;

initialization
  {$R *.lfm}

end.

