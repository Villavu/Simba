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
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AboutMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation
uses
  TestUnit;
{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  AboutMemo.Lines.Add('---Simba---');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('');
  AboutMemo.Lines.Add('Simba is released under the GPL license.');
  AboutMemo.Lines.Add(format('You are currently using version: %d',[Testunit.SimbaVersion]));
end;

initialization
  {$I about.lrs}

end.

