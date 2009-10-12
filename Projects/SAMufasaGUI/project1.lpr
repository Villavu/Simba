{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venentië and Merlijn Wajer

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

    SAMufasaGUI for the Mufasa Macro Library
}                 

program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads, cmem,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, Window, files, MufasaTypes, Client, TestUnit, finder,
  MMLThread, mmlpsthread, bitmaps, colour_conv, plugins, DTM, colourpicker,
  windowselector, dtmutil, mmath;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
  Application.Title:='Mufasa Stand Alone';
  {$I project1.lrs}
  Application.Initialize;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

