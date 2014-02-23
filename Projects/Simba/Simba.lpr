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

    Simba for the Mufasa Macro Library
}

program Simba;

{$mode objfpc}{$H+}

{$I Simba.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}cthreads, cmem,{$ENDIF}{$ENDIF}
  Interfaces, Forms, SimbaUnit, colourhistory, About, debugimage, bitmapconv,
   debugger, selectonruntime, code, cselectonruntime, cstruct,
  {$IFDEF USE_FORMDESIGNER}design_frm, frmdesigner,{$ENDIF}
  sclist, dcpbase64,sm_main;

{$R Simba.res}

begin
  Application.Initialize;

  Application.CreateForm(TSimbaForm, SimbaForm);
  Application.CreateForm(TColourHistoryForm, ColourHistoryForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TDebugImgForm, DebugImgForm);
//  Application.CreateForm(TExtensionsForm, ExtensionsForm);
  Application.CreateForm(TBitmapConvForm, BitmapConvForm);
  Application.CreateForm(TSmanager, SManager);
  {$IFDEF USE_FORMDESIGNER}Application.CreateForm(TCompForm,CompForm);{$ENDIF}
//  Application.CreateForm(TDebuggerForm, DebuggerForm);
//  Application.CreateForm(TSimbaUpdateForm, SimbaUpdateForm);
//  Application.CreateForm(TSettingsSimpleForm, SettingsSimpleForm); Done in FormCreate of MainForm
//  Application.CreateForm(TSettingsForm, SettingsForm); Done in FormCreate of MainForm
  Application.Run;
end.

