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
{$R Simba.res}

uses
  {$IFDEF UNIX}
  cthreads, cmem, linux_startup,
  {$ENDIF}
  Interfaces, Forms,
  simbaunit, colourhistory, about, debugimage, bitmapconv, updateform, simbasettingsold, simbasettingssimple,
  {$IFDEF USE_FORMDESIGNER}
  design_frm, frmdesigner,
  {$ENDIF}
  // initialize in import order.
  script_import_system, script_import_classes, script_import_target, script_import_input,
  script_import_finder, script_import_web, script_import_arrays_algorithms, script_import_matrix,
  script_import_math, script_import_time_date, script_import_ocr, script_import_string,
  script_import_simba, script_import_colormath, script_import_bitmap, script_import_settings,
  script_import_dtm, script_import_file, script_import_other, script_import_script,
  script_import_crypto, script_import_deprecated;

begin
  {$IF DECLARED(SetHeapTraceOutput)}
  SetHeapTraceOutput('trace.trc');
  {$ENDIF}

  Application.Initialize();
  Application.CreateForm(TSimbaForm, SimbaForm);
  Application.CreateForm(TColourHistoryForm, ColourHistoryForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TDebugImgForm, DebugImgForm);
  Application.CreateForm(TBitmapConvForm, BitmapConvForm);
  Application.CreateForm(TSimbaUpdateForm, SimbaUpdateForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TSettingsSimpleForm, SettingsSimpleForm);
  {$IFDEF USE_FORMDESIGNER}
  Application.CreateForm(TCompForm, CompForm);
  {$ENDIF}

  Application.Run();
end.

