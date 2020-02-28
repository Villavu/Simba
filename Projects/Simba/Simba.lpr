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
{$R ../SimbaResources/SimbaResources.res}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF LINUX}
  simba.linux_initialization,
  {$ENDIF}
  classes, interfaces, forms, sysutils,
  simba.settings, simba.main, simba.aboutform, simba.debugimage, simba.bitmapconv,
  simba.functionlistform, simba.scripttabsform, simba.debugform, simba.filebrowserform,
  simba.notesform, simba.package_form, simba.colorpicker_historyform, simba.mufasabase,
  simba.settingsform
  {$IFDEF USE_FORMDESIGNER},
  simba.formdesigner
  {$ENDIF};

type
  TApplicationHelper = class helper for TApplication
    procedure CreateForm(InstanceClass: TComponentClass; out Reference);
  end;

procedure TApplicationHelper.CreateForm(InstanceClass: TComponentClass; out Reference);
begin
  WriteLn('Create ', InstanceClass.ClassName);

  inherited CreateForm(InstanceClass, Reference);
end;

begin
  {$IF DECLARED(SetHeapTraceOutput)}
  if FileExists('memoryleaks.trc') then
    DeleteFile('memoryleaks.trc');

  SetHeapTraceOutput('memoryleaks.trc');
  {$ENDIF}

  WriteLn('Simba ', IntToStr(SimbaVersion));
  WriteLn('Built at ', {$I %TIME%}, ' on ', {$I %DATE%});
  WriteLn('');

  if Application.HasOption('help') or (Application.ParamCount = 2) and (Application.CheckOptions('', ['open', 'compile', 'run']) <> '') then
  begin
    WriteLn(
      'Options:'                               + LineEnding +
      '  --open:    Opens the given script'    + LineEnding +
      '  --run:     Runs the given script'     + LineEnding +
      '  --compile: Compiles the given script' + LineEnding +
      ''                                       + LineEnding +
      'Example:'                               + LineEnding +
      '  Simba.exe --run "script.simba"'       + LineEnding +
      ''
    );

    Halt(0);
  end;

  Application.Title := 'Simba';
  Application.Scaled := True;
  Application.ShowMainForm := False;
  Application.Initialize();

  Application.CreateForm(TSimbaForm, SimbaForm);
  Application.CreateForm(TSimbaFunctionListForm, SimbaFunctionListForm);
  Application.CreateForm(TSimbaDebugImageForm, SimbaDebugImageForm);
  Application.CreateForm(TSimbaNotesForm, SimbaNotesForm);
  Application.CreateForm(TSimbaScriptTabsForm, SimbaScriptTabsForm);
  Application.CreateForm(TSimbaDebugForm, SimbaDebugForm);
  Application.CreateForm(TSimbaFileBrowserForm, SimbaFileBrowserForm);
  Application.CreateForm(TSimbaAboutForm, SimbaAboutForm);
  Application.CreateForm(TSimbaSettingsForm, SimbaSettingsForm);
  Application.CreateForm(TSimbaBitmapConversionForm, SimbaBitmapConversionForm);
  Application.CreateForm(TSimbaPackageForm, SimbaPackageForm);
  Application.CreateForm(TSimbaColorHistoryForm, SimbaColorHistoryForm);
  {$IFDEF USE_FORMDESIGNER}
  Application.CreateForm(TCompForm, CompForm);
  {$ENDIF}

  WriteLn('');

  Application.QueueASyncCall(@SimbaForm.Initialize, 0);
  Application.Run();
end.

