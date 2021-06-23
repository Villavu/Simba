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

{$R Simba.res}

uses
  simba.init, // Things that must happen before other units are initialized
  classes, sysutils, interfaces, forms,
  simba.main, simba.aboutform, simba.debugimage, simba.bitmapconv,
  simba.functionlistform, simba.scripttabsform, simba.debugform,
  simba.filebrowserform, simba.notesform, simba.package_form,
  simba.colorpicker_historyform, simba.settingsform,
  simba.associate, simba.script, simba.script_dump, simba.scripttemplateform;

begin
  {$IF DECLARED(SetHeapTraceOutput)}
  SetHeapTraceOutput('memory-leaks.trc');
  {$ENDIF}

  Application.Title := 'Simba';
  Application.Scaled := True;
  Application.ShowMainForm := False;
  Application.Initialize();

  if Application.HasOption('dump') then
  begin
    with DumpPlugin(Application.GetOptionValue('dump')) do
      SaveToFile(Application.Params[Application.ParamCount]);

    Halt();
  end;

  if Application.HasOption('associate') then
  begin
    Associate();

    Halt();
  end;

  if not Application.HasOption('open') and Application.HasOption('run') or Application.HasOption('compile') then
  begin
    if not FileExists(Application.Params[Application.ParamCount]) then
    begin
      WriteLn('Script "' + Application.Params[Application.ParamCount] + '" does not exist.');

      Halt();
    end;

    SimbaScript := TSimbaScript.Create();

    SimbaScript.ScriptFile               := Application.Params[Application.ParamCount];
    SimbaScript.ScriptName               := Application.GetOptionValue('scriptname');
    SimbaScript.Debugging                := Application.HasOption('debugging');
    SimbaScript.CompileOnly              := Application.HasOption('compile');
    SimbaScript.SimbaCommunicationServer := Application.GetOptionValue('simbacommunication');
    SimbaScript.Target                   := Application.GetOptionValue('target');
    SimbaScript.Log                      := Application.GetOptionValue('log');

    SimbaScript.Start();
  end else
  begin
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
    Application.CreateForm(TSimbaScriptTemplateForm, SimbaScriptTemplateForm);

    Application.QueueASyncCall(@SimbaForm.Setup, 0);
  end;

  Application.Run();
end.

