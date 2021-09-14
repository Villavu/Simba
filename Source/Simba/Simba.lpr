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
{$I simba.inc}
{$R Simba.res}

uses
  simba.init,
  classes, sysutils, interfaces, forms, lazloggerbase,
  simba.settings, simba.main, simba.aboutform, simba.debugimage,
  simba.bitmapconv, simba.functionlistform, simba.scripttabsform,
  simba.debugform, simba.colorpickerhistoryform, simba.filebrowserform,
  simba.notesform, simba.package_form, simba.settingsform, simba.associate,
  simba.script, simba.script_dump, simba.openexampleform;

type
  TApplicationHelper = class helper for TApplication
    procedure HandleException(Sender: TObject; E: Exception);
  end;

procedure TApplicationHelper.HandleException(Sender: TObject; E: Exception);
begin
  { no graphical error message at this point }
end;

begin
  {$IF DECLARED(SetHeapTraceOutput)}
  SetHeapTraceOutput('memory-leaks.trc');
  {$ENDIF}

  Application.OnException := @Application.HandleException;
  Application.Title := 'Simba';
  Application.Scaled := True;
  Application.Initialize();

  if Application.HasOption('dumpcompiler') then
  begin
    with DumpCompiler() do
      SaveToFile(Application.Params[Application.ParamCount]);

    Halt();
  end;

  if Application.HasOption('dumpplugin') then
  begin
    with DumpPlugin(Application.GetOptionValue('dumpplugin')) do
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
      DebugLn('Script "' + Application.Params[Application.ParamCount] + '" does not exist.');

      Halt();
    end;

    SimbaScript := TSimbaScript.Create();

    SimbaScript.ScriptFile               := Application.Params[Application.ParamCount];
    SimbaScript.ScriptName               := Application.GetOptionValue('scriptname');
    SimbaScript.SimbaCommunicationServer := Application.GetOptionValue('simbacommunication');
    SimbaScript.Target                   := Application.GetOptionValue('target');
    SimbaScript.Debugging                := Application.HasOption('debugging');
    SimbaScript.CompileOnly              := Application.HasOption('compile');
    SimbaScript.Silent                   := Application.HasOption('silent');

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
    Application.CreateForm(TSimbaOpenExampleForm, SimbaOpenExampleForm);
    Application.CreateForm(TSimbaColorPickerHistoryForm, SimbaColorPickerHistoryForm);
  end;

  Application.Run();
end.

