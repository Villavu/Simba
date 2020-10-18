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

{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

{$R Simba.res}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF LINUX}
  simba.linux_initialization,
  {$ENDIF}
  {$IFDEF DARWIN}
  simba.darwin_initialization, cocoaint,
  {$ENDIF}
  classes, interfaces, forms, sysutils,
  // Simba
  simba.main, simba.aboutform, simba.debugimage, simba.bitmapconv, simba.functionlistform,
  simba.scripttabsform, simba.debugform, simba.filebrowserform, simba.notesform,
  simba.package_form, simba.colorpicker_historyform, simba.settingsform, simba.associate,
  // Simba Script
  simba.script, simba.script_dump;

type
  TApplicationHelper = class Helper for TApplication
    procedure CreateForm(InstanceClass: TComponentClass; out Reference);
    procedure Terminate(Sender: TObject);
  end;

procedure TApplicationHelper.CreateForm(InstanceClass: TComponentClass; out Reference);
begin
  WriteLn('Creating ' + InstanceClass.ClassName);

  inherited CreateForm(InstanceClass, Reference);
end;

procedure TApplicationHelper.Terminate(Sender: TObject);
begin
  inherited Terminate();

  if (WakeMainThread <> nil) then
    WakeMainThread(Self);

  {$IFDEF DARWIN}
  CocoaWidgetSet.NSApp.Terminate(nil);  // MacOS needs extra help
  {$ENDIF}
end;

begin
  Application.Title := 'Simba';
  Application.Scaled := True;
  Application.ShowMainForm := False;
  Application.Initialize();

  if Application.HasOption('open') or (Application.ParamCount <= 1) then
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

    Application.QueueASyncCall(@SimbaForm.Setup, 0);
  end else
  if Application.HasOption('run') or Application.HasOption('compile') then
  begin
    SimbaScript := TSimbaScript.Create();
    SimbaScript.OnTerminate := @Application.Terminate;

    SimbaScript.ScriptFile    := Application.Params[Application.ParamCount];
    SimbaScript.ScriptName    := Application.GetOptionValue('scriptname');

    SimbaScript.AppPath       := Application.GetOptionValue('apppath');
    SimbaScript.DataPath      := Application.GetOptionValue('datapath');
    SimbaScript.PluginPath    := Application.GetOptionValue('pluginpath');
    SimbaScript.FontPath      := Application.GetOptionValue('fontpath');
    SimbaScript.IncludePath   := Application.GetOptionValue('includepath');
    SimbaScript.ScriptPath    := Application.GetOptionValue('scriptpath');

    SimbaScript.CompileOnly   := Application.HasOption('compile');
    SimbaScript.SimbaMethods  := Application.GetOptionValue('simbamethods');
    SimbaScript.Target        := Application.GetOptionValue('target');

    SimbaScript.Start();
  end else
  if Application.HasOption('dump') then
  begin
    with DumpPlugin(Application.GetOptionValue('dump')) do
      SaveToFile(Application.Params[Application.ParamCount]);

    Halt();
  end else
  if Application.HasOption('associate') then
  begin
    Associate();

    Halt();
  end;

  Application.Run();
end.

