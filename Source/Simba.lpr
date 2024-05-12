{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
program Simba;

{$I simba.inc}
{$R Simba.res}

uses
  simba.init,
  Classes, SysUtils, Interfaces, Forms,
  simba.base,
  simba.form_main, simba.form_tabs, simba.form_about, simba.form_debugimage, simba.form_imagestring,
  simba.form_functionlist, simba.form_output, simba.form_colorpickhistory, simba.form_filebrowser,
  simba.form_notes, simba.form_settings, simba.form_openexample, simba.form_shapebox,
  simba.form_backups, simba.form_findinfiles, simba.form_downloadsimba, simba.form_package,
  simba.compiler_dump, simba.plugin_dump, simba.script_runner,
  simba.ide_initialization;

begin
  {$IF DECLARED(SetHeapTraceOutput)}
  SetHeapTraceOutput(Application.Location + '/' + IntToStr(GetProcessID()) + '.trc');
  {$ENDIF}

  Application.CaptureExceptions := False;
  Application.Initialize();

  if Application.HasOption('help') then
  begin
    DebugLn('Usage:');
    DebugLn('  Simba.exe [options] "path/to/script.simba"');
    DebugLn('');
    DebugLn('Options:');
    DebugLn('  --run       Run a script');
    DebugLn('  --compile   Compile a script');
    DebugLn('  --open      Open a script in Simba');
    DebugLn('');
    DebugLn('Examples:');
    DebugLn('  Run a script without opening Simba:');
    DebugLn('    Simba.exe --run "script.simba"');
    DebugLn('');
    DebugLn('  Open a script in Simba');
    DebugLn('    Simba.exe --open "script.simba"');
    DebugLn('');
    DebugLn('  Open a script in Simba and run');
    DebugLn('    Simba.exe --open --run "script.simba"');
    DebugLn('');

    Halt();
  end;

  if Application.HasOption('dumpcompiler') then
  begin
    DumpCompiler(Application.Params[Application.ParamCount]);

    Halt();
  end;

  if Application.HasOption('dumpplugin') then
  begin
    with DumpPlugin(Application.GetOptionValue('dumpplugin')) do
      SaveToFile(Application.Params[Application.ParamCount]);

    Halt();
  end;

  if (not Application.HasOption('open')) and (Application.HasOption('run') or Application.HasOption('compile')) then
  begin
    if Application.HasOption('simbacommunication') then
      SimbaProcessType := ESimbaProcessType.SCRIPT_WITH_COMMUNICATION
    else
      SimbaProcessType := ESimbaProcessType.SCRIPT;

    // Script will be sent though communication
    if (Application.Params[Application.ParamCount] = '--run') or (Application.Params[Application.ParamCount] = '--compile') then
    begin
      TSimbaScriptRunner.Create(
        Application.GetOptionValue('simbacommunication'),
        Application.GetOptionValue('target'),
        Application.HasOption('compile'),
        Application.HasOption('hints')
      );
    end else
    // Script will be loaded from file
    begin
      if not FileExists(Application.Params[Application.ParamCount]) then
      begin
        DebugLn('Script "' + Application.Params[Application.ParamCount] + '" does not exist.');
        Halt();
      end;

      TSimbaScriptRunner.Create(
        Application.Params[Application.ParamCount],
        Application.GetOptionValue('simbacommunication'),
        Application.GetOptionValue('target'),
        Application.HasOption('compile'),
        Application.HasOption('hints')
      );
    end;
  end else
  begin
    SimbaProcessType := ESimbaProcessType.IDE;

    SimbaIDEInitialization_CallBeforeCreate();

    Application.ShowMainForm := False;
    Application.CreateForm(TSimbaMainForm, SimbaMainForm);
    Application.CreateForm(TSimbaTabsForm, SimbaTabsForm);
    Application.CreateForm(TSimbaFunctionListForm, SimbaFunctionListForm);
    Application.CreateForm(TSimbaDebugImageForm, SimbaDebugImageForm);
    Application.CreateForm(TSimbaNotesForm, SimbaNotesForm);
    Application.CreateForm(TSimbaOutputForm, SimbaOutputForm);
    Application.CreateForm(TSimbaFileBrowserForm, SimbaFileBrowserForm);
    Application.CreateForm(TSimbaAboutForm, SimbaAboutForm);
    Application.CreateForm(TSimbaSettingsForm, SimbaSettingsForm);
    Application.CreateForm(TSimbaImageStringForm, SimbaImageStringForm);
    Application.CreateForm(TSimbaOpenExampleForm, SimbaOpenExampleForm);
    Application.CreateForm(TSimbaColorPickHistoryForm, SimbaColorPickHistoryForm);
    Application.CreateForm(TSimbaShapeBoxForm, SimbaShapeBoxForm);
    Application.CreateForm(TSimbaBackupsForm, SimbaBackupsForm);
    Application.CreateForm(TSimbaFindInFilesForm, SimbaFindInFilesForm);
    Application.CreateForm(TSimbaDownloadSimbaForm, SimbaDownloadSimbaForm);
    Application.CreateForm(TSimbaPackageForm, SimbaPackageForm);

    SimbaIDEInitialization_CallBeforeShow();
  end;

  Application.Run();
end.
