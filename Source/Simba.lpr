{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
program Simba;

{$i simba.inc}
{$R Simba.res}

uses
  simba.init,
  Classes, SysUtils, Interfaces, Forms,
  simba.mufasatypes, simba.main,
  simba.aboutform, simba.debugimageform, simba.imagetostringform, simba.functionlistform,
  simba.scripttabsform, simba.outputform, simba.colorpickerhistoryform, simba.filebrowserform,
  simba.notesform, simba.settingsform, simba.associate, simba.openexampleform,
  simba.package_form, simba.shapeboxform, simba.backupsform, simba.findinfilesform,
  simba.downloadsimbaform,
  simba.compiler_dump, simba.plugin_dump,
  simba.scriptthread,
  simba.ide_initialization, simba.ide_analytics, simba.ide_codetools_setup,
  simba.openssl;

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

  if Application.HasOption('associate') then
  begin
    Associate();

    Halt();
  end;

  if (not Application.HasOption('open')) and (Application.HasOption('run') or Application.HasOption('compile')) then
  begin
    if not FileExists(Application.Params[Application.ParamCount]) then
    begin
      DebugLn('Script "' + Application.Params[Application.ParamCount] + '" does not exist.');

      Halt();
    end;

    if Application.HasOption('simbacommunication') then
      SimbaProcessType := ESimbaProcessType.SCRIPT_WITH_COMMUNICATION
    else
      SimbaProcessType := ESimbaProcessType.SCRIPT;

    TSimbaScriptRunner.Create(
      Application.Params[Application.ParamCount],
      Application.GetOptionValue('simbacommunication'),
      Application.GetOptionValue('target'),
      Application.HasOption('compile')
    );
  end else
  begin
    SimbaProcessType := ESimbaProcessType.IDE;

    SimbaIDEInitialization_CallBeforeCreate();

    Application.ShowMainForm := False;
    Application.CreateForm(TSimbaForm, SimbaForm);
    Application.CreateForm(TSimbaFunctionListForm, SimbaFunctionListForm);
    Application.CreateForm(TSimbaDebugImageForm, SimbaDebugImageForm);
    Application.CreateForm(TSimbaNotesForm, SimbaNotesForm);
    Application.CreateForm(TSimbaScriptTabsForm, SimbaScriptTabsForm);
    Application.CreateForm(TSimbaOutputForm, SimbaOutputForm);
    Application.CreateForm(TSimbaFileBrowserForm, SimbaFileBrowserForm);
    Application.CreateForm(TSimbaAboutForm, SimbaAboutForm);
    Application.CreateForm(TSimbaSettingsForm, SimbaSettingsForm);
    Application.CreateForm(TSimbaImageToStringForm, SimbaImageToStringForm);
    Application.CreateForm(TSimbaOpenExampleForm, SimbaOpenExampleForm);
    Application.CreateForm(TSimbaColorPickerHistoryForm, SimbaColorPickerHistoryForm);
    Application.CreateForm(TSimbaPackageForm, SimbaPackageForm);
    Application.CreateForm(TSimbaShapeBoxForm, SimbaShapeBoxForm);
    Application.CreateForm(TSimbaBackupsForm, SimbaBackupsForm);
    Application.CreateForm(TSimbaFindInFilesForm, SimbaFindInFilesForm);
    Application.CreateForm(TSimbaDownloadSimbaForm, SimbaDownloadSimbaForm);

    SimbaIDEInitialization_CallBeforeShow();
  end;

  Application.Run();
end.
