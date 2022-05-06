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
  classes, sysutils, interfaces, forms, lazlogger,
  simba.settings, simba.main, simba.aboutform, simba.debugimageform,
  simba.bitmapconv, simba.functionlistform, simba.scripttabsform,
  simba.outputform, simba.colorpickerhistoryform, simba.filebrowserform,
  simba.notesform, simba.package_form, simba.settingsform, simba.associate,
  simba.script_dump, simba.openexampleform, simba.httpclient, simba.scriptthread;

type
  TApplicationHelper = class helper for TApplication
    procedure HandleAnalytics(Data: PtrInt);
    procedure SendAnalytics;

    procedure DebugLnSilent(Sender: TObject; S: string; var Handled: Boolean);
  end;

procedure TApplicationHelper.HandleAnalytics(Data: PtrInt);
begin
  TThread.ExecuteInThread(@SendAnalytics);
end;

procedure TApplicationHelper.SendAnalytics;
begin
  if HasOption('secret') then
    Exit;

  with TSimbaHTTPClient.Create() do
  try
    // Simple HTTP request - nothing extra is sent.
    // Only used for logging very basic (ide) launch count.
    Get(SIMBA_ANALYTICS_URL);
  finally
    Free();
  end;
end;

procedure TApplicationHelper.DebugLnSilent(Sender: TObject; S: string; var Handled: Boolean);
begin
  Handled := True;
end;

begin
  {$IF DECLARED(SetHeapTraceOutput)}
  SetHeapTraceOutput(IntToStr(GetProcessID()) + '.trc');
  {$ENDIF}

  Application.CaptureExceptions := False;
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

  DebugLogger.CloseLogFileBetweenWrites := True;
  if Application.HasOption('silent') then
    DebugLogger.OnDebugLn := @Application.DebugLnSilent;

  if not Application.HasOption('open') and Application.HasOption('run') or Application.HasOption('compile') then
  begin
    if not FileExists(Application.Params[Application.ParamCount]) then
    begin
      DebugLn('Script "' + Application.Params[Application.ParamCount] + '" does not exist.');

      Halt();
    end;

    SimbaScriptThread := TSimbaScriptThread.Create(
      Application.Params[Application.ParamCount],
      Application.GetOptionValue('simbacommunication'),
      Application.GetOptionValue('target'),
      Application.HasOption('compile'),
      Application.HasOption('debugging')
    );
  end else
  begin
    Application.CreateForm(TSimbaForm, SimbaForm);
    Application.CreateForm(TSimbaFunctionListForm, SimbaFunctionListForm);
    Application.CreateForm(TSimbaDebugImageForm, SimbaDebugImageForm);
    Application.CreateForm(TSimbaNotesForm, SimbaNotesForm);
    Application.CreateForm(TSimbaScriptTabsForm, SimbaScriptTabsForm);
    Application.CreateForm(TSimbaOutputForm, SimbaOutputForm);
    Application.CreateForm(TSimbaFileBrowserForm, SimbaFileBrowserForm);
    Application.CreateForm(TSimbaAboutForm, SimbaAboutForm);
    Application.CreateForm(TSimbaSettingsForm, SimbaSettingsForm);
    Application.CreateForm(TSimbaBitmapConversionForm, SimbaBitmapConversionForm);
    Application.CreateForm(TSimbaPackageForm, SimbaPackageForm);
    Application.CreateForm(TSimbaOpenExampleForm, SimbaOpenExampleForm);
    Application.CreateForm(TSimbaColorPickerHistoryForm, SimbaColorPickerHistoryForm);

    Application.QueueAsyncCall(@Application.HandleAnalytics, 0);
  end;
  Application.Run();
end.
