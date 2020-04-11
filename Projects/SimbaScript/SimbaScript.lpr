program SimbaScript;

{$mode objfpc}{$H+}

{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

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
  sysutils, classes, interfaces, forms,
  simbascript.script, simba.ipc, simbascript.dumper;

{$R *.res}

type
  TApplicationHelper = class helper for TApplication
  public
    procedure Terminate(Sender: TObject);
    procedure RunScript(Data: PtrInt);
    procedure DumpPlugin(Data: PtrInt);
    procedure DumpCompiler(Data: PtrInt);
  end;

procedure TApplicationHelper.RunScript(Data: PtrInt);
begin
  with TSimbaScript.Create(True) do
  begin
    FreeOnTerminate := True;
    OnTerminate := @Self.Terminate;
    CompileOnly := Application.HasOption('compile');

    ScriptFile := Application.Params[Application.ParamCount];

    with TStringList.Create() do
    try
      LoadFromFile(ScriptFile);

      Script := Text;
    finally
      Free();
    end;

    if Application.HasOption('scriptname') then
    begin
      if ExtractFileExt(ScriptFile) = '.tmp' then
        DeleteFile(ScriptFile);

      ScriptFile := Application.GetOptionValue('scriptname');
    end;

    AppPath := Application.GetOptionValue('apppath');
    if AppPath = '' then
      AppPath := IncludeTrailingPathDelimiter(Application.Location);

    DataPath := Application.GetOptionValue('datapath');
    if DataPath = '' then
      DataPath := IncludeTrailingPathDelimiter(Application.Location) + 'Data';

    PluginPath := Application.GetOptionValue('pluginpath');
    if PluginPath = '' then
      PluginPath := IncludeTrailingPathDelimiter(Application.Location) + 'Plugins';

    FontPath := Application.GetOptionValue('fontpath');
    if FontPath = '' then
      FontPath := IncludeTrailingPathDelimiter(Application.Location) + 'Fonts';

    IncludePath := Application.GetOptionValue('includepath');
    if IncludePath = '' then
      IncludePath := IncludeTrailingPathDelimiter(Application.Location) + 'Includes';

    if Application.HasOption('target') then
      Target := Application.GetOptionValue('target').ToInt64();

    if Application.HasOption('simba-output-server') then
      SimbaOutputServer := TSimbaIPC_Client.Create(Application.GetOptionValue('simba-output-server'));

    if Application.HasOption('simba-method-server') then
      SimbaMethodServer := TSimbaIPC_Client.Create(Application.GetOptionValue('simba-method-server'));

    if Application.HasOption('simba-state-server') then
      SimbaStateServer := TSimbaIPC_Client.Create(Application.GetOptionValue('simba-state-server'));

    Start();
  end;
end;

procedure TApplicationHelper.Terminate(Sender: TObject);
begin
  inherited Terminate();

  if Sender is TThread then
    with Sender as TThread do
    begin
      if (FatalException <> nil) then
      begin
        WriteLn(Exception(FatalException).Message);

        ExitCode := 1;
      end;
    end;

  {$IFDEF DARWIN}
  CocoaWidgetSet.NSApp.Terminate(nil); // Lazarus doesn't exit the message loop
  {$ELSE}
  if (WakeMainThread <> nil) then
    WakeMainThread(nil);
  {$ENDIF}
end;

procedure TApplicationHelper.DumpPlugin(Data: PtrInt);
begin
  with TSimbaScript_PluginDumper.Create(True) do
  begin
    OnTerminate := @Self.Terminate;
    FileName := Application.Params[Application.ParamCount];
    Start();
  end;
end;

procedure TApplicationHelper.DumpCompiler(Data: PtrInt);
begin
  with TSimbaScript_CompilerDumper.Create(True) do
  begin
    OnTerminate := @Self.Terminate;
    Start();
  end;
end;

var
  I: Int32;

begin
  try
    Application.Title := 'SimbaScript';
    Application.CaptureExceptions := False;
    Application.Scaled := True;
    Application.Initialize();

    if Application.HasOption('dump-compiler') then
    begin
      Application.QueueAsyncCall(@Application.DumpCompiler, 0);
    end else
    if Application.HasOption('dump-plugin') then
    begin
      if (not FileExists(Application.Params[Application.ParamCount])) then
      begin
        WriteLn('Plugin not found');

        Halt(1);
      end;

      Application.QueueAsyncCall(@Application.DumpPlugin, 0)
    end else
    if Application.HasOption('run') or Application.HasOption('compile') then
    begin
      if (not FileExists(Application.Params[Application.ParamCount])) then
      begin
        WriteLn('Script not found');

        Halt(1);
      end;

      Application.QueueAsyncCall(@Application.RunScript, 0);
    end else
    begin
      WriteLn(
          'Options:'                                                       + LineEnding +
          '  --run:          Runs the given script'                        + LineEnding +
          '  --compile:      Compiles the given script'                    + LineEnding +
          ''                                                               + LineEnding +
          '  --target:       Window handle to target. Defaults to Desktop' + LineEnding +
          '  --apppath:      Defaults to SimbaScript.exe location'         + LineEnding +
          '  --datapath:     Defaults to AppPath/Data'                     + LineEnding +
          '  --pluginpath:   Defaults to AppPath/Plugins'                  + LineEnding +
          '  --fontpath:     Defaults to AppPath/Fonts'                    + LineEnding +
          '  --includepath:  Defaults to AppPath/Includes'                 + LineEnding +
          ''                                                               + LineEnding +
          'Example:'                                                       + LineEnding +
          '  SimbaScript.exe --run "script.simba"'                         + LineEnding +
          ''
        );

      Halt(0);
    end;

    Application.Run();
  except
    on E: Exception do
    begin
      ExitCode := 1;

      WriteLn(StringOfChar('-', 80));
      WriteLn('');
      WriteLn('Exception: ', E.Message);
      WriteLn('Exception Class: ', E.ClassName);
      WriteLn('');
      WriteLn('Parameters:');
      for i := 1 to Application.ParamCount + 1 do
        WriteLn('  ' + Application.Params[i]);

      WriteLn(StringOfChar('-', 80));
    end;
  end;
end.

