program SimbaScript;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF LINUX}
  simba.linux_initialization,
  {$ENDIF}
  {$IFDEF DARWIN}
  simba.darwin_initialization,
  {$ENDIF}
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  sysutils, classes, interfaces, forms,
  simbascript.script, simba.ipc, simbascript.dumper;

{$R *.res}

procedure DumpCompiler;
begin
  with TSimbaScript_CompilerDumper.Create() do
  begin
    Output := Application.Params[Application.ParamCount];

    Start();
  end;
end;

procedure DumpPlugin;
begin
  with TSimbaScript_PluginDumper.Create() do
  begin
    Plugin := Application.GetOptionValue('dump-plugin').Trim(['"']);
    Output := Application.Params[Application.ParamCount];

    Start();
  end;
end;

procedure RunScript;
begin
  with TSimbaScript.Create() do
  begin
    CompileOnly := Application.HasOption('compile');
    Debugging := Application.HasOption('debugging');

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

    ScriptPath := Application.GetOptionValue('scriptpath');
    if ScriptPath = '' then
      ScriptPath := IncludeTrailingPathDelimiter(Application.Location) + 'Scripts';

    if Application.HasOption('target') then
      Target := Application.GetOptionValue('target').ToInt64();

    if Application.HasOption('simbaipc') then
      SimbaIPC := TSimbaIPC_Client.Create(Application.GetOptionValue('simbaipc'));

    Start();
  end;
end;

{$IFDEF WINDOWS}
var
  PID, Mode: UInt32;
{$ENDIF}
begin
  Application.Title := 'SimbaScript';
  Application.Scaled := True;
  Application.Initialize();

  if Application.HasOption('dump-compiler') then
    DumpCompiler()
  else
  if Application.HasOption('dump-plugin') then
    DumpPlugin()
  else
  if Application.HasOption('run') or Application.HasOption('compile') then
    RunScript()
  else
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
      '  --scriptpath:   Defaults to AppPath/Scripts/'                 + LineEnding +
      ''                                                               + LineEnding +
      'Example:'                                                       + LineEnding +
      '  SimbaScript.exe --run "script.simba"'                         + LineEnding +
      ''
    );

    Halt(0);
  end;

  {$IFDEF WINDOWS}
  GetWindowThreadProcessId(GetConsoleWindow(), PID);
  if (PID = GetCurrentProcessID()) then
  begin
    GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode);
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode and not (ENABLE_QUICK_EDIT_MODE or ENABLE_INSERT_MODE));

    SetConsoleTitle(PChar(ExtractFileName(Application.Params[Application.ParamCount])));
  end;
  {$ENDIF}

  Application.Run();
end.

