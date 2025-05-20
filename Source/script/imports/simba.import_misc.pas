unit simba.import_misc;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportMisc(Script: TSimbaScript);

implementation

uses
  clipbrd, dialogs,
  lptypes,
  simba.script_importutil,
  simba.process,
  simba.nativeinterface, simba.settings, simba.compress, simba.env,
  simba.dtmeditor, simba.dialog, simba.threading, simba.target,
  simba.finder_color, simba.finder_image, simba.matchtemplate,
  simba.colormath, simba.aca;

type
  PSimbaTarget = ^TSimbaTarget;
  PProcessID = ^TProcessID;

(*
Misc
====
Miscellaneous methods that dont go in any other sections.
*)

(*
SimbaEnv
--------
```
type
  SimbaEnv = record
    const SimbaPath       = 'PathToSimbaDir';
    const IncludesPath    = 'PathToSimbaDir/Includes';
    const PluginsPath     = 'PathToSimbaDir/Plugins';
    const ScriptsPath     = 'PathToSimbaDir/Scripts';
    const ScreenshotsPath = 'PathToSimbaDir/Screenshots';
    const DataPath        = 'PathToSimbaDir/Data';
    const TempPath        = 'PathToSimbaDir/Data/Temp';
  end;
```

Record which contains constants to Simba environment paths.

*Example:*

```
WriteLn(SimbaEnv.ScriptsPath);
```
*)

(*
ClearSimbaOutput
----------------
```
procedure ClearSimbaOutput;
```

Clear the scripts output box in Simba.
*)
procedure ClearSimbaOutput(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  DebugLn([EDebugLn.CLEAR], '');
end;

(*
SetSimbaSetting
---------------
```
function SetSimbaSetting(Name: String; DefValue: String = ''): String;
```
*)
procedure _LapeGetSimpleSetting(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := SimbaSettings.GetSimpleSetting(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
GetSimbaSetting
---------------
```
procedure GetSimbaSetting(Name, Value: String);
```
*)
procedure _LapeSetSimpleSetting(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaSettings.SetSimpleSetting(PString(Params^[0])^, PString(Params^[1])^);
end;

(*
PlaySound
---------
```
procedure PlaySound(Sound: String);
```
*)
procedure _LapePlaySound(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.PlaySound(PString(Params^[0])^);
end;

(*
StopSound
---------
```
procedure StopSound;
```
*)
procedure _LapeStopSound(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaNativeInterface.StopSound();
end;

(*
Simba
-----
```
procedure Simba;
```
*)
procedure _LapeSimba(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  DebugLn(DecompressString('eJzdlEsOgCAMRPcm3sGEZQP3P54h2hb6AzRunGVnnqUleBys1Grf7LpwDY98x8NEaAPJD0Igl7r9UiQxOBZiVcTkSxHInICIjclWWaDTZJZiEgefJBGEngtAHBAk4jPVbEaECRSI7PejWY0a+6HtPoTdu3lFW92NT9g0xfvyQmdR4+fv9VM1K6hTw5/D73IfBBeSKZ0cut7f'));
end;

(*
SetClipBoard
------------
```
procedure SetClipBoard(Data: string);
```

Sets the systems clipboard string.
*)
procedure _LapeSetClipBoard(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  try
    Clipboard.AsText := PString(Params^[0])^;
  except
  end;
end;

(*
GetClipBoard
------------
```
function GetClipBoard: String;
```

Returns the systems clipboard string.
*)
procedure _LapeGetClipBoard(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  try
    PString(Result)^ := Clipboard.AsText;
  except
  end;
end;

(*
ShowComboDialog
---------------
```
function ShowComboDialog(Caption, Prompt: string; List: TStringArray): Integer;
```
*)
procedure _LapeInputCombo(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PInteger(Result)^ := InputCombo(PString(Params^[0])^, PString(Params^[1])^, PStringArray(Params^[2])^);
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowQueryDialog
---------------
```
function ShowQueryDialog(Caption, Prompt: String; var Value: String): Boolean;
```
*)
procedure _LapeInputQuery(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := InputQuery(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowMessage
-----------
```
procedure ShowMessage(Message: String);
```
*)
procedure _LapeShowMessage(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    ShowMessage(PString(Params^[0])^);
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowDTMEditor
-------------
```
function ShowDTMEditor(Target: TTarget): String;
function ShowDTMEditor: String;
```
*)
procedure _LapeShowDTMEditor(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    ShowDTMEditor(PSimbaTarget(PLapeObject(Params^[0])^)^, False, PString(Result)^);
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowACA
-------
```
function ShowACA(Target: TTarget; Title: String): TColorTolerance;
function ShowACA: TColorTolerance;
```
*)
procedure _LapeShowACA(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    ShowACA(PSimbaTarget(PLapeObject(Params^[0])^)^, False, PColorTolerance(Result)^);
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowDirectoryDialog
-------------------
```
function ShowDirectoryDialog(Title, InitialDirectory: String; out Directory: String): Boolean;
```
*)
procedure _LapeSelectDirectory(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := SelectDirectory(PString(Params^[0])^, PString(Params^[1])^, PString(Params^[2])^);
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowQuestionDialog
------------------
```
function ShowQuestionDialog(Title, Question: String): Boolean;
```
*)
procedure _LapeShowQuestionDialog(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV

  procedure Execute;
  begin
    PBoolean(Result)^ := SimbaQuestionDlg(PString(Params^[0])^, PString(Params^[1])^, []) = ESimbaDialogResult.YES;
  end;

begin
  RunInMainThread(@Execute);
end;

(*
ShowTrayNotification
--------------------
```
procedure ShowTrayNotification(Title, Message: String; Timeout: Integer = 3000);
```
*)
procedure _LapeShowTrayNotfication(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  with TSimbaScript(Params^[0]) do
  begin
    if (SimbaCommunication = nil) then
      SimbaException('ShowTrayNotification requires Simba communication');
    SimbaCommunication.ShowTrayNotification(PString(Params^[1])^, PString(Params^[2])^, PInteger(Params^[3])^);
  end;
end;

(*
SetSimbaTitle
--------------
```
procedure SetSimbaTitle(S: String);
```
*)
procedure _LapeSetSimbaTitle(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  with TSimbaScript(Params^[0]) do
  begin
    if (SimbaCommunication = nil) then
      SimbaException('SetSimbaTitle requires Simba communication');
    SimbaCommunication.SetSimbaTitle(PString(Params^[1])^);
  end;
end;

(*
GetSimbaPID
-----------
```
function GetSimbaPID: TProcessID;
```

Returns the Simba's PID this script is running in.
*)
procedure _LapeGetSimbaPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with TSimbaScript(Params^[0]) do
  begin
    if (SimbaCommunication = nil) then
      SimbaException('GetSimbaPID requires Simba communication');
    PProcessID(Result)^ := SimbaCommunication.GetSimbaTargetPID();
  end;
end;

(*
GetSimbaTargetPID
------------------
```
function GetSimbaTargetPID: TProcessID;
```

Returns the current Simba target PID (what is selected with the crosshair)
*)
procedure _LapeGetSimbaTargetPID(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with TSimbaScript(Params^[0]) do
  begin
    if (SimbaCommunication = nil) then
      SimbaException('GetSimbaTargetPID requires Simba communication');
    PProcessID(Result)^ := SimbaCommunication.GetSimbaTargetPID();
  end;
end;

(*
GetSimbaTargetWindow
--------------------
```
function GetSimbaTargetWindow: TWindowHandle;
```

Returns the current Simba target window (what is selected with the crosshair)
*)
procedure _LapeGetSimbaTargetWindow(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  with TSimbaScript(Params^[0]) do
  begin
    if (SimbaCommunication = nil) then
      SimbaException('GetSimbaTargetWindow requires Simba communication');
    PWindowHandle(Result)^ := SimbaCommunication.GetSimbaTargetWindow();
  end;
end;

(*
SaveScreenshot
--------------
```
function SaveScreenshot: String;
```

Saves a screenshot of the current target.
Returns the screenshot path, which will be in Simba's Screenshots directory.
*)

(*
SaveScreenshot
--------------
```
function SaveScreenshot(FileName: String): String;
```
Saves a screenshot of the current target to the given filename.
Will overwrite if the file already exists.
*)

(*
AddOnTerminate
--------------
```
procedure AddOnTerminate(Proc: procedure);
procedure AddOnTerminate(Proc: procedure of object);
```

Adds a procedure to be called when the script is terminated.
*)

(*
AddOnUserTerminate
------------------
```
procedure AddOnUserTerminate(Proc: procedure);
procedure AddOnUserTerminate(Proc: procedure of object);
```

Adds a procedure to be called when the script is terminated with the stop button being clicked.
*)

(*
AddOnPause
----------
```
procedure AddOnPause(Proc: procedure);
procedure AddOnPause(Proc: procedure of object);
```

Adds a procedure to be called when the script is paused.
*)

(*
AddOnResume
-----------
```
procedure AddOnResume(Proc: procedure);
procedure AddOnResume(Proc: procedure of object);
```

Adds a procedure to be called when the script is resumed from pause.
*)

(*
AddOnResume
-----------
```
procedure AddOnResume(Proc: procedure);
procedure AddOnResume(Proc: procedure of object);
```

Adds a procedure to be called when the script is resumed from pause.
*)

(*
TerminateScript
---------------
```
procedure TerminateScript;
procedure TerminateScript(Reason: String);
```

Terminates the script on the next operation.
*)

(*
PauseScript
-----------
```
procedure PauseScript;
```

Programmatically pauses the script. The only way for the script to resumed is by the user clicking the play button.
*)
procedure _LapePauseScript(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaScript(Params^[0]).State := ESimbaScriptState.STATE_PAUSED;
end;

(*
GetTimeRunning
--------------
```
function GetTimeRunning: UInt64;
```
Returns milliseconds since the script was started.
*)

(*
GetTimeStamp
------------
```
function GetTimeStamp(Format: String = "[hh:mm:ss:uu]"): String;
```
Formats GetTimeRunning using `FormatMilliseconds`
*)

procedure ImportMisc(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    DumpSection := 'Misc';

    addGlobalVar('record Enabled: Boolean; SliceWidth, SliceHeight: Integer; end;', @ColorFinderMultithreadOpts, 'ColorFinderMultithreadOpts');
    addGlobalVar('record Enabled: Boolean; SliceWidth, SliceHeight: Integer; end;', @ImageFinderMultithreadOpts, 'ImageFinderMultithreadOpts');
    addGlobalVar('record Enabled: Boolean; SliceWidth, SliceHeight: Integer; end;', @MatchTemplateMultithreadOpts, 'MatchTemplateMultithreadOpts');

    addGlobalType([
      'record',
      '  const SimbaPath       = "' + SimbaEnv.SimbaPath       + '";',
      '  const IncludesPath    = "' + SimbaEnv.IncludesPath    + '";',
      '  const PluginsPath     = "' + SimbaEnv.PluginsPath     + '";',
      '  const ScriptsPath     = "' + SimbaEnv.ScriptsPath     + '";',
      '  const ScreenshotsPath = "' + SimbaEnv.ScreenshotsPath + '";',
      '  const DataPath        = "' + SimbaEnv.DataPath        + '";',
      '  const TempPath        = "' + SimbaEnv.TempPath        + '";',
      'end;'
    ], 'SimbaEnv');

    addGlobalVar(Script.ScriptFileName, 'SCRIPT_FILE').isConstant := True;
    addGlobalVar(SimbaNativeInterface.UnixTime(), 'SCRIPT_START_TIME').isConstant := True;

    addGlobalFunc(
      'function GetTimeRunning: UInt64;', [
      'begin',
      '  Result := Time() - SCRIPT_START_TIME;',
      'end;'
    ]);

    addGlobalFunc(
      'function GetTimeStamp(Format: String = "[hh:mm:ss:uu]"): String;', [
      'begin',
      '  Result := FormatMilliseconds(Time() - SCRIPT_START_TIME, Format);',
      'end;'
    ]);

    addGlobalMethod('procedure PauseScript;', @_LapePauseScript, Script);
    addGlobalMethod('function GetSimbaPID: TProcessID;', @_LapeGetSimbaPID, Script);
    addGlobalMethod('function GetSimbaTargetPID: TProcessID;', @_LapeGetSimbaTargetPID, Script);
    addGlobalMethod('function GetSimbaTargetWindow: TWindowHandle;', @_LapeGetSimbaTargetWindow, Script);
    addGlobalMethod('procedure SetSimbaTitle(S: String);', @_LapeSetSimbaTitle, Script);
    addGlobalMethod('procedure ShowTrayNotification(Title, Message: String; Timeout: Integer = 3000);', @_LapeShowTrayNotfication, Script);

    addGlobalFunc(
      'procedure TerminateScript; overload;', [
      'begin',
      '  Halt();',
      'end;'
    ]);

    addGlobalFunc(
      'procedure TerminateScript(Reason: String); overload;', [
      'begin',
      '  WriteLn("Script Terminated: " + Reason);',
      '  TerminateScript();',
      'end;'
    ]);

    addGlobalFunc('procedure ClearSimbaOutput', @ClearSimbaOutput);
    addGlobalFunc('function SetSimbaSetting(Name: String; DefValue: String = ""): String', @_LapeGetSimpleSetting);
    addGlobalFunc('procedure GetSimbaSetting(Name, Value: String);', @_LapeSetSimpleSetting);
    addGlobalFunc('procedure PlaySound(Sound: String)', @_LapePlaySound);
    addGlobalFunc('procedure StopSound', @_LapeStopSound);
    addGlobalFunc('procedure Simba', @_LapeSimba);
    addGlobalFunc('procedure SetClipBoard(Data: string)', @_LapeSetClipBoard);
    addGlobalFunc('function GetClipBoard: String', @_LapeGetClipBoard);

    addGlobalFunc('function ShowDirectoryDialog(Title, InitialDirectory: String; out Directory: String): Boolean;', @_LapeSelectDirectory);
    addGlobalFunc('function ShowQueryDialog(Caption, Prompt: String; var Value: String): Boolean', @_LapeInputQuery);
    addGlobalFunc('function ShowComboDialog(Caption, Prompt: string; List: TStringArray): Integer', @_LapeInputCombo);
    addGlobalFunc('procedure ShowMessage(Message: String)', @_LapeShowMessage);
    addGlobalFunc('function ShowQuestionDialog(Title, Question: String): Boolean', @_LapeShowQuestionDialog);
    addGlobalFunc('function ShowDTMEditor(Target: TTarget): String; overload', @_LapeShowDTMEditor);
    addGlobalFunc('function ShowACA(Target: TTarget): TColorTolerance; overload', @_LapeShowACA);

    addGlobalFunc(
      'function ShowDTMEditor: String; overload;', [
      'begin',
      '  Result := ShowDTMEditor(Target);',
      'end;'
    ]);

    addGlobalFunc(
      'function ShowACA: TColorTolerance; overload;', [
      'begin',
      '  Result := ShowACA(Target);',
      'end;'
    ]);

    addGlobalFunc(
      'function SaveScreenshot: String; overload;', [
      'var',
      '  FileName: String;',
      '  T: Int64;',
      '  Image: TImage;',
      'begin',
      '  Image := Target.GetImage();',
      '  FileName := SimbaEnv.ScreenshotsPath + PathExtractNameWithoutExt(SCRIPT_FILE) + #32 + TDateTime.CreateFromSystem().ToString("dd-mm hh-mm-ss") + ".png";',
      '  if Image.Save(FileName) then',
      '    Exit(FileName);',
      '',
      '  // File not available! Try for a little with milliseconds (zzz)',
      '  T := Time() + 1000;',
      '  while (Time() < T) do',
      '  begin',
      '    FileName := SimbaEnv.ScreenshotsPath + PathExtractNameWithoutExt(SCRIPT_FILE) + #32 + TDateTime.CreateFromSystem().ToString("dd-mm hh-mm-ss-zzz") + ".png";',
      '    if Image.Save(FileName) then',
      '      Exit(FileName);',
      '',
      '    Sleep(10);',
      '  end;',
      'end;'
    ]);

    addGlobalFunc(
      'function SaveScreenshot(FileName: String): String; overload;', [
      'var',
      '  Image: TImage;',
      'begin',
      '  if (PathExtractExt(FileName) = "") then',
      '    FileName := FileName + ".png";',
      '',
      '  Image := Target.GetImage();',
      '  if Image.Save(FileName, True) then',
      '    Result := FileName;',
      'end;'
    ]);

    addDelayedCode([
      'var',
      '  _TerminateEvents, _UserTerminateEvents, _PauseEvents, _ResumeEvents: array of record',
      '    Proc: procedure;',
      '    Method: procedure of object;',
      '  end;',
      '',
      'procedure _CallOnTerminate;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_TerminateEvents) do',
      '  begin',
      '    if Assigned(_TerminateEvents[I].Proc) then _TerminateEvents[I].Proc();',
      '    if Assigned(_TerminateEvents[I].Method) then _TerminateEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnUserTerminate;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_UserTerminateEvents) do',
      '  begin',
      '    if Assigned(_UserTerminateEvents[I].Proc) then _UserTerminateEvents[I].Proc();',
      '    if Assigned(_UserTerminateEvents[I].Method) then _UserTerminateEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnPause;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_PauseEvents) do',
      '  begin',
      '    if Assigned(_PauseEvents[I].Proc) then _PauseEvents[I].Proc();',
      '    if Assigned(_PauseEvents[I].Method) then _PauseEvents[I].Method();',
      '  end;',
      'end;',
      '',
      'procedure _CallOnResume;',
      'var I: Integer;',
      'begin',
      '  for I := 0 to High(_ResumeEvents) do',
      '  begin',
      '    if Assigned(_ResumeEvents[I].Proc) then _ResumeEvents[I].Proc();',
      '    if Assigned(_ResumeEvents[I].Method) then _ResumeEvents[I].Method();',
      '  end;',
      'end;'
    ], '!Events');

    addDelayedCode([
      'procedure AddOnTerminate(Proc: procedure); overload;',
      'begin',
      '  _TerminateEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnTerminate(Proc: procedure of object); overload;',
      'begin',
      '  _TerminateEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnUserTerminate(Proc: procedure); overload;',
      'begin',
      '  _UserTerminateEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnUserTerminate(Proc: procedure of object); overload;',
      'begin',
      '  _UserTerminateEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnPause(Proc: procedure); overload;',
      'begin',
      '  _PauseEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnPause(Proc: procedure of object); overload;',
      'begin',
      '  _PauseEvents += [,Proc];',
      'end;',
      '',
      'procedure AddOnResume(Proc: procedure); overload;',
      'begin',
      '  _ResumeEvents += [Proc];',
      'end;',
      '',
      'procedure AddOnResume(Proc: procedure of object); overload;',
      'begin',
      '  _ResumeEvents += [,Proc];',
      'end;'
    ],
    'Misc');

    DumpSection := '';
  end;
end;

end.
