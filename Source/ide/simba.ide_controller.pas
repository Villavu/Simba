{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  -------------------------------------------------------------------------
  Common functions used throughout the IDE to prevent the need for constantly
  referencing different files.
}
unit simba.ide_controller;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls,
  simba.base,
  simba.ide_tab,
  simba.ide_output_components,
  simba.ide_codetools_parser;

type
  SimbaController = class
  public
    class procedure GetTabContents(out Names, Contents: TStringArray);
    class function FindTab(TabID: Integer): TSimbaScriptTab; static;
    class procedure NewTab(Script: String); static;
    class procedure OpenTab(Tab: TSimbaScriptTab; CaretX, CaretY: Integer); static;
    class procedure OpenInTab(FileName: String); overload; static;
    class procedure OpenInTab(FileName: String; CaretX, CaretY: Integer); overload; static;
    class procedure OpenInTabAndRun(FileName: String); static;
    class procedure OpenInExplorer(FileName: String); static;
    class procedure OpenInBrowser(URL: String); static;
    class function CloseAllTabs: Boolean; static;
    class function FindOutputListForTab(TabID: Integer): TOutputListComponentReal;
    class procedure OpenSettings(Page: String);
    class function GetScriptButtonStates(Tab: TSimbaScriptTab; out CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean): Boolean; static; overload;
    class function GetScriptButtonStates(out CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean): Boolean; static; overload;
    class function GetEditorButtonStates(Tab: TSimbaScriptTab; out CanSave, CanCut, CanCopy, CanPaste: Boolean): Boolean; static; overload;
    class function GetEditorButtonStates(out CanSave, CanCut, CanCopy, CanPaste: Boolean): Boolean; static; overload;

    class procedure ShowTrayNotifaction(Title, Message: String; Timeout: Integer); static;
    class procedure SetWindowTitle(Title: String); static;
    class procedure ShowDecl(Decl: TDeclaration); static;
  end;

implementation

uses
  simba.nativeinterface,
  simba.ide_maintoolbar,
  simba.form_main,
  simba.form_scripttabs,
  simba.form_output,
  simba.form_settings;

{$DEFINE ASSERT_MAIN_THREAD :=
  if (GetCurrentThreadId() <> MainThreadID) then
    raise Exception.Create('Needs to be on main thread: ' + {$I %CURRENTROUTINE%});
}

class procedure SimbaController.GetTabContents(out Names, Contents: TStringArray);
var
  I: Integer;
begin
  ASSERT_MAIN_THREAD

  SetLength(Names, SimbaScriptTabsForm.TabCount);
  SetLength(Contents, SimbaScriptTabsForm.TabCount);

  for I := 0 to SimbaScriptTabsForm.TabCount - 1 do
  begin
    Names[I] := SimbaScriptTabsForm.Tabs[I].ScriptTitle;
    if (Names[I] = '') then
      Names[I] := 'Untitled';
    Contents[I] := SimbaScriptTabsForm.Tabs[I].Script;
  end;
end;

class function SimbaController.FindTab(TabID: Integer): TSimbaScriptTab;
begin
  ASSERT_MAIN_THREAD

  Result := SimbaScriptTabsForm.FindTab(TabID);
end;

class procedure SimbaController.NewTab(Script: String);
begin
  ASSERT_MAIN_THREAD

  SimbaScriptTabsForm.AddTab().Script := Script;
end;

class procedure SimbaController.OpenTab(Tab: TSimbaScriptTab; CaretX, CaretY: Integer);
begin
  ASSERT_MAIN_THREAD

  if not SimbaScriptTabsForm.HasTab(Tab) then
    Exit;
  SimbaScriptTabsForm.ActiveTab := Tab;

  Tab.Editor.CaretX  := CaretX;
  Tab.Editor.CaretY  := CaretY;
  Tab.Editor.TopLine := CaretY - (Tab.Editor.LinesInWindow div 2);
end;

class procedure SimbaController.OpenInTab(FileName: String);
begin
  ASSERT_MAIN_THREAD

  SimbaScriptTabsForm.Open(FileName);
end;

class procedure SimbaController.OpenInTab(FileName: String; CaretX, CaretY: Integer);
begin
  ASSERT_MAIN_THREAD

  if SimbaScriptTabsForm.Open(FileName) then
    with SimbaScriptTabsForm.ActiveTab do
    begin
      Editor.CaretX  := CaretX;
      Editor.CaretY  := CaretY;
      Editor.TopLine := CaretY - (Editor.LinesInWindow div 2);
    end;
end;

class procedure SimbaController.OpenInTabAndRun(FileName: String);
begin
  ASSERT_MAIN_THREAD

  if SimbaScriptTabsForm.Open(FileName) then
    SimbaScriptTabsForm.ActiveTab.Run();
end;

class procedure SimbaController.OpenInExplorer(FileName: String);
begin
  ASSERT_MAIN_THREAD

  if DirectoryExists(FileName) then
    SimbaNativeInterface.OpenDirectory(FileName)
  else if FileExists(FileName) then
    SimbaNativeInterface.OpenFile(FileName);
end;

class procedure SimbaController.OpenInBrowser(URL: String);
begin
  ASSERT_MAIN_THREAD

  SimbaNativeInterface.OpenURL(URL);
end;

class function SimbaController.CloseAllTabs: Boolean;
begin
  ASSERT_MAIN_THREAD

  Result := SimbaScriptTabsForm.CloseAllTabs(False);
end;

class function SimbaController.FindOutputListForTab(TabID: Integer): TOutputListComponentReal;
begin
  ASSERT_MAIN_THREAD

  Result := SimbaOutputForm.FindList(TabID);
end;

class procedure SimbaController.OpenSettings(Page: String);
begin
  ASSERT_MAIN_THREAD

  SimbaSettingsForm.Open(Page);
end;

class function SimbaController.GetScriptButtonStates(Tab: TSimbaScriptTab; out CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean): Boolean;
var
  State: ESimbaScriptState;
begin
  ASSERT_MAIN_THREAD

  if (Tab <> nil) then
  begin
    State := Tab.RunningState;

    CanRun       := (State = ESimbaScriptState.PAUSED) or (State = ESimbaScriptState.NONE);
    CanPause     := (State = ESimbaScriptState.RUNNING);
    CanCompile   := (State = ESimbaScriptState.PAUSED) or (State = ESimbaScriptState.NONE);
    CanStop      := (State <> ESimbaScriptState.NONE);
    CanForceStop := (State = ESimbaScriptState.STOP);

    Result := True;
  end else
    Result := False;
end;

class function SimbaController.GetScriptButtonStates(out CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean): Boolean;
begin
  ASSERT_MAIN_THREAD

  Result := (SimbaScriptTabsForm <> nil) and GetScriptButtonStates(SimbaScriptTabsForm.ActiveTab, CanRun, CanPause, CanCompile, CanStop, CanForceStop);
end;

class function SimbaController.GetEditorButtonStates(Tab: TSimbaScriptTab; out CanSave, CanCut, CanCopy, CanPaste: Boolean): Boolean;
begin
  ASSERT_MAIN_THREAD

  if (Tab <> nil) then
  begin
    CanSave  := Tab.CanSave();
    CanCut   := Tab.Editor.SelAvail;
    CanCopy  := Tab.Editor.SelAvail;
    CanPaste := Tab.Editor.CanPaste;

    Result := True;
  end else
    Result := False;
end;

class function SimbaController.GetEditorButtonStates(out CanSave, CanCut, CanCopy, CanPaste: Boolean): Boolean;
begin
  ASSERT_MAIN_THREAD

  Result := (SimbaScriptTabsForm <> nil) and GetEditorButtonStates(SimbaScriptTabsForm.ActiveTab, CanSave, CanCut, CanCopy, CanPaste);
end;

class procedure SimbaController.ShowTrayNotifaction(Title, Message: String; Timeout: Integer);
begin
  ASSERT_MAIN_THREAD

  SimbaMainForm.TrayIcon.BalloonTitle   := Title;
  SimbaMainForm.TrayIcon.BalloonHint    := Message;
  SimbaMainForm.TrayIcon.BalloonTimeout := Timeout;
  SimbaMainForm.TrayIcon.ShowBalloonHint();
end;

class procedure SimbaController.SetWindowTitle(Title: String);
begin
  ASSERT_MAIN_THREAD

  SimbaMainForm.Caption := Title;
end;

class procedure SimbaController.ShowDecl(Decl: TDeclaration);
begin
  if (Decl = nil) then
    Exit;

  ASSERT_MAIN_THREAD

  case Decl.Parser.SourceType of
    EParserSourceType.SIMBA:
      begin
        DebugLn('Declared internally in Simba: %s', [Decl.DocPos.FileName]);
        DebugLn('Declaration: %s', [Decl.Header]);
        DebugLn(DEBUG_FOCUS);
      end;

    EParserSourceType.PLUGIN:
      begin
        DebugLn('Declared internally in plugin: %s', [Decl.DocPos.FileName]);
        DebugLn('Declaration: %s', [Decl.Header]);
        DebugLn(DEBUG_FOCUS);
      end;

    EParserSourceType.SCRIPT,
    EParserSourceType.INCLUDE:
      begin
        if FileExists(Decl.DocPos.FileName) then
          SimbaScriptTabsForm.Open(Decl.DocPos.FileName);

        with SimbaScriptTabsForm.ActiveTab.Editor do
        begin
          SelStart := Decl.StartPos;
          SelEnd := Decl.EndPos;
          TopLine := (Decl.DocPos.Line + 1) - (LinesInWindow div 2);
          if CanSetFocus() then
            SetFocus();
        end;
      end;
  end;
end;

end.

