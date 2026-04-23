{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_controller;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.ide_tab;

type
  SimbaController = class
  public
    class procedure GetTabContents(out Names, Contents: TStringArray);
    class function FindTab(TabID: Integer): TSimbaScriptTab; static;
    class procedure NewTab(Script: String); static;
    class procedure OpenTab(Tab: TSimbaScriptTab; CaretX, CaretY: Integer); static;
    class procedure OpenInTab(FileName: String); overload; static;
    class procedure OpenInTab(FileName: String; CaretX, CaretY: Integer); overload; static;
    class procedure OpenInExplorer(FileName: String); static;
    class function CloseAllTabs: Boolean;
  end;

implementation

uses
  simba.nativeinterface,
  simba.form_scripttabs;

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

class procedure SimbaController.OpenInExplorer(FileName: String);
begin
  ASSERT_MAIN_THREAD

  if DirectoryExists(FileName) then
    SimbaNativeInterface.OpenDirectory(FileName)
  else if FileExists(FileName) then
    SimbaNativeInterface.OpenFile(FileName);
end;

class function SimbaController.CloseAllTabs: Boolean;
begin
  ASSERT_MAIN_THREAD

  Result := SimbaScriptTabsForm.CloseAllTabs(False);
end;

end.

