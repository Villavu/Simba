{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_maintoolbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls, Graphics,
  simba.mufasatypes, simba.settings, simba.process,
  simba.component_toolbar, simba.component_button;

type
  TSimbaMainToolBar = class(TComponent)
  protected
    FToolBar: TSimbaToolbar;

    FButtonNew: TSimbaButton;
    FButtonOpen: TSimbaButton;
    FButtonOpenDrop: TSimbaButton;
    FButtonSave: TSimbaButton;
    FButtonSaveAll: TSimbaButton;

    FButtonRun: TSimbaButton;
    FButtonCompile: TSimbaButton;
    FButtonStop: TSimbaButton;
    FButtonPause: TSimbaButton;

    FButtonPickColor: TSimbaButton;
    FButtonPickArea: TSimbaButton;
    FButtonPickTarget: TSimbaButton;

    FButtonPackage: TSimbaButton;

    FWindowSelection: TWindowHandle;
    FProcessSelection: TProcessID;

    FAreaSelection: TBox;

    procedure SetStates(ScriptState: ESimbaScriptState);

    procedure DoTabScriptStateChange(Sender: TObject);
    procedure DoTabChange(Sender: TObject);
    procedure DoTabModified(Sender: TObject);

    procedure DoClickFileButton(Sender: TObject);
    procedure DoClickScriptButton(Sender: TObject);
    procedure DoClickClearOutputButton(Sender: TObject);
    procedure DoClickPackageButton(Sender: TObject);

    procedure DoClickColorPicker(Sender: TObject);
    procedure DoClickAreaSelector(Sender: TObject);
    procedure DoClickWindowSelector(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure DoSettingChanged_Spacing(Setting: TSimbaSetting);
    procedure DoSettingChanged_Size(Setting: TSimbaSetting);
    procedure DoSettingChanged_Position(Setting: TSimbaSetting);
  public
    property ButtonNew: TSimbaButton read FButtonNew;
    property ButtonOpen: TSimbaButton read FButtonOpen;
    property ButtonSave: TSimbaButton read FButtonSave;
    property ButtonSaveAll: TSimbaButton read FButtonSaveAll;

    property ButtonRun: TSimbaButton read FButtonRun;
    property ButtonCompile: TSimbaButton read FButtonCompile;
    property ButtonStop: TSimbaButton read FButtonStop;
    property ButtonPause: TSimbaButton read FButtonPause;

    property ButtonColorPicker: TSimbaButton read FButtonPickColor;
    property ButtonPackage: TSimbaButton read FButtonPackage;

    property WindowSelection: TWindowHandle read FWindowSelection;
    property ProcessSelection: TProcessID read FProcessSelection;
    property AreaSelection: TBox read FAreaSelection;

    constructor Create; reintroduce;
  end;

var
  SimbaMainToolBar: TSimbaMainToolBar;

implementation

uses
  Dialogs,
  simba.ide_initialization, simba.ide_events, simba.main,
  simba.scripttab, simba.scripttabsform, simba.outputform, simba.package_form,
  simba.colorpicker, simba.windowselector, simba.areaselector,
  simba.windowhandle;

procedure TSimbaMainToolBar.SetStates(ScriptState: ESimbaScriptState);

  procedure UpdateButtons(RunEnabled, PauseEnabed, CompileEnabled, StopEnabled: Boolean; StopImageIndex: Integer);
  begin
    FButtonRun.Enabled := RunEnabled;
    FButtonPause.Enabled := PauseEnabed;
    FButtonCompile.Enabled := CompileEnabled;

    FButtonStop.Enabled := StopEnabled;
    FButtonStop.ImageIndex := StopImageIndex;
  end;

begin
  case ScriptState of
    ESimbaScriptState.STATE_PAUSED:  UpdateButtons(True,  False, True,  True,  IMG_STOP);
    ESimbaScriptState.STATE_STOP:    UpdateButtons(False, False, False, True,  IMG_POWER);
    ESimbaScriptState.STATE_RUNNING: UpdateButtons(False, True,  False, True,  IMG_STOP);
    ESimbaScriptState.STATE_NONE:    UpdateButtons(True,  False, True,  False, IMG_STOP);
  end;
end;

procedure TSimbaMainToolBar.DoTabScriptStateChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) and TSimbaScriptTab(Sender).IsActiveTab() then
    SetStates(TSimbaScriptTab(Sender).ScriptState);
end;

procedure TSimbaMainToolBar.DoTabChange(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) then
  begin
    FButtonSave.Enabled := TSimbaScriptTab(Sender).ScriptChanged;

    SetStates(TSimbaScriptTab(Sender).ScriptState);
  end;

  FButtonSaveAll.Enabled := SimbaScriptTabsForm.TabCount > 1;
end;

procedure TSimbaMainToolBar.DoTabModified(Sender: TObject);
begin
  if (Sender is TSimbaScriptTab) and TSimbaScriptTab(Sender).IsActiveTab() then
    FButtonSave.Enabled := TSimbaScriptTab(Sender).ScriptChanged;
end;

procedure TSimbaMainToolBar.DoClickFileButton(Sender: TObject);
begin
       if (Sender = FButtonNew)     then SimbaScriptTabsForm.AddTab()
  else if (Sender = FButtonOpen)    then SimbaScriptTabsForm.Open()
  else if (Sender = FButtonSave)    then SimbaScriptTabsForm.Save()
  else if (Sender = FButtonSaveAll) then SimbaScriptTabsForm.SaveAll();
end;

procedure TSimbaMainToolBar.DoClickScriptButton(Sender: TObject);
var
  Tab: TSimbaScriptTab;
begin
  Tab := GetSimbaActiveTab();
  if (Tab = nil) then
    Exit;

  if (Sender = FButtonRun) or (Sender = FButtonCompile) then
  begin
    Tab.OutputBox.MakeVisible();
    if SimbaSettings.OutputBox.ClearOnCompile.Value then
      Tab.OutputBox.Empty();
  end;

       if (Sender = FButtonCompile) then Tab.Compile()
  else if (Sender = FButtonRun)     then Tab.Run(FWindowSelection)
  else if (Sender = FButtonPause)   then Tab.Pause()
  else if (Sender = FButtonStop)    then Tab.Stop();

  if Tab.Editor.CanSetFocus() then
    Tab.Editor.SetFocus();
end;

procedure TSimbaMainToolBar.DoClickClearOutputButton(Sender: TObject);
begin
  SimbaOutputForm.ActiveOutputBox.Empty();
end;

procedure TSimbaMainToolBar.DoClickPackageButton(Sender: TObject);
begin
  SimbaPackageForm.Show();
end;

procedure TSimbaMainToolBar.DoClickColorPicker(Sender: TObject);
var
  X, Y: Integer;
  Color: TColor;
begin
  try
    if ShowColorPicker(FWindowSelection, X, Y, Color) then
    begin
      SimbaDebugLn([EDebugLn.FOCUS], 'Color picked: %d at (%d, %d)'.Format([Color, X, Y]));

      SimbaIDEEvents.Notify(SimbaIDEEvent.COLOR_PICKED, Self);
    end;
  except
    on E: Exception do
      ShowMessage('Exception while picking color: ' + E.Message);
  end;
end;

procedure TSimbaMainToolBar.DoClickAreaSelector(Sender: TObject);
begin
  try
    FAreaSelection := ShowAreaSelector(FWindowSelection);
    with FAreaSelection do
      SimbaDebugLn([EDebugLn.FOCUS], 'Area picked: [%d, %d, %d, %d]'.Format([X1, Y1, X2, Y2]));

    SimbaIDEEvents.Notify(SimbaIDEEvent.AREA_SELECTED, Self);
  except
    on E: Exception do
      ShowMessage('Exception while selecting area: ' + E.Message);
  end;
end;

procedure TSimbaMainToolBar.DoClickWindowSelector(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    FWindowSelection := ShowWindowSelector();
    FProcessSelection := FWindowSelection.GetPID();

    SimbaDebugLn([EDebugLn.FOCUS], [
      'Window Selected: ' + IntToStr(FWindowSelection),
      ' - Dimensions: '   + IntToStr(FWindowSelection.GetBounds().Width - 1) + 'x' + IntToStr(FWindowSelection.GetBounds().Height - 1),
      ' - Title: "'       + FWindowSelection.GetTitle() + '"',
      ' - Class: "'       + FWindowSelection.GetClassName() + '"',
      ' - PID: '          + IntToStr(FWindowSelection.GetPID()) + ' (' + BoolToStr(SimbaProcess.IsProcess64Bit(FWindowSelection.GetPID()), '64', '32') + ' bit)',
      ' - Executable: "'  + SimbaProcess.GetProcessPath(FWindowSelection.GetPID()) + '"'
    ]);

    SimbaIDEEvents.Notify(SimbaIDEEvent.WINDOW_SELECTED, Self);
  except
    on E: Exception do
      ShowMessage('Exception while picking color: ' + E.Message);
  end;
end;

procedure TSimbaMainToolBar.DoSettingChanged_Spacing(Setting: TSimbaSetting);
begin
  FToolBar.Spacing := Setting.Value;
end;

procedure TSimbaMainToolBar.DoSettingChanged_Size(Setting: TSimbaSetting);
begin
  FToolBar.ImageWidth := Setting.Value;
end;

procedure TSimbaMainToolBar.DoSettingChanged_Position(Setting: TSimbaSetting);

  procedure SetPosition(AParent: TWinControl; AAlign: TAlign; AVertical: Boolean);
  begin
    FToolBar.Parent   := AParent;
    FToolBar.Align    := AAlign;
    FToolBar.Vertical := AVertical;
  end;

begin
       if (Setting.Value = 'Top')   then SetPosition(SimbaForm.MainMenuPanel, alClient, False)
  else if (Setting.Value = 'Left')  then SetPosition(SimbaForm, alLeft, True)
  else if (Setting.Value = 'Right') then SetPosition(SimbaForm, alRight, True)
end;

constructor TSimbaMainToolBar.Create;
begin
  inherited Create(nil);

  FToolBar := TSimbaToolbar.Create(Self);
  FToolBar.Parent := SimbaForm.MainMenuPanel;
  FToolBar.Align := alClient;

  FButtonNew := FToolBar.AddButton(IMG_NEW, 'New File (Ctrl + N)', @DoClickFileButton);
  FButtonOpen := FToolBar.AddButton(IMG_OPEN, 'Open File (Ctrl + O)', @DoClickFileButton);
  FButtonOpen.BorderSpacing.Right := 0;
  FButtonOpenDrop := FToolBar.AddDropdownButton('Open Recent File', SimbaForm.RecentFilesPopup);
  FButtonOpenDrop.BorderSpacing.Left := 0;
  FButtonSave := FToolBar.AddButton(IMG_SAVE, 'Save Script (Ctrl + S)', @DoClickFileButton);
  FButtonSaveAll := FToolBar.AddButton(IMG_SAVE_ALL, 'Save All', @DoClickFileButton);

  FToolBar.AddDivider();

  FButtonCompile := FToolBar.AddButton(IMG_COMPILE, 'Compile Script (Alt + C)', @DoClickScriptButton);

  FToolBar.AddDivider();

  FButtonRun   := FToolBar.AddButton(IMG_PLAY,'Run Script (Alt + R)', @DoClickScriptButton);
  FButtonPause := FToolBar.AddButton(IMG_PAUSE, 'Pause Script', @DoClickScriptButton);
  FButtonStop  := FToolBar.AddButton(IMG_STOP, 'Stop Script (Alt + S)', @DoClickScriptButton);

  FToolBar.AddDivider();
  FButtonPickColor := FToolBar.AddButton(IMG_PICK, 'Color Picker', @DoClickColorPicker);
  FButtonPickTarget := FToolBar.AddButton(IMG_TARGET, 'Target Selector');
  FButtonPickTarget.OnMouseDown := @DoClickWindowSelector;
  FButtonPickArea := FToolBar.AddButton(IMG_AREA, 'Area Selector', @DoClickAreaSelector);
  FToolBar.AddDivider();
  FToolBar.AddButton(IMG_ERASER, 'Clear Output Box', @DoClickClearOutputButton);
  FToolBar.AddDivider();
  FButtonPackage := FToolBar.AddButton(IMG_PACKAGE, 'Open Packages', @DoClickPackageButton);

  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_SCRIPTSTATE_CHANGE, @DoTabScriptStateChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_CHANGE, @DoTabChange);
  SimbaIDEEvents.Register(Self, SimbaIDEEvent.TAB_MODIFIED, @DoTabModified);

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ToolbarSize, @DoSettingChanged_Size, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ToolbarPosition, @DoSettingChanged_Position, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ToolBarSpacing, @DoSettingChanged_Spacing, True);
end;

procedure CreateMainToolBar;
begin
  SimbaMainToolBar := TSimbaMainToolBar.Create();
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@CreateMainToolBar, 'Create Main ToolBar');

finalization
  if Assigned(SimbaMainToolBar) then
    FreeAndNil(SimbaMainToolBar);

end.

