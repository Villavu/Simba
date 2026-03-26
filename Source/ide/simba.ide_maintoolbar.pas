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
  simba.base,
  simba.settings,
  simba.ide_events,
  simba.component_toolbar,
  simba.component_button;

type
  TSimbaMainToolBar = class(TComponent)
  private type
    EEventProducer = (epMouseClick, epMouseDown);
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

    // Add button, storing the event in the buttons .tag
    function AddButton(
        Image: Integer; Text: String;
        Event: ESimbaEvent; EventProducer: EEventProducer = epMouseClick
      ): TSimbaButton;

    procedure SetStates(ScriptState: ESimbaScriptState);

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoButtonClick(Sender: TObject);
    procedure DoButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

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

    constructor Create; reintroduce;
  end;

var
  SimbaMainToolBar: TSimbaMainToolBar;

implementation

uses
  simba.initializations,
  simba.ide_tab,
  simba.form_tabs,
  simba.form_main;

function TSimbaMainToolBar.AddButton(Image: Integer; Text: String; Event: ESimbaEvent; EventProducer: EEventProducer): TSimbaButton;
begin
  Result := FToolBar.AddButton(Image, Text);
  case EventProducer of
    epMouseClick: Result.OnClick     := @DoButtonClick;
    epMouseDown:  Result.OnMouseDown := @DoButtonMouseDown;
  end;
  Result.Tag := Int32(Event);
end;

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

procedure TSimbaMainToolBar.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of

    ESimbaEvent.TAB_SCRIPTSTATE_CHANGE:
      begin
        if TSimbaScriptTab(Data).IsActiveTab then
          SetStates(TSimbaScriptTab(Data).ScriptState);
      end;

    ESimbaEvent.TAB_CHANGE:
      begin
        FButtonSave.Enabled := TSimbaScriptTab(Data).ScriptChanged;
        FButtonSaveAll.Enabled := SimbaTabsForm.TabCount > 1;
        SetStates(TSimbaScriptTab(Data).ScriptState);
      end;

    ESimbaEvent.TAB_MODIFIED:
      begin
        if TSimbaScriptTab(Data).IsActiveTab then
          FButtonSave.Enabled := TSimbaScriptTab(Data).ScriptChanged;
      end;
  end;
end;

procedure TSimbaMainToolBar.DoButtonClick(Sender: TObject);
begin
  SimbaEvents.Post(ESimbaEvent(TComponent(Sender).Tag), Sender);
end;

procedure TSimbaMainToolBar.DoButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SimbaEvents.Post(ESimbaEvent(TComponent(Sender).Tag), Sender);
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
       if (Setting.Value = 'Top')   then SetPosition(SimbaMainForm.MainMenuPanel, alClient, False)
  else if (Setting.Value = 'Left')  then SetPosition(SimbaMainForm, alLeft, True)
  else if (Setting.Value = 'Right') then SetPosition(SimbaMainForm, alRight, True)
end;

constructor TSimbaMainToolBar.Create;
begin
  inherited Create(nil);

  FToolBar := TSimbaToolbar.Create(Self);
  FToolBar.Parent := SimbaMainForm.MainMenuPanel;
  FToolBar.Align := alClient;

  FButtonNew := AddButton(IMG_NEW, 'New File (Ctrl + N)', ESimbaEvent.TOOLBAR_NEW);

  FButtonOpen := AddButton(IMG_OPEN, 'Open File (Ctrl + O)', ESimbaEvent.TOOLBAR_OPEN);
  FButtonOpen.Parent := FToolbar.AddGroup();
  FButtonOpen.Align := alClient;
  FButtonOpen.BorderSpacing.Right := 0;

  FButtonOpenDrop := FToolBar.AddDropdownButton('Open Recent File', SimbaMainForm.RecentFilesPopup);
  FButtonOpenDrop.Parent := FButtonOpen.Parent;
  FButtonOpenDrop.Align := alRight;
  FButtonOpenDrop.BorderSpacing.Left := 0;

  FButtonSave := AddButton(IMG_SAVE, 'Save Script (Ctrl + S)', ESimbaEvent.TOOLBAR_SAVE);
  FButtonSaveAll := AddButton(IMG_SAVE_ALL, 'Save All', ESimbaEvent.TOOLBAR_SAVEALL);

  FToolBar.AddDivider();

  FButtonCompile := AddButton(IMG_COMPILE, 'Compile Script (Alt + C)', ESimbaEvent.TOOLBAR_COMPILE);

  FToolBar.AddDivider();

  FButtonRun   := AddButton(IMG_PLAY,'Run Script (Alt + R)', ESimbaEvent.TOOLBAR_RUN);
  FButtonPause := AddButton(IMG_PAUSE, 'Pause Script', ESimbaEvent.TOOLBAR_PAUSE);
  FButtonStop  := AddButton(IMG_STOP, 'Stop Script (Alt + S)', ESimbaEvent.TOOLBAR_STOP);

  FToolBar.AddDivider();
  FButtonPickColor := AddButton(IMG_PICK, 'Color Picker', ESimbaEvent.TOOLBAR_PICKCOLOR);
  FButtonPickTarget := AddButton(IMG_TARGET, 'Target Selector', ESimbaEvent.TOOLBAR_PICKTARGET, epMouseDown);
  FButtonPickArea := AddButton(IMG_AREA, 'Area Selector', ESimbaEvent.TOOLBAR_PICKAREA);
  FToolBar.AddDivider();
  AddButton(IMG_ERASER, 'Clear Output Box', ESimbaEvent.TOOLBAR_CLEAROUTPUT);
  FToolBar.AddDivider();
  FButtonPackage := AddButton(IMG_PACKAGE, 'Open Packages', ESimbaEvent.TOOLBAR_PACKAGE);

  SimbaEvents.Register(Self, @DoSimbaEvent);

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ToolbarSize, @DoSettingChanged_Size, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ToolbarPosition, @DoSettingChanged_Position, True);
  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.General.ToolBarSpacing, @DoSettingChanged_Spacing, True);
end;

procedure DoCreate;
begin
  SimbaMainToolBar := TSimbaMainToolBar.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaMainToolBar);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaMainToolBar');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaMainToolBar');

end.


