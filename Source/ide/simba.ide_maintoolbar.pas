{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_maintoolbar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, ExtCtrls, Graphics, Menus,
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

    FRecentFilesPopup: TPopupMenu;

    // Add button, storing the event in the buttons .tag
    function AddButton(
        Image: Integer; Text: String;
        Event: ESimbaEvent; EventProducer: EEventProducer = epMouseClick
      ): TSimbaButton;

    procedure DoOpenRecentFileClick(Sender: TObject);
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
  LazFileUtils,
  simba.initializations,
  simba.component_images,
  simba.form_main,
  simba.ide_tab,
  simba.ide_controller;

function TSimbaMainToolBar.AddButton(Image: Integer; Text: String; Event: ESimbaEvent; EventProducer: EEventProducer): TSimbaButton;
begin
  Result := FToolBar.AddButton(Image, Text);
  case EventProducer of
    epMouseClick: Result.OnClick     := @DoButtonClick;
    epMouseDown:  Result.OnMouseDown := @DoButtonMouseDown;
  end;
  Result.Tag := Int32(Event);
end;

procedure TSimbaMainToolBar.DoOpenRecentFileClick(Sender: TObject);
begin
  SimbaController.OpenInTab(TMenuItem(Sender).Hint);
end;

procedure TSimbaMainToolBar.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoCanSave(Tab: TSimbaScriptTab);
  begin
    FButtonSave.Enabled := True;
  end;

  procedure DoCannotSave(Tab: TSimbaScriptTab);
  begin
    FButtonSave.Enabled := False;
  end;

  procedure DoTabScriptStateChange(Tab: TSimbaScriptTab);
  var
    CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean;
  begin
    if Tab.IsActiveTab and SimbaController.GetScriptButtonStates(Tab, CanRun, CanPause, CanCompile, CanStop, CanForceStop) then
    begin
      FButtonRun.Enabled := CanRun;
      FButtonPause.Enabled := CanPause;
      FButtonCompile.Enabled := CanCompile;
      FButtonStop.Enabled := CanStop;
      FButtonStop.ImageIndex := IfThen(CanForceStop, SimbaImages.POWER, SimbaImages.STOP);
    end;
  end;

  procedure DoTabChange(Tab: TSimbaScriptTab);
  var
    CanRun, CanPause, CanCompile, CanStop, CanForceStop: Boolean;
    CanSave, CanCut, CanCopy, CanPaste: Boolean;
  begin
    if SimbaController.GetScriptButtonStates(Tab, CanRun, CanPause, CanCompile, CanStop, CanForceStop) and
       SimbaController.GetEditorButtonStates(Tab, CanSave, CanCut, CanCopy, CanPaste) then
    begin
      FButtonRun.Enabled := CanRun;
      FButtonPause.Enabled := CanPause;
      FButtonCompile.Enabled := CanCompile;
      FButtonStop.Enabled := CanStop;
      FButtonStop.ImageIndex := IfThen(CanForceStop, SimbaImages.POWER, SimbaImages.STOP);
      FButtonSave.Enabled := CanSave;
    end;
  end;

  procedure DoTabLoaded(Tab: TSimbaScriptTab);
  var
    I: Integer;
    Item: TMenuItem;
    Files: TStringList;
  begin
    Files := TStringList.Create();
    Files.Text := SimbaSettings.General.RecentFiles.Value;
    if (Files.IndexOf(Tab.ScriptFileName) > -1) then
      Files.Delete(Files.IndexOf(Tab.ScriptFileName));
    Files.Insert(0, Tab.ScriptFileName);
    while (Files.Count > 8) do
      Files.Delete(Files.Count - 1);

    FRecentFilesPopup.Items.Clear();
    for I := 0 to Files.Count - 1 do
    begin
      Item := TMenuItem.Create(FRecentFilesPopup);
      Item.Caption := ShortDisplayFilename(Files[I]);
      Item.Hint := Files[I];
      Item.OnClick := @DoOpenRecentFileClick;

      FRecentFilesPopup.Items.Add(Item);
    end;
    SimbaSettings.General.RecentFiles.Value := Files.Text;

    Files.Free();
  end;

begin
  case Event of
    ESimbaEvent.TAB_CAN_SAVE:           DoCanSave(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_CANNOT_SAVE:        DoCannotSave(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_SCRIPTSTATE_CHANGE: DoTabScriptStateChange(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_CHANGE:             DoTabChange(TSimbaScriptTab(Data));
    ESimbaEvent.TAB_LOADED:             DoTabLoaded(TSimbaScriptTab(Data));
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

  FRecentFilesPopup := TPopupMenu.Create(Self);

  FButtonNew := AddButton(SimbaImages.NEW, 'New File (Ctrl + N)', ESimbaEvent.ACTION_NEW);

  FButtonOpen := AddButton(SimbaImages.FOLDER, 'Open File (Ctrl + O)', ESimbaEvent.ACTION_OPEN);
  FButtonOpen.Parent := FToolbar.AddGroup();
  FButtonOpen.Align := alClient;
  FButtonOpen.BorderSpacing.Right := 0;

  FButtonOpenDrop := FToolBar.AddDropdownButton('Open Recent File', FRecentFilesPopup);
  FButtonOpenDrop.Parent := FButtonOpen.Parent;
  FButtonOpenDrop.Align := alRight;
  FButtonOpenDrop.BorderSpacing.Left := 0;

  FButtonSave := AddButton(SimbaImages.SAVE, 'Save Script (Ctrl + S)', ESimbaEvent.ACTION_SAVE);
  FButtonSaveAll := AddButton(SimbaImages.SAVE_ALL, 'Save All', ESimbaEvent.ACTION_SAVE_ALL);

  FToolBar.AddDivider();

  FButtonCompile := AddButton(SimbaImages.COMPILE, 'Compile Script (Alt + C)', ESimbaEvent.ACTION_COMPILE);

  FToolBar.AddDivider();

  FButtonRun   := AddButton(SimbaImages.PLAY,  'Run Script (Alt + R)', ESimbaEvent.ACTION_RUN);
  FButtonPause := AddButton(SimbaImages.PAUSE, 'Pause Script', ESimbaEvent.ACTION_PAUSE);
  FButtonStop  := AddButton(SimbaImages.STOP, 'Stop Script (Alt + S)', ESimbaEvent.ACTION_STOP);

  FToolBar.AddDivider();
  FButtonPickColor := AddButton(SimbaImages.COLOR_PICKER, 'Color Picker', ESimbaEvent.ACTION_PICKCOLOR);
  FButtonPickTarget := AddButton(SimbaImages.TARGET_SELECTOR, 'Target Selector', ESimbaEvent.ACTION_PICKTARGET, epMouseDown);
  FButtonPickArea := AddButton(SimbaImages.AREA_SELECTOR, 'Area Selector', ESimbaEvent.ACTION_PICKAREA);
  FToolBar.AddDivider();
  AddButton(SimbaImages.ERASER, 'Clear Output Box', ESimbaEvent.ACTION_CLEAROUTPUT);
  FToolBar.AddDivider();
  FButtonPackage := AddButton(SimbaImages.PACKAGE, 'Open Packages', ESimbaEvent.ACTION_PACKAGES);

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
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaMainToolBar', 5); // needs some priority (before script tabs)
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaMainToolBar', 5);

end.


