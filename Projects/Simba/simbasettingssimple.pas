unit simbasettingssimple;

{$mode objfpc}{$H+}

{

Settings:
    - General

        -   Updater:
            -   Check for updates
            -   Check every X minutes
            -   URLs

        -   Interpreter
            -   Lape / PS / others

    - Environment

        -   Code Tools:
            -   Automatically show hints
            -   Automatically show completion

        -   Tab options:
            -   Open Next on Close
            -   Open Script in new Tab
            -   Check tabs for open script before opening

        -   Colour Picker:
            -   Show history on pick
            -   Add to history on pick

        -   Source Editor
            -   LazColors (boolean)
            -   Default Script (Path)

        -   Tray:
            -   Always Visible

        -   Function List:
            -   ShowOnStart

        -   Show Command Prompt (Windows only)

    - Advanced

        -   Paths:
            Includes
            Plugins
            Fonts
            Extensions
            Scripts

}

{$I Simba.inc}


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, CheckLst, NewSimbaSettings, types;

type

  { TSettingsSimpleForm }

  TSettingsSimpleForm = class(TForm)
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckForUpdatesBox: TCheckBox;
    HighlightLazColours: TCheckBox;
    CodeToolsCheckBoxes: TCheckGroup;
    TabSettingsCheckBoxes: TCheckGroup;
    ColourPickerCheckGroup: TCheckGroup;
    EnvOther: TCheckGroup;
    UpdateMinutesEdit: TEdit;
    DefaultScriptedit: TEdit;
    UpdaterGroup: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    PgControlEnvironment: TPageControl;
    PgControlAdvanced: TPageControl;
    InterpreterGroup: TRadioGroup;
    SettingsTabsList: TListView;
    PgControlGeneral: TPageControl;
    SettingsTabsPanel: TPanel;
    PathsTreeView: TTreeView;
    tsEditor: TTabSheet;
    tsOther: TTabSheet;
    tsUpdater: TTabSheet;
    tsInterpreter: TTabSheet;
    tsTabs: TTabSheet;
    tsAdvanced: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure EnvOtherItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SettingsTabsListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure SettingsTabsListClick(Sender: TObject);
    procedure SettingsTabsListMouseLeave(Sender: TObject);
    procedure SettingsTabsListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SwitchSettingsTab(NewTab: Integer);
    procedure HighlightSettingsTab(NewTab: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SettingsSimpleForm: TSettingsSimpleForm;
  SettingsTabState: Array of Integer;
  ClickSettingsTab: Boolean;

implementation

{$R *.lfm}

{ TSettingsSimpleForm }

procedure TSettingsSimpleForm.FormCreate(Sender: TObject);
begin
  SetLength(SettingsTabState, SettingsTabsList.Items.Count);
  SettingsTabState[0] := 1;
end;

procedure TSettingsSimpleForm.ButtonOKClick(Sender: TObject);
begin
  // Updater
  SimbaSettings.Updater.CheckForUpdates.Value := CheckForUpdatesBox.Checked;
  SimbaSettings.Updater.CheckEveryXMinutes.Value := StrToIntDef(UpdateMinutesEdit.Text, 30);

  // Interpreter

  // Tabs
  SimbaSettings.Tab.OpenNextOnClose.Value := TabSettingsCheckBoxes.Checked[0];
  SimbaSettings.Tab.OpenScriptInNewTab.Value := TabSettingsCheckBoxes.Checked[1];
  SimbaSettings.Tab.CheckBeforeOpen.Value := TabSettingsCheckBoxes.Checked[2];

  // Code Tools
  SimbaSettings.CodeHints.ShowAutomatically.Value := CodeToolsCheckBoxes.Checked[0];
  SimbaSettings.CodeCompletion.ShowAutomatically.Value := CodeToolsCheckBoxes.Checked[1];

  // Colour Picker
  SimbaSettings.ColourPicker.ShowHistoryOnPick.Value := ColourPickerCheckGroup.Checked[0];
  //SimbaSettings.ColourPicker.AddToHistoryOnPick.SetValue(ColourPickerCheckGroup.Checked[1]);

  // Source Editor
  SimbaSettings.SourceEditor.LazColors.Value := HighlightLazColours.Checked;
  SimbaSettings.SourceEditor.DefScriptPath.Value := DefaultScriptedit.Text;

  // Other
  SimbaSettings.Tray.AlwaysVisible.Value := TabSettingsCheckBoxes.Checked[0];
  SimbaSettings.FunctionList.ShowOnStart.Value := TabSettingsCheckBoxes.Checked[1];
  // Add 'Show Command prompt'

  // Paths

  //SettingsSimpleForm.Destroy;
  SettingsSimpleForm.Close;
end;

{ For live changes }
procedure TSettingsSimpleForm.EnvOtherItemClick(Sender: TObject;
  Index: integer);
begin
  if Index = 0 then
    SimbaSettings.Tray.AlwaysVisible.Value:= EnvOther.Checked[index];
end;

procedure TSettingsSimpleForm.ButtonCancelClick(Sender: TObject);
begin
  SettingsSimpleForm.Close;
end;

procedure TSettingsSimpleForm.FormShow(Sender: TObject);
begin
  // Updater
  CheckForUpdatesBox.Checked := SimbaSettings.Updater.CheckForUpdates.Value;
  UpdateMinutesEdit.Text := IntToStr(SimbaSettings.Updater.CheckEveryXMinutes.Value);

  // Interpreter

  // Tabs
  TabSettingsCheckBoxes.Checked[0] := SimbaSettings.Tab.OpenNextOnClose.Value;
  TabSettingsCheckBoxes.Checked[1] := SimbaSettings.Tab.OpenScriptInNewTab.Value;
  TabSettingsCheckBoxes.Checked[2] := SimbaSettings.Tab.CheckBeforeOpen.Value;

  // Code Tools
  CodeToolsCheckBoxes.Checked[0] := SimbaSettings.CodeHints.ShowAutomatically.Value;
  CodeToolsCheckBoxes.Checked[1] := SimbaSettings.CodeCompletion.ShowAutomatically.Value;

  // Colour Picker
  ColourPickerCheckGroup.Checked[0] := SimbaSettings.ColourPicker.ShowHistoryOnPick.Value;
  //ColourPickerCheckGroup.Checked[1] := SimbaSettings.ColourPicker.AddToHistoryOnPick.GetValue; // Wizzup: This doesn't actually get set?

  // Source Editor
  HighlightLazColours.Checked := SimbaSettings.SourceEditor.LazColors.Value;
  DefaultScriptedit.Text := SimbaSettings.SourceEditor.DefScriptPath.Value;

  // Other
  Writeln('Always Checked: ' + BoolToStr(SimbaSettings.Tray.AlwaysVisible.Value, True));
  EnvOther.Checked[0] := SimbaSettings.Tray.AlwaysVisible.Value;
  Writeln('FOO Always Checked: ' + BoolToStr(EnvOther.Checked[0], True));

  EnvOther.Checked[1] := SimbaSettings.FunctionList.ShowOnStart.Value;


  // Add 'Show Command prompt', not set in SimbaSettings?

  // Paths

end;


// Part of Faux Tabs - Controls switching of tabs
procedure TSettingsSimpleForm.SwitchSettingsTab(NewTab: Integer);
var i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
  begin
    if i = NewTab then
      SettingsTabState[i] := 1
    else
    begin
      SettingsTabState[i] := 0;
    end;
  end;

  PgControlGeneral.Visible := False;
  PgControlEnvironment.Visible := False;
  PgControlAdvanced.Visible := False;

  if NewTab = 0 then
    PgControlGeneral.Visible := True
  else
    if NewTab = 1 then
      PgControlEnvironment.Visible := True
    else
      if NewTab = 2 then
        PgControlAdvanced.Visible := True;

  SettingsTabsList.Refresh;
end;

// Part of Faux Tabs - Controls the highlight state
procedure TSettingsSimpleForm.HighlightSettingsTab(NewTab: Integer);
var i: Integer;
begin
  for i := 0 to High(SettingsTabState) do
  begin
    if SettingsTabState[i] = 1 then
      Continue;
    if i = NewTab then
      SettingsTabState[i] := 2
    else
    begin
      SettingsTabState[i] := 0;
    end;
  end;

  SettingsTabsList.Refresh;
end;



// Part of Faux Tabs - Custom draws tabs
procedure TSettingsSimpleForm.SettingsTabsListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  ItemBoundsRect, IconRect, LabelRect: TRect;
begin

  if stage = cdPostPaint then
  begin

    ItemBoundsRect := item.DisplayRect(drBounds);

    case SettingsTabState[Item.Index] of
        0:  begin
              Sender.Canvas.Brush.Color := clWhite;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
        1:  begin
              Sender.Canvas.Brush.Color := $00EED2C1;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
        2:  begin
              Sender.Canvas.Brush.Color := $00F6E8E0;
              Sender.Canvas.FillRect(ItemBoundsRect);
            end;
    end;

    IconRect := item.DisplayRect(drIcon);
    LabelRect := item.DisplayRect(drlabel);


    ImageList1.Draw(Sender.Canvas, ItemBoundsRect.Left + (((ItemBoundsRect.Right - ItemBoundsRect.Left) div 2) - 16), IconRect.Top+3, Item.ImageIndex);
    Sender.Canvas.TextOut(LabelRect.Left+2, LabelRect.Top, Item.Caption);

  end;

end;

// Part of Faux Tabs - OnClick
procedure TSettingsSimpleForm.SettingsTabsListClick(Sender: TObject);
var
  x, y: Integer;
  f: TListItem;

begin
  x := ScreenToClient(Mouse.CursorPos).x;
  y := ScreenToClient(Mouse.CursorPos).y;

  f := SettingsTabsList.GetItemAt(x, y);

  if f = nil then
    exit;

  SwitchSettingsTab(f.Index);
end;

// Part of Faux Tabs - On mouse leave
procedure TSettingsSimpleForm.SettingsTabsListMouseLeave(Sender: TObject);
var i: integer;

begin
  for i := 0 to High(SettingsTabState) do
  begin
    if SettingsTabState[i] = 1 then
      Continue;
    SettingsTabState[i] := 0;
  end;

  SettingsTabsList.Repaint;
end;

// Part of Faux Tabs - On mouse move
procedure TSettingsSimpleForm.SettingsTabsListMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  f: TListItem;

begin
  f := SettingsTabsList.GetItemAt(x, y);

  if f = nil then
    exit;
  HighlightSettingsTab(f.Index);
  writeln('Highlighting: ' + Inttostr(f.index));
end;





end.

