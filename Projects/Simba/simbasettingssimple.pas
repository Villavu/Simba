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
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckGroup1: TCheckGroup;
    CheckGroup2: TCheckGroup;
    CheckGroup5: TCheckGroup;
    CheckGroup6: TCheckGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    PgControlEnvironment: TPageControl;
    PgControlAdvanced: TPageControl;
    RadioGroup1: TRadioGroup;
    SettingsTabsList: TListView;
    PgControlGeneral: TPageControl;
    SettingsTabsPanel: TPanel;
    TreeView1: TTreeView;
    tsEditor: TTabSheet;
    tsOther: TTabSheet;
    tsUpdater: TTabSheet;
    tsInterpreter: TTabSheet;
    tsTabs: TTabSheet;
    tsAdvanced: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
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
  SimbaSettings.Updater.CheckForUpdates.SetValue(CheckBox1.Checked);
  SimbaSettings.Updater.CheckEveryXMinutes.SetValue(StrToInt(Edit1.Text));

  // Interpreter

  // Tabs
  SimbaSettings.Tab.OpenNextOnClose.SetValue(CheckGroup2.Checked[0]);
  SimbaSettings.Tab.OpenScriptInNewTab.SetValue(CheckGroup2.Checked[1]);
  SimbaSettings.Tab.CheckBeforeOpen.SetValue(CheckGroup2.Checked[2]);

  // Code Tools
  SimbaSettings.CodeHints.ShowAutomatically.SetValue(CheckGroup1.Checked[0]);
  SimbaSettings.CodeCompletion.ShowAutomatically.SetValue(CheckGroup1.Checked[1]);

  // Colour Picker
  SimbaSettings.ColourPicker.ShowHistoryOnPick.SetValue(CheckGroup5.Checked[0]);
  //SimbaSettings.ColourPicker.AddToHistoryOnPick.SetValue(CheckGroup5.Checked[1]);

  // Source Editor
  SimbaSettings.SourceEditor.LazColors.SetValue(CheckBox2.Checked);
  SimbaSettings.SourceEditor.DefScriptPath.SetValue(Edit2.Text);

  // Other
  SimbaSettings.Tray.AlwaysVisible.SetValue(CheckGroup2.Checked[0]);
  SimbaSettings.FunctionList.ShowOnStart.SetValue(CheckGroup2.Checked[1]);
  // Add 'Show Command prompt'

  // Paths

  //SettingsSimpleForm.Destroy;
  SettingsSimpleForm.Close;
end;

procedure TSettingsSimpleForm.ButtonCancelClick(Sender: TObject);
begin
  SettingsSimpleForm.Close;
end;

procedure TSettingsSimpleForm.FormShow(Sender: TObject);
begin
  // Updater
  CheckBox1.Checked := SimbaSettings.Updater.CheckForUpdates.GetValue;
  Edit1.Text := IntToStr(SimbaSettings.Updater.CheckEveryXMinutes.GetValue);

  // Interpreter

  // Tabs
  CheckGroup2.Checked[0] := SimbaSettings.Tab.OpenNextOnClose.GetValue;
  CheckGroup2.Checked[1] := SimbaSettings.Tab.OpenScriptInNewTab.GetValue;
  CheckGroup2.Checked[2] := SimbaSettings.Tab.CheckBeforeOpen.GetValue;

  // Code Tools
  CheckGroup1.Checked[0] := SimbaSettings.CodeHints.ShowAutomatically.GetValue;
  CheckGroup1.Checked[1] := SimbaSettings.CodeCompletion.ShowAutomatically.GetValue;

  // Colour Picker
  CheckGroup5.Checked[0] := SimbaSettings.ColourPicker.ShowHistoryOnPick.GetValue;
  //CheckGroup5.Checked[1] := SimbaSettings.ColourPicker.AddToHistoryOnPick.GetValue; // Wizzup: This doesn't actually get set?

  // Source Editor
  CheckBox2.Checked := SimbaSettings.SourceEditor.LazColors.GetValue;
  Edit2.Text := SimbaSettings.SourceEditor.DefScriptPath.GetValue;

  // Other
  CheckGroup2.Checked[0] := SimbaSettings.Tray.AlwaysVisible.GetValue;
  CheckGroup2.Checked[1] := SimbaSettings.FunctionList.ShowOnStart.GetValue;
  // Add 'Show Command prompt', not set in SimbaSettings?

  // Paths

end;

procedure TSettingsSimpleForm.RadioGroup1Click(Sender: TObject);
begin

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
  x, y, i: Integer;
  tmpRect: TRect;
begin
  x := ScreenToClient(Mouse.CursorPos).x;
  y := ScreenToClient(Mouse.CursorPos).y;
  for i := 0 to (SettingsTabsList.Items.Count - 1) do
    begin
      tmpRect := SettingsTabsList.Items.Item[i].DisplayRect(drBounds);
      if (tmpRect.Left <= x) and (x <= tmpRect.Right) and (tmpRect.Top <= y) and (y <= tmpRect.Bottom) then
      begin
        SwitchSettingsTab(i);


        Break;
      end;

    end;
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
  tmpRect: TRect;
  i: Integer;
begin
  for i := 0 to (SettingsTabsList.Items.Count - 1) do
    begin
      tmpRect := SettingsTabsList.Items.Item[i].DisplayRect(drBounds);
      if (tmpRect.Left <= x) and (x <= tmpRect.Right) and (tmpRect.Top <= y) and (y <= tmpRect.Bottom) then
      begin
        HighlightSettingsTab(i);
        Break;
      end;
    end;
end;





end.

