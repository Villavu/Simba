{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_tabs;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Menus, Buttons,
  simba.ide_tab, simba.ide_editor, simba.ide_editor_findreplace,
  simba.component_tabcontrol, simba.component_button, simba.component_edit;

type
  TSimbaTabsForm = class(TForm)
    OpenDialog: TOpenDialog;
    MenuItemNewTab: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseOtherTabs: TMenuItem;
    FindPanel: TPanel;
    FindEditPanel: TPanel;
    FindButtonPanel: TPanel;
    TabPopupMenu: TPopupMenu;

    procedure DoOnDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DoTabPopupClick(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure FindButtonResize(Sender: TObject);
    procedure FindButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FindEditChange(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TabPopupMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
  protected
    FTabControl: TSimbaTabControl;

    FEditorReplace: TSimbaEditorReplace;
    FEditorFind: TSimbaEditorFind;

    FFindEdit: TSimbaEdit;
    FFindButtonDown: TSimbaButton;
    FFindButtonUp: TSimbaButton;
    FFindButtonCaseSens: TSimbaToggleButton;
    FFindButtonWholeWord: TSimbaToggleButton;
    FFindButtonClose: TSimbaTransparentButton;

    FMouseDown: Boolean;
    FMouseDownX, FMouseDownY: Integer;

    procedure FontChanged(Sender: TObject); override;
    procedure CalculateFindButtonSizes;

    function CanAnchorDocking(X, Y: Integer): Boolean;

    procedure DoFindPanelVisibleChanged(Sender: TObject);
    procedure DoTabCanChange(Sender: TSimbaTabControl; OldTab, NewTab: TSimbaTab; var AllowChange: Boolean);
    procedure DoTabChange(Sender: TSimbaTabControl; NewTab: TSimbaTab);
    procedure DoTabMoved(Sender: TSimbaTabControl; AFrom, ATo: Integer);
    procedure DoTabClosed(Sender: TSimbaTabControl; Tab: TSimbaTab; var CanClose: Boolean);

    function GetTabCount: Integer;
    function GetTab(Index: Integer): TSimbaScriptTab;
    function GetCurrentTab: TSimbaScriptTab;
    function GetCurrentEditor: TSimbaEditor;

    procedure SetCurrentTab(Value: TSimbaScriptTab);
  public
    constructor Create(AOwner: TComponent); override;

    property TabCount: Integer read GetTabCount;
    property Tabs[Index: Integer]: TSimbaScriptTab read GetTab;

    property CurrentTab: TSimbaScriptTab read GetCurrentTab write SetCurrentTab;
    property CurrentEditor: TSimbaEditor read GetCurrentEditor;

    procedure Replace;
    procedure Find;
    procedure FindNext;
    procedure FindPrevious;

    function AddTab: TSimbaScriptTab;

    function CloseTab(Tab: TSimbaScriptTab; KeepOne: Boolean): Boolean;
    function CloseOtherTabs(Tab: TSimbaScriptTab): Boolean;
    function CloseAllTabs: Boolean;

    function Open(FileName: String; CheckOtherTabs: Boolean = True): Boolean; overload;
    procedure Open; overload;

    procedure Save;
    procedure SaveAll;
  end;

var
  SimbaTabsForm: TSimbaTabsForm;

function GetSimbaActiveTab: TSimbaScriptTab;

implementation

{$R *.lfm}

uses
  LCLType,
  simba.base, simba.env, simba.ide_editor_docgenerator,
  simba.form_main, simba.form_output,
  simba.ide_dockinghelpers, simba.nativeinterface,
  simba.ide_events, simba.ide_utils, simba.ide_theme, simba.settings;

function GetSimbaActiveTab: TSimbaScriptTab;
begin
  if Assigned(SimbaTabsForm) then
    Result := SimbaTabsForm.CurrentTab
  else
    Result := nil;
end;

procedure TSimbaTabsForm.DoOnDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  for I := 0 to High(FileNames) do
    Self.Open(FileNames[I], True);
end;

procedure TSimbaTabsForm.DoTabPopupClick(Sender: TObject);
var
  Tab: TSimbaScriptTab;
begin
  if (Sender = MenuItemNewTab) then
    AddTab()
  else
  begin
    with FTabControl.ScreenToClient(TabPopupMenu.PopupPoint) do
      Tab := TSimbaScriptTab(FTabControl.GetTabAt(X, Y));

    if Assigned(Tab) then
    begin
      if (Sender = MenuItemCloseTab)       then CloseTab(Tab, True);
      if (Sender = MenuItemCloseOtherTabs) then CloseOtherTabs(Tab);
    end;
  end;
end;

procedure TSimbaTabsForm.FindButtonClick(Sender: TObject);
begin
  if Sender.Equals(FFindButtonUp) then
    FindPrevious()
  else
  if Sender.Equals(FFindButtonDown) then
    FindNext()
  else
  if Sender.Equals(FFindButtonClose) then
    FindPanel.Hide();
end;

procedure TSimbaTabsForm.FindButtonResize(Sender: TObject);
var
  Button: TBitBtn absolute Sender;
begin
  Button.Width := Button.Height;

  if (Button.Width >= 45) then
    TBitBtn(Sender).ImageWidth := 32
  else
  if (Button.Width >= 35) then
    TBitBtn(Sender).ImageWidth := 24
  else
    TBitBtn(Sender).ImageWidth := 16;
end;

procedure TSimbaTabsForm.FindButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F3:
      begin
        if (ssShift in Shift) then
          FFindButtonUp.Click()
        else
          FFindButtonDown.Click();

        Key := 0;
      end;

    VK_ESCAPE:
      begin
        FindPanel.Hide();
        Key := 0;
      end;

    VK_UP:
      begin
        FFindButtonUp.Click();
        Key := 0;
      end;

    VK_RETURN, VK_DOWN:
      begin
        FFindButtonDown.Click();
        Key := 0;
      end;
  end;
end;

procedure TSimbaTabsForm.FindEditChange(Sender: TObject);
begin
  if (CurrentEditor <> nil) then
    FEditorFind.ExecuteNoDialog(CurrentEditor, FFindEdit.Text, FFindButtonCaseSens.Down, FFindButtonWholeWord.Down);
end;

procedure TSimbaTabsForm.FindPanelResize(Sender: TObject);
begin
  FFindEdit.Width := FindPanel.Width div 3;
end;

procedure TSimbaTabsForm.FormDestroy(Sender: TObject);
begin
  SimbaTabsForm := nil;
end;

procedure TSimbaTabsForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := True;
  FMouseDownX := X;
  FMouseDownY := Y;
end;

procedure TSimbaTabsForm.FormMouseLeave(Sender: TObject);
begin
  FMouseDown := False;
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaTabsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown and CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    if TSimbaAnchorDockHostSite(HostDockSite).Header.Dragging then
      TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y)
    else
      TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(mbLeft, Shift, X, Y);
  end;
end;

procedure TSimbaTabsForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;

  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseUp(Button, Shift, X, Y);
end;

procedure TSimbaTabsForm.TabPopupMenuMeasureItem(Sender: TObject; ACanvas: TCanvas; var AWidth, AHeight: Integer);
begin
  MenuItemHeight(Sender as TMenuItem, ACanvas, AHeight);
end;

procedure TSimbaTabsForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  CalculateFindButtonSizes();
end;

procedure TSimbaTabsForm.CalculateFindButtonSizes;
begin
  if (FFindButtonDown = nil) or (FFindButtonUp = nil) or (FFindButtonCaseSens = nil) or (FFindButtonWholeWord = nil) then
    Exit;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Bold := True;
    Canvas.Font.Size := Round(Abs(GetFontData(Canvas.Font.Handle).Height) * 72 / Canvas.Font.PixelsPerInch) + 2; // Measure on larger font size - Font size can be 0

    with Canvas.TextExtent('AaW') do
    begin
      FFindButtonDown.Width := Width;
      FFindButtonUp.Width := Width;
      FFindButtonCaseSens.Width := Width;
      FFindButtonWholeWord.Width := Width;
    end;
  finally
    Free();
  end;
end;

// Needs special handling so double click to add tabs works
// Forwarding all mouse events to header wont work.
// Only forward events if dragged > 10 distance.
function TSimbaTabsForm.CanAnchorDocking(X, Y: Integer): Boolean;
begin
  Result := FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging) and (Abs(X - FMouseDownX) > 10) and (Abs(Y - FMouseDownY) > 10);
end;

procedure TSimbaTabsForm.DoFindPanelVisibleChanged(Sender: TObject);
begin
  if (not FindPanel.Visible) and (CurrentEditor <> nil) and CurrentEditor.CanSetFocus() then
    CurrentEditor.SetFocus();

  SimbaSettings.Editor.FindPanelVisible.Value := FindPanel.Visible;
end;

procedure TSimbaTabsForm.DoTabCanChange(Sender: TSimbaTabControl; OldTab, NewTab: TSimbaTab; var AllowChange: Boolean);
begin
  if Assigned(OldTab) then
    SimbaIDEEvents.Notify(SimbaIDEEvent.TAB_BEFORECHANGE, OldTab);
end;

procedure TSimbaTabsForm.DoTabChange(Sender: TSimbaTabControl; NewTab: TSimbaTab);
begin
  SimbaIDEEvents.Notify(SimbaIDEEvent.TAB_CHANGE, NewTab);

  if (NewTab is TSimbaScriptTab) and TSimbaScriptTab(NewTab).Editor.CanSetFocus() then
    TSimbaScriptTab(NewTab).Editor.SetFocus();
end;

procedure TSimbaTabsForm.DoTabClosed(Sender: TSimbaTabControl; Tab: TSimbaTab; var CanClose: Boolean);
begin
  CanClose := TSimbaScriptTab(Tab).CanClose();
  if CanClose and (FTabControl.TabCount <= 1) and FTabControl.IsClickingCloseButton then
    AddTab();
end;

procedure TSimbaTabsForm.DoTabMoved(Sender: TSimbaTabControl; AFrom, ATo: Integer);
begin
  SimbaOutputForm.MoveTab(AFrom, ATo);
end;

procedure TSimbaTabsForm.SetCurrentTab(Value: TSimbaScriptTab);
begin
  FTabControl.ActiveTab := Value;
end;

constructor TSimbaTabsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTabControl := TSimbaTabControl.Create(Self, TSimbaScriptTab);
  FTabControl.Parent := Self;
  FTabControl.Align := alClient;
  FTabControl.PopupMenu := TabPopupMenu;
  FTabControl.OnTabMoved := @DoTabMoved;
  FTabControl.OnTabClose := @DoTabClosed;
  FTabControl.OnTabCanChange := @DoTabCanChange;
  FTabControl.OnTabChange := @DoTabChange;
  FTabControl.OnMouseMove := @FormMouseMove;
  FTabControl.OnMouseDown := @FormMouseDown;
  FTabControl.OnMouseUp := @FormMouseUp;
  FTabControl.OnMouseLeave := @FormMouseLeave;
  FTabControl.DefaultTitle := 'Untitled';
  FTabControl.CanAddTabOnDoubleClick := True;

  FEditorReplace := TSimbaEditorReplace.Create(Self);
  FEditorFind := TSimbaEditorFind.Create(Self);

  FindPanel.Color := SimbaTheme.ColorFrame;
  FindPanel.Visible := SimbaSettings.Editor.FindPanelVisible.Value;
  FindPanel.AddHandlerOnVisibleChanged(@DoFindPanelVisibleChanged);

  FFindEdit := TSimbaEdit.Create(Self);
  FFindEdit.Parent := FindEditPanel;
  FFindEdit.Align := alLeft;
  FFindEdit.BorderSpacing.Around := 5;
  FFindEdit.OnChange := @FindEditChange;
  FFindEdit.OnKeyDown := @FindButtonKeyDown;

  FFindButtonWholeWord := TSimbaToggleButton.Create(Self);
  with FFindButtonWholeWord do
  begin
    Parent := FindButtonPanel;
    Caption := 'W';
    Font.Bold := True;
    Align := alLeft;
    Hint := 'Match whole words';
    ShowHint := True;
    BorderSpacing.Around := 5;
    OnClick := @FindEditChange;
  end;

  FFindButtonCaseSens := TSimbaToggleButton.Create(Self);
  with FFindButtonCaseSens do
  begin
    Parent := FindButtonPanel;
    Caption := 'Aa';
    Font.Bold := True;
    Align := alLeft;
    Hint := 'Case sensitive';
    ShowHint := True;
    BorderSpacing.Around := 5;
    OnClick := @FindEditChange;
  end;

  FFindButtonDown := TSimbaButton.Create(Self);
  with FFindButtonDown do
  begin
    Parent := FindButtonPanel;
    Align := alLeft;
    ImageIndex := IMG_ARROW_DOWN;
    Hint := 'Find Next';
    ShowHint := True;
    BorderSpacing.Around := 5;
    OnClick := @FindButtonClick;
  end;

  FFindButtonUp := TSimbaButton.Create(Self);
  with FFindButtonUp do
  begin
    Parent := FindButtonPanel;
    Align := alLeft;
    ImageIndex := IMG_ARROW_UP;
    Hint := 'Find Previous';
    ShowHint := True;
    BorderSpacing.Around := 5;
    OnClick := @FindButtonClick;
  end;

  FFindButtonClose := TSimbaTransparentButton.Create(Self);
  with FFindButtonClose do
  begin
    Parent := FindPanel;
    Align := alRight;
    BorderSpacing.Around := 5;
    Image := ESimbaButtonImage.CLOSE;
    OnClick := @FindButtonClick;
  end;

  CalculateFindButtonSizes();
end;

function TSimbaTabsForm.GetTabCount: Integer;
begin
  Result := FTabControl.TabCount;
end;

function TSimbaTabsForm.GetTab(Index: Integer): TSimbaScriptTab;
begin
  Result := FTabControl.Tabs[Index] as TSimbaScriptTab;
end;

function TSimbaTabsForm.GetCurrentTab: TSimbaScriptTab;
begin
  Result := TSimbaScriptTab(FTabControl.ActiveTab);
end;

function TSimbaTabsForm.GetCurrentEditor: TSimbaEditor;
begin
  if (CurrentTab <> nil) then
    Result := CurrentTab.Editor
  else
    Result := nil;
end;

procedure TSimbaTabsForm.Replace;
begin
  if (CurrentEditor <> nil) then
    FEditorReplace.Execute(CurrentEditor);
end;

procedure TSimbaTabsForm.Find;
begin
  if FFindEdit.Focused() then
  begin
    FindPanel.Hide();
    Exit;
  end;

  FindPanel.Show();

  FFindEdit.SelectAll();
  if FFindEdit.CanSetFocus() then
    FFindEdit.SetFocus();
end;

procedure TSimbaTabsForm.FindNext;
begin
  if (CurrentEditor <> nil) then
    FEditorFind.FindNext(CurrentEditor);
end;

procedure TSimbaTabsForm.FindPrevious;
begin
  if (CurrentEditor <> nil) then
    FEditorFind.FindPrev(CurrentEditor);
end;

function TSimbaTabsForm.AddTab: TSimbaScriptTab;
begin
  Result := FTabControl.AddTab() as TSimbaScriptTab;
end;

function TSimbaTabsForm.CloseTab(Tab: TSimbaScriptTab; KeepOne: Boolean): Boolean;
begin
  Result := FTabControl.DeleteTab(Tab);
  if KeepOne and (FTabControl.TabCount = 0) then
    AddTab();
end;

function TSimbaTabsForm.CloseOtherTabs(Tab: TSimbaScriptTab): Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := TabCount - 1 downto 0 do
    if (Tabs[I] <> Tab) and (not CloseTab(Tabs[I], True)) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSimbaTabsForm.CloseAllTabs: Boolean;
var
  I: Integer;
begin
  Result := True;

  for I := TabCount - 1 downto 0 do
    if not CloseTab(Tabs[I], False) then
    begin
      Result := False;
      Exit;
    end;
end;

function TSimbaTabsForm.Open(FileName: String; CheckOtherTabs: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;

  FileName := ExpandFileName(FileName);

  if CheckOtherTabs then
    for I := 0 to TabCount - 1 do
      if SameFileName(Tabs[I].ScriptFileName, FileName) then
      begin
        CurrentTab := Tabs[I];
        Result := True;
        Exit;
      end;

  if FileExists(FileName) then
  begin
    if (CurrentTab.ScriptFileName <> '') or CurrentTab.ScriptChanged then // Use current tab if default
      CurrentTab := AddTab();

    Result := CurrentTab.Load(FileName);
  end;
end;

procedure TSimbaTabsForm.Open;
var
  I: Integer;
begin
  try
    OpenDialog.InitialDir := ExtractFileDir(CurrentTab.ScriptFileName);
    if (OpenDialog.InitialDir = '') then
      OpenDialog.InitialDir := SimbaEnv.ScriptsPath;

    if OpenDialog.Execute() then
      for I := 0 to OpenDialog.Files.Count - 1 do
        Open(OpenDialog.Files[I], True);
  except
    on E: Exception do
      ShowMessage('Exception while opening file: ' + E.Message);
  end;
end;

procedure TSimbaTabsForm.Save;
begin
  CurrentTab.Save(CurrentTab.ScriptFileName);
end;

procedure TSimbaTabsForm.SaveAll;
var
  I: Integer;
begin
  for I := TabCount - 1 downto 0 do
    if Tabs[I].ScriptChanged then
    begin
      if (Tabs[I].ScriptFileName = '') then
        Tabs[I].Show();

      Tabs[I].Save(Tabs[I].ScriptFileName);
    end;
end;

end.
