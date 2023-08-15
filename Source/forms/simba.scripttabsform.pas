{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scripttabsform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Menus, Buttons,
  simba.scripttab, simba.editor, simba.editor_findreplace,
  simba.component_tabcontrol, simba.component_button, simba.component_edit;

type
  TSimbaScriptTabsForm = class(TForm)
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
  end;

var
  SimbaScriptTabsForm: TSimbaScriptTabsForm;

implementation

{$R *.lfm}

uses
  LCLType,
  simba.mufasatypes, simba.env, simba.editor_docgenerator, simba.main,
  simba.dockinghelpers, simba.nativeinterface, simba.outputform,
  simba.ide_events, simba.theme, simba.settings;

procedure TSimbaScriptTabsForm.DoOnDropFiles(Sender: TObject; const FileNames: array of String);
var
  I: Integer;
begin
  for I := 0 to High(FileNames) do
    Self.Open(FileNames[I], True);
end;

procedure TSimbaScriptTabsForm.DoTabPopupClick(Sender: TObject);
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

procedure TSimbaScriptTabsForm.FindButtonClick(Sender: TObject);
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

procedure TSimbaScriptTabsForm.FindButtonResize(Sender: TObject);
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

procedure TSimbaScriptTabsForm.FindButtonKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
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

procedure TSimbaScriptTabsForm.FindEditChange(Sender: TObject);
begin
  if (CurrentEditor <> nil) then
    FEditorFind.ExecuteNoDialog(CurrentEditor, FFindEdit.Text, FFindButtonCaseSens.Down, FFindButtonWholeWord.Down);
end;

procedure TSimbaScriptTabsForm.FindPanelResize(Sender: TObject);
begin
  FFindEdit.Width := FindPanel.Width div 3;
end;

procedure TSimbaScriptTabsForm.FormDestroy(Sender: TObject);
begin
  SimbaScriptTabsForm := nil;
end;

procedure TSimbaScriptTabsForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := True;
  FMouseDownX := X;
  FMouseDownY := Y;
end;

procedure TSimbaScriptTabsForm.FormMouseLeave(Sender: TObject);
begin
  FMouseDown := False;
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaScriptTabsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FMouseDown and CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    if TSimbaAnchorDockHostSite(HostDockSite).Header.Dragging then
      TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y)
    else
      TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(mbLeft, Shift, X, Y);
  end;
end;

procedure TSimbaScriptTabsForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown := False;

  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseUp(Button, Shift, X, Y);
end;

procedure TSimbaScriptTabsForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  CalculateFindButtonSizes();
end;

procedure TSimbaScriptTabsForm.CalculateFindButtonSizes;
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
function TSimbaScriptTabsForm.CanAnchorDocking(X, Y: Integer): Boolean;
begin
  Result := FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging) and (Abs(X - FMouseDownX) > 10) and (Abs(Y - FMouseDownY) > 10);
end;

procedure TSimbaScriptTabsForm.DoFindPanelVisibleChanged(Sender: TObject);
begin
  if (not FindPanel.Visible) and (CurrentEditor <> nil) and CurrentEditor.CanSetFocus() then
    CurrentEditor.SetFocus();

  SimbaSettings.Editor.FindPanelVisible.Value := FindPanel.Visible;
end;

procedure TSimbaScriptTabsForm.DoTabCanChange(Sender: TSimbaTabControl; OldTab, NewTab: TSimbaTab; var AllowChange: Boolean);
begin
  if Assigned(OldTab) then
    SimbaIDEEvents.CallOnBeforeTabChange(OldTab);
end;

procedure TSimbaScriptTabsForm.DoTabChange(Sender: TSimbaTabControl; NewTab: TSimbaTab);
begin
  SimbaIDEEvents.CallOnScriptTabChange(NewTab);

  if (NewTab is TSimbaScriptTab) and TSimbaScriptTab(NewTab).Editor.CanSetFocus() then
    TSimbaScriptTab(NewTab).Editor.SetFocus();
end;

procedure TSimbaScriptTabsForm.DoTabClosed(Sender: TSimbaTabControl; Tab: TSimbaTab; var CanClose: Boolean);
begin
  CanClose := TSimbaScriptTab(Tab).CanClose();
  if CanClose and (FTabControl.TabCount <= 1) and FTabControl.IsClickingCloseButton then
    AddTab();
end;

procedure TSimbaScriptTabsForm.DoTabMoved(Sender: TSimbaTabControl; AFrom, ATo: Integer);
begin
  SimbaOutputForm.MoveTab(AFrom, ATo);
end;

procedure TSimbaScriptTabsForm.SetCurrentTab(Value: TSimbaScriptTab);
begin
  FTabControl.ActiveTab := Value;
end;

constructor TSimbaScriptTabsForm.Create(AOwner: TComponent);
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
  FFindEdit.Align := alClient;
  FFindEdit.AnchorVerticalCenterTo(FindEditPanel);
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

  FFindButtonDown := TSimbaToggleButton.Create(Self);
  with FFindButtonDown do
  begin
    Images := SimbaForm.Images;
    Parent := FindButtonPanel;
    Align := alLeft;
    ImageINdex := IMG_ARROW_DOWN;
    Hint := 'Find Next';
    ShowHint := True;
    BorderSpacing.Around := 5;
    OnClick := @FindButtonClick;
  end;

  FFindButtonUp := TSimbaToggleButton.Create(Self);
  with FFindButtonUp do
  begin
    Images := SimbaForm.Images;
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
    Images := SimbaForm.Images;
    Parent := FindPanel;
    Align := alRight;
    BorderSpacing.Around := 5;
    SetCloseGlyph();
    OnClick := @FindButtonClick;
  end;

  CalculateFindButtonSizes();
end;

function TSimbaScriptTabsForm.GetTabCount: Integer;
begin
  Result := FTabControl.TabCount;
end;

function TSimbaScriptTabsForm.GetTab(Index: Integer): TSimbaScriptTab;
begin
  Result := FTabControl.Tabs[Index] as TSimbaScriptTab;
end;

function TSimbaScriptTabsForm.GetCurrentTab: TSimbaScriptTab;
begin
  Result := TSimbaScriptTab(FTabControl.ActiveTab);
end;

function TSimbaScriptTabsForm.GetCurrentEditor: TSimbaEditor;
begin
  if (CurrentTab <> nil) then
    Result := CurrentTab.Editor
  else
    Result := nil;
end;

procedure TSimbaScriptTabsForm.Replace;
begin
  if (CurrentEditor <> nil) then
    FEditorReplace.Execute(CurrentEditor);
end;

procedure TSimbaScriptTabsForm.Find;
begin
  if FFindEdit.Focused() then
  begin
    FindPanel.Hide();
    Exit;
  end;

  FindPanel.Show();
  if FFindEdit.CanSetFocus() then
    FFindEdit.SetFocus();
end;

procedure TSimbaScriptTabsForm.FindNext;
begin
  if (CurrentEditor <> nil) then
    FEditorFind.FindNext(CurrentEditor);
end;

procedure TSimbaScriptTabsForm.FindPrevious;
begin
  if (CurrentEditor <> nil) then
    FEditorFind.FindPrev(CurrentEditor);
end;

function TSimbaScriptTabsForm.AddTab: TSimbaScriptTab;
begin
  Result := FTabControl.AddTab() as TSimbaScriptTab;
end;

function TSimbaScriptTabsForm.CloseTab(Tab: TSimbaScriptTab; KeepOne: Boolean): Boolean;
begin
  Result := FTabControl.DeleteTab(Tab);
  if KeepOne and (FTabControl.TabCount = 0) then
    AddTab();
end;

function TSimbaScriptTabsForm.CloseOtherTabs(Tab: TSimbaScriptTab): Boolean;
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

function TSimbaScriptTabsForm.CloseAllTabs: Boolean;
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

function TSimbaScriptTabsForm.Open(FileName: String; CheckOtherTabs: Boolean): Boolean;
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

procedure TSimbaScriptTabsForm.Open;
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

end.
