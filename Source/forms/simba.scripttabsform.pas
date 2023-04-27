{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scripttabsform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs,
  extctrls, comctrls, menus, StdCtrls, Buttons,
  simba.scripttab, simba.editor, simba.editor_findreplace, simba.component_tabcontrol;

type
  TSimbaScriptTabsForm = class(TForm)
    FindCheckBoxCaseSens: TCheckBox;
    FindCheckboxWholeWord: TCheckBox;
    FindButtonDown: TBitBtn;
    FindButtonUp: TBitBtn;
    FindEdit: TEdit;
    OpenDialog: TOpenDialog;
    MenuItemNewTab: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseOtherTabs: TMenuItem;
    FindPanel: TPanel;
    FindButtonClose: TSpeedButton;
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
  protected
    FTabControl: TSimbaTabControl;

    FEditorReplace: TSimbaEditorReplace;
    FEditorFind: TSimbaEditorFind;

    function CanAnchorDocking(X, Y: Integer): Boolean;

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

    function FindTab(ScriptInstance: TObject): TSimbaScriptTab;

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
  simba.mufasatypes, simba.files, simba.editor_docgenerator,
  simba.dockinghelpers, simba.nativeinterface, simba.outputform, simba.ide_events;

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
  if Sender.Equals(FindButtonUp) then
    FindPrevious()
  else
  if Sender.Equals(FindButtonDown) then
    FindNext()
  else
  if Sender.Equals(FindButtonClose) then
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
        if (CurrentEditor <> nil) and CurrentEditor.CanSetFocus() then
          CurrentEditor.SetFocus();
        Key := 0;
      end;

    VK_UP:
      begin
        FindButtonUp.Click();
        Key := 0;
      end;

    VK_RETURN, VK_DOWN:
      begin
        FindButtonDown.Click();
        Key := 0;
      end;
  end;
end;

procedure TSimbaScriptTabsForm.FindEditChange(Sender: TObject);
begin
  if (CurrentEditor <> nil) then
    FEditorFind.ExecuteNoDialog(CurrentEditor, FindEdit.Text, FindCheckBoxCaseSens.Checked, FindCheckboxWholeWord.Checked);
end;

procedure TSimbaScriptTabsForm.FindPanelResize(Sender: TObject);
begin
  FindEdit.Width := FindPanel.Width div 3;
end;

procedure TSimbaScriptTabsForm.FormDestroy(Sender: TObject);
begin
  SimbaScriptTabsForm := nil;
end;

procedure TSimbaScriptTabsForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaScriptTabsForm.FormMouseLeave(Sender: TObject);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaScriptTabsForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if CanAnchorDocking(X, Y) and (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y);
end;

function TSimbaScriptTabsForm.CanAnchorDocking(X, Y: Integer): Boolean;
begin
  Result := FTabControl.InEmptySpace(X, Y) and (not FTabControl.Dragging);
end;

procedure TSimbaScriptTabsForm.DoTabChange(Sender: TSimbaTabControl; NewTab: TSimbaTab);
begin
  SimbaIDEEvents.CallOnScriptTabChange(NewTab);
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
  FTabControl.OnTabChange := @DoTabChange;
  FTabControl.OnMouseMove := @FormMouseMove;
  FTabControl.OnMouseDown := @FormMouseDown;
  FTabControl.OnMouseLeave := @FormMouseLeave;
  FTabControl.DefaultTitle := 'Untitled';

  FEditorReplace := TSimbaEditorReplace.Create(Self);
  FEditorFind := TSimbaEditorFind.Create(Self);
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
  FindPanel.Show();
  if FindEdit.CanSetFocus() then
    FindEdit.SetFocus();
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

function TSimbaScriptTabsForm.FindTab(ScriptInstance: TObject): TSimbaScriptTab;
var
  I: Integer;
begin
  for I := 0 to TabCount - 1 do
    if (Tabs[I].ScriptInstance = ScriptInstance) then
    begin
      Result := Tabs[I];
      Exit;
    end;

  Result := nil;
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
      OpenDialog.InitialDir := GetScriptPath();

    if OpenDialog.Execute() then
      for I := 0 to OpenDialog.Files.Count - 1 do
        Open(OpenDialog.Files[I], True);
  except
    on E: Exception do
      ShowMessage('Exception while opening file: ' + E.Message);
  end;
end;

end.

