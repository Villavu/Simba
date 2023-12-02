unit simba.ide_mainmenubar;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls,
  simba.component_menubar;

type
  TSimbaMainMenuBar = class(TComponent)
  protected
    FMenuBar: TSimbaMenuBar;

    procedure DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoTabModified(Sender: TObject);
    procedure DoTabChanged(Sender: TObject);
    procedure DoTabScriptStateChange(Sender: TObject);
  public
    constructor Create; reintroduce;

    property MenuBar: TSimbaMenuBar read FMenuBar;
  end;

var
  SimbaMainMenuBar: TSimbaMainMenuBar;

implementation

uses
  Forms, LCLType,
  simba.ide_initialization, simba.ide_events, simba.ide_maintoolbar, simba.main;

procedure TSimbaMainMenuBar.DoApplicationKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_MENU) and (not (ssCtrl in Shift)) and FMenuBar.CanSetFocus() then
  begin
    FMenuBar.SetFocus();

    Key := 0;
  end;
end;

procedure TSimbaMainMenuBar.DoTabModified(Sender: TObject);
begin
  SimbaForm.MenuItemSave.Enabled := SimbaMainToolBar.ButtonSave.Enabled;
end;

procedure TSimbaMainMenuBar.DoTabChanged(Sender: TObject);
begin
  SimbaForm.MenuItemSaveAll.Enabled := SimbaMainToolBar.ButtonSaveAll.Enabled;
  SimbaForm.MenuItemSave.Enabled := SimbaMainToolBar.ButtonSave.Enabled;

  SimbaForm.MenuItemRun.Enabled := SimbaMainToolBar.ButtonRun.Enabled;
  SimbaForm.MenuItemPause.Enabled := SimbaMainToolBar.ButtonPause.Enabled;
  SimbaForm.MenuItemCompile.Enabled := SimbaMainToolBar.ButtonCompile.Enabled;
  SimbaForm.MenuItemStop.Enabled := SimbaMainToolBar.ButtonStop.Enabled;
end;

procedure TSimbaMainMenuBar.DoTabScriptStateChange(Sender: TObject);
begin
  SimbaForm.MenuItemRun.Enabled := SimbaMainToolBar.ButtonRun.Enabled;
  SimbaForm.MenuItemPause.Enabled := SimbaMainToolBar.ButtonPause.Enabled;
  SimbaForm.MenuItemCompile.Enabled := SimbaMainToolBar.ButtonCompile.Enabled;
  SimbaForm.MenuItemStop.Enabled := SimbaMainToolBar.ButtonStop.Enabled;
end;

constructor TSimbaMainMenuBar.Create;
begin
  inherited Create(nil);

  FMenuBar := TSimbaMenuBar.Create(Self);
  FMenuBar.Parent := SimbaForm.MainMenuPanel;
  FMenuBar.Align := alTop;
  FMenuBar.AddMenu('File', SimbaForm.MainMenuFile);
  FMenuBar.AddMenu('Edit', SimbaForm.MainMenuEdit);
  FMenuBar.AddMenu('Search', SimbaForm.MainMenuSearch);
  FMenuBar.AddMenu('Script', SimbaForm.MainMenuScript);
  FMenuBar.AddMenu('Tools', SimbaForm.MainMenuTools);
  FMenuBar.AddMenu('View', SimbaForm.MainMenuView);
  FMenuBar.AddMenu('Help', SimbaForm.MainMenuHelp);

  Application.AddOnKeyDownBeforeHandler(@DoApplicationKeyDown);

  SimbaIDEEvents.Register(SimbaIDEEvent.TAB_MODIFIED, @DoTabModified, True);
  SimbaIDEEvents.Register(SimbaIDEEvent.TAB_CHANGE, @DoTabChanged, True);
  SimbaIDEEvents.Register(SimbaIDEEvent.TAB_SCRIPTSTATE_CHANGE, @DoTabScriptStateChange, True);
end;

procedure CreateMainMenuBar;
begin
  SimbaMainMenuBar := TSimbaMainMenuBar.Create();
end;

initialization
  SimbaIDEInitialization_AddBeforeShow(@CreateMainMenuBar, 'Create Main MenuBar');

finalization
  if Assigned(SimbaMainMenuBar) then
    FreeAndNil(SimbaMainMenuBar);

end.

