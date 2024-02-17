{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
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
    procedure DoActiveControlChange(Sender: TObject; LastControl: TControl);

    procedure DoTabModified(Sender: TObject);
    procedure DoTabChanged(Sender: TObject);
    procedure DoTabScriptStateChange(Sender: TObject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

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
  case Key of
    VK_ESCAPE:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;
        FMenuBar.HotIndex := -1;

        Key := 0;
      end;

    VK_MENU:
      begin
        if (FMenuBar.HotIndex > -1) then // move to next
        begin
          if (FMenuBar.HotIndex = FMenuBar.MenuCount - 1) then
            FMenuBar.HotIndex := 0
          else
            FMenuBar.HotIndex := FMenuBar.HotIndex + 1;
        end else
          FMenuBar.HotIndex := 0;

        Key := 0;
      end;

    VK_LEFT:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;

        if (FMenuBar.HotIndex = 0) then
          FMenuBar.HotIndex := FMenuBar.MenuCount - 1
        else
          FMenuBar.HotIndex := FMenuBar.HotIndex - 1;

        Key := 0;
      end;

    VK_RIGHT:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;

        if (FMenuBar.HotIndex = FMenuBar.MenuCount - 1) then
          FMenuBar.HotIndex := 0
        else
          FMenuBar.HotIndex := FMenuBar.HotIndex + 1;

        Key := 0;
      end;

    VK_RETURN:
      begin
        if (FMenuBar.HotIndex = -1) then
          Exit;
        Application.QueueAsyncCall(@FMenuBar.PopupDelayed, FMenuBar.HotIndex);

        Key := 0;
      end;
    else
      FMenuBar.HotIndex := -1;
  end;
end;

procedure TSimbaMainMenuBar.DoActiveControlChange(Sender: TObject; LastControl: TControl);
begin
  FMenuBar.HotIndex := -1;
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
  Screen.AddHandlerActiveControlChanged(@DoActiveControlChange);

  SimbaIDEEvents.Register(SimbaIDEEvent.TAB_MODIFIED, @DoTabModified, True);
  SimbaIDEEvents.Register(SimbaIDEEvent.TAB_CHANGE, @DoTabChanged, True);
  SimbaIDEEvents.Register(SimbaIDEEvent.TAB_SCRIPTSTATE_CHANGE, @DoTabScriptStateChange, True);
end;

destructor TSimbaMainMenuBar.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  Application.RemoveAllHandlersOfObject(Self);

  Screen.RemoveAllHandlersOfObject(Self);

  inherited Destroy();
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

