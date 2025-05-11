{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Currently unused.
}
unit simba.ide_editor_history;

{$i simba.inc}

interface

uses
  classes, sysutils, lcltype, lazsyneditmousecmdstypes,
  synedit, syneditkeycmds, syneditmousecmds;

type
  TSimbaEditorPlugin_History = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    function DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
    procedure CaretStatus(Sender: TObject; Changes: TSynStatusChanges);
  public
    class var EditorCommandForward: TSynEditorCommand;
    class var EditorCommandBack: TSynEditorCommand;
    class constructor Create;
  end;

implementation

uses
  SynEditTypes, simba.ide_editor,
  simba.ide_utils, simba.form_tabs_history, simba.ide_tab, simba.base, simba.form_tabs;

class constructor TSimbaEditorPlugin_History.Create;
begin
  DebugLn('TSimbaEditorPlugin_History.Create');
  EditorCommandForward := AllocatePluginKeyRange(1);
  EditorCommandBack    := AllocatePluginKeyRange(1);
end;

{ caret-move hook }
procedure TSimbaEditorPlugin_History.CaretStatus(Sender: TObject; Changes: TSynStatusChanges);
var
  Tab: TSimbaScriptTab;
  Ed: TSimbaEditor;
begin
  DebugLn('TSimbaEditorPlugin_History.CaretStatus');
  if SimbaScriptTabHistory.FSilent
     then Exit;      // ignore Back/Forward moves
  if (scCaretX in Changes) or (scCaretY in Changes) then
  begin
    Ed := TSimbaEditor(Sender);
    if (Ed.CaretX = 1) and (Ed.CaretY = 1) then
    begin
      DebugLn('Since CaretXY = [1, 1] not adding to history');
      Exit;
    end;
    DebugLn(Sender.ToString());
    Tab := GetSimbaActiveTab();
    SimbaScriptTabHistory.PushFromEditor(Tab, Ed.CaretXY);     // de-dup inside history
  end;
end;

procedure TSimbaEditorPlugin_History.DoEditorAdded(Value: TCustomSynEdit);
begin
  DebugLn('TSimbaEditorPlugin_History.DoEditorAdded');
  inherited DoEditorAdded(Value);

   Value.MouseActions.AddCommand(EditorCommandForward, False,
     LazSynEditMouseCmdsTypes.mbExtra2, ccSingle, cdDown, [], []);
   Value.MouseActions.AddCommand(EditorCommandBack, False,
     LazSynEditMouseCmdsTypes.mbExtra1, ccSingle, cdDown, [], []);

  Value.RegisterMouseActionExecHandler(@DoMouseAction);

  DebugLn('Registering CaretStatus');
  Value.RegisterStatusChangedHandler(@CaretStatus, [scCaretX, scCaretY]);
end;

function TSimbaEditorPlugin_History.DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  DebugLn('TSimbaEditorPlugin_History.DoMouseAction');
  if AnAction.Command = EditorCommandForward then
    SimbaScriptTabHistory.GoForward
  else
  if AnAction.Command = EditorCommandBack then
    SimbaScriptTabHistory.GoBack;

  Result := False;
end;



end.

