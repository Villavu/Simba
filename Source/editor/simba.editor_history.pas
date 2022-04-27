{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_history;

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
  public
    class var EditorCommandForward: TSynEditorCommand;
    class var EditorCommandBack: TSynEditorCommand;
    class constructor Create;
  end;

implementation

uses
  simba.settings, simba.scripttabhistory;

procedure TSimbaEditorPlugin_History.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  Value.RegisterMouseActionExecHandler(@DoMouseAction);
  Value.MouseActions.AddCommand(EditorCommandForward, False, LazSynEditMouseCmdsTypes.mbExtra2, ccSingle, cdDown, [], []);
  Value.MouseActions.AddCommand(EditorCommandBack, False, LazSynEditMouseCmdsTypes.mbExtra1, ccSingle, cdDown, [], []);
end;

function TSimbaEditorPlugin_History.DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;

  if (AnAction.Command = EditorCommandForward) then
    SimbaScriptTabHistory.GoForward()
  else
  if (AnAction.Command = EditorCommandBack) then
    SimbaScriptTabHistory.GoBack();
end;

class constructor TSimbaEditorPlugin_History.Create;
begin
  EditorCommandForward := AllocatePluginKeyRange(1);
  EditorCommandBack := AllocatePluginKeyRange(1);
end;

end.

