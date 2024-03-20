{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_mousewheelzoom;

{$i simba.inc}

interface

uses
  classes, sysutils, lcltype,
  synedit, syneditkeycmds, syneditmousecmds;

type
  TSimbaEditorPlugin_MouseWheelZoom = class(TLazSynEditPlugin)
  protected
    procedure DoEditorAdded(Value: TCustomSynEdit); override;
    function DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
    procedure DoZoom(Data: PtrInt);
  public
    class var EditorCommand: TSynEditorCommand;
    class constructor Create;
  end;

implementation

uses
  forms,
  simba.settings;

procedure TSimbaEditorPlugin_MouseWheelZoom.DoEditorAdded(Value: TCustomSynEdit);
begin
  inherited DoEditorAdded(Value);

  Value.RegisterMouseActionExecHandler(@DoMouseAction);
end;

function TSimbaEditorPlugin_MouseWheelZoom.DoMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := False;

  if (AnAction.Command = emcWheelZoomIn) or (AnAction.Command = emcWheelZoomOut) then
    Application.QueueAsyncCall(@DoZoom, 0);
end;

procedure TSimbaEditorPlugin_MouseWheelZoom.DoZoom(Data: PtrInt);
begin
  SimbaSettings.Editor.FontSize.Value := Editor.Font.Size;
end;

class constructor TSimbaEditorPlugin_MouseWheelZoom.Create;
begin
  EditorCommand := AllocatePluginKeyRange(1);
end;

end.

