{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform_editor_general;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ExtCtrls, DividerBevel;

type
  TEditorGeneralFrame = class(TFrame)
    CompleteBeginCheckbox: TCheckBox;
    CompleteDivider1: TDividerBevel;
    CompleteParenthesesCheckbox: TCheckBox;
    CompleteIndexCheckbox: TCheckBox;
    CompleteDivider: TDividerBevel;
    CaretPastEOLCheckBox: TCheckBox;
    DividerBevel4: TDividerBevel;
    CompleteLabel: TLabel;
    DocCommentMemo: TMemo;
    VisibleRightMarginCheckbox: TCheckBox;
    MarginValueLabel: TLabel;
    RightMarginEdit: TSpinEdit;
  public
    procedure Load;
    procedure Save;
  end;

implementation

{$R *.lfm}

uses
  simba.settings;

procedure TEditorGeneralFrame.Load;
begin
  DocCommentMemo.Lines.Text := SimbaSettings.Editor.DocumentationComment.Value;
end;

procedure TEditorGeneralFrame.Save;
begin
  SimbaSettings.Editor.DocumentationComment.Value := DocCommentMemo.Lines.Text;
end;

end.

