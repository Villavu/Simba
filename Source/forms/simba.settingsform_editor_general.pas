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
    CompleteParenthesesCheckbox: TCheckBox;
    CompleteIndexCheckbox: TCheckBox;
    CompleteDivider: TDividerBevel;
    IgnoreCodetoolsDirectiveCheckbox: TCheckBox;
    CaretPastEOLCheckBox: TCheckBox;
    DividerBevel4: TDividerBevel;
    CompleteLabel: TLabel;
    VisibleRightMarginCheckbox: TCheckBox;
    MarginValueLabel: TLabel;
    ShowParameterHintsCheckbox: TCheckBox;
    OpenAutoCompletionCheckbox: TCheckBox;
    DividerBevel1: TDividerBevel;
    RightMarginEdit: TSpinEdit;
  private

  public

  end;

implementation

{$R *.lfm}

end.

