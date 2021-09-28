unit simba.settingsform_editor_general;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, DividerBevel;

type
  TEditorGeneralFrame = class(TFrame)
    IgnoreCodeToolsDirectiveCheckbox: TCheckBox;
    CaretPastEOLCheckBox: TCheckBox;
    DividerBevel4: TDividerBevel;
    VisibleRightMarginCheckbox: TCheckBox;
    Label2: TLabel;
    ShowParameterHintsCheckbox: TCheckBox;
    OpenAutoCompletionCheckbox: TCheckBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    RightMarginEdit: TSpinEdit;
  private

  public

  end;

implementation

{$R *.lfm}

end.

