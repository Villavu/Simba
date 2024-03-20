{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.frame_settings_codetools;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, DividerBevel;

type
  TSimbaCodetoolsFrame = class(TFrame)
    CompletionModShiftCheckbox: TCheckBox;
    CompletionKeywordsCheckbox: TCheckBox;
    CompletionModAltCheckbox: TCheckBox;
    CompletionModCtrlCheckbox: TCheckBox;
    ParamHintModShiftCheckbox: TCheckBox;
    ParamHintModAltCheckbox: TCheckBox;
    ParamHintModCtrlCheckbox: TCheckBox;
    AutoOpenCompletionCheckbox: TCheckBox;
    AutoOpenParamHintCheckbox: TCheckBox;
    IgnoreDirectiveCheckbox: TCheckBox;
    CompletionKeyCombo: TComboBox;
    ParamHintKeyCombo: TComboBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
  private
    FAddedKeys: Boolean;

    procedure AddKeysToComboBoxes;
  public
    procedure Load;
    procedure Save;
  end;

implementation

{$R *.lfm}

uses
  simba.settings, LCLType, LCLProc;

procedure TSimbaCodetoolsFrame.AddKeysToComboBoxes;

  procedure Add(const Key: integer);
  var
    S: String;
  begin
    S := KeyAndShiftStateToKeyString(Key, []);

    if not KeyStringIsIrregular(S) then
    begin
      CompletionKeyCombo.Items.AddObject(S, TObject(Pointer(PtrUInt(Key))));
      ParamHintKeyCombo.Items.AddObject(S, TObject(Pointer(PtrUInt(Key))));
    end;
  end;

var
  Key: Integer;
begin
  if FAddedKeys then
    Exit;
  FAddedKeys := True;

  for Key := 0 to VK_SCROLL do
    Add(Key);
  for Key := VK_BROWSER_BACK to VK_OEM_8 do
    Add(Key);
end;

procedure TSimbaCodetoolsFrame.Load;
begin
  AddKeysToComboBoxes();

  CompletionKeywordsCheckbox.Checked := SimbaSettings.CodeTools.CompletionAddKeywords.Value;
  IgnoreDirectiveCheckbox.Checked    := SimbaSettings.CodeTools.IgnoreIDEDirective.Value;
  AutoOpenParamHintCheckbox.Checked  := SimbaSettings.CodeTools.ParamHintOpenAutomatically.Value;
  AutoOpenCompletionCheckbox.Checked := SimbaSettings.CodeTools.CompletionOpenAutomatically.Value;

  CompletionKeyCombo.ItemIndex       := CompletionKeyCombo.Items.IndexOfObject(TObject(Pointer(PtrUInt(SimbaSettings.CodeTools.CompletionKey.Value))));
  CompletionModShiftCheckbox.Checked := ssShift in TShiftState(Int32(SimbaSettings.CodeTools.CompletionKeyModifiers.Value));
  CompletionModAltCheckbox.Checked   := ssAlt   in TShiftState(Int32(SimbaSettings.CodeTools.CompletionKeyModifiers.Value));
  CompletionModCtrlCheckbox.Checked  := ssCtrl  in TShiftState(Int32(SimbaSettings.CodeTools.CompletionKeyModifiers.Value));

  ParamHintKeyCombo.ItemIndex       := ParamHintKeyCombo.Items.IndexOfObject(TObject(Pointer(PtrUInt(SimbaSettings.CodeTools.ParamHintKey.Value))));
  ParamHintModShiftCheckbox.Checked := ssShift in TShiftState(Int32(SimbaSettings.CodeTools.ParamHintKeyModifiers.Value));
  ParamHintModAltCheckbox.Checked   := ssAlt   in TShiftState(Int32(SimbaSettings.CodeTools.ParamHintKeyModifiers.Value));
  ParamHintModCtrlCheckbox.Checked  := ssCtrl  in TShiftState(Int32(SimbaSettings.CodeTools.ParamHintKeyModifiers.Value));
end;

procedure TSimbaCodetoolsFrame.Save;
var
  CompletionKeyState, ParamKeyState: TShiftState;
begin
  SimbaSettings.CodeTools.IgnoreIDEDirective.Value          := IgnoreDirectiveCheckbox.Checked;
  SimbaSettings.CodeTools.ParamHintOpenAutomatically.Value  := AutoOpenParamHintCheckbox.Checked;
  SimbaSettings.CodeTools.CompletionAddKeywords.Value       := CompletionKeywordsCheckbox.Checked;
  SimbaSettings.CodeTools.CompletionOpenAutomatically.Value := AutoOpenCompletionCheckbox.Checked;

  if (CompletionKeyCombo.ItemIndex > -1) then
    SimbaSettings.CodeTools.CompletionKey.Value := PtrUInt(CompletionKeyCombo.Items.Objects[CompletionKeyCombo.ItemIndex]);
  if (ParamHintKeyCombo.ItemIndex > -1) then
    SimbaSettings.CodeTools.ParamHintKey.Value := PtrUInt(ParamHintKeyCombo.Items.Objects[ParamHintKeyCombo.ItemIndex]);

  CompletionKeyState := [];
  ParamKeyState      := [];

  if CompletionModShiftCheckbox.Checked then Include(CompletionKeyState, ssShift);
  if CompletionModAltCheckbox.Checked   then Include(CompletionKeyState, ssAlt);
  if CompletionModCtrlCheckbox.Checked  then Include(CompletionKeyState, ssCtrl);

  if ParamHintModShiftCheckbox.Checked then Include(ParamKeyState, ssShift);
  if ParamHintModAltCheckbox.Checked   then Include(ParamKeyState, ssAlt);
  if ParamHintModCtrlCheckbox.Checked  then Include(ParamKeyState, ssCtrl);

  SimbaSettings.CodeTools.CompletionKeyModifiers.Value := Int32(CompletionKeyState);
  SimbaSettings.CodeTools.ParamHintKeyModifiers.Value  := Int32(ParamKeyState);
end;

end.

