{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.settingsform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, ButtonPanel, Spin,
  simba.settingsform_editor_font, simba.settingsform_editor_colors, simba.settingsform_editor_general,
  simba.settingsform_simba_general, simba.settingsform_outputbox, simba.settingsform_backup;

type
  TSimbaSettingsForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Notebook: TNotebook;
    Splitter: TSplitter;
    TreeView: TTreeView;

    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TreeViewSelectionChanged(Sender: TObject);
  public
    SimbaGeneralFrame: TSimbaGeneralFrame;
    SimbaOutputBoxFrame: TSimbaOutputBoxFrame;
    SimbaBackupFrame: TSimbaBackupFrame;

    EditorGeneralFrame: TEditorGeneralFrame;
    EditorFontFrame: TEditorFontFrame;
    EditorColorsFrame: TEditorColorsFrame;

    procedure ShowPage(Title: String);

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaSettingsForm: TSimbaSettingsForm;

implementation

{$R *.lfm}

uses
  simba.settings, simba.fonthelpers;

procedure TSimbaSettingsForm.TreeViewSelectionChanged(Sender: TObject);
begin
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
    Notebook.ShowControl(TPage(TreeView.Selected.Data));
end;

procedure TSimbaSettingsForm.ShowPage(Title: String);
begin
  TreeView.Selected := TreeView.Items.FindNodeWithText(Title);
end;

procedure TSimbaSettingsForm.FormShow(Sender: TObject);
begin
  if (EditorFontFrame.FontsNameComboBox.Items.Count = 0) then
  begin
    EditorFontFrame.FontsNameComboBox.Items.AddStrings(GetFixedFonts());
    EditorFontFrame.FontsNameComboBox.ItemIndex := EditorFontFrame.FontsNameComboBox.Items.IndexOf(SimbaSettings.Editor.FontName.Value);
    if (EditorFontFrame.FontsNameComboBox.ItemIndex < 0) then
      EditorFontFrame.FontsNameComboBox.ItemIndex := 0;
  end;

  EditorFontFrame.FontSizeSpinEdit.Value := SimbaSettings.Editor.FontSize.Value;
  EditorFontFrame.FontAntiAliasedCheckbox.Checked := SimbaSettings.Editor.AntiAliased.Value;

  EditorGeneralFrame.RightMarginEdit.Value := SimbaSettings.Editor.RightMargin.Value;
  EditorGeneralFrame.VisibleRightMarginCheckbox.Checked := SimbaSettings.Editor.RightMarginVisible.Value;
  EditorGeneralFrame.CaretPastEOLCheckBox.Checked := SimbaSettings.Editor.AllowCaretPastEOL.Value;
  EditorGeneralFrame.OpenAutoCompletionCheckbox.Checked := SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value;
  EditorGeneralFrame.ShowParameterHintsCheckbox.Checked := SimbaSettings.Editor.AutomaticallyShowParameterHints.Value;
  EditorGeneralFrame.IgnoreCodeToolsDirectiveCheckbox.Checked := SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value;

  EditorGeneralFrame.CompleteBeginCheckbox.Checked := SimbaSettings.Editor.AutomaticallyCompleteBegin.Value;
  EditorGeneralFrame.CompleteParenthesesCheckbox.Checked := SimbaSettings.Editor.AutomaticallyCompleteParentheses.Value;
  EditorGeneralFrame.CompleteIndexCheckbox.Checked := SimbaSettings.Editor.AutomaticallyCompleteIndex.Value;

  SimbaGeneralFrame.ToolbarSizeTrackBar.Position := SimbaSettings.General.ToolbarSize.Value;
  SimbaGeneralFrame.ToolbarSizeTrackBar.OnChange(nil);
  SimbaGeneralFrame.FontSizeTrackBar.Position := SimbaSettings.General.CustomFontSize.Value;
  SimbaGeneralFrame.FontSizeTrackBar.OnChange(nil);

  SimbaGeneralFrame.ToolbarPosition := SimbaSettings.General.ToolbarPosition.Value;

  SimbaGeneralFrame.CheckGroup1.Checked[0] := SimbaSettings.General.OutputClearOnCompile.Value;
  SimbaGeneralFrame.CheckGroup1.Checked[1] := SimbaSettings.General.OpenSSLExtractOnLaunch.Value;
  SimbaGeneralFrame.CheckGroup1.Checked[2] := SimbaSettings.General.MacOSKeystrokes.Value;

  EditorGeneralFrame.Load();
  EditorColorsFrame.Load();
  SimbaOutputBoxFrame.Load();
  SimbaBackupFrame.Load();
end;

procedure TSimbaSettingsForm.OKButtonClick(Sender: TObject);
begin
  SimbaSettings.Editor.FontName.Value := EditorFontFrame.FontsNameComboBox.Text;
  SimbaSettings.Editor.FontSize.Value := EditorFontFrame.FontSizeSpinEdit.Value;
  SimbaSettings.Editor.AntiAliased.Value := EditorFontFrame.FontAntiAliasedCheckbox.Checked;

  SimbaSettings.Editor.AllowCaretPastEOL.Value := EditorGeneralFrame.CaretPastEOLCheckBox.Checked;
  SimbaSettings.Editor.RightMargin.Value := EditorGeneralFrame.RightMarginEdit.Value;
  SimbaSettings.Editor.RightMarginVisible.Value := EditorGeneralFrame.VisibleRightMarginCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value := EditorGeneralFrame.OpenAutoCompletionCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyShowParameterHints.Value := EditorGeneralFrame.ShowParameterHintsCheckbox.Checked;
  SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value := EditorGeneralFrame.IgnoreCodeToolsDirectiveCheckbox.Checked;

  SimbaSettings.Editor.AutomaticallyCompleteBegin.Value := EditorGeneralFrame.CompleteBeginCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyCompleteIndex.Value := EditorGeneralFrame.CompleteIndexCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyCompleteParentheses.Value := EditorGeneralFrame.CompleteParenthesesCheckbox.Checked;

  if (SimbaGeneralFrame.ToolbarSizeTrackBar.Position = SimbaGeneralFrame.ToolbarSizeTrackBar.Min) then
    SimbaSettings.General.ToolbarSize.Value := SimbaSettings.General.ToolbarSize.DefaultValue
  else
    SimbaSettings.General.ToolbarSize.Value := SimbaGeneralFrame.ToolbarSizeTrackBar.Position;

  if (SimbaGeneralFrame.FontSizeTrackBar.Position = SimbaGeneralFrame.FontSizeTrackBar.Min) then
    SimbaSettings.General.CustomFontSize.Value := SimbaSettings.General.CustomFontSize.DefaultValue
  else
    SimbaSettings.General.CustomFontSize.Value := SimbaGeneralFrame.FontSizeTrackBar.Position;

  SimbaSettings.General.ToolbarPosition.Value := SimbaGeneralFrame.ToolbarPosition;

  SimbaSettings.General.OutputClearOnCompile.Value := SimbaGeneralFrame.CheckGroup1.Checked[0];
  SimbaSettings.General.OpenSSLExtractOnLaunch.Value := SimbaGeneralFrame.CheckGroup1.Checked[1];
  SimbaSettings.General.MacOSKeystrokes.Value := SimbaGeneralFrame.CheckGroup1.Checked[2];

  EditorGeneralFrame.Save();
  EditorColorsFrame.Save();
  SimbaOutputBoxFrame.Save();
  SimbaBackupFrame.Save();
end;

constructor TSimbaSettingsForm.Create(AOwner: TComponent);

  function AddPage(Name: String; ParentNode: TTreeNode): TPage;
  var
    Node: TTreeNode;
  begin
    Result := TPage.Create(Notebook);
    Result.Parent := Notebook;

    Node := TreeView.Items.AddChild(ParentNode, Name);
    Node.Data := Result;

    if (ParentNode.Data = nil) then
      ParentNode.Data := Result;
  end;

var
  Node: TTreeNode;
begin
  inherited Create(AOwner);

  Width  := Scale96ToScreen(800);
  Height := Scale96ToScreen(600);

  Constraints.MinWidth  := Round(Width * 0.75);
  Constraints.MinHeight := Round(Height * 0.75);

  Node := TreeView.Items.Add(nil, 'Simba');

  SimbaGeneralFrame := TSimbaGeneralFrame.Create(Self);
  SimbaGeneralFrame.Parent := AddPage('General', Node);
  SimbaGeneralFrame.Align := alClient;
  SimbaGeneralFrame.ParentFont := True;

  SimbaOutputBoxFrame := TSimbaOutputBoxFrame.Create(Self);
  SimbaOutputBoxFrame.Parent := AddPage('Output Box', Node);
  SimbaOutputBoxFrame.Align := alClient;
  SimbaOutputBoxFrame.ParentFont := True;

  SimbaBackupFrame := TSimbaBackupFrame.Create(Self);
  SimbaBackupFrame.Parent := AddPage('Script Backup', Node);
  SimbaBackupFrame.Align := alClient;
  SimbaBackupFrame.ParentFont := True;

  Node := TreeView.Items.Add(nil, 'Editor');

  EditorGeneralFrame := TEditorGeneralFrame.Create(Self);
  EditorGeneralFrame.Parent := AddPage('General', Node);
  EditorGeneralFrame.Align := alClient;
  EditorGeneralFrame.ParentFont := True;

  EditorFontFrame := TEditorFontFrame.Create(Self);
  EditorFontFrame.Parent := AddPage('Font', Node);
  EditorFontFrame.Align := alClient;
  EditorFontFrame.ParentFont := True;

  EditorColorsFrame := TEditorColorsFrame.Create(Self);
  EditorColorsFrame.Parent := AddPage('Colors', Node);
  EditorColorsFrame.Align := alClient;
  EditorColorsFrame.ParentFont := True;

  TreeView.Selected := TreeView.Items.GetFirstNode();
end;

end.

