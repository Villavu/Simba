{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_settings;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, ButtonPanel, Spin,
  simba.frame_settings_editor,
  simba.frame_settings_editorcolors,
  simba.frame_settings_editordefault,
  simba.frame_settings_editorfont,
  simba.frame_settings_general,
  simba.frame_settings_output,
  simba.frame_settings_backup,
  simba.frame_settings_codetools;

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
    SimbaCodetoolsFrame: TSimbaCodetoolsFrame;
    SimbaOutputBoxFrame: TSimbaOutputBoxFrame;
    SimbaBackupFrame: TSimbaBackupFrame;

    EditorGeneralFrame: TEditorGeneralFrame;
    EditorFontFrame: TEditorFontFrame;
    EditorColorsFrame: TEditorColorsFrame;
    EditorDefaultFrame: TEditorDefaultFrame;

    procedure ShowPage(Title: String);

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaSettingsForm: TSimbaSettingsForm;

implementation

{$R *.lfm}

uses
  simba.settings, simba.misc;

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

  EditorGeneralFrame.CompleteBeginCheckbox.Checked := SimbaSettings.Editor.AutomaticallyCompleteBegin.Value;
  EditorGeneralFrame.CompleteParenthesesCheckbox.Checked := SimbaSettings.Editor.AutomaticallyCompleteParentheses.Value;
  EditorGeneralFrame.CompleteIndexCheckbox.Checked := SimbaSettings.Editor.AutomaticallyCompleteIndex.Value;

  SimbaGeneralFrame.ToolbarSizeTrackBar.Position := SimbaSettings.General.ToolbarSize.Value;
  SimbaGeneralFrame.ToolbarSizeTrackBar.OnChange(nil);
  SimbaGeneralFrame.FontSizeTrackBar.Position := SimbaSettings.General.CustomFontSize.Value;
  SimbaGeneralFrame.FontSizeTrackBar.OnChange(nil);

  SimbaGeneralFrame.Load();
  SimbaCodetoolsFrame.Load();
  EditorGeneralFrame.Load();
  EditorColorsFrame.Load();
  EditorDefaultFrame.Load();
  SimbaOutputBoxFrame.Load();
  SimbaBackupFrame.Load();

  TreeView.Selected := TreeView.Items.GetFirstNode();
end;

procedure TSimbaSettingsForm.OKButtonClick(Sender: TObject);
begin
  SimbaSettings.Editor.FontName.Value := EditorFontFrame.FontsNameComboBox.Text;
  SimbaSettings.Editor.FontSize.Value := EditorFontFrame.FontSizeSpinEdit.Value;
  SimbaSettings.Editor.AntiAliased.Value := EditorFontFrame.FontAntiAliasedCheckbox.Checked;

  SimbaSettings.Editor.AllowCaretPastEOL.Value := EditorGeneralFrame.CaretPastEOLCheckBox.Checked;
  SimbaSettings.Editor.RightMargin.Value := EditorGeneralFrame.RightMarginEdit.Value;
  SimbaSettings.Editor.RightMarginVisible.Value := EditorGeneralFrame.VisibleRightMarginCheckbox.Checked;

  SimbaSettings.Editor.AutomaticallyCompleteBegin.Value := EditorGeneralFrame.CompleteBeginCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyCompleteIndex.Value := EditorGeneralFrame.CompleteIndexCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyCompleteParentheses.Value := EditorGeneralFrame.CompleteParenthesesCheckbox.Checked;

  SimbaGeneralFrame.Save();
  SimbaCodetoolsFrame.Save();
  EditorGeneralFrame.Save();
  EditorColorsFrame.Save();
  EditorDefaultFrame.Save();
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

  SimbaCodetoolsFrame := TSimbaCodetoolsFrame.Create(Self);
  SimbaCodetoolsFrame.Parent := AddPage('Code Tools', Node);
  SimbaCodetoolsFrame.Align := alClient;
  SimbaCodetoolsFrame.ParentFont := True;

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

  EditorDefaultFrame := TEditorDefaultFrame.Create(Self);
  EditorDefaultFrame.Parent := AddPage('Default Script', Node);
  EditorDefaultFrame.Align := alClient;
  EditorDefaultFrame.ParentFont := True;
end;

end.

