unit simba.settingsform;

{$mode objfpc}{$H+}
{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, ButtonPanel, Spin,
  simba.settingsform_editor_font, simba.settingsform_editor_colors,
  simba.settingsform_editor_general, simba.settingsform_gui;

type
  TSimbaSettingsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Notebook: TNotebook;
    Splitter1: TSplitter;
    TreeView: TTreeView;

    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
  public
    EditorGeneralFrame: TEditorGeneralFrame;
    EditorFontFrame: TEditorFontFrame;
    EditorColorsFrame: TEditorColorsFrame;

    GUIFrame: TGUIFrame;

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaSettingsForm: TSimbaSettingsForm;

implementation

{$R *.lfm}

uses
  simba.settings, simba.fonthelpers;

procedure TSimbaSettingsForm.TreeViewClick(Sender: TObject);
begin
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
    Notebook.ShowControl(TPage(TreeView.Selected.Data));
end;

procedure TSimbaSettingsForm.FormShow(Sender: TObject);
begin
  if (EditorFontFrame.FontsNameComboBox.Items.Count = 0) then
  begin
    EditorFontFrame.FontsNameComboBox.Items.AddStrings(SimbaFontHelpers.GetFixedFonts());
    EditorFontFrame.FontsNameComboBox.ItemIndex := EditorFontFrame.FontsNameComboBox.Items.IndexOf(SimbaSettings.Editor.FontName.Value);
    if (EditorFontFrame.FontsNameComboBox.ItemIndex < 0) then
      EditorFontFrame.FontsNameComboBox.ItemIndex := 0;
  end;

  EditorFontFrame.FontSizeSpinEdit.Value := SimbaSettings.Editor.FontSize.Value;
  EditorFontFrame.FontAntiAliasedCheckbox.Checked := SimbaSettings.Editor.AntiAliased.Value;

  //EnvironmentFrame.OpenSSLOnLaunch.Checked := SimbaSettings.Environment.OpenSSLOnLaunch.Value;

  EditorGeneralFrame.RightMarginEdit.Value := SimbaSettings.Editor.RightMargin.Value;
  EditorGeneralFrame.VisibleRightMarginCheckbox.Checked := SimbaSettings.Editor.RightMarginVisible.Value;
  EditorGeneralFrame.CaretPastEOLCheckBox.Checked := SimbaSettings.Editor.AllowCaretPastEOL.Value;
  EditorGeneralFrame.OpenAutoCompletionCheckbox.Checked := SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value;
  EditorGeneralFrame.ShowParameterHintsCheckbox.Checked := SimbaSettings.Editor.AutomaticallyShowParameterHints.Value;
  EditorGeneralFrame.IgnoreCodeToolsDirectiveCheckbox.Checked := SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value;

  GUIFrame.ToolbarSizeTrackBar.Position := SimbaSettings.GUI.ToolbarSize.Value;
  GUIFrame.FontSizeTrackBar.Position := SimbaSettings.GUI.CustomFontSize.Value;
end;

procedure TSimbaSettingsForm.OKButtonClick(Sender: TObject);
begin
  //SimbaSettings.Environment.OpenSSLOnLaunch.Value := EnvironmentFrame.OpenSSLOnLaunch.Checked;

  SimbaSettings.Editor.FontName.Value := EditorFontFrame.FontsNameComboBox.Text;
  SimbaSettings.Editor.FontSize.Value := EditorFontFrame.FontSizeSpinEdit.Value;
  SimbaSettings.Editor.AntiAliased.Value := EditorFontFrame.FontAntiAliasedCheckbox.Checked;

  SimbaSettings.Editor.AllowCaretPastEOL.Value := EditorGeneralFrame.CaretPastEOLCheckBox.Checked;
  SimbaSettings.Editor.RightMargin.Value := EditorGeneralFrame.RightMarginEdit.Value;
  SimbaSettings.Editor.RightMarginVisible.Value := EditorGeneralFrame.VisibleRightMarginCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value := EditorGeneralFrame.OpenAutoCompletionCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyShowParameterHints.Value := EditorGeneralFrame.ShowParameterHintsCheckbox.Checked;
  SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value := EditorGeneralFrame.IgnoreCodeToolsDirectiveCheckbox.Checked;

  if (GUIFrame.ToolbarSizeTrackBar.Position = GUIFrame.ToolbarSizeTrackBar.Min) then
    SimbaSettings.GUI.ToolbarSize.Value := SimbaSettings.GUI.ToolbarSize.DefaultValue
  else
    SimbaSettings.GUI.ToolbarSize.Value := GUIFrame.ToolbarSizeTrackBar.Position;

  if (GUIFrame.FontSizeTrackBar.Position = GUIFrame.FontSizeTrackBar.Min) then
    SimbaSettings.GUI.CustomFontSize.Value := SimbaSettings.GUI.CustomFontSize.DefaultValue
  else
    SimbaSettings.GUI.CustomFontSize.Value := GUIFrame.FontSizeTrackBar.Position;
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

  Width := 950;
  Height := 650;

  Node := TreeView.Items.Add(nil, 'Editor');

  EditorGeneralFrame := TEditorGeneralFrame.Create(Self);
  EditorGeneralFrame.Parent := AddPage('General', Node);
  EditorGeneralFrame.Align := alClient;

  EditorFontFrame := TEditorFontFrame.Create(Self);
  EditorFontFrame.Parent := AddPage('Font', Node);
  EditorFontFrame.Align := alClient;

  EditorColorsFrame := TEditorColorsFrame.Create(Self);
  EditorColorsFrame.Parent := AddPage('Colors', Node);
  EditorColorsFrame.Align := alClient;

  Node := TreeView.Items.Add(nil, 'Window');

  GUIFrame := TGUIFrame.Create(Self);
  GUIFrame.Parent := AddPage('General', Node);
  GUIFrame.Align := alClient;
end;

end.

