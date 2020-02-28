unit simba.settingsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, ButtonPanel,
  simba.settingsform_editor_font, simba.settingsform_editor_colors, simba.settingsform_environment, simba.settingsform_editor_general,
  simba.settingsform_gui;

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

    EnvironmentFrame: TEnvironmentFrame;
    GUIFrame: TGUIFrame;

    constructor Create(AOwner: TComponent); override;
  end;

var
  SimbaSettingsForm: TSimbaSettingsForm;

implementation

uses
  simba.settings, simba.editor;

procedure TSimbaSettingsForm.TreeViewClick(Sender: TObject);
begin
  if (TreeView.Selected <> nil) and (TreeView.Selected.Data <> nil) then
    Notebook.ShowControl(TPage(TreeView.Selected.Data));
end;

procedure TSimbaSettingsForm.FormShow(Sender: TObject);
begin
  EditorFontFrame.FontHeightEdit.Value := SimbaSettings.Editor.FontHeight.Value;
  EditorFontFrame.FontHeightEdit.OnChange(nil);
  EditorFontFrame.FontAntiAliasedCheckbox.Checked := SimbaSettings.Editor.AntiAliasing.Value;
  EditorFontFrame.FontAntiAliasedCheckbox.OnChange(nil);

  EnvironmentFrame.IncludePathEdit.Text := SimbaSettings.Environment.IncludePath.Value;
  EnvironmentFrame.PluginPathEdit.Text := SimbaSettings.Environment.PluginPath.Value;
  EnvironmentFrame.FontPathEdit.Text := SimbaSettings.Environment.FontPath.Value;
  EnvironmentFrame.ScriptPathEdit.Text := SimbaSettings.Environment.ScriptPath.Value;
  EnvironmentFrame.ScriptExecutableEdit.Text := SimbaSettings.Environment.ScriptExectuablePath.Value;

  EnvironmentFrame.OpenSSLExtractCheckbox.Checked := SimbaSettings.Resources.ExtractOpenSSL.Value;
  EnvironmentFrame.OpenSSLInitalizeCheckbox.Checked := SimbaSettings.Resources.InitializeOpenSSL.Value;
  EnvironmentFrame.ScriptExecutableExtractCheckbox.Checked := SimbaSettings.Resources.ExtractSimbaScript.Value;

  EditorGeneralFrame.RightMarginEdit.Value := SimbaSettings.Editor.RightMargin.Value;
  EditorGeneralFrame.VisibleRightMarginCheckbox.Checked := SimbaSettings.Editor.RightMarginVisible.Value;
  EditorGeneralFrame.DefaultScriptEdit.Text := SimbaSettings.Editor.DefaultScriptPath.Value;
  EditorGeneralFrame.CaretPastEOLCheckBox.Checked := SimbaSettings.Editor.AllowCaretPastEOL.Value;
  EditorGeneralFrame.OpenAutoCompletionCheckbox.Checked := SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value;
  EditorGeneralFrame.ShowParameterHintsCheckbox.Checked := SimbaSettings.Editor.AutomaticallyShowParameterHints.Value;
  EditorGeneralFrame.DividerVisibleCheckbox.Checked := SimbaSettings.Editor.DividerVisible.Value;
  EditorGeneralFrame.IgnoreCodeToolsDirectiveCheckbox.Checked := SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value;

  GUIFrame.ToolbarSizeTrackBar.Position := SimbaSettings.GUI.CustomToolbarSize.Value;
  GUIFrame.FontSizeTrackBar.Position := SimbaSettings.GUI.CustomFontSize.Value;
end;

procedure TSimbaSettingsForm.OKButtonClick(Sender: TObject);
begin
  SimbaSettings.Environment.IncludePath.Value := EnvironmentFrame.IncludePathEdit.Text;
  SimbaSettings.Environment.PluginPath.Value := EnvironmentFrame.PluginPathEdit.Text;
  SimbaSettings.Environment.FontPath.Value := EnvironmentFrame.FontPathEdit.Text;
  SimbaSettings.Environment.ScriptPath.Value := EnvironmentFrame.ScriptPathEdit.Text;
  SimbaSettings.Environment.ScriptExectuablePath.Value := EnvironmentFrame.ScriptExecutableEdit.Text;

  SimbaSettings.Resources.ExtractOpenSSL.Value := EnvironmentFrame.OpenSSLExtractCheckbox.Checked;
  SimbaSettings.Resources.InitializeOpenSSL.Value := EnvironmentFrame.OpenSSLInitalizeCheckbox.Checked;
  SimbaSettings.Resources.ExtractSimbaScript.Value := EnvironmentFrame.ScriptExecutableExtractCheckbox.Checked;

  SimbaSettings.Editor.FontName.Value := EditorFontFrame.Editor.FontName;
  SimbaSettings.Editor.FontHeight.Value := EditorFontFrame.Editor.Font.Height;
  SimbaSettings.Editor.AntiAliasing.Value := EditorFontFrame.Editor.AntiAliasing;

  SimbaSettings.Editor.AllowCaretPastEOL.Value := EditorGeneralFrame.CaretPastEOLCheckBox.Checked;
  SimbaSettings.Editor.RightMargin.Value := EditorGeneralFrame.RightMarginEdit.Value;
  SimbaSettings.Editor.RightMarginVisible.Value := EditorGeneralFrame.VisibleRightMarginCheckbox.Checked;
  SimbaSettings.Editor.DefaultScriptPath.Value := EditorGeneralFrame.DefaultScriptEdit.Text;
  SimbaSettings.Editor.AutomaticallyOpenAutoCompletion.Value := EditorGeneralFrame.OpenAutoCompletionCheckbox.Checked;
  SimbaSettings.Editor.AutomaticallyShowParameterHints.Value := EditorGeneralFrame.ShowParameterHintsCheckbox.Checked;
  SimbaSettings.Editor.DividerVisible.Value := EditorGeneralFrame.DividerVisibleCheckbox.Checked;
  SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value := EditorGeneralFrame.IgnoreCodeToolsDirectiveCheckbox.Checked;

  SimbaSettings.GUI.CustomToolbarSize.Value := GUIFrame.ToolbarSizeTrackBar.Position;
  SimbaSettings.GUI.CustomFontSize.Value := GUIFrame.FontSizeTrackBar.Position;
end;

constructor TSimbaSettingsForm.Create(AOwner: TComponent);

  function AddPage(Name: String; ParentNode: TTreeNode = nil): TPage;
  var
    Node: TTreeNode;
  begin
    if ParentNode = nil then
      Node := TreeView.Items.Add(nil, Name)
    else
      Node := TreeView.Items.AddChild(ParentNode, Name);

    Result := TPage.Create(Notebook);
    Result.Parent := Notebook;

    Node.Data := Result;
  end;

var
  EditorNode: TTreeNode;
begin
  inherited Create(AOwner);

  Width := 950;
  Height := 650;

  EditorNode := TreeView.Items.Add(nil, 'Editor');

  EnvironmentFrame := TEnvironmentFrame.Create(Self);
  EnvironmentFrame.Parent := AddPage('Environment');
  EnvironmentFrame.Align := alClient;

  EditorGeneralFrame := TEditorGeneralFrame.Create(Self);
  EditorGeneralFrame.Parent := AddPage('General', EditorNode);
  EditorGeneralFrame.Align := alClient;

  EditorFontFrame := TEditorFontFrame.Create(Self);
  EditorFontFrame.Parent := AddPage('Font', EditorNode);
  EditorFontFrame.Align := alClient;

  EditorColorsFrame := TEditorColorsFrame.Create(Self);
  EditorColorsFrame.Parent := AddPage('Colors', EditorNode);
  EditorColorsFrame.Align := alClient;

  GUIFrame := TGUIFrame.Create(Self);
  GUIFrame.Parent := AddPage('GUI');
  GUIFrame.Align := alClient;
end;

initialization
  {$I simba.settingsform.lrs}

end.

