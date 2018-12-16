unit simba.settingsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ButtonPanel, ExtCtrls;

// For form editing: right click > `Show Page` to switch pages.

type
  TSimbaSettingsForm = class(TForm)
    General_ShowSimbaNews: TCheckBox;
    General_SaveScriptOnCompile: TCheckBox;
    General_AutomaticallyCheckForSimbaUpdate: TCheckBox;
    General_TrayIconVisible: TCheckBox;
    Editor_EditColorsButton: TButton;
    Editor_EditFontButton: TButton;
    ButtonPanel: TButtonPanel;
    Editor_DefaultScriptPath: TEdit;
    Editor_HighlighterPath: TEdit;
    Editor_CaretPastEOL: TCheckBox;
    CodeTools_AutomaticallyShowAutoComplete: TCheckBox;
    CodeTools_AutomaticallyShowParameterHints: TCheckBox;
    Editor_ShowSpecialCharacters: TCheckBox;
    Environment_PluginPath: TEdit;
    Environment_IncludePath: TEdit;
    Environment_ScriptPath: TEdit;
    Environment_FontPath: TEdit;
    Environment_PluginPath_Label: TLabel;
    Environment_IncludePath_Label: TLabel;
    Environment_ScriptPath_Label: TLabel;
    Environment_FontPath_Label: TLabel;
    Editor_DefaultScriptPath_Label: TLabel;
    Editor_HighlighterPath_Label: TLabel;
    ColorPickerPage: TPage;
    PageList: TListBox;
    Pages: TNotebook;
    EnvironmentPage: TPage;
    CodeToolsPage: TPage;
    EditorPage: TPage;
    GeneralPage: TPage;
    ColorPicker_ClipboardMode: TRadioGroup;
    procedure Editor_EditColorsButtonClick(Sender: TObject);
    procedure Editor_EditFontButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ChangePage(Sender: TObject; User: Boolean);
    procedure OKButtonClick(Sender: TObject);
  end;

var
  SimbaSettingsForm: TSimbaSettingsForm;

implementation

uses
  simbaunit, simba.settings;

procedure TSimbaSettingsForm.ChangePage(Sender: TObject; User: Boolean);
begin
  case TListBox(Sender).GetSelectedText() of
    'General': GeneralPage.Show();
    'Environment': EnvironmentPage.Show();
    'Code Tools': CodeToolsPage.Show();
    'Editor': EditorPage.Show();
    'Color Picker': ColorPickerPage.Show();
  end;
end;

procedure TSimbaSettingsForm.OKButtonClick(Sender: TObject);
begin
  // General
  SimbaSettings.Form.TrayIconVisible.Value := General_TrayIconVisible.Checked;
  SimbaSettings.General.AutomaticallyCheckForSimbaUpdates.Value := General_AutomaticallyCheckForSimbaUpdate.Checked;
  SimbaSettings.General.SaveScriptOnCompile.Value := General_SaveScriptOnCompile.Checked;
  SimbaSettings.General.ShowSimbaNews.Value := General_ShowSimbaNews.Checked;

  // Environment
  SimbaSettings.Environment.IncludePath.Value := SimbaSettings.Environment.IncludePath.Value;
  SimbaSettings.Environment.PluginPath.Value := SimbaSettings.Environment.PluginPath.Value;
  SimbaSettings.Environment.PluginPath.Value := SimbaSettings.Environment.PluginPath.Value;
  SimbaSettings.Environment.FontPath.Value := SimbaSettings.Environment.FontPath.Value;

  // CodeTools
  SimbaSettings.CodeTools.AutomaticallyShowAutoComplete.Value := CodeTools_AutomaticallyShowAutoComplete.Checked;
  SimbaSettings.CodeTools.AutomaticallyShowParameterHints.Value := CodeTools_AutomaticallyShowParameterHints.Checked;

  // Editor
  SimbaSettings.Editor.DefaultScriptPath.Value := Editor_DefaultScriptPath.Caption;
  SimbaSettings.Editor.HighlighterPath.Value := Editor_HighlighterPath.Caption;
  SimbaSettings.Editor.ShowSpecialCharacters.Value := Editor_ShowSpecialCharacters.Checked;
  SimbaSettings.Editor.CaretPastEOL.Value := Editor_CaretPastEOL.Checked;

  // Color picker
  case ColorPicker_ClipboardMode.ItemIndex of
    0: SimbaSettings.ColorPicker.AddColorToClipBoard.Value := True;
    1: SimbaSettings.ColorPicker.AddCoordinateToClipBoard.Value := True;
    2: SimbaSettings.ColorPicker.AddColorAndCoordinateToClipboard.Value := True;
  end;
end;

procedure TSimbaSettingsForm.FormShow(Sender: TObject);
begin
  // General
  General_TrayIconVisible.Checked := SimbaSettings.Form.TrayIconVisible.Value;
  General_AutomaticallyCheckForSimbaUpdate.Checked := SimbaSettings.General.AutomaticallyCheckForSimbaUpdates.Value;
  General_SaveScriptOnCompile.Checked := SimbaSettings.General.SaveScriptOnCompile.Value;
  General_ShowSimbaNews.Checked := SimbaSettings.General.ShowSimbaNews.Value;

  // Environment
  Environment_IncludePath.Caption := SimbaSettings.Environment.IncludePath.Value;
  Environment_PluginPath.Caption := SimbaSettings.Environment.PluginPath.Value;
  Environment_ScriptPath.Caption := SimbaSettings.Environment.PluginPath.Value;
  Environment_FontPath.Caption := SimbaSettings.Environment.FontPath.Value;

  // CodeTools
  CodeTools_AutomaticallyShowAutoComplete.Checked := SimbaSettings.CodeTools.AutomaticallyShowAutoComplete.Value;
  CodeTools_AutomaticallyShowParameterHints.Checked := SimbaSettings.CodeTools.AutomaticallyShowParameterHints.Value;

  // Editor
  Editor_HighlighterPath.Caption := SimbaSettings.Editor.HighlighterPath.Value;
  Editor_DefaultScriptPath.Caption := SimbaSettings.Editor.DefaultScriptPath.Value;
  Editor_ShowSpecialCharacters.Checked := SimbaSettings.Editor.ShowSpecialCharacters.Value;
  Editor_CaretPastEOL.Checked := SimbaSettings.Editor.CaretPastEOL.Value;

  // Color picker
  if SimbaSettings.ColorPicker.AddColorToClipBoard.Value then
    ColorPicker_ClipboardMode.ItemIndex := 0
  else
  if SimbaSettings.ColorPicker.AddCoordinateToClipBoard.Value then
    ColorPicker_ClipboardMode.ItemIndex := 1
  else
  if SimbaSettings.ColorPicker.AddColorAndCoordinateToClipboard.Value then
    ColorPicker_ClipboardMode.ItemIndex := 2;
end;

procedure TSimbaSettingsForm.Editor_EditColorsButtonClick(Sender: TObject);
begin
  SimbaForm.Editor_EditColors();
end;

procedure TSimbaSettingsForm.Editor_EditFontButtonClick(Sender: TObject);
begin
  SimbaForm.Editor_EditFont();
end;

procedure TSimbaSettingsForm.FormCreate(Sender: TObject);
begin
  PageList.ItemIndex := 0;

  GeneralPage.Show();
end;

initialization
  {$I simba.settingsform.lrs}

end.

