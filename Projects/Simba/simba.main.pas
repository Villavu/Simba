{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Simba: GUI for the Mufasa Macro Library
}
unit simba.main;

{$mode objfpc}{$H+}

{$I Simba.inc}

interface

uses
  classes, sysutils, fileutil, lresources, forms, controls, graphics, dialogs,
  stdctrls, menus, comctrls, extctrls, actnlist, buttons, imglist,
  simba.windowselector, simba.scripttab, simba.oswindow, castaliapaslextypes;

const
  IMAGE_COMPILE             = 0;
  IMAGE_PACKAGE             = 1;
  IMAGE_COPY                = 2;
  IMAGE_CUT                 = 3;
  IMAGE_OPEN                = 4;
  IMAGE_PASTE               = 5;
  IMAGE_SAVE                = 6;
  IMAGE_TRASH               = 7;
  IMAGE_CLOSE               = 8;
  IMAGE_CLOSE_ALL           = 9;
  IMAGE_POWER               = 10;
  IMAGE_NEW_FILE            = 11;
  IMAGE_PAUSE               = 12;
  IMAGE_REDO                = 13;
  IMAGE_PLAY                = 14;
  IMAGE_SAVE_ALL            = 15;
  IMAGE_STOP                = 16;
  IMAGE_UNDO                = 17;
  IMAGE_SEARCH              = 18;
  IMAGE_COLOR_PICKER        = 19;
  IMAGE_SELECT              = 20;
  IMAGE_GEAR                = 21;
  IMAGE_OPEN_RECENT         = 22;
  IMAGE_PACKAGE_NOTIFCATION = 23;
  IMAGE_WRITE_BUG           = 24;
  IMAGE_MINUS               = 25;
  IMAGE_PLUS                = 26;
  IMAGE_DIRECTORY           = 27;
  IMAGE_SIMBA               = 28;
  IMAGE_BOOK                = 29;
  IMAGE_FILE                = 30;
  IMAGE_TYPE                = 31;
  IMAGE_FUNCTION            = 32;
  IMAGE_PROCEDURE           = 33;
  IMAGE_GITHUB              = 34;
  IMAGE_CONSTANT            = 35;
  IMAGE_VARIABLE            = 36;

type
  TScriptButtonState = (ss_None, ss_Running, ss_Paused, ss_Stopping);

  TSimbaForm = class(TForm)
    Images: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemEditor: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemConsole: TMenuItem;
    MenuItemTrayIcon: TMenuItem;
    MenuItem4: TMenuItem;
    MainMenu: TMainMenu;
    MenuItemDTMEditor: TMenuItem;
    MenuEdit: TMenuItem;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemLockLayout: TMenuItem;
    MenuItemOutput: TMenuItem;
    MenuItemResetLayout: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemACA: TMenuItem;
    MenuItemBitmapConv: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseTabs: TMenuItem;
    MenuItemColourHistory: TMenuItem;
    MenuItemCompile: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemDebugImage: TMenuItem;
    MenuItemDivider10: TMenuItem;
    MenuItemDivider11: TMenuItem;
    MenuItemDivider14: TMenuItem;
    MenuItemDivider2: TMenuItem;
    MenuItemDivider3: TMenuItem;
    MenuItemDivider4: TMenuItem;
    MenuItemDivider5: TMenuItem;
    MenuItemDivider6: TMenuItem;
    MenuItemExportHTML: TMenuItem;
    MenuItemFileBrowser: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemFindNext: TMenuItem;
    MenuItemFindPrev: TMenuItem;
    MenuItemFormDesigner: TMenuItem;
    MenuItemFunctionList: TMenuItem;
    MenuItemGoto: TMenuItem;
    MenuItemMainExit: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemNotes: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemOpenRecent: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemPause: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemReportBug: TMenuItem;
    MenuItemRun: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAll: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSaveAsDefault: TMenuItem;
    MenuItemScript: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemSettingsSimpleButton: TMenuItem;
    MenuItemStop: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuTools: TMenuItem;
    MenuView: TMenuItem;
    ScriptProcessorTimer: TTimer;
    ClearOutputButton: TToolButton;
    CompileButton: TToolButton;
    NewButton: TToolButton;
    OpenButton: TToolButton;
    PauseButton: TToolButton;
    ColorPickerButton: TToolButton;
    RunButton: TToolButton;
    SaveButton: TToolButton;
    SaveAllButton: TToolButton;
    SelectTargetButton: TToolButton;
    StopButton: TToolButton;
    ToolBar: TToolBar;
    ButtonSeperator1: TToolButton;
    ButtonSeperator5: TToolButton;
    ButtonSeperator3: TToolButton;
    ButtonSeperator2: TToolButton;
    ButtonSeperator4: TToolButton;
    MenuItemHide: TMenuItem;
    PackageButton: TToolButton;
    TrayDivider: TMenuItem;
    TrayPlay: TMenuItem;
    TrayStop: TMenuItem;
    TrayPause: TMenuItem;
    MenuItemShow: TMenuItem;
    MenuItemExit: TMenuItem;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;

    procedure RecentFileItemsClick(Sender: TObject);
    procedure MenuClearOutputClick(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuSaveAsDefaultClick(Sender: TObject);
    procedure MenuCloseAllTabsClick(Sender: TObject);
    procedure MenuCloseTabClick(Sender: TObject);
    procedure MenuCompileClick(Sender: TObject);

    procedure MenuCopyClick(Sender: TObject);
    procedure MenuCutClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuGotoClick(Sender: TObject);
    procedure MenuNewClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuPauseClick(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure MenuItemFindNextClick(Sender: TObject);
    procedure MenuItemFindPrevClick(Sender: TObject);
    procedure MenuPasteClick(Sender: TObject);
    procedure MenuRedoClick(Sender: TObject);
    procedure MenuRunClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuStopClick(Sender: TObject);
    procedure MenuReplaceClick(Sender: TObject);
    procedure MenuSelectAllClick(Sender: TObject);
    procedure MenuUndoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormResizeASync(Data: PtrInt);
    procedure HandleResetLayoutClick(Sender: TObject);
    procedure MenuItemConsoleClick(Sender: TObject);
    procedure MenuItemLockLayoutClick(Sender: TObject);
    procedure MenuItemTrayIconClick(Sender: TObject);
    procedure PackageButtonClick(Sender: TObject);
    procedure SaveAllButtonClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure ShowDTMEditor(Sender: TObject);
    procedure ShowFormDesigner(Sender: TObject);
    procedure HandleRunningScripts(Sender: TObject);
    procedure MenuItemBitmapConvClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure FormDestroy(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseTabsClick(Sender: TObject);
    procedure MenuItemExportHTMLClick(Sender: TObject);
    procedure MenuItemHideClick(Sender: TObject);
    procedure MenuItemReportBugClick(Sender: TObject);
    procedure MenuItemSettingsSimpleButtonClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ButtonTrayClick(Sender: TObject);
    procedure ShowACA(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
    procedure SetScriptState(const State: TScriptButtonState);
    procedure CodeTools_OnMessage(Sender: TObject; const Typ: TMessageEventType; const Message: String; X, Y: Integer);
    procedure CodeTools_OnLoadLibrary(Sender: TObject; FileName: String; var Contents: String);

    function CodeTools_OnFindInclude(Sender: TObject; var FileName: String): Boolean;
    function CodeTools_OnFindLibrary(Sender: TObject; var FileName: String): Boolean;

    procedure HandleMenuViewClick(Sender: TObject);
    procedure ResetDockingSplitters(Data: PtrInt);

    procedure SettingChanged_CustomToolbarSize(Value: Int64);
    procedure SettingChanged_CustomFontSize(Value: Int64);
    procedure SettingChanged_LayoutLocked(Value: Boolean);
    procedure SettingChanged_ConsoleVisible(Value: Boolean);
    procedure SettingChanged_TrayIconVisible(Value: Boolean);
  protected
    FWindowSelection: TOSWindow;
    FScriptState: TScriptButtonState;

    procedure RemoveTabASync(Data: PtrInt);

    procedure PrintDTM(constref DTM: String);

    procedure CustomExceptionHandler(Sender: TObject; E: Exception);

    function LoadLayout: Boolean;
    procedure SaveLayout;
    procedure SetupLayout(Reset: Boolean = False);
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: Boolean); override;
  public
    Initialized: Boolean;

    property WindowSelection: TOSWindow read FWindowSelection;

    procedure RunScript;
    procedure CompileScript;
    procedure PauseScript;
    procedure StopScript;

    procedure AddRecentFile(const FileName: String);

    procedure AddSettingChangeHandlers;
    procedure RemoveSettingChangeHandlers;

    procedure Initialize(Data: PtrInt);
    procedure Initialize_Console;
    procedure Initialize_Docking;
    procedure Initialize_Settings;
    procedure Initialize_CodeTools;
    procedure Initialize_SimbaScript;
    procedure Initialize_OpenSSL;
    procedure Initialize_CommandLineOptions;

    procedure ShowForm(Form: TForm);
    procedure HideForm(Form: TForm);
    procedure CenterForm(Form: TForm);
  end;

var
  SimbaForm: TSimbaForm;

implementation

uses
  lclintf, synexporthtml, anchordocking, xmlconf, xmlpropstorage, anchordockstorage,
  simba.mufasatypes, simba.misc, simba.mufasabase, simba.settings, simba.httpclient,
  simba.files, simba.resourceextractor, simba.codeparser, simba.codeinsight,
  simba.debugimage, simba.bitmapconv, simba.colorpicker_historyform, simba.aca,
  simba.dtmeditor, simba.scriptinstance, simba.package_form, simba.aboutform,
  simba.functionlistform, simba.scripttabsform, simba.debugform, simba.filebrowserform,
  simba.notesform, simba.settingsform, simba.colorpicker, simba.ci_includecache,
  simba.highlighter, simba.scriptpluginloader
  {$IFDEF WINDOWS},
  windows
  {$ENDIF}
  {$IFDEF USE_FORMDESIGNER},
  simba.formdesigner
  {$ENDIF};

type
  TSimbaAnchorDockHeader = class(TAnchorDockHeader)
  protected
    procedure Paint; override;

    procedure SetAlign(Value: TAlign); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaAnchorDockHostSite = class(TAnchorDockHostSite)
  protected
    FMenuItem: TMenuItem;

    procedure MenuItemDestroyed(Sender: TObject);

    function GetFormCount: Int32;

    procedure SetMenuItem(Value: TMenuItem);
    procedure SetVisible(Value: Boolean); override;
    procedure SetParent(Value: TWinControl); override;
  public
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property FormCount: Int32 read GetFormCount;

    procedure UpdateDockCaption(Exclude: TControl = nil); override;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

  TSimbaAnchorDockSplitter = class(TAnchorDockSplitter)
  protected
    procedure SetParent(Value: TWinControl); override;
    procedure Paint; override;
  end;

procedure TSimbaAnchorDockHeader.Paint;
var
  Style: TTextStyle;
begin
  Style := Canvas.TextStyle;
  Style.Layout := tlCenter;
  Style.Alignment := taCenter;

  Font := SimbaForm.Font;

  Canvas.TextRect(ClientRect, 0, 0, Self.Caption, Style);
end;

procedure TSimbaAnchorDockHeader.SetAlign(Value: TAlign);
begin
  inherited SetAlign(alTop);
end;

constructor TSimbaAnchorDockHeader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  BevelWidth := 3;
  PopupMenu := nil;
  Color := clForm;

  CloseButton.Parent := nil;
end;

procedure TSimbaAnchorDockHostSite.MenuItemDestroyed(Sender: TObject);
begin
  FMenuItem := nil;
end;

function TSimbaAnchorDockHostSite.GetFormCount: Int32;
var
  I: Int32;
begin
  Result := 0;

  for I := 0 to ControlCount - 1 do
    if Controls[I] is TForm then
      Result := Result + 1;
end;

procedure TSimbaAnchorDockHostSite.SetMenuItem(Value: TMenuItem);
begin
  FMenuItem := Value;
  FMenuItem.AddHandlerOnDestroy(@MenuItemDestroyed);
end;

procedure TSimbaAnchorDockHostSite.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value and SimbaForm.Initialized);

  if (MenuItem <> nil) then
    MenuItem.Checked := Value;

  if (Parent <> nil) then
    ShowInTaskBar := stNever
  else
    ShowInTaskBar := stAlways;
end;

procedure TSimbaAnchorDockHostSite.SetParent(Value: TWinControl);
begin
  if (Value <> nil) then
    ShowInTaskBar := stNever
  else
    ShowInTaskBar := stAlways;

  inherited SetParent(Value);
end;

procedure TSimbaAnchorDockHostSite.UpdateDockCaption(Exclude: TControl);
var
  I: Int32;
begin
  inherited UpdateDockCaption(Exclude);

  if FormCount = 1 then
  begin
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TForm then
        Caption := Controls[i].Caption;
  end else
    Caption := 'Simba';
end;

constructor TSimbaAnchorDockHostSite.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);

  DefaultMonitor := dmMainForm;
end;

procedure TSimbaAnchorDockSplitter.SetParent(Value: TWinControl);
begin
  if (Value = SimbaForm) then
    inherited SetParent(nil)
  else
    inherited SetParent(Value);
end;

procedure TSimbaAnchorDockSplitter.Paint;
var
  Center: Int32;
begin
  Canvas.Brush.Color := clForm;
  Canvas.FillRect(ClientRect);
  Canvas.Brush.Color := cl3DShadow;

  if ResizeAnchor in [akLeft, akRight] then // vertical
  begin
    Center := Width div 2;
    Canvas.FillRect(Center - 2, Height div 2 - 50, Center + 1, Height div 2 + 50);
  end else
  begin
    Center := Height div 2;
    Canvas.FillRect(Width div 2 - 50, Center - 2, Width div 2 + 50, Center + 1);
  end;
end;

procedure TSimbaForm.CustomExceptionHandler(Sender: TObject; E: Exception);

  function DumpExceptionCallStack(E: Exception): string;
  var
    I: Integer;
    Frames: PPointer;
    Report: string;
  begin
    Report := 'Program exception! ' + LineEnding +
      'Stacktrace:' + LineEnding + LineEnding;
    if E <> nil then begin
      Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
      'Message: ' + E.Message + LineEnding;
    end;
    Report := Report + BackTraceStrFunc(ExceptAddr);
    Frames := ExceptFrames;
    for I := 0 to ExceptFrameCount - 1 do
      Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

    result := report;
  end;

var
  Trace, LogName: string;
  MsgDlgRet: Integer;

begin
  WriteLn('');
  WriteLn('Something went wrong...');
  WriteLn('');

  Trace := DumpExceptionCallStack(E);
  Trace += LineEnding + 'Simba Version: ' + IntToStr(SimbaVersion) + LineEnding;

  LogName := SimbaSettings.Environment.DataPath.Value + 'ErrorLog_' + FormatDateTime('dd-mm-yy_hh-nn-ss', Now) + '.txt';
  WriteLn(Format('Going to try to save the error information to "%s".', [LogName]));

  try
    with TFileStream.Create(LogName, fmOpenWrite or fmOpenReadWrite or fmCreate) do
    try
      Write(Trace[1], Length(Trace));
    finally
      Free;
    end;
  except
    WriteLn(Format('Unable to save log file! [%s]', [LogName]));
  end;

  MsgDlgRet := mrOk;
  Application.DisableIdleHandler;
  try
     MsgDlgRet := MessageDlg('Something went wrong in Simba. ' +
     'If you press OK, Simba will try to save your scripts and then close. (Recommended) ' +
     'See ' + LogName + ' for more information.' , mtError, mbOKCancel, 0);
  finally
    Application.EnableIdleHandler;
  end;

  if MsgDlgRet = mrOK then
  begin
    try
     // Self.CloseTabs; // Save scripts
      SimbaForm.Free;
    finally
      WriteLn('Finally Free...');
    end;

    { Stop Simba }
    Halt(1); // Error code 1
  end;
end;

procedure TSimbaForm.CodeTools_OnMessage(Sender: TObject; const Typ: TMessageEventType; const Message: string; X, Y: Integer);
var
  Parser: TCodeParser absolute Sender;
begin
  if (Parser.Lexer.TokenPos + Parser.Lexer.TokenLen < Parser.Lexer.CaretPos) then
  begin
    SimbaDebugForm.Add('Simba''s code parser encountered an error. This could break code tools:');

    if Parser.Lexer.FileName <> '' then
      SimbaDebugForm.Add(Format('"%s" at line %d, column %d in file "%s"', [Message, Y, X, Parser.Lexer.FileName]))
    else
      SimbaDebugForm.Add(Format('"%s" at line %d, column %d', [Message, Y, X]));
  end;
end;

function TSimbaForm.CodeTools_OnFindInclude(Sender: TObject; var FileName: String): Boolean;
begin
  Result := FindFile(FileName, [ExtractFileDir(TCodeParser(Sender).Lexer.FileName), SimbaSettings.Environment.IncludePath.Value, Application.Location]);
end;

function TSimbaForm.CodeTools_OnFindLibrary(Sender: TObject; var FileName: String): Boolean;
begin
  Result := TSimbaScriptPluginLoader.FindFile(FileName, [ExtractFileDir(TCodeParser(Sender).Lexer.FileName), SimbaSettings.Environment.PluginPath.Value, Application.Location]);
end;

procedure TSimbaForm.CodeTools_OnLoadLibrary(Sender: TObject; FileName: String; var Contents: String);
var
  Plugin: TSimbaScriptPluginLoader;
begin
  Plugin := TSimbaScriptPluginLoader.Create(FileName);

  if (Plugin <> nil) then
  try
    Contents := Plugin.Dump;
  finally
    Plugin.Free();
  end;
end;

procedure TSimbaForm.RecentFileItemsClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Open(TMenuItem(Sender).Caption, True);
end;

procedure TSimbaForm.ShowACA(Sender: TObject);
begin
  with TSimbaACAForm.Create(WindowSelection) do
  begin
    Font := Self.Font;
    ShowOnTop();
  end;
end;

procedure TSimbaForm.TrayPopupPopup(Sender: TObject);
begin
  { XXX: What's up with this? }
  MenuItemHide.enabled:= SimbaForm.Visible;
  {$ifdef MSWindows}
  MenuItemShow.Enabled:= not SimbaForm.Visible;
  if SimbaForm.Visible then
    if SimbaForm.CanFocus then
      SimbaForm.SetFocus;
  {$endif}
end;

procedure TSimbaForm.RunScript;
begin
  with SimbaScriptTabsForm.CurrentTab do
  try
    if (ScriptInstance = nil) then
    begin
      if FileName <> '' then
        Save(FileName);

      ScriptInstance := TSimbaScriptInstance.Create();
      ScriptInstance.ManageOutput := True;
      ScriptInstance.TargetWindow := Self.WindowSelection;
      ScriptInstance.ScriptName := ScriptName;

      if (FileName <> '') then
        ScriptInstance.ScriptFile := FileName
      else
        ScriptInstance.Script := Script;

      ScriptInstance.Run();
    end else
      ScriptInstance.Resume();
  except
    on e: Exception do
      MessageDlg('Run Script Error: ' + e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TSimbaForm.CompileScript;
begin
  with SimbaScriptTabsForm.CurrentTab do
  try
    if (ScriptInstance = nil) then
    begin
      if (FileName <> '') then
        Save(FileName);

      ScriptInstance := TSimbaScriptInstance.Create();
      ScriptInstance.ManageOutput := True;
      ScriptInstance.TargetWindow := Self.WindowSelection;
      ScriptInstance.ScriptName := ScriptName;

      if (FileName <> '') then
        ScriptInstance.ScriptFile := FileName
      else
        ScriptInstance.Script := Script;

      ScriptInstance.Compile();
    end;
  except
    on e: Exception do
      MessageDlg('Compile Script Error: ' + e.Message, mtError, [mbOK], 0);
  end;
end;

procedure TSimbaForm.PauseScript;
begin
  with SimbaScriptTabsForm.CurrentTab do
    if (ScriptInstance <> nil) then
      ScriptInstance.Pause();
end;

procedure TSimbaForm.StopScript;
begin
  with SimbaScriptTabsForm.CurrentTab do
   if (ScriptInstance <> nil) then
   begin
     if ScriptInstance.IsStopping then
       ScriptInstance.Kill()
     else
       ScriptInstance.Stop();
   end;
end;

procedure TSimbaForm.AddRecentFile(const FileName: String);
var
  i: Int32;
  Item: TMenuItem;
begin
  if FileExists(FileName) then
  begin
    for i := 0 to MenuItemOpenRecent.Count - 1 do
      if SameFileName(MenuItemOpenRecent[i].Caption, FileName) then
      begin
        MenuItemOpenRecent.Delete(i);

        Break;
      end;

    while MenuItemOpenRecent.Count > 10 do
      MenuItemOpenRecent.Delete(MenuItemOpenRecent.Count - 1);

    Item := TMenuItem.Create(MenuItemOpenRecent);
    Item.OnClick := @RecentFileItemsClick;
    Item.Caption := FileName;

    MenuItemOpenRecent.Insert(0, Item);
  end;
end;

procedure TSimbaForm.MenuCloseTabClick(Sender: TObject);
begin
  Application.QueueAsyncCall(@RemoveTabASync, 0);
end;

procedure TSimbaForm.MenuCompileClick(Sender: TObject);
begin
  Self.CompileScript();
end;

{$IFDEF WINDOWS}
function ConsoleHandler(Event: DWord): WINBOOL; stdcall;
begin
  Result := True;

  TThread.Synchronize(nil, @SimbaForm.Close);
end;
{$ENDIF}

procedure TSimbaForm.MenuCopyClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    SimbaScriptTabsForm.CurrentEditor.CopyToClipboard();
end;

procedure TSimbaForm.MenuCutClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    SimbaScriptTabsForm.CurrentEditor.CutToClipboard();
end;

procedure TSimbaForm.MenuExitClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TSimbaForm.MenuGotoClick(Sender: TObject);
var
  Value: String;
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
  begin
    Value := '';
    if InputQuery('Goto line', 'Goto line:', Value) and (StrToIntDef(Value, -1) > -1) then
      SimbaScriptTabsForm.CurrentEditor.TopLine := StrToInt(Value) - (SimbaScriptTabsForm.CurrentEditor.LinesInWindow div 2);
  end;
end;

procedure TSimbaForm.MenuClearOutputClick(Sender: TObject);
begin
  SimbaDebugForm.Clear();
end;

procedure TSimbaForm.MenuFileClick(Sender: TObject);
begin
  MenuItemSaveAll.Enabled := SimbaScriptTabsForm.TabCount > 1;
end;

procedure TSimbaForm.MenuSaveAsDefaultClick(Sender: TObject);
begin
  if MessageDlg('Save Default Script', 'Are you sure you want to overwrite the default script?', mtConfirmation, [mbYes, mbCancel], 0) = mrYes then
  try
    SimbaScriptTabsForm.CurrentEditor.Lines.SaveToFile(SimbaSettings.Editor.DefaultScriptPath.Value);
  except
    on E: Exception do
      ShowMessage('Exception while saving default script: ' + E.Message);
  end;
end;

procedure TSimbaForm.MenuCloseAllTabsClick(Sender: TObject);
var
  Aborted: Boolean;
begin
  SimbaScriptTabsForm.RemoveAllTabs(Aborted);
end;

procedure TSimbaForm.MenuNewClick(Sender: TObject);
begin
  SimbaScriptTabsForm.AddTab();
end;

procedure TSimbaForm.HandleMenuViewClick(Sender: TObject);

  procedure Toggle(Item: TMenuItem; Form: TForm);
  begin
    if Item.Checked then
    begin
      Self.ShowForm(Form);
      Self.CenterForm(Form);
    end else
      Self.HideForm(Form);
  end;

begin
  if (Sender = MenuItemFileBrowser)   then Toggle(MenuItemFileBrowser, SimbaFileBrowserForm);
  if (Sender = MenuItemNotes)         then Toggle(MenuItemNotes, SimbaNotesForm);
  if (Sender = MenuItemFunctionList)  then Toggle(MenuItemFunctionList, SimbaFunctionListForm);
  if (Sender = MenuItemDebugImage)    then Toggle(MenuItemDebugImage, SimbaDebugImageForm);
  if (Sender = MenuItemOutput)        then Toggle(MenuItemOutput, SimbaDebugForm);
  if (Sender = MenuItemEditor)        then Toggle(MenuItemEditor, SimbaScriptTabsForm);
  if (Sender = MenuItemColourHistory) then Toggle(MenuItemColourHistory, SimbaColorHistoryForm);
end;

procedure TSimbaForm.ResetDockingSplitters(Data: PtrInt);
begin
  DockMaster.ResetSplitters();
end;

procedure TSimbaForm.SettingChanged_CustomToolbarSize(Value: Int64);
begin
  if Value = 0 then
  begin
    Value := Images.WidthForPPI[Images.Width, ToolBar.Font.PixelsPerInch];
    if Screen.PixelsPerInch = 96 then
      Value := 24;
  end;

  Self.ToolBar.ImagesWidth := Value;
  Self.Toolbar.ButtonWidth := Value + 8;
  Self.Toolbar.ButtonHeight := Value + 8;

  Application.QueueASyncCall(@FormResizeASync, 0);
end;

procedure TSimbaForm.SettingChanged_CustomFontSize(Value: Int64);
var
  i: Int32;
begin
  if Value = 0 then
  begin
    Font.SetDefault();

    Value := Round((-Graphics.GetFontData(Font.Reference.Handle).Height * 72 / Font.PixelsPerInch)) + 1;
  end;

  for i := 0 to Screen.CustomFormCount - 1 do
    Screen.CustomForms[i].Font.Size := Value;

  Screen.HintFont.Size := Value;
end;

procedure TSimbaForm.SettingChanged_LayoutLocked(Value: Boolean);
begin
  MenuItemLockLayout.Checked := Value;

  DockMaster.ShowHeader := not Value;
  DockMaster.AllowDragging := not Value;

  Application.QueueAsyncCall(@FormResizeASync, 0);
end;

procedure TSimbaForm.SettingChanged_ConsoleVisible(Value: Boolean);
var
  PID: UInt32;
begin
  MenuItemConsole.Checked := Value;

  {$IFDEF WINDOWS}
  GetWindowThreadProcessId(GetConsoleWindow(), PID);

  if (PID = GetCurrentProcessID()) then
  begin
    case Value of
      True: ShowWindow(GetConsoleWindow(), SW_SHOWNORMAL);
      False: ShowWindow(GetConsoleWindow(), SW_HIDE);
    end;
  end;
  {$ENDIF}
end;

procedure TSimbaForm.SettingChanged_TrayIconVisible(Value: Boolean);
begin
  MenuItemTrayIcon.Checked := Value;

  TrayIcon.Visible := Value;
end;

type
  TToolBar_Helper = class helper for TToolBar
    function Size: TPoint;
  end;

function TToolBar_Helper.Size: TPoint;
begin
  CalculatePreferredSize(Result.X, Result.Y, True);
end;

procedure TSimbaForm.RemoveTabASync(Data: PtrInt);
var
  Aborted: Boolean;
begin
  SimbaScriptTabsForm.RemoveTab(SimbaScriptTabsForm.CurrentTab, Aborted);
end;

procedure TSimbaForm.PrintDTM(constref DTM: String);
begin
  SimbaDebugForm.Add('DTM := DTMFromString(' + #39 + DTM + #39 + ');');
end;

procedure TSimbaForm.SetVisible(Value: Boolean);
begin
  if (not Value) or (not FormIsUpdating) then
    inherited SetVisible(Value);
end;

procedure TSimbaForm.MenuOpenClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Open();
end;

procedure TSimbaForm.MenuPauseClick(Sender: TObject);
begin
  Self.PauseScript();
end;

procedure TSimbaForm.MenuFindClick(Sender: TObject);
begin
  SimbaScriptTabsForm.FindVisible := True;
end;

procedure TSimbaForm.MenuItemFindNextClick(Sender: TObject);
begin
  SimbaScriptTabsForm.FindNext();
end;

procedure TSimbaForm.MenuItemFindPrevClick(Sender: TObject);
begin
  SimbaScriptTabsForm.FindPrevious();
end;

procedure TSimbaForm.MenuReplaceClick(Sender: TObject);
begin
  SimbaScriptTabsForm.Replace();
end;

procedure TSimbaForm.MenuPasteClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    SimbaScriptTabsForm.CurrentEditor.PasteFromClipboard();
end;

procedure TSimbaForm.MenuRedoClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    SimbaScriptTabsForm.CurrentEditor.Redo();
end;

procedure TSimbaForm.MenuRunClick(Sender: TObject);
begin
  Self.RunScript();
end;

procedure TSimbaForm.MenuSaveClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentTab <> nil then
  begin
    if SimbaScriptTabsForm.CurrentTab.FileName = '' then
      SimbaScriptTabsForm.CurrentTab.SaveAs()
    else
      SimbaScriptTabsForm.CurrentTab.Save(SimbaScriptTabsForm.CurrentTab.FileName);
  end;
end;

procedure TSimbaForm.MenuStopClick(Sender: TObject);
begin
  Self.StopScript();
end;

procedure TSimbaForm.MenuSelectAllClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    SimbaScriptTabsForm.CurrentEditor.SelectAll();
end;

procedure TSimbaForm.MenuUndoClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    SimbaScriptTabsForm.CurrentEditor.Undo();
end;

procedure TSimbaForm.HandleResetLayoutClick(Sender: TObject);
begin
  SetupLayout(True);
end;

procedure TSimbaForm.MenuItemConsoleClick(Sender: TObject);
begin
  SimbaSettings.GUI.ConsoleVisible.Value := not SimbaSettings.GUI.ConsoleVisible.Value;
end;

procedure TSimbaForm.MenuItemLockLayoutClick(Sender: TObject);
begin
  SimbaSettings.GUI.LayoutLocked.Value := not SimbaSettings.GUI.LayoutLocked.Value;
end;

procedure TSimbaForm.MenuItemTrayIconClick(Sender: TObject);
begin
  SimbaSettings.GUI.TrayIconVisible.Value := not SimbaSettings.GUI.TrayIconVisible.Value;
end;

procedure TSimbaForm.PackageButtonClick(Sender: TObject);
begin
  SimbaPackageForm.Show();
end;

procedure TSimbaForm.SaveAllButtonClick(Sender: TObject);
var
  I: Int32;
begin
  for I := SimbaScriptTabsForm.TabCount - 1 downto 0 do
    if SimbaScriptTabsForm.Tabs[I].ScriptChanged then
    begin
      if (SimbaScriptTabsForm.Tabs[I].FileName = '') then
      begin
        SimbaScriptTabsForm.Tabs[I].MakeVisible();
        SimbaScriptTabsForm.Tabs[I].SaveAs();
      end else
        SimbaScriptTabsForm.Tabs[I].Save(SimbaScriptTabsForm.Tabs[I].FileName);
    end;
end;

procedure TSimbaForm.MenuSaveAsClick(Sender: TObject);
begin
  if (SimbaScriptTabsForm.CurrentTab <> nil) and SimbaScriptTabsForm.CurrentTab.ScriptChanged then
    SimbaScriptTabsForm.CurrentTab.SaveAs();
end;

procedure TSimbaForm.ShowDTMEditor(Sender: TObject);
begin
  with TSimbaDTMEditorForm.Create(WindowSelection) do
  begin
    Font := Self.Font;
    OnPrintDTM := @PrintDTM;
    ShowOnTop();
  end;
end;

procedure TSimbaForm.ShowFormDesigner(Sender: TObject);
begin
  {$IFDEF USE_FORMDESIGNER}
  CompForm.ShowOnTop();
  {$ENDIF}
end;

procedure TSimbaForm.HandleRunningScripts(Sender: TObject);
var
  i: Int32;
  Tab: TSimbaScriptTab;
  Output: TStringList;
begin
  Output := TStringList.Create();

  for i := 0 to SimbaScriptTabsForm.TabCount - 1 do
  begin
    with SimbaScriptTabsForm.Tabs[i] do
    begin
      if ScriptInstance <> nil then
      begin
        if (not ScriptInstance.IsRunning) then
        begin
          if ScriptInstance.ExitCode > 0 then
            Output.Add('Script process didn''t exit cleanly. Exit code: ' + IntToStr(ScriptInstance.ExitCode));

          ScriptInstance.Free();
          ScriptInstance := nil;
        end;
      end;
    end;
  end;

  // Update buttons
  Tab := SimbaScriptTabsForm.CurrentTab;
  if (Tab <> nil) then
  begin
    if (Tab.ScriptInstance <> nil) and Tab.ScriptInstance.IsRunning then
    begin
      if Tab.ScriptInstance.IsStopping then
      begin
        SimbaScriptTabsForm.StatusPanelState.Caption := 'Stopping';
        SetScriptState(ss_Stopping);
      end
      else
      if Tab.ScriptInstance.IsPaused then
      begin
        SimbaScriptTabsForm.StatusPanelState.Caption := 'Paused';
        SetScriptState(ss_Paused);
      end
      else
      begin
        SetScriptState(ss_Running);
        SimbaScriptTabsForm.StatusPanelState.Caption := TimeStamp(Tab.ScriptInstance.TimeRunning);
      end;
    end else
    begin
      SimbaScriptTabsForm.StatusPanelState.Caption := 'Stopped';
      SetScriptState(ss_None);
    end;
  end;

  Output.Free();
end;

procedure TSimbaForm.MenuItemBitmapConvClick(Sender: TObject);
begin
  SimbaBitmapConversionForm.Show();
end;

procedure TSimbaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Int32;
  Aborted: Boolean;
begin
  SimbaScriptTabsForm.RemoveAllTabs(Aborted);

  if Aborted then
    CloseAction := caNone
  else
  begin
    CloseAction := caFree;

    SimbaSettings.GUI.RecentFiles.Value := '';
    for i := MenuItemOpenRecent.Count - 1 downto 0 do
      SimbaSettings.GUI.RecentFiles.Value := SimbaSettings.GUI.RecentFiles.Value + ',' + MenuItemOpenRecent[i].Caption;

    SaveLayout();
  end;
end;

procedure TSimbaForm.FormResize(Sender: TObject);
var
  i: Int32;
  R: TRect;
begin
  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i] is TAnchorDockHostSite then
    begin
      R := TAnchorDockHostSite(Controls[i]).BoundsRect;
      R.Top := ToolBar.Size.Y;
      if SimbaSettings.GUI.LayoutLocked.Value then
        R.Top := R.Top - 2;

      TAnchorDockHostSite(Controls[i]).BoundsRect := R;
    end;
  end;
end;

procedure TSimbaForm.FormResizeASync(Data: PtrInt);
begin
  FormResize(nil);
end;

procedure TSimbaForm.AddSettingChangeHandlers;
begin
  SimbaSettings.GUI.TrayIconVisible.AddHandlerOnChange(@SettingChanged_TrayIconVisible);
  SimbaSettings.GUI.ConsoleVisible.AddHandlerOnChange(@SettingChanged_ConsoleVisible);
  SimbaSettings.GUI.LayoutLocked.AddHandlerOnChange(@SettingChanged_LayoutLocked);
  SimbaSettings.GUI.CustomFontSize.AddHandlerOnChange(@SettingChanged_CustomFontSize);
  SimbaSettings.GUI.CustomToolbarSize.AddHandlerOnChange(@SettingChanged_CustomToolbarSize);
end;

procedure TSimbaForm.RemoveSettingChangeHandlers;
begin
  SimbaSettings.GUI.TrayIconVisible.RemoveHandlerOnChange(@SettingChanged_TrayIconVisible);
  SimbaSettings.GUI.ConsoleVisible.RemoveHandlerOnChange(@SettingChanged_ConsoleVisible);
  SimbaSettings.GUI.LayoutLocked.RemoveHandlerOnChange(@SettingChanged_LayoutLocked);
  SimbaSettings.GUI.CustomFontSize.RemoveHandlerOnChange(@SettingChanged_CustomFontSize);
  SimbaSettings.GUI.CustomToolbarSize.RemoveHandlerOnChange(@SettingChanged_CustomToolbarSize);
end;

procedure TSimbaForm.Initialize_Console;
{$IFDEF WINDOWS}
var
  Mode: UInt32 = 0;
{$ENDIF}
begin
  WriteLn('Initialize Console');

  {$IFDEF WINDOWS}
  // Close Simba when console is closed
  SetConsoleCtrlHandler(@ConsoleHandler, True);

  // Disable "Features" that can freeze Simba
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode and not (ENABLE_QUICK_EDIT_MODE or ENABLE_INSERT_MODE));
  {$ENDIF}
end;

procedure TSimbaForm.Initialize_Docking;
begin
  WriteLn('Initialize Docking');

  DockMaster.HeaderClass := TSimbaAnchorDockHeader;
  DockMaster.SplitterClass := TSimbaAnchorDockSplitter;
  DockMaster.SiteClass := TSimbaAnchorDockHostSite;
  DockMaster.HideHeaderCaptionFloatingControl := False;
  DockMaster.HeaderAlignTop := $FFFFFF;
  DockMaster.PageAreaInPercent := 0;
  DockMaster.SplitterWidth := 8;
  DockMaster.HeaderHint := 'Use the mouse to drag and dock this window';
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);

  DockMaster.MakeDockable(SimbaScriptTabsForm, False);
  DockMaster.MakeDockable(SimbaDebugForm, False);
  DockMaster.MakeDockable(SimbaFileBrowserForm, False);
  DockMaster.MakeDockable(SimbaFunctionListForm, False);
  DockMaster.MakeDockable(SimbaNotesForm, False);
  DockMaster.MakeDockable(SimbaDebugImageForm, False);
  DockMaster.MakeDockable(SimbaColorHistoryForm, False);

  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaDebugForm)).MenuItem := MenuItemOutput;
  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaFileBrowserForm)).MenuItem := MenuItemFileBrowser;
  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaFunctionListForm)).MenuItem := MenuItemFunctionList;
  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaNotesForm)).MenuItem := MenuItemNotes;
  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaDebugImageForm)).MenuItem := MenuItemDebugImage;
  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaScriptTabsForm)).MenuItem := MenuItemEditor;
  TSimbaAnchorDockHostSite(DockMaster.GetAnchorSite(SimbaColorHistoryForm)).MenuItem := MenuItemColourHistory;

  SetupLayout(False);
end;

procedure TSimbaForm.Initialize_Settings;
var
  RecentFile: String;
begin
  WriteLn('Initialize Settings');

  for RecentFile in SimbaSettings.GUI.RecentFiles.Value.Split([',']) do
    AddRecentFile(RecentFile);

  AddSettingChangeHandlers();
end;

procedure TSimbaForm.Initialize_CodeTools;
var
  ScriptInstance: TSimbaScriptInstance;
  Parser: TCodeInsight_Include;
  i: Int32;
begin
  WriteLn('Initialize Code Tools');

  ScriptInstance := nil;

  try
    ScriptInstance := TSimbaScriptInstance.Create();
    ScriptInstance.Dump();

    for i := 0 to ScriptInstance.Output.Count - 1 do
    begin
      Parser := TCodeInsight_Include.Create();
      Parser.OnMessage := @Self.CodeTools_OnMessage;
      Parser.Run(ScriptInstance.Output.ValueFromIndex[i], ScriptInstance.Output.Names[i]);

      if (Parser.Lexer.FileName <> 'Classes') then
        SimbaFunctionListForm.addDeclarations(Parser.Items, SimbaFunctionListForm.addSimbaSection(Parser.Lexer.FileName), False, True, False);

      TCodeInsight.AddBaseInclude(Parser);
    end;

    SimbaFunctionListForm.SimbaNode.AlphaSort();
    SimbaFunctionListForm.SimbaNode.Expanded := True;
  except
    on E: Exception do
      SimbaDebugForm.Add('Error parsing internals: ' + E.Message);
  end;

  if (ScriptInstance <> nil) then
    ScriptInstance.Free();
end;

procedure TSimbaForm.Initialize_SimbaScript;
begin
  WriteLn('Initialize SimbaScript');

  if SimbaSettings.Resources.ExtractSimbaScript.Value then
    SimbaResourceExtractor.Extract('SIMBASCRIPT', Application.Location);
end;

procedure TSimbaForm.Initialize_OpenSSL;
begin
  Writeln('Initialize OpenSSL');

  if SimbaSettings.Resources.ExtractOpenSSL.Value then
    SimbaResourceExtractor.Extract('OPENSSL', Application.Location);

  if SimbaSettings.Resources.InitializeOpenSSL.Value then
    InitializeOpenSSL(Application.Location);
end;

procedure TSimbaForm.ShowForm(Form: TForm);
begin
  if DockMaster.GetAnchorSite(Form) = nil then
    raise Exception.Create('Not a Simba form');

  DockMaster.GetAnchorSite(Form).ShowOnTop();
end;

procedure TSimbaForm.HideForm(Form: TForm);
begin
  if DockMaster.GetAnchorSite(Form) = nil then
    raise Exception.Create('Not a Simba form');

  DockMaster.GetAnchorSite(Form).Header.CloseButton.Click();
end;

procedure TSimbaForm.Initialize_CommandLineOptions;
begin
  if (Application.ParamCount = 2) then
  begin
    WriteLn('Initialize Command Line Options');

    if not FileExists(Application.Params[2]) then
    begin
      WriteLn('Script does not exist!');
      Halt(0);
    end;

    SimbaScriptTabsForm.Open(Application.Params[2]);

    if Application.HasOption('c', 'compile') then
      Self.CompileScript();
    if Application.HasOption('r', 'run') then
      Self.RunScript();
  end;
end;

procedure TSimbaForm.CenterForm(Form: TForm);
var
  W, H, X, Y: Int32;
begin
  if DockMaster.GetAnchorSite(Form) = nil then
    raise Exception.Create('Not a Simba form');

  W := DockMaster.GetAnchorSite(Form).Width;
  H := DockMaster.GetAnchorSite(Form).Height;
  X := (Monitor.Width - W) div 2;
  Y := (Monitor.Height - H) div 2;

  DockMaster.GetAnchorSite(Form).SetBounds(X, Y, W, H);
end;

procedure TSimbaForm.Initialize(Data: PtrInt);
var
  I: Int32;
  Site: TSimbaAnchorDockHostSite;
begin
  Initialize_Console();
  Initialize_Docking();
  Initialize_OpenSSL();
  Initialize_SimbaScript();
  Initialize_CodeTools();
  Initialize_SimbaScript();
  Initialize_Settings();
  Initialize_CommandLineOptions();

  Self.ToolBar.Images := TImageList.Create(ToolBar);
  Self.ToolBar.Images.Assign(Self.Images);
  Self.ToolBar.Images.Scaled := False;

  Application.OnException := @CustomExceptionHandler;
  Application.QueueAsyncCall(@ResetDockingSplitters, 0);

  // Everything should be loaded. Display our forms!
  Self.Initialized := True;

  for I := 0 to Screen.CustomFormCount - 1 do
    if (Screen.CustomForms[I] is TSimbaAnchorDockHostSite) then
    begin
      Site := Screen.CustomForms[I] as TSimbaAnchorDockHostSite;
      if (Site.MenuItem <> nil) and (not Site.MenuItem.Checked) then
        Continue;

      if DockMaster.ShowHeader then
        Screen.CustomForms[I].Visible := (Screen.CustomForms[I].ControlCount > 1)
      else
        Screen.CustomForms[I].Visible := (Screen.CustomForms[I].ControlCount > 0);
    end;

  Self.Visible := True;
  Self.ScriptProcessorTimer.Enabled := True;

  WriteLn('');
end;

procedure TSimbaForm.SetEnabled(Value: Boolean);
var
  i: Int32;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TTimer then
      TTimer(Components[i]).Enabled := Value;

  inherited SetEnabled(Value);
end;

procedure TSimbaForm.FormDestroy(Sender: TObject);
begin
  RemoveSettingChangeHandlers();
end;

procedure TSimbaForm.MenuEditClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentEditor <> nil then
    with SimbaScriptTabsForm.CurrentEditor do
    begin
      MenuItemCut.Enabled := SelText <> '';
      MenuItemCopy.Enabled := SelText <> '';
      MenuItemPaste.Enabled := SelText <> '';
    end;
end;

procedure TSimbaForm.MenuItemAboutClick(Sender: TObject);
begin
  SimbaAboutForm.ShowModal();
end;

procedure TSimbaForm.MenuItemCloseTabsClick(Sender: TObject);
var
  Aborted: Boolean;
begin
  SimbaScriptTabsForm.RemoveAllTabs(Aborted);
end;

procedure TSimbaForm.MenuItemExportHTMLClick(Sender: TObject);
var
  SynExporterHTML: TSynExporterHTML;
begin
  SynExporterHTML := TSynExporterHTML.Create(nil);
  SynExporterHTML.Highlighter := TSynFreePascalSyn.Create(SynExporterHTML);
  SynExporterHTML.ExportAsText := True;

  with TSaveDialog.Create(nil) do
  try
    Filter := 'HTML Files (*.html;*.htm)|*.html;*.htm|All files(*.*)|*.*';
    Options := [ofOverwritePrompt, ofEnableSizing];
    DefaultExt := 'html';

    if Execute then
    begin
      SynExporterHTML.Title := 'Simba - ' + SimbaScriptTabsForm.CurrentTab.ScriptName;
      SynExporterHTML.ExportAll(SimbaScriptTabsForm.CurrentEditor.Lines);
      SynExporterHTML.SaveToFile(FileName);
    end;
  finally
    Free();
  end;

  SynExporterHTML.Free();
end;

procedure TSimbaForm.MenuItemHideClick(Sender: TObject);
begin
  if Self.Visible = false then
    MenuItemShowClick(sender)
  else
    Self.Hide;
end;

procedure TSimbaForm.MenuItemReportBugClick(Sender: TObject);
begin
  OpenURL('https://github.com/MerlijnWajer/Simba/issues/new');
end;

procedure TSimbaForm.MenuItemSettingsSimpleButtonClick(Sender: TObject);
begin
  SimbaSettingsForm.ShowModal();
end;

procedure TSimbaForm.MenuItemShowClick(Sender: TObject);
begin
  Self.Show;
  Self.WindowState := wsNormal;
end;

procedure TSimbaForm.TrayIconClick(Sender: TObject);
begin
  Self.Show;
  if Self.CanSetFocus then
    Self.SetFocus;
end;

procedure TSimbaForm.ButtonPickClick(Sender: TObject);
begin
  try
    with TSimbaColorPicker.Create(FWindowSelection) do
    try
      SimbaColorHistoryForm.AddColObj(TColourPickerObject.Create(Color, Point, ''), True);

      SimbaDebugForm.Add('Color picked: ' + IntToStr(Color) + ' at (' + IntToStr(Point.X) + ', ' + IntToStr(Point.Y) + ')');
    finally
      Free();
    end;

    DockMaster.GetAnchorSite(SimbaColorHistoryForm).ShowOnTop();
  except
    on E: Exception do
      ShowMessage('Exception while picking color: ' + E.Message + '(' + E.ClassName + ')');
  end;
end;

procedure TSimbaForm.ButtonSelectorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  try
    with TSimbaWindowSelector.Create() do
    try
      SimbaDebugForm.Add('Window Selected: ' + IntToStr(Selected));
      SimbaDebugForm.Add(' - Dimensions: ' + IntToStr(Selected.GetBounds().Width - 1) + 'x' + IntToStr(Selected.GetBounds().Height - 1));
      SimbaDebugForm.Add(' - PID: ' + IntToStr(Selected.GetPID));

      if Selected.GetTitle() <> '' then
        SimbaDebugForm.Add(' - Title: ' + Selected.GetTitle());
      if Selected.GetClassName <> '' then
        SimbaDebugForm.Add(' - ClassName: ' + Selected.GetClassName());

      FWindowSelection := Selected;
    finally
      Free();
    end;
  except
    on E: Exception do
      ShowMessage('Exception while selecting window: ' + E.Message + '(' + E.ClassName + ')');
  end;
end;

procedure TSimbaForm.ButtonTrayClick(Sender: TObject);
begin
  TrayIcon.Show;
  self.hide;
end;

procedure TSimbaForm.SetScriptState(const State: TScriptButtonState);
begin
  if (State = FScriptState) then
    Exit;

  FScriptState := State;

  case FScriptState of
    ss_Running : begin RunButton.Enabled := False; PauseButton.Enabled:= True; CompileButton.Enabled := True;
                       StopButton.ImageIndex := IMAGE_STOP; StopButton.Enabled := True;
                       TrayPlay.Checked := True; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := True;
                       TrayStop.Enabled := True; TrayStop.Checked := False;
                 end;
    ss_Paused  : begin RunButton.Enabled:= True; PauseButton.Enabled := False; CompileButton.Enabled := True;
                       StopButton.ImageIndex := IMAGE_STOP; StopButton.Enabled := True;
                       TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := True; TrayPause.Enabled := True;
                       TrayStop.Enabled := True; TrayStop.Checked:= False;
                 end;
    ss_Stopping: begin RunButton.Enabled := False; PauseButton.Enabled := False; StopButton.Enabled := True; CompileButton.Enabled := True;
                       StopButton.ImageIndex := IMAGE_POWER;
                       TrayPlay.Checked := False; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := False;
                       TrayStop.Enabled := True; TrayStop.Checked := True;

                 end;
    ss_None    : begin RunButton.Enabled := True; PauseButton.Enabled := False; StopButton.Enabled := False; CompileButton.Enabled := True;
                       StopButton.ImageIndex := IMAGE_STOP;
                       TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := False; TrayPause.Enabled := False;
                       TrayStop.Enabled := False; TrayStop.Checked := False;
                 end;
    end;
end;

function TSimbaForm.LoadLayout: Boolean;
var
  Config: TXMLConfigStorage;
  Stream: TStringStream;
begin
  Result := False;

  Stream := TStringStream.Create(SimbaSettings.GUI.Layout.Value);

  if Stream.Size > 0 then
  try
    Position := poDesigned;

    Config := TXMLConfigStorage.Create(Stream);

    try
      DockMaster.LoadLayoutFromConfig(Config, True);
    finally
      Config.Free();
    end;

    Result := True;
  except
  end;

  Stream.Free();
end;

procedure TSimbaForm.SaveLayout;
var
  XML: TXMLConfig;
  Config: TXMLConfigStorage;
  Stream: TStringStream;
begin
  XML := TXMLConfig.Create(nil);
  Stream := TStringStream.Create('');
  Config := TXMLConfigStorage.Create(XML);

  DockMaster.SaveLayoutToConfig(Config);

  XML.SaveToStream(Stream);

  SimbaSettings.GUI.Layout.Value := Stream.DataString;

  XML.Free();
  Stream.Free();
  Config.Free();
end;

procedure TSimbaForm.SetupLayout(Reset: Boolean);
var
  i: Int32;
begin
  BeginFormUpdate();

  DockMaster.BeginUpdate();

  if Reset then
  begin
    SimbaScriptTabsForm.Hide();
    SimbaDebugForm.Hide();
    SimbaFileBrowserForm.Hide();
    SimbaFunctionListForm.Hide();
    SimbaNotesForm.Hide();

    DockMaster.ManualFloat(SimbaScriptTabsForm);
    DockMaster.ManualFloat(SimbaDebugForm);
    DockMaster.ManualFloat(SimbaFileBrowserForm);
    DockMaster.ManualFloat(SimbaFunctionListForm);
    DockMaster.ManualFloat(SimbaNotesForm);
    DockMaster.ManualFloat(SimbaDebugImageForm);
    DockMaster.ManualFloat(SimbaColorHistoryForm);

    for i := ControlCount - 1 downto 0 do
      if (Controls[i] is TAnchorDockSplitter) or (Controls[i] is TAnchorDockHostSite) then
        Controls[i].Free();
  end;

  Self.Width := 1000;
  Self.Height := 0;

  DockMaster.GetAnchorSite(SimbaScriptTabsForm).Width := 500;
  DockMaster.GetAnchorSite(SimbaScriptTabsForm).Height := 600;
  DockMaster.GetAnchorSite(SimbaFileBrowserForm).Width := 250;
  DockMaster.GetAnchorSite(SimbaFunctionListForm).Width := 165;
  DockMaster.GetAnchorSite(SimbaDebugForm).Height := 200;

  if Reset or (not LoadLayout()) then
  begin
    DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaScriptTabsForm), DockMaster.GetSite(Self), alBottom);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaDebugForm), DockMaster.GetSite(SimbaScriptTabsForm), alBottom);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFunctionListForm), DockMaster.GetSite(SimbaScriptTabsForm), alLeft);
    DockMaster.ManualDock(DockMaster.GetAnchorSite(SimbaFileBrowserForm), DockMaster.GetSite(SimbaScriptTabsForm), alRight);

    DockMaster.MakeVisible(SimbaScriptTabsForm, False);
    DockMaster.MakeVisible(SimbaDebugForm, False);
    DockMaster.MakeVisible(SimbaFunctionListForm, False);
    DockMaster.MakeVisible(SimbaFileBrowserForm, False);
  end;

  if Reset then
    SimbaSettings.GUI.LayoutLocked.Value := False;

  DockMaster.EndUpdate();

  if Reset then
  begin
    if (Position <> poScreenCenter) then
      Position := poScreenCenter;

    MoveToDefaultPosition();
  end;

  EndFormUpdate();
end;

initialization
  FormatSettings.DecimalSeparator := '.';

  {$R *.lfm}

end.
