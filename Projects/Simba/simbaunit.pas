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
unit SimbaUnit;

{$mode objfpc}{$H+}

{$I Simba.inc}

interface

uses
  {$IFDEF LINUX}cthreads, cmem, pthreads,{$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, SynEdit,
  mufasabase, MufasaTypes,
  synedittypes,
  script_thread,

  {$IFDEF MSWINDOWS} os_windows, windows, {$ENDIF} //For ColorPicker etc.
  {$IFDEF LINUX} os_linux, {$ENDIF} //For ColorPicker etc.

  colourpicker, windowselector, Clipbrd, // We need these for the Colour Picker and Window Selector

  framescript,

  lcltype, ActnList,
  SynEditMarkupHighAll, LMessages, Buttons, ShellCtrls, PairSplitter,
  mmisc,
  about, framefunctionlist, updateform,
  v_ideCodeInsight, v_ideCodeParser, CastaliaPasLexTypes, // Code completion units
  CastaliaSimplePasPar, v_AutoCompleteForm,  // Code completion units

  package,
  simba.settings;

const
  {$IFDEF LINUX}
  shortcut_StartScript = '<Ctrl><Alt>R';
  shortcut_StopScript =  '<Ctrl><Alt>S';
  shortcut_PickColour =  '<Ctrl><Alt>P';
  {$ELSE}
  shortcut_StartScriptMod = MOD_CONTROL or MOD_ALT;
  shortcut_StartScriptKey = VK_R;
  shortcut_StartScriptID  = 0;

  shortcut_StopScriptMod  = MOD_CONTROL or MOD_ALT;
  shortcut_StopScriptKey  = VK_S;
  shortcut_StopScriptID   = 1;

  shortcut_PickColourMod  = MOD_CONTROL or MOD_ALT;
  shortcut_PickColourKey  = VK_P;
  shortcut_PickColorID    = 2;
  {$ENDIF}

type
  TMufasaTab = class(Tobject)
  private
    PageCtrl: TPageControl;
  public
    TabSheet: TTabsheet;
    ScriptFrame: TScriptFrame;

    procedure Clear;//This will 'reset' the ScriptFrame
    constructor Create(Page : TPageControl);
    destructor Destroy; override;
  end;

  { TSimbaForm }

  TSimbaForm = class(TForm)
    ActionColors: TAction;                     
    ActionFileBrowser: TAction;
    ActionFindPrev: TAction;
    ActionFont: TAction;
    ActionNotes: TAction;
    ActionGoto: TAction;
    ActionSaveDef: TAction;
    ActionConsole: TAction;
    ActionCompileScript: TAction;
    ActionExit: TAction;
    ActionReplace: TAction;
    ActionFindNext: TAction;
    ActionRedo: TAction;
    ActionUndo: TAction;
    ActionSelectAll: TAction;
    ActionDelete: TAction;
    ActionPaste: TAction;
    ActionCopy: TAction;
    ActionCut: TAction;
    ActionFindStart: TAction;
    ActionClearDebug: TAction;
    ActionSaveAll: TAction;
    ActionStopScript: TAction;
    ActionSaveScript: TAction;
    ActionSaveScriptAs: TAction;
    ActionRunScript: TAction;
    ActionPauseScript: TAction;
    ActionNewScript: TAction;
    ActionOpenScript: TAction;
    ActionNewTab: TAction;
    ActionCloseTab: TAction;
    ActionTabLast: TAction;
    ActionTabNext: TAction;
    ActionList: TActionList;
    CheckBoxMatchCase: TCheckBox;
    FunctionList: TFunctionList_Frame;
    lblNotes: TLabel;
    lblFileBrowser: TLabel;
    LabeledEditSearch: TLabeledEdit;
    MainMenu: TMainMenu;
    DebugMemo: TMemo;
    MenuDTMEditor: TMenuItem;
    MenuItem1: TMenuItem;
    MenuColors: TMenuItem;
    MenuItemColors: TMenuItem;
    MenuItemUnloadPlugin: TMenuItem;
    MenuItemDivider12: TMenuItem;
    popupFileBrowserOpen: TMenuItem;
    popupFileBrowserOpenExternally: TMenuItem;
    memoNotes: TMemo;
    MenuItemFileBrowser: TMenuItem;
    MenuItemACA: TMenuItem;
    MenuItemFindPrev: TMenuItem;
    MenuItemFont: TMenuItem;
    MenuItemDivider13: TMenuItem;
    MenuItemNotes: TMenuItem;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemFormDesigner: TMenuItem;
    MenuItemSettingsSimpleButton: TMenuItem;
    MenuItemReadOnlyTab: TMenuItem;
    MenuItemGoto: TMenuItem;
    MenuItemDivider14: TMenuItem;
    MenuItemOpenPluginsFolder: TMenuItem;
    MenuItemOpenIncludesFolder: TMenuItem;
    MenuItemOpenScriptsFolder: TMenuItem;
    MenuItemDivider11: TMenuItem;
    MenuItemSaveDef: TMenuItem;
    MenuItemBitmapConv: TMenuItem;
    MenuItemDivider10: TMenuItem;
    MenuTools: TMenuItem;
    MenuItemOpenRecent: TMenuItem;
    MenuItemCompile: TMenuItem;
    MenuItemHandbook: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemReportBug: TMenuItem;
    MenuItemExportHTML: TMenuItem;
    MenuItemDivider9: TMenuItem;
    MouseTimer: TTimer;
    PanelUtilites: TPairSplitter;
    PanelCodeBrowser: TPairSplitterSide;
    PanelNotes: TPairSplitterSide;
    FileBrowser: TShellTreeView;
    FileBrowserPopup: TPopupMenu;
    btnRefreshFileBrowser: TSpeedButton;
    SpeedButtonFindNext: TSpeedButton;
    SpeedButtonFindPrev: TSpeedButton;
    NotesSplitter: TSplitter;
    ToolButton5: TToolButton;
    TB_ShowPackages: TToolButton;
    TT_ScriptManager: TToolButton;
    ToolButton6: TToolButton;
    TB_Console: TToolButton;
    TT_Cut: TToolButton;
    TT_Copy: TToolButton;
    TT_Paste: TToolButton;
    ToolButton9: TToolButton;
    UpdateTimer: TTimer;
    ToolButton3: TToolButton;
    TT_Update: TToolButton;
    UpdateMenuButton: TMenuItem;
    MenuItemFunctionList: TMenuItem;
    MenuItemHide: TMenuItem;
    MenuItemDebugImage: TMenuItem;
    MenuItemMainExit: TMenuItem;
    MenuItemDivider6: TMenuItem;
    PopupItemReplace: TMenuItem;
    MenuItemReplace: TMenuItem;
    dlgReplace: TReplaceDialog;
    MenuItemColourHistory: TMenuItem;
    MenuView: TMenuItem;
    MenuItemFindNext: TMenuItem;
    PopupItemDelete: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemDivider5: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    PopupItemSelectAll: TMenuItem;
    PopupItemDivider2: TMenuItem;
    PopupItemPaste: TMenuItem;
    PopupItemCopy: TMenuItem;
    PopupItemCut: TMenuItem;
    PopupItemDivider1: TMenuItem;
    PopupItemRedo: TMenuItem;
    PopupItemUndo: TMenuItem;
    PopupItemDivider3: TMenuItem;
    PopupItemFind: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemDivider4: TMenuItem;
    MenuItemDivider3: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSaveAll: TMenuItem;
    MenuItemTabCloseOthers: TMenuItem;
    MenuItemTabAdd: TMenuItem;
    MenuItemTabClose: TMenuItem;
    MenuItemCloseTabs: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemNewTab: TMenuItem;
    MenuItemDivider2: TMenuItem;
    PageControl1: TPageControl;
    ScriptPopup: TPopupMenu;
    SearchPanel: TPanel;
    ScriptPanel: TPanel;
    SpeedButtonSearch: TSpeedButton;
    SplitterFunctionList: TSplitter;
    TabPopup: TPopupMenu;
    TB_SaveAll: TToolButton;
    TrayDivider: TMenuItem;
    TrayPlay: TMenuItem;
    TrayStop: TMenuItem;
    TrayPause: TMenuItem;
    MenuItemPause: TMenuItem;
    MenuItemStop: TMenuItem;
    MenuItemShow: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemSave: TMenuItem;
    Mufasa_Image_List: TImageList;
    MenuItemScript: TMenuItem;
    MenuItemRun: TMenuItem;
    PanelMemo: TPanel;
    SplitterMemoSynedit: TSplitter;
    TrayPopup: TPopupMenu;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    TB_Run: TToolButton;
    TB_Pause: TToolButton;
    TB_Stop: TToolButton;
    ToolButton1: TToolButton;
    TB_Tray: TToolButton;
    TB_NewTab: TToolButton;
    TB_CloseTab: TToolButton;
    TB_New: TToolButton;
    ToolButton2: TToolButton;
    TB_Open: TToolButton;
    TB_Save: TToolButton;
    ToolButton4: TToolButton;
    TB_ClearDebug: TToolButton;
    TB_PickColour: TToolButton;
    TB_SelectClient: TToolButton;
    ToolButton8: TToolButton;
    MTrayIcon: TTrayIcon;
    FunctionListHint: THintWindow;
    procedure ActionColorsExecute(Sender: TObject);                                            
    procedure ActionClearDebugExecute(Sender: TObject);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionCompileScriptExecute(Sender: TObject);
    procedure ActionConsoleExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindPrevExecute(Sender: TObject);
    procedure ActionFindStartExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    procedure ActionGotoExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNewTabExecute(Sender: TObject);
    procedure ActionFileBrowserExecute(Sender: TObject);
    procedure ActionNotesExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveDefExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionTabLastExecute(Sender: TObject);
    procedure ActionTabNextExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ChangeMouseStatus(Sender: TObject);
    procedure CheckBoxMatchCaseClick(Sender: TObject);
    procedure CloseFindPanel;
    procedure doOnHide(Sender: TObject);
    procedure FileBrowserDoubleClick(Sender: TObject);
    procedure FileBrowserRefresh(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DebugMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowDTMEditor(Sender: TObject);
    procedure ShowFormDesigner(Sender: TObject);
    procedure UnloadPlugin(Sender: TObject);
    procedure MenuToolsClick(Sender: TObject);
    procedure popupFileBrowserOpenClick(Sender: TObject);
    procedure popupFileBrowserOpenExternallyClick(Sender: TObject);
    procedure MenuItemReadOnlyTabClick(Sender: TObject);
    procedure MenuItemBitmapConvClick(Sender: TObject);
    procedure MenuItemHandbookClick(Sender: TObject);
    procedure MenuItemColourHistoryClick(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
    procedure LabeledEditSearchEnter(Sender: TObject);
    procedure LabeledEditSearchExit(Sender: TObject);
    procedure LabeledEditSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure LabeledEditSearchKeyPress(Sender: TObject; var Key: char);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseTabsClick(Sender: TObject);
    procedure MenuItemDebugImageClick(Sender: TObject);
    procedure MenuItemHideClick(Sender: TObject);
    procedure MenuItemReportBugClick(Sender: TObject);
    procedure MenuItemSettingsSimpleButtonClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemTabCloseClick(Sender: TObject);
    procedure MenuItemTabCloseOthersClick(Sender: TObject);
    procedure MenuItemFunctionListClick(Sender: TObject);
    procedure MTrayIconClick(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControl1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PageControl1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PopupItemFindClick(Sender: TObject);
    procedure RecentFileItemsClick(Sender: TObject);
    procedure ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ScriptPopupPopup(Sender: TObject);
    procedure ShowACA(Sender: TObject);
    procedure SpeedButtonSearchClick(Sender: TObject);
    procedure SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure TB_ShowPackagesClick(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
    procedure TT_UpdateClick(Sender: TObject);
    procedure UpdateMenuButtonClick(Sender: TObject);
    procedure UpdateTimerCheck(Sender: TObject);
    procedure OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    procedure OnCompleteCode(Str: string);
    function OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
    function OnCCLoadLibrary(Sender: TObject; var Argument: string; out Parser: TCodeInsight): Boolean;
    procedure FileBrowserExpand(Sender: TObject; Node: TTreeNode);
  protected
    procedure WndProc(var Message: TLMessage); override;
  public
    PopupTab: Integer;
    SearchStart: TPoint;
    LastTab: Integer;
    News: String;
    SearchString: String;
    CurrScript: TScriptFrame; //The current scriptframe
    CurrTab: TMufasaTab; //The current TMufasaTab
    CodeCompletionForm: TAutoCompletePopup;
    CodeCompletionStart: TPoint;
    ParamHint: TParamHint;
    Tabs: TList;
    Manager: TIOManager;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    Exiting: Boolean;

    procedure Settings_Init;
    procedure Settings_ConsoleVisibleChanged(Value: Boolean);
    procedure Settings_FileBrowserVisibleChanged(Value: Boolean);
    procedure Settings_FunctionListVisibleChanged(Value: Boolean);
    procedure Settings_NotesVisibleChanged(Value: Boolean);
    procedure Settings_EditorFontNameChanged(Value: WideString);
    procedure Settings_EditorFontSizeChanged(Value: Int64);
    procedure Settings_EditorCaretPastEOLChanged(Value: Boolean);
    procedure Settings_EditorShowSpecialCharactersChanged(Value: Boolean);
    procedure Settings_Form_TrayIconVisible_Changed(Value: Boolean);

    procedure Utilities_Init;
    procedure Utilities_Update;

    procedure Environment_Init;

    procedure Editor_EditFont;
    procedure Editor_EditColors;

    procedure Form_LoadSettings;
    procedure Form_SaveSettings;

    procedure Parameters_Handle(Data: PtrInt);
    procedure Parameters_RunTest(Data: PtrInt);

    procedure SimbaNews_Download;
    procedure SimbaNews_Write(Sender: TObject);
    procedure SimbaNews_Execute(Data: PtrInt);

    procedure CodeTools_Initialize(Data: PtrInt);
    procedure CodeTools_ParseInternals;
    procedure CodeTools_FillFunctionList(Sender: TObject);

    function GetScriptState: TScriptState;
    procedure SetScriptState(const State: TScriptState);

    procedure CustomExceptionHandler(Sender: TObject; E: Exception);

    procedure UpdateTitle;
    function OpenScript: Boolean;
    function LoadScriptFile(FileName: String; AlwaysOpenInNewTab: Boolean = False; CheckOtherTabs: Boolean = True): Boolean;
    function SaveCurrentScript: Boolean;
    function SaveCurrentScriptAs: Boolean;
    function SaveCurrentScriptAsDefault: Boolean;
    function CanExitOrOpen: Boolean;
    function ClearScript: Boolean;
    procedure RunScript;
    procedure CompileScript;
    procedure PauseScript;
    procedure StopScript;
    procedure AddTab;
    procedure StopCodeCompletion;
    function FindTab(FileName: string): Integer;
    function DeleteTab(TabIndex: Integer; CloseLast: Boolean; Silent: Boolean = False): Boolean;
    procedure ClearTab(TabIndex: Integer);
    procedure CloseTabs(Exclude: Integer = -1; Silent: Boolean = False); //-1 for no exclusion
    procedure SetEditActions;
    procedure DoSearch(SearchOptions: TSynSearchOptions; HighlightAll: Boolean);
    procedure RefreshTab;
    procedure AddRecentFile(const FileName : string);
    procedure InitializeTMThread(out Thread : TMMLScriptThread);
    procedure OnSaveScript(const Filename : string);
    function DefaultScript: string;

    property ScriptState: TScriptState read GetScriptState write SetScriptState;
  end;

  procedure formWriteln(constref S: String);

const
  WindowTitle = 'Simba - %s';//Title, where %s = the place of the filename.

  Panel_State = 0;
  Panel_Coords = 1;
  Panel_ScriptName = 2;
  Panel_ScriptPath = 3;
  Panel_General = 3;

  Image_Stop = 7;
  Image_Terminate = 19;

var
  SimbaForm: TSimbaForm;
  AppPath, DataPath: string;

implementation

uses
  LCLIntf,
  LazUTF8,
  LazFileUtils,
  debugimage,
  files,
  bitmapconv,
  colourhistory,
  math,
  script_imports, script_plugins,
  openssl,
  aca, fphttpclient, dtm_editor, colorscheme,
  simba.settingsform
  {$IFDEF USE_FORMDESIGNER},
  frmdesigner
  {$ENDIF}
  {$IFDEF LINUX_HOTKEYS},
  keybinder
  {$ENDIF};

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
  mDebugLn('');
  mDebugLn('Something went wrong...');
  mDebugLn('');
  
  Trace := DumpExceptionCallStack(E);
  Trace += LineEnding + 'Simba Version: ' + IntToStr(SimbaVersion) + LineEnding;

  LogName := DataPath + 'ErrorLog_' + FormatDateTime('dd-mm-yy_hh-nn-ss', Now) + '.txt';
  mDebugLn(Format('Going to try to save the error information to "%s".', [LogName]));

  try
    with TFileStream.Create(UTF8ToSys(logname), fmOpenWrite or fmOpenReadWrite or fmCreate) do
    try
      Write(Trace[1], Length(Trace));
    finally
      Free;
    end;
  except
    mDebugLn(Format('Unable to save log file! [%s]', [LogName]));
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
      Self.CloseTabs; // Save scripts
      SimbaForm.Free;
    finally
      mDebugLn('Finally Free...');
    end;

    { Stop Simba }
    Halt(1); // Error code 1
  end;
end;

procedure TSimbaForm.Settings_Init;
begin
  if Application.HasOption('s', 'settings') then
    SimbaSettings := TSimbaSettings.Create(Application.GetOptionValue('s', 'settings'))
  else
    SimbaSettings := TSimbaSettings.Create(DataPath + 'settings.json');

  SimbaSettings.Form.ConsoleVisible.OnChange := @Settings_ConsoleVisibleChanged;
  SimbaSettings.Form.FileBrowserVisible.OnChange := @Settings_FileBrowserVisibleChanged;
  SimbaSettings.Form.FunctionListVisible.OnChange := @Settings_FunctionListVisibleChanged;
  SimbaSettings.Form.NotesVisible.OnChange := @Settings_NotesVisibleChanged;
  SimbaSettings.Form.TrayIconVisible.OnChange := @Settings_Form_TrayIconVisible_Changed;

  SimbaSettings.Editor.FontName.OnChange := @Settings_EditorFontNameChanged;
  SimbaSettings.Editor.FontSize.OnChange := @Settings_EditorFontSizeChanged;
  SimbaSettings.Editor.CaretPastEOL.OnChange := @Settings_EditorCaretPastEOLChanged;
  SimbaSettings.Editor.ShowSpecialCharacters.OnChange := @Settings_EditorShowSpecialCharactersChanged;
end;

procedure TSimbaForm.Form_LoadSettings;
var
  i: Int32;
begin
  BeginFormUpdate();

  try
    if SimbaSettings.Form.Maximized.Exists() and SimbaSettings.Form.Maximized.Value then
      Self.WindowState := wsMaximized
    else
    if SimbaSettings.Form.Left.Exists() and SimbaSettings.Form.Top.Exists() and
       SimbaSettings.Form.Width.Exists() and SimbaSettings.Form.Height.Exists() then
    begin
      Position := poDesigned;

      Self.Left := SimbaSettings.Form.Left.Value;
      Self.Top := SimbaSettings.Form.Top.Value;
      Self.Width := SimbaSettings.Form.Width.Value;
      Self.Height := SimbaSettings.Form.Height.Value;
    end;

    MTrayIcon.Visible := SimbaSettings.Form.TrayIconVisible.Value;

    // if not on the current monitor, let's just maximize on the current.
    if (Self.WindowState <> wsMaximized) and (not Self.Monitor.WorkAreaRect.Contains(Self.BoundsRect)) then
      Self.WindowState := wsMaximized;

    if SimbaSettings.Form.RecentFiles.Exists() then
      for i := 0 to High(SimbaSettings.Form.RecentFiles.Value) do
        if FileExists(SimbaSettings.Form.RecentFiles.Value[i]) then
          AddRecentFile(SimbaSettings.Form.RecentFiles.Value[i]);

    MemoNotes.Text := SimbaSettings.Form.NotesContent.Value;
  finally
    EndFormUpdate();
  end;
end;

procedure TSimbaForm.Form_SaveSettings;
var
  i: Int32;
  Files: TStringArray;
begin
  SimbaSettings.Form.Left.Value := Self.Left;
  SimbaSettings.Form.Top.Value := Self.Top;
  SimbaSettings.Form.Width.Value := Self.Width;
  SimbaSettings.Form.Height.Value := Self.Height;
  SimbaSettings.Form.Maximized.Value := (Self.WindowState = wsMaximized);

  SimbaSettings.Form.NotesContent.Value := MemoNotes.Text;

  for i := 0 to MenuItemOpenRecent.Count - 1 do
  begin
    SetLength(Files, Length(Files) + 1);

    Files[High(Files)] := MenuItemOpenRecent[i].Caption;
  end;

  SimbaSettings.Form.RecentFiles.Value := Files;
end;

procedure TSimbaForm.Parameters_RunTest(Data: PtrInt);
begin
  if Application.HasOption('test') then
  begin
    Application.RemoveASyncCalls(Self);

    while (Self.GetScriptState() in [ss_Running, ss_Stopping]) do
      Application.ProcessMessages();

    WriteLn('Testing ' + CurrScript.ScriptFile);
    Writeln(DebugMemo.Text);

    // This whole testing interface is a hack right now until out of process scripts are implemented.
    // But terminating like this allows nothing else to raise a exception while freeing (halt calls finalization sections).
    {$IFDEF WINDOWS}
    if (Self.CurrScript.ScriptErrorLine > -1) then
      TerminateProcess(OpenProcess(PROCESS_TERMINATE, True, GetProcessID()), 1)
    else
      TerminateProcess(OpenProcess(PROCESS_TERMINATE, True, GetProcessID()), 0);
    {$ELSE}
    if (Self.CurrScript.ScriptErrorLine > -1) then
      Halt(1)
    else
      Halt(0);
    {$ENDIF}
  end;
end;

procedure TSimbaForm.SimbaNews_Download;
begin
  try
    News := TFPHTTPClient.SimpleGet('http://simba.villavu.com/bin/news');
  except
    on e: Exception do
      WriteLn('Failed downloading Simba news: ' + e.Message);
  end;
end;

procedure TSimbaForm.SimbaNews_Write(Sender: TObject);
begin
  with TStringList.Create() do
  try
    Text := News; // Process line endings.

    if (Text <> '') then
      DebugMemo.Lines.Add(Text);
  finally
    Free();
  end;
end;

procedure TSimbaForm.SimbaNews_Execute(Data: PtrInt);
begin
  if SimbaSettings.General.ShowSimbaNews.Value then
    with TProcThread.Create() do
    begin
      ClassProc := @SimbaNews_Download;
      OnTerminate := @SimbaNews_Write;
      Start();
    end;
end;

procedure TSimbaForm.Environment_Init;
begin
  // Reset paths to default if they don't exist
  if (not DirectoryExistsUTF8(SimbaSettings.Environment.PluginPath.Value)) then
    SimbaSettings.Environment.PluginPath.Value := SimbaSettings.Environment.PluginPath.DefaultValue;
  if (not DirectoryExistsUTF8(SimbaSettings.Environment.FontPath.Value)) then
    SimbaSettings.Environment.FontPath.Value := SimbaSettings.Environment.FontPath.DefaultValue;
  if (not DirectoryExistsUTF8(SimbaSettings.Environment.IncludePath.Value)) then
    SimbaSettings.Environment.IncludePath.Value := SimbaSettings.Environment.IncludePath.DefaultValue;
  if (not DirectoryExistsUTF8(SimbaSettings.Environment.ScriptPath.Value)) then
    SimbaSettings.Environment.ScriptPath.Value := SimbaSettings.Environment.ScriptPath.DefaultValue;

  // Create the dirs if they don't exist
  CreateDirUTF8(SimbaSettings.Environment.PluginPath.Value);
  CreateDirUTF8(SimbaSettings.Environment.FontPath.Value);
  CreateDirUTF8(SimbaSettings.Environment.IncludePath.Value);
  CreateDirUTF8(SimbaSettings.Environment.ScriptPath.Value);
end;

procedure TSimbaForm.Editor_EditFont;
var
  Dialog: TFontDialog;
begin
  Dialog := TFontDialog.Create(nil);

  with Dialog do
  try
    Options := [fdEffects, fdFixedPitchOnly];
    Title := 'Font Editor';

    if Execute() then
    begin
      SimbaSettings.Editor.FontName.Value := Font.Name;
      SimbaSettings.Editor.FontSize.Value := Font.Size;
    end;
  finally
    Dialog.Free();
  end;
end;

procedure TSimbaForm.Editor_EditColors;
begin
  SimbaColorsForm.ShowModal();
end;


procedure TSimbaForm.Settings_ConsoleVisibleChanged(Value: Boolean);
{$IFDEF WINDOWS}
var
  PID: UInt32;
begin
  GetWindowThreadProcessId(GetConsoleWindow(), PID);

  if (PID = GetCurrentProcessID()) then
  begin
    case Value of
      True: ShowWindow(GetConsoleWindow(), SW_SHOWNORMAL);
      False: ShowWindow(GetConsoleWindow(), SW_HIDE);
    end;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TSimbaForm.Settings_FileBrowserVisibleChanged(Value: Boolean);
begin
  Utilities_Update();

  MenuItemFileBrowser.Checked := Value;
end;

procedure TSimbaForm.Settings_FunctionListVisibleChanged(Value: Boolean);
begin
  case Value of
    True:
      begin
        FunctionList.FrameEndDock(FunctionList, FunctionList.Parent, 0, 0);

        if (FunctionList.Parent is TPanel) then
        begin
          SplitterFunctionList.Show();
          FunctionList.Show();
        end else
          FunctionList.Parent.Show();
      end;

    False:
      begin
        if (FunctionList.Parent is TPanel) then
          FunctionList.Hide()
        else
          FunctionList.Parent.Hide();

        SplitterFunctionList.Hide();
      end;
  end;

  MenuItemFunctionList.Checked := SimbaSettings.Form.FunctionListVisible.Value;
end;

procedure TSimbaForm.Settings_NotesVisibleChanged(Value: Boolean);
begin
  Utilities_Update();

  MenuItemNotes.Checked := Value;
end;

procedure TSimbaForm.Settings_EditorFontNameChanged(Value: WideString);
var
  i: Int32;
begin
  if (Tabs <> nil) then
    for i := 0 to Tabs.Count - 1 do
      TMufasaTab(Tabs[i]).ScriptFrame.SynEdit.Font.Name := Value;
end;

procedure TSimbaForm.Settings_EditorFontSizeChanged(Value: Int64);
var
  i: Int32;
begin
  if (Tabs <> nil) then
    for i := 0 to Tabs.Count - 1 do
      TMufasaTab(Tabs[i]).ScriptFrame.SynEdit.Font.Size := Value;
end;

procedure TSimbaForm.Settings_EditorCaretPastEOLChanged(Value: Boolean);
var
  i: Int32;
begin
  if (Tabs <> nil) then
    for i := 0 to Tabs.Count - 1 do
    begin
      with TMufasaTab(Tabs[i]).ScriptFrame do
        if Value then
          SynEdit.Options := SynEdit.Options + [eoScrollPastEOL, eoTrimTrailingSpaces]
        else
          SynEdit.Options := SynEdit.Options - [eoScrollPastEOL, eoTrimTrailingSpaces];
    end;
end;

procedure TSimbaForm.Settings_EditorShowSpecialCharactersChanged(Value: Boolean);
var
  i: Int32;
begin
  if (Tabs <> nil) then
    for i := 0 to Tabs.Count - 1 do
    begin
      with TMufasaTab(Tabs[i]).ScriptFrame do
        if Value then
          SynEdit.Options := SynEdit.Options + [eoShowSpecialChars]
        else
          SynEdit.Options := SynEdit.Options - [eoShowSpecialChars];
    end;
end;

procedure TSimbaForm.Settings_Form_TrayIconVisible_Changed(Value: Boolean);
begin
  MTrayIcon.Visible := Value;
end;

procedure TSimbaForm.Utilities_Init;
begin
  FileBrowser.Root := AppPath;

  if (FileBrowser.Items.Count > 0) then
  begin
    FileBrowser.Items[0].Text := ExtractFileName(ExcludeTrailingPathDelimiter(FileBrowser.Root));
    FileBrowser.Items[0].ImageIndex := 54;
    FileBrowser.Items[0].SelectedIndex := 54;
  end;

  Utilities_Update();
end;

procedure TSimbaForm.Utilities_Update;
var
  Browser, Notes: TPairSplitterSide;
  i: Int32;
begin
  if (not PanelUtilites.HandleAllocated) then
    PanelUtilites.HandleNeeded();

  Browser := PanelUtilites.Sides[0];
  Browser.Visible := SimbaSettings.Form.FileBrowserVisible.Value;

  Notes := PanelUtilites.Sides[1];
  Notes.Visible := SimbaSettings.Form.NotesVisible.Value;

  for i := 0 to PanelUtilites.ControlCount - 1 do
    if PanelUtilites.Controls[i] is TSplitter then
      TSplitter(PanelUtilites.Controls[i]).Visible := Browser.Visible and Notes.Visible;

  if Browser.Visible and Notes.Visible then
  begin
    Browser.Align := alTop;
    Notes.Align := alClient;
  end else
  if Browser.Visible and (not Notes.Visible) then
    Browser.Align := alClient
  else
  if Notes.Visible and (not Browser.Visible) then
    Notes.Align := alClient;

  PanelUtilites.Visible := Browser.Visible or Notes.Visible;
end;

{$IFDEF WINDOWS}
procedure Bind_Windows_Keys;
begin
  if not RegisterHotkey(SimbaForm.Handle, shortcut_StartScriptID, shortcut_StartScriptMod, shortcut_StartScriptKey) then
    WriteLn('Unable to register start script hotkey - prehaps another Simba instance is running?');
  if not RegisterHotkey(SimbaForm.Handle, shortcut_StopScriptID, shortcut_StopScriptMod, shortcut_StopScriptKey) then
    WriteLn('Unable to register stop script hotkey - prehaps another Simba instance is running?');
  if not RegisterHotkey(SimbaForm.Handle, shortcut_PickColorID, shortcut_PickColourMod, shortcut_PickColourKey) then
    WriteLn('Unable to register pick colour hotkey - prehaps another Simba instance is running?');
end;

procedure Unbind_Windows_Keys;
begin
  UnregisterHotKey(SimbaForm.Handle, shortcut_StartScriptID);
  UnregisterHotKey(SimbaForm.Handle, shortcut_StopScriptID);
  UnregisterHotKey(SimbaForm.Handle, shortcut_PickColorID);
end;
{$ENDIF}

{$IFDEF LINUX_HOTKEYS}
{ Used for global callbacks on LINUX }
procedure keybinder_callback(keystring: PChar; user_data: PtrUInt); cdecl;
begin
  if keystring = shortcut_StartScript then
    SimbaForm.ActionRunScript.Execute
  else if keystring = shortcut_StopScript then
    SimbaForm.ActionStopScript.Execute
  else if keystring = shortcut_PickColour then
    SimbaForm.ButtonPickClick(nil);
end;

{ XXX, TODO: Pressing the stop shortcut twice (quickly) may crash Simba. }
procedure Bind_Linux_Keys;
begin
  keybinder_init(); { Initialise keybinder }

  { Bind keys }
  if not keybinder_bind(PChar(shortcut_StartScript), @keybinder_callback, PtrUInt(0)) then
    mDebugLn('Unable to register '+ shortcut_StartScript + ' as global hotkey');
  if not keybinder_bind(PChar(shortcut_StopScript), @keybinder_callback, PtrUInt(0)) then
    mDebugLn('Unable to register '+ shortcut_StopScript + ' as global hotkey');
  if not keybinder_bind(PChar(shortcut_PickColour), @keybinder_callback, PtrUInt(0)) then
    mDebugLn('Unable to register '+ shortcut_PickColour + ' as global hotkey');
end;

procedure Unbind_Linux_Keys;
begin
  keybinder_unbind(PChar(shortcut_StartScript), @keybinder_callback, PtrUInt(0));
  keybinder_unbind(PChar(shortcut_StopScript), @keybinder_callback, PtrUInt(0));
  keybinder_unbind(PChar(shortcut_PickColour), @keybinder_callback, PtrUInt(0));
end;
{$ENDIF}

procedure TSimbaForm.OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  if (Typ = meNotSupported) then
    Exit;
  if (Sender is TmwSimplePasPar) then
    if (TmwSimplePasPar(Sender).Lexer.TokenID = tok_DONE) then
      Exit;
  mDebugLn('CC ERROR: '+Format('%d:%d %s', [Y + 1, X, Msg])+' in '+TCodeInsight(Sender).FileName);
end;

procedure TSimbaForm.OnCompleteCode(Str: string);
var
  sp, ep: Integer;
  s: string;
begin
  sp := -1;
  ep := -1;

  if (Str <> '') then
  begin
    s := WordAtCaret(CurrScript.SynEdit, sp, ep);
    if (s <> '') then
    begin
      CurrScript.SynEdit.SelStart := CurrScript.SynEdit.SelStart + (sp - CurrScript.SynEdit.CaretX);
      CurrScript.SynEdit.SelEnd := CurrScript.SynEdit.SelStart + (ep - CurrScript.SynEdit.CaretX) + 1;
      CurrScript.SynEdit.SelText := Str;
    end
    else
      CurrScript.SynEdit.InsertTextAtCaret(Str);
  end;
end;

function TSimbaForm.OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
begin
  Result := FindFile(Filename, [AppPath, SimbaSettings.Environment.IncludePath.Value]);
end;

function TSimbaForm.OnCCLoadLibrary(Sender: TObject; var Argument: string; out Parser: TCodeInsight): Boolean;
var
  Dump: String;
  Plugin: TMPlugin;
  i: Int32;
  MS: TMemoryStream;
begin
  Dump := '';

  try
    Plugin := Plugins.Get(TCodeInsight(Sender).FileName, Argument, False);
    for i := 0 to Plugin.Declarations.Count - 1 do
      Plugin.Declarations[i].Dump(Dump);
  except
    on e: Exception do
      mDebugLn(e.Message);
  end;

  if (Dump <> '') then
  begin
    MS := TMemoryStream.Create();
    MS.Write(Dump[1], Length(Dump));

    Parser := TCodeInsight.Create();
    Parser.FileName := Plugin.FilePath;
    Parser.OnMessage := @OnCCMessage;

    try
      Parser.Run(MS, nil, -1, True);

      Exit(True);
    except
      on e: Exception do
        Parser.Free();
    end;
  end;

  Exit(False);
end;

procedure TSimbaForm.RecentFileItemsClick(Sender: TObject);
begin
  LoadScriptFile(TMenuItem(Sender).Caption);
end;

procedure TSimbaForm.ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  if(X <= (ScriptPanel.Width div 2))then
  begin
    FunctionList.Align := alLeft;
    PageControl1.Align := alRight;
    SplitterFunctionList.ResizeAnchor := akLeft;
    SplitterFunctionList.Align := alLeft;
    SplitterFunctionList.Left := FunctionList.Left + FunctionList.Width;
  end else begin
    FunctionList.Align := alRight;
    PageControl1.Align := alLeft;
    SplitterFunctionList.ResizeAnchor := akRight;
    SplitterFunctionList.Align := alRight;
    SplitterFunctionList.Left := FunctionList.Left;
  end;
  PageControl1.Width := ScriptPanel.Width - (Source.DockRect.Right - Source.DockRect.Left);
  FunctionList.Width := ScriptPanel.Width - PageControl1.Width;
  PageControl1.Align := alClient;
  SplitterFunctionList.Show;
end;

procedure TSimbaForm.ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; //is there a better way to do all of this?
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
   P: TPoint;
begin
  Accept := FunctionList.DragKind = dkDock;
  if(Accept)then
  begin
    P := ScriptPanel.ClientToScreen(Classes.Point(0, 0));
    if(X <= (ScriptPanel.Width div 2))then
      Source.DockRect := Classes.Rect(P.x, P.y, min(P.x + FunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y + ScriptPanel.Height)
    else
      Source.DockRect := Classes.Rect(max(P.x + ScriptPanel.Width - FunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y, P.x + ScriptPanel.Width, P.y + ScriptPanel.Height);
  end;
end;

procedure TSimbaForm.ScriptPopupPopup(Sender: TObject);
begin
  SetEditActions;
end;

procedure TSimbaForm.ShowACA(Sender: TObject);
begin
  TACAForm.Create(Manager).Show();
end;

procedure TSimbaForm.SpeedButtonSearchClick(Sender: TObject);
begin
  CloseFindPanel;
end;

procedure TSimbaForm.SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  if(NewSize > ScriptPanel.Width div 2)then
    NewSize := ScriptPanel.Width div 2;
end;

procedure TSimbaForm.TB_ShowPackagesClick(Sender: TObject);
begin
  if PackageForm.Showing then
    PackageForm.BringToFront()
  else
    PackageForm.Show();
end;

procedure TSimbaForm.TrayPopupPopup(Sender: TObject);
begin
  { XXX: What's up with this? }
  MenuItemHide.Enabled:= SimbaForm.Visible;
  {$IFDEF WINDOWS}
  MenuItemShow.Enabled:= not SimbaForm.Visible;
  if SimbaForm.Visible then
    if SimbaForm.CanFocus then
      SimbaForm.SetFocus;
  {$ENDIF}
end;

procedure TSimbaForm.TT_UpdateClick(Sender: TObject);
begin
  SimbaUpdateForm.ShowModal;
  TT_Update.Visible:=False;
end;

procedure TSimbaForm.WndProc(var Message: TLMessage);
begin
  inherited WndProc(Message);

  {$IFDEF WINDOWS}
  if (Message.Msg = WM_HOTKEY) then
    case Message.wParam of
      shortcut_StartScriptID: RunScript();
      shortcut_StopScriptID:  StopScript();
      shortcut_PickColorID:   ButtonPickClick(nil);
    end;
  {$ENDIF}
end;

procedure TSimbaForm.UpdateTimerCheck(Sender: TObject);
var
  LatestVersion: Int32;
begin
  if SimbaSettings.General.AutomaticallyCheckForSimbaUpdates.Value then
  begin
    WriteLn('Checking for Simba update...');

    LatestVersion := SimbaUpdateForm.GetLatestSimbaVersion();

    if (LatestVersion > SimbaVersion) then
    begin
      TT_Update.Visible := True;

      formWriteLn('A new update of Simba is available!');
      formWriteLn('To update, click the update button in the toolbar');
      formWriteLn(Format('Current version is %d. Latest version is %d', [SimbaVersion, LatestVersion]));
    end;
  end;
end;

procedure TSimbaForm.UpdateMenuButtonClick(Sender: TObject);
begin
  if SimbaUpdateForm.CanUpdate then
    SimbaUpdateForm.ShowModal
  else if MessageDlg('Your Simba seems to be up to date. ' +
         'Do you want to force an update?', mtConfirmation, mbOKCancel, 0) = mrOk then
  begin
    SimbaUpdateForm.ShowModal;
  end;
end;

procedure formWriteln(constref S: String);
begin
  if (GetCurrentThreadId() <> MainThreadID) then
    WriteLn('formWriteLn called off main thread')
  else
    SimbaForm.DebugMemo.Lines.Add(s);
end;

procedure TSimbaForm.RunScript;
begin
  case ScriptState of
    ss_Paused:
      begin
        CurrScript.ScriptThread.State := ssRun;

        ScriptState := ss_Running;
      end;

   ss_None:
    begin
      InitializeTMThread(CurrScript.ScriptThread);

      if (CurrScript.ScriptThread <> nil) then
      begin
        CurrScript.ScriptThread.OnTerminate := @CurrScript.ScriptThreadTerminate;
        CurrScript.ScriptThread.Start();

        ScriptState := ss_Running;
      end;
    end;
  end;
end;

procedure TSimbaForm.CompileScript;
begin
  InitializeTMThread(CurrScript.ScriptThread);

  if (CurrScript.ScriptThread <> nil) then
  begin
    CurrScript.ScriptThread.Options := CurrScript.ScriptThread.Options + [soCompileOnly];
    CurrScript.ScriptThread.OnTerminate := @CurrScript.ScriptThreadTerminate;
    CurrScript.ScriptThread.Start();

    ScriptState := ss_Running;
  end;
end;

procedure TSimbaForm.PauseScript;
begin
  if (ScriptState = ss_Running) then
  begin
    CurrScript.ScriptThread.State := ssPause;

    ScriptState := ss_Paused;
  end;
end;

procedure TSimbaForm.StopScript;
begin
  case ScriptState of
    ss_Stopping:
      begin
        Sleep(500); // Give a little time to terminate on it's own.

        if (CurrScript.ScriptThread <> nil) then
        begin
          formWriteLn('Forcefully terminating the script thread');

          if (not CurrScript.ScriptThread.Kill()) then
            formWriteLn('Failed to forcefully kill the script thread');
        end;
      end;

    ss_Running:
      begin
        CurrScript.ScriptThread.TerminateOptions := CurrScript.ScriptThread.TerminateOptions + [stoUserTerminated];
        CurrScript.ScriptThread.State := ssStop;

        ScriptState := ss_Stopping;
      end;

    ss_Paused:
      begin
        CurrScript.ScriptThread.State := ssStop;

        ScriptState := ss_Stopping;
      end;
  end;
end;

procedure TSimbaForm.AddTab;
var
  Tab: TMufasaTab;
begin
  Tab := TMufasaTab.Create(Self.PageControl1);
  Tabs.Add(Tab);
  Tab.TabSheet.ImageIndex := 8;

  // customize based on settings.
  with Tab.ScriptFrame do
  begin
    if SimbaSettings.Editor.FontName.Exists() then
      SynEdit.Font.Name := SimbaSettings.Editor.FontName.Value;

    if SimbaSettings.Editor.FontSize.Exists() then
      SynEdit.Font.Size := SimbaSettings.Editor.FontSize.Value;

    if SimbaSettings.Editor.CaretPastEOL.Exists() and SimbaSettings.Editor.CaretPastEOL.Value then
      SynEdit.Options := SynEdit.Options + [eoScrollPastEOL, eoTrimTrailingSpaces]
    else
      SynEdit.Options := SynEdit.Options - [eoScrollPastEOL, eoTrimTrailingSpaces];

    if SimbaSettings.Editor.ShowSpecialCharacters.Exists() and SimbaSettings.Editor.ShowSpecialCharacters.Value then
      SynEdit.Options := SynEdit.Options + [eoShowSpecialChars];
  end;

  PageControl1.TabIndex := Tabs.Count - 1;

  RefreshTab();

  if (Tabs.Count > 1) then
  begin
    TB_SaveAll.Enabled := True;

    MenuItemSaveAll.Enabled := True;
  end;
end;

function TSimbaForm.DeleteTab(TabIndex: Integer; CloseLast: Boolean; Silent: Boolean): Boolean;
var
  Tab : TMufasaTab;
  OldIndex : integer;//So that we can switch back, if needed.
begin
  if not Silent then
  begin
    OldIndex := PageControl1.TabIndex;
    if TabIndex = OldIndex then
      OldIndex := Min(Tabs.Count - 1,OldIndex + 1);
    PageControl1.TabIndex:= TabIndex;
  end;
  //ScriptFrame now is now correct ;-D
  result := CanExitOrOpen;
  if not result then
    exit;
  Tab := TMufasaTab(Tabs[TabIndex]);
  if (Tabs.Count = 1) and (not CloseLast) then
    Tab.Clear
  else
  begin;
    Tab.Free;
    Tabs.Delete(TabIndex);
    if not Silent then
    begin;
      if OldIndex > TabIndex then
        PageControl1.TabIndex := OldIndex - 1
      else if OldIndex < TabIndex then
        PageControl1.TabIndex := OldIndex;
    end;
  end;
  if tabs.count <= 1 then
  begin;
    TB_SaveAll.Enabled:= false;
    MenuItemSaveAll.Enabled:= false;
  end;
  if not silent then
    RefreshTab;
end;

procedure TSimbaForm.ClearTab(TabIndex: Integer);
begin
  TMufasaTab(Tabs[TabIndex]).Clear;
end;

procedure TSimbaForm.CloseTabs(Exclude: Integer; Silent: Boolean);
var
  I : integer;
begin
  for i := tabs.count - 1 downto 0 do
    if i <> exclude then
      if not DeleteTab(i,false,silent) then
        exit;
end;

procedure TSimbaForm.SetEditActions;

  procedure EditActions(Undo,Redo,Cut,Copy,Paste,Delete : boolean);
  begin;
    ActionUndo.Enabled:= Undo;
    ActionRedo.Enabled:= Redo;
    ActionCut.Enabled:= Cut;
    ActionCopy.Enabled:= Copy;
    ActionPaste.Enabled:= Paste;
    ActionDelete.Enabled:= Delete;
    {$ifdef UpdateEditButtons}
    TT_Cut.Enabled:= Cut;
    TT_Paste.Enabled:=Paste;
    TT_Copy.enabled := Copy;
    {$endif}
  end;

var
  S: String;
  B: Boolean;
begin
  {
    TODO: This causes segfaults in EditActions(...) in certain cases.
    Simply checking if CurrScript and CurrScript.SynEdit is assigned is not
    enough.)
  }

  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
  begin
    with CurrScript.SynEdit do
    begin
      EditActions(CanUndo,CanRedo,SelText <> '',SelText <> '',CanPaste,SelText <> '');
//      B:= SelText <> '';
      B := SelAvail;
      PopupItemFind.Enabled:= B;
      PopupItemReplace.Enabled:= B;
      if(B)then
      begin
        s := SelText;
        if(Length(S) > 13)then
          S:= Format('"%s"', [Copy(S, 1, 10) + '...'])
        else
          S:= Format('"%s"', [S]);
        PopupItemFind.Caption:= 'Find next: ' + S;
        PopupItemReplace.Caption:= 'Replace:   ' + S;
      end;
    end
  end
  else if DebugMemo.Focused then
    with DebugMemo do
      EditActions(CanUndo,False,SelText <>'',SelText <> '',True,SelText <> '')
  else
    EditActions(false,false,false,false,false,false);
end;

procedure TSimbaForm.DoSearch(SearchOptions: TSynSearchOptions; HighlightAll: Boolean);
var
  Res : integer;
begin
  if CheckBoxMatchCase.Checked then
    Include(SearchOptions, ssoMatchCase);

  if SearchString = '' then
  begin
    res := -1;
    CurrScript.Synedit.SetHighlightSearch('',[]);
    CurrScript.SynEdit.LogicalCaretXY := SearchStart;
  end
  else
  begin
    if (not (ssoFindContinue in SearchOptions)) and CurrScript.SynEdit.SelAvail then
      if (ssoBackwards in SearchOptions) then
        CurrScript.SynEdit.LogicalCaretXY := CurrScript.SynEdit.BlockEnd
      else
        CurrScript.SynEdit.LogicalCaretXY := CurrScript.SynEdit.BlockBegin;

    mDebugLn('Searching: ' + SearchString);
    res := CurrScript.SynEdit.SearchReplace(SearchString, '', SearchOptions);

    if (res = 0) then
    begin
      mDebugLn('Search wrap around');
      Include(SearchOptions, ssoEntireScope);
      res := CurrScript.SynEdit.SearchReplace(SearchString, '', SearchOptions);
    end;
  end;

  if res = 0 then
  begin;
    LabeledEditSearch.Color := 6711039;
    LabeledEditSearch.Font.Color:= clWhite;
    CurrScript.Synedit.SetHighlightSearch('',[]);
    CurrScript.SynEdit.LogicalCaretXY := SearchStart;
  end
  else
  begin
    LabeledEditSearch.Color:= clWindow;
    LabeledEditSearch.Font.Color:= clWindowText;

    with CurrScript.SynEdit do
    begin
      HighlightAllColor.Background:= clYellow;
      if HighlightAll then
        SetHighlightSearch(SearchString, SearchOptions)
      else
        SetHighlightSearch('',[]);
    end;
  end;
end;

procedure TSimbaForm.RefreshTab;
var
  Tab : TMufasaTab;
  Script : TScriptFrame;
  NewTab : integer;
begin
  if tabs.Count < 1 then
  begin;
    mDebugLn('Cannot refresh tab, since there are no tabs.');
    exit;
  end;
  NewTab := PageControl1.TabIndex;
  if NewTab < 0 then
    Exit;
  Tab := TMufasaTab(Tabs[Newtab]);
  Script := Tab.ScriptFrame;
  Self.CurrScript := Script;
  Self.CurrTab := Tab;
  SetScriptState(Tab.ScriptFrame.FScriptState);//To set the buttons right
  if Self.Showing then
    if Tab.TabSheet.TabIndex = Self.PageControl1.TabIndex then
      if CurrScript.SynEdit.CanFocus then
        CurrScript.SynEdit.SetFocus;      // XXX: This is never called

  StopCodeCompletion;//To set the highlighting back to normal;

  with CurrScript.SynEdit do
  begin
    SetHighlightSearch('',[]);
    UseIncrementalColor:= false;
    MarkupByClass[TSynEditMarkupHighlightAllCaret].TempEnable;
    Invalidate;
  end;
  LabeledEditSearch.SelLength:= 0;
  LabeledEditSearch.Color:= clWindow;
  LabeledEditSearch.Font.Color:= clWindowText;

  //Set tha edit buttons right
  SetEditActions;
  UpdateTitle();
end;

procedure TSimbaForm.AddRecentFile(const FileName: string);
var
  Item: TMenuItem;
begin
  Item := TMenuItem.Create(MenuItemOpenRecent);
  Item.Caption := FileName;
  Item.OnClick := @RecentFileItemsClick;

  MenuItemOpenRecent.Insert(0, Item);
  if MenuItemOpenRecent.Count > 10 then
    MenuItemOpenRecent[10].Free();
end;

procedure TSimbaForm.InitializeTMThread(out Thread: TMMLScriptThread);
var
  Script: String;
  Continue: Boolean;
begin
  Thread := nil;

  if SimbaSettings.General.SaveScriptOnCompile.Value then
    SaveCurrentScript();

  Script := CurrScript.SynEdit.Lines.Text;

  try
    Thread := TMMLScriptThread.Create(Script, CurrScript.ScriptFile);
    Thread.Output := DebugMemo.Lines;
    Thread.Error.Data := @CurrScript.ErrorData;
    Thread.Error.Callback := @CurrScript.HandleErrorData;

    Thread.ScriptFile := ExtractFileName(CurrScript.ScriptFile);
    Thread.ScriptPath := ExtractFilePath(CurrScript.ScriptFile);
    Thread.FontPath := SimbaSettings.Environment.FontPath.Value;
    Thread.PluginPath := SimbaSettings.Environment.PluginPath.Value;
    Thread.IncludePath := SimbaSettings.Environment.IncludePath.Value;
    Thread.AppPath := AppPath;

    if Selector.HasPicked then
      Thread.Client.IOManager.SetTarget(Selector.LastPick);

    Thread.SetFonts(SimbaSettings.Environment.FontPath.Value); // Create font constants

    CurrScript.ScriptErrorLine := -1;
  except
    on e: Exception do
      formWriteln('Failed to initialise script thread: ' + e.ClassName + '::' + e.Message);
  end;
end;

procedure TSimbaForm.Parameters_Handle(Data: PtrInt);
begin
  if Application.HasOption('h', 'help') then
  begin
    WriteLn('');
    WriteLn('Options:');
    WriteLn('  -open, -o: opens the given script');
    WriteLn('  -run, -r: runs the script opened with -open');
    WriteLn('  -compile, -c: compiles the script opened with -open');
    WriteLn('  -settings, -s: uses the given settings file');
    WriteLn('  -test: will wait for script to run then terminates Simba, exit code 1 if errored. requires -open and -run or -compile');
    WriteLn('');
  end;

  if (Application.ParamCount = 1) then
    LoadScriptFile(Application.Params[1]);
  if Application.HasOption('o', 'open') then
    LoadScriptFile(Application.GetOptionValue('o', 'open'));
  if Application.HasOption('r', 'run') then
    Self.RunScript();
  if Application.HasOption('c', 'compile') then
    Self.CompileScript();
end;

procedure TSimbaForm.CodeTools_Initialize(Data: PtrInt);
begin
  with TProcThread.Create() do
  begin
    ClassProc := @CodeTools_ParseInternals;
    OnTerminate := @CodeTools_FillFunctionList;
    Start();
  end;
end;

procedure TSimbaForm.CodeTools_ParseInternals;

  function Parse(Section, Text: String): TCodeInsight;
  var
    Stream: TMemoryStream;
  begin
    Stream := TMemoryStream.Create();
    Stream.Write(Text[1], Length(Text));

    Result := TCodeInsight.Create();
    Result.FileName := Section;
    Result.OnMessage := @OnCCMessage;
    Result.Run(Stream, nil, -1, True);
  end;

var
  Imports: TScriptImports_Dump;
  i: Int32;
begin
  try
    Imports := TScriptImports_Dump.Create();

    try
      for i := 0 to Imports.Dump.Count - 1 do
      begin
        SetLength(CoreBuffer, Length(CoreBuffer) + 1);
        CoreBuffer[High(CoreBuffer)] := Parse(Imports.Dump.Names[i], Imports.Dump.ValueFromIndex[i]);
      end;
    finally
      Imports.Free();
    end;
  except
    on e: Exception do
      WriteLn('ERROR parsing internals: ' + e.ClassName + ' :: ' + e.Message);
  end;
end;

procedure TSimbaForm.CodeTools_FillFunctionList(Sender: TObject);
var
  i: Int32;
begin
  for i := 0 to High(CoreBuffer) do
  begin
    if (CoreBuffer[i].FileName = 'LCLClasses') or (CoreBuffer[i].FileName = 'MMLClasses') then
      Continue;

    with FunctionList do
      addDeclarations(CoreBuffer[i].Items, addSimbaSection(CoreBuffer[i].FileName), False, True);
  end;
end;

procedure TSimbaForm.OnSaveScript(const Filename: string);
begin
  with CurrScript do
  begin
    ScriptFile:= SetDirSeparators(Filename);
    ScriptName:= ExtractFileNameOnly(ScriptFile);
    mDebugLn('Script name will be: ' + ScriptName);
    formWriteLn('Successfully saved: ' + ScriptFile);
    StartText:= SynEdit.Lines.Text;
    ScriptChanged := false;
    SynEdit.MarkTextAsSaved;
    CurrTab.TabSheet.Caption:= ScriptName;
    Self.AddRecentFile(ScriptFile);
    UpdateTitle;
  end;
end;

function TSimbaForm.DefaultScript: string;
begin
  Result := 'program new;' + LineEnding +
            'begin' + LineEnding +
            'end.';

  if FileExistsUTF8(SimbaSettings.Editor.DefaultScriptPath.Value) then
  try
    with TStringList.Create do
    try
      LoadFromFile(SimbaSettings.Editor.DefaultScriptPath.Value);

      Result := Text;
    finally
      Free();
    end;
  except
  end;
end;

procedure TSimbaForm.ActionTabLastExecute(Sender: TObject);
var
  CurrIndex : integer;
begin
  CurrIndex := PageControl1.TabIndex;
  if CurrIndex = 0 then
    CurrIndex := Tabs.count - 1
  else
    Dec(CurrIndex);
  PageControl1.TabIndex:= CurrIndex;
end;

procedure TSimbaForm.ActionCloseTabExecute(Sender: TObject);
begin
  if(PageControl1.PageCount > 1) then
  begin
    if Sender is TTabSheet then
      Self.DeleteTab(TTabSheet(Sender).TabIndex,false)
    else
      Self.DeleteTab(PageControl1.TabIndex,false)
  end
  else
    Self.ClearScript;  //DeleteTab would take care of this already, but yeah, it's neater this way.
end;

procedure TSimbaForm.ActionCompileScriptExecute(Sender: TObject);
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

procedure TSimbaForm.ActionCopyExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CopyToClipboard
  else if DebugMemo.Focused then
    DebugMemo.CopyToClipboard;
end;

procedure TSimbaForm.ActionCutExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CutToClipboard
  else if DebugMemo.Focused then
    DebugMemo.CutToClipboard;
end;

procedure TSimbaForm.ActionDeleteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.ClearSelection
  else if DebugMemo.Focused then
    DebugMemo.ClearSelection;
end;

procedure TSimbaForm.ActionExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TSimbaForm.ActionFindNextExecute(Sender: TObject);
begin
  DoSearch([ssoFindContinue], False);
end;

procedure TSimbaForm.ActionFindPrevExecute(Sender: TObject);
begin
  DoSearch([ssoFindContinue, ssoBackwards], False);
end;

procedure TSimbaForm.ActionFindStartExecute(Sender: TObject);
begin
  if (CurrScript.SynEdit.SelAvail) then
      LabeledEditSearch.Text := CurrScript.SynEdit.SelText;

  SearchPanel.Visible := True;
  if LabeledEditSearch.CanFocus then
    LabeledEditSearch.SetFocus;
end;

procedure TSimbaForm.ActionFontExecute(Sender: TObject);
begin
  Editor_EditFont();
end;

procedure TSimbaForm.ActionColorsExecute(Sender: TObject);
begin
  Editor_EditColors();
end;

procedure TSimbaForm.ActionGotoExecute(Sender: TObject);
var
  Value : string;
  P : TPoint;
begin
  Value := '';
  if InputQuery('Goto line','Goto line:',Value) then
  begin
    P.x := 1;
    P.y := StrToIntDef(Value,-1);
    if p.y < 1 then p.y :=1;
    CurrScript.SynEdit.CaretXY := p;
    CurrScript.SynEdit.TopLine:= max(P.y - (CurrScript.SynEdit.LinesInWindow div 2),1);
  end;
end;

procedure TSimbaForm.ActionClearDebugExecute(Sender: TObject);
begin
  DebugMemo.Clear;
end;

procedure TSimbaForm.ActionNewExecute(Sender: TObject);
begin
  //Self.ClearScript;
  Self.AddTab;
end;

procedure TSimbaForm.ActionNewTabExecute(Sender: TObject);
begin
  Self.AddTab;
end;

procedure TSimbaForm.ActionFileBrowserExecute(Sender: TObject);
begin
  SimbaSettings.Form.FileBrowserVisible.Value := not SimbaSettings.Form.FileBrowserVisible.Value;
end;

procedure TSimbaForm.ActionNotesExecute(Sender: TObject);
begin
  SimbaSettings.Form.NotesVisible.Value := not SimbaSettings.Form.NotesVisible.Value;
end;

procedure TSimbaForm.ActionOpenExecute(Sender: TObject);
begin
  Self.OpenScript;
end;

procedure TSimbaForm.ActionPasteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.PasteFromClipboard
  else if DebugMemo.Focused then
    DebugMemo.PasteFromClipboard
  else if LabeledEditSearch.Focused then
    LabeledEditSearch.PasteFromClipboard;
end;

procedure TSimbaForm.ActionPauseExecute(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TSimbaForm.ActionRedoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Redo
  else if DebugMemo.Focused then
    DebugMemo.Undo; //?
end;

procedure TSimbaForm.ActionReplaceExecute(Sender: TObject);
begin
  if (ScriptPopup.HandleAllocated) and (CurrScript.SynEdit.SelAvail) then
    dlgReplace.FindText:= CurrScript.SynEdit.SelText;
  dlgReplace.Execute;
end;

procedure TSimbaForm.ActionRunExecute(Sender: TObject);
begin
  Self.RunScript;
end;

procedure TSimbaForm.ActionSaveAllExecute(Sender: TObject);
var
  i : integer;
  OldIndex : integer;
begin
  OldIndex := PageControl1.TabIndex;
  for i := 0 to Tabs.Count - 1 do
  begin;
    PageControl1.TabIndex:= i;
    SaveCurrentScript;
  end;
  PageControl1.TabIndex:= oldindex;
end;

procedure TSimbaForm.ActionSaveAsExecute(Sender: TObject);
begin
  Self.SaveCurrentScriptAs;
end;

procedure TSimbaForm.ActionSaveDefExecute(Sender: TObject);
begin
  Self.SaveCurrentScriptAsDefault;
end;

procedure TSimbaForm.ActionSaveExecute(Sender: TObject);
begin
  Self.SaveCurrentScript;
end;

procedure TSimbaForm.ActionSelectAllExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.SelectAll
  else if DebugMemo.Focused then
    DebugMemo.SelectAll
  else if LabeledEditSearch.Focused then
    LabeledEditSearch.SelectAll;

end;

procedure TSimbaForm.ActionStopExecute(Sender: TObject);
begin
  Self.StopScript;
end;

procedure TSimbaForm.ActionTabNextExecute(Sender: TObject);
var
  CurrIndex : integer;
begin
  CurrIndex := PageControl1.TabIndex;
  if CurrIndex = Tabs.count - 1 then
    CurrIndex := 0
  else
    Inc(CurrIndex);
  PageControl1.TabIndex:= CurrIndex;
end;

procedure TSimbaForm.ActionUndoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Undo
  else if DebugMemo.Focused then
    DebugMemo.Undo;
end;

procedure TSimbaForm.ChangeMouseStatus(Sender: TObject);
var
  x: integer = -1;
  y: integer = -1;
begin;
  if (not Self.Manager.TargetValid()) then
    Self.Manager.SetDesktop();
  Self.Manager.GetMousePos(x, y);

  StatusBar.Panels[Panel_Coords].Text := Format('(%d, %d)', [x, y]);
end;

procedure TSimbaForm.CheckBoxMatchCaseClick(Sender: TObject);
begin
  RefreshTab;
  CurrScript.SynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable;
  SearchString := LabeledEditSearch.Text;
  DoSearch([], true);
  CurrScript.SynEdit.UseIncrementalColor:= true;

  if LabeledEditSearch.CanFocus then
    LabeledEditSearch.SetFocus;
end;

procedure TSimbaForm.CloseFindPanel;
begin
  SearchPanel.Visible:= false;
  if CurrScript.SynEdit.CanFocus then
    CurrScript.SynEdit.SetFocus;
end;

{
  If we are being sent to the background; then minimize other active windows as
  well.
}
procedure TSimbaForm.doOnHide(Sender: TObject);
begin
  if (not (csDestroying in ComponentState)) and (DebugImgForm <> nil) and DebugImgForm.Showing then
    DebugImgForm.Hide;
end;

procedure TSimbaForm.FileBrowserDoubleClick(Sender: TObject);
begin
  if (FileBrowser.Selected <> nil) then
    LoadScriptFile(TShellTreeNode(FileBrowser.Selected).FullFileName, True, True);
end;

procedure TSimbaForm.StopCodeCompletion;
begin
  CodeCompletionForm.Hide;
end;

function TSimbaForm.FindTab(FileName: string): Integer;
var
  i : integer;
begin
  FileName := SetDirSeparators(filename);
  for i := 0 to SimbaForm.Tabs.Count - 1 do
    {$ifdef MSWindows} //Case insensitive
    if lowercase(TMufasaTab(Tabs[i]).ScriptFrame.ScriptFile) = lowercase(filename) then
    {$else}
    if TMufasaTab(Tabs[i]).ScriptFrame.ScriptFile = filename then
    {$endif}
      exit(i);
  result := -1;
end;

procedure TSimbaForm.FileBrowserRefresh(Sender: TObject);
begin
  FileBrowser.Items[0].Expand(False);
end;

procedure TSimbaForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i : integer;
begin
  if (length(FileNames) = 1) then
  begin
    LoadScriptFile(FileNames[0]); //One file saves us some work
    exit;
  end;
  if (length(FileNames) > 5) then //> 5 seems nice to me, cant imagine you want to open that many scripts on a regular base
    case MessageDlg('Are you sure you want to open '+inttostr(length(filenames))+
                    ' scripts?', mtConfirmation, mbYesNo, 0)  of
      IDNO: exit;
    end;
  {$IfDef WINDOWS}
  //Fix for the really old Windows kernel bug which probably will never be fixed
  for i := 1 to high(filenames) do
   LoadScriptFile(FileNames[i],true);
  LoadScriptFile(FileNames[0],true);
  {$Else} //in this case its tolerable as Windows is the only OS with this bug
  for i := 0 to high(filenames) do
   LoadScriptFile(FileNames[i],true);
  {$EndIf};
end;

procedure TSimbaForm.DebugMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((ssCtrl in Shift) and (not ((ssShift in Shift) or (ssAlt in Shift)))) then
  begin
    if (Key = VK_A) then DebugMemo.SelectAll;
  end;
  //Are there any more?
end;

procedure TSimbaForm.ShowDTMEditor(Sender: TObject);
begin
  TDTMForm.Create(Manager, DebugMemo).Show();
end;

procedure TSimbaForm.ShowFormDesigner(Sender: TObject);
begin
  {$IFDEF USE_FORMDESIGNER}
  CompForm.ShowOnTop();
  {$ENDIF}
end;

procedure TSimbaForm.UnloadPlugin(Sender: TObject);
begin
  Plugins.Unload(TMenuItem(Sender).Caption);
end;

// Populate unload plugin items
procedure TSimbaForm.MenuToolsClick(Sender: TObject);
var
  Files: TStringArray;
  RefCounts: TIntegerArray;
  i: Int32;
  Item: TMenuItem;
begin
  MenuItemUnloadPlugin.Clear();

  Plugins.GetAll(Files, RefCounts);

  if (Length(Files) > 0) then
  begin
    for i := 0 to High(Files) do
    begin
      Item := TMenuItem.Create(MenuItemUnloadPlugin);
      Item.Caption := Files[i];
      Item.Enabled := RefCounts[i] = 0;
      Item.OnClick := @UnloadPlugin;
      if (not Item.Enabled) then
        Item.Caption := Item.Caption + ' [' + IntToStr(RefCounts[i]) + ']';

      MenuItemUnloadPlugin.Add(Item);
    end;
  end else
  begin
    Item := TMenuItem.Create(MenuItemUnloadPlugin);
    Item.Caption := '(none)';
    Item.Enabled := False;

    MenuItemUnloadPlugin.Add(Item);
  end;
end;

procedure TSimbaForm.popupFileBrowserOpenClick(Sender: TObject);
begin
  if (FileBrowser.Selected <> nil) and FileExists(TShellTreeNode(FileBrowser.Selected).FullFileName) then
    LoadScriptFile(TShellTreeNode(FileBrowser.Selected).FullFileName, True, True);
end;

procedure TSimbaForm.popupFileBrowserOpenExternallyClick(Sender: TObject);
begin
  if (FileBrowser.Selected <> nil) and FileExists(TShellTreeNode(FileBrowser.Selected).FullFileName) then
    OpenDocument(TShellTreeNode(FileBrowser.Selected).FullFileName);
end;

procedure TSimbaForm.MenuItemBitmapConvClick(Sender: TObject);
begin
  BitmapConvForm.Show;
end;

procedure TSimbaForm.MenuItemHandbookClick(Sender: TObject);
begin
  OpenURL('http://docs.villavu.com/simba/');
end;

procedure TSimbaForm.MenuItemColourHistoryClick(Sender: TObject);
begin
  MenuItemColourHistory.Checked := not ColourHistoryForm.Visible;
  if MenuItemColourHistory.Checked then
    ColourHistoryForm.Show
  else
    ColourHistoryForm.Hide;
end;

procedure TSimbaForm.dlgReplaceFind(Sender: TObject);
begin
  SearchString := dlgReplace.FindText;
  DoSearch([ssoFindContinue], False);
end;

procedure TSimbaForm.dlgReplaceReplace(Sender: TObject);
var
  SOptions: TSynSearchOptions;
  P: TPoint;
  Y: Boolean;
  Btns: TMsgDlgButtons;

 procedure Replace;
 begin
   CurrScript.SynEdit.SearchReplaceEx(dlgReplace.FindText, dlgReplace.ReplaceText, SOptions + [ssoReplace], P);
 end;

begin
  Y:= not (frPromptOnReplace in dlgReplace.Options);
  SOptions:= [];
  if(frMatchCase in dlgReplace.Options)then SOptions:= [ssoMatchCase];
  if(frWholeWord in dlgReplace.Options)then SOptions+= [ssoWholeWord];
  with CurrScript.SynEdit do
  begin
    Btns:= [mbYes, mbNo];
    if(frReplaceAll in dlgReplace.Options)then Btns+= [mbYesToAll];
    if(frEntireScope in dlgReplace.Options)then P:= Classes.Point(0, 0) else P:= CaretXY;
    while SearchReplaceEx(dlgReplace.FindText, '', SOptions, P) > 0 do
    begin
      if(Y)then
        Replace
      else case MessageDlg('Replace', Format('Do you want to replace "%s" with "%s"?', [dlgReplace.FindText, dlgReplace.ReplaceText]), mtConfirmation, Btns, 0) of
        mrYes: Replace;
        mrYesToAll: begin
                      Replace;
                      Y:= True;
                    end;
      end;
      if(not(frReplaceAll in dlgReplace.Options))then exit;
      P:= CaretXY;
    end;
  end;
end;

procedure TSimbaForm.EditSearchChange(Sender: TObject);
begin
  SearchString := LabeledEditSearch.Text;
  DoSearch([], true);
end;

procedure TSimbaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Int32;
begin
  Exiting := True;

  FunctionList.Stop();

  if (Assigned(Tabs)) then
    for i := Tabs.Count - 1 downto 0 do
      if not DeleteTab(i,true) then
      begin
        CloseAction := caNone;
        Exit;
      end;

  CloseAction := caFree;
end;

{$IFDEF WINDOWS}
function SetDLLDirectory(Directory: PChar): LongBool; stdcall; external 'kernel32.dll' name 'SetDllDirectoryA';
{$ENDIF}

procedure TSimbaForm.FormCreate(Sender: TObject);
var
  Mode: UInt32 = 0;
begin
  Application.OnException := @CustomExceptionHandler;

  AppPath := IncludeTrailingPathDelimiter(Application.Location);
  DataPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(Application.Location) + 'AppData');

  if (not DirectoryIsWritable(Application.Location)) then
    ShowMessage('No permission to write to Simba''s directory. Run as ' + {$IFDEF WINDOWS} 'administrator' {$ELSE} 'sudo' {$ENDIF} + ' if this causes issues.');
  if (not DirectoryExists(DataPath)) and (not CreateDir(DataPath)) then
    ShowMessage('Unable to create data path');

  // On windows:
  // 1) Terminate simba when command prompt is closed.
  // 2) Disable windows features that suspend Simba while interacting with the console.
  {$IFDEF WINDOWS}
  SetConsoleCtrlHandler(@ConsoleHandler, True);

  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode and not (ENABLE_QUICK_EDIT_MODE or ENABLE_INSERT_MODE));
  {$ENDIF}

  Settings_Init();
  Environment_Init();
  Utilities_Init();

  Form_LoadSettings();

  SimbaColorsForm := TSimbaColors.Create(Self);
  PackageForm := TPackageForm.Create(Self, TB_ShowPackages);

  Manager := TIOManager.Create(SimbaSettings.Environment.PluginPath.Value);
  Picker := TMColorPicker.Create(Manager);
  Selector := TMWindowSelector.Create(Manager);

  CodeCompletionForm := TAutoCompletePopup.Create(Self);
  CodeCompletionForm.InsertProc := @OnCompleteCode;

  ParamHint := TParamHint.Create(Self);

  Application.QueueASyncCall(@Parameters_Handle, 0);
  Application.QueueASyncCall(@Parameters_RunTest, 0);
  Application.QueueASyncCall(@CodeTools_Initialize, 0);
  Application.QueueASyncCall(@SimbaNews_Execute, 0);

  Plugins.Paths.Add(SimbaSettings.Environment.PluginPath.Value);

  Tabs := TList.Create();
  AddTab();

  {$IFDEF WINDOWS}
    {$IFDEF CPU32}
      {$i openssl32.lrs}
    {$ELSE}
      {$i openssl64.lrs}
    {$ENDIF}

    {$IFDEF CPU32}
    if not DirectoryExists(IncludeTrailingPathDelimiter(DataPath + 'libs32')) then
      CreateDir(IncludeTrailingPathDelimiter(DataPath + 'libs32'));

    SetDLLDirectory(PChar(IncludeTrailingPathDelimiter(DataPath + 'libs32')));

    DLLSSLName := IncludeTrailingPathDelimiter(DataPath + 'libs32') + 'ssleay32.dll';
    DLLUtilName := IncludeTrailingPathDelimiter(DataPath + 'libs32') + 'libeay32.dll';
    {$ELSE}
    if not DirectoryExists(IncludeTrailingPathDelimiter(DataPath + 'libs64')) then
      CreateDir(IncludeTrailingPathDelimiter(DataPath + 'libs64'));

    SetDLLDirectory(PChar(IncludeTrailingPathDelimiter(DataPath + 'libs64')));

    DLLSSLName := IncludeTrailingPathDelimiter(DataPath + 'libs64') + 'ssleay32.dll';
    DLLUtilName := IncludeTrailingPathDelimiter(DataPath + 'libs64') + 'libeay32.dll';
    {$ENDIF}

    if (not FileExists(DLLSSLName)) then
      with TLazarusResourceStream.Create('ssleay32', nil) do
      try
        SaveToFile(DLLSSLName);
      finally
        Free();
      end;

    if (not FileExists(DLLUtilName)) then
      with TLazarusResourceStream.Create('libeay32', nil) do
      try
        SaveToFile(DLLUtilName);
      finally
        Free();
      end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  Bind_Windows_Keys();
  {$ENDIF}

  {$IFDEF LINUX_HOTKEYS}
  Bind_Linux_Keys();
  {$ENDIF}
end;

procedure TSimbaForm.FormDestroy(Sender: TObject);
var
  i: Int32;
begin
  if Assigned(Tabs) then
    for i := Tabs.Count - 1 downto 0 do
      TMufasaTab(Tabs[i]).Free();

  if Assigned(Tabs) then
    FreeAndNil(Tabs);

  if Assigned(Selector) then
    FreeAndNil(Selector);
  if Assigned(Picker) then
    FreeAndNil(Picker);
  if Assigned(Manager) then
    FreeAndNil(Manager);

  if Assigned(ParamHint) then
    FreeAndNil(ParamHint);

  {$IFDEF MSWindows}
  Unbind_Windows_Keys();
  {$ENDIF}
  {$IFDEF LINUX_HOTKEYS}
  Unbind_Linux_Keys();
  {$ENDIF}

  Form_SaveSettings();
end;

procedure TSimbaForm.FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
begin
  SetEditActions;
  Handled := ActionList.IsShortCut(Msg);
end;

procedure TSimbaForm.LabeledEditSearchEnter(Sender: TObject);
begin
  SearchStart := CurrScript.SynEdit.LogicalCaretXY;
  with CurrScript.SynEdit do
  begin
    UseIncrementalColor:= true;
    MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable
  end;
end;

procedure TSimbaForm.LabeledEditSearchExit(Sender: TObject);
begin
  if not CheckBoxMatchCase.MouseEntered then
    RefreshTab;
end;

procedure TSimbaForm.LabeledEditSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (key = vk_f) then
  begin;
    LabeledEditSearch.SelectAll;
  end else
  if key = VK_ESCAPE then
  begin
    CloseFindPanel;
    key := 0;
  end;
end;

procedure TSimbaForm.LabeledEditSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin;
    SearchString:= LabeledEditSearch.Text;
    DoSearch([ssoFindContinue], True);
    key := #0;
//    LabeledEditSearch.SelStart:= Length(LabeledEditSearch.Text);
  end;
end;

procedure TSimbaForm.MenuEditClick(Sender: TObject);
begin
  SetEditActions;
end;

procedure TSimbaForm.MenuItemAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TSimbaForm.MenuItemCloseTabsClick(Sender: TObject);
begin
  Self.CloseTabs;
end;

procedure TSimbaForm.MenuItemDebugImageClick(Sender: TObject);
begin
  MenuItemDebugImage.Checked := not DebugImgForm.Visible;
  if MenuItemDebugImage.Checked then
    DebugImgForm.Show
  else
    DebugImgForm.Hide;
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
  SimbaSettingsForm.ShowModal;
end;

procedure TSimbaForm.MenuItemShowClick(Sender: TObject);
begin
  Self.Show;
  Self.WindowState := wsNormal;
end;

procedure TSimbaForm.MenuItemTabCloseClick(Sender: TObject);
begin
  DeleteTab(PopupTab,false);
end;

procedure TSimbaForm.MenuItemTabCloseOthersClick(Sender: TObject);
begin
  CloseTabs(PopupTab);
end;

procedure TSimbaForm.MenuItemReadOnlyTabClick(Sender: TObject);
var
  Tab: TMufasaTab;

begin
  Tab := TMufasaTab(Tabs[PopupTab]);
  if Tab.ScriptFrame.ScriptFile = '' then
  begin
    MessageDlg('Script needs to be saved before it can be put in Read Only mode',
                       mtWarning, [mbOK], 0);
    Exit;
  end;

  Tab.ScriptFrame.SetReadOnly(not Tab.ScriptFrame.GetReadOnly());
  MenuItemReadOnlyTab.Checked := not Tab.ScriptFrame.GetReadOnly();
end;

procedure TSimbaForm.MenuItemFunctionListClick(Sender: TObject);
begin
  SimbaSettings.Form.FunctionListVisible.Value := not SimbaSettings.Form.FunctionListVisible.Value;
end;

procedure TSimbaForm.MTrayIconClick(Sender: TObject);
begin
  Self.Show();
  if Self.CanFocus() then
    Self.SetFocus();
end;

procedure TSimbaForm.ButtonPickClick(Sender: TObject);
var
   Selected: record
     Color: Int32;
     X, Y: Int32;
   end;
   ClipboardText: String;
begin
  if (not Picker.Picking) then
  begin
    Picker.Pick(Selected.Color, Selected.X, Selected.Y);

    ColourHistoryForm.AddColObj(TColourPickerObject.Create(Selected.Color, TPoint.Create(Selected.X, Selected.Y), ''), True);
    ColourHistoryForm.Show();

    if SimbaSettings.ColorPicker.AddColorToClipBoard.Value then
      ClipboardText := IntToStr(Selected.Color)
    else
    if SimbaSettings.ColorPicker.AddCoordinateToClipBoard.Value then
      ClipboardText := IntToStr(Selected.X) + ', ' + IntToStr(Selected.Y)
    else
    if SimbaSettings.ColorPicker.AddColorAndCoordinateToClipboard.Value then
      ClipboardText := IntToStr(Selected.Color) + ' ' + IntToStr(Selected.X) + ', ' + IntToStr(Selected.Y)
    else
      ClipboardText := '';

    try
      Clipboard.AsText := ClipboardText;
    except
    end;

    with Selected do
      formWriteLn(Format('Picked color %d at (%d, %d)', [Color, X, Y]));
  end;
end;

procedure TSimbaForm.ButtonSelectorDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Manager.SetTarget(Selector.Drag);

  formWriteLn('New window: ' + IntToStr(Selector.LastPick));
end;

procedure TSimbaForm.PageControl1Change(Sender: TObject);
begin
  RefreshTab();
  UpdateTitle();
end;

procedure TSimbaForm.ButtonTrayClick(Sender: TObject);
begin
  MTrayIcon.Show;
  self.hide;
end;

procedure TSimbaForm.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  LastTab := PageControl1.TabIndex;

  AllowChange := True;
end;

procedure TSimbaForm.PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  PopupTab := PageControl1.IndexOfPageAt(MousePos);
  if PopupTab = -1 then
  begin
    mDebugLn('We couldn''t find which tab you clicked on, closing the popup');
    Handled := true;
  end else
  begin
    MenuItemReadOnlyTab.Checked := TMufasaTab(Tabs[PopupTab]).ScriptFrame.GetReadOnly();
  end;
end;

procedure TSimbaForm.PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  NewPos : integer;
  OldPos : integer;
begin
  if sender <> PageControl1 then
    exit;
  NewPos := PageControl1.IndexOfPageAt(Classes.Point(x,y));
  OldPos := PageControl1.TabIndex;
  if (NewPos <> OldPos) and (NewPos <> -1) then
  begin;
    Tabs.Move(OldPos,NewPos);
    PageControl1.Pages[OldPos].PageIndex := NewPos;
  end;
end;

procedure TSimbaForm.PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Pos: Integer;
begin
  Pos := PageControl1.IndexOfPageAt(Classes.Point(x,y));
  Accept := (Pos <> PageControl1.TabIndex) and (Pos <> -1);
end;

procedure TSimbaForm.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbLeft)then
  begin
    {$ifdef linux}
    PageControl1.TabIndex := PageControl1.IndexOfPageAt(Point(x,y));
    {$endif}
    PageControl1.BeginDrag(false, 10);
  end;
end;

procedure TSimbaForm.PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbMiddle) and (not(PageControl1.Dragging))then
    if(PageControl1.IndexOfPageAt(Classes.Point(x,y)) <> -1)then
      DeleteTab(PageControl1.IndexOfPageAt(Classes.Point(x,y)), False);
end;

procedure TSimbaForm.PopupItemFindClick(Sender: TObject);
begin
  SearchString := CurrScript.SynEdit.SelText;
  ActionFindNextExecute(ScriptPopup);
end;

function TSimbaForm.GetScriptState: TScriptState;
begin
  result := CurrScript.FScriptState;
end;

procedure TSimbaForm.SetScriptState(const State: TScriptState);
begin
  CurrScript.FScriptState:= State;
  with Self.StatusBar.panels[Panel_State] do
    case state of
      ss_Running : begin Text := 'Running'; TB_Run.Enabled:= False; TB_Pause.Enabled:= True;
                         TB_Stop.ImageIndex := Image_Stop; TB_Stop.Enabled:= True;
                         TrayPlay.Checked := True; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := True;
                         TrayStop.Enabled:= True; TrayStop.Checked := False;
                   end;
      ss_Paused  : begin Text := 'Paused'; TB_Run.Enabled:= True; TB_Pause.Enabled := False;
                         TB_Stop.ImageIndex := Image_Stop; TB_Stop.Enabled:= True;
                         TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := True; TrayPause.Enabled := True;
                         TrayStop.Enabled:= True; TrayStop.Checked:= False;
                   end;
      ss_Stopping: begin Text := 'Stopping'; TB_Run.Enabled:= False; TB_Pause.Enabled:= False; TB_Stop.Enabled:= True;
                         TB_Stop.ImageIndex := Image_Terminate;
                         TrayPlay.Checked := False; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := False;
                         TrayStop.Enabled:= True; TrayStop.Checked:= True;

                   end;
      ss_None    : begin Text := 'Done'; TB_Run.Enabled:= True; TB_Pause.Enabled:= False; TB_Stop.Enabled:= False;
                         TB_Stop.ImageIndex := Image_Stop;
                         TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := False; TrayPause.Enabled := False;
                         TrayStop.Enabled:= False; TrayStop.Checked:= False;
                   end;
    end;
end;

procedure TSimbaForm.ActionConsoleExecute(Sender: TObject);
begin
  SimbaSettings.Form.ConsoleVisible.Value := not SimbaSettings.Form.ConsoleVisible.Value;
end;

procedure TSimbaForm.FileBrowserExpand(Sender: TObject; Node: TTreeNode);
var
  i: Int32;
begin
  for i := 0 to Node.Count - 1 do
    case TShellTreeNode(Node.Items[i]).IsDirectory of
    True:
      begin
        Node.Items[i].ImageIndex := 54;
        Node.Items[i].SelectedIndex := 54;
      end;

    False:
      begin
        if ExtractFileExt(TShellTreeNode(Node.Items[i]).FullFilename) = '.simba' then
        begin
          Node.Items[i].ImageIndex := 8;
          Node.Items[i].SelectedIndex := 8;
        end else
        begin
          Node.Items[i].ImageIndex := 56;
          Node.Items[i].SelectedIndex := 56;
        end;
      end;
  end;
end;

procedure TSimbaForm.UpdateTitle;
begin
  Application.Title:= PChar('Simba'); // XXX - Sure you want to do this for Disguise?
  if CurrScript.ScriptChanged then
  begin;
    CurrTab.TabSheet.Caption:= CurrScript.ScriptName + '*';
    Self.Caption := Format(WindowTitle,[CurrScript.ScriptName + '*']);
    ActionSaveScript.Enabled:= True;
  end else
  begin;
    ActionSaveScript.Enabled:= False;
    CurrTab.TabSheet.Caption:= CurrScript.ScriptName;
    Self.Caption := Format(WindowTitle,[CurrScript.ScriptName]);
  end;
  StatusBar.Panels[Panel_ScriptName].Text:= CurrScript.ScriptName;
  StatusBar.Panels[Panel_ScriptPath].text:= CurrScript.ScriptFile;
end;

function TSimbaForm.OpenScript: Boolean;
var
  i: Integer;
  OpenInNewTab : boolean;
begin
  Result := False;
  OpenInNewTab := True;
  //OpenInNewTab := SimbaSettings.Tab.OpenScriptInNewTab.GetDefValue(True);
  if not OpenInNewTab then
    if CanExitOrOpen = false then
      Exit;
  with TOpenDialog.Create(nil) do
  try
    if (CurrScript.ScriptFile <> '') then
      InitialDir := ExtractFileDir(CurrScript.ScriptFile)
    else
      InitialDir := SimbaSettings.Environment.ScriptPath.Value;
    Options := [ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofViewDetail];
    Filter:= 'Simba Files|*.simba;*.simb;*.cogat;*.mufa;*.txt;*.pas;|Any files|*.*';
    if Execute then
    begin
      Result := True;
      for i := 0 to Files.Count - 1 do
        if (not FileExistsUTF8(Files[i])) or (not LoadScriptFile(Files[i])) then
        begin
          Result := False;
          Break;
        end;
    end;
  finally
    Free;
  end;
  if result then
    UpdateTitle;
end;

function TSimbaForm.LoadScriptFile(FileName: String; AlwaysOpenInNewTab: Boolean; CheckOtherTabs: Boolean): Boolean;
var
  OpenInNewTab: boolean;
  CheckTabsFirst: boolean;
  Tab: integer;
begin
  OpenInNewTab := true;
  CheckTabsFirst := True;

  if FileExistsUTF8(FileName) then
  begin
    if CheckTabsFirst then
    begin
      Tab :=  FindTab(filename);
      if tab <> -1 then
      begin
        TMufasaTab(Tabs[tab]).ScriptFrame.MakeActiveScriptFrame;
        CurrScript := TMufasaTab(Tabs[tab]).ScriptFrame;
        exit(true);
      end;
    end;
    if OpenInNewTab and (CurrScript.SynEdit.Text <> CurrScript.ScriptDefault) then //Add la tab!
      Self.AddTab();
    with CurrScript do
    begin
      filename := SetDirSeparators(filename);
      SynEdit.Lines.LoadFromFile(filename);

      StartText := SynEdit.Lines.text;

      ScriptName:= ExtractFileNameOnly(filename);
      mDebugLn('Script name will be: ' + ScriptName);
      ScriptFile:= FileName;
      ScriptChanged := false;
      AddRecentFile(filename);
      RefreshTab();
      Result := True;
    end;
  end;
  if Result then
    UpdateTitle;
end;

function TSimbaForm.SaveCurrentScript: Boolean;
begin
  if CurrScript.GetReadOnly() then
  begin
    formWriteln('Script is in read-only/external editor mode. Not saving!');

    Exit(False);
  end;

  if not CurrScript.ScriptChanged then
  begin
    mDebugLn('SaveScript - no changes.');

    Exit(False);
  end;

  with CurrScript do
  begin
    Result := (ScriptFile <> '');

    if Result then
    begin
      try
        SynEdit.Lines.SaveToFile(ScriptFile);
      except
        mDebugLn('Cannot save the file. Try specifying a different location.');

        Exit(SaveCurrentScriptAs());
      end;
      OnSaveScript(scriptfile);
    end
    else
      Result := SaveCurrentScriptAs();
  end;
end;

function TSimbaForm.SaveCurrentScriptAs: Boolean;
var
  ScriptFile : string;
begin
  Result := false;
  with TSaveDialog.Create(nil) do
  try
    if (CurrScript.ScriptFile <> '') then
      InitialDir := ExtractFileDir(CurrScript.ScriptFile)
    else
      InitialDir := SimbaSettings.Environment.ScriptPath.Value;
    filter := 'Simba Files|*.simba;*.simb;*.cogat;*.mufa;*.txt;*.pas;|Any files|*.*';
    if Execute then
    begin;
      if ExtractFileExt(FileName) = '' then
      begin;
        ScriptFile := FileName + '.simba';
      end else
        ScriptFile := FileName;
      CurrScript.SynEdit.Lines.SaveToFile(ScriptFile);
      OnSaveScript(scriptfile);
    end;
  finally
    free;
  end;
end;

function TSimbaForm.SaveCurrentScriptAsDefault: Boolean;
begin
  try
    CurrScript.SynEdit.Lines.SaveToFile(SimbaSettings.Editor.DefaultScriptPath.Value);

    Exit(True);
  except
    Exit(False);
  end;
end;

function TSimbaForm.CanExitOrOpen: Boolean;
begin;
  Self.Enabled := False;//We HAVE to answer the popup
  Result := True;

  if (ScriptState <> ss_None) then
  begin
    if ScriptState <> ss_Stopping then
    begin
      result := False;
      case MessageDlg('Script is still running', 'Do you want to stop the script?',
                        mtConfirmation, mbYesNoCancel, 0) of
                          mrYes: StopScript;
                        end;
    end else
      case MessageDlg('Script is stopping.', 'Do you want to terminate the script?',
                      mtConfirmation, mbYesNoCancel, 0) of
                        mrNo, mrCancel: Result := false;
                        mrYes: StopScript;
                      end;
  end;

  if Result and (CurrScript.StartText <> CurrScript.SynEdit.Lines.text) then
  begin
    case MessageDlg('Script has been modified.', 'Do you want to save the script?',
                mtConfirmation, mbYesNoCancel, 0) of
          mrCancel : Result := False;
          mrYes : Result := SaveCurrentScript;
    end;
  end;
  Self.Enabled := True;
  if Self.CanFocus then
    Self.SetFocus;
end;

function TSimbaForm.ClearScript: Boolean;
begin
  result := false;
  if CanExitOrOpen then
  begin;
    result := true;
    CurrTab.Clear;
    RefreshTab();
  end;
end;


{ TMufasaTab }

procedure TMufasaTab.Clear;
begin
  ScriptFrame.Free;
  ScriptFrame := TScriptFrame.Create(Tabsheet);
  ScriptFrame.Parent := Tabsheet;
  ScriptFrame.Align:= alClient;
end;

constructor TMufasaTab.Create(Page: TPageControl);
begin
  inherited Create;
  PageCtrl := Page;
  Tabsheet := TTabSheet.Create(Page);
  Tabsheet.PageControl := Page;
  ScriptFrame := TScriptFrame.Create(Tabsheet);
  ScriptFrame.Parent := Tabsheet;
  ScriptFrame.Align := alClient;
end;

destructor TMufasaTab.Destroy;
begin
//  ScriptFrame.Free;
  TabSheet.Free;
  inherited Destroy;
end;

initialization
  FormatSettings.DecimalSeparator := '.';

{$R *.lfm}

end.
