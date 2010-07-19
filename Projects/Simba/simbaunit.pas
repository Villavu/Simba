{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

    Simba/GUI for the Mufasa Macro Library
}

{TODO: Implement Disguise and Status bars}

unit SimbaUnit;

{$undef EditButtons}
{$Undef ProcessMessages} //Define this for processmessages in ThreadSafeCall
{$mode objfpc}{$H+}

interface

uses
  {$ifdef linux}cthreads,{$endif}Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, SynEdit, SynHighlighterPas,
  //Client,
  MufasaTypes,
  mmlpsthread,synedittypes,
  {$IFDEF MSWINDOWS} os_windows, windows,{$ENDIF} //For ColorPicker etc.
  {$IFDEF LINUX} os_linux, {$ENDIF} //For ColorPicker etc.
  colourpicker, framescript, windowselector, lcltype, ActnList,
  SynExportHTML, SynEditKeyCmds, SynEditHighlighter,
  SynEditMarkupHighAll, LMessages, Buttons,mmisc,
  stringutil,mufasatypesutil,mufasabase,  v_ideCodeParser,
  about, framefunctionlist, ocr, updateform, Simbasettings, psextension, virtualextension,
  extensionmanager, settingssandbox, v_ideCodeInsight, CastaliaPasLexTypes,
  CastaliaSimplePasPar, v_AutoCompleteForm, PSDump;

const
  SimbaVersion = 703;

type

  { TMufasaTab }

  TMufasaTab = class(Tobject)
  private
    PageCtrl : TPageControl;
  public
    TabSheet : TTabsheet;
    ScriptFrame : TScriptFrame;
    procedure Clear;//This will 'reset' the ScriptFrame
    constructor Create(Page : TPageControl);
    destructor Destroy; override;
  end;

//  Tab
  { TSimbaForm }

  TSimbaForm = class(TForm)
    ActionSaveDef: TAction;
    ActionConsole: TAction;
    ActionNormalSize: TAction;
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
    frmFunctionList: TFunctionListFrame;
    LabeledEditSearch: TLabeledEdit;
    LazHighlighter: TSynPasSyn;
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItemSaveDef: TMenuItem;
    MenuItemBitmapConv: TMenuItem;
    MenuItemExtensions: TMenuItem;
    MenuItemSettingsButton: TMenuItem;
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
    NewsTimer: TTimer;
    FunctionListTimer: TTimer;
    SCARHighlighter: TSynPasSyn;
    TT_Console: TToolButton;
    TT_Cut: TToolButton;
    TT_Copy: TToolButton;
    TT_Paste: TToolButton;
    ToolButton9: TToolButton;
    UpdateTimer: TTimer;
    ToolButton3: TToolButton;
    TT_Update: TToolButton;
    UpdateMenuButton: TMenuItem;
    MenuitemFillFunctionList: TMenuItem;
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
    MenuItemDivider: TMenuItem;
    PageControl1: TPageControl;
    ScriptPopup: TPopupMenu;
    SearchPanel: TPanel;
    ScriptPanel: TPanel;
    SpeedButtonSearch: TSpeedButton;
    SplitterFunctionList: TSplitter;
    TabPopup: TPopupMenu;
    TB_SaveAll: TToolButton;
    DebugTimer: TTimer;
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
    ToolBar1: TToolBar;
    TB_Run: TToolButton;
    TB_Pause: TToolButton;
    TB_Stop: TToolButton;
    ToolButton1: TToolButton;
    TB_ReloadPlugins: TToolButton;
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
    procedure ActionClearDebugExecute(Sender: TObject);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionCompileScriptExecute(Sender: TObject);
    procedure ActionConsoleExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindstartExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNewTabExecute(Sender: TObject);
    procedure ActionNormalSizeExecute(Sender: TObject);
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
    procedure editSearchListExit(Sender: TObject);
    procedure editSearchListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editSearchListKeyPress(Sender: TObject; var Key: char);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FunctionListChange(Sender: TObject; Node: TTreeNode);
    procedure FunctionListEnter(Sender: TObject);
    procedure FunctionListExit(Sender: TObject);
    procedure FunctionListTimerTimer(Sender: TObject);
    procedure MenuItemBitmapConvClick(Sender: TObject);
    procedure MenuItemExtensionsClick(Sender: TObject);
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
    procedure LabeledEditSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEditSearchKeyPress(Sender: TObject; var Key: char);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemCloseTabsClick(Sender: TObject);
    procedure MenuItemDebugImageClick(Sender: TObject);
    procedure MenuItemExportHTMLClick(Sender: TObject);
    procedure MenuitemFillFunctionListClick(Sender: TObject);
    procedure MenuItemHideClick(Sender: TObject);
    procedure MenuItemReportBugClick(Sender: TObject);
    procedure MenuItemSettingsButtonClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemTabCloseClick(Sender: TObject);
    procedure MenuItemTabCloseOthersClick(Sender: TObject);
    procedure MenuItemFunctionListClick(Sender: TObject);
    procedure MTrayIconClick(Sender: TObject);
    procedure NewsTimerTimer(Sender: TObject);
    procedure OnLinePSScript(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PickerPick(Sender: TObject; const Colour, colourx,
      coloury: integer);
    procedure PopupItemFindClick(Sender: TObject);
    procedure ProcessDebugStream(Sender: TObject);
    procedure RecentFileItemsClick(Sender: TObject);
    procedure ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ScriptPopupPopup(Sender: TObject);
    procedure SpeedButtonSearchClick(Sender: TObject);
    procedure SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure TB_ReloadPluginsClick(Sender: TObject);
    procedure ThreadOpenConnectionEvent(Sender: TObject; var url: string;
      var Continue: boolean);
    procedure ThreadOpenFileEvent(Sender: TObject; var Filename: string;
      var Continue: boolean);
    procedure ThreadWriteFileEvent(Sender: TObject; var Filename: string;
      var Continue: boolean);
    procedure ScriptStartEvent(Sender: TObject; var Script : string; var Continue : boolean);
    procedure TrayPopupPopup(Sender: TObject);
    procedure TT_UpdateClick(Sender: TObject);
    procedure UpdateMenuButtonClick(Sender: TObject);
    procedure UpdateTimerCheck(Sender: TObject);

    procedure OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    procedure OnCompleteCode(Str: string);
    function OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
    function OnCCLoadLibrary(Sender: TObject; var LibName: string; out ci: TCodeInsight): Boolean;
  private
    PopupTab : integer;
    RecentFileItems : array of TMenuItem;
    RecentFiles : TStringList;
    FirstRun : boolean;//Only show the warnings the first run (path not existing one's)
    SearchStart : TPoint;
    LastTab  : integer;
    UpdatingFonts : boolean;
    OpenConnectionData : TOpenConnectionData;
    OpenFileData : TOpenFileData;
    WriteFileData : TWriteFileData;
    ScriptStartData : TScriptStartData;
    procedure HandleConnectionData;
    procedure HandleOpenFileData;
    procedure HandleWriteFileData;
    procedure HandleScriptStartData;
    function GetDefScriptPath: string;
    function GetScriptPath : string;
    function GetExtPath: string;
    function GetFontPath: String;
    function GetHighlighter: TSynCustomHighlighter;
    function GetIncludePath: String;
    function GetPluginPath: string;
    function GetScriptState: TScriptState;
    function GetShowParamHintAuto: boolean;
    function GetShowCodeCompletionAuto: Boolean;
    function GetSimbaNews: String;
    procedure SetDefScriptPath(const AValue: string);
    procedure SetExtPath(const AValue: string);
    procedure SetFontPath(const AValue: String);
    procedure SetIncludePath(const AValue: String);
    procedure SetPluginPath(const AValue: string);
    procedure SetScriptPath(const AValue: string);
    procedure SetShowParamHintAuto(const AValue: boolean);
    procedure SetShowCodeCompletionAuto(const AValue: boolean);
    procedure SetScriptState(const State: TScriptState);
    function LoadSettingDef(const Key, Def : string) : string;
    function CreateSetting(const Key, Value : string) : string;
    procedure SetSetting(const key,Value : string; save : boolean = false);
    function SettingExtists(const key : string) : boolean;
    procedure FontUpdate;
  public
    DebugStream: String;
    SearchString : string;
    CurrScript : TScriptFrame; //The current scriptframe
    CurrTab    : TMufasaTab; //The current TMufasaTab
    CodeCompletionForm: TAutoCompletePopup;
    CodeCompletionStart: TPoint;
    ParamHint : TParamHint;
    Tabs : TList;
    Manager: TIOManager;
    OCR_Fonts: TMOCR;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    OnScriptStart : TScriptStartEvent;
    {$ifdef mswindows}
    ConsoleVisible : boolean;
    procedure ShowConsole( ShowIt : boolean);
    {$endif}
    procedure FunctionListShown( ShowIt : boolean);
    property ScriptState : TScriptState read GetScriptState write SetScriptState;
    procedure SafeCallThread;
    function OpenScript : boolean;
    function LoadScriptFile(filename : string; AlwaysOpenInNewTab : boolean = false; CheckOtherTabs : boolean = true) : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function SaveCurrentScriptAsDefault : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
    procedure RunScript;
    procedure PauseScript;
    procedure StopScript;
    procedure AddTab;
    procedure StopCodeCompletion;
    function FindTab(filename : string) : integer;
    function DeleteTab( TabIndex : integer; CloseLast : boolean; Silent : boolean = false) : boolean;
    procedure ClearTab( TabIndex : integer);
    procedure CloseTabs(Exclude: integer = -1; Silent : boolean = false); //-1 for no exclusion
    procedure SetEditActions;
    procedure DoSearch(Next : boolean; HighlightAll : boolean);
    procedure RefreshTab;//Refreshes all the form items that depend on the Script (Panels, title etc.)
    procedure RefreshTabSender(sender : PtrInt);
    procedure CreateDefaultEnvironment;
    procedure LoadFormSettings;
    procedure SaveFormSettings;
    procedure LoadExtensions;
    procedure AddRecentFile(const filename : string);
    procedure InitalizeTMThread(var Thread : TMThread);
    procedure HandleParameters;
    procedure OnSaveScript(const Filename : string);
    property ShowParamHintAuto : boolean read GetShowParamHintAuto write SetShowParamHintAuto;
    property ShowCodeCompletionAuto: Boolean read GetShowCodeCompletionAuto write SetShowCodeCompletionAuto;
    property IncludePath : String read GetIncludePath write SetIncludePath;
    property FontPath : String read GetFontPath write SetFontPath;
    property PluginPath : string read GetPluginPath write SetPluginPath;
    property ExtPath : string read GetExtPath write SetExtPath;
    property ScriptDir : string read GetScriptPath write SetScriptPath;
    property DefScriptPath : string read GetDefScriptPath write SetDefScriptPath;
    property CurrHighlighter : TSynCustomHighlighter read GetHighlighter;
  end;

  procedure ClearDebug;
  procedure formWriteln( S : String);
  procedure formWritelnEx( S : String);
  function GetMethodName( Decl : string; PlusNextChar : boolean) : string;

const
  WindowTitle = 'Simba - %s';//Title, where %s = the place of the filename.
  Panel_State = 0;
  Panel_Coords = 1;
  Panel_ScriptName = 2;
  Panel_ScriptPath = 3;


  Image_Stop = 7;
  Image_Terminate = 19;
var
  SimbaForm: TSimbaForm;
  MainDir : string;
  {$ifdef MSWindows}
  PrevWndProc : WNDPROC;
  {$endif}
  CurrentSyncInfo : TSyncInfo;//We need this for SafeCallThread

implementation
uses
   lclintf,
   syncobjs, // for the critical sections
   debugimage,
   files,
   InterfaceBase,
   bitmapconv,
   extensionmanagergui,
   colourhistory,
   math;

{$ifdef mswindows}
function ConsoleHandler( eventType : DWord) : WINBOOL;stdcall;
begin
  TThread.Synchronize(nil,@SimbaForm.Close);
  Result := true;
end;


function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam;
  lParam: LParam): LRESULT stdcall;
begin
  if uMsg = WM_HOTKEY then
  begin
    SimbaForm.ActionStopScript.Execute;
    Result := 0;
  end else
    Result := Windows.CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
end;

{$endif}

var
   DebugCriticalSection: syncobjs.TCriticalSection;

procedure TSimbaForm.OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  if (Typ = meNotSupported) then
    Exit;
  if (Sender is TmwSimplePasPar) then
    if (TmwSimplePasPar(Sender).Lexer.TokenID = tok_DONE) then
      Exit;
  mDebugLn('ERROR: '+Format('%d:%d %s', [Y + 1, X, Msg])+' in '+TCodeInsight(Sender).FileName);
end;

procedure TSimbaForm.OnCompleteCode(Str: string);
var
  sp, ep: Integer;
  s: string;
begin
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
var
  Temp : string;
begin
  Temp := FindFile(filename,[MainDir+DS,IncludePath]);
  if temp <> '' then
  begin;
    filename := temp;
    result := true;
  end else
    result := false;
end;

function TSimbaForm.OnCCLoadLibrary(Sender: TObject; var LibName: string; out ci: TCodeInsight): Boolean;
var
  i, Index: Integer;
  b: TStringList;
  ms: TMemoryStream;
begin
  try
    Index := PluginsGlob.LoadPlugin(LibName);
  except
    Result := false;
    Index := -1;
  end;
  if (Index < 0) then
    Exit(False)
  else
  begin
    b := TStringList.Create;
    try
      ms := TMemoryStream.Create;

      with PluginsGlob.MPlugins[Index] do
        for i := 0 to MethodLen - 1 do
          b.Add(Methods[i].FuncStr + 'forward;');
      ci := TCodeInsight.Create;
      with ci do
      begin
        OnMessage := @SimbaForm.OnCCMessage;
        b.SaveToStream(ms);
        FileName := LibName;
        Run(ms, nil, -1, True);
      end;
    finally
      b.Free;
    end;
  end;
end;

procedure TSimbaForm.HandleConnectionData;
var
  Args : TVariantArray;
begin
  SetLength(Args,2);
  Args[0] := OpenConnectionData.URL^;
  Args[1] := OpenConnectionData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onOpenConnection].HookName,Args);
    OpenConnectionData.URL^ := Args[0];
    OpenConnectionData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleConnectiondata: ' + e.message);
  end;
end;

function TSimbaForm.GetDefScriptPath: string;
begin
  result :=LoadSettingDef('Settings/SourceEditor/DefScriptPath', ExpandFileName(MainDir+DS+'default.simba'));
end;

function TSimbaForm.GetScriptPath: string;
begin
  result :=IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Scripts/Path', ExpandFileName(MainDir+DS+'Scripts' + DS)));
end;

procedure TSimbaForm.HandleOpenFileData;
var
  Args : TVariantArray;
begin
  SetLength(Args,2);
  Args[0] := OpenFileData.FileName^;
  Args[1] := OpenFileData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onOpenFile].HookName,Args);
    OpenFileData.FileName^ := Args[0];
    OpenFileData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleOpenFileData: ' + e.message);
  end;
end;

procedure TSimbaForm.HandleWriteFileData;
var
  Args : TVariantArray;
begin
  SetLength(Args,2);
  Args[0] := WriteFileData.FileName^;
  Args[1] := WriteFileData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onWriteFile].HookName,Args);
    WriteFileData.FileName^ := Args[0];
    WriteFileData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleWriteFileData: ' + e.message);
  end;
end;

procedure TSimbaForm.HandleScriptStartData;
var
  Args : TVariantArray;
begin
  SetLength(Args,2);
  Args[0] := ScriptStartData.Script^;
  Args[1] := ScriptStartData.Continue^;
  try
    ExtManager.HandleHook(EventHooks[SExt_onScriptStart].HookName,Args);
    ScriptStartData.Script^ := Args[0];
    ScriptStartData.Continue^ := Args[1];
  except
    on e : Exception do
      mDebugLn('ERROR in HandleScriptStartData: ' + e.message);
  end;
end;

procedure TSimbaForm.ProcessDebugStream(Sender: TObject);
begin
  if length(DebugStream) = 0 then
    Exit;

  // cut off 1 newline char

  DebugCriticalSection.Enter;

  try
    setlength(DebugStream, length(DebugStream) - 1);
    Memo1.Lines.Add(DebugStream);
    SetLength(DebugStream, 0);
  finally
    DebugCriticalSection.Leave;
  end;
end;

procedure TSimbaForm.RecentFileItemsClick(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to high(RecentFileItems) do
    if RecentFileItems[i] = sender then
    begin;
      LoadScriptFile(RecentFiles[RecentFiles.Count - 1 -i]);//Inverse order
      exit;
    end;
end;

procedure TSimbaForm.ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer);
begin
  if(X <= (ScriptPanel.Width div 2))then
  begin
    frmFunctionList.Align := alLeft;
    PageControl1.Align := alRight;
    SplitterFunctionList.ResizeAnchor := akLeft;
    SplitterFunctionList.Align := alLeft;
    SplitterFunctionList.Left := frmFunctionList.Left + frmFunctionList.Width;
  end else begin
    frmFunctionList.Align := alRight;
    PageControl1.Align := alLeft;
    SplitterFunctionList.ResizeAnchor := akRight;
    SplitterFunctionList.Align := alRight;
    SplitterFunctionList.Left := frmFunctionList.Left;
  end;
  PageControl1.Width := ScriptPanel.Width - (Source.DockRect.Right - Source.DockRect.Left);
  frmFunctionList.Width := ScriptPanel.Width - PageControl1.Width;
  PageControl1.Align := alClient;
  SplitterFunctionList.Show;
end;

procedure TSimbaForm.ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; //is there a better way to do all of this?
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
   P: TPoint;
begin
  Accept := frmFunctionList.DragKind = dkDock;
  if(Accept)then
  begin
    P := ScriptPanel.ClientToScreen(Classes.Point(0, 0));
    if(X <= (ScriptPanel.Width div 2))then
      Source.DockRect := Classes.Rect(P.x, P.y, min(P.x + frmFunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y + ScriptPanel.Height)
    else
      Source.DockRect := Classes.Rect(max(P.x + ScriptPanel.Width - frmFunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y, P.x + ScriptPanel.Width, P.y + ScriptPanel.Height);
  end;
end;

procedure TSimbaForm.ScriptPopupPopup(Sender: TObject);
begin
  SetEditActions;
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

procedure TSimbaForm.TB_ReloadPluginsClick(Sender: TObject);
begin
//  PluginsGlob.FreePlugins;
end;

procedure TSimbaForm.ThreadOpenConnectionEvent(Sender: TObject; var url: string;var Continue: boolean);
begin
  OpenConnectionData.Sender := Sender;
  OpenConnectionData.URL:= @URL;
  OpenConnectionData.Continue:= @Continue;
  TThread.Synchronize(nil,@HandleConnectionData);
end;

procedure TSimbaForm.ThreadOpenFileEvent(Sender: TObject; var Filename: string;
  var Continue: boolean);
begin
  OpenFileData.Sender := Sender;
  OpenFileData.FileName:= @FileName;
  OpenFileData.Continue:= @Continue;
  TThread.Synchronize(nil,@HandleOpenFileData);
end;

procedure TSimbaForm.ThreadWriteFileEvent(Sender: TObject; var Filename: string;
  var Continue: boolean);
begin
  WriteFileData.Sender := Sender;
  WriteFileData.FileName:= @FileName;
  WriteFileData.Continue:= @Continue;
  TThread.Synchronize(nil,@HandleWriteFileData);
end;

procedure TSimbaForm.TrayPopupPopup(Sender: TObject);
begin
  MenuItemHide.enabled:= SimbaForm.Visible;
  {$ifdef MSWindows}
  MenuItemShow.Enabled:= not SimbaForm.Visible;
  if SimbaForm.Visible then
    if SimbaForm.CanFocus then
      SimbaForm.SetFocus;
  {$endif}
end;

procedure TSimbaForm.TT_UpdateClick(Sender: TObject);
begin
  SimbaUpdateForm.ShowModal;
  TT_Update.Visible:=False;
end;

procedure TSimbaForm.UpdateTimerCheck(Sender: TObject);
var
   chk: String;
   time:integer;
  LatestVersion : integer;
begin
  UpdateTimer.Interval:= MaxInt;
  FontUpdate;
  chk := LowerCase(LoadSettingDef('Settings/Updater/CheckForUpdates','True'));

  if chk <> 'true' then
    Exit;

  LatestVersion:= SimbaUpdateForm.GetLatestSimbaVersion;
  if LatestVersion > SimbaVersion then
  begin;
    TT_Update.Visible:=True;
    formWritelnEx('A new update of Simba is available!');
    formWritelnEx(format('Current version is %d. Latest version is %d',[SimbaVersion,LatestVersion]));
  end else
  begin
    mDebugLn(format('Current Simba version: %d',[SimbaVersion]));
    mDebugLn('Latest Simba Version: ' + IntToStr(LatestVersion));
  end;
  time := StrToIntDef(LoadSettingDef('Settings/Updater/CheckEveryXMinutes','30'),30);
  UpdateTimer.Interval:= time {mins} * 60 {secs} * 1000 {ms};//Every half hour
end;

procedure TSimbaForm.UpdateMenuButtonClick(Sender: TObject);
begin
  SimbaUpdateForm.ShowModal;
end;

procedure ClearDebug;
begin
  {$IFNDEF MSWINDOWS}
  SimbaForm.ProcessDebugStream(nil);
  {$ENDIF}
  TThread.Synchronize(nil,@SimbaForm.Memo1.Clear);
end;

procedure formWriteln( S : String);
begin
  mDebugLn('formWriteln: ' + s);
  {$ifdef MSWindows}
  //Ha, we cán acces the debugmemo
  SimbaForm.Memo1.Lines.Add(s);
  {$else}
  DebugCriticalSection.Enter;
  try
    s := s + MEOL;
    SimbaForm.DebugStream:= SimbaForm.DebugStream + s;
  finally
    DebugCriticalSection.Leave;
  end;
  {$endif}
end;

//{$ENDIF}

procedure TSimbaForm.RunScript;
begin
  with CurrScript do
  begin
    if ScriptState = ss_Paused then
    begin;
      ScriptThread.Resume;
      ScriptState := ss_Running;
      Exit;
    end else
    if ScriptState <> ss_None then
    begin;
      FormWritelnEx('The script hasn''t stopped yet, so we cannot start a new one.');
      exit;
    end;
    InitalizeTMThread(scriptthread);
    ScriptThread.CompileOnly:= false;
    ScriptThread.OnTerminate:=@ScriptThreadTerminate;
    ScriptState:= ss_Running;
    FirstRun := false;
    //Lets run it!
    ScriptThread.Resume;
  end;
end;

procedure TSimbaForm.PauseScript;
begin
  with CurrScript do
  begin;
    if ScriptState = ss_Running then
    begin;
      {$ifdef MSWindows}
      ScriptThread.Suspended:= True;
      ScriptState:= ss_Paused;
      {$else}
      mDebugLn('Linux users are screwed, no pause button for u!');
      {$endif}
    end else if ScriptState = ss_Paused then
    begin;
      ScriptThread.Resume;
      ScriptState := ss_Running;
    end;
  end;
end;

procedure TSimbaForm.StopScript;
begin
  with CurrScript do
  begin;
    case ScriptState of
      ss_Stopping:
        begin    //Terminate the thread the tough way.
          mDebugLn('Terminating the Scriptthread');
          mDebugLn('Exit code terminate: ' +inttostr(KillThread(ScriptThread.Handle)));
          WaitForThreadTerminate(ScriptThread.Handle, 0);
          ScriptThread.Free;
          ScriptState := ss_None;
        end;
      ss_Running:
        begin
          ScriptThread.Terminate;
          ScriptState := ss_Stopping;
        end;
      ss_Paused:
        begin
          ScriptThread.Resume;
          ScriptThread.Terminate;
          ScriptState:= ss_Stopping;
        end;
    end;
  end;
end;

procedure TSimbaForm.AddTab;
var
  Tab : TMufasaTab;
begin;
  Tab := TMufasaTab.Create(Self.PageControl1);
  Tabs.Add(Tab);
  Tab.TabSheet.ImageIndex:= 8;
//  Tab.TabSheet.OnContextPopup:= @TabPopup;
  PageControl1.TabIndex:= Tabs.Count - 1;
  RefreshTab;
  if tabs.count > 1 then
  begin;
    TB_SaveAll.Enabled:= True;
    MenuItemSaveAll.Enabled:= True;
  end;
end;

function TSimbaForm.DeleteTab(TabIndex: integer; CloseLast : boolean; Silent : boolean = false) : boolean;
var
  Tab : TMufasaTab;
  OldIndex : integer;//So that we can switch back, if needed.
begin
  if not Silent then
  begin;
    OldIndex := PageControl1.TabIndex;
    if TabIndex = OldIndex then
    begin;
      if lowercase(LoadSettingDef('Settings/Tabs/OpenNextOnClose','False')) = 'false' then
        OldIndex := LastTab //We are closing the 'current'  tab, lets go back in history
      else
        OldIndex := Min(Tabs.Count - 1,OldIndex + 1);
    end;
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

procedure TSimbaForm.ClearTab(TabIndex: integer);
begin
  TMufasaTab(Tabs[TabIndex]).Clear;
end;

procedure TSimbaForm.CloseTabs(Exclude: integer = -1; Silent : boolean = false);
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
  else if Memo1.Focused then
    with Memo1 do
      EditActions(CanUndo,False,SelText <>'',SelText <> '',True,SelText <> '')
  else
    EditActions(false,false,false,false,false,false);
end;

procedure TSimbaForm.DoSearch(Next: boolean; HighlightAll : boolean);
var
  Res : integer;
  CurrPos : TPoint;
  SearchOptions : TSynSearchOptions;
begin
  SearchOptions:= [];
  if CheckBoxMatchCase.Checked then
    SearchOptions := [ssoMatchCase];
  if SearchString = '' then
  begin
    res := -1;
    CurrScript.Synedit.SetHighlightSearch('',[]);
//    CurrScript.SynEdit.SelectionMode:=
//    CurrScript.SynEdit.CaretXY :=     CurrScript.SynEdit.CaretXY;
    CurrScript.SynEdit.LogicalCaretXY := SearchStart;
  end
  else
  begin
    mDebugLn('Searching: ' + SearchString);
    if next then
      CurrPos := CurrScript.SynEdit.LogicalCaretXY
    else
      CurrPos := SearchStart;
    Res := CurrScript.SynEdit.SearchReplaceEx(SearchString,'',SearchOptions,CurrPos);
    if res = 0 then
    begin
      res := CurrScript.SynEdit.SearchReplaceEx(SearchString,'',SearchOptions,Classes.Point(0,0));
      if res > 0 then
      begin;
        mDebugLn('End of document reached');
        SearchStart.x := 0;
        SearchStart.Y := CurrScript.SynEdit.LogicalCaretXY.y;
      end;
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
        SetHighlightSearch(SearchString,[])
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
  if NewTab < 0 then exit;
  Tab := TMufasaTab(Tabs[Newtab]);
  Script := Tab.ScriptFrame;
  Self.CurrScript := Script;
  Self.CurrTab := Tab;
  if Script.ScriptChanged then
  begin;
    Tab.TabSheet.Caption:= Script.ScriptName + '*';
    Self.Caption := Format(WindowTitle,[Script.ScriptName + '*'])
  end else
  begin;
    Tab.TabSheet.Caption:= Script.ScriptName;
    Self.Caption := Format(WindowTitle,[Script.ScriptName]);
  end;
  StatusBar.Panels[Panel_ScriptName].Text:= Script.ScriptName;
  StatusBar.Panels[Panel_ScriptPath].text:= Script.ScriptFile;
  SetScriptState(Tab.ScriptFrame.FScriptState);//To set the buttons right
  if Self.Showing then
    if Tab.TabSheet.TabIndex = Self.PageControl1.TabIndex then
      if CurrScript.SynEdit.CanFocus then
        CurrScript.SynEdit.SetFocus;
  StopCodeCompletion;//To set the highlighting back to normal;
  frmFunctionList.LoadScriptTree(CurrScript.SynEdit.Text);
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
end;

procedure TSimbaForm.RefreshTabSender(sender: PtrInt);
begin
  RefreshTab;
end;

procedure TSimbaForm.CreateDefaultEnvironment;
var
  PluginsPath,extensionsPath : string;
begin
  CreateSetting('Settings/Updater/CheckForUpdates','True');
  CreateSetting('Settings/Updater/CheckEveryXMinutes','30');
  CreateSetting('Settings/Interpreter/UseCPascal', 'False');
  CreateSetting('Settings/Fonts/LoadOnStartUp', 'True');
  CreateSetting('Settings/Fonts/Version','-1');
  CreateSetting('Settings/Tabs/OpenNextOnClose','False');
  CreateSetting('Settings/Tabs/OpenScriptInNewTab','True');
  CreateSetting('Settings/Tabs/CheckTabsBeforeOpen','True');
  CreateSetting('Settings/ColourPicker/ShowHistoryOnPick', 'True');
  CreateSetting('Settings/General/MaxRecentFiles','10');
  CreateSetting('Settings/MainForm/NormalSize','739:555');
  CreateSetting('Settings/FunctionList/ShowOnStart','True');
  CreateSetting('Settings/CodeHints/ShowAutomatically','True');
  CreateSetting('Settings/CodeCompletion/ShowAutomatically','True');
  CreateSetting('Settings/SourceEditor/LazColors','True');
  CreateSetting('Settings/Extensions/FileExtension','sex');

  CreateSetting('Settings/Updater/RemoteLink',SimbaURL + 'Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF});
  CreateSetting('Settings/Updater/RemoteVersionLink',SimbaURL + 'Version');
  CreateSetting('Settings/Fonts/VersionLink', FontURL + 'Version');
  CreateSetting('Settings/Fonts/UpdateLink', FontURL + 'Fonts.tar.bz2');

  CreateSetting('Settings/News/URL', 'http://simba.villavu.com/bin/news');

  {Creates the paths and returns the path}
  PluginsPath := CreateSetting('Settings/Plugins/Path', ExpandFileName(MainDir+ DS+ 'Plugins' + DS));
  extensionsPath := CreateSetting('Settings/Extensions/Path',ExpandFileName(MainDir +DS + 'Extensions' + DS));
  CreateSetting('Extensions/ExtensionCount','0');
  CreateSetting('LastConfig/MainForm/Position','');
  CreateSetting('LastConfig/MainForm/State','Normal');
  {$ifdef MSWindows}
  CreateSetting('LastConfig/Console/Visible','True');
  ShowConsole(True);
  {$endif}
  CreateSetting('Settings/Tray/AlwaysVisible', 'True');
  if not DirectoryExists(IncludePath) then
    CreateDir(IncludePath);
  if not DirectoryExists(FontPath) then
    CreateDir(FontPath);
  if not DirectoryExists(PluginsPath) then
    CreateDir(PluginsPath);
  if not DirectoryExists(extensionsPath) then
    CreateDir(extensionsPath);
  if not DirectoryExists(ExtPath) then
    CreateDir(ExtPath);
  if not DirectoryExists(ScriptDir) then
    CreateDir(ScriptDir);
  SettingsForm.SettingsTreeView.Items.GetFirstNode.Expand(false);
  SettingsForm.SaveCurrent;
  LoadFormSettings;
  UpdateTimer.Interval:=25;
end;

procedure TSimbaForm.LoadFormSettings;
var
  str,str2 : string;
  Data : TStringArray;
  i,ii : integer;
begin
  self.BeginFormUpdate;
  str := LoadSettingDef('LastConfig/MainForm/Position','');
  if str <> '' then
  begin;
    Data := Explode(':',str);
    if length(Data) <> 4 then
      Exit;
    Self.Left:= StrToIntDef(Data[0],Self.Left);
    Self.Top:= StrToIntDef(Data[1],self.top);
    Self.Width:= StrToIntDef(Data[2],self.width);
    Self.Height:= StrToIntDef(Data[3],self.height);
  end;
  str := lowercase(LoadSettingDef('LastConfig/MainForm/State','Normal'));
  if str = 'maximized' then
    self.windowstate := wsMaximized
  else
//  if str = 'normal' then
    Self.WindowState := wsNormal;
  if SettingExtists('LastConfig/MainForm/RecentFiles/Count') then
  begin;
    ii := StrToIntDef(LoadSettingDef('LastConfig/MainForm/RecentFiles/Count','-1'),-1);
    for i := 0 to ii do
    begin
      str := LoadSettingDef('LastConfig/MainForm/RecentFiles/File' + inttostr(I),'');
      if str <> '' then
        AddRecentFile(str);
    end;
  end;
  str := LowerCase(LoadSettingDef('Settings/FunctionList/ShowOnStart','True'));
  str2 := lowercase(LoadSettingDef('LastConfig/MainForm/FunctionListShown',''));
  if (str = 'true') or (str2 = 'true') then
    FunctionListShown(True)
  else
    FunctionListShown(false);
  {$ifdef MSWindows}
  str := LowerCase(LoadSettingDef('LastConfig/Console/Visible','True'));
  if str = 'true' then
    ShowConsole(True)
  else
    ShowConsole(false);
  {$endif}
  if Lowercase(LoadSettingDef('Settings/Tray/AlwaysVisible', 'True')) <> 'true' then
  begin
    MTrayIcon.Hide;
    writeln('Hiding tray.');
  end;
  self.EndFormUpdate;
end;

procedure TSimbaForm.SaveFormSettings;
var
  Data : TStringArray;
  path : string;
  i : integer;
begin
  with SettingsForm.Settings do
  begin
    if Self.WindowState = wsMaximized then
      SetSetting('LastConfig/MainForm/State','maximized')
    else
    begin; //Only save the form position if its non maximized.
      SetSetting('LastConfig/MainForm/State','normal');
      Data := ConvArr([inttostr(Self.left),inttostr(self.top),inttostr(self.width),inttostr(self.height)]);
      SetSetting('LastConfig/MainForm/Position', Implode(':',Data ));
    end;
    DeleteKey('LastConfig/MainForm/RecentFiles');
    if RecentFiles.Count > 0 then
    begin
      SetSetting('LastConfig/MainForm/RecentFiles/Count',inttostr(RecentFiles.Count));
      SetLength(data,RecentFiles.Count);
      for i := 0 to RecentFiles.Count - 1 do
        SetSetting('LastConfig/MainForm/RecentFiles/File'+inttostr(i),RecentFiles[i]);
    end;
    if MenuItemFunctionList.Checked then
      SetSetting('LastConfig/MainForm/FunctionListShown','True')
    else
      SetSetting('LastConfig/MainForm/FunctionListShown','False');
    {$ifdef MSWindows}
    if ConsoleVisible then
      SetSetting('LastConfig/Console/Visible','True')
    else
      SetSetting('LastConfig/Console/Visible','False');
    {$endif}
    SetSetting('Extensions/ExtensionCount',inttostr(ExtManager.Extensions.Count));
    for i := 0 to ExtManager.Extensions.Count-1 do
    begin;

      path :='Extensions/Extension' + inttostr(I);
      SetSetting(Path + '/Path',TVirtualSimbaExtension(ExtManager.Extensions[i]).Filename);
      SetSetting(Path + '/Enabled',BoolToStr(TVirtualSimbaExtension(ExtManager.Extensions[i]).Enabled,True));
    end;
    SaveToXML(SimbaSettingsFile);
  end;
end;

procedure TSimbaForm.LoadExtensions;
var
  extCount : integer;
  function LoadExtension(Number : integer) : boolean;
  var
    Path : string;
    ExtPath : string;
    ExtEnabled : boolean;
  begin;
    result := false;
    if (number < 0) or (number >= extCount) then
      exit;
    path := 'Extensions/Extension' + inttostr(number);
    if SettingExtists(Path) = false then
      exit;
    ExtPath := LoadSettingDef(Path + '/Path','');
    if ExtPath = '' then
      exit;
    ExtEnabled := StrToBoolDef(LoadSettingDef(Path + '/Enabled','false'),false);
    if ExtManager.LoadPSExtension(ExtPath,ExtEnabled) = false then
      exit;
    Result := true;
  end;
  procedure DeleteExtension(number : integer);
  var
    i : integer;
    path : string;
  begin;
    path := 'Extensions/Extension';
    SettingsForm.Settings.DeleteKey(path + inttostr(number));
    for i := number + 1 to extCount - 1 do
      SettingsForm.Settings.RenameKey(path + inttostr(i),'Extension' + inttostr(i-1));
    SetSetting('Extensions/ExtensionCount',inttostr(extCount - 1),true);
    dec(extCount);
  end;

var
  str,str2 : string;
  i : integer;
begin
  extCount := StrToIntDef(LoadSettingDef('Extensions/ExtensionCount/','0'),0);
  for i := 0 to extCount - 1 do
    while (i < extCount) and not LoadExtension(i) do
      DeleteExtension(i);
  SetSetting('Extensions/ExtensionCount',inttostr(extCount));
  str := LoadSettingDef('Settings/Extensions/Path',ExpandFileName(MainDir +DS + 'Extensions' + DS));
  str2 := LoadSettingDef('Settings/Extensions/FileExtension','sex');
  ExtManager.LoadPSExtensionsDir(str,str2);
end;

procedure TSimbaForm.AddRecentFile(const filename: string);
var
  MaxRecentFiles : integer;
  Len,i : integer;
begin
  MaxRecentFiles:= StrToIntDef(LoadSettingDef('Settings/General/MaxRecentFiles','10'),10);
  i := RecentFiles.IndexOf(filename);
  if i <> -1 then
    RecentFiles.Delete(i);
  if RecentFiles.Count = MaxRecentFiles then
    RecentFiles.Delete(0);
  RecentFiles.Add(filename);
  Len := RecentFiles.Count;
  if len <> length(RecentFileItems) then //Not reached maximum yet, add those files!
  begin
    SetLength(RecentFileItems,len);
    RecentFileItems[len-1] := TMenuItem.Create(MenuItemOpenRecent);
    RecentFileItems[len-1].OnClick:=@RecentFileItemsClick;
    MenuItemOpenRecent.Add(RecentFileItems[len-1]);
  end;
  for i := 0 to len - 1 do
    RecentFileItems[len - 1-i].Caption:= ExtractFileName(RecentFiles[i]);
end;

procedure TSimbaForm.InitalizeTMThread(var Thread: TMThread);
var
  DbgImgInfo : TDbgImgInfo;
  AppPath : string;
  ScriptPath : string;
  UseCPascal: String;
  Script : string;
  Se: TMMLSettingsSandbox;
  loadFontsOnScriptStart: boolean;
  Continue : boolean;
begin
  Script :=CurrScript.SynEdit.Lines.Text;
  if Assigned(OnScriptStart) then
  begin
    Continue := True;
    OnScriptStart(Self,script,continue);
    if not Continue then
      exit;
  end;
  AppPath:= MainDir + DS;
  CurrScript.ScriptErrorLine:= -1;
  CurrentSyncInfo.SyncMethod:= @Self.SafeCallThread;
  UseCPascal := LoadSettingDef('Settings/Interpreter/UseCPascal', 'False');
  try
    if lowercase(UseCPascal) = 'true' then
      Thread := TCPThread.Create(True,@CurrentSyncInfo,PluginPath)
    else
      Thread := TPSThread.Create(True,@CurrentSyncInfo,PluginPath);
  except
    mDebugLn('Failed to initialise the library!');
    Exit;
  end;
  {$IFNDEF TERMINALWRITELN}
  Thread.SetDebug(@formWriteln);
  Thread.SetDebugClear(@ClearDebug);
  {$ENDIF}
  Thread.SetScript(Script);
  DbgImgInfo.DispSize := @DebugImgForm.DispSize;
  DbgImgInfo.ShowForm := @DebugImgForm.ShowDebugImgForm;
  DbgImgInfo.ToDrawBitmap:= @DebugImgForm.ToDrawBmp;
  DbgImgInfo.DrawBitmap:= @DebugImgForm.DrawBitmap;
  DbgImgInfo.GetDebugBitmap:= @DebugImgForm.GetDbgBmp;
  DbgImgInfo.GetBitmap:= @DebugImgForm.GetDebugImage;
  Thread.SetDbgImg(DbgImgInfo);
  Thread.ErrorData:= @CurrScript.ErrorData;
  Thread.OnError:= @CurrScript.HandleErrorData;

  if CurrScript.ScriptFile <> '' then
    ScriptPath := IncludeTrailingPathDelimiter(ExtractFileDir(CurrScript.ScriptFile));

  if DirectoryExists(PluginPath) then
     PluginsGlob.AddPath(PluginPath);
  if not DirectoryExists(IncludePath) then
    if FirstRun then
      FormWritelnEx('Warning: The include directory specified in the Settings isn''t valid.');
  if not DirectoryExists(fontPath) then
    if FirstRun then
      FormWritelnEx('Warning: The font directory specified in the Settings isn''t valid. Can''t load fonts now');
  Thread.SetPaths(ScriptPath,AppPath,Includepath,PluginPath,fontPath);

  if selector.haspicked then
    Thread.Client.IOManager.SetTarget(Selector.LastPick);

  loadFontsOnScriptStart := (lowercase(LoadSettingDef('Settings/Fonts/LoadOnStartUp', 'True')) = 'true');
  // Copy our current fonts
  if not assigned(Self.OCR_Fonts) and loadFontsOnScriptStart and DirectoryExists(fontPath) then
  begin
    Self.OCR_Fonts := TMOCR.Create(Thread.Client);
    OCR_Fonts.InitTOCR(fontPath);
    Thread.Client.MOCR.Fonts := OCR_Fonts.Fonts
  end else
    if assigned(Self.OCR_Fonts) and loadFontsOnScriptStart then
      Thread.Client.MOCR.Fonts := OCR_Fonts.Fonts;

  {
    We pass the entire settings to the script; it will then create a Sandbox
    for settings that are exported to the script. This way we can access all
    the settings from the PSTHread, and scripts can only access limited
    resources. Hopefully this won't cause any form / thread related problems?
    (Settings doesn't use the Settings form, iirc)
    Well, it was like this previously as well, we just passed a sandbox to it
    directly, but the sandbox still called Settings.
  }
  Thread.SetSettings(SettingsForm.Settings, SimbaSettingsFile);
  Thread.OpenConnectionEvent:=@ThreadOpenConnectionEvent;
  Thread.WriteFileEvent:=@ThreadWriteFileEvent;
  Thread.OpenFileEvent:=@ThreadOpenFileEvent;
end;

procedure TSimbaForm.HandleParameters;
var
  DoRun : Boolean;
  ErrorMsg : string;
begin
  DoRun := false;
  // paramcount = 1 means we got only one parameter. We assume this to be a file.
  // and try to open it accordingly
  if (Paramcount = 1) and not (Application.HasOption('open')) then
  begin
    writeln('Opening file: ' + ParamStr(1));
    if FileExistsUTF8(ParamStrUTF8(1)) then
      LoadScriptFile(ParamStrUTF8(1));
  end else
  // we have more parameters. Check for specific options. (-r -o, --run --open)
  begin
    ErrorMsg:=Application.CheckOptions('ro:',['run', 'open:']);
    if ErrorMsg <> '' then
    begin
      mDebugLn('ERROR IN COMMAND LINE ARGS: ' + ErrorMSG)
    end else
    begin
      if Application.HasOption('o','open') then
      begin
        writeln('Opening file: ' + Application.GetOptionValue('o','open'));
        LoadScriptFile(Application.GetOptionValue('o','open'));
        DoRun:= Application.HasOption('r','run');
      end else
      // no valid options
      begin
        writeln('No valid command line args are passed');
      end;
    end;
  end;
  if DoRun then
    Self.RunScript;
end;

procedure TSimbaForm.OnSaveScript(const Filename: string);
begin
  with CurrScript do
  begin
    ScriptFile:= SetDirSeparators(Filename);
    ScriptName:= ExtractFileNameOnly(ScriptFile);
    mDebugLn('Script name will be: ' + ScriptName);
    FormWritelnEx('Succesfully saved: ' + ScriptFile);
    StartText:= SynEdit.Lines.Text;
    ScriptChanged := false;
    SynEdit.MarkTextAsSaved;
    Self.Caption:= Format(WindowTitle,[ScriptName]);
    CurrTab.TabSheet.Caption:= ScriptName;
    Self.AddRecentFile(ScriptFile);
    StatusBar.Panels[Panel_ScriptName].Text:= ScriptName;
    StatusBar.Panels[Panel_ScriptPath].text:= ScriptFile;
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
  if(PageControl1.PageCount > 1)then
    Self.DeleteTab(PageControl1.TabIndex,false)
  else
    Self.ClearScript;  //DeleteTab would take care of this already, but yeah, it's neater this way.
end;

procedure TSimbaForm.ActionCompileScriptExecute(Sender: TObject);
var
  TempThread : TMThread;
begin
  InitalizeTMThread(TempThread);
  TempThread.CompileOnly:= true;
  TempThread.Resume;
end;

procedure TSimbaForm.ActionConsoleExecute(Sender: TObject);
begin
  {$ifdef mswindows}
  ShowConsole(not ConsoleVisible);
  {$endif}
end;

procedure TSimbaForm.ActionCopyExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CopyToClipboard
  else if Memo1.Focused then
    Memo1.CopyToClipboard;
end;

procedure TSimbaForm.ActionCutExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CutToClipboard
  else if Memo1.Focused then
    Memo1.CutToClipboard;
end;

procedure TSimbaForm.ActionDeleteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.ClearSelection
  else if Memo1.Focused then
    Memo1.ClearSelection;
end;

procedure TSimbaForm.ActionExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TSimbaForm.ActionFindNextExecute(Sender: TObject);
begin
  DoSearch(true, false);
end;

procedure TSimbaForm.ActionFindstartExecute(Sender: TObject);
begin
  if frmFunctionList.Focused or frmFunctionList.FunctionList.Focused or frmFunctionList.editSearchList.Focused then
  begin
    if frmFunctionList.editSearchList.CanFocus then
      frmFunctionList.editSearchList.SetFocus;
  end else
  begin
    SearchPanel.Visible:= true;
    if LabeledEditSearch.CanFocus then
      LabeledEditSearch.SetFocus;
  end;
end;

procedure TSimbaForm.ActionClearDebugExecute(Sender: TObject);
begin
  Memo1.Clear;
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

procedure TSimbaForm.ActionNormalSizeExecute(Sender: TObject);
var
  SizeStr : string;
  Data : TStringArray;
begin
  SizeStr := LoadSettingDef('Settings/MainForm/NormalSize','739:555');
  Data := Explode(':',SizeStr);
  if length(Data) = 2 then
  begin
    Self.Width:= StrToIntDef(Data[0],739);
    Self.Height:= StrToIntDef(Data[1],555);
  end else
  begin;
    self.width := 739;
    self.height := 555;
  end;
end;

procedure TSimbaForm.ActionOpenExecute(Sender: TObject);
begin
  Self.OpenScript;
end;

procedure TSimbaForm.ActionPasteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.PasteFromClipboard
  else if Memo1.Focused then
    Memo1.PasteFromClipboard
  else if LabeledEditSearch.Focused then
    LabeledEditSearch.PasteFromClipboard
  else if frmFunctionList.editSearchList.Focused then
    frmFunctionList.editSearchList.PasteFromClipboard;

end;

procedure TSimbaForm.ActionPauseExecute(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TSimbaForm.ActionRedoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Redo
  else if Memo1.Focused then
    Memo1.Undo; //?
end;

procedure TSimbaForm.ActionReplaceExecute(Sender: TObject);
begin
  if(ScriptPopup.HandleAllocated)then
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
  else if Memo1.Focused then
    Memo1.SelectAll
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
  else if Memo1.Focused then
    Memo1.Undo;
end;

procedure TSimbaForm.ChangeMouseStatus(Sender: TObject);
var
  x, y: Integer;
begin
  if Self.Manager.TargetValid = false then
    self.Manager.SetDesktop;
  Self.Manager.GetMousePos(x, y);
  if self.Manager.ReceivedError() then
  begin
    FormWritelnEx('Our window no longer exists -> Resetting to desktop');
    self.Manager.SetDesktop;
    self.Manager.ResetError;
  end;
  StatusBar.Panels[Panel_Coords].Text := Format('(%d, %d)', [x, y]);
end;

procedure TSimbaForm.CheckBoxMatchCaseClick(Sender: TObject);
begin
  RefreshTab;
  CurrScript.SynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable;
  SearchString := LabeledEditSearch.Text;
  DoSearch(false, true);
  CurrScript.SynEdit.UseIncrementalColor:= true;
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

procedure TSimbaForm.StopCodeCompletion;
begin
  CodeCompletionForm.Hide;
  if frmFunctionList.InCodeCompletion then
    with CurrScript,frmFunctionList do
    begin;
      editSearchList.Color:= clWhite;
      if FilterTree.Focused then
      begin;
        mDebugLn('This is currently not supported');
        SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
        SynEdit.LogicalCaretXY:= Classes.point(CompletionCaret.x,CompletionCaret.y);
        SynEdit.SelEnd:= SynEdit.SelStart;
      end;
      InCodeCompletion:= false;
      SynEdit.SelectedColor.Style:= [];
      SynEdit.SelectedColor.Foreground:= clHighlightText;
      SynEdit.SelectedColor.Background:= clHighlight;
      Synedit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempEnable;
    end;
end;

function TSimbaForm.FindTab(filename: string): integer;
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

procedure TSimbaForm.editSearchListExit(Sender: TObject);
begin
  frmFunctionList.editSearchList.Color := clWhite;
  StopCodeCompletion;
end;

procedure TSimbaForm.editSearchListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = vk_up then
  begin
    frmFunctionList.Find(True,true);
    key := 0;
  end else
  if key = vk_down then
  begin
    frmFunctionList.Find(true);
    key := 0;
  end;
end;

procedure TSimbaForm.editSearchListKeyPress(Sender: TObject; var Key: char);
var
  linetext : string;
begin
  if key = #13 then//enter
  begin;
    key := #0;
    frmFunctionList.Find(True);
  end else
  if frmFunctionList.InCodeCompletion then
  begin;
    if key = #27 then//esc -> C'est error!
    begin
      key := #0;
      StopCodeCompletion;
      CurrScript.SynEdit.Lines[frmFunctionList.CompletionCaret.y - 1] := frmFunctionList.CompletionStart;
      CurrScript.SynEdit.LogicalCaretXY:= Classes.point(frmFunctionList.CompletionCaret.x,frmFunctionList.CompletionCaret.y);
      CurrScript.SynEdit.SelEnd:= CurrScript.SynEdit.SelStart;
      CurrScript.SynEdit.SetFocus;
    end else
    if key in [' ',',','.','(',')'] then //on on these chars we will insert the function!
    begin;
      StopCodeCompletion;
      linetext := CurrScript.SynEdit.Lines[frmFunctionList.CompletionCaret.y - 1];
      while  (frmFunctionList.CompletionCaret.x <= length(linetext)) and (linetext[frmFunctionList.CompletionCaret.x] in ['a'..'z','A'..'Z','0'..'9','_']) do
        inc(frmFunctionList.CompletionCaret.x);
      CurrScript.SynEdit.LogicalCaretXY:= frmFunctionList.CompletionCaret;
      CurrScript.SynEdit.SelStart:= CurrScript.SynEdit.SelEnd;
      CurrScript.SynEdit.ExecuteCommand(ecChar,key,nil);
      CurrScript.SynEdit.SetFocus;
      key := #0;
    end;
  end;
end;

procedure TSimbaForm.FormDropFiles(Sender: TObject; const FileNames: array of String
  );
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

procedure TSimbaForm.FunctionListChange(Sender: TObject; Node: TTreeNode);
var
  MethodInfo : TMethodInfo;
begin
  if node = nil then
    exit;
  if Node.level = 0 then
    StatusBar.Panels[Panel_ScriptPath].Text := 'Section: ' + Node.Text;
  if (Node.Level > 0) and (Node.Data <> nil) then
  begin
    MethodInfo := PMethodInfo(node.Data)^;
    StatusBar.Panels[Panel_ScriptPath].Text := MethodInfo.MethodStr;
  end;
end;

procedure TSimbaForm.FunctionListEnter(Sender: TObject);
begin
  frmFunctionList.LoadScriptTree(CurrScript.SynEdit.Text);
end;

procedure TSimbaForm.FunctionListExit(Sender: TObject);
begin
//  StatusBar.Panels[2].Text:= '';
end;

procedure TSimbaForm.FunctionListTimerTimer(Sender: TObject);
begin
  if Self.Visible and (CurrScript <> nil) then
    frmFunctionList.LoadScriptTree(CurrScript.SynEdit.Text);
end;

procedure TSimbaForm.MenuItemBitmapConvClick(Sender: TObject);
begin
  BitmapConvForm.Show;
end;

procedure TSimbaForm.MenuItemExtensionsClick(Sender: TObject);
begin
  MenuItemExtensions.Checked := not ExtensionsForm.Visible;
  if MenuItemExtensions.Checked then
    ExtensionsForm.Show
  else
    ExtensionsForm.Hide;
end;

procedure TSimbaForm.MenuItemHandbookClick(Sender: TObject);
begin
  OpenURL('http://wizzup.org/simbadoc/');
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
  DoSearch(True, False);
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
  Y:= False;
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
  SearchString :=LabeledEditSearch.Text;
  DoSearch(false, true);
end;

procedure TSimbaForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i : integer;
begin
  Self.SaveFormSettings;
  for i := Tabs.Count - 1 downto 0 do
    if not DeleteTab(i,true) then
    begin;
      CloseAction := caNone;
      exit;
    end;
  FunctionListTimer.Enabled:= false;
  CloseAction := caFree;
  FreeAndNil(ExtManager);
end;

procedure CCFillCore;
var
  t: TMThread;
  a: TPSScriptExtension;
  b: TStringList;
  ms: TMemoryStream;
  buf : TCodeInsight;
begin
  if SimbaForm.UpdatingFonts then
  begin
    mDebugLn('Updating the fonts, thus waiting a bit till we init the OCR.');
    while SimbaForm.UpdatingFonts do
    begin
      if GetCurrentThreadId = MainThreadID then
        Application.ProcessMessages;
      sleep(25);
    end;
  end;
  SimbaForm.InitalizeTMThread(t);
  KillThread(t.ThreadID);
  if (t is TPSThread) then
  try
    a := TPSScriptExtension.Create(SimbaForm);
    b := TStringList.Create;
    ms := TMemoryStream.Create;

    try
      with TPSThread(t).PSScript do
      begin
        a.OnCompile := OnCompile;
        a.OnCompImport := OnCompImport;
        a.OnExecImport := OnExecImport;
        a.Defines.Assign(Defines);
      end;
      a.GetValueDefs(b);

      CoreDefines.AddStrings(a.Defines);

      buf := TCodeInsight.Create;
      with buf do
      begin
        OnMessage := @SimbaForm.OnCCMessage;
        b.SaveToStream(ms);
        Run(ms, nil, -1, True);
        FileName := '"PSCORE"';
      end;
      SetLength(CoreBuffer, 1);
      CoreBuffer[0] := buf;
    finally
      b.Free;
      a.Free;
    end;
  finally
    //KillThread(t.ThreadID);
    t.Free;
  end;
end;

procedure TSimbaForm.FormCreate(Sender: TObject);
var
  FillThread : TProcThread;
begin
  self.BeginFormUpdate;
  Randomize;
  DecimalSeparator := '.';
  MainDir:= ExtractFileDir(Application.ExeName);
  RecentFiles := TStringList.Create;
  SimbaSettingsFile := MainDir + DS + 'settings.xml';

  //AutoCompletionStart := Point(-1, -1);
  CodeCompletionForm := TAutoCompletePopup.Create(Self);
  CodeCompletionForm.InsertProc := @OnCompleteCode;
  ParamHint := TParamHint.Create(self);

  {$ifdef MSWindows}
  ConsoleVisible := True;
  PrevWndProc := Windows.WNDPROC(GetWindowLong(self.handle,GWL_WNDPROC));
  SetWindowLong(Self.Handle,GWL_WNDPROC,PtrInt(@WndCallback));
  if not RegisterHotkey(Self.Handle,0,MOD_CONTROL or MOD_ALT,VK_S) then
    mDebugLn('Unable to register ctrl + alt + s as global hotkey');
  {$else}
  TT_Console.Visible:= false;
  {$endif}
  InitmDebug;

  Self.OnScriptStart:= @ScriptStartEvent;

  FillThread := TProcThread.Create;
  FillThread.FreeOnTerminate:= True;
  FillThread.NormalProc:= @CCFillCore;
  UpdateTimer.OnTimer:= @UpdateTimerCheck;
  Application.CreateForm(TSimbaUpdateForm, SimbaUpdateForm);
  if FileExistsUTF8(SimbaSettingsFile) then
  begin
    Application.CreateForm(TSettingsForm,SettingsForm);
    Self.LoadFormSettings;
  end else
  begin
    Application.CreateForm(TSettingsForm,SettingsForm);
    Self.CreateDefaultEnvironment;
    FillThread.StartWait:= 250;
  end;

  //Show close buttons @ tabs
  PageControl1.Options:=PageControl1.Options+[nboShowCloseButtons];
  PageControl1.OnCloseTabClicked:=ActionCloseTab.OnExecute;
  Tabs := TList.Create;
  AddTab;//Give it alteast 1 tab ;-).
  Manager := TIOManager.Create; //No need to load plugins for the Global manager
  Picker := TMColorPicker.Create(Manager);
  Picker.OnPick:=@PickerPick;
  Selector := TMWindowSelector.Create(Manager);
  { For writeln }
  SetLength(DebugStream, 0);
  DebugCriticalSection := syncobjs.TCriticalSection.Create;
  {$ifdef mswindows}
  DebugTimer.Enabled:= false;
  {$endif}
  Application.QueueAsyncCall(@RefreshTabSender,0);
  {$ifdef mswindows}
  if FileExists(Application.ExeName+'_old_') then
  begin
    mDebugLn('We still have an out-dated exe file in the dir, lets remove!');
    mDebugLn(format('Sucesfully deleted the file? %s',[BoolToStr(DeleteFile(PChar(Application.ExeName + '_old_')),true)]));
  end;
  SetConsoleCtrlHandler(@ConsoleHandler,true);
  {$endif}
  frmFunctionList.OnEndDock:= @frmFunctionList.FrameEndDock;
  FirstRun := true;//Our next run is the first run.
  HandleParameters;
  TT_Update.Visible:= false;

  //Fill the codeinsight buffer
  FillThread.Resume;
  //Load the extensions
  LoadExtensions;
  self.EndFormUpdate;
end;

procedure TSimbaForm.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  for i := Tabs.Count - 1 downto 0 do
    TMufasaTab(Tabs[i]).Free;
  for i := 0 to high(RecentFileItems) do
    RecentFileItems[i].Free;
  if ExtManager <> nil then
    FreeAndNil(extmanager);
  Tabs.free;
  Selector.Free;
  Picker.Free;
  Manager.Free;
  PluginsGlob.Free;
  SetLength(DebugStream, 0);
  RecentFiles.Free;
  DebugCriticalSection.Free;
  ParamHint.Free;
  {$ifdef MSWindows}
  if not UnRegisterHotkey(Self.Handle,0) then
    mDebugLn('Unable to unregister ctrl + alt + s as global hotkey');
  {$endif}
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
    DoSearch(true, true);
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

procedure TSimbaForm.MenuItemExportHTMLClick(Sender: TObject);
var
  SynExporterHTML : TSynExporterHTML;
begin;
  SynExporterHTML := TSynExporterHTML.Create(nil);
  SynExporterHTML.Highlighter := CurrHighlighter;
  SynExporterHTML.ExportAsText:= True;
  with TSaveDialog.Create(nil) do
    try
      Filter:= 'HTML Files (*.html;*.htm)|*.html;*.htm|All files(*.*)|*.*';
      Options:= [ofOverwritePrompt,ofEnableSizing];
      DefaultExt:= 'html';
      if Execute then
      begin
        if CurrScript.ScriptName <> '' then
          SynExporterHTML.Title:= 'Simba - ' + CurrScript.ScriptName
        else
          SynExporterHTML.Title:= 'Cogat - Untitled';
        SynExporterHTML.ExportAll(CurrScript.SynEdit.Lines);
        SynExporterHTML.SaveToFile(FileName);
      end;
    finally
      free;
      SynExporterHTML.Free;
    end;
end;

procedure formWritelnEx(S: String);
begin
  SimbaForm.Memo1.Lines.Add(s);
end;

function GetMethodName( Decl : string; PlusNextChar : boolean) : string;
var
  I : integer;
  ii : integer;
begin;
  I := pos(' ',Decl) + 1;
  for ii := i to Length(decl) do
  begin;
    if (Decl[ii] = '(') or (Decl[ii] = ';') then
    begin;
      if PlusNextChar then
        result := result + decl[ii];
      exit;
    end;
    if (Decl[ii] = ' ') or (Decl[ii] = ':') then
    begin;
      if PlusNextChar then
        result := result + ' ';
      exit;
    end;
    result := result + decl[ii];
  end;
  //We made it out of the loop.. This is a method without ';' we might wanne add that!
  if PlusNextChar then
    result := result + ';';
end;

procedure TSimbaForm.MenuitemFillFunctionListClick(Sender: TObject);
var
  Methods : TExpMethodArr;
  LastSection : string;
  Sections : TStringList;
  Nodes : array of TTreeNode;
  i : integer;
  Index : integer;
  TempNode : TTreeNode;
  Temp2Node : TTreeNode;
  Tree : TTreeView;
begin
  SetLength(nodes,0);
  frmFunctionList.FunctionList.BeginUpdate;
  if frmFunctionList.FunctionList.Items.Count = 0 then
  begin;
    Methods := TMThread.GetExportedMethods;
    Tree := frmFunctionList.FunctionList;
    Tree.Items.Clear;
    Sections := TStringList.Create;
    LastSection := '';
    frmFunctionList.ScriptNode := Tree.Items.Add(nil,'Script');
    frmFunctionList.IncludesNode := Tree.Items.Add(nil,'Includes');
    for i := 0 to high(Methods) do
    begin;
      if Methods[i].Section <> LastSection then
      begin;
        LastSection := Methods[i].Section;
        Index :=  Sections.IndexOf(LastSection);
        if Index <> -1 then
          TempNode := Nodes[index]
        else
        begin
          TempNode := Tree.Items.Add(nil,LastSection);
          Sections.Add(LastSection);
          setlength(nodes,length(nodes)+1);
          nodes[high(nodes)] := tempNode;
        end;
      end;
      Temp2Node := Tree.Items.AddChild(Tempnode,GetMethodName(Methods[i].FuncDecl,false));
      Temp2Node.Data := GetMem(SizeOf(TMethodInfo));
      FillChar(PMethodInfo(Temp2Node.Data)^,SizeOf(TMethodInfo),0);
      with PMethodInfo(Temp2Node.Data)^ do
      begin
        MethodStr:= strnew(PChar(Methods[i].FuncDecl));
        BeginPos:= -1;
      end;
    end;
    Sections.free;
  end;
  frmFunctionList.FunctionList.EndUpdate;
  if CurrScript <> nil then
    frmFunctionList.LoadScriptTree(CurrScript.SynEdit.Text);
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
  OpenURL('http://mufasa.villavu.com/mantis/bug_report_page.php');
end;

procedure TSimbaForm.MenuItemSettingsButtonClick(Sender: TObject);
begin
  SettingsForm.ShowModal;
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

procedure TSimbaForm.MenuItemFunctionListClick(Sender: TObject);
begin
  FunctionListShown(not MenuItemFunctionList.Checked);
end;

procedure TSimbaForm.MTrayIconClick(Sender: TObject);
begin
  self.Show;
  if Lowercase(LoadSettingDef('Settings/Tray/AlwaysVisible', 'True')) <> 'true' then
    MTrayIcon.Hide;
  if Self.CanFocus then
    self.SetFocus;
end;

function TSimbaForm.GetSimbaNews: String;
var
  t: TDownloadThread;
begin
  t := TDownloadThread.Create(LoadSettingDef('Settings/News/URL', 'http://Simba.villavu.com/bin/news'),
                              @Result);
  t.Resume;
  while not t.done do
  begin
    Application.ProcessMessages;
    Sleep(50);
  end;
end;

procedure TSimbaForm.SetDefScriptPath(const AValue: string);
begin
  SetSetting('Settings/SourceEditor/DefScriptPath',AValue,True);
end;

procedure TSimbaForm.SetExtPath(const AValue: string);
begin
  SetSetting('Settings/Extensions/Path',AValue,true);
end;

procedure TSimbaForm.NewsTimerTimer(Sender: TObject);
var
  s: String;
  News : TStringList; {Need it for correct EOL stuff}
begin
  NewsTimer.Enabled:=False;
  s := GetSimbaNews;
  News := TStringList.Create;
  News.Text:= s;
  Memo1.Lines.AddStrings(News);
  Memo1.Lines.add('');
  News.free;
end;

procedure TSimbaForm.OnLinePSScript(Sender: TObject);
begin
  {$IFDEF ProcessMessages}
  Application.ProcessMessages; //Don't think that this is neccesary though
  {$ENDIF}
end;



procedure TSimbaForm.ButtonPickClick(Sender: TObject);
var
   c, x, y: Integer;
   cobj: TColourPickerObject;
begin
  Picker.Pick(c, x, y);
  cobj := TColourPickerObject.Create(c, Classes.Point(x,y), '');

  { TODO: This should be no problem if the form is hidden? }
  if lowercase(LoadSettingDef('Settings/ColourPicker/AddToHistoryOnPick', 'True')) = 'true' then
    ColourHistoryForm.AddColObj(cobj, true);

  if lowercase(LoadSettingDef('Settings/ColourPicker/ShowHistoryOnPick', 'True')) = 'true' then
    ColourHistoryForm.Show;

  FormWritelnEx('Picked colour: ' + inttostr(c) + ' at (' + inttostr(x) + ', ' + inttostr(y) + ')');
end;


procedure TSimbaForm.ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Manager.SetTarget(Selector.Drag);
  FormWritelnEx('New window: ' + IntToStr(Selector.LastPick));
end;

procedure TSimbaForm.PageControl1Change(Sender: TObject);
begin
  RefreshTab();
end;

procedure TSimbaForm.ButtonTrayClick(Sender: TObject);
begin
  MTrayIcon.Show;
  self.hide;
end;

procedure TSimbaForm.PageControl1Changing(Sender: TObject; var AllowChange: Boolean
  );
begin
  LastTab:= PageControl1.TabIndex;
end;

procedure TSimbaForm.PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  PopupTab := PageControl1.TabIndexAtClientPos(MousePos);
  if PopupTab = -1 then
  begin
    mDebugLn('We couldn''t find which tab you clicked on, closing the popup');
    Handled := true;
  end;
end;

procedure TSimbaForm.PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  NewPos : integer;
  OldPos : integer;
begin
  if sender <> PageControl1 then
    exit;
  NewPos := PageControl1.TabIndexAtClientPos(Classes.Point(x,y));
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
  Pos := PageControl1.TabIndexAtClientPos(Classes.Point(x,y));
  Accept := (Pos <> PageControl1.TabIndex) and (Pos <> -1);
end;

procedure TSimbaForm.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbLeft)then
  begin
    {$ifdef linux}
    PageControl1.TabIndex := PageControl1.TabIndexAtClientPos(Point(x,y));
    {$endif}
    PageControl1.BeginDrag(false, 10);
  end;
end;

procedure TSimbaForm.PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbMiddle) and (not(PageControl1.Dragging))then
    if(PageControl1.TabIndexAtClientPos(Classes.Point(x,y)) <> -1)then
      DeleteTab(PageControl1.TabIndexAtClientPos(Classes.Point(x,y)), False);
end;

procedure TSimbaForm.PickerPick(Sender: TObject; const Colour, colourx,
  coloury: integer);
var
  Args : TVariantArray;
begin
  SetLength(args,3);
  Args[0] := Colour;
  Args[1] := Colourx;
  Args[2] := Coloury;
  ExtManager.HandleHook(EventHooks[SExt_OnColourPick].HookName,Args);
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

function TSimbaForm.GetShowParamHintAuto: boolean;
begin
  Result := LowerCase(LoadSettingDef('Settings/CodeHints/ShowAutomatically','True')) = 'true';
end;

function TSimbaForm.GetShowCodeCompletionAuto: boolean;
begin
  Result := LowerCase(LoadSettingDef('Settings/CodeCompletion/ShowAutomatically','True')) = 'true';
end;

procedure TSimbaForm.SetFontPath(const AValue: String);
begin
  SetSetting('Settings/Fonts/Path',AValue,true);
end;

function TSimbaForm.GetFontPath: String;
begin
  Result := IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Fonts/Path', ExpandFileName(MainDir+DS+'Fonts' + DS)));
end;

function TSimbaForm.GetExtPath: string;
begin
  result :=IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Extensions/Path', ExpandFileName(MainDir+DS+'Extensions' + DS)));
end;

function TSimbaForm.GetHighlighter: TSynCustomHighlighter;
begin
  if lowercase(LoadSettingDef('Settings/SourceEditor/LazColors','True')) = 'true' then
    result := LazHighlighter
  else
    result := SCARHighlighter;
end;

function TSimbaForm.GetIncludePath: String;
begin
  Result := IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Includes/Path', ExpandFileName(MainDir+DS+'Includes' + DS)));
end;

function TSimbaForm.GetPluginPath: string;
begin
  Result := IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Plugins/Path', ExpandFileName(MainDir+DS+'Plugins' + DS)));
end;

procedure TSimbaForm.SetIncludePath(const AValue: String);
begin
  SetSetting('Settings/Includes/Path',AValue,true);
end;

procedure TSimbaForm.SetPluginPath(const AValue: string);
begin
  SetSetting('Settings/Plugins/Path',AValue,true);
end;

procedure TSimbaForm.SetScriptPath(const AValue: string);
begin
  SetSetting('Settings/Scripts/Path',AValue,True);
end;

procedure TSimbaForm.SetScriptState(const State: TScriptState);
begin
  CurrScript.FScriptState:= State;
  with Self.StatusBar.panels[Panel_State] do
    case state of
      ss_Running : begin Text := 'Running'; TB_Run.Enabled:= False; {$ifdef MSWindows}TB_Pause.Enabled:= True; {$endif}
                         TB_Stop.ImageIndex := Image_Stop; TB_Stop.Enabled:= True;
                         TrayPlay.Checked := True; TrayPlay.Enabled := False; {$ifdef MSWindows}TrayPause.Checked := false; TrayPause.Enabled := True;{$endif}
                         TrayStop.Enabled:= True; TrayStop.Checked:= False;
                   end;
      ss_Paused  : begin Text := 'Paused'; TB_Run.Enabled:= True; {$ifdef MSWindows}TB_Pause.Enabled:= True; {$endif}
                         TB_Stop.ImageIndex := Image_Stop; TB_Stop.Enabled:= True;
                         TrayPlay.Checked := false; TrayPlay.Enabled := True; {$ifdef MSWindows}TrayPause.Checked := True; TrayPause.Enabled := True;{$endif}
                         TrayStop.Enabled:= True; TrayStop.Checked:= False;
                   end;
      ss_Stopping: begin Text := 'Stopping';TB_Run.Enabled:= False; TB_Pause.Enabled:= False; TB_Stop.Enabled:= True;
                         TB_Stop.ImageIndex := Image_Terminate;
                         TrayPlay.Checked := False; TrayPlay.Enabled := False; {$ifdef MSWindows}TrayPause.Checked := false; TrayPause.Enabled := False;{$endif}
                         TrayStop.Enabled:= True; TrayStop.Checked:= True;
                   end;
      ss_None    : begin Text := 'Done'; TB_Run.Enabled:= True; TB_Pause.Enabled:= False; TB_Stop.Enabled:= False;
                         TB_Stop.ImageIndex := Image_Stop;
                         TrayPlay.Checked := false; TrayPlay.Enabled := True; {$ifdef MSWindows}TrayPause.Checked := false; TrayPause.Enabled := False;{$endif}
                         TrayStop.Enabled:= false; TrayStop.Checked:= False;
                   end;
    end;
end;

function TSimbaForm.LoadSettingDef(const Key,Def: string): string;
begin
  result := SettingsForm.Settings.GetKeyValueDefLoad(Key,def,SimbaSettingsFile);
end;

function TSimbaForm.CreateSetting(const Key,Value: string): string;
begin
  result := SettingsForm.Settings.GetKeyValueDef(Key,value);
end;

procedure TSimbaForm.SetSetting(const key,Value: string; save : boolean);
begin
  //Creates the setting if needed
  SettingsForm.Settings.SetKeyValue(key,value);
  if save then
    SettingsForm.Settings.SaveToXML(SimbaSettingsFile);
end;

function TSimbaForm.SettingExtists(const key: string): boolean;
begin
  result :=SettingsForm.Settings.KeyExists(key);
end;

procedure TSimbaForm.FontUpdate;
  procedure Idler;
  begin
    Application.ProcessMessages;
    Sleep(25);
  end;

var
  CurrVersion : integer;
  LatestVersion : integer;
  FontDownload : TDownloadThread;
  Stream : TStringStream;
  UnTarrer : TUntarThread;
  Fonts : string;
  Decompress : TDecompressThread;
begin
  if UpdatingFonts then
    exit;
  UpdatingFonts := True;
  CurrVersion := StrToIntDef(LoadSettingDef('Settings/Fonts/Version','-1'),-1);
  LatestVersion := SimbaUpdateForm.GetLatestFontVersion;
  if LatestVersion > CurrVersion then
  begin;
    formWriteln(format('New fonts available. Current version: %d. Latest version: %d',[CurrVersion,LatestVersion]));
    FontDownload := TDownloadThread.Create(LoadSettingDef('Settings/Fonts/UpdateLink',FontURL + 'Fonts.tar.bz2'),
                                           @Fonts);
    FontDownload.resume;
    while FontDownload.Done = false do
      Idler;
    //Fontdownload is freed now
    Stream := TStringStream.Create(Fonts);
    try
      Decompress := TDecompressThread.Create(Stream);
      Decompress.Resume;
      while Decompress.Finished = false do
        Idler;
      if Decompress.Result <> nil then
      begin;
        UnTarrer := TUntarThread.Create(Decompress.Result,FontPath,True);
        UnTarrer.Resume;
        while UnTarrer.Finished = false do
          Idler;
        if UnTarrer.Result then
        begin;
          FormWriteln('Succesfully installed the new fonts!');
          SetSetting('Settings/Fonts/Version',IntToStr(LatestVersion),true);
          if Assigned(self.OCR_Fonts) then
            self.OCR_Fonts.Free;
          Self.OCR_Fonts := TMOCR.Create(nil);
          OCR_Fonts.InitTOCR(fontPath);
        end;
        UnTarrer.Free;
        Decompress.Result.Free;
      end;
      Decompress.free;
    finally
      Stream.Free;
    end;
  end;
  UpdatingFonts := False;
end;

procedure TSimbaForm.ScriptStartEvent(Sender: TObject; var Script: string;
  var Continue: boolean);
begin
  ScriptStartData.Sender:=Sender;
  ScriptStartData.Script:= @Script;
  ScriptStartData.Continue:= @Continue;
  TThread.Synchronize(nil,@HandleScriptStartData);
end;

procedure TSimbaForm.SetShowParamHintAuto(const AValue: boolean);
begin
  SetSetting('Settings/CodeHints/ShowAutomatically', Booltostr(AValue,true));
end;

procedure TSimbaForm.SetShowCodeCompletionAuto(const AValue: boolean);
begin
  SetSetting('Settings/CodeCompletion/ShowAutomatically', Booltostr(AValue,true));
end;

{$ifdef mswindows}
function GetConsoleWindow: HWND; stdcall; external kernel32 name 'GetConsoleWindow';

procedure TSimbaForm.ShowConsole(ShowIt: boolean);
var
  ProcessId : DWOrd;
begin
  if ShowIt = ConsoleVisible then
    Exit;
  //Check if the console is ours (if it's not, do not hide it!!
  GetWindowThreadProcessId(GetConsoleWindow,ProcessId);
  if ProcessId = GetCurrentProcessId then
  begin
    if showit then
      ShowWindow(GetConsoleWindow,SW_SHOWNA)
    else
      ShowWindow(GetConsoleWindow,sw_hide);
    ConsoleVisible:= ShowIt;
  end else
    Writeln('You cannot hide the window, since its not created by Simba');
end;
{$endif}

procedure TSimbaForm.FunctionListShown(ShowIt: boolean);
begin
  with MenuItemFunctionList, frmFunctionList do
  begin
    Checked := ShowIt;
    if(Checked)then
    begin
      if FunctionList.Items.Count = 0 then
        MenuitemFillFunctionListClick(nil);
      FrameEndDock(frmFunctionList,frmFunctionList.Parent,0,0);//Set the label correctly
      if(frmFunctionList.Parent is TPanel)then
      begin
        SplitterFunctionList.Show;
        frmFunctionList.Show;
      end else frmFunctionList.Parent.Show;
      if Self.Visible then
        if editSearchList.CanFocus then
          editSearchList.SetFocus;
      //Lets load up this Script tree!
      if CurrScript <> nil then
        frmFunctionList.LoadScriptTree(CurrScript.SynEdit.text);
    end else begin
      if(frmFunctionList.Parent is TPanel)then
        frmFunctionList.Hide
      else
        frmFunctionList.Parent.Hide;
      SplitterFunctionList.Hide;
    end;
  end;
end;


procedure TSimbaForm.SafeCallThread;
var
  thread: TMThread;
  LocalCopy : TSyncInfo;
begin
  LocalCopy := CurrentSyncInfo;
  mDebugLn('Executing : ' + LocalCopy.MethodName);
  thread:= TMThread(LocalCopy.OldThread);
  mmlpsthread.CurrThread:= thread;
  try
    if thread is TPSThread then
    begin
      with TPSThread(thread).PSScript do
      begin
        OnLine:=@OnLinePSScript;
        LocalCopy.Res^:= Exec.RunProcPVar(LocalCopy.V^,Exec.GetProc(LocalCopy.MethodName));
        Online := nil;
      end;
    end else
    begin
      raise Exception.Create('ThreadSafeCall not implemented on this client');
    end;
  finally
    mmlpsthread.CurrThread:= nil;
  end;
end;

function TSimbaForm.OpenScript: boolean;
var
  i: Integer;
  OpenInNewTab : boolean;
begin
  Result := False;
  OpenInNewTab:= (LowerCase(LoadSettingDef('Settings/Tabs/OpenScriptInNewTab','True')) = 'true');
  if not OpenInNewTab then
    if CanExitOrOpen = false then
      Exit;
  with TOpenDialog.Create(nil) do
  try
    if (CurrScript.ScriptFile <> '') then
      InitialDir := ExtractFileDir(CurrScript.ScriptFile)
    else
      InitialDir := ScriptDir;
    Options := [ofAllowMultiSelect, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofViewDetail];
    Filter:= 'Simba Files|*.simba;*.simb;*.cogat;*.mufa;*.txt;*.' +LoadSettingDef('Settings/Extensions/FileExtension','sex')+
             '|Any files|*.*';
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
end;

function TSimbaForm.LoadScriptFile(filename: string; AlwaysOpenInNewTab: boolean; CheckOtherTabs : boolean
  ): boolean;
var
  OpenInNewTab : boolean;
  CheckTabsFirst : boolean;
  Tab : integer;
begin
  if AlwaysOpenInNewTab then
    OpenInNewTab := true
  else
    OpenInNewTab:= (LowerCase(LoadSettingDef('Settings/Tabs/OpenScriptInNewTab','True')) = 'true');
  if CheckOtherTabs then
    CheckTabsFirst := True
  else
    CheckTabsFirst := (Lowercase(LoadSettingDef('Settings/Tabs/CheckTabsBeforeOpen','True')) = 'true');
  if FileExistsUTF8(FileName) then
  begin;
    if CheckTabsFirst then
    begin;
      Tab :=  FindTab(filename);
      if tab <> -1 then
      begin
        TMufasaTab(Tabs[tab]).ScriptFrame.MakeActiveScriptFrame;
        exit(true);
      end;
    end;
    if OpenInNewTab and (CurrScript.SynEdit.Text <> CurrScript.ScriptDefault) then //Add la tab!
      self.addtab;
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
end;

function TSimbaForm.SaveCurrentScript: boolean;
begin
  if not CurrScript.ScriptChanged then
  begin
    writeln('SaveScript - no changes.');
    exit(false);
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
        result := SaveCurrentScriptAs;
        exit;
      end;
      OnSaveScript(scriptfile);
    end
    else
      result := SaveCurrentScriptAs;
  end;
end;

function TSimbaForm.SaveCurrentScriptAs: boolean;
var
  ScriptFile : string;
begin
  Result := false;
  with TSaveDialog.Create(nil) do
  try
    if (CurrScript.ScriptFile <> '') then
      InitialDir := ExtractFileDir(CurrScript.ScriptFile)
    else
      InitialDir := ScriptDir;
    filter := 'Simba Files|*.simba;*.simb;*.cogat;*.mufa;*.txt;*.' +
              LoadSettingDef('Settings/Extensions/FileExtension','sex')+
              '|Any files|*.*';
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

function TSimbaForm.SaveCurrentScriptAsDefault : boolean;
begin
  with CurrScript do
  begin
    try
      SynEdit.Lines.SaveToFile(DefScriptPath);
      mDebugLn('Script saved as default.');
      Result := True;
    except
      mDebugLn('Cannot save script as default.');
      Result := False;
    end;
  end;
end;

function TSimbaForm.CanExitOrOpen: boolean;
begin;
  Self.Enabled := False;//We HAVE to answer the popup
  Result := True;
  if ScriptState <> ss_None then
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

function TSimbaForm.ClearScript: boolean;
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
  {$R *.lfm}


end.
