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

    TestUnit/GUI for the Mufasa Macro Library
}

unit TestUnit;

{$undef EditButtons}
{$Undef ProcessMessages} //Define this for processmessages in ThreadSafeCall
{$mode objfpc}{$H+}

interface

uses
  {$ifdef linux}cthreads,{$endif}Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, SynEdit, SynHighlighterPas, SynMemo,
  //Client,
  MufasaTypes,
  mmlpsthread,synedittypes,
  {$IFDEF MSWINDOWS} os_windows, {$ENDIF} //For ColorPicker etc.
  {$IFDEF LINUX} os_linux, {$ENDIF} //For ColorPicker etc.
  colourpicker, framescript, windowselector, lcltype, ActnList, StdActns,
  SynExportHTML, SynEditKeyCmds, SynEditHighlighter, SynEditMarkupSpecialLine,
  SynEditMarkupHighAll, SynEditMiscClasses, LMessages, Buttons, PairSplitter,
  ColorBox              , about, framefunctionlist, ocr, updateform, simbasettings;

const
    SimbaVersion = 544;

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
  { TForm1 }

  TForm1 = class(TForm)
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
    MainMenu: TMainMenu;
    Memo1: TMemo;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuExtra: TMenuItem;
    MenuItemCompile: TMenuItem;
    MenuItemHandbook: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemReportBug: TMenuItem;
    MenuViewSettings: TMenuItem;
    MenuItemExportHTML: TMenuItem;
    MenuItemDivider9: TMenuItem;
    MouseTimer: TTimer;
    NewsTimer: TTimer;
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
    MainMenu1: TMainMenu;
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
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionFindstartExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNewTabExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSelectAllExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionTabLastExecute(Sender: TObject);
    procedure ActionTabNextExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ChangeMouseStatus(Sender: TObject);
    procedure CheckBoxMatchCaseClick(Sender: TObject);
    procedure CloseFindPanel;
    procedure editSearchListExit(Sender: TObject);
    procedure editSearchListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure editSearchListKeyPress(Sender: TObject; var Key: char);
    procedure FunctionListChange(Sender: TObject; Node: TTreeNode);
    procedure FunctionListEnter(Sender: TObject);
    procedure FunctionListExit(Sender: TObject);
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
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemTabCloseClick(Sender: TObject);
    procedure MenuItemTabCloseOthersClick(Sender: TObject);
    procedure MenuItemFunctionListClick(Sender: TObject);
    procedure MenuViewSettingsClick(Sender: TObject);
    procedure NewsTimerTimer(Sender: TObject);
    procedure OnLinePSScript(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoTray(Sender: TObject);
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
    procedure PopupItemFindClick(Sender: TObject);
    procedure ProcessDebugStream(Sender: TObject);
    procedure ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer);
    procedure ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ScriptPopupPopup(Sender: TObject);
    procedure SpeedButtonSearchClick(Sender: TObject);
    procedure SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure TB_ReloadPluginsClick(Sender: TObject);
    procedure TrayPopupPopup(Sender: TObject);
    procedure TT_UpdateClick(Sender: TObject);
    procedure UpdateMenuButtonClick(Sender: TObject);
    procedure UpdateTimerCheck(Sender: TObject);
  private
    PopupTab : integer;
    FirstRun : boolean;//Only show the warnings the first run (path not existing one's)
    SearchStart : TPoint;
    LastTab  : integer;
    function GetScriptState: TScriptState;
    procedure SetScriptState(const State: TScriptState);
    function LoadSettingDef(Key : string; Def : string) : string;
    function CreateSetting(Key : string; Value : string) : string;
  public
    DebugStream: String;
    SearchString : string;
    CurrScript : TScriptFrame; //The current scriptframe
    CurrTab    : TMufasaTab; //The current TMufasaTab
    Tabs : TList;
    Manager: TIOManager;
    OCR_Fonts: TMOCR;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    procedure FunctionListShown( ShowIt : boolean);
    property ScriptState : TScriptState read GetScriptState write SetScriptState;
    procedure SafeCallThread;
    function OpenScript : boolean;
    function LoadScriptFile(filename : string) : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
    procedure RunScript;
    procedure PauseScript;
    procedure StopScript;
    procedure AddTab;
    procedure StopCodeCompletion;
    function DeleteTab( TabIndex : integer; CloseLast : boolean; Silent : boolean = false) : boolean;
    procedure ClearTab( TabIndex : integer);
    procedure CloseTabs(Exclude: integer = -1; Silent : boolean = false); //-1 for no exclusion
    procedure SetEditActions;
    procedure DoSearch(Next : boolean; HighlightAll : boolean);
    procedure RefreshTab;//Refreshes all the form items that depend on the Script (Panels, title etc.)
    procedure RefreshTabSender(sender : PtrInt);
    procedure CreateDefaultEnvironment;
    procedure InitalizeTMThread(var Thread : TMThread);
    procedure HandleParameters;
  end;

  procedure formWriteln( S : String);
  function GetMethodName( Decl : string; PlusNextChar : boolean) : string;

const
  // Rip Mufasa -> Simba ftw
  //WindowTitle = 'Mufasa v2 - %s';//Title, where %s = the place of the filename.
  WindowTitle = 'Simba - %s';//Title, where %s = the place of the filename.
  Panel_State = 0;
  Panel_Coords = 1;
  Panel_ScriptName = 2;
  Panel_ScriptPath = 3;


  Image_Stop = 7;
  Image_Terminate = 19;
var
  Form1: TForm1;
  MainDir : string;
  CurrentSyncInfo : TSyncInfo;//We need this for SafeCallThread

implementation
uses
   lclintf,plugins,
   syncobjs, // for the critical sections
   debugimage,
   bitmaps,
   colourhistory,
   simpleanalyzer,
   math;

//{$ifdef mswindows}

var
   DebugCriticalSection: syncobjs.TCriticalSection;

procedure TForm1.ProcessDebugStream(Sender: TObject);

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

procedure TForm1.ScriptPanelDockDrop(Sender: TObject; Source: TDragDockObject;
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

procedure TForm1.ScriptPanelDockOver(Sender: TObject; Source: TDragDockObject; //is there a better way to do all of this?
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
   P: TPoint;
begin
  Accept := frmFunctionList.DragKind = dkDock;
  if(Accept)then
  begin
    P := ScriptPanel.ClientToScreen(Point(0, 0));
    if(X <= (ScriptPanel.Width div 2))then
      Source.DockRect := Rect(P.x, P.y, min(P.x + frmFunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y + ScriptPanel.Height)
    else
      Source.DockRect := Rect(max(P.x + ScriptPanel.Width - frmFunctionList.Width, P.x + (ScriptPanel.Width div 2)), P.y, P.x + ScriptPanel.Width, P.y + ScriptPanel.Height);
  end;
end;

procedure TForm1.ScriptPopupPopup(Sender: TObject);
begin
  SetEditActions;
end;

procedure TForm1.SpeedButtonSearchClick(Sender: TObject);
begin
  CloseFindPanel;
end;

procedure TForm1.SplitterFunctionListCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  if(NewSize > ScriptPanel.Width div 2)then
    NewSize := ScriptPanel.Width div 2;
end;

procedure TForm1.TB_ReloadPluginsClick(Sender: TObject);
begin
//  PluginsGlob.FreePlugins;
end;

procedure TForm1.TrayPopupPopup(Sender: TObject);
begin
  MenuItemHide.enabled:= Form1.Visible;
  {$ifdef MSWindows}
  MenuItemShow.Enabled:= not Form1.Visible;
  if Form1.Visible then
    if Form1.CanFocus then
      form1.SetFocus;
  {$endif}
end;

procedure TForm1.TT_UpdateClick(Sender: TObject);
begin
  SimbaUpdateForm.ShowModal;
  TT_Update.Visible:=False;
end;

procedure TForm1.UpdateTimerCheck(Sender: TObject);
var
   chk: String;
   time:integer;
begin
  chk := LoadSettingDef('Settings/Updater/CheckForUpdates','True');

  if chk <> 'True' then
    Exit;

  if SimbaUpdateForm.CanUpdate then
  begin;
    TT_Update.Visible:=True;
    formWriteln('A new update of Simba is available!');
  end;

  time := StrToIntDef(LoadSettingDef('Settings/Updater/CheckEveryXMinutes','30'),30);
  UpdateTimer.Interval:= time {mins} * 60 {secs} * 1000 {ms};//Every half hour
end;

procedure TForm1.UpdateMenuButtonClick(Sender: TObject);
begin
  SimbaUpdateForm.ShowModal;
end;

procedure formWriteln( S : String);
begin
  writeln('formWriteln: ' + s);
  DebugCriticalSection.Enter;
  try
    {$ifdef MSWindows}
    //Ha, we cán acces the debugmemo
    Form1.Memo1.Lines.Add(s);
    {$else}
    s := s + MEOL;
    Form1.DebugStream:= Form1.DebugStream + s;
    {$endif}
  finally
    DebugCriticalSection.Leave;
  end;
  //Form1.Memo1.Lines.Add(s);
end;

//{$ENDIF}

procedure TForm1.RunScript;
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
      Writeln('The script hasn''t stopped yet, so we cannot start a new one.');
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

procedure TForm1.PauseScript;
begin
  with CurrScript do
  begin;
    if ScriptState = ss_Running then
    begin;
      {$ifdef MSWindows}
      ScriptThread.Suspended:= True;
      ScriptState:= ss_Paused;
      {$else}
      Writeln('Linux users are screwed, no pause button for u!');
      {$endif}
    end else if ScriptState = ss_Paused then
    begin;
      ScriptThread.Resume;
      ScriptState := ss_Running;
    end;
  end;
end;

procedure TForm1.StopScript;
begin
  with CurrScript do
  begin;
    case ScriptState of
      ss_Stopping:
        begin    //Terminate the thread the tough way.
          writeln('Terminating the Scriptthread');
          Writeln('Exit code terminate: ' +inttostr(KillThread(ScriptThread.Handle)));
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

procedure TForm1.AddTab;
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

function TForm1.DeleteTab(TabIndex: integer; CloseLast : boolean; Silent : boolean = false) : boolean;
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

procedure TForm1.ClearTab(TabIndex: integer);
begin
  TMufasaTab(Tabs[TabIndex]).Clear;
end;

procedure TForm1.CloseTabs(Exclude: integer = -1; Silent : boolean = false);
var
  I : integer;
begin
  for i := tabs.count - 1 downto 0 do
    if i <> exclude then
      if not DeleteTab(i,false,silent) then
        exit;
end;

procedure TForm1.SetEditActions;
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

procedure TForm1.DoSearch(Next: boolean; HighlightAll : boolean);
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
    Writeln('Searching: ' + SearchString);
    if next then
      CurrPos := CurrScript.SynEdit.LogicalCaretXY
    else
      CurrPos := SearchStart;
    Res := CurrScript.SynEdit.SearchReplaceEx(SearchString,'',SearchOptions,CurrPos);
    if res = 0 then
    begin
      res := CurrScript.SynEdit.SearchReplaceEx(SearchString,'',SearchOptions,Point(0,0));
      if res > 0 then
      begin;
        Writeln('End of document reached');
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

procedure TForm1.RefreshTab;
var
  Tab : TMufasaTab;
  Script : TScriptFrame;
  NewTab : integer;
begin
  if tabs.Count < 1 then
  begin;
    Writeln('Cannot refresh tab, since there are no tabs.');
    exit;
  end;
  NewTab := PageControl1.TabIndex;
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

procedure TForm1.RefreshTabSender(sender: PtrInt);
begin
  RefreshTab;
end;

procedure TForm1.CreateDefaultEnvironment;
var
  IncludePath,FontPath,PluginsPath : string;
begin
  CreateSetting('Settings/Updater/CheckForUpdates','True');
  CreateSetting('Settings/Updater/CheckEveryXMinutes','30');
  CreateSetting('Settings/Interpreter/UseCPascal', 'False');
  CreateSetting('Settings/Fonts/LoadOnStartUp', 'True');
  CreateSetting('Settings/Tabs/OpenNextOnClose','False');
  CreateSetting('Settings/Tabs/OpenScriptInNewTab','True');
  CreateSetting('Settings/ColourPicker/ShowHistoryOnPick', 'True');

  CreateSetting('Settings/Updater/RemoteVersion',
                {$IFDEF WINDOWS}
                  {$IFDEF CPUI386}
                  'http://simba.villavu.com/bin/Windows/x86/Stable/Simba.exe'
                  {$ELSE}
                  'http://simba.villavu.com/bin/Windows/x86_64/Stable/Simba.exe'
                  {$ENDIF}
                {$ELSE}
                  {$IFDEF CPUI386}
                  'http://simba.villavu.com/bin/Linux/x86/Stable/Simba'
                  {$ELSE}
                  'http://simba.villavu.com/bin/Linux/x86_64/Stable/Simba'
                  {$ENDIF}
                {$ENDIF}
                );
  CreateSetting('Settings/Updater/RemoteVersionLink',
                {$IFDEF WINDOWS}
                  {$IFDEF CPUI386}
                  'http://simba.villavu.com/bin/Windows/x86/Stable/Version'
                  {$ELSE}
                  'http://simba.villavu.com/bin/Windows/x86_64/Stable/Version'
                  {$ENDIF}
                {$ELSE}
                  {$IFDEF CPUI386}
                  'http://simba.villavu.com/bin/Linux/x86/Stable/Version'
                  {$ELSE}
                  'http://simba.villavu.com/bin/Linux/x86_64/Stable/Version'
                  {$ENDIF}
                {$ENDIF}
                );
  {Creates the paths and returns the path}
  includePath:= CreateSetting('Settings/Includes/Path', ExpandFileName(MainDir+DS+'Includes' + DS));
  fontPath := CreateSetting('Settings/Fonts/Path', ExpandFileName(MainDir+DS+  'Fonts' + DS));
  PluginsPath := CreateSetting('Settings/Plugins/Path', ExpandFileName(MainDir+ DS+ 'Plugins' + DS));
  if not DirectoryExists(IncludePath) then
    CreateDir(IncludePath);
  if not DirectoryExists(FontPath) then
    CreateDir(FontPath);
  if not DirectoryExists(PluginsPath) then
    CreateDir(PluginsPath);
  SettingsForm.SettingsTreeView.FullExpand;
  SettingsForm.SaveCurrent;
end;

procedure TForm1.InitalizeTMThread(var Thread: TMThread);
var
  DbgImgInfo : TDbgImgInfo;
  fontPath: String;
  includePath : string;
  AppPath : string;
  pluginspath: string;
  ScriptPath : string;
  UseCPascal: String;
  loadFontsOnScriptStart: boolean;
begin
  AppPath:= MainDir + DS;
  includePath:= IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Includes/Path', ExpandFileName(MainDir+DS+'Includes' + DS)));
  fontPath := IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Fonts/Path', ExpandFileName(MainDir+DS+  'Fonts' + DS)));
  PluginsPath := IncludeTrailingPathDelimiter(LoadSettingDef('Settings/Plugins/Path', ExpandFileName(MainDir+ DS+ 'Plugins' + DS)));
  CurrScript.ScriptErrorLine:= -1;
  CurrentSyncInfo.SyncMethod:= @Self.SafeCallThread;
  UseCPascal := LoadSettingDef('Settings/Interpreter/UseCPascal', 'False');
  try
    if lowercase(UseCPascal) = 'true' then
      Thread := TCPThread.Create(True,@CurrentSyncInfo,PluginsPath)
    else
      Thread := TPSThread.Create(True,@CurrentSyncInfo,PluginsPath);
  except
    writeln('Failed to initialise the library!');
    Exit;
  end;
  {$IFNDEF TERMINALWRITELN}
  Thread.SetDebug(@formWriteln);
  Thread.DebugMemo := Self.Memo1;
  {$ENDIF}
  Thread.SetScript(CurrScript.SynEdit.Lines.Text);
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
    ScriptPath := ExtractFileDir(CurrScript.ScriptFile);

  if DirectoryExists(PluginsPath) then
     PluginsGlob.AddPath(PluginsPath);
  if not DirectoryExists(IncludePath) then
    if FirstRun then
      Writeln('Warning: The include directory specified in the Settings isn''t valid.');
  if not DirectoryExists(fontPath) then
    if FirstRun then
      Writeln('Warning: The font directory specified in the Settings isn''t valid. Can''t load fonts now');
  Thread.SetPaths(ScriptPath,AppPath,Includepath,PluginsPath,fontPath);

  if selector.haspicked then Thread.Client.IOManager.SetTarget(Selector.LastPick);


  loadFontsOnScriptStart := (lowercase(LoadSettingDef('Settings/Fonts/LoadOnStartUp', 'True')) = 'true');
  // Copy our current fonts
  if not assigned(Self.OCR_Fonts) and loadFontsOnScriptStart and DirectoryExists(fontPath) then
  begin
    Self.OCR_Fonts := TMOCR.Create(Thread.Client);
    OCR_Fonts.InitTOCR(fontPath);
    Thread.Client.MOCR.SetFonts(OCR_Fonts.GetFonts);
  end else
    if assigned(Self.OCR_Fonts) and loadFontsOnScriptStart then
      Thread.Client.MOCR.SetFonts(OCR_Fonts.GetFonts);

end;

procedure TForm1.HandleParameters;
var
  DoRun : Boolean;
  ErrorMsg : string;
begin
  DoRun := false;
  if Paramcount = 1 then
  begin
    if FileExists(ParamStr(1)) then
      LoadScriptFile(paramstr(1));
  end else
  begin;
    ErrorMsg:=Application.CheckOptions('ro:','run open:');
    if ErrorMsg <> '' then
      writeln(ErrorMSG)
    else
    begin
      if Application.HasOption('o','open') then
      begin;
        LoadScriptFile(Application.GetOptionValue('o','open'));
        DoRun:= Application.HasOption('r','run');
      end;
    end;
  end;
  if DoRun then
    Self.RunScript;
end;


procedure TForm1.ActionTabLastExecute(Sender: TObject);
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

procedure TForm1.ActionCloseTabExecute(Sender: TObject);
begin
  if(PageControl1.PageCount > 1)then
    Self.DeleteTab(PageControl1.TabIndex,false)
  else
    Self.ClearScript;  //DeleteTab would take care of this already, but yeah, it's neater this way.
end;

procedure TForm1.ActionCompileScriptExecute(Sender: TObject);
var
  TempThread : TMThread;
begin
  InitalizeTMThread(TempThread);
  TempThread.CompileOnly:= true;
  TempThread.Resume;
end;

procedure TForm1.ActionCopyExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CopyToClipboard
  else if Memo1.Focused then
    Memo1.CopyToClipboard
 { else
    Writeln(Sender.ToString);     }
end;

procedure TForm1.ActionCutExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.CutToClipboard
  else if Memo1.Focused then
    Memo1.CutToClipboard;
end;

procedure TForm1.ActionDeleteExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.ClearSelection
  else if Memo1.Focused then
    Memo1.ClearSelection;
end;

procedure TForm1.ActionExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.ActionFindNextExecute(Sender: TObject);
begin
  DoSearch(true, false);
end;

procedure TForm1.ActionFindstartExecute(Sender: TObject);
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

procedure TForm1.ActionClearDebugExecute(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm1.ActionNewExecute(Sender: TObject);
begin
  //Self.ClearScript;
  Self.AddTab;
end;

procedure TForm1.ActionNewTabExecute(Sender: TObject);
begin
  Self.AddTab;
end;

procedure TForm1.ActionOpenExecute(Sender: TObject);
begin
  Self.OpenScript;
end;

procedure TForm1.ActionPasteExecute(Sender: TObject);
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

procedure TForm1.ActionPauseExecute(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TForm1.ActionRedoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Redo
  else if Memo1.Focused then
    Memo1.Undo; //?
end;

procedure TForm1.ActionReplaceExecute(Sender: TObject);
begin
  if(ScriptPopup.HandleAllocated)then
    dlgReplace.FindText:= CurrScript.SynEdit.SelText;
  dlgReplace.Execute;
end;

procedure TForm1.ActionRunExecute(Sender: TObject);
begin
  Self.RunScript;
end;

procedure TForm1.ActionSaveAllExecute(Sender: TObject);
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

procedure TForm1.ActionSaveAsExecute(Sender: TObject);
begin
  Self.SaveCurrentScriptAs;
end;

procedure TForm1.ActionSaveExecute(Sender: TObject);
begin
  Self.SaveCurrentScript;
end;

procedure TForm1.ActionSelectAllExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.SynEdit.SelectAll
  else if Memo1.Focused then
    Memo1.SelectAll
  else if LabeledEditSearch.Focused then
    LabeledEditSearch.SelectAll;

end;

procedure TForm1.ActionStopExecute(Sender: TObject);
begin
  Self.StopScript;
end;

procedure TForm1.ActionTabNextExecute(Sender: TObject);
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

procedure TForm1.ActionUndoExecute(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused or ScriptPopup.HandleAllocated then
    CurrScript.Undo
  else if Memo1.Focused then
    Memo1.Undo;
end;

procedure TForm1.ChangeMouseStatus(Sender: TObject);
var
  x, y: Integer;
begin
  Self.Manager.GetMousePos(x, y);
  StatusBar.Panels[Panel_Coords].Text := Format('(%d, %d)', [x, y]);
end;

procedure TForm1.CheckBoxMatchCaseClick(Sender: TObject);
begin
  RefreshTab;
  CurrScript.SynEdit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable;
  SearchString := LabeledEditSearch.Text;
  DoSearch(false, true);
  CurrScript.SynEdit.UseIncrementalColor:= true;
  LabeledEditSearch.SetFocus;
end;



procedure TForm1.CloseFindPanel;
begin
  SearchPanel.Visible:= false;
  if CurrScript.SynEdit.CanFocus then
    CurrScript.SynEdit.SetFocus;
end;

procedure TForm1.StopCodeCompletion;
begin
  if frmFunctionList.InCodeCompletion then
    with CurrScript,frmFunctionList do
    begin;
      editSearchList.Color:= clWhite;
      if FilterTree.Focused then
      begin;
        Writeln('This is currently not supported');
        SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
        SynEdit.LogicalCaretXY:= point(CompletionCaret.x,CompletionCaret.y);
        SynEdit.SelEnd:= SynEdit.SelStart;
      end;
      InCodeCompletion:= false;
      SynEdit.SelectedColor.Style:= [];
      SynEdit.SelectedColor.Foreground:= clHighlightText;
      SynEdit.SelectedColor.Background:= clHighlight;
      Synedit.MarkupByClass[TSynEditMarkupHighlightAllCaret].TempEnable;
    end;
end;

procedure TForm1.editSearchListExit(Sender: TObject);
begin
  frmFunctionList.editSearchList.Color := clWhite;
  StopCodeCompletion;
end;

procedure TForm1.editSearchListKeyDown(Sender: TObject; var Key: Word;
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

procedure TForm1.editSearchListKeyPress(Sender: TObject; var Key: char);
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
      CurrScript.SynEdit.LogicalCaretXY:= point(frmFunctionList.CompletionCaret.x,frmFunctionList.CompletionCaret.y);
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

procedure TForm1.FunctionListChange(Sender: TObject; Node: TTreeNode);
var
  MethodInfo : TMethodInfo;
begin
  if node = nil then
    exit;
  if Node.Level > 0 then
  begin
    MethodInfo := PMethodInfo(node.Data)^;
    StatusBar.Panels[Panel_ScriptPath].Text := MethodInfo.MethodStr;
  end;
  if Node.level = 0 then
    StatusBar.Panels[Panel_ScriptPath].Text := 'Section: ' + Node.Text;
end;

procedure TForm1.FunctionListEnter(Sender: TObject);
begin
  frmFunctionList.LoadScriptTree(CurrScript.SynEdit.Text);
end;

procedure TForm1.FunctionListExit(Sender: TObject);
begin
//  StatusBar.Panels[2].Text:= '';
end;

procedure TForm1.MenuItemHandbookClick(Sender: TObject);
begin
  OpenURL('http://vila.villavu.com/mufasa/mufasa_ps_handbook/');
end;

procedure TForm1.MenuItemColourHistoryClick(Sender: TObject);
begin
  MenuItemColourHistory.Checked := not ColourHistoryForm.Visible;
  if MenuItemColourHistory.Checked then
    ColourHistoryForm.Show
  else
    ColourHistoryForm.Hide;
end;

procedure TForm1.dlgReplaceFind(Sender: TObject);
begin
  SearchString := dlgReplace.FindText;
  DoSearch(True, False);
end;

procedure TForm1.dlgReplaceReplace(Sender: TObject);
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
    if(frEntireScope in dlgReplace.Options)then P:= Point(0, 0) else P:= CaretXY;
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

procedure TForm1.EditSearchChange(Sender: TObject);
begin
  SearchString :=LabeledEditSearch.Text;
  DoSearch(false, true);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i : integer;
begin
  for i := Tabs.Count - 1 downto 0 do
    if not DeleteTab(i,true) then
    begin;
      CloseAction := caNone;
      exit
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  MainDir:= ExtractFileDir(Application.ExeName);
  SimbaSettingsFile := MainDir + DS + 'settings.xml';
  if FileExists(SimbaSettingsFile) then
    Application.CreateForm(TSettingsForm,SettingsForm)
  else  begin
    Application.CreateForm(TSettingsForm,SettingsForm);
    Self.CreateDefaultEnvironment;
  end;
  UpdateTimer.OnTimer:= @UpdateTimerCheck;
  //Show close buttons @ tabs
  PageControl1.Options:=PageControl1.Options+[nboShowCloseButtons];
  PageControl1.OnCloseTabClicked:=ActionCloseTab.OnExecute;
  Tabs := TList.Create;
  AddTab;//Give it alteast 1 tab ;-).
  FunctionListShown(True); //Show this function list bitch!
  Manager := TIOManager.Create; //No need to load plugins for the Global manager
  Picker := TMColorPicker.Create(Manager);
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
    Writeln('We still have an out-dated exe file in the dir, lets remove!');
    Writeln(format('Sucesfully deleted the file? %s',[BoolToStr(DeleteFile(Application.ExeName + '_old_'),true)]));
  end;
  {$endif}
  frmFunctionList.OnEndDock:= @frmFunctionList.FrameEndDock;
  FirstRun := true;//Our next run is the first run.
  HandleParameters;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  for i := Tabs.Count - 1 downto 0 do
    TMufasaTab(Tabs[i]).Free;
  Tabs.free;
  Selector.Free;
  Picker.Free;
  Manager.Free;
  PluginsGlob.Free;

  SetLength(DebugStream, 0);
  DebugCriticalSection.Free;
end;

procedure TForm1.FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
begin
  SetEditActions;
  Handled := ActionList.IsShortCut(Msg);
end;



procedure TForm1.LabeledEditSearchEnter(Sender: TObject);
begin
  SearchStart := CurrScript.SynEdit.LogicalCaretXY;
  with CurrScript.SynEdit do
  begin
    UseIncrementalColor:= true;
    MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable
  end;
end;

procedure TForm1.LabeledEditSearchExit(Sender: TObject);
begin
  if not CheckBoxMatchCase.MouseEntered then
    RefreshTab;
end;

procedure TForm1.LabeledEditSearchKeyDown(Sender: TObject; var Key: Word;
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

procedure TForm1.LabeledEditSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then
  begin;
    SearchString:= LabeledEditSearch.Text;
    DoSearch(true, true);
    key := #0;
//    LabeledEditSearch.SelStart:= Length(LabeledEditSearch.Text);
  end;
end;

procedure TForm1.MenuEditClick(Sender: TObject);
begin
  SetEditActions;
end;

procedure TForm1.MenuItemAboutClick(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TForm1.MenuItemCloseTabsClick(Sender: TObject);
begin
  Self.CloseTabs;
end;

procedure TForm1.MenuItemDebugImageClick(Sender: TObject);
begin
  MenuItemDebugImage.Checked := not DebugImgForm.Visible;
  if MenuItemDebugImage.Checked then
    DebugImgForm.Show
  else
    DebugImgForm.Hide;
end;

procedure TForm1.MenuItemExportHTMLClick(Sender: TObject);
var
  SynExporterHTML : TSynExporterHTML;
begin;
  SynExporterHTML := TSynExporterHTML.Create(nil);
  SynExporterHTML.Highlighter := CurrScript.SynFreePascalSyn1;
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

procedure TForm1.MenuitemFillFunctionListClick(Sender: TObject);
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
  if frmFunctionList.FunctionList.Items.Count = 0 then
  begin;
    Methods := TMThread.GetExportedMethods;
    Tree := frmFunctionList.FunctionList;
    Tree.Items.Clear;
    Sections := TStringList.Create;
    LastSection := '';
    frmFunctionList.ScriptNode := Tree.Items.Add(nil,'Script');
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
      with PMethodInfo(Temp2Node.Data)^ do
      begin
        MethodStr:= strnew(PChar(Methods[i].FuncDecl));
        BeginPos:= -1;
      end;
    end;
    Sections.free;
  end;
  frmFunctionList.LoadScriptTree(CurrScript.SynEdit.Text);
end;

procedure TForm1.MenuItemHideClick(Sender: TObject);
begin
  if Self.Visible = false then
    MenuItemShowClick(sender)
  else
    Self.hide;
end;

procedure TForm1.MenuItemReportBugClick(Sender: TObject);
begin
  OpenURL('http://mufasa.villavu.com/mantis/bug_report_page.php');
end;

procedure TForm1.MenuItemShowClick(Sender: TObject);
begin
  Self.Show;
  Self.WindowState := wsNormal;
end;

procedure TForm1.MenuItemTabCloseClick(Sender: TObject);
begin
  DeleteTab(PopupTab,false);
end;

procedure TForm1.MenuItemTabCloseOthersClick(Sender: TObject);
begin
  CloseTabs(PopupTab);
end;

procedure TForm1.MenuItemFunctionListClick(Sender: TObject);
begin
  FunctionListShown(not MenuItemFunctionList.Checked);
end;

procedure TForm1.MenuViewSettingsClick(Sender: TObject);
begin
  SettingsForm.ShowModal;
end;

function GetSimbaNews: String;
var
  t: TSimbaVersionThread;
begin
  t := TSimbaVersionThread.Create(true);
  t.InputURL:='http://simba.villavu.com/bin/news';
  t.Resume;
  while not t.done do
  begin
    Application.ProcessMessages;
    Sleep(50);
  end;
  Exit(t.ResultStr);
end;

procedure TForm1.NewsTimerTimer(Sender: TObject);
var
  s: String;
  News : TStringList; {Need it for correct EOL stuff}
begin
  NewsTimer.Enabled:=False;
  s := GetSimbaNews;
  News := TStringList.Create;
  News.Text:= s;
  Memo1.Lines.AddStrings(News);
  News.free;
end;

procedure TForm1.OnLinePSScript(Sender: TObject);
begin
  //Writeln('We just completed a line!!');
  {$IFDEF ProcessMessages}
  Application.ProcessMessages; //Don't think that this is neccesary though
  {$ENDIF}
end;



procedure TForm1.ButtonPickClick(Sender: TObject);
var
   c, x, y: Integer;
   cobj: TColourPickerObject;
begin
  Picker.Pick(c, x, y);
  cobj := TColourPickerObject.Create(c, Point(x,y), '');

  if lowercase(LoadSettingDef('Settings/ColourPicker/ShowHistoryOnPick', 'True')) = 'true' then
  begin
    ColourHistoryForm.AddColObj(cobj, true);
    ColourHistoryForm.Show;
  end;
  formWriteln('Picked colour: ' + inttostr(c) + ' at (' + inttostr(x) + ', ' + inttostr(y) + ')');
end;


procedure TForm1.ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Manager.SetTarget(Selector.Drag);
  writeln('New window: ' + IntToStr(Selector.LastPick));
end;

procedure TForm1.NoTray(Sender: TObject);
begin
  if Not Form1.IsVisible then
    Self.MenuItemShowClick(Sender)
  else
    Form1.Hide;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  RefreshTab();
end;

procedure TForm1.ButtonTrayClick(Sender: TObject);
begin
  Form1.Hide;
end;

procedure TForm1.PageControl1Changing(Sender: TObject; var AllowChange: Boolean
  );
begin
  LastTab:= PageControl1.TabIndex;
end;

procedure TForm1.PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  PopupTab := PageControl1.TabIndexAtClientPos(MousePos);
  if PopupTab = -1 then
  begin
    Writeln('We couldn''t find which tab you clicked on, closing the popup');
    Handled := true;
  end;
end;

procedure TForm1.PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  NewPos : integer;
  OldPos : integer;
begin
  if sender <> PageControl1 then
    exit;
  NewPos := PageControl1.TabIndexAtClientPos(Point(x,y));
  OldPos := PageControl1.TabIndex;
  if (NewPos <> OldPos) and (NewPos <> -1) then
  begin;
    Tabs.Move(OldPos,NewPos);
    PageControl1.Pages[OldPos].TabIndex:= NewPos;
  end;
end;

procedure TForm1.PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Pos: Integer;
begin
  Pos := PageControl1.TabIndexAtClientPos(Point(x,y));
  Accept := (Pos <> PageControl1.TabIndex) and (Pos <> -1);
end;

procedure TForm1.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TForm1.PageControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbMiddle) and (not(PageControl1.Dragging))then
    if(PageControl1.TabIndexAtClientPos(Point(x,y)) <> -1)then
      DeleteTab(PageControl1.TabIndexAtClientPos(Point(x,y)), False);
end;

procedure TForm1.PopupItemFindClick(Sender: TObject);
begin
  SearchString := CurrScript.SynEdit.SelText;
  ActionFindNextExecute(ScriptPopup);
end;

function TForm1.GetScriptState: TScriptState;
begin
  result := CurrScript.FScriptState;
end;

procedure TForm1.SetScriptState(const State: TScriptState);
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

function TForm1.LoadSettingDef(Key: string; Def: string): string;
begin
  result := SettingsForm.Settings.GetSetLoadSaveDefaultKeyValueIfNotExists(Key,def,SimbaSettingsFile);
end;

function TForm1.CreateSetting(Key: string; Value: string): string;
begin
  result := SettingsForm.Settings.GetSetDefaultKeyValue(Key,value);
end;

procedure TForm1.FunctionListShown(ShowIt: boolean);
var
  Node : TTreeNode;
  tmpNode : TTreeNode;
  Tree : TTreeView;
  Analyzer : TScriptAnalyzer;
  I,ii : integer;
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


procedure TForm1.SafeCallThread;
var
  thread: TMThread;
begin
  Writeln('Executing : ' + CurrentSyncInfo.MethodName);
  thread:= TMThread(CurrentSyncInfo.OldThread);
  mmlpsthread.CurrThread:= thread;
  try
    if thread is TPSThread then
    begin
      with TPSThread(thread).PSScript do
      begin
        OnLine:=@OnLinePSScript;
        CurrentSyncInfo.Res:= Exec.RunProcPVar(CurrentSyncInfo.V,Exec.GetProc(CurrentSyncInfo.MethodName));
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

function TForm1.OpenScript: boolean;
var
  OpenInNewTab : boolean;
begin
  Result := False;
  OpenInNewTab:= (LowerCase(LoadSettingDef('Settings/Tabs/OpenScriptInNewTab','True')) = 'true');
  if not OpenInNewTab then
    if CanExitOrOpen = false then
      Exit;
  with TOpenDialog.Create(nil) do
  try
    Filter:= 'Simba Files|*.simb;*.cogat;*.mufa;*.txt|Any files|*.*';
    if Execute then
      if FileExists(filename) then
        result := LoadScriptFile(filename);
  finally
    Free;
  end;
end;

function TForm1.LoadScriptFile(FileName : string): boolean;
var
  OpenInNewTab : boolean;
begin
  OpenInNewTab:= (LowerCase(LoadSettingDef('Settings/Tabs/OpenScriptInNewTab','True')) = 'true');
  if FileExists(FileName) then
  begin;
    if OpenInNewTab and (CurrScript.SynEdit.Text <> CurrScript.ScriptDefault) then //Add la tab!
      self.addtab;
    with CurrScript do
    begin
      filename := SetDirSeparators(filename);
      SynEdit.Lines.LoadFromFile(FileName);
      StartText := SynEdit.Lines.text;
      ScriptName:= ExtractFileNameOnly(filename);
      WriteLn('Script name will be: ' + ScriptName);
      ScriptFile:= FileName;
      ScriptChanged := false;
      RefreshTab();
      Result := True;
    end;
  end;
end;

function TForm1.SaveCurrentScript: boolean;
begin
  with CurrScript do
  begin
    Result := (ScriptFile <> '');
    if Result then
    begin;
      ScriptChanged := false;
      SynEdit.Lines.SaveToFile(ScriptFile);
      StartText:= SynEdit.Lines.Text;
      SynEdit.MarkTextAsSaved;
      Self.Caption:= Format(WindowTitle,[ScriptName]);
    end
    else
      result := SaveCurrentScriptAs;
  end;
  RefreshTab;
end;

function TForm1.SaveCurrentScriptAs: boolean;
begin
  with CurrScript do
  begin;
    Result := false;
    with TSaveDialog.Create(nil) do
    try
      Filter:= 'Simba files|*.simb;*.cogat;*.mufa;*.pas;*.txt|Any Files|*.*';
      if Execute then
      begin;
        if ExtractFileExt(FileName) = '' then
        begin;
          ScriptFile := FileName + '.simb';
        end else
          ScriptFile := FileName;
        SynEdit.Lines.SaveToFile(ScriptFile);
        ScriptName:= ExtractFileNameOnly(ScriptFile);
        Writeln('Saving to: ' + FileName);
        WriteLn('Script name will be: ' + ScriptName);
        RefreshTab();
        Result := True;
      end;
    finally
      Free;
    end;
    if result then
    begin;
      Writeln('Succesfully saved: ' + ScriptFile);
      StartText:= SynEdit.Lines.Text;
      SynEdit.MarkTextAsSaved;
      ScriptChanged := false;
    end;
  end;
end;

function TForm1.CanExitOrOpen: boolean;
var
  I : integer;
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

function TForm1.ClearScript: boolean;
begin
  if CanExitOrOpen then
  begin;
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
  ScriptFrame.Free;
  TabSheet.Free;
  inherited Destroy;
end;

initialization
  {$I testunit.lrs}


end.

