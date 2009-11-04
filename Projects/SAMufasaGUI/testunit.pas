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

{$Undef ProcessMessages} //Define this for processmessages in ThreadSafeCall
{$mode objfpc}{$H+}

interface

uses
  {$ifdef linux}cthreads,{$endif}Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, SynEdit, SynHighlighterPas, SynMemo,
  //Client,
  MufasaTypes,
  mmlpsthread,
  window, // for the comp picker and selector
  colourpicker, framescript, windowselector, lcltype, ActnList, StdActns,
  SynEditKeyCmds, SynEditHighlighter, SynEditMarkupSpecialLine,SynEditMarkupHighAll,
  SynEditMiscClasses, LMessages, Buttons;

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
    LabeledEditSearch: TLabeledEdit;
    Memo1: TMemo;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
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
    SearchPanel: TPanel;
    ScriptPanel: TPanel;
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
    TB_Convert: TToolButton;
    MTrayIcon: TTrayIcon;
    procedure ActionClearDebugExecute(Sender: TObject);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNewTabExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPauseExecute(Sender: TObject);
    procedure ActionRunExecute(Sender: TObject);
    procedure ActionSaveAllExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure ActionTabLastExecute(Sender: TObject);
    procedure ActionTabNextExecute(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
    procedure MenuItemCloseTabsClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemFindClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemTabCloseClick(Sender: TObject);
    procedure MenuItemTabCloseOthersClick(Sender: TObject);
    procedure OnLinePSScript(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoTray(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure PageControl1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    PopupTab : integer;
    function GetScriptState: TScriptState;
    procedure SetScriptState(const State: TScriptState);
  public
    CurrScript : TScriptFrame; //The current scriptframe
    CurrTab    : TMufasaTab; //The current TMufasaTab
    Tabs : TList;
    Window: TMWindow;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    property ScriptState : TScriptState read GetScriptState write SetScriptState;
    procedure SafeCallThread;
    function OpenScript : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
    procedure RunScript;
    procedure PauseScript;
    procedure StopScript;
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure AddTab;
    function DeleteTab( TabIndex : integer; CloseLast : boolean) : boolean;
    procedure ClearTab( TabIndex : integer);
    procedure CloseTabs( Exclude : integer);overload;//-1 for none
    procedure CloseTabs;overload;
    procedure RefreshTab;//Refreshes all the form items that depend on the Script (Panels, title etc.)
  end;
  {$ifdef mswindows}
  procedure Writeln( S : String);
  {$endif}
const
  WindowTitle = 'Mufasa v2 - %s';//Title, where %s = the place of the filename.
  Panel_State = 0;
  Panel_ScriptName = 1;
  Panel_ScriptPath = 2;
  Image_Stop = 7;
  Image_Terminate = 19;
var
  Form1: TForm1;
  MainDir : string;
  CurrentSyncInfo : TSyncInfo;//We need this for SafeCallThread

implementation
uses
   lclintf,plugins;

{$ifdef mswindows}
procedure Writeln( S : String);
begin;
  Form1.Memo1.Lines.Add(s);
end;
{$ENDIF}

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
    ScriptErrorLine:= -1;
    CurrentSyncInfo.SyncMethod:= @Self.SafeCallThread;
    ScriptThread := TMMLPSThread.Create(True,@CurrentSyncInfo);
    ScriptThread.SetPSScript(CurrScript.SynEdit.Lines.Text);
    ScriptThread.SetDebug(Self.Memo1);
    ScriptThread.OnError:=@ErrorThread;
    if ScriptFile <> '' then
      ScriptThread.SetPaths( ExtractFileDir(ScriptFile) + DS,IncludeTrailingPathDelimiter(ExpandFileName(MainDir +DS + '..' + DS + '..' + ds)))
    else
      ScriptThread.SetPaths('', IncludeTrailingPathDelimiter(ExpandFileName(MainDir +DS + '..' + DS + '..' + ds)));

    // This doesn't actually set the Client's MWindow to the passed window, it
    // only copies the current set window handle.
    ScriptThread.Client.MWindow.SetWindow(Self.Window);

    // we MUST set the OCR Path
    writeln(IncludeTrailingPathDelimiter(ExpandFileName(MainDir +DS + '..' + DS + '..' + ds)) + DS + 'Fonts' + DS);
    ScriptThread.Client.MOCR.InitTOCR(IncludeTrailingPathDelimiter(ExpandFileName(MainDir +DS + '..' + DS + '..' + ds)) + 'Fonts' + DS);

    ScriptThread.OnTerminate:=@ScriptThreadTerminate;
    ScriptState:= ss_Running;
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
          ScriptThread.PSScript.Stop;
          ScriptState := ss_Stopping;
        end;
      ss_Paused:
        begin
          ScriptThread.Resume;
          ScriptThread.PSScript.Stop;
          ScriptState:= ss_Stopping;
        end;
    end;
  end;
end;

procedure TForm1.Cut;
begin
  CurrScript.SynEdit.CutToClipboard;
end;

procedure TForm1.Copy;
begin
  CurrScript.SynEdit.CopyToClipboard;
end;

procedure TForm1.Paste;
begin
  CurrScript.SynEdit.PasteFromClipboard;
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

function TForm1.DeleteTab(TabIndex: integer; CloseLast : boolean) : boolean;
var
  Tab : TMufasaTab;
  OldIndex : integer;//So that we can switch back, if needed.
begin
  OldIndex := PageControl1.TabIndex;
  PageControl1.TabIndex:= TabIndex;
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
    if OldIndex > TabIndex then
      PageControl1.TabIndex := OldIndex - 1
    else if OldIndex < TabIndex then
      PageControl1.TabIndex := OldIndex;
  end;
  if tabs.count <= 1 then
  begin;
    TB_SaveAll.Enabled:= false;
    MenuItemSaveAll.Enabled:= false;
  end;
end;

procedure TForm1.ClearTab(TabIndex: integer);
begin
  TMufasaTab(Tabs[TabIndex]).Clear;
end;

procedure TForm1.CloseTabs(Exclude: integer);
var
  I : integer;
begin
  for i := tabs.count - 1 downto 0 do
    if i <> exclude then
      if not DeleteTab(i,false) then
        exit;
end;

procedure TForm1.CloseTabs;
begin
  CloseTabs(-1);
end;

procedure TForm1.RefreshTab;
var
  Tab : TMufasaTab;
  Script : TScriptFrame;
  NewTab : integer;
begin
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
    Self.Caption := Format(WindowTitle,[Script.ScriptName]);
    Tab.TabSheet.Caption:= Script.ScriptName;
  end;
  StatusBar.Panels[Panel_ScriptName].Text:= Script.ScriptName;
  StatusBar.Panels[Panel_ScriptPath].text:= Script.ScriptFile;
  SetScriptState(Tab.ScriptFrame.FScriptState);//To set the buttons right
  if Self.Showing then
    if Tab.TabSheet.TabIndex = Self.PageControl1.TabIndex then
      CurrScript.SynEdit.SetFocus;
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

procedure TForm1.ActionClearDebugExecute(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm1.ActionNewExecute(Sender: TObject);
begin
  Self.ClearScript;
end;

procedure TForm1.ActionNewTabExecute(Sender: TObject);
begin
  Self.AddTab;
end;

procedure TForm1.ActionOpenExecute(Sender: TObject);
begin
  Self.OpenScript;
end;

procedure TForm1.ActionPauseExecute(Sender: TObject);
begin
  Self.PauseScript;
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

procedure TForm1.EditSearchChange(Sender: TObject);
begin
  Writeln(LabeledEditSearch.Text);
  CurrScript.SynEdit.SelStart:= -1;
  CurrScript.SynEdit.SearchReplaceEx(LabeledEditSearch.Text,'',[],point(0,0));
  with CurrScript.SynEdit do
    SetHighlightSearch(LabeledEditSearch.text,[]);
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
  //Don't show search panel now
  SearchPanel.Height:= 0;
  Tabs := TList.Create;
  AddTab;//Give it alteast 1 tab ;-).
  Window := TMWindow.Create;
  Picker := TMColorPicker.Create(Window);
  Selector := TMWindowSelector.Create(Window);
  MainDir:= ExtractFileDir(Application.ExeName);
  PluginsGlob := TMPlugins.Create;
  PluginsGlob.PluginDirs.Add(ExpandFileName(MainDir + DS + '..' + DS + '..'+ DS + 'Plugins'+ DS));
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
  Window.Free;
  PluginsGlob.Free;
end;

procedure TForm1.FormShortCuts(var Msg: TLMKey; var Handled: Boolean);
begin
  Handled := ActionList.IsShortCut(Msg);
{  ShiftState := MsgKeyDataToShiftState(Message.KeyData);
  ShortCut := KeyToShortCut(Message.CharCode, ShiftState);}
end;

procedure TForm1.MenuItemCloseTabsClick(Sender: TObject);
begin
  Self.CloseTabs;
end;

procedure TForm1.MenuItemCopyClick(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused then
    Self.Copy
  else if Memo1.Focused then
    Memo1.CopyToClipboard;
end;

procedure TForm1.MenuItemCutClick(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused then
    Self.Cut
  else if Memo1.Focused then
    Memo1.CutToClipboard;
end;

procedure TForm1.MenuItemExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.MenuItemFindClick(Sender: TObject);
begin
  if SearchPanel.Height > 0 then
    SearchPanel.Height:= 0
  else
    SearchPanel.Height:= 27;
  with  CurrScript.SynEdit do
  begin;
    UseIncrementalColor:= true;
    if SearchPanel.Visible then
      MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable
    else
      MarkupByClass[TSynEditMarkupHighlightAllCaret].TempEnable;
  end;
end;



procedure TForm1.MenuItemPasteClick(Sender: TObject);
begin
  if CurrScript.SynEdit.Focused then
    Self.Paste
  else if Memo1.Focused then
    Memo1.PasteFromClipboard;
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
begin
  Picker.Pick(c, x, y);
  writeln('Picked colour: ' + inttostr(c) + ' at (' + inttostr(x) + ', ' + inttostr(y) + ')');
end;


procedure TForm1.MenuItemRedoClick(Sender: TObject);
begin
  CurrScript.redo;
end;

procedure TForm1.ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Window.SetTarget(Selector.Drag {$ifdef MSWINDOWS},w_window{$endif});
  writeln('New window: ' + IntToStr(Window.{$ifdef MSWindows}TargetHandle{$else}CurWindow{$ENDIF}));
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

procedure TForm1.MenuItemUndoClick(Sender: TObject);
begin
  CurrScript.Undo;
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
begin
  if Sender = PageControl1 then;
    Accept := True;
end;

procedure TForm1.PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {$ifdef mswindows}
  PageControl1.BeginDrag(false);
  {$endif}
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


procedure TForm1.SafeCallThread;
begin
  Writeln('Executing : ' + CurrentSyncInfo.MethodName);
  mmlpsthread.CurrThread := TMMLPSTHREAD(CurrentSyncInfo.OldThread);
  with CurrentSyncInfo.PSScript do
  begin;
    OnLine:=@OnLinePSScript;
    CurrentSyncInfo.Res:= Exec.RunProcPVar(CurrentSyncInfo.V,Exec.GetProc(CurrentSyncInfo.MethodName));
    Online := nil;
  end;
  mmlpsthread.CurrThread := nil;
end;

function TForm1.OpenScript: boolean;
begin;
  Result := False;
  if CanExitOrOpen = false then
    Exit;
  with TOpenDialog.Create(nil) do
  try
    Filter:= 'Mufasa Files|*.cogat;*.mufa;*.txt|Any files|*.*';
    if Execute then
      if FileExists(FileName) then
      begin;
        with CurrScript do
        begin
          SynEdit.Lines.LoadFromFile(FileName);
          StartText := SynEdit.Lines.text;
          ScriptName:= ExtractFileNameOnly(FileName);
          WriteLn('Script name will be: ' + ScriptName);
          ScriptFile:= FileName;
          ScriptChanged := false;
          RefreshTab();
          Result := True;
        end;
      end;
  finally
    Free;
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
end;

function TForm1.SaveCurrentScriptAs: boolean;
begin
  with CurrScript do
  begin;
    Result := false;
    with TSaveDialog.Create(nil) do
    try
      Filter:= 'Mufasa files|*.cogat;*.mufa;*.pas;*.txt|Any Files|*.*';
      if Execute then
      begin;
        if ExtractFileExt(FileName) = '' then
        begin;
          ScriptFile := FileName + '.mufa';
        end else
          ScriptFile := FileName;
        SynEdit.Lines.SaveToFile(ScriptFile);
        ScriptName:= ExtractFileNameOnly(ScriptFile);
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
      Case MessageBox(0,pchar('Do you want to stop the script?'), Pchar('Script is still running.'),
                               MB_YESNOCANCEL or MB_ICONQUESTION) of
          IDYES : StopScript;
      end;
    end else
      Case MessageBox(0,pchar('Do you want to terminate the script?'), Pchar('Script is stopping.'),
                               MB_YESNOCANCEL or MB_ICONQUESTION) of
          IDNO,IDCancel: Result := false;
          IDYES : StopScript;
      end;
  end;
  if Result and (CurrScript.StartText <> CurrScript.SynEdit.Lines.text) then
    Case MessageBox(0,pchar('Do you want to save the script?'), Pchar('Script has been modified.'),
                MB_YESNOCANCEL or MB_ICONQUESTION) of
          IDCANCEL : Result := False;
          IDYES : Result := SaveCurrentScript;
      end;
  Self.Enabled := True;
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
  ScriptFrame.Align:= alClient;
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

