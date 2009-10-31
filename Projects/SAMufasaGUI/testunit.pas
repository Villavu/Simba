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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, SynEdit, SynHighlighterPas, SynMemo,
  //Client,
  MufasaTypes,
  mmlpsthread,
  window, // for the comp picker and selector
  colourpicker, framescript,
  windowselector,
  lcltype
  , SynEditKeyCmds,SynEditHighlighter, SynEditMarkupSpecialLine, SynEditMiscClasses;

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
    Memo1: TMemo;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
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
    PageControl1: TPageControl;
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
    procedure ButtonCloseTabClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonNewTabClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemPauseClick(Sender: TObject);
    procedure MenuItemRunClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuItemShowClick(Sender: TObject);
    procedure MenuItemStopClick(Sender: TObject);
    procedure OnLinePSScript(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoTray(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure TrayPauseClick(Sender: TObject);
    procedure TrayPlayClick(Sender: TObject);
    procedure TrayStopClick(Sender: TObject);
  private
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
    procedure Paste;
    procedure AddTab;
    function DeleteTab( TabIndex : integer; CloseLast : boolean) : boolean;
    procedure ClearTab( TabIndex : integer);
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
          KillThread(ScriptThread.Handle);
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
  PageControl1.TabIndex:= Tabs.Count - 1;
  RefreshTab;
end;

function TForm1.DeleteTab(TabIndex: integer; CloseLast : boolean) : boolean;
var
  Tab : TMufasaTab;
begin
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
  end;
end;

procedure TForm1.ClearTab(TabIndex: integer);
begin
  TMufasaTab(Tabs[TabIndex]).Clear;
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
    CurrScript.SynEdit.SetFocus;
end;

procedure TForm1.ButtonRunClick(Sender: TObject);
begin;
  Self.RunScript;
end;

procedure TForm1.ButtonSaveClick(Sender: TObject);
begin
  Self.SaveCurrentScript;
end;

procedure TForm1.ButtonNewClick(Sender: TObject);
begin
  Self.ClearScript;
end;

procedure TForm1.ButtonNewTabClick(Sender: TObject);
begin;
  AddTab;
end;

procedure TForm1.ButtonCloseTabClick(Sender: TObject);
begin
  DeleteTab( PageControl1.TabIndex,false);
end;

procedure TForm1.ButtonOpenClick(Sender: TObject);
begin
  Self.OpenScript;
end;

procedure TForm1.ButtonPauseClick(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  Memo1.Clear;
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

procedure TForm1.MenuEditClick(Sender: TObject);
begin

end;

procedure TForm1.MenuFileClick(Sender: TObject);
begin

end;

procedure TForm1.MenuItemCutClick(Sender: TObject);
begin
  Self.cut;
end;

procedure TForm1.MenuItemExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.MenuItemNewClick(Sender: TObject);
begin
  ClearScript;
end;

procedure TForm1.MenuItemOpenClick(Sender: TObject);
begin
  OpenScript;
end;

procedure TForm1.MenuItemPasteClick(Sender: TObject);
begin
  Self.Paste;
end;

procedure TForm1.MenuItemPauseClick(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TForm1.MenuItemRunClick(Sender: TObject);
begin
  RunScript;
end;

procedure TForm1.MenuItemSaveAsClick(Sender: TObject);
begin
  SaveCurrentScriptAs;
end;

procedure TForm1.MenuItemSaveClick(Sender: TObject);
begin
  SaveCurrentScript;
end;

procedure TForm1.MenuItemShowClick(Sender: TObject);
begin
  Self.Show;
  Self.WindowState := wsNormal;
end;

procedure TForm1.MenuItemStopClick(Sender: TObject);
begin
  self.StopScript;
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
    Form1.Show
  else
    Form1.Hide;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  RefreshTab();
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  Self.StopScript;
end;


procedure TForm1.ButtonTrayClick(Sender: TObject);
begin
  Form1.Hide;
end;

procedure TForm1.MenuItemUndoClick(Sender: TObject);
begin
  CurrScript.Undo;
end;

procedure TForm1.TrayPauseClick(Sender: TObject);
begin
  Self.PauseScript;
end;

procedure TForm1.TrayPlayClick(Sender: TObject);
begin
  Self.RunScript;
end;

procedure TForm1.TrayStopClick(Sender: TObject);
begin
  Self.StopScript;
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

