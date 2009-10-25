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
  colourpicker,
  windowselector,
  lcltype
  , SynEditKeyCmds;

type
  TScriptState = (ss_None,ss_Running,ss_Paused,ss_Stopping);
  {
    ss_None: Means the script either hasn't been run yet, or it has ended (Succesfully or terminated)
    ss_Running: Means the script is running as we speak :-)
    ss_Paused: Means the script is currently in pause modus.
    ss_Stopping: Means we've asked PS-Script politely to stop the script (next time we press the stop button we won't be that nice).
  }
  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
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
    PanelSynedit: TPanel;
    PanelMemo: TPanel;
    SplitterMemoSynedit: TSplitter;
    TrayPopup: TPopupMenu;
    StatusBar: TStatusBar;
    SynEdit1: TSynEdit;
    SynFreePascalSyn1: TSynFreePascalSyn;
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
    procedure ButtonDragClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
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
    procedure OnSyneditChange(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoTray(Sender: TObject);
    procedure ScriptThreadTerminate(Sender: TObject);
    procedure SynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
  private
    ScriptFile : string;//The path to the saved/opened file currently in the SynEdit
    StartText : string;//The text synedit holds upon start/open/save
    ScriptName : string;//The name of the currently opened/saved file.
    ScriptDefault : string;//The default script e.g. program new; begin end.
    ScriptChanged : boolean;//We need this for that little * (edited star).
    ScriptThread : TMMLPSThread;//Just one thread for now..
    FScriptState : TScriptState;//Defines the "state" of the current script. (Acces through property for form updates).
    procedure SetScriptState(const State: TScriptState);
  public
    Window: TMWindow;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    property ScriptState : TScriptState read FScriptState write SetScriptState;
    procedure SafeCallThread;
    function OpenScript : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
    procedure RunScript;
    procedure PauseScript;
    procedure StopScript;
    procedure undo;
    procedure redo;
    procedure Cut;
    procedure Paste;
  end;
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
  CurrentSyncInfo.SyncMethod:= @Self.SafeCallThread;
  ScriptThread := TMMLPSThread.Create(True,@CurrentSyncInfo);
  ScriptThread.SetPSScript(Self.SynEdit1.Lines.Text);
  ScriptThread.SetDebug(Self.Memo1);
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

procedure TForm1.PauseScript;
begin
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

procedure TForm1.StopScript;
begin
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

procedure TForm1.Undo;
begin
  SynEdit1.Undo;
  if ScriptChanged then
    if SynEdit1.Lines.Text = StartText then
    begin;
      Self.Caption:= format(WindowTitle,[ScriptName]);
      ScriptChanged := false;
    end;
end;

procedure TForm1.Redo;
begin
  SynEdit1.Redo;
  if ScriptChanged then
    if SynEdit1.Lines.Text = StartText then
    begin;
      Self.Caption:= format(WindowTitle,[ScriptName]);
      ScriptChanged := false;
    end;
end;

procedure TForm1.Cut;
begin
  SynEdit1.CutToClipboard;
end;

procedure TForm1.Paste;
begin
  SynEdit1.PasteFromClipboard;
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

procedure TForm1.ButtonDragClick(Sender: TObject);
begin

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
begin
  if not CanExitOrOpen then
    CloseAction := caNone;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Window := TMWindow.Create;
  Picker := TMColorPicker.Create(Window);
  Selector := TMWindowSelector.Create(Window);
  StartText:= SynEdit1.Lines.text;
  ScriptDefault:= StartText;
  Caption := Format(WindowTitle,['Untitled']);
  ScriptName:= 'Untitled';
  ScriptChanged := false;
  ScriptState:= ss_None;
  MainDir:= ExtractFileDir(Application.ExeName);
  PluginsGlob := TMPlugins.Create;
  PluginsGlob.PluginDirs.Add(ExpandFileName(MainDir + DS + '..' + DS + '..'+ DS + 'Plugins'+ DS));
//  SynMemo1.sc
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
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

procedure TForm1.OnSyneditChange(Sender: TObject);
begin
  if not ScriptChanged then
  begin;
    ScriptChanged:= True;
    Self.Caption:= Format(WindowTitle,[ScriptName + '*']);
  end;
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
  Self.redo;
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

procedure TForm1.ScriptThreadTerminate(Sender: TObject);
begin
  ScriptState:= ss_None;
end;

procedure TForm1.SynEditProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  if Command = ecUndo then
  begin;
    Command:= ecNone;
    Self.Undo;
  end else
  if Command = ecRedo then
  begin;
    Command := ecNone;
    Self.Redo;
  end;
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
  Self.Undo;
end;

procedure TForm1.SetScriptState(const State: TScriptState);
begin
  FScriptState:= State;
  with Self.StatusBar.panels[Panel_State] do
    case FScriptState of
      ss_Running : begin Text := 'Running'; TB_Run.Enabled:= False; {$ifdef MSWindows}TB_Pause.Enabled:= True; {$endif} TB_Stop.Enabled:= True; end;
      ss_Paused  : begin Text := 'Paused'; TB_Run.Enabled:= True; {$ifdef MSWindows}TB_Pause.Enabled:= True; {$endif} TB_Stop.Enabled:= True; end;
      ss_Stopping: begin Text := 'Stopping';TB_Run.Enabled:= False; TB_Pause.Enabled:= False; TB_Stop.Enabled:= True; TB_Stop.ImageIndex := Image_Terminate end;
      ss_None    : begin Text := 'Done'; TB_Run.Enabled:= True; TB_Pause.Enabled:= False; TB_Stop.Enabled:= False; TB_Stop.ImageIndex := Image_Stop end;
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
        SynEdit1.Lines.LoadFromFile(FileName);
        StartText := SynEdit1.Lines.text;
        ScriptName:= ExtractFileNameOnly(FileName);
        WriteLn('Script name will be: ' + ScriptName);
        ScriptFile:= FileName;
        StatusBar.Panels[Panel_ScriptName].Text:= ScriptName;
        StatusBar.Panels[Panel_ScriptPath].text:= FileName;
        Self.Caption:= Format(WindowTitle,[ScriptName]);
        ScriptChanged := false;
        Result := True;
      end;
  finally
    Free;
  end;
end;

function TForm1.SaveCurrentScript: boolean;
begin
  Result := (ScriptFile <> '');
  if Result then
  begin;
    ScriptChanged := false;
    SynEdit1.Lines.SaveToFile(ScriptFile);
    StartText:= SynEdit1.Lines.Text;
    SynEdit1.MarkTextAsSaved;
    Self.Caption:= Format(WindowTitle,[ScriptName]);
  end
  else
    result := SaveCurrentScriptAs;
end;

function TForm1.SaveCurrentScriptAs: boolean;
begin
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
      SynEdit1.Lines.SaveToFile(ScriptFile);
      ScriptName:= ExtractFileNameOnly(ScriptFile);
      StatusBar.Panels[Panel_ScriptName].Text:= ScriptName;
      StatusBar.Panels[Panel_ScriptPath].text := ScriptFile;
      Self.Caption:= Format(WindowTitle,[ScriptName]);
      WriteLn('Script name will be: ' + ScriptName);
      Result := True;
    end;
  finally
    Free;
  end;
  if result then
  begin;
    Writeln('Succesfully saved: ' + ScriptFile);
    StartText:= SynEdit1.Lines.Text;
    SynEdit1.MarkTextAsSaved;
    ScriptChanged := false;
  end;
end;

function TForm1.CanExitOrOpen: boolean;
var
  I : integer;
begin;
  Result := True;
  if StartText <> Synedit1.Lines.text then
    Case MessageBox(0,pchar('Do you want to save the script?'), Pchar('Script has been modified.'),
                MB_YESNOCANCEL or MB_ICONQUESTION) of
          IDCANCEL : Result := False;
          IDYES : Result := SaveCurrentScript;
      end;
end;

function TForm1.ClearScript: boolean;
begin
  if CanExitOrOpen then
  begin;
    ScriptFile:= '';
    ScriptName:= 'Untitled';
    StartText:= ScriptDefault;
    SynEdit1.Lines.Text:= ScriptDefault;
    StatusBar.Panels[Panel_ScriptName].Text:= 'Untitled';
    StatusBar.Panels[Panel_ScriptPath].Text:= '';
    Self.Caption := Format(WindowTitle,['Untitled']);
    ScriptChanged:= false;
  end;
end;


initialization
  {$I testunit.lrs}

end.

