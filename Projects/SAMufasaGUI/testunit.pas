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

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
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
    ToolButton3: TToolButton;
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
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuEditClick(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemRunClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure OnLinePSScript(Sender: TObject);
    procedure OnSyneditChange(Sender: TObject);
    procedure ButtonPickClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure ButtonSelectorDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoTray(Sender: TObject);
    procedure SynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ToolBar1Click(Sender: TObject);
    procedure ButtonTrayClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
  private
    ScriptFile : string;//The path to the saved/opened file currently in the SynEdit
    StartText : string;//The text synedit holds upon start/open/save
    ScriptName : string;//The name of the currently opened/saved file.
    ScriptDefault : string;//The default script e.g. program new; begin end.
    ScriptChanged : boolean;//We need this for that little * (edited star).
  public
    Window: TMWindow;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    procedure SafeCallThread;
    function OpenScript : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
    procedure run;
    procedure undo;
    procedure redo;
    procedure Cut;
    procedure Paste;
  end;
const
  WindowTitle = 'Mufasa v2 - %s';//Title, where %s = the place of the filename.
var
  Form1: TForm1;
  MainDir : string;
  CurrentSyncInfo : TSyncInfo;//We need this for SafeCallThread

implementation
uses
   lclintf,plugins;



procedure TForm1.Run;
Var
  MMLPSThread : TMMLPSThread;

begin
  CurrentSyncInfo.SyncMethod:= @Self.SafeCallThread;
  MMLPSThread := TMMLPSThread.Create(True,@CurrentSyncInfo);
  MMLPSThread.SetPSScript(Self.SynEdit1.Lines.Text);
  MMLPSThread.SetDebug(Self.Memo1);
  if ScriptFile <> '' then
    MMLPSThread.SetPaths( ExtractFileDir(ScriptFile) + DS,IncludeTrailingPathDelimiter(ExpandFileName(MainDir +DS + '..' + DS + '..' + ds)))
  else
    MMLPSThread.SetPaths('', IncludeTrailingPathDelimiter(ExpandFileName(MainDir +DS + '..' + DS + '..' + ds)));

  // This doesn't actually set the Client's MWindow to the passed window, it
  // only copies the current set window handle.
  MMLPSThread.Client.MWindow.SetWindow(Self.Window);

  MMLPSThread.Resume;

 // sleep(500);
 // MMLPSThread.PSScript.Stop;

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
  Run;
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

procedure TForm1.MenuItemRunClick(Sender: TObject);
begin
  Run;
end;

procedure TForm1.MenuItemSaveAsClick(Sender: TObject);
begin
  SaveCurrentScriptAs;
end;

procedure TForm1.MenuItemSaveClick(Sender: TObject);
begin
  SaveCurrentScript;
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

procedure TForm1.ToolBar1Click(Sender: TObject);
begin

end;

procedure TForm1.ButtonTrayClick(Sender: TObject);
begin
  Form1.Hide;
end;

procedure TForm1.MenuItemUndoClick(Sender: TObject);
begin
  Self.Undo;
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
        StatusBar.Panels[0].Text:= ScriptName;
        StatusBar.Panels[1].text:= FileName;
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
      StatusBar.Panels[0].Text:= ScriptName;
      StatusBar.Panels[1].text := ScriptFile;
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
    StatusBar.Panels[0].Text:= 'Untitled';
    StatusBar.Panels[1].Text:= '';
    Self.Caption := Format(WindowTitle,['Untitled']);
    ScriptChanged:= false;
  end;
end;


initialization
  {$I testunit.lrs}

end.

