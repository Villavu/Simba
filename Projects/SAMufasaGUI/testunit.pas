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
    ToolButton4: TToolButton;
    TB_ClearDebug: TToolButton;
    TB_PickColour: TToolButton;
    TB_SelectClient: TToolButton;
    ToolButton8: TToolButton;
    TB_Convert: TToolButton;
    MTrayIcon: TTrayIcon;
    procedure Button1Click(Sender: TObject);
    procedure ClearDebug(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemRunClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure OnSyneditChange(Sender: TObject);
    procedure PickColorEvent(Sender: TObject);
    procedure Redo(Sender: TObject);
    procedure Selector_DOWN(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NoTray(Sender: TObject);
    procedure SynEditProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ToTray(Sender: TObject);
    procedure Undo(Sender: TObject);
  private
    ScriptPath : string;//The path to the saved/opened file currently in the SynEdit
    StartText : string;//The text synedit holds upon start/open/save
    ScriptName : string;//The name of the currently opened/saved file.
    ScriptDefault : string;//The default script e.g. program new; begin end.
    ScriptChanged : boolean;//We need this for that little * (edited star).
  public
    Window: TMWindow;
    Picker: TMColorPicker;
    Selector: TMWindowSelector;
    function OpenScript : boolean;
    function SaveCurrentScript : boolean;
    function SaveCurrentScriptAs : boolean;
    function CanExitOrOpen : boolean;
    function ClearScript : boolean;
  end;
const
  WindowTitle = 'Mufasa v2 - %s';//Title, where %s = the place of the filename.
var
  Form1: TForm1;


implementation
uses
   lclintf,plugins;



{ TForm1 }
procedure Run;
Var
  MMLPSThread : TMMLPSThread;

begin
  MMLPSThread := TMMLPSThread.Create(True);
  MMLPSThread.SetPSScript(Form1.SynEdit1.Lines.Text);
  MMLPSThread.SetDebug(Form1.Memo1);

  // This doesn't actually set the Client's MWindow to the passed window, it
  // only copies the current set window handle.
  MMLPSThread.Client.MWindow.SetWindow(Form1.Window);

  MMLPSThread.Resume;

 // sleep(500);
 // MMLPSThread.PSScript.Stop;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin;
  Run;
end;

procedure TForm1.ClearDebug(Sender: TObject);
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

procedure TForm1.MenuFileClick(Sender: TObject);
begin

end;

procedure TForm1.MenuItemNewClick(Sender: TObject);
begin
  ClearScript;
end;

procedure TForm1.MenuItemOpenClick(Sender: TObject);
begin
  OpenScript;
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

procedure TForm1.OnSyneditChange(Sender: TObject);
begin
  if not ScriptChanged then
  begin;
    ScriptChanged:= True;
    Self.Caption:= Format(WindowTitle,[ScriptName + '*']);
  end;
end;

procedure TForm1.PickColorEvent(Sender: TObject);
var
   c, x, y: Integer;
begin
  Picker.Pick(c, x, y);
  writeln('Picked colour: ' + inttostr(c) + ' at (' + inttostr(x) + ', ' + inttostr(y) + ')');
end;

procedure TForm1.Redo(Sender: TObject);
begin
  SynEdit1.Redo;
  if ScriptChanged then
    if SynEdit1.Lines.Text = StartText then
    begin;
      Self.Caption:= format(WindowTitle,[ScriptName]);
      ScriptChanged := false;
    end;
end;

procedure TForm1.Selector_DOWN(Sender: TObject; Button: TMouseButton;
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
    Undo(Sender);
  end else
  if Command = ecRedo then
  begin;
    Command := ecNone;
    Redo(Sender);
  end;
end;

procedure TForm1.ToTray(Sender: TObject);
begin
  Form1.Hide;
end;

procedure TForm1.Undo(Sender: TObject);
begin
  SynEdit1.Undo;
  if ScriptChanged then
    if SynEdit1.Lines.Text = StartText then
    begin;
      Self.Caption:= format(WindowTitle,[ScriptName]);
      ScriptChanged := false;
    end;
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
        ScriptPath:= FileName;
        StatusBar.Panels[0].Text:= ScriptName;
        StatusBar.Panels[1].text:= ScriptPath;
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
  Result := (ScriptPath <> '');
  if Result then
  begin;
    ScriptChanged := false;
    SynEdit1.Lines.SaveToFile(ScriptPath);
    StartText:= SynEdit1.Lines.Text;
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
        ScriptPath := FileName + '.mufa';
      end else
        ScriptPath := FileName;
      SynEdit1.Lines.SaveToFile(ScriptPath);
      ScriptName:= ExtractFileNameOnly(ScriptPath);
      StatusBar.Panels[0].Text:= ScriptName;
      StatusBar.Panels[1].text := ScriptPath;
      Self.Caption:= Format(WindowTitle,[ScriptName]);
      WriteLn('Script name will be: ' + ScriptName);
      Result := True;
    end;
  finally
    Free;
  end;
  if result then
  begin;
    Writeln('Succesfully saved: ' + ScriptPath);
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
    ScriptPath:= '';
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

