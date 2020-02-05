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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ComCtrls, ExtCtrls, ActnList, LMessages, Buttons,
  CastaliaPasLexTypes, CastaliaSimplePasPar,
  simba.codeinsight, simba.updater, simba.package_form, simba.windowselector, simba.scripttab,
  simba.codeparser, simba.setting, LCLType, ImgList, simba.oswindow;

const
  IMAGE_COMPILE = 0;
  IMAGE_PACKAGE = 1;
  IMAGE_PACKAGE_NOTIFCATION = 23;
  IMAGE_DIRECTORY = 27;
  IMAGE_SIMBA = 28;
  IMAGE_BOOK = 29;
  IMAGE_FILE = 30;
  IMAGE_TYPE = 31;
  IMAGE_FUNCTION = 32;
  IMAGE_PROCEDURE = 33;
  IMAGE_GITHUB = 34;
  IMAGE_CONSTANT = 35;
  IMAGE_VARIABLE = 36;

type
  TScriptButtonState = (ss_None, ss_Running, ss_Paused, ss_Stopping, ss_Locked);

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
    MenuColors: TMenuItem;
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
    MenuItemOpenPluginsFolder: TMenuItem;
    MenuItemOpenIncludesFolder: TMenuItem;
    MenuItemOpenRecent: TMenuItem;
    MenuItemOpenScriptsFolder: TMenuItem;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure MenuItemColourHistoryClick(Sender: TObject);
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

    procedure OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Message: string; X, Y: Integer);
    function OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
    function OnCCFindLibrary(Sender: TObject; var FileName: string): Boolean;
    procedure OnCCLoadLibrary(Sender: TObject; FileName: String; var Contents: String);
    procedure HandleMenuViewClick(Sender: TObject);
    procedure ResetDockingSplitters(Data: PtrInt);
    procedure SettingChanged_CustomToolbarSize(Value: Int64);
    procedure SettingChanged_CustomFontSize(Value: Int64);
  private
    function GetLayerLocked: Boolean;
    procedure SetLayoutLocked(Value: Boolean);
  protected
    News: String;
    FWindowSelection: TOSWindow;
    FScriptState: TScriptButtonState;

    procedure PrintDTM(constref DTM: String);

    function GetScriptState: TScriptButtonState;
    procedure GetSimbaNews;
    procedure WriteSimbaNews(Sender: TObject);

    procedure SetTrayIconVisible(Value: Boolean);
    procedure SetConsoleVisible(Value: Boolean);

    procedure SetScriptState(const State: TScriptButtonState);

    procedure CustomExceptionHandler(Sender: TObject; E: Exception);

    function LoadLayout: Boolean;
    procedure SaveLayout;
    procedure SetupLayout(Reset: Boolean = False);
    procedure SetEnabled(Value: Boolean); override;
    procedure SetVisible(Value: boolean); override;
  public
    Initialized: Boolean;

    property WindowSelection: TOSWindow read FWindowSelection;

    property ScriptButtonState : TScriptButtonState read FScriptState write SetScriptState;
    property ConsoleVisible: Boolean write SetConsoleVisible;
    property LayoutLocked: Boolean read GetLayerLocked write SetLayoutLocked;

    procedure RunScript;
    procedure CompileScript;
    procedure PauseScript;
    procedure StopScript;

    procedure AddRecentFile(const FileName: String);

    procedure DoSimbaNews(Data: PtrInt);

    procedure Error(Message: String);
    procedure WndProc(var Message: TLMessage); override;

    procedure AddSettingChangeHandlers;
    procedure RemoveSettingChangeHandlers;

    procedure Init(Data: PtrInt);
    procedure InitConsole;
    procedure InitDocking;
    procedure InitSettings;
    procedure InitCodeTools;
    procedure InitSimbaScript;
    procedure InitOpenSSL;
    procedure Startup_Parameters;

    procedure ShowForm(Form: TForm);
    procedure HideForm(Form: TForm);
    procedure CenterForm(Form: TForm);
  end;

var
  SimbaForm: TSimbaForm;

implementation

uses
  clipbrd, lclintf, lazfileutils,  SynExportHTML, anchordocking, xmlconf, simba.mufasatypes,
  XMLPropStorage, AnchorDockStorage,
  simba.misc, simba.mufasabase, simba.settings,
  simba.httpclient, simba.files, simba.resourceextractor,
  simba.debugimage, simba.bitmapconv, simba.colorpicker_historyform,
  simba.aca, simba.dtmeditor,  simba.scriptinstance,
  simba.aboutform,  simba.functionlistform, simba.scripttabsform, simba.debugform, simba.filebrowserform,
  simba.notesform, simba.settingsform, simba.colorpicker, simba.ci_includecache
  {$IFDEF WINDOWS},
  windows
  {$ENDIF}
  {$IFDEF USE_FORMDESIGNER},
  simba.formdesigner
  {$ENDIF}
  {$IFDEF LINUX_HOTKEYS},
  simba.linux_keybinder
  {$ENDIF},
  dynlibs;

const
  {$IFDEF LINUX}
  shortcut_StartScript = '<Ctrl><Alt>R';
  shortcut_StopScript =  '<Ctrl><Alt>S';
  shortcut_PickColour =  '<Ctrl><Alt>P';
  {$ELSE}
  shortcut_StartScriptID  = 0;
  shortcut_StartScriptMod = MOD_CONTROL or MOD_ALT;
  shortcut_StartScriptKey = VK_R;

  shortcut_StopScriptID   = 1;
  shortcut_StopScriptMod  = MOD_CONTROL or MOD_ALT;
  shortcut_StopScriptKey  = VK_S;

  shortcut_PickColorID    = 2;
  shortcut_PickColourMod  = MOD_CONTROL or MOD_ALT;
  shortcut_PickColourKey  = VK_P;
  {$ENDIF}

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

    procedure SetMenuItem(Value: TMenuItem);
    procedure SetVisible(Value: Boolean); override;
    procedure SetParent(Value: TWinControl); override;
  public
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;

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
begin
  inherited UpdateDockCaption(Exclude);

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

{$IFDEF WINDOWS}

procedure Bind_Windows_Keys;
begin
  if not RegisterHotkey(SimbaForm.Handle,shortcut_StartScriptID,shortcut_StartScriptMod,shortcut_StartScriptKey) then
    WriteLn('Unable to register start script global hotkey');
  if not RegisterHotkey(SimbaForm.Handle,shortcut_StopScriptID,shortcut_StopScriptMod,shortcut_StopScriptKey) then
    WriteLn('Unable to register stop script global hotkey');
  if not RegisterHotkey(SimbaForm.Handle,shortcut_PickColorID,shortcut_PickColourMod,shortcut_PickColourKey) then
    WriteLn('Unable to register pick colour global hotkey');
end;

procedure Unbind_Windows_Keys;
var
  i : integer;
begin
  for i := 0 to 2 do
    if not UnRegisterHotkey(SimbaForm.Handle,i) then
      WriteLn('Unable to unregister '+ inttostr(i) + ' global hotkey');
end;


{$ELSE}
  {$IFDEF LINUX_HOTKEYS}
  {$WARNING This will probably not work if people don't have libkeybinder installed. Perhaps ship it with Simba? }

{ Used for global callbacks on LINUX }
procedure keybinder_callback(keystring: PChar; user_data: PtrUInt); cdecl;
begin
  //if keystring = shortcut_StartScript then
  //  SimbaForm.ActionRunScript.Execute
  //else if keystring = shortcut_StopScript then
   // SimbaForm.ActionStopScript.Execute
  //else if keystring = shortcut_PickColour then
  //  SimbaForm.ButtonPickClick(nil);
end;

{ XXX, TODO: Pressing the stop shortcut twice (quickly) may crash Simba. }
procedure Bind_Linux_Keys;
begin
  if KeybinderLoaded() then
  begin
    keybinder_init(); { Initialise keybinder }

    { Bind keys }
    if not keybinder_bind(PChar(shortcut_StartScript), @keybinder_callback, PtrUInt(0)) then
      WriteLn('Unable to register '+ shortcut_StartScript + ' as global hotkey');
    if not keybinder_bind(PChar(shortcut_StopScript), @keybinder_callback, PtrUInt(0)) then
      WriteLn('Unable to register '+ shortcut_StopScript + ' as global hotkey');
    if not keybinder_bind(PChar(shortcut_PickColour), @keybinder_callback, PtrUInt(0)) then
      WriteLn('Unable to register '+ shortcut_PickColour + ' as global hotkey');
  end;
end;

procedure Unbind_Linux_Keys;
begin
  if KeybinderLoaded() then
  begin
    keybinder_unbind(PChar(shortcut_StartScript), @keybinder_callback, PtrUInt(0));
    keybinder_unbind(PChar(shortcut_StopScript), @keybinder_callback, PtrUInt(0));
    keybinder_unbind(PChar(shortcut_PickColour), @keybinder_callback, PtrUInt(0));
  end;
end;
  {$ENDIF}

{$ENDIF}

procedure TSimbaForm.OnCCMessage(Sender: TObject; const Typ: TMessageEventType; const Message: string; X, Y: Integer);
begin
  if TCodeParser(Sender).Lexer.FileName <> '' then
    WriteLn(Format('Parser: %s at line %d, column %d in file "%s"', [Message, Y, X, TCodeParser(Sender).Lexer.FileName]))
  else
    WriteLn(Format('Parser: %s at line %d, column %d', [Message, Y, X]))
end;

function TSimbaForm.OnCCFindInclude(Sender: TObject; var FileName: string): Boolean;
begin
  Result := FindFile(FileName, [ExtractFileDir(TCodeParser(Sender).Lexer.FileName), SimbaSettings.Environment.IncludePath.Value, Application.Location]);
end;

function TSimbaForm.OnCCFindLibrary(Sender: TObject; var FileName: string): Boolean;
begin
  Result := FindPlugin(FileName, [ExtractFileDir(TCodeParser(Sender).Lexer.FileName), SimbaSettings.Environment.PluginPath.Value, Application.Location]);
end;

procedure TSimbaForm.OnCCLoadLibrary(Sender: TObject; FileName: String; var Contents: String);
var
  Lib: TLibHandle;
  Header, Info: PChar;
  Index: Int32;
  Address: Pointer;
var
  GetFunctionInfo: function(Index: Int32; var Address: Pointer; var Header: PChar): Int32; cdecl;
  GetFunctionCount: function: Int32; cdecl;
  GetType: function(Index: Int32; var Name: PChar; var Str: PChar): Int32; cdecl;
  GetTypeCount: function: Int32; cdecl;
  GetCode: procedure(var Code: PChar); cdecl;
  GetCodeLength: function: Int32; cdecl;
begin
  WriteLn('Loading library "', FileName, '"');

  try
    Lib := LoadLibrary(FileName);

    if Lib <> NilHandle then
    try
      Pointer(GetFunctionInfo) := GetProcedureAddress(Lib, 'GetFunctionInfo');
      Pointer(GetFunctionCount) := GetProcedureAddress(Lib, 'GetFunctionCount');
      Pointer(GetType) := GetProcedureAddress(Lib, 'GetTypeInfo');
      Pointer(GetTypeCount) := GetProcedureAddress(Lib, 'GetTypeCount');
      Pointer(GetCode) := GetProcedureAddress(Lib, 'GetCode');
      Pointer(GetCodeLength) := GetProcedureAddress(Lib, 'GetCodeLength');

      // Types
      if (Pointer(GetTypeCount) <> nil) and (Pointer(GetType) <> nil) then
      begin
        Header := StrAlloc(2048);
        Info := StrAlloc(2048);

        try
          for Index := 0 to GetTypeCount() - 1 do
          begin
            GetType(Index, Header, Info);

            Contents := Contents + 'type ' + Header + ' = ' + Info;
            if (not Contents.EndsWith(';')) then
              Contents := Contents + ';';
          end;
        finally
          StrDispose(Header);
          StrDispose(Info);
        end;
      end;

      // Functions
      if (Pointer(GetFunctionCount) <> nil) and (Pointer(GetFunctionInfo) <> nil) then
      begin
        Header := StrAlloc(2048);

        try
          for Index := 0 to GetFunctionCount() - 1 do
          begin
            GetFunctionInfo(Index, Address, Header);

            Contents := Contents + Header;

            // Remove native, and add trailing semicolon if needed
            if (not Contents.EndsWith(';')) then
              Contents := Contents + ';';
            if Contents.EndsWith('native;', True) then
              Contents.Remove(Length(Contents) - Length('native;'), $FFFFFF);
            if (not Contents.EndsWith(';')) then
              Contents := Contents + ';';

            Contents := Contents + 'begin end;';
          end;
        finally
          StrDispose(Header);
        end;
      end;

      // Code
      if (Pointer(GetCodeLength) <> nil) and (Pointer(GetCode) <> nil) then
      begin
        Info := StrAlloc(GetCodeLength() + 1);

        try
          GetCode(Info);

          Contents := Contents + Info;
        finally
          StrDispose(Info);
        end;
      end;
    finally
      FreeLibrary(Lib);
    end;
  except
    on E: Exception do
      WriteLn('Error loading library: ', E.Message);
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

procedure TSimbaForm.SetConsoleVisible(Value: Boolean);
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

procedure TSimbaForm.Error(Message: String);
begin
  MessageDlg('Simba Error', Message, mtError, [mbOK], 0);
end;

procedure TSimbaForm.WndProc(var Message: TLMessage);
begin
  inherited WndProc(Message);

  {$IFDEF WINDOWS}
  if Message.Msg = WM_HOTKEY then
    case Message.wParam of
      shortcut_StartScriptID:
        if RunButton.Enabled then
          RunButton.Click();

      shortcut_StopScriptID:
        if StopButton.Enabled then
          StopButton.Click();

      shortcut_PickColorID:
        ColorPickerButton.Click();
    end;
  {$ENDIF}
end;

procedure TSimbaForm.RunScript;
begin
  with SimbaScriptTabsForm.CurrentTab do
  try
    if (ScriptInstance = nil) then
    begin
      if ScriptFile <> '' then
        Save(ScriptFile);

      ScriptInstance := TSimbaScriptInstance.Create();
      ScriptInstance.ManageOutput := True;
      ScriptInstance.TargetWindow := Self.WindowSelection;
      ScriptInstance.ScriptName := ScriptName;

      if (ScriptFile <> '') then
        ScriptInstance.ScriptFile := ScriptFile
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
      if (ScriptFile <> '') then
        Save(ScriptFile);

      ScriptInstance := TSimbaScriptInstance.Create();
      ScriptInstance.ManageOutput := True;
      ScriptInstance.TargetWindow := Self.WindowSelection;
      ScriptInstance.ScriptName := ScriptName;

      if (ScriptFile <> '') then
        ScriptInstance.ScriptFile := ScriptFile
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

procedure TSimbaForm.Startup_Parameters;
var
  Script: String;
begin
  if Application.HasOption('h', 'help') then
  begin
    WriteLn('');
    WriteLn('Options:');
    WriteLn('  -open, -o: opens the given script');
    WriteLn('  -run, -r: runs the given script');
    WriteLn('  -compile, -c: compiles the given script');
    WriteLn('');
    WriteLn('Example:');
    WriteLn('  Simba.exe -run "Test.simba"');
    WriteLn('');
  end;

  if Application.ParamCount > 0 then
  begin
    Script := '';

    if (Application.ParamCount = 1) then
      Script := Application.Params[1]
    else
    if (Application.ParamCount = 2) then
      Script := Application.Params[2];

    if not FileExists(Script) then
    begin
      WriteLn('A valid script is required for this option.');
      Halt(1);
    end;

    if Application.HasOption('o', 'open') or Application.HasOption('c', 'compile') or Application.HasOption('r', 'run') then
    begin
      SimbaScriptTabsForm.Open(Script);

      if Application.HasOption('c', 'compile') then
        Self.CompileScript();
      if Application.HasOption('r', 'run') then
        Self.RunScript();
    end;
  end;
end;

procedure TSimbaForm.DoSimbaNews(Data: PtrInt);
begin
  with TProcThread.Create() do
  begin
    ClassProc := @GetSimbaNews;
    OnTerminate := @WriteSimbaNews;
    Start();
  end;
end;

procedure TSimbaForm.MenuCloseTabClick(Sender: TObject);
var
  Aborted: Boolean;
begin
  SimbaScriptTabsForm.RemoveTab(SimbaScriptTabsForm.CurrentTab, Aborted);
end;

procedure TSimbaForm.MenuCompileClick(Sender: TObject);
begin
  Self.CompileScript();
end;

// Handle old hotkeys.
procedure TSimbaForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F9) and ((Shift * [ssCtrl]) = [ssCtrl]) then
    MenuItemCompile.Click()
  else
  if (Key = VK_F9) then
    MenuItemRun.Click()
  else
  if (Key = VK_F2) then
    MenuItemStop.Click();
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
    if InputQuery('Goto line','Goto line:', Value) and (StrToIntDef(Value, -1) > -1) then
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
    SimbaScriptTabsForm.CurrentTab.Save(SimbaSettings.Editor.DefaultScriptPath.Value);
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

function TSimbaForm.GetLayerLocked: Boolean;
begin
  Result := SimbaSettings.GUI.LayoutLocked.Value;
end;

type
  TToolBar_Helper = class helper for TToolBar
    function Size: TPoint;
  end;

function TToolBar_Helper.Size: TPoint;
begin
  CalculatePreferredSize(Result.X, Result.Y, True);
end;

procedure TSimbaForm.SetLayoutLocked(Value: Boolean);
begin
  MenuItemLockLayout.Checked := Value;

  DockMaster.ShowHeader := not Value;
  DockMaster.AllowDragging := not Value;

  if OnResize <> nil then
    OnResize(nil);
end;

procedure TSimbaForm.PrintDTM(constref DTM: String);
begin
  SimbaDebugForm.Add('DTM := DTMFromString(' + #39 + DTM + #39 + ');');
end;

procedure TSimbaForm.SetVisible(Value: boolean);
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
    SimbaScriptTabsForm.CurrentTab.Save(SimbaScriptTabsForm.CurrentTab.FileName);
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
begin
  SimbaScriptTabsForm.SaveAll();
end;

procedure TSimbaForm.MenuSaveAsClick(Sender: TObject);
begin
  if SimbaScriptTabsForm.CurrentTab <> nil then
    SimbaScriptTabsForm.CurrentTab.Save();
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
        ScriptButtonState := ss_Stopping;
      end
      else
      if Tab.ScriptInstance.IsPaused then
      begin
        SimbaScriptTabsForm.StatusPanelState.Caption := 'Paused';
        ScriptButtonState := ss_Paused;
      end
      else
      begin
        ScriptButtonState := ss_Running;
        SimbaScriptTabsForm.StatusPanelState.Caption := ShortTimeStamp(Tab.ScriptInstance.TimeRunning);
      end;
    end else
    begin
      SimbaScriptTabsForm.StatusPanelState.Caption := 'Stopped';
      ScriptButtonState := ss_None;
    end;
  end;



  Output.Free();
end;

procedure TSimbaForm.MenuItemBitmapConvClick(Sender: TObject);
begin
  BitmapConvForm.Show;
end;

procedure TSimbaForm.MenuItemColourHistoryClick(Sender: TObject);
begin
  {
  MenuItemColourHistory.Checked := not ColourHistoryForm.Visible;
  if MenuItemColourHistory.Checked then
    ColourHistoryForm.Show
  else
    ColourHistoryForm.Hide;    }
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
      if LayoutLocked then
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
 // SimbaSettings.GUI.ToolbarSize.AddHandlerOnChange(@SetToolbarSize);
  SimbaSettings.GUI.TrayIconVisible.AddHandlerOnChange(@SetTrayIconVisible);
  SimbaSettings.GUI.ConsoleVisible.AddHandlerOnChange(@SetConsoleVisible);
  SimbaSettings.GUI.LayoutLocked.AddHandlerOnChange(@SetLayoutLocked);
  SimbaSettings.GUI.CustomFontSize.AddHandlerOnChange(@SettingChanged_CustomFontSize);
  SimbaSettings.GUI.CustomToolbarSize.AddHandlerOnChange(@SettingChanged_CustomToolbarSize);
end;

procedure TSimbaForm.RemoveSettingChangeHandlers;
begin
  SimbaSettings.GUI.TrayIconVisible.RemoveHandlerOnChange(@SetTrayIconVisible);
  SimbaSettings.GUI.ConsoleVisible.RemoveHandlerOnChange(@SetConsoleVisible);
  SimbaSettings.GUI.LayoutLocked.RemoveHandlerOnChange(@SetLayoutLocked);
  SimbaSettings.GUI.CustomFontSize.RemoveHandlerOnChange(@SettingChanged_CustomFontSize);
  SimbaSettings.GUI.CustomToolbarSize.RemoveHandlerOnChange(@SettingChanged_CustomToolbarSize);
end;

procedure TSimbaForm.InitConsole;
{$IFDEF WINDOWS}
var
  Mode: UInt32 = 0;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  // Close Simba when console is closed
  SetConsoleCtrlHandler(@ConsoleHandler, True);

  // Disable "Features" that can freeze Simba
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), Mode and not (ENABLE_QUICK_EDIT_MODE or ENABLE_INSERT_MODE));
  {$ENDIF}
end;

procedure TSimbaForm.InitDocking;
begin
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

procedure TSimbaForm.InitSettings;
var
  RecentFile: String;
begin
  for RecentFile in SimbaSettings.GUI.RecentFiles.Value.Split([',']) do
    AddRecentFile(RecentFile);

  AddSettingChangeHandlers();
end;

procedure TSimbaForm.InitSimbaScript;
begin
  if SimbaSettings.Resources.ExtractSimbaScript.Value then
    SimbaResourceExtractor.Extract('SIMBASCRIPT', Application.Location);
end;

procedure TSimbaForm.InitOpenSSL;
begin
  if SimbaSettings.Resources.ExtractOpenSSL.Value then
    SimbaResourceExtractor.Extract('OPENSSL', Application.Location);

  if SimbaSettings.Resources.InitializeOpenSSL.Value then
    InitializeOpenSSL(Application.Location);
end;

procedure TSimbaForm.ShowForm(Form: TForm);
begin
  if DockMaster.GetAnchorSite(Form) = nil then
    raise Exception.Create('Not a Simba form');

  DockMaster.GetAnchorSite(Form).Visible := True;
end;

procedure TSimbaForm.HideForm(Form: TForm);
begin
  if DockMaster.GetAnchorSite(Form) = nil then
    raise Exception.Create('Not a Simba form');

  DockMaster.GetAnchorSite(Form).Header.CloseButton.Click();
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

procedure TSimbaForm.InitCodeTools;
var
  ScriptInstance: TSimbaScriptInstance;
  Parser: TCodeInsight_Include;
  i: Int32;
begin
  ScriptInstance := nil;

  try
    ScriptInstance := TSimbaScriptInstance.Create();
    ScriptInstance.Script := 'begin end.';
    ScriptInstance.Dump();

    for i := 0 to ScriptInstance.Output.Count - 1 do
    begin
      Parser := TCodeInsight_Include.Create();
      Parser.OnMessage := @Self.OnCCMessage;
      Parser.Run(ScriptInstance.Output.ValueFromIndex[i], ScriptInstance.Output.Names[i]);

      if Parser.Lexer.FileName <> 'Classes' then
        SimbaFunctionListForm.addDeclarations(Parser.Items, SimbaFunctionListForm.addSimbaSection(Parser.Lexer.FileName), False, True, False);

      TCodeInsight.AddBaseInclude(Parser);
    end;

    SimbaFunctionListForm.SimbaNode.Expanded := True;
  except
    on E: Exception do
    begin
      Writeln('EXCEPTION!');
      SimbaDebugForm.Add('Error parsing internals: ' + E.Message);
    end;
  end;
   Writeln('Free');
  if ScriptInstance <> nil then
    ScriptInstance.Free();
end;

procedure TSimbaForm.Init(Data: PtrInt);
var
  i: Int32;
begin
  if (not DirectoryIsWritable(Application.Location)) then
    ShowMessage('No permission to write to Simba''s directory. This will likely cause issues.');


  InitConsole;
  InitDocking;
  InitOpenSSL;
  InitSimbaScript;
  InitOpenSSL;
  InitCodeTools;
  InitSimbaScript;
  InitSettings;
  Startup_Parameters;

  Self.ToolBar.Images := TImageList.Create(ToolBar);
  Self.ToolBar.Images.Assign(Self.Images);
  Self.ToolBar.Images.Scaled := False;

  ScriptProcessorTimer.Enabled := True;

  Application.OnException := @CustomExceptionHandler;
  Application.QueueAsyncCall(@ResetDockingSplitters, 0);

  // Everything should be loaded. Display our forms!
  Self.Initialized := True;

  for i := 0 to Screen.CustomFormCount - 1 do
    if (Screen.CustomForms[i] is TSimbaAnchorDockHostSite) and ((TSimbaAnchorDockHostSite(Screen.CustomForms[i]).MenuItem = nil) or (TSimbaAnchorDockHostSite(Screen.CustomForms[i]).MenuItem.Checked)) then
      Screen.CustomForms[i].Visible := True;

  Self.Visible := True;
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

  {$ifdef MSWindows}
  Unbind_Windows_Keys;
  {$else}
  {$IFDEF LINUX_HOTKEYS}
  Unbind_Linux_Keys;
  {$ENDIF}
  {$endif}
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
  AboutForm.ShowModal();
end;

procedure TSimbaForm.MenuItemCloseTabsClick(Sender: TObject);
var
  Aborted: Boolean;
begin
  SimbaScriptTabsForm.RemoveAllTabs(Aborted);
end;

procedure TSimbaForm.MenuItemExportHTMLClick(Sender: TObject);
var
  SynExporterHTML : TSynExporterHTML;
begin;
  {
  SynExporterHTML := TSynExporterHTML.Create(nil);
  SynExporterHTML.Highlighter := CurrentScriptTab.Editor.Highlighter;
  SynExporterHTML.ExportAsText:= True;
  with TSaveDialog.Create(nil) do
    try
      Filter:= 'HTML Files (*.html;*.htm)|*.html;*.htm|All files(*.*)|*.*';
      Options:= [ofOverwritePrompt,ofEnableSizing];
      DefaultExt:= 'html';
      if Execute then
      begin
        if CurrentScriptTab.ScriptName <> '' then
          SynExporterHTML.Title:= 'Simba - ' + CurrentScriptTab.ScriptName
        else
          SynExporterHTML.Title:= 'Simba - Untitled';
        SynExporterHTML.ExportAll(CurrentScriptTab.Editor.Lines);
        SynExporterHTML.SaveToFile(FileName);
      end;
    finally
      free;
      SynExporterHTML.Free;
    end;}
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

procedure TSimbaForm.GetSimbaNews;
begin
  try
    with TSimbaHTTPClient.Create() do
    try
      //News := Get(SimbaSettings.News.URL.Value);
      if ResponseCode <> HTTP_OK then
        WriteLn('[NEWS]: Invaild response code ', ResponseCode);
    finally
      Free();
    end;
  except
    on e: Exception do
      WriteLn('[NEWS]: Exception "', e.Message, '" encountered');
  end;
end;

procedure TSimbaForm.WriteSimbaNews(Sender: TObject);
begin
  {
  with TStringList.Create() do
  try
    Text := News; // Process line endings.

    if (Text <> '') then
      Debug(Text);
  finally
    Free();
  end;  }
end;

procedure TSimbaForm.SetTrayIconVisible(Value: Boolean);
begin
  MenuItemTrayIcon.Checked := Value;

  TrayIcon.Visible := Value;
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
      SimbaDebugForm.Add(' - Title: ' + Selected.GetTitle());
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

function TSimbaForm.GetScriptState: TScriptButtonState;
begin
  Result := FScriptState;
end;

procedure TSimbaForm.SetScriptState(const State: TScriptButtonState);
begin
  if (State = FScriptState) then
    Exit;

  FScriptState := State;


    case FScriptState of
      ss_Running : begin {Text := 'Running';} RunButton.Enabled := False; PauseButton.Enabled:= True; CompileButton.Enabled := True;
                         StopButton.ImageIndex := 16; StopButton.Enabled := True;
                         TrayPlay.Checked := True; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := True;
                         TrayStop.Enabled := True; TrayStop.Checked := False;
                   end;
      ss_Paused  : begin {Text := 'Paused';} RunButton.Enabled:= True; PauseButton.Enabled := False; CompileButton.Enabled := True;
                         StopButton.ImageIndex := 16; StopButton.Enabled := True;
                         TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := True; TrayPause.Enabled := True;
                         TrayStop.Enabled := True; TrayStop.Checked:= False;
                   end;
      ss_Stopping: begin {Text := 'Stopping';} RunButton.Enabled := False; PauseButton.Enabled := False; StopButton.Enabled := True; CompileButton.Enabled := True;
                         StopButton.ImageIndex := 10;
                         TrayPlay.Checked := False; TrayPlay.Enabled := False; TrayPause.Checked := False; TrayPause.Enabled := False;
                         TrayStop.Enabled := True; TrayStop.Checked := True;

                   end;
      ss_None    : begin {Text := 'Done';} RunButton.Enabled := True; PauseButton.Enabled := False; StopButton.Enabled := False; CompileButton.Enabled := True;
                         StopButton.ImageIndex := 16;
                         TrayPlay.Checked := False; TrayPlay.Enabled := True; TrayPause.Checked := False; TrayPause.Enabled := False;
                         TrayStop.Enabled := False; TrayStop.Checked := False;
                   end;
      ss_Locked: begin
                   {Text := 'Loading';}
                   RunButton.Enabled := False; PauseButton.Enabled := False; StopButton.Enabled := False; CompileButton.Enabled := False;
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
