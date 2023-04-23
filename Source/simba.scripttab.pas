{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.scripttab;

{$i simba.inc}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, Dialogs,
  SynEdit, SynEditTypes,
  simba.mufasatypes, simba.editor, simba.scriptinstance,
  simba.debuggerform, simba.functionlistform, simba.outputform, simba.component_tabcontrol;

type
  TSimbaScriptTab = class(TSimbaTab)
  protected
    FEditor: TSimbaEditor;
    FSavedText: String;
    FScriptFileName: String;
    FScriptTitle: String;
    FScriptInstance: TSimbaScriptInstance;
    FFunctionList: TSimbaFunctionList;
    FDebuggingForm: TSimbaDebuggerForm;
    FOutputBox: TSimbaOutputBox;

    procedure TabShow; override;
    procedure TabHide; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ScriptStateChanged(Sender: TObject);

    procedure DoEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoEditorStatusChanges(Sender: TObject; Changes: TSynStatusChanges);

    function GetScript: String;
    function GetScriptChanged: Boolean;
  public
    property DebuggingForm: TSimbaDebuggerForm read FDebuggingForm;
    property FunctionList: TSimbaFunctionList read FFunctionList;
    property OutputBox: TSimbaOutputBox read FOutputBox;

    property ScriptInstance: TSimbaScriptInstance read FScriptInstance;
    property ScriptTitle: String read FScriptTitle;
    property ScriptFileName: String read FScriptFileName;

    property ScriptChanged: Boolean read GetScriptChanged;
    property Script: String read GetScript;
    property Editor: TSimbaEditor read FEditor;

    function SaveAsDialog: String;

    function Save(FileName: String): Boolean;
    function Load(FileName: String): Boolean;

    procedure Undo;
    procedure Redo;

    function CanClose: Boolean;

    function ScriptState: ESimbaScriptState;
    function ScriptTimeRunning: UInt64;

    procedure Run(Target: THandle);
    procedure RunWithDebugging(Target: THandle);
    procedure Compile;
    procedure Pause;
    procedure Stop;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  LazFileUtils,
  simba.settings, simba.ide_events,
  simba.main, simba.files, simba.functionlist_updater, simba.ide_showdeclaration, simba.threading;

function TSimbaScriptTab.GetScript: String;
begin
  Result := FEditor.Text;
end;

function TSimbaScriptTab.GetScriptChanged: Boolean;
begin
  Result := FEditor.Text <> FSavedText;
end;

procedure TSimbaScriptTab.TabShow;
begin
  FOutputBox.Tab.Show();

  FFunctionList.Show();
  if Editor.CanSetFocus() then
    Editor.SetFocus();
end;

procedure TSimbaScriptTab.TabHide;
begin
  FFunctionList.Hide();
end;

procedure TSimbaScriptTab.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FScriptInstance) then
  begin
    DebugLn('TSimbaScriptTab.Notification :: FScriptInstance = nil');

    FScriptInstance := nil;
  end;

  inherited Notification(AComponent, Operation);
end;

procedure TSimbaScriptTab.ScriptStateChanged(Sender: TObject);
begin
  case TSimbaScriptInstance(Sender).State of
    ESimbaScriptState.STATE_RUNNING: FOutputBox.Tab.ImageIndex := IMAGE_PLAY;
    ESimbaScriptState.STATE_PAUSED:  FOutputBox.Tab.ImageIndex := IMAGE_PAUSE;
    else
      FOutputBox.Tab.ImageIndex := IMAGE_STOP;
  end;
end;

procedure TSimbaScriptTab.DoEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure Execute(Params: TVariantArray);
  begin
    FindAndShowDeclaration(Params[0], Params[1], Params[2], Params[3]);
  end;

begin
  if (Sender is TSynEdit) then
  begin
    X := TSynEdit(Sender).PixelsToRowColumn(ScreenToControl(Mouse.CursorPos), []).X;
    Y := TSynEdit(Sender).PixelsToRowColumn(ScreenToControl(Mouse.CursorPos), []).Y;
  end;

  QueueOnMainThread(@Execute, [Script, ScriptFileName, Editor.GetCaretPos(True), Editor.GetExpressionEx(X, Y)]);
end;

procedure TSimbaScriptTab.DoEditorStatusChanges(Sender: TObject; Changes: TSynStatusChanges);
begin
  SimbaIDEEvents.CallOnEditorCaretMoved(Self);
  SimbaIDEEvents.CallOnEditorModified(Self);
end;

function TSimbaScriptTab.SaveAsDialog: String;
begin
  Result := '';

  with TSaveDialog.Create(Self) do
  try
    Filter := 'Simba Files|*.simba;*.pas;*.inc;|Any Files|*.*';
    Options := Options + [ofOverwritePrompt];
    InitialDir := ExtractFileDir(FScriptFileName);
    if (InitialDir = '') then
      InitialDir := GetScriptPath();

    if Execute() then
    begin
      if (ExtractFileExt(FileName) = '') then
        FileName := ChangeFileExt(FileName, '.simba');

      Result := FileName;
    end;
  finally
    Free();
  end;
end;

function TSimbaScriptTab.Save(FileName: String): Boolean;
begin
  Result := False;

  if (FileName = '') then
  begin
    FileName := SaveAsDialog();
    if (FileName = '') then
      Exit;
  end;

  try
    FEditor.Lines.SaveToFile(FileName);

    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Unable to save script: ' + E.Message, mtError, [mbOK], 0);

      Exit;
    end;
  end;

  FScriptTitle := ExtractFileNameOnly(FileName);
  FScriptFileName := FileName;
  FSavedText := FEditor.Text;

  FOutputBox.Tab.Caption := FScriptTitle;

  Caption := FScriptTitle;
end;

function TSimbaScriptTab.Load(FileName: String): Boolean;
begin
  Result := False;
  if (FileName = '') then
    Exit;

  try
    FEditor.Lines.LoadFromFile(FileName);

    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Unable to load script: ' + E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  FScriptTitle := ExtractFileNameOnly(FileName);
  FScriptFileName := FileName;
  FSavedText := FEditor.Text;

  FOutputBox.Tab.Caption := FScriptTitle;

  Caption := FScriptTitle;
  if Result then
    SimbaIDEEvents.CallOnEditorLoadedMethods(Self);
end;

procedure TSimbaScriptTab.Undo;
begin
  FEditor.Undo();
end;

procedure TSimbaScriptTab.Redo;
begin
  FEditor.Redo();
end;

function TSimbaScriptTab.CanClose: Boolean;
begin
  Result := True;

  if (FScriptInstance <> nil) then
  begin
    Show();

    // Don't close if user doesn't want to focefully stop the script
    if (MessageDlg('Script is still running. Forcefully stop this script?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    begin
      Result := False;
      Exit;
    end;

    if (FScriptInstance <> nil) then
      FScriptInstance.Kill();
  end;

  if ScriptChanged then
  begin
    Show();

    // Ask to save the script yes/no = can close. Else cannot close.
    case MessageDlg('Script has been modified. Save this script?', mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
      mrYes:
        Result := Save(FScriptFileName);
      mrNo:
        Result := True;
      else
        Result := False;
    end;
  end;
end;

function TSimbaScriptTab.ScriptState: ESimbaScriptState;
begin
  Result := ESimbaScriptState.STATE_NONE;
  if (FScriptInstance <> nil) then
    Result := FScriptInstance.State;
end;

function TSimbaScriptTab.ScriptTimeRunning: UInt64;
begin
  Result := 0;
  if (FScriptInstance <> nil) then
    Result := FScriptInstance.TimeRunning;
end;

procedure TSimbaScriptTab.Run(Target: THandle);
begin
  DebugLn('TSimbaScriptTab.Run :: ' + ScriptTitle + ' ' + ScriptFileName);

  if (FScriptInstance <> nil) then
    FScriptInstance.Resume()
  else
  begin
    if (FScriptFileName <> '') then
      Save(FScriptFileName);

    FScriptInstance := TSimbaScriptInstance.Create(Self, FOutputBox);
    FScriptInstance.RegisterStateChangeHandler(@ScriptStateChanged);
    FScriptInstance.Target := Target;
    if (FScriptFileName = '') then
      FScriptInstance.ScriptFile := CreateTempFile(Script, ScriptTitle)
    else
      FScriptInstance.ScriptFile := ScriptFileName;

    FScriptInstance.Run();
  end;
end;

procedure TSimbaScriptTab.RunWithDebugging(Target: THandle);
begin
  DebugLn('TSimbaScriptTab.RunWithDebugging :: ' + ScriptTitle + ' ' + ScriptFileName);

  if (FScriptInstance <> nil) then // Already running
    Exit;

  if (FDebuggingForm = nil) then
    FDebuggingForm := TSimbaDebuggerForm.Create(Self);
  FDebuggingForm.Caption := 'Debugger - ' + FScriptTitle;
  FDebuggingForm.Clear();
  FDebuggingForm.ShowOnTop();

  if (FScriptFileName <> '') then
    Save(FScriptFileName);

  FScriptInstance := TSimbaScriptInstance.Create(Self, FOutputBox);
  FScriptInstance.Target := Target;

  if (FScriptFileName = '') then
    FScriptInstance.ScriptFile := CreateTempFile(Script, ScriptTitle)
  else
    FScriptInstance.ScriptFile := ScriptFileName;

  FScriptInstance.Run(FDebuggingForm);
end;

procedure TSimbaScriptTab.Compile;
begin
  DebugLn('TSimbaScriptTab.Compile :: ' + ScriptTitle + ' ' + ScriptFileName);

  if (FScriptInstance = nil) then
  begin
    if (FScriptFileName <> '') then
      Save(FScriptFileName);

    FScriptInstance := TSimbaScriptInstance.Create(Self, FOutputBox);

    if (FScriptFileName = '') then
      FScriptInstance.ScriptFile := CreateTempFile(Script, ScriptTitle)
    else
      FScriptInstance.ScriptFile := ScriptFileName;

    FScriptInstance.Compile();
  end;
end;

procedure TSimbaScriptTab.Pause;
begin
  DebugLn('TSimbaScriptTab.Pause :: ' + ScriptTitle + ' ' + ScriptFileName);

  if (FScriptInstance <> nil) then
    FScriptInstance.Pause();
end;

procedure TSimbaScriptTab.Stop;
begin
  DebugLn('TSimbaScriptTab.Stop :: ' + ScriptTitle + ' ' + ScriptFileName);

  if (FScriptInstance <> nil) then
    FScriptInstance.Stop();
end;

constructor TSimbaScriptTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScriptTitle := 'Untitled';
  FScriptFileName := '';

  FEditor := TSimbaEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.BorderStyle := bsNone;
  FEditor.TabStop := False;
  FEditor.Text := SimbaSettings.Editor.DefaultScript.Value;
  FEditor.MarkTextAsSaved();
  FEditor.RegisterStatusChangedHandler(@DoEditorStatusChanges, [scCaretX, scCaretY, scModified]);
  FEditor.OnClickLink := @DoEditorLinkClick;

  FFunctionList := TSimbaFunctionList.Create();
  FFunctionList.Parent := SimbaFunctionListForm;
  FFunctionList.Align := alClient;

  FOutputBox := SimbaOutputForm.AddScriptOutput('Untitled');
  FOutputBox.Tab.ImageIndex := IMAGE_STOP;

  FSavedText := FEditor.Text;
end;

destructor TSimbaScriptTab.Destroy;
begin
  if Assigned(SimbaOutputForm) then
    SimbaOutputForm.RemoveTab(FOutputBox);

  FFunctionList.DecRef();
  FFunctionList := nil;

  inherited Destroy();
end;

end.

