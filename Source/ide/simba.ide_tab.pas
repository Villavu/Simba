{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_tab;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Process,
  simba.base,
  simba.ide_editor,
  simba.ide_events,
  simba.component_tabcontrol,
  simba.ide_output_components;

type
  TSimbaScriptTab = class;

  // main class which will run the script of a given tab.
  // Manages output etc.
  TSimbaScriptTabRunner = class(TComponent)
  protected
    FTab: TSimbaScriptTab;

    FErrorSet: Boolean;
    FError: record
      Message, FileName: String;
      Line, Col: Integer;
    end;

    FProcess: TProcess;

    FStartTime: UInt64;

    FOutputThread: TThread;
    FOutputBox: TOutputListComponentReal;

    FState: ESimbaScriptState;

    FScriptFile: String;
    FScript: String;
    FScriptTitle: String;

    procedure Start(Args: TStringArray);

    procedure ShowError;
    procedure ShowOutputBox;

    procedure DoOutputThread;
    procedure DoOutputThreadTerminated(Sender: TObject);

    function GetTimeRunning: UInt64;

    procedure SetState(Value: ESimbaScriptState);
  public
    property Script: String read FScript;
    property ScriptTitle: String read FScriptTitle;

    property Tab: TSimbaScriptTab read FTab;

    property Process: TProcess read FProcess;
    property State: ESimbaScriptState read FState write SetState;
    property TimeRunning: UInt64 read GetTimeRunning;

    // Start
    procedure Run(Args: TStringArray);
    procedure Compile(Args: TStringArray);

    // Change the running state
    procedure Resume;
    procedure Pause;
    procedure Stop;
    procedure Kill;

    procedure SetError(Message, FileName: String; Line, Col: Integer);

    constructor Create(ATab: TSimbaScriptTab); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaScriptTab = class(TSimbaTab)
  protected
    FEditor: TSimbaEditor;

    // The text we last saved or loaded, to detect changes
    FUnmodifiedText: record
      LineCount: SizeInt; // store line count for faster initial check
      Text: String;
    end;
    FPostedCanSave: Boolean;
    FPostedCantSave: Boolean;

    FScriptFileName: String;
    FScriptTitle: String;
    FFileAge: Int32; // disk age when we loaded the file

    FScriptRunner: TSimbaScriptTabRunner;

    procedure UpdateModifiedText();
    function CheckIsModified: Boolean;

    procedure LoadDefaultScript;
    procedure FindDeclarationAtCaretASync(Data: PtrInt);

    // Keep output tab in sync
    procedure TextChanged; override;
    // Reset PostedCanSave & PostedCanSave
    procedure VisibleChanged; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function DoEditorGetFileName(Sender: TObject): String;
    procedure DoEditorModified(Sender: TObject);
    procedure DoEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoEditorCaretMoved(Sender: TObject);

    function GetScript: String;
    procedure SetScript(AValue: String);
  public
    property ScriptTitle: String read FScriptTitle;
    property ScriptFileName: String read FScriptFileName;
    property Script: String read GetScript write SetScript;
    property Editor: TSimbaEditor read FEditor;

    // Has the file on disk changed since we loaded?
    function IsOutdatedOnDisk: Boolean;
    // Query a reload if changed on disk
    function QueryReloadOutdated: Boolean;

    // Save the script to a file.
    // FileName='' will open a save dialog to query for filename
    function Save(FileName: String): Boolean;

    // Load editor lines from file
    // Will set editor to read only if file is such.
    function Load(FileName: String): Boolean;

    procedure FindDeclarationAtCaret;

    function CanSave: Boolean;
    function CanClose: Boolean;

    function RunningState: ESimbaScriptState;
    function RunningTime: UInt64;

    procedure Run;
    procedure Compile;
    procedure Pause;
    procedure Stop;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Forms, Dialogs,
  simba.fs,
  simba.settings,
  simba.form_scripttabs,
  simba.env,
  simba.ide_showdeclaration,
  simba.threading,
  simba.ide_scriptcommunication,
  simba.ide_editor_popupmenu,
  simba.ide_vars,
  simba.dialog,
  simba.vartype_string,
  simba.vartype_windowhandle,
  simba.ide_controller;

procedure TSimbaScriptTabRunner.DoOutputThread;
var
  ReadBuffer: String;

  procedure EmptyProcessOutput;
  var
    Count: Integer;
  begin
    while (FProcess.Output.NumBytesAvailable > 0) do
    begin
      Count := FProcess.Output.Read(ReadBuffer[1], Length(ReadBuffer));
      if (Count > 0) then
        FOutputBox.Add(@ReadBuffer[1], Count);
    end;
  end;

begin
  try
    SetLength(ReadBuffer, 8192);
    while FProcess.Running do
    begin
      EmptyProcessOutput();
      Sleep(500);
    end;
    EmptyProcessOutput();
  except
    on E: Exception do
      DebugLn('Script process[%d] output thread crashed: %s', [FProcess.ProcessID, E.Message]);
  end;
end;

procedure TSimbaScriptTabRunner.DoOutputThreadTerminated(Sender: TObject);
begin
  CheckMainThread('TSimbaScriptTabRunner.DoOutputThreadTerminated');
  if FProcess.Running then
    FProcess.Terminate(0);

  if FErrorSet then
  begin
    ShowError();
    ShowOutputBox();
  end;

  Self.Free();
end;

function TSimbaScriptTabRunner.GetTimeRunning: UInt64;
begin
  if (FStartTime = 0) then
    Result := 0
  else
    Result := GetTickCount64() - FStartTime;
end;

procedure TSimbaScriptTabRunner.SetState(Value: ESimbaScriptState);
begin
  FState := Value;

  SimbaEvents.Post(ESimbaEvent.TAB_SCRIPTSTATE_CHANGE, FTab);
end;

procedure TSimbaScriptTabRunner.Start(Args: TStringArray);
begin
  FScriptFile := FTab.ScriptFileName;
  FScriptTitle := FTab.ScriptTitle;
  FScript := FTab.Script;

  FStartTime := GetTickCount64();

  FProcess.Parameters.Add('--simbacommunication=%s', [TSimbaScriptInstanceCommunication.Create(Self).ClientID]);
  if SimbaSettings.Compiler.ShowHints.Value then
    FProcess.Parameters.Add('--hints');

  FProcess.Parameters.AddStrings(Args);
  if (FScriptFile <> '') then
    FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FOutputThread := RunInThread(@DoOutputThread);
  FOutputThread.OnTerminate := @DoOutputThreadTerminated;

  State := ESimbaScriptState.RUNNING;
end;

procedure TSimbaScriptTabRunner.ShowError;
begin
  // FError is in the script tab that ran the script
  if (FTab.ScriptFileName = FError.FileName) or ((FTab.ScriptFileName = '') and (FTab.ScriptTitle = FError.FileName)) then
  begin
    FTab.Show();
    FTab.Editor.FocusLine(FError.Line, FError.Col, $0000A5);
  end else
  // else, open the file and display.
  if SimbaScriptTabsForm.Open(FError.FileName) then
    SimbaScriptTabsForm.ActiveTab.Editor.FocusLine(FError.Line, FError.Col, $0000A5);

  FTab.Editor.FocusLine(FError.Line, FError.Col, $0000A5);
end;

procedure TSimbaScriptTabRunner.ShowOutputBox;
begin
  //FTab.OutputBox.MakeVisible();
end;

procedure TSimbaScriptTabRunner.Run(Args: TStringArray);
begin
  Start(Args + ['--run']);
end;

procedure TSimbaScriptTabRunner.Compile(Args: TStringArray);
begin
  Start(Args + ['--compile']);
end;

procedure TSimbaScriptTabRunner.Resume;
begin
  FState := ESimbaScriptState.RUNNING;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

procedure TSimbaScriptTabRunner.Pause;
begin
  FState := ESimbaScriptState.PAUSED;
  FProcess.Input.Write(FState, SizeOf(Int32));
end;

procedure TSimbaScriptTabRunner.Stop;
begin
  if (FState = ESimbaScriptState.STOP) then
    FProcess.Terminate(1001)
  else
  begin
    FState := ESimbaScriptState.STOP;
    FProcess.Input.Write(FState, SizeOf(Int32));
  end;
end;

constructor TSimbaScriptTabRunner.Create(ATab: TSimbaScriptTab);
begin
  inherited Create(ATab);

  FTab := ATab;
  FOutputBox := SimbaController.FindOutputListForTab(FTab.UID);
  FState := ESimbaScriptState.RUNNING;

  FProcess := TProcess.Create(Self);
  FProcess.PipeBufferSize := 16 * 1024;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.Options := FProcess.Options + [poUsePipes, poStderrToOutPut, poDetached];
  FProcess.Executable := Application.ExeName;
end;

procedure TSimbaScriptTabRunner.Kill;
begin
  FProcess.Terminate(0);
end;

procedure TSimbaScriptTabRunner.SetError(Message, FileName: String; Line, Col: Integer);
begin
  FErrorSet := True;

  FError.Message := Message;
  FError.FileName := FileName;
  FError.Line := Line;
  FError.Col := Col;
end;

destructor TSimbaScriptTabRunner.Destroy;
begin
  State := ESimbaScriptState.NONE;

  inherited Destroy();
end;

function TSimbaScriptTab.GetScript: String;
begin
  Result := FEditor.Text;
end;

procedure TSimbaScriptTab.SetScript(AValue: String);
begin
  FEditor.BeginUndoBlock();
  FEditor.ClearAll();
  FEditor.InsertTextAtCaret(Script);
  FEditor.EndUndoBlock();
end;

function TSimbaScriptTab.IsOutdatedOnDisk: Boolean;
begin
  Result := (FScriptFileName <> '') and FileExists(FScriptFileName) and
            (FFileAge > 0) and (FileAge(FScriptFileName) > FFileAge);
end;

function TSimbaScriptTab.QueryReloadOutdated: Boolean;
const
  Message = 'File "%s" has changed on disk' + LINE_SEP +
            'Do you want to reload it?';
begin
  Result := False;

  if IsOutdatedOnDisk() then
  begin
    if (ShowQuestionDialog('Simba', Message, [FScriptFileName]) = ESimbaDialogButton.YES) then
      Result := Load(FScriptFileName)
    else
      FFileAge := FileAge(FScriptFileName);
  end;
end;

procedure TSimbaScriptTab.UpdateModifiedText();
begin
  FPostedCanSave := False;
  FPostedCantSave := False;

  FUnmodifiedText.Text := FEditor.TextView.Text;
  FUnmodifiedText.LineCount := FEditor.TextView.Count;
end;

function TSimbaScriptTab.CheckIsModified: Boolean;
begin
  Result := (FUnmodifiedText.LineCount <> FEditor.TextView.Count) or
            (FUnmodifiedText.Text <> FEditor.TextView.Text);
end;

procedure TSimbaScriptTab.LoadDefaultScript;
begin
  case SimbaSettings.Editor.DefaultScriptType.Value of
    0: FEditor.Text := TSimbaFile.FileRead(SimbaSettings.Editor.DefaultScriptFile.Value);
    1: FEditor.Text := SimbaSettings.Editor.DefaultScript.Value;
  end;

  FEditor.MarkTextAsSaved();
end;

procedure TSimbaScriptTab.TextChanged;
begin
  inherited TextChanged();

  SimbaEvents.Post(ESimbaEvent.TAB_CAPTION, Self);
end;

procedure TSimbaScriptTab.VisibleChanged;
begin
  inherited VisibleChanged;

  if Visible then
  begin
    FPostedCanSave := False;
    FPostedCantSave := False;
  end;
end;

procedure TSimbaScriptTab.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FScriptRunner) then
    FScriptRunner := nil;

  inherited Notification(AComponent, Operation);
end;

function TSimbaScriptTab.DoEditorGetFileName(Sender: TObject): String;
begin
  Result := FScriptFileName;
end;

procedure TSimbaScriptTab.FindDeclarationAtCaretASync(Data: PtrInt);
begin
  FindAndShowDeclaration(Script, ScriptFileName, Editor.GetCaretPos(True), Editor.GetExpressionEx(FEditor.CaretX, FEditor.CaretY));
end;

procedure TSimbaScriptTab.FindDeclarationAtCaret;
begin
  Application.QueueAsyncCall(@FindDeclarationAtCaretASync, 0); // queue the event to let synedit finishing painting
end;

procedure TSimbaScriptTab.DoEditorModified(Sender: TObject);
begin
  if CheckIsModified() then
  begin
    if not FPostedCanSave then
    begin
      Caption := '*' + FScriptTitle;

      SimbaEvents.Post(ESimbaEvent.TAB_CAN_SAVE, Self);
      FPostedCanSave := True;
      FPostedCantSave := False;
    end;
  end else
  begin
    if not FPostedCantSave then
    begin
      Caption := FScriptTitle;

      SimbaEvents.Post(ESimbaEvent.TAB_CANNOT_SAVE, Self);
      FPostedCantSave := True;
      FPostedCanSave := False;
    end;
  end;

  SimbaEvents.Post(ESimbaEvent.TAB_MODIFIED, Self);
end;

procedure TSimbaScriptTab.DoEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FindDeclarationAtCaret();
end;

procedure TSimbaScriptTab.DoEditorCaretMoved(Sender: TObject);
begin
  SimbaEvents.Post(ESimbaEvent.TAB_CARETMOVED, Self);
end;

function TSimbaScriptTab.Save(FileName: String): Boolean;
begin
  Result := False;

  if (FileName = '') then
  begin
    FileName := ShowSaveDialog(
      IfThen(FScriptFileName = '', SimbaEnv.ScriptsPath, TSimbaPath.PathExtractDir(FScriptFileName)),
      '.simba',
      'Simba Files|*.simba;*.pas;*.inc;|Any Files|*.*'
    );

    if (FileName = '') then // dialog was cancelled
      Exit;
  end;

  try
    FEditor.Lines.SaveToFile(FileName);
    FEditor.ReadOnly := False;

    Result := True;
  except
    on E: Exception do
    begin
      MessageDlg('Unable to save script: ' + E.Message, mtError, [mbOK], 0);

      Exit;
    end;
  end;

  UpdateModifiedText();

  FScriptFileName := FileName;
  FScriptTitle := TSimbaPath.PathExtractName(FScriptFileName);
  if FScriptTitle.EndsWith('.simba') then
    FScriptTitle := FScriptTitle.Before('.simba');
  FFileAge := FileAge(FScriptFileName);

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

  FEditor.ReadOnly := FileIsReadOnly(FileName);

  UpdateModifiedText();

  FFileAge := FileAge(FileName);
  FScriptFileName := FileName;
  FScriptTitle := TSimbaPath.PathExtractName(FScriptFileName);
  if FScriptTitle.EndsWith('.simba') then
    FScriptTitle := FScriptTitle.Before('.simba');

  Caption := FScriptTitle;
  if Result then
    SimbaEvents.Post(ESimbaEvent.TAB_LOADED, Self);
end;

function TSimbaScriptTab.CanSave: Boolean;
begin
  Result := CheckIsModified();
end;

function TSimbaScriptTab.CanClose: Boolean;
begin
  Result := True;

  if (FScriptRunner <> nil) then
  begin
    Show();

    // Don't close if user doesn't want to forcefully stop the script
    if (MessageDlg('Script is still running. Forcefully stop this script?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
    begin
      Result := False;
      Exit;
    end;

    if (FScriptRunner <> nil) then
      FScriptRunner.Kill();
  end;

  if CanSave() then
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

function TSimbaScriptTab.RunningState: ESimbaScriptState;
begin
  Result := ESimbaScriptState.NONE;
  if (FScriptRunner <> nil) then
    Result := FScriptRunner.State;
end;

function TSimbaScriptTab.RunningTime: UInt64;
begin
  if (FScriptRunner <> nil) then
    Result := FScriptRunner.TimeRunning
  else
    Result := 0;
end;

procedure TSimbaScriptTab.Run;
begin
  if QueryReloadOutdated() then
    Exit;

  if (FScriptRunner <> nil) then
    FScriptRunner.Resume()
  else
  begin
    if (not FEditor.ReadOnly) and (FScriptFileName <> '') then
      Save(FScriptFileName);

    FScriptRunner := TSimbaScriptTabRunner.Create(Self);
    if (SimbaIDEVars.WindowSelection.IsValid()) then
      FScriptRunner.Run(['--target=' + IntToStr(SimbaIDEVars.WindowSelection)])
    else
      FScriptRunner.Run([]);
  end;
end;

procedure TSimbaScriptTab.Compile;
begin
  if QueryReloadOutdated() then
    Exit;

  if (FScriptRunner = nil) then
  begin
    if (not FEditor.ReadOnly) and (FScriptFileName <> '') then
      Save(FScriptFileName);

    FScriptRunner := TSimbaScriptTabRunner.Create(Self);
    FScriptRunner.Compile([]);
  end;
end;

procedure TSimbaScriptTab.Pause;
begin
  if (FScriptRunner <> nil) then
    FScriptRunner.Pause();
end;

procedure TSimbaScriptTab.Stop;
begin
  if (FScriptRunner <> nil) then
    FScriptRunner.Stop();
end;

constructor TSimbaScriptTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScriptTitle := 'Untitled';
  FScriptFileName := '';

  FEditor := TSimbaEditor.Create(Self, [seoColors, seoKeybindings]);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.OnClickLink := @DoEditorLinkClick;
  FEditor.OnModified := @DoEditorModified;
  FEditor.OnGetFileName := @DoEditorGetFileName;
  FEditor.RegisterCaretMoveHandler(@DoEditorCaretMoved);
  FEditor.PopupMenu := TSimbaTabPopupMenu.Create(Self);

  LoadDefaultScript();
  UpdateModifiedText();

  SimbaEvents.Post(ESimbaEvent.TAB_ADD, Self);
end;

destructor TSimbaScriptTab.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  SimbaEvents.Post(ESimbaEvent.TAB_CLOSED, Self);

  inherited Destroy();
end;

end.
