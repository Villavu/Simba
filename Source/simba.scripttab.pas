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
  simba.mufasatypes, simba.editor, simba.scriptinstance,
  simba.debuggerform, simba.functionlistform, simba.outputform;

type
  TSimbaScriptTab = class(TTabSheet)
  protected
    FEditor: TSimbaEditor;
    FSavedText: String;
    FScriptFileName: String;
    FScriptTitle: String;
    FScriptInstance: TSimbaScriptInstance;
    FFunctionList: TSimbaFunctionList;
    FDebuggingForm: TSimbaDebuggerForm;
    FOutputBox: TSimbaOutputBox;
    FLinkExpression: String;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoHide; override;
    procedure DoShow; override;
    procedure DoShowDeclaration(Data: PtrInt);

    procedure ScriptStateChanged(Sender: TObject);

    procedure HandleEditorChange(Sender: TObject);
    procedure HandleEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure UpdateSavedText;

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
    function Load(FileName: String): Boolean; overload;
    function Load(FileName: String; AScriptFileName, AScriptTitle: String): Boolean; overload;

    procedure Undo;
    procedure Redo;

    function CanClose: Boolean;

    procedure Reset;

    function ScriptState: ESimbaScriptState;
    function ScriptTimeRunning: UInt64;

    procedure Run(Target: THandle);
    procedure RunWithDebugging(Target: THandle);
    procedure Compile;
    procedure Pause;
    procedure Stop;

    constructor Create(APageControl: TPageControl); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Forms, SynEdit, LazFileUtils,
  simba.settings, simba.ide_codetools_parser, simba.ide_codetools_insight,
  simba.main, simba.files, simba.functionlist_updater, simba.ide_showdeclaration;

function TSimbaScriptTab.GetScript: String;
begin
  Result := FEditor.Text;
end;

function TSimbaScriptTab.GetScriptChanged: Boolean;
begin
  Result := FEditor.Text <> FSavedText;
end;

procedure TSimbaScriptTab.UpdateSavedText;
begin
  FOutputBox.Tab.Caption := FScriptTitle;
  FSavedText := FEditor.Text;

  FEditor.MarkTextAsSaved();
  if (FEditor.OnChange <> nil) then
    FEditor.OnChange(FEditor);
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

procedure TSimbaScriptTab.DoHide;
begin
  inherited DoHide();

  if (FFunctionList <> nil) then
    FFunctionList.Hide();
end;

procedure TSimbaScriptTab.DoShow;
begin
  inherited DoShow();

  if (FOutputBox <> nil) then
    FOutputBox.Show();
  if (FFunctionList <> nil) then
    FFunctionList.Show();
  if (FEditor <> nil) and FEditor.CanSetFocus() then
    FEditor.SetFocus();
end;

procedure TSimbaScriptTab.DoShowDeclaration(Data: PtrInt);
var
  Decl: TDeclaration;
  Decls: TDeclarationArray;
  Codeinsight: TCodeinsight;
begin
  try
    Codeinsight := TCodeinsight.Create();

    try
      Codeinsight.SetScript(Script, ScriptFileName, Editor.SelStart, Editor.SelStart);
      Codeinsight.Run();

      Decl := Codeinsight.ParseExpression(FLinkExpression, []);
      if (Decl <> nil) then
      begin
        if (Decl.ClassType = TDeclaration_Method) then
        begin
          Decls := Codeinsight.GetOverloads(Decl);
          if (Length(Decls) > 1) then
          begin
            ShowDeclarationDialog(Decls);
            Exit;
          end;
        end;

        ShowDeclaration(Decl);
      end;
    finally
      Codeinsight.Free();
    end;
  except
    on E: Exception do
      DebugLn('TSimbaScriptTab.DoShowDeclaration: ' + E.ToString());
  end;
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

procedure TSimbaScriptTab.HandleEditorChange(Sender: TObject);
begin
  if ScriptChanged then
  begin
    Caption := '*' + FScriptTitle;

    SimbaForm.MenuItemSave.Enabled := True;
    SimbaForm.ToolbarButtonSave.Enabled := True;
  end else
  begin
    Caption := FScriptTitle;

    SimbaForm.MenuItemSave.Enabled := False;
    SimbaForm.ToolbarButtonSave.Enabled := False;
  end;
end;

procedure TSimbaScriptTab.HandleEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TSynEdit) then
  begin
    X := TSynEdit(Sender).PixelsToRowColumn(ScreenToControl(Mouse.CursorPos), []).X;
    Y := TSynEdit(Sender).PixelsToRowColumn(ScreenToControl(Mouse.CursorPos), []).Y;
  end;

  FLinkExpression := Editor.GetExpressionEx(X, Y);

  Application.QueueAsyncCall(@DoShowDeclaration, 0);
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

  UpdateSavedText();
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

  UpdateSavedText();
end;

function TSimbaScriptTab.Load(FileName: String; AScriptFileName, AScriptTitle: String): Boolean;
begin
  Result := False;

  try
    FSavedText := '';

    FScriptTitle := AScriptTitle;
    FScriptFileName := AScriptFileName;

    FEditor.Lines.LoadFromFile(FileName);
    if (FEditor.OnChange <> nil) then
      FEditor.OnChange(FEditor);

    Result := True;
  except
    on E: Exception do
      MessageDlg('Unable to load script: ' + E.Message, mtError, [mbOK], 0);
  end;
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
    if MessageDlg('Script is still running. Forcefully stop this script?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
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

procedure TSimbaScriptTab.Reset;
begin
  FScriptTitle := 'Untitled';
  FScriptFileName := '';

  FEditor.Text := SimbaSettings.Editor.DefaultScript.Value;
  FEditor.ClearUndo();

  UpdateSavedText();
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

constructor TSimbaScriptTab.Create(APageControl: TPageControl);
begin
  inherited Create(APageControl);

  Caption := FScriptTitle;
  ImageIndex := IMAGE_SIMBA;
  TabStop := False;

  FEditor := TSimbaEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.BorderStyle := bsNone;
  FEditor.TabStop := False;

  FEditor.OnChange := @HandleEditorChange;
  FEditor.OnClickLink := @HandleEditorLinkClick;

  FFunctionList := TSimbaFunctionList.Create();
  FFunctionList.Parent := SimbaFunctionListForm;
  FFunctionList.Align := alClient;

  FOutputBox := SimbaOutputForm.AddScriptOutput(Self, 'Untitled');
  FOutputBox.Tab.ImageIndex := IMAGE_STOP;

  Reset();
end;

destructor TSimbaScriptTab.Destroy;
begin
  FFunctionList.DecRef();
  FFunctionList := nil;

  inherited Destroy();
end;

end.

