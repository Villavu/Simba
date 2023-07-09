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
  simba.outputform, simba.component_tabcontrol;

type
  TSimbaScriptTab = class(TSimbaTab)
  protected
    FUID: Integer;
    FEditor: TSimbaEditor;
    FSavedText: String;
    FScriptFileName: String;
    FScriptTitle: String;
    FScriptInstance: TSimbaScriptInstance;
    FOutputBox: TSimbaOutputBox;

    // Keep output tab in sync
    procedure TextChanged; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoEditorModified(Sender: TObject);
    procedure DoEditorLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoEditorStatusChanges(Sender: TObject; Changes: TSynStatusChanges);

    function GetScript: String;
    function GetScriptChanged: Boolean;
  public
    property UID: Integer read FUID;
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

    function IsActiveTab: Boolean;
    function CanClose: Boolean;

    function ScriptState: ESimbaScriptState;
    function ScriptTimeRunning: UInt64;

    procedure Run(Target: THandle);
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
  simba.main, simba.env, simba.ide_showdeclaration, simba.threading;

var
  __UID: Integer = 0;

function TSimbaScriptTab.GetScript: String;
begin
  Result := FEditor.Text;
end;

function TSimbaScriptTab.GetScriptChanged: Boolean;
begin
  Result := FEditor.Text <> FSavedText;
end;

procedure TSimbaScriptTab.TextChanged;
begin
  inherited TextChanged();

  if Assigned(FOutputBox) then
    FOutputBox.Tab.Caption := Caption;
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

procedure TSimbaScriptTab.DoEditorModified(Sender: TObject);
begin
  SimbaIDEEvents.CallOnEditorModified(Self);
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
    FEditor.FileName := FileName;

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

  Caption := FScriptTitle;
end;

function TSimbaScriptTab.Load(FileName: String): Boolean;
begin
  Result := False;
  if (FileName = '') then
    Exit;

  try
    FEditor.Lines.LoadFromFile(FileName);
    FEditor.FileName := FileName;

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

function TSimbaScriptTab.IsActiveTab: Boolean;
begin
  Result := TabControl.ActiveTab = Self;
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
    FScriptInstance.Target := Target;
    if (FScriptFileName = '') then
      FScriptInstance.ScriptFile := CreateTempFile(Script, ScriptTitle)
    else
      FScriptInstance.ScriptFile := ScriptFileName;

    FScriptInstance.Run();
  end;
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

  Inc(__UID);

  FUID := __UID;
  FScriptTitle := 'Untitled';
  FScriptFileName := '';

  FEditor := TSimbaEditor.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := alClient;
  FEditor.TabStop := True;
  FEditor.Text := SimbaSettings.Editor.DefaultScript.Value;
  FEditor.MarkTextAsSaved();
  FEditor.RegisterStatusChangedHandler(@DoEditorStatusChanges, [scCaretX, scCaretY, scModified]);
  FEditor.OnClickLink := @DoEditorLinkClick;
  FEditor.OnModified := @DoEditorModified;
  FEditor.UseSimbaColors := True;

  FOutputBox := SimbaOutputForm.AddScriptOutput('Untitled');
  FOutputBox.Tab.ImageIndex := IMAGE_STOP;

  FSavedText := FEditor.Text;
end;

destructor TSimbaScriptTab.Destroy;
begin
  SimbaIDEEvents.CallOnScriptTabClose(Self);

  if Assigned(SimbaOutputForm) then
    SimbaOutputForm.RemoveTab(FOutputBox);

  inherited Destroy();
end;

end.
