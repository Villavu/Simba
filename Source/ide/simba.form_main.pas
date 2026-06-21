{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.form_main;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Menus, ImgList, AnchorDockPanel,
  simba.base,
  simba.settings,
  simba.ide_events,
  simba.image;

type
  TSimbaMainForm = class(TForm)
    DockPanel: TAnchorDockPanel;
    TrayIcon: TTrayIcon;
    TrayPopup: TPopupMenu;
    TrayPopupExit: TMenuItem;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayPopupExitClick(Sender: TObject);
  protected
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoAssociateIfFirstLaunch;
    procedure DoApplicationParameters;
    procedure DoSetupCompleted;
    function DoGetTargetImage: TSimbaImage;
  public
    procedure Setup;

    destructor Destroy; override;
  end;

var
  SimbaMainForm: TSimbaMainForm;

implementation

{$R *.lfm}

uses
  simba.initializations,
  simba.ide_scriptbackup,
  simba.ide_associate,
  simba.ide_debugimage,
  simba.ide_dtmeditor,
  simba.ide_controller,
  simba.form_functionlist,
  simba.form_scripttabs,
  simba.form_output,
  simba.aca,
  simba.env,
  simba.nativeinterface,
  simba.threading;

procedure TSimbaMainForm.DoException(Sender: TObject; E: Exception);

  procedure Dump(Addr: Pointer; List: TStringList);
  begin
    // preventing another exception, while dumping stack trace
    try
      List.Add(BackTraceStrFunc(Addr));
    except
      List.Add(SysBackTraceStr(Addr));
    end;
  end;

  procedure DumpStack(List: TStringList);
  var
    FrameCount, FrameIndex: Integer;
    Frames: PPointer;
  begin
    Dump(ExceptAddr, List);

    FrameCount := ExceptFrameCount;
    Frames := ExceptFrames;
    for FrameIndex := 0 to FrameCount - 1 do
      Dump(Frames[FrameIndex], List);
  end;

var
  Log: TStringList;
  FileName, Message: String;
begin
  try
    Log := TStringList.Create();
    Log.Add('Simba %d encountered an unhandled exception.', [SIMBA_VERSION]);
    Log.Add('Simba commit: %s', [SIMBA_COMMIT]);
    Log.Add('');
    Log.Add('Exception: %s', [E.Message]);
    Log.Add('Exception class: %s', [E.ClassName]);
    Log.Add('');

    DumpStack(Log);

    FileName := SimbaEnv.DataPath + FormatDateTime('dd-mm_hh-mm-ss', Now()) + '.crash';

    Log.SaveToFile(FileName);
    Log.Free();

    Message := '%s'                                                     + LineEnding +
               ''                                                       + LineEnding +
               'Press OK to save your scripts and close. (Recommended)' + LineEnding +
               'Press Cancel to ignore and risk data corruption.'       + LineEnding +
               ''                                                       + LineEnding +
               'A crash log has been saved in the data directory.';

    if MessageDlg(Format(Message, [E.Message, ExtractRelativePath(SimbaEnv.DataPath, FileName)]), mtError, mbOKCancel, 0) = mrOk then
    begin
      SimbaController.CloseAllTabs();

      Halt(1);
    end;
  except
    // circular exception ...
  end;
end;

procedure TSimbaMainForm.DoAssociateIfFirstLaunch;
begin
  if SimbaSettings.FirstLaunch then
    SimbaEvents.Post(ESimbaEvent.ACTION_ASSOCIATE, nil);
end;

procedure TSimbaMainForm.DoApplicationParameters;
var
  FileName: String;
begin
  if Application.HasOption('open') then
  begin
    FileName := Application.Params[Application.ParamCount];
    if FileExists(FileName) then
    begin
      SimbaController.OpenInTab(FileName);

      if Application.HasOption('compile') then
        SimbaEvents.Post(ESimbaEvent.ACTION_COMPILE, nil)
      else if Application.HasOption('run') then
        SimbaEvents.Post(ESimbaEvent.ACTION_RUN, nil);
    end;
  end;
end;

procedure TSimbaMainForm.DoSetupCompleted;
begin
  SimbaEvents.Post(ESimbaEvent.SIMBA_SETUP_COMPLETED, nil);
end;

procedure TSimbaMainForm.TrayPopupExitClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaMainForm.Setup;
begin
  // Register events etc
  Application.CaptureExceptions := True;
  Application.OnException := @Self.DoException;

  Caption := Format('Simba %.1f', [SIMBA_VERSION / 1000]);
  TrayIcon.Visible := SimbaSettings.General.TrayIconVisible.Value;

  SimbaEvents.Register(Self, @DoSimbaEvent, [
    ESimbaEvent.ACTION_COMPILER_HINTS,
    ESimbaEvent.ACTION_VIEW_TRAYICON,
    ESimbaEvent.ACTION_REPORTBUG,
    ESimbaEvent.ACTION_SIMBAGITHUB,
    ESimbaEvent.ACTION_ONLINEDOCS,
    ESimbaEvent.ACTION_ASSOCIATE,
    ESimbaEvent.ACTION_ACA,
    ESimbaEvent.ACTION_DTM_EDITOR,
    ESimbaEvent.ACTION_QUIT
  ]);

  QueueOnMainThread(@DoAssociateIfFirstLaunch);
  QueueOnMainThread(@DoApplicationParameters); // open/compile/run parameters
  QueueOnMainThread(@DoSetupCompleted);
end;

destructor TSimbaMainForm.Destroy;
begin
  inherited Destroy();

  SimbaInitialization_Call(ESimbaInit.IDE_DESTROY);
end;

procedure TSimbaMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SimbaScriptTabsForm.CloseAllTabs();
end;

procedure TSimbaMainForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoViewTrayIcon(MenuItem: TMenuItem);
  begin
    SimbaSettings.General.TrayIconVisible.Value := MenuItem.Checked;

    TrayIcon.Visible := MenuItem.Checked;
  end;

  procedure DoReportBug;
  begin
    SimbaNativeInterface.OpenURL(SIMBA_BUGS_URL);
  end;

  procedure DoSimbaGithub;
  begin
    SimbaNativeInterface.OpenURL(SIMBA_GITHUB_URL);
  end;

  procedure DoOnlineDocs;
  begin
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL);
  end;

  procedure DoDTMEditor;
  begin
    with TSimbaDTMEditor.Create(@DoGetTargetImage) do
    begin
      FreeOnClose := True;
      ShowOnTop();
    end;
  end;

  procedure DoACA;
  begin
    with TSimbaACA.Create(@DoGetTargetImage) do
    begin
      FreeOnClose := True;
      ShowOnTop();
    end;
  end;

  procedure DoAssociate;
  const
    Message = 'Would you like to associate Simba files with this Simba?'                                   + LineEnding +
              'This means when opening a .simba file the file will be opened using this Simba executable.' + LineEnding +
              'It also adds right click actions to run the script.';
  begin
    {$IFDEF WINDOWS}
    if (MessageDlg(Message, mtConfirmation, mbYesNo, 0) = mrYes) then
      Associate();
    {$ENDIF}
  end;

  procedure DoQuit;
  begin
    Close();
  end;

  procedure DoCompilerHints(MenuItem: TMenuItem);
  begin
    SimbaSettings.Compiler.ShowHints.Value := MenuItem.Checked;
  end;

begin
  case Event of
    ESimbaEvent.ACTION_COMPILER_HINTS: DoCompilerHints(TMenuItem(Data));
    ESimbaEvent.ACTION_VIEW_TRAYICON:  DoViewTrayIcon(TMenuItem(Data));
    ESimbaEvent.ACTION_REPORTBUG:      DoReportBug();
    ESimbaEvent.ACTION_SIMBAGITHUB:    DoSimbaGithub();
    ESimbaEvent.ACTION_ONLINEDOCS:     DoOnlineDocs();
    ESimbaEvent.ACTION_ASSOCIATE:      DoAssociate();
    ESimbaEvent.ACTION_ACA:            DoACA();
    ESimbaEvent.ACTION_DTM_EDITOR:     DoDTMEditor();
    ESimbaEvent.ACTION_QUIT:           DoQuit();
  end;
end;

function TSimbaMainForm.DoGetTargetImage: TSimbaImage;
begin
  Result := SimbaController.GetTargetImage();
end;

procedure TSimbaMainForm.TrayIconClick(Sender: TObject);
begin
  ShowOnTop();
  if CanSetFocus() then
    SetFocus();
end;

procedure SetupSimbaForm;
begin
  if (SimbaMainForm = nil) then
    SimbaException('SimbaForm is nil');

  SimbaMainForm.Setup();
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @SetupSimbaForm, 'SimbaForm');

end.
