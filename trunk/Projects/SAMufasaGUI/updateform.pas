unit updateform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, updater;

type

  { TSimbaUpdateForm }

  { TSimbaVersionThread }

  TSimbaVersionThread = class(TThread)
  public
    ResultStr : string;
    InputURL : string;
    Done : boolean;
    procedure Execute; override;
  end;
  TSimbaUpdateForm = class(TForm)
    DownloadSpeed: TLabel;
    UpdateLog: TMemo;
    UpdateButton: TButton;
    OkButton: TButton;
    CancelButton: TButton;
    DownloadProgress: TProgressBar;
    procedure CancelButtonClick(Sender: TObject);
    procedure CleanUpdateForm(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    function CanUpdate: Boolean;

  private
    { private declarations }

    Updater: TMMLFileDownloader;
    FStartTime : longword;
    FCancelling: Boolean;
    FDone: Boolean;
    FUpdating : boolean;
    FOldSpeed : integer;
    FLastUpdateSpeed : longword;
    FSimbaVersion: Integer;
    SimbaVersionThread : TSimbaVersionThread;
  private
    function OnUpdateBeat: Boolean;
    function GetLatestSimbaVersion: Integer;
  public
    { public declarations }
    procedure PerformUpdate;
  protected
    FCancelled: Boolean;
  end;

const
  DownloadSpeedTextRunning = 'Downloading at %d kB/s';
  DownloadSpeedTextEnded = 'Downloaded at %d kB/s';

var
  SimbaUpdateForm: TSimbaUpdateForm;

implementation

uses
  internets,  TestUnit, simbasettings,lclintf;

function TSimbaUpdateForm.CanUpdate: Boolean;
begin
  GetLatestSimbaVersion;
  Writeln(format('Current Simba version: %d',[TestUnit.SimbaVersion]));
  Writeln('Latest Simba Version: ' + IntToStr(FSimbaVersion));
  Exit(testunit.SimbaVersion < FSimbaVersion);
end;

function TSimbaUpdateForm.GetLatestSimbaVersion: Integer;

begin
  if SimbaVersionThread = nil then//Create thread (only if no-other one is already running)
  begin
    SimbaVersionThread := TSimbaVersionThread.Create(true);

    SimbaVersionThread.InputURL := SettingsForm.Settings.GetSetLoadSaveDefaultKeyValueIfNotExists(
                'Settings/Updater/RemoteVersionLink',
                {$IFDEF WINDOWS}
                  {$IFDEF CPUI386}
                  'http://simba.villavu.com/bin/Windows/x86/Stable/Version'
                  {$ELSE}
                  'http://simba.villavu.com/bin/Windows/x86_64/Stable/Version'
                  {$ENDIF}
                {$ELSE}
                  {$IFDEF CPUI386}
                  'http://simba.villavu.com/bin/Linux/x86/Stable/Version'
                  {$ELSE}
                  'http://simba.villavu.com/bin/Linux/x86_64/Stable/Version'
                  {$ENDIF}
                {$ENDIF}
                , SimbaSettingsFile);

    SimbaVersionThread.Resume;
    while SimbaVersionThread.Done = false do//Wait till thread is done
    begin
      Application.ProcessMessages;
      Sleep(50);
    end;
    FSimbaVersion := StrToIntDef(Trim(SimbaVersionThread.ResultStr), -1);//Read output
    FreeAndNil(SimbaVersionThread);//Free the thread
  end else
  begin
    //Another thread is already running, lets wait for it! (When it's nil, it means that the result is written!)
    while SimbaVersionThread = nil do
    begin;
      Application.ProcessMessages;
      Sleep(50);
    end;
  end;
  Exit(FSimbaVersion);
end;

procedure TSimbaUpdateForm.UpdateButtonClick(Sender: TObject);
begin
  if FUpdating then
    UpdateLog.Lines.Add('Already performing an update!')
  else
    Self.PerformUpdate;
end;

procedure TSimbaUpdateForm.CancelButtonClick(Sender: TObject);
begin
  if FCancelled or FDone then
  begin
    Self.ModalResult:=mrCancel;
    Self.Hide;
  end else
  begin
    FCancelling := True;
  end;
end;

procedure TSimbaUpdateForm.CleanUpdateForm(Sender: TObject);
begin
  Self.DownloadProgress.Position:=0;
  Self.UpdateLog.Clear;
  Self.UpdateLog.Lines.Add('---------- Update Session ----------');
  Self.DownloadSpeed.Visible:= false;
end;

procedure TSimbaUpdateForm.FormCreate(Sender: TObject);
begin
  FDone := True;
  FUpdating:= false;

end;

procedure TSimbaUpdateForm.OkButtonClick(Sender: TObject);
begin
  Self.ModalResult:=mrOK;
  Self.Hide;
end;

{ Return true if we have to cancel }
function TSimbaUpdateForm.OnUpdateBeat: Boolean;
var
  Percentage: Integer;
  NewSpeed : integer;
begin
  Application.ProcessMessages;

  Percentage := Updater.GetPercentage();
  if Percentage <> -1 then
    DownloadProgress.Position:=Percentage;
  // Formula for speed (kB/s) -> (Bytes div 1000) / (MSecSinceStart div 1000) = Bytes/ MSecSinceStart
  NewSpeed :=(Updater.DownloadedSize)  div ((GetTickCount-FStartTime));
  if abs(NewSpeed - FOldSpeed) > 1 then
    if (GetTickCount - FLastUpdateSpeed) > 1000 then    //Only update the speed text every second
    begin;
      FOldSpeed:= NewSpeed;
      DownloadSpeed.Caption:= Format(DownloadSpeedTextRunning,[NewSpeed]);
      FLastUpdateSpeed:= GetTickCount;
    end;
  Result := FCancelling;
end;

procedure TSimbaUpdateForm.PerformUpdate;

begin
  FUpdating:= True;
  Updater := TMMLFileDownloader.Create;

  FDone := False;
  FCancelling := False;
  FCancelled := False;

  Updater.FileURL := SettingsForm.Settings.GetSetLoadSaveDefaultKeyValueIfNotExists(
        'Settings/Updater/RemoteLink',
        'http://old.villavu.com/merlijn/Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF},
        SimbaSettingsFile
  );

  //ApplicationName{$IFDEF WINDOWS} +'.exe'{$ENDIF};

  // Should work on Windows as well
  Updater.ReplacementFile := ExtractFileName(Application.ExeName);
  Updater.OnBeat := @Self.OnUpdateBeat;
  Updater.BasePath := ExtractFilePath(Application.ExeName);

  Self.UpdateLog.Lines.Add('Starting download of ' + Updater.FileURL + ' ...');
  try
    Self.OkButton.Enabled := False; // grey out button
    DownloadSpeed.Visible:= true;
    DownloadSpeed.Caption:= Format(DownloadSpeedTextRunning,[0]);
    FStartTime:= GetTickCount - 1;//Be sure that we don't get div 0
    Updater.DownloadAndSave;
    DownloadSpeed.Caption := Format(DownloadSpeedTextEnded,[Updater.FileSize div (GetTickCount-FStartTime)]);
    Self.UpdateLog.Lines.Add('Downloaded to ' + Updater.ReplacementFile + '_ ...');
    Updater.Replace;
    Self.UpdateLog.Lines.Add('Renaming ' + Updater.ReplacementFile + ' to ' + Updater.ReplacementFile + '_old_');
    Self.UpdateLog.Lines.Add('Renaming ' + Updater.ReplacementFile + '_ to ' + Updater.ReplacementFile);
    Self.UpdateLog.Lines.Add('Deleting ' + Updater.ReplacementFile + '_old_');
    Updater.Free;
    Self.UpdateLog.Lines.Add('Done ... ');
    Self.UpdateLog.Lines.Add('Please restart all currently running Simba binaries.');
  except
    FCancelling := False;
    FCancelled := True;
    DownloadSpeed.Visible:= false;
    Self.UpdateLog.Lines.Add('Download stopped at '+inttostr(DownloadProgress.Position)+'%... Simba did not succesfully update.');
    // more detailed info
    writeln('EXCEPTION IN UPDATEFORM: We either hit Cancel, or something went wrong with files');
  end;
  FDone := True;
  Self.OkButton.Enabled := True; // un-grey out button
  FUpdating:= false;
end;

{ TSimbaVersionThread }

procedure TSimbaVersionThread.Execute;
begin
  ResultStr:= GetPage(InputURL);
  done := true;
end;

initialization
  {$I updateform.lrs}

end.

