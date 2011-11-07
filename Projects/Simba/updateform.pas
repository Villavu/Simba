{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

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

    update form for the Mufasa Macro Library
}
unit updateform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms,MufasaBase, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, updater;

type

  { TSimbaUpdateForm }
  TSimbaUpdateForm = class(TForm)
    DownloadSpeed: TLabel;
    UpdateLog: TMemo;
    UpdateButton: TButton;
    CloseButton: TButton;
    DownloadProgress: TProgressBar;
    procedure CloseButtonClick(Sender: TObject);
    procedure CleanUpdateForm(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
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
    FFontVersion : integer;
    SimbaVersionThread : TDownloadThread;
    FontVersionThread : TDownloadThread;
  private
    function OnUpdateBeat: Boolean;
  public
    function CanUpdate: Boolean;
    function GetLatestFontVersion : integer;
    function GetLatestSimbaVersion: Integer;
    procedure PerformUpdate;
  protected
    FCancelled: Boolean;
  end;

const
  DownloadSpeedTextRunning = 'Downloading at %d kB/s';
  DownloadSpeedTextEnded = 'Downloaded at %d kB/s';
  SimbaURL =     {$IFDEF WINDOWS}
                  {$IFDEF CPUI386}
                  'http://Simba.villavu.com/bin/Windows/x86/Stable/'
                  {$ELSE}
                  'http://Simba.villavu.com/bin/Windows/x86_64/Stable/'
                  {$ENDIF}
                {$ELSE}
                  {$IFDEF CPUI386}
                  'http://Simba.villavu.com/bin/Linux/x86/Stable/'
                  {$ELSE}
                  'http://Simba.villavu.com/bin/Linux/x86_64/Stable/'
                  {$ENDIF}
                {$ENDIF};
  FontURL = 'http://Simba.villavu.com/bin/Fonts/';

var
  SimbaUpdateForm: TSimbaUpdateForm;

implementation
uses
  internets,  SimbaUnit, Simbasettings,lclintf;

function TSimbaUpdateForm.CanUpdate: Boolean;
begin
  GetLatestSimbaVersion;
  mDebugLn(format('Current Simba version: %d',[SimbaUnit.SimbaVersion]));
  mDebugLn('Latest Simba Version: ' + IntToStr(FSimbaVersion));
  Exit(SimbaUnit.SimbaVersion < FSimbaVersion);
end;

function TSimbaUpdateForm.GetLatestFontVersion: integer;
var
  Vers : string;
begin
  if FontVersionThread = nil then//Create thread (only if no-other one is already running)
  begin
    FontVersionThread := TDownloadThread.Create(SettingsForm.Settings.GetKeyValueDefLoad(
                                                'Settings/Fonts/VersionLink',FontURL  + 'Version',SimbaSettingsFile),
                                                @Vers);
    FontVersionThread.Resume;
    while FontVersionThread.Done = false do//Wait till thread is done
    begin
      Application.ProcessMessages;
      Sleep(25);
    end;
    FFontVersion := StrToIntDef(Trim(Vers), -1);//Read output
    FontVersionThread := nil; //It's already freed
  end else
  begin
    //Another thread is already running, lets wait for it! (When it's nil, it means that the result is written!)
    while FontVersionThread = nil do
    begin;
      Application.ProcessMessages;
      Sleep(50);
    end;
  end;
  Exit(FFontVersion);

end;

function TSimbaUpdateForm.GetLatestSimbaVersion: Integer;
var
  Vers : string;
begin
  if SimbaVersionThread = nil then//Create thread (only if no-other one is already running)
  begin
    SimbaVersionThread := TDownloadThread.Create(SettingsForm.Settings.GetKeyValueDefLoad(
                                                 'Settings/Updater/RemoteVersionLink',SimbaURL + 'Version'
                                                 ,SimbaSettingsFile),@Vers);
    SimbaVersionThread.Resume;
    while SimbaVersionThread.Done = false do//Wait till thread is done
    begin
      Application.ProcessMessages;
      Sleep(25);
    end;
    FSimbaVersion := StrToIntDef(Trim(Vers), -1);//Read output
    SimbaVersionThread := nil;//It's automatically freed
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
    FCancelling := True
  else
    Self.PerformUpdate;
end;

procedure TSimbaUpdateForm.CloseButtonClick(Sender: TObject);
begin
  if FCancelled or FDone then
    Self.ModalResult := mrCancel
  else
    Self.UpdateLog.Lines.Add('Update in progress!');
end;

procedure TSimbaUpdateForm.CleanUpdateForm(Sender: TObject);
begin
  Self.DownloadProgress.Position := 0;
  Self.UpdateLog.Clear;
  Self.UpdateLog.Lines.Add('---------- Update Session ----------');
  Self.DownloadSpeed.Visible := false;
  if not CanUpdate then
  begin
    //ShowMessage('No Updates Available!');
    Self.UpdateLog.Lines.Add('No Updates Available!');
    Self.UpdateButton.Enabled := False;
  end else
    Self.UpdateButton.Enabled := true;
end;

procedure TSimbaUpdateForm.FormCreate(Sender: TObject);
begin
  FDone := True;
  FUpdating := false;
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

  Updater.FileURL := SettingsForm.Settings.GetKeyValueDefLoad(
        'Settings/Updater/RemoteLink',
        SimbaURL + 'Simba'{$IFDEF WINDOWS} +'.exe'{$ENDIF},
        SimbaSettingsFile
  );

  //ApplicationName{$IFDEF WINDOWS} +'.exe'{$ENDIF};

  // Should work on Windows as well
  Updater.ReplacementFile := ExtractFileName(Application.ExeName);
  Updater.OnBeat := @Self.OnUpdateBeat;
  Updater.BasePath := ExtractFilePath(Application.ExeName);

  Self.UpdateLog.Lines.Add('Starting download of ' + Updater.FileURL + ' ...');
  try
    Self.UpdateButton.Caption := 'Cancel'; // Update to Cancel
    Self.CloseButton.Enabled := false;
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
    DownloadSpeed.Visible := false;
    Self.UpdateLog.Lines.Add('Download stopped at '+inttostr(DownloadProgress.Position)+
      '%... Simba did not successfully update.');
    // more detailed info
    mDebugLn('EXCEPTION IN UPDATEFORM: We either hit Cancel, or something went wrong with files');
    if FileExists(Updater.BasePath + Updater.ReplacementFile + '_') then
    begin
      Self.UpdateLog.Lines.Add(Format('Deleting the ghost file (%s)',[Updater.BasePath + Updater.ReplacementFile + '_']));
      DeleteFile(Updater.BasePath + Updater.ReplacementFile + '_');
    end;
  end;
  FDone := True;
  Self.UpdateButton.Caption := 'Updated!';
  Self.UpdateButton.Enabled := False;
  Self.CloseButton.Enabled := true;
  FUpdating:= false;
end;

initialization
  {$R *.lfm}

end.

