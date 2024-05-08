{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.fs_async;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.threading, Zipper;

type
  TASyncUnzipResult = record
    ZipFile: String;
    DestPath: String;
    Success: Boolean;
    Exception: String;
    TimeUsed: Double;
  end;
  TASyncUnzipFinishedEvent = procedure(constref Result: TASyncUnzipResult) of object;
  TASyncUnzipProgressEvent = procedure(Position, Total: Int64) of object;

  ASyncUnzip = class
    class procedure Unzip(ZipFile, DestPath: String;
                          OnFinished: TASyncUnzipFinishedEvent;
                          OnProgress: TASyncUnzipProgressEvent); static;
  end;

implementation

uses
  simba.datetime;

type
  TUnzipInBackground = class(TThread)
  protected
    FZipFile: String;
    FDestPath: String;
    FOnProgress: TASyncUnzipProgressEvent;
    FOnFinished: TASyncUnzipFinishedEvent;

    procedure DoProgress(Sender: TObject; Const ATotPos, ATotSize: Int64);

    procedure Execute; override;
  public
    constructor Create(ZipFile, DestPath: String; OnProgress: TASyncUnzipProgressEvent; OnFinished: TASyncUnzipFinishedEvent); reintroduce;
  end;

class procedure ASyncUnzip.Unzip(ZipFile, DestPath: String; OnFinished: TASyncUnzipFinishedEvent; OnProgress: TASyncUnzipProgressEvent);
begin
  TUnzipInBackground.Create(ZipFile, DestPath, OnProgress, OnFinished);
end;

procedure TUnzipInBackground.DoProgress(Sender: TObject; const ATotPos, ATotSize: Int64);
begin
  if Assigned(FOnProgress) then
    FOnProgress(ATotPos, ATotSize);
end;

procedure TUnzipInBackground.Execute;
var
  Result: TASyncUnzipResult;
begin
  Result := Default(TASyncUnzipResult);
  Result.TimeUsed := HighResolutionTime();
  Result.Success := True;
  Result.ZipFile := FZipFile;
  Result.DestPath := FDestPath;

  try
    with TUnZipper.Create() do
    try
      FileName := FZipFile;
      OutputPath := FDestPath;
      OnProgressEx := @DoProgress;

      UnZipAllFiles();
    finally
      Free();
    end;
  except
    on E: Exception do
    begin
      Result.Exception := E.Message;
      Result.Success := False;
    end;
  end;

  Result.TimeUsed := HighResolutionTime() - Result.TimeUsed;

  if Assigned(FOnFinished) then
    FOnFinished(Result);
end;

constructor TUnzipInBackground.Create(ZipFile, DestPath: String; OnProgress: TASyncUnzipProgressEvent; OnFinished: TASyncUnzipFinishedEvent);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FZipFile := ZipFile;
  FDestPath := DestPath;
  FOnProgress := OnProgress;
  FOnFinished := OnFinished;
end;

end.

