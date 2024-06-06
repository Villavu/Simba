{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_utils;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Menus, Graphics,
  simba.base;

function ImageWidthForDPI(DPI: Integer): Integer;
procedure MenuItemHeight(Item: TMenuItem; Canvas: TCanvas; var Height: Integer);

type
  TApplicationHelper = class helper for TApplication
  public type
    TProcedureNested = procedure is nested;

    TRunInThread = class(TThread)
    protected
      FProcNested: TProcedureNested;
      FProcOfObject: TProcedureOfObject;

      procedure Execute; override;
    public
      constructor Create(ProcNested: TProcedureNested; ProcOfObject: TProcedureOfObject); reintroduce;
    end;
  public
    function RunInThreadAndWait(Proc: TProcedureNested): String; overload;
    function RunInThreadAndWait(Proc, IdleProc: TProcedureNested): String; overload;

    procedure RunInThreadsAndWait(Procs: array of TProcedureOfObject);
  end;

implementation

uses
  simba.settings;

function ImageWidthForDPI(DPI: Integer): Integer;
begin
  if not SimbaSettings.General.CustomImageSize.IsDefault() then
  begin
    Result := SimbaSettings.General.CustomImageSize.Value;
    Exit;
  end;

  if (DPI <= 96) then
    Result := 16  // 100%, no scaling
  else
  if (DPI <= 120) then
    Result := 16  // 125%
  else
  if (DPI <= 168) then
    Result := 24  // 150%
  else
    Result := 32; // 200% +
end;

procedure MenuItemHeight(Item: TMenuItem; Canvas: TCanvas; var Height: Integer);
var
  ImgWidth: Integer;
begin
  if not Item.IsLine then
  begin
    ImgWidth := ImageWidthForDPI(Canvas.Font.PixelsPerInch);
    if (ImgWidth > 16) then
      Height := Round(ImgWidth * 1.3);
  end;
end;

function TApplicationHelper.RunInThreadAndWait(Proc: TProcedureNested): String;
begin
  Result := '';

  if not Assigned(Proc) then
  begin
    Result := 'Proc=nil';
    Exit;
  end;

  with TRunInThread.Create(Proc, nil) do
  try
    while not Finished do
    begin
      if (GetCurrentThreadID() = MainThreadID) then
        ProcessMessages();

      Sleep(50);
    end;

    if Assigned(FatalException) then
      if (FatalException is Exception) then
        Result := Exception(FatalException).Message
      else
        Result := FatalException.ToString;
  finally
    Free();
  end;
end;

function TApplicationHelper.RunInThreadAndWait(Proc, IdleProc: TProcedureNested): String;
begin
  Result := '';

  if not Assigned(Proc) then
    Exit('Proc=nil');
  if not Assigned(IdleProc) then
    Exit('IdleProc=nil');

  with TRunInThread.Create(Proc, nil) do
  try
    while not Finished do
      IdleProc();

    if Assigned(FatalException) then
      if (FatalException is Exception) then
        Result := Exception(FatalException).Message
      else
        Result := FatalException.ToString;
  finally
    Free();
  end;
end;

procedure TApplicationHelper.RunInThreadsAndWait(Procs: array of TProcedureOfObject);
var
  I: Integer;
  Threads: array of TRunInThread;
begin
  SetLength(Threads, Length(Procs));
  for I := 0 to High(Procs) do
    Threads[I] := TRunInThread.Create(nil, Procs[I]);

  for I := 0 to High(Threads) do
  begin
    while not Threads[I].Finished do
    begin
      if (GetCurrentThreadID() = MainThreadID) then
        ProcessMessages();

      Sleep(25);
    end;

    Threads[I].Free();
  end;
end;

procedure TApplicationHelper.TRunInThread.Execute;
begin
  if Assigned(FProcNested) then
    FProcNested()
  else
  if Assigned(FProcOfObject) then
    FProcOfObject();
end;

constructor TApplicationHelper.TRunInThread.Create(ProcNested: TProcedureNested; ProcOfObject: TProcedureOfObject);
begin
  inherited Create(False, 512*512);

  FProcNested := ProcNested;
  FProcOfObject := ProcOfObject;
end;

end.

