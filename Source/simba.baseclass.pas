{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.baseclass;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  TSimbaBaseClass = class
  protected
    FName: String;
    FFreeOnTerminate: Boolean;

    procedure NotifyUnfreed; virtual;

    function GetName: String;
    procedure SetName(Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    function GetSelf: TSimbaBaseClass;

    property Name: String read GetName write SetName;
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
  end;
  TSimbaBaseClassType = class of TSimbaBaseClass;
  TSimbaBaseClassArray = array of TSimbaBaseClass;

  TSimbaBaseThread = class(TThread)
  protected
    FName: String;

    procedure NotifyUnfreed; virtual; // also used for unfinished
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    property Terminated;
  end;

  procedure PrintUnfreedObjects;
  procedure PrintUnfinishedThreads;
  procedure PrintUnfreedThreads;

  function GetSimbaObjectsOfClass(ClassType: TSimbaBaseClassType): TSimbaBaseClassArray;

implementation

uses
  simba.containers, simba.threading;

type
  TTrackedObjects = specialize TSimbaThreadsafeObjectList<TSimbaBaseClass>;
  TTrackedThreads = specialize TSimbaThreadsafeObjectList<TSimbaBaseThread>;

var
  TrackedObjects: TTrackedObjects;
  TrackedThreads: TTrackedThreads;

procedure PrintUnfreedObjects;
var
  NeedHeader: Boolean = True;
begin
  TrackedObjects.Lock();
  try
    while (TrackedObjects.Count > 0) do
    begin
      if not TrackedObjects.First.FreeOnTerminate then
      begin
        if NeedHeader then
          DebugLn(DEBUG_YELLOW + 'The following objects were not freed:' + DEBUG_RESET);
        NeedHeader := False;

        TrackedObjects.First.NotifyUnfreed();
      end;

      TrackedObjects.First.Free();
    end;
  finally
    TrackedObjects.Unlock();
  end;
end;

procedure PrintUnfinishedThreads;
var
  NeedHeader: Boolean = True;
  I: Integer;
begin
  TrackedThreads.Lock();
  try
    for I := 0 to TrackedThreads.Count - 1 do
      if not TrackedThreads[I].Finished then
      begin
        if NeedHeader then
          DebugLn(DEBUG_YELLOW + 'The following threads were still running:' + DEBUG_RESET);
        NeedHeader := False;

        TrackedThreads[I].NotifyUnfreed();
      end;
  finally
    TrackedThreads.Unlock();
  end;
end;

procedure PrintUnfreedThreads;
var
  NeedHeader: Boolean = True;
begin
  TrackedThreads.Lock();
  try
    while (TrackedThreads.Count > 0) do
    begin
      if TrackedThreads.First.Finished and (not TrackedThreads.First.FreeOnTerminate) then
      begin
        if NeedHeader then
          DebugLn(DEBUG_YELLOW + 'The following threads were not freed:' + DEBUG_RESET);
        NeedHeader := False;

        TrackedThreads.First.NotifyUnfreed();
      end;

      if TrackedThreads.First.FreeOnTerminate then
        TrackedThreads.Delete(TrackedThreads.First)
      else
        TrackedThreads.First.Free();
    end;
  finally
    TrackedThreads.Unlock();
  end;
end;

function GetSimbaObjectsOfClass(ClassType: TSimbaBaseClassType): TSimbaBaseClassArray;
var
  I: Integer;
begin
  Result := [];

  TrackedObjects.Lock();
  try
    for I := 0 to TrackedObjects.Count - 1 do
      if (TrackedObjects[I] is ClassType) then
        Result := Result + [TrackedObjects[I]];
  finally
    TrackedObjects.Unlock();
  end;
end;

procedure TSimbaBaseClass.NotifyUnfreed;
begin
  DebugLn(DEBUG_YELLOW + '  ' + ClassName + ' (' + HexStr(Self) + ')' + IfThen(Name <> '', ' "' + Name + '"', '') + DEBUG_RESET);
end;

function TSimbaBaseClass.GetName: String;
begin
  Result := FName;
end;

procedure TSimbaBaseClass.SetName(Value: String);
begin
  FName := Value;
end;

constructor TSimbaBaseClass.Create;
begin
  inherited Create();

  if (TrackedObjects <> nil) then
    TrackedObjects.Add(Self);
end;

destructor TSimbaBaseClass.Destroy;
begin
  if (TrackedObjects <> nil) then
    TrackedObjects.Delete(Self);

  inherited Destroy();
end;

function TSimbaBaseClass.GetSelf: TSimbaBaseClass;
begin
  Result := Self;
end;

procedure TSimbaBaseThread.NotifyUnfreed;
begin
  DebugLn(DEBUG_YELLOW + '  ' + ClassName + ' (' + HexStr(Self) + ')' + IfThen(FName <> '', ' "' + FName + '"', '') + DEBUG_RESET);
end;

constructor TSimbaBaseThread.Create;
begin
  inherited Create(True, DefaultStackSize div 2);

  if (TrackedThreads <> nil) then
    TrackedThreads.Add(Self);
end;

destructor TSimbaBaseThread.Destroy;
begin
  inherited Destroy();

  if (TrackedThreads <> nil) then
    TrackedThreads.Delete(Self);
end;

initialization
  TrackedObjects := TTrackedObjects.Create();
  TrackedThreads := TTrackedThreads.Create();

finalization
  if (TrackedObjects <> nil) then
  begin
    while (TrackedObjects.Count > 0) do
      TrackedObjects.First.Free();
    FreeAndNil(TrackedObjects);
  end;

  if (TrackedThreads <> nil) then
  begin
    while (TrackedThreads.Count > 0) do
      TrackedThreads.First.Free();
    FreeAndNil(TrackedThreads);
  end;

end.
