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
  simba.base, simba.containers;

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

  TSimbaBaseThread = class(TThread)
  protected
    FName: String;

    procedure NotifyUnfreed; virtual; // also used for unfinished
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
  end;

  procedure PrintUnfreedObjects;
  procedure PrintUnfinishedThreads;
  procedure PrintUnfreedThreads;

implementation

var
  TrackedObjects: specialize TSimbaObjectList<TSimbaBaseClass>;
  TrackedThreads: specialize TSimbaObjectList<TSimbaBaseThread>;

procedure PrintUnfreedObjects;
var
  I: Integer;
begin
  for I := 0 to TrackedObjects.Count - 1 do
    if not TrackedObjects[I].FreeOnTerminate then
    begin
      DebugLn([EDebugLn.YELLOW], 'The following objects were not freed:');
      Break;
    end;

  while (TrackedObjects.Count > 0) do
  begin
    if not TrackedObjects[0].FreeOnTerminate then
      TrackedObjects[0].NotifyUnfreed();
    TrackedObjects[0].Free();
  end;
end;

procedure PrintUnfinishedThreads;
var
  I: Integer;
begin
  for I := 0 to TrackedThreads.Count - 1 do
    if not TrackedThreads[I].Finished then
    begin
      DebugLn([EDebugLn.YELLOW], 'The following threads were still running:');
      Break;
    end;

  for I := 0 to TrackedThreads.Count - 1 do
    if not TrackedThreads[I].Finished then
      TrackedThreads[I].NotifyUnfreed();
end;

procedure PrintUnfreedThreads;
var
  I: Integer;
begin
  for I := 0 to TrackedThreads.Count - 1 do
    if TrackedThreads[I].Finished and (not TrackedThreads[I].FreeOnTerminate) then
    begin
      DebugLn([EDebugLn.YELLOW], 'The following threads were not freed:');
      Break;
    end;

  while (TrackedThreads.Count > 0) do
  begin
    if not TrackedThreads[0].FreeOnTerminate then
      TrackedThreads[0].NotifyUnfreed();
    TrackedThreads[0].Free();
  end;
end;

procedure TSimbaBaseClass.NotifyUnfreed;
begin
  DebugLn([EDebugLn.YELLOW], '  ' + ClassName + ' (' + HexStr(Self) + ')' + IfThen(Name <> '', ' "' + Name + '"', ''));
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
  DebugLn([EDebugLn.YELLOW], '  ' + ClassName + ' (' + HexStr(Self) + ')' + IfThen(FName <> '', ' "' + FName + '"', ''));
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
  TrackedObjects := specialize TSimbaObjectList<TSimbaBaseClass>.Create();
  TrackedThreads := specialize TSimbaObjectList<TSimbaBaseThread>.Create();

finalization
  while (TrackedObjects.Count > 0) do
    TrackedObjects[0].Free();
  TrackedObjects.Free();

  while (TrackedThreads.Count > 0) do
    TrackedThreads[0].Free();
  TrackedThreads.Free();

end.

