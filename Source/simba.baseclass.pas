{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.baseclass;

{$i simba.inc}

interface

uses
  classes, sysutils;

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

  TSimbaObjectTracker = class
  protected
    FList: TList;
    FDestroying: Boolean;
  public
    procedure Add(Obj: TSimbaBaseClass);
    procedure Remove(Obj: TSimbaBaseClass);

    constructor Create;
    destructor Destroy; override;
  end;

var
  SimbaObjectTracker: TSimbaObjectTracker;

implementation

uses
  simba.base;

procedure TSimbaBaseClass.NotifyUnfreed;
begin
  if (Name <> '') then
    DebugLn([EDebugLn.YELLOW], '  %s (%s) "%s"'.Format([ClassName, HexStr(Self), Name]))
  else
    DebugLn([EDebugLn.YELLOW], '  %s (%s)'.Format([ClassName, HexStr(Self)]));
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

  if (SimbaObjectTracker <> nil) then
    SimbaObjectTracker.Add(Self);
end;

destructor TSimbaBaseClass.Destroy;
begin
  if (SimbaObjectTracker <> nil) then
    SimbaObjectTracker.Remove(Self);

  inherited Destroy();
end;

function TSimbaBaseClass.GetSelf: TSimbaBaseClass;
begin
  Result := Self;
end;

procedure TSimbaObjectTracker.Add(Obj: TSimbaBaseClass);
begin
  FList.Add(Obj);
end;

procedure TSimbaObjectTracker.Remove(Obj: TSimbaBaseClass);
begin
  if FDestroying then
  begin
    if (FList.IndexOf(Obj) > -1) then
      FList[FList.IndexOf(Obj)] := nil;
  end else
    FList.Remove(Obj);
end;

constructor TSimbaObjectTracker.Create;
begin
  inherited Create();

  FList := TList.Create();
end;

destructor TSimbaObjectTracker.Destroy;
var
  I: Integer;
  HasUnfreed: Boolean;
begin
  FDestroying := True;

  if (FList <> nil) then
  begin
    if (FList.Count > 0) then
    begin
      for I := FList.Count - 1 downto 0 do
        if (FList[I] <> nil) and TSimbaBaseClass(FList[I]).FreeOnTerminate then
          TSimbaBaseClass(FList[I]).Free();

      HasUnfreed := False;
      for I := 0 to FList.Count - 1 do
        if (FList[I] <> nil) then
        begin
          if not HasUnfreed then
          begin
            DebugLn([EDebugLn.YELLOW], 'The following objects were not freed:');

            HasUnfreed := True;
          end;

          TSimbaBaseClass(FList[I]).NotifyUnfreed();
          TSimbaBaseClass(FList[I]).Free();
        end;
    end;

    FreeAndNil(FList);
  end;

  inherited Destroy();
end;

initialization
  SimbaObjectTracker := TSimbaObjectTracker.Create();

finalization
  SimbaObjectTracker.Free();

end.

