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

    function GetName: String;
    procedure SetName(Value: String);
  public
    constructor Create;
    destructor Destroy; override;

    property Name: String read GetName write SetName;
  end;

  TSimbaObjectTracker = class
  protected
    FList: TList;
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
  simba.outputform;

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

procedure TSimbaObjectTracker.Add(Obj: TSimbaBaseClass);
begin
  FList.Add(Obj);
end;

procedure TSimbaObjectTracker.Remove(Obj: TSimbaBaseClass);
begin
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
begin
  if (FList <> nil) then
  begin
    if (FList.Count > 0) then
    begin
      DebugLnHint('The following %d objects were not freed:', [FList.Count]);

      for I := 0 to FList.Count - 1 do
        with TSimbaBaseClass(FList[I]) do
          if (Name <> '') then
            DebugLnHint('%s (%s) "%s"', [ClassName, HexStr(FList[I]), Name])
          else
            DebugLnHint('%s (%s)', [ClassName, HexStr(FList[I])]);
    end;

    FList.Free();
  end;

  inherited Destroy();
end;

initialization
  SimbaObjectTracker := TSimbaObjectTracker.Create();

finalization
  SimbaObjectTracker.Free();

end.

