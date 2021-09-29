{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.eventhandlerlist;

{$i simba.inc}

interface

uses
  classes, sysutils, generics.collections, syncobjs;

type
  TNotifyEventHandlers = class
  protected
  type
    TNotifyEventList = specialize TList<TNotifyEvent>;
  protected
    FList: TNotifyEventList;
    FLock: TCriticalSection;
  public
    function Add(Event: TNotifyEvent): Int32;
    procedure Remove(Index: Int32);
    procedure Call(Sender: TObject);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

function TNotifyEventHandlers.Add(Event: TNotifyEvent): Int32;
begin
  FLock.Enter();

  try
    Result := FList.IndexOf(nil);
    if Result = -1 then
      Exit(FList.Add(Event));

    FList[Result] := Event;
  finally
    FLock.Leave();
  end;
end;

procedure TNotifyEventHandlers.Remove(Index: Int32);
begin
  FList[Index] := nil;
end;

procedure TNotifyEventHandlers.Call(Sender: TObject);
var
  i: Int32;
begin
  FLock.Enter();

  try
    for i := 0 to FList.Count - 1 do
      if FList[i] <> nil then
        FList[i](Sender);
  finally
    FLock.Leave();
  end;
end;

constructor TNotifyEventHandlers.Create;
begin
  FList := TNotifyEventList.Create();
  FLock := TCriticalSection.Create();
end;

destructor TNotifyEventHandlers.Destroy;
begin
  FList.Free();
  FLock.Free();
end;

end.

