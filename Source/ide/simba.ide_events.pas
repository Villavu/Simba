{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_events;

{$i simba.inc}
{.$DEFINE SIMBA_PRINT_IDE_EVENTS}

interface

uses
  Classes, SysUtils, LazMethodList,
  simba.base;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  SimbaIDEEvent = (
    // Event called when codetools is setup. Sender=nil
    CODETOOLS_SETUP,
    // Event called on editor caret moved. Sender=TSimbaScriptTab
    TAB_CARETMOVED,
    TAB_MODIFIED,
    TAB_LOADED,
    TAB_SEARCH,
    TAB_BEFORECHANGE,
    TAB_CHANGE,
    TAB_CLOSED,
    // Event called when a tabs script state changes. Sender=TSimbaScriptTab
    TAB_SCRIPTSTATE_CHANGE,
    // Event called on mouselogger change. Sender=TSimbaMouseLogger
    MOUSELOGGER_CHANGE,
    // Function list selection changed. Sender=TSimbaFunctionListNode
    FUNCTIONLIST_SELECTION,
    // Window selection changed
    WINDOW_SELECTED,
    // A color was picked
    COLOR_PICKED,
    // A area was selected
    AREA_SELECTED
  );
  {$POP}

  TSimbaIDEEvents = class(TObject)
  private
    FEvents: array[SimbaIDEEvent] of TMethodList;
  public
    procedure Notify(EventType: SimbaIDEEvent; Sender: TObject);
    procedure Register(Owner: TComponent; EventType: SimbaIDEEvent; Method: TNotifyEvent; AsFirst: Boolean = False); overload;
    procedure Register(EventType: SimbaIDEEvent; Method: TNotifyEvent; AsFirst: Boolean = False); overload;
    procedure UnRegister(EventType: SimbaIDEEvent; Proc: TNotifyEvent);

    constructor Create;
    destructor Destroy; override;
  end;

var
  SimbaIDEEvents: TSimbaIDEEvents;

implementation

uses
  simba.ide_initialization;

type
  TManagedEvent = class(TComponent)
  protected
    FMethod: TMethod;
    FList: TMethodList;
  public
    constructor Create(AOwner: TComponent; AMethod: TMethod; AsFirst: Boolean; AList: TMethodList); reintroduce;
    destructor Destroy; override;
  end;

constructor TManagedEvent.Create(AOwner: TComponent; AMethod: TMethod; AsFirst: Boolean; AList: TMethodList);
begin
  inherited Create(AOwner);

  FMethod := AMethod;

  FList := AList;
  FList.Add(FMethod, not AsFirst);
end;

destructor TManagedEvent.Destroy;
begin
  FList.Remove(FMethod);

  inherited Destroy();
end;

procedure TSimbaIDEEvents.Notify(EventType: SimbaIDEEvent; Sender: TObject);
begin
  {$IFDEF SIMBA_PRINT_IDE_EVENTS}
  WriteLn(EventType);
  {$ENDIF}
  FEvents[EventType].CallNotifyEvents(Sender);
end;

procedure TSimbaIDEEvents.Register(Owner: TComponent; EventType: SimbaIDEEvent; Method: TNotifyEvent; AsFirst: Boolean);
begin
  TManagedEvent.Create(Owner, TMethod(Method), AsFirst, FEvents[EventType]);
end;

procedure TSimbaIDEEvents.Register(EventType: SimbaIDEEvent; Method: TNotifyEvent; AsFirst: Boolean);
begin
  FEvents[EventType].Add(TMethod(Method), not AsFirst);
end;

procedure TSimbaIDEEvents.UnRegister(EventType: SimbaIDEEvent; Proc: TNotifyEvent);
begin
  FEvents[EventType].Remove(TMethod(Proc));
end;

constructor TSimbaIDEEvents.Create;
var
  EventType: SimbaIDEEvent;
begin
  inherited Create();

  for EventType in SimbaIDEEvent do
    FEvents[EventType] := TMethodList.Create();
end;

destructor TSimbaIDEEvents.Destroy;
var
  EventType: SimbaIDEEvent;
begin
  for EventType in SimbaIDEEvent do
    if (FEvents[EventType] <> nil) then
      FreeAndNil(FEvents[EventType]);

  inherited Destroy();
end;

procedure CreateSimbaIDEEvents;
begin
  SimbaIDEEvents := TSimbaIDEEvents.Create();
end;

initialization
  SimbaIDEInitialization_AddBeforeCreate(@CreateSimbaIDEEvents, 'Create SimbaIDEEvents');

finalization
  if Assigned(SimbaIDEEvents) then
    FreeAndNil(SimbaIDEEvents);

end.


