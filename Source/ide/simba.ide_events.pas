{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_events;

{$i simba.inc}
{$DEFINE SIMBA_PRINT_IDE_EVENTS}

interface

uses
  Classes, SysUtils,
  simba.base, simba.colormath, simba.containers;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaEvent = (
    // Raw toolbar button click events, data=nil
    TOOLBAR_NEW,
    TOOLBAR_OPEN,
    TOOLBAR_SAVE,
    TOOLBAR_SAVEALL,
    TOOLBAR_COMPILE,
    TOOLBAR_RUN,
    TOOLBAR_PAUSE,
    TOOLBAR_STOP,
    TOOLBAR_PICKCOLOR,
    TOOLBAR_PICKTARGET,
    TOOLBAR_PICKAREA,
    TOOLBAR_CLEAROUTPUT,
    TOOLBAR_PACKAGE,

    // Event called when codetools is setup. Data=nil
    CODETOOLS_SETUP,

    // Data=TSimbaScriptTab
    TAB_CARETMOVED,
    TAB_MODIFIED,
    TAB_LOADED,
    TAB_SEARCH,
    TAB_BEFORECHANGE,
    TAB_CHANGE,
    TAB_ADD,
    TAB_CLOSED,
    TAB_CAPTION,

    // Event called on a form dock/undock
    FORM_DOCK,
    FORM_UNDOCK,
    SPLITTER_DOUBLE_CLICK,
    // Event called when a tabs script state changes. Data=TSimbaScriptTab
    TAB_SCRIPTSTATE_CHANGE,
    // Event called on mouselogger change. Data=TSimbaMouseLogger
    MOUSELOGGER_CHANGE,
    // Function list selection changed. Data=TSimbaFunctionListNode
    FUNCTIONLIST_SELECTION,

    // Color selector used. Data=TSimbaEventData_ColorPicked
    COLOR_PICKED
  );
  {$POP}

  TSimbaEventData_ColorPicked = record
    Color: TColor;
    Point: TPoint;
  end;

  TSimbaEventCallback = procedure(Event: ESimbaEvent; Data: Pointer) of object;

  TSimbaEvents = class(TObject)
  private type
    TCallbackList = specialize TSimbaList<TSimbaEventCallback>;
  private
    FCallbacks: TCallbackList;
  public
    procedure Post(Event: ESimbaEvent; Data: Pointer);
    procedure Register(Owner: TComponent; Callback: TSimbaEventCallback); overload;
    procedure Register(Callback: TSimbaEventCallback); overload;
    procedure UnRegister(Callback: TSimbaEventCallback);

    constructor Create;
    destructor Destroy; override;
  end;

var
  SimbaEvents: TSimbaEvents;

implementation

uses
  simba.initializations;

type
  TManagedEvent = class(TComponent)
  protected
    FCallback: TSimbaEventCallback;
  public
    constructor Create(AOwner: TComponent; Callback: TSimbaEventCallback); reintroduce;
    destructor Destroy; override;
  end;

constructor TManagedEvent.Create(AOwner: TComponent; Callback: TSimbaEventCallback);
begin
  inherited Create(AOwner);

  FCallback := Callback;
end;

destructor TManagedEvent.Destroy;
begin
  SimbaEvents.UnRegister(FCallback);

  inherited Destroy();
end;

procedure TSimbaEvents.Post(Event: ESimbaEvent; Data: Pointer);
var
  I: Integer;
begin
  {$IFDEF SIMBA_PRINT_IDE_EVENTS}
  WriteLn(Event);
  {$ENDIF}

  for I := 0 to FCallbacks.Count - 1 do
    FCallbacks[I](Event, Data);
end;

procedure TSimbaEvents.Register(Owner: TComponent; Callback: TSimbaEventCallback);
begin
  FCallbacks.Add(Callback);
  if (Owner <> nil) then
    TManagedEvent.Create(Owner, Callback);
end;

procedure TSimbaEvents.Register(Callback: TSimbaEventCallback);
begin
  Register(nil, Callback);
end;

procedure TSimbaEvents.UnRegister(Callback: TSimbaEventCallback);
var
  I: Integer;
begin
  for I := 0 to FCallbacks.Count - 1 do
    if (FCallbacks[I] = Callback) then
    begin
      FCallbacks.Delete(I);
      Exit;
    end;
end;

constructor TSimbaEvents.Create;
begin
  inherited Create();
  FCallbacks := TCallbackList.Create();
end;

destructor TSimbaEvents.Destroy;
begin
  FreeAndNil(FCallbacks);
  inherited Destroy();
end;

procedure DoCreate;
begin
  SimbaEvents := TSimbaEvents.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaEvents);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'SimbaEvents', 10); // Priority 10  = create first
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaEvents', -10);     // Priority -10 = finalize last

end.
