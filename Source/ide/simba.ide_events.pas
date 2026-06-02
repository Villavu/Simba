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
  Classes, SysUtils, ExtCtrls,
  simba.base,
  simba.colormath,
  simba.containers;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaEvent = (
    NONE,

    // Simba has been setup completely and should now be showing. Data=nil
    SIMBA_SETUP_COMPLETED,

    // General use for timer execute every 750ms. Data=nil
    TIMER_750,

    // "file" menu, Data=TMenuItem
    ACTION_NEW,
    ACTION_OPEN,
    ACTION_OPEN_EXAMPLE,
    ACTION_OPEN_BACKUP,
    ACTION_SAVE,
    ACTION_SAVE_AS,
    ACTION_SAVE_AS_DEFAULT,
    ACTION_SAVE_ALL,
    ACTION_CLOSE_TAB,
    ACTION_CLOSE_ALL_TABS,
    ACTION_QUIT,

    // "edit" menu, Data=TMenuItem
    ACTION_UNDO,
    ACTION_REDO,
    ACTION_CUT,
    ACTION_COPY,
    ACTION_PASTE,
    ACTION_SELECT_ALL,
    ACTION_SELECT_LINE,
    ACTION_SELECT_WORD,
    ACTION_LOWER_SELECTION,
    ACTION_UPPER_SELECTION,

    // "Search" menu, Data=TMenuItem
    ACTION_FIND,
    ACTION_FIND_NEXT,
    ACTION_FIND_PREV,
    ACTION_FIND_IN_FILES,
    ACTION_REPLACE,
    ACTION_GOTO_LINE,

    // "Script" menu, Data=TMenuItem
    ACTION_COMPILE,
    ACTION_RUN,
    ACTION_PAUSE,
    ACTION_STOP,
    ACTION_COMPILER_HINTS,

    // "Tools" menu, Data=TMenuItem
    ACTION_SETTINGS,
    ACTION_PACKAGES,
    ACTION_ASSOCIATE,
    ACTION_IMG_TO_STRING,
    ACTION_ACA,
    ACTION_DTM_EDITOR,
    ACTION_SHAPE_BOX,
    ACTION_FORMAT_SCRIPT,
    ACTION_DOWNLOAD_SIMBA,

    // "View" menu, Data=TMenuItem
    ACTION_VIEW_TRAYICON,
    ACTION_VIEW_COLORHISTORY,
    ACTION_VIEW_DEBUGIMAGE,
    ACTION_VIEW_DEBUGMATRIX,
    ACTION_VIEW_EDITOR,
    ACTION_VIEW_FUNCTIONLIST,
    ACTION_VIEW_NOTES,
    ACTION_VIEW_FILEBROWSER,
    ACTION_VIEW_OUTPUT,
    ACTION_VIEW_BACKUP,
    ACTION_VIEW_FINDINFILES,
    ACTION_RESET_LAYOUT,
    ACTION_LOCK_LAYOUT,

    // "Help" menu, Data=TMenuItem
    ACTION_ABOUT,
    ACTION_REPORTBUG,
    ACTION_SIMBAGITHUB,
    ACTION_ONLINEDOCS,

    // Misc actions
    // Data = nil
    ACTION_PICKCOLOR,
    ACTION_PICKTARGET,
    ACTION_PICKAREA,
    ACTION_CLEAROUTPUT,

    // Editor popup menu, Data=nil
    ACTION_FIND_DECL_AT_CARET,
    ACTION_COPY_FILENAME,
    ACTION_OPEN_DIRECTORY,
    ACTION_DOC_COMMENT,

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
    TAB_MOVED,
    TAB_CAN_SAVE,
    TAB_CANNOT_SAVE,
    TAB_ACTIVE_750,
    TAB_SCRIPT_START,

    // Event called on a form dock/undock
    FORM_DOCK,
    FORM_UNDOCK,
    // Event called when a splitter is double clicked
    SPLITTER_DOUBLE_CLICK,

    // Event called when a tabs script state changes. Data=TSimbaScriptTab
    TAB_SCRIPTSTATE_CHANGE,

    //Data=TDeclaration
    FUNCTIONLIST_SELECTION_CHANGE,

    // Color selector used. Data=TSimbaEventData_ColorPicked
    COLOR_PICKED,

    // Package installations changed. Data=TSimbaPackageArray
    PACKAGE_INSTALLS_CHANGED,
    // Package from closed. Data=nil
    PACKAGE_FORM_CLOSED
  );
  {$POP}

  TSimbaEventCallback = procedure(Event: ESimbaEvent; Data: Pointer) of object;

  TSimbaEvents = class(TObject)
  public type
    TColorPicked = record
      Color: TColor;
      Point: TPoint;
    end;

    TTabMoved = record
      Tab: Pointer;
      FromIndex: Integer;
      ToIndex: Integer;
    end;
  private type
    TCallbackList = specialize TSimbaList<TSimbaEventCallback>;
  private
    FCallbacks: array[ESimbaEvent] of TCallbackList;
    FTimer: TTimer;

    procedure DoTimer(Sender: TObject);
  public
    procedure Post(Event: ESimbaEvent; Data: Pointer);
    procedure Register(Owner: TComponent; Callback: TSimbaEventCallback; Events: array of ESimbaEvent); overload;
    procedure Register(Callback: TSimbaEventCallback; Events: array of ESimbaEvent); overload;
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

procedure TSimbaEvents.DoTimer(Sender: TObject);
begin
  SimbaEvents.Post(ESimbaEvent.TIMER_750, nil);
end;

procedure TSimbaEvents.Post(Event: ESimbaEvent; Data: Pointer);
var
  I: Integer;
begin
  {$IFDEF SIMBA_PRINT_IDE_EVENTS}
  if (Event <> ESimbaEvent.TIMER_750) and (Event <> ESimbaEvent.TAB_ACTIVE_750) then
    WriteLn(Event);
  {$ENDIF}

  for I := 0 to FCallbacks[Event].Count - 1 do
    FCallbacks[Event][I](Event, Data);
end;

procedure TSimbaEvents.Register(Owner: TComponent; Callback: TSimbaEventCallback; Events: array of ESimbaEvent);
var
  Event: ESimbaEvent;
begin
  for Event in Events do
    FCallbacks[Event].Add(Callback);
  if (Owner <> nil) then
    TManagedEvent.Create(Owner, Callback);
end;

procedure TSimbaEvents.Register(Callback: TSimbaEventCallback; Events: array of ESimbaEvent);
begin
  Register(nil, Callback, Events);
end;

procedure TSimbaEvents.UnRegister(Callback: TSimbaEventCallback);
var
  I: Integer;
  Event: ESimbaEvent;
begin
  for Event in ESimbaEvent do
  begin
    I := 0;
    while (I < FCallbacks[Event].Count) do
    begin
      if (FCallbacks[Event][I] = Callback) then
        FCallbacks[Event].Delete(I)
      else
        Inc(I);
    end;
  end;
end;

constructor TSimbaEvents.Create;
var
  Event: ESimbaEvent;
begin
  inherited Create();

  for Event in ESimbaEvent do
    FCallbacks[Event] := TCallbackList.Create();

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := @DoTimer;
  FTimer.Interval := 750;
  FTimer.Enabled := True;
end;

destructor TSimbaEvents.Destroy;
var
  Event: ESimbaEvent;
begin
  FreeAndNil(FTimer);
  for Event in ESimbaEvent do
    FreeAndNil(FCallbacks[Event]);

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
