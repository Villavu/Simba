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
  Classes, SysUtils,
  simba.base,
  simba.colormath,
  simba.containers;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaEvent = (
    NONE,

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
    // Data=PString
    ACTION_OPEN_FILE,

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
    // Event called when a splitter is double clicked
    SPLITTER_DOUBLE_CLICK,

    // Event called when a tabs script state changes. Data=TSimbaScriptTab
    TAB_SCRIPTSTATE_CHANGE,
    // Event called on mouselogger change. Data=TSimbaMouseLogger
    MOUSELOGGER_CHANGE,

    //Data=TSimbaFunctionListNode
    FUNCTIONLIST_SELECTION_CHANGE,

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
