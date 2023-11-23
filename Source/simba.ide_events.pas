unit simba.ide_events;

{$i simba.inc}

interface

uses
  Classes, SysUtils, LazMethodList,
  simba.mufasatypes;

type
  SimbaIDEEvents = class
  private
  type
    EMethodType = (
      CODETOOLS_SETUP,
      TAB_CARETMOVED, TAB_MODIFIED, TAB_LOADED, TAB_SEARCH, TAB_BEFORECHANGE, TAB_CHANGE, TAB_CLOSED,
      MOUSELOGGER_CHANGE,
      SCRIPTSTATE_CHANGE,
      FUNCTIONLIST_NODE_SELECTION,
      ACTIVE_SCRIPTSTATE_CHANGE
    );
  class var
    FMethodLists: array[EMethodType] of TMethodList;

    class procedure Call(Typ: EMethodType; Sender: TObject);
    class procedure RegisterMethod(Typ: EMethodType; Proc: TNotifyEvent);
    class procedure UnRegisterMethod(Typ: EMethodType; Proc: TNotifyEvent);
  public
    class constructor Create;
    class destructor Destroy;

    // Sender = TSimbaScriptTab
    class procedure RegisterOnTabClosed(Proc: TNotifyEvent);
    class procedure CallOnScriptTabClose(Sender: TObject);

    // When a function list node is selected.
    // Sender=TSimbaFunctionListNode
    class procedure RegisterOnFunctionListNodeSelection(Proc: TNotifyEvent);
    class procedure CallOnFunctionListNodeSelection(Sender: TObject);

    // When the script state of the active tab changes.
    // Sender=TSimbaScriptTab
    class procedure RegisterActiveScriptStateChange(Proc: TNotifyEvent);
    class procedure CallOnActiveScriptStateChange(Sender: TObject);

    class procedure CallOnScriptStateChange(Sender: TObject); // Sender = TSimbaScriptTab
    class procedure CallOnBeforeTabChange(Sender: TObject);
    class procedure CallOnScriptTabChange(Sender: TObject); // Sender = TSimbaScriptTab
    class procedure CallOnEditorLoadedMethods(Sender: TObject);
    class procedure CallOnEditorSearchMethods(Sender: TObject);
    class procedure CallOnEditorCaretMoved(Sender: TObject);
    class procedure CallOnEditorModified(Sender: TObject);
    class procedure CallOnMouseLoggerChange(Sender: TObject);

    class procedure RegisterMethodOnScriptStateChange(Proc: TNotifyEvent);
    class procedure RegisterMethodOnScriptTabChange(Proc: TNotifyEvent);
    class procedure RegisterOnBeforeTabChange(Proc: TNotifyEvent);

    class procedure RegisterMethodOnMouseLoggerChange(Proc: TNotifyEvent);
    class procedure UnRegisterMethodOnMouseLoggerChange(Proc: TNotifyEvent);

    class procedure UnRegisterMethodOnEditorLoaded(Proc: TNotifyEvent);
    class procedure UnRegisterMethodOnEditorSearch(Proc: TNotifyEvent);

    class procedure RegisterMethodOnEditorCaretMoved(Proc: TNotifyEvent); // Sender = TSimbaEditor
    class procedure RegisterMethodOnEditorModified(Proc: TNotifyEvent); // Sender = TSimbaEditor

    class procedure RegisterMethodOnEditorLoaded(Proc: TNotifyEvent); // Sender = TSimbaScriptTab
    class procedure RegisterMethodOnEditorSearch(Proc: TNotifyEvent); // Sender = TSimbaEditorFind

    class procedure RegisterOnCodetoolsSetup(Proc: TNotifyEvent);
    class procedure UnRegisterOnCodetoolsSetup(Proc: TNotifyEvent);
    class procedure CallOnCodetoolsSetup(Sender: TObject);
  end;

implementation

class procedure SimbaIDEEvents.Call(Typ: EMethodType; Sender: TObject);
begin
  FMethodLists[Typ].CallNotifyEvents(Sender);
end;

class procedure SimbaIDEEvents.RegisterMethod(Typ: EMethodType; Proc: TNotifyEvent);
begin
  FMethodLists[Typ].Add(TMethod(Proc));
end;

class procedure SimbaIDEEvents.UnRegisterMethod(Typ: EMethodType; Proc: TNotifyEvent);
begin
  FMethodLists[Typ].Remove(TMethod(Proc));
end;

class constructor SimbaIDEEvents.Create;
var
  Typ: EMethodType;
begin
  for Typ in EMethodType do
    FMethodLists[Typ] := TMethodList.Create();
end;

class destructor SimbaIDEEvents.Destroy;
var
  Typ: EMethodType;
begin
  for Typ in EMethodType do
    if (FMethodLists[Typ] <> nil) then
      FreeAndNil(FMethodLists[Typ]);
end;

class procedure SimbaIDEEvents.RegisterOnTabClosed(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_CLOSED, Proc);
end;

class procedure SimbaIDEEvents.CallOnScriptTabClose(Sender: TObject);
begin
  Call(TAB_CLOSED, Sender);
end;

class procedure SimbaIDEEvents.RegisterOnFunctionListNodeSelection(Proc: TNotifyEvent);
begin
  RegisterMethod(FUNCTIONLIST_NODE_SELECTION, Proc);
end;

class procedure SimbaIDEEvents.CallOnFunctionListNodeSelection(Sender: TObject);
begin
  Call(FUNCTIONLIST_NODE_SELECTION, Sender);
end;

class procedure SimbaIDEEvents.RegisterActiveScriptStateChange(Proc: TNotifyEvent);
begin
  RegisterMethod(ACTIVE_SCRIPTSTATE_CHANGE, Proc);
end;

class procedure SimbaIDEEvents.CallOnActiveScriptStateChange(Sender: TObject);
begin
  Call(ACTIVE_SCRIPTSTATE_CHANGE, Sender);
end;

class procedure SimbaIDEEvents.CallOnScriptStateChange(Sender: TObject);
begin
  Call(SCRIPTSTATE_CHANGE, Sender);
end;

class procedure SimbaIDEEvents.CallOnBeforeTabChange(Sender: TObject);
begin
  Call(TAB_BEFORECHANGE, Sender);
end;

class procedure SimbaIDEEvents.CallOnScriptTabChange(Sender: TObject);
begin
  Call(TAB_CHANGE, Sender);
end;

class procedure SimbaIDEEvents.CallOnEditorLoadedMethods(Sender: TObject);
begin
  Call(TAB_LOADED, Sender);
end;

class procedure SimbaIDEEvents.CallOnEditorSearchMethods(Sender: TObject);
begin
  Call(TAB_SEARCH, Sender);
end;

class procedure SimbaIDEEvents.CallOnEditorCaretMoved(Sender: TObject);
begin
  Call(TAB_CARETMOVED, Sender);
end;

class procedure SimbaIDEEvents.CallOnEditorModified(Sender: TObject);
begin
  Call(TAB_MODIFIED, Sender);
end;

class procedure SimbaIDEEvents.CallOnMouseLoggerChange(Sender: TObject);
begin
  Call(MOUSELOGGER_CHANGE, Sender);
end;

class procedure SimbaIDEEvents.RegisterMethodOnScriptStateChange(Proc: TNotifyEvent);
begin
  RegisterMethod(SCRIPTSTATE_CHANGE, Proc);
end;

class procedure SimbaIDEEvents.RegisterMethodOnScriptTabChange(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_CHANGE, Proc);
end;

class procedure SimbaIDEEvents.RegisterOnBeforeTabChange(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_BEFORECHANGE, Proc);
end;

class procedure SimbaIDEEvents.RegisterMethodOnMouseLoggerChange(Proc: TNotifyEvent);
begin
  RegisterMethod(MOUSELOGGER_CHANGE, Proc);
end;

class procedure SimbaIDEEvents.UnRegisterMethodOnMouseLoggerChange(Proc: TNotifyEvent);
begin
  UnRegisterMethod(MOUSELOGGER_CHANGE, Proc);
end;

class procedure SimbaIDEEvents.UnRegisterMethodOnEditorLoaded(Proc: TNotifyEvent);
begin
  UnRegisterMethod(TAB_LOADED, Proc);
end;

class procedure SimbaIDEEvents.UnRegisterMethodOnEditorSearch(Proc: TNotifyEvent);
begin
  UnRegisterMethod(TAB_SEARCH, Proc);
end;

class procedure SimbaIDEEvents.RegisterMethodOnEditorCaretMoved(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_CARETMOVED, Proc);
end;

class procedure SimbaIDEEvents.RegisterMethodOnEditorModified(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_MODIFIED, Proc);
end;

class procedure SimbaIDEEvents.RegisterMethodOnEditorLoaded(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_LOADED, Proc);
end;

class procedure SimbaIDEEvents.RegisterMethodOnEditorSearch(Proc: TNotifyEvent);
begin
  RegisterMethod(TAB_SEARCH, Proc);
end;

class procedure SimbaIDEEvents.RegisterOnCodetoolsSetup(Proc: TNotifyEvent);
begin
  RegisterMethod(CODETOOLS_SETUP, Proc);
end;

class procedure SimbaIDEEvents.UnRegisterOnCodetoolsSetup(Proc: TNotifyEvent);
begin
  UnRegisterMethod(CODETOOLS_SETUP, Proc);
end;

class procedure SimbaIDEEvents.CallOnCodetoolsSetup(Sender: TObject);
begin
  Call(CODETOOLS_SETUP, Sender);
end;

end.


