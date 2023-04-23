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
    EMethodType = (TAB_CARETMOVED, TAB_MODIFIED, TAB_LOADED, TAB_SEARCH);
  class var
    FMethodLists: array[EMethodType] of TMethodList;

    class procedure Call(Typ: EMethodType; Sender: TObject);
    class procedure RegisterMethod(Typ: EMethodType; Proc: TNotifyEvent);
    class procedure UnRegisterMethod(Typ: EMethodType; Proc: TNotifyEvent);
  public
    class constructor Create;
    class destructor Destroy;

    class procedure CallOnEditorLoadedMethods(Sender: TObject);
    class procedure CallOnEditorSearchMethods(Sender: TObject);
    class procedure CallOnEditorCaretMoved(Sender: TObject);
    class procedure CallOnEditorModified(Sender: TObject);

    class procedure UnRegisterMethodOnEditorLoaded(Proc: TNotifyEvent);
    class procedure UnRegisterMethodOnEditorSearch(Proc: TNotifyEvent);

    class procedure RegisterMethodOnEditorCaretMoved(Proc: TNotifyEvent); // Sender = TSimbaEditor
    class procedure RegisterMethodOnEditorModified(Proc: TNotifyEvent); // Sender = TSimbaEditor

    class procedure RegisterMethodOnEditorLoaded(Proc: TNotifyEvent); // Sender = TSimbaScriptTab
    class procedure RegisterMethodOnEditorSearch(Proc: TNotifyEvent); // Sender = TSimbaEditorFind
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

end.

