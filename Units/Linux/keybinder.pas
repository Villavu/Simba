{
 libKeybinder.

 https://github.com/engla/keybinder & http://kaizer.se/wiki/keybinder/

 GPL 2
}
unit keybinder;

{$mode objfpc}{$H+}

interface

uses
  ctypes, dynlibs, glib2;

const
  libkeybinder = 'libkeybinder.so.0.1.0';

type
  TKeyBinderHandler = procedure (keystring: PChar; user_data: PtrUInt); cdecl;

  TKeyBinderInit = procedure(); cdecl;
  TKeyBinderBind = function(keystring: PChar; handler: TKeyBinderHandler; user_data: PtrUInt): gboolean; cdecl;
  TKeyBinderUnbind = procedure(keystring: PChar; handler: TKeyBinderHandler; user_data: PtrUInt); cdecl;
  TKeyBinderGetCET = function(): guint32; cdecl;

var
  keybinder_init: TKeyBinderInit = nil;
  keybinder_bind: TKeyBinderBind = nil;
  keybinder_unbind: TKeyBinderUnbind = nil;
  keybinder_get_current_event_time: TKeyBinderGetCET = nil;

  keybinderLibrary: TLibHandle = NilHandle;

function KeybinderLoaded(): Boolean;
procedure AssertKeybinderLoaded();

procedure LoadKeybinder(LibPath: string = ''; LibName: string = libkeybinder);
procedure UnloadKeybinder();

implementation

uses
  SysUtils;

function KeybinderLoaded(): Boolean;
begin
  Result := keybinderLibrary <> NilHandle;
end;

procedure AssertKeybinderLoaded();
begin
  if (not KeybinderLoaded()) then
     raise EAssertionFailed.Create('LibKeybinder not loaded!');
end;

procedure LoadKeybinder(LibPath: string = ''; LibName: string = libkeybinder);
begin
  UnloadKeybinder();
  if (LibPath <> '') then
     LibPath := IncludeTrailingPathDelimiter(LibPath);
  keybinderLibrary := LoadLibrary(LibPath + LibName);

  if KeybinderLoaded() then
  begin
    Pointer(keybinder_init) := GetProcAddress(keybinderLibrary, 'keybinder_init');
    Pointer(keybinder_bind) := GetProcAddress(keybinderLibrary, 'keybinder_bind');
    Pointer(keybinder_unbind) := GetProcAddress(keybinderLibrary, 'keybinder_unbind');
    Pointer(keybinder_get_current_event_time) := GetProcAddress(keybinderLibrary, 'keybinder_get_current_event_time');
  end else
    WriteLn('ERROR: Loading keybinder failed!');
end;

procedure UnloadKeybinder();
begin
  if KeybinderLoaded() then
    if FreeLibrary(keybinderLibrary) then
    begin
      keybinderLibrary := NilHandle;

      Pointer(keybinder_init) := nil;
      Pointer(keybinder_bind) := nil;
      Pointer(keybinder_unbind) := nil;
      Pointer(keybinder_get_current_event_time) := nil;
    end;
end;

initialization
  LoadKeybinder();
finalization
  UnloadKeybinder();
end.

