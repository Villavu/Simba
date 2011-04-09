unit keybinder;

{
 libKeybinder.

 https://github.com/engla/keybinder & http://kaizer.se/wiki/keybinder/

 GPL 2
}

{$mode objfpc}

interface

uses
  ctypes, glib2;


const libKeybinder = 'keybinder';

type TKeyBinderHandler = procedure (keystring: PChar; user_data: PtrUInt); cdecl;

procedure keybinder_init; cdecl; external libKeybinder;

function keybinder_bind (keystring: PChar; handler: TKeyBinderHandler; user_data: PtrUInt): gboolean; cdecl; external libKeybinder;
procedure keybinder_unbind (keystring: PChar; handler: TKeyBinderHandler; user_data: PtrUInt); cdecl; external libKeybinder;

function keybinder_get_current_event_time(): guint32;  cdecl; external libKeybinder;

implementation

end.

