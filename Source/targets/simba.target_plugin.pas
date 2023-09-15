unit simba.target_plugin;

{$i simba.inc}

interface

uses
  Classes, SysUtils, DynLibs,
  simba.mufasatypes, simba.externalimage;

type
  PSimbaPluginTarget = ^TSimbaPluginTarget;
  TSimbaPluginTarget = record
    Lib: TLibHandle;
    FileName: String;
    Target: Pointer;

    Request: function(Args: PChar): Pointer; cdecl;
    RequestWithDebugImage: function(Args: PChar; out DebugImage: TSimbaExternalImage): Pointer; cdecl;
    Release: procedure(Target: Pointer); cdecl;

    GetDimensions: procedure(Target: Pointer; out W, H: Int32); cdecl;
    GetImageData: function(Target: Pointer; X, Y, Width, Height: Int32; var Data: PColorBGRA; var DataWidth: Int32): Boolean; cdecl;

    MousePressed: function(Target: Pointer; Button: Int32): Boolean; cdecl;
    MousePosition: function(Target: Pointer): TPoint; cdecl;
    MouseTeleport: procedure(Target: Pointer; P: TPoint); cdecl;
    MouseUp: procedure(Target: Pointer; Button: Int32); cdecl;
    MouseDown: procedure(Target: Pointer; Button: Int32); cdecl;
    MouseScroll: procedure(Target: Pointer; Scrolls: Int32); cdecl;

    KeyDown: procedure(Target: Pointer; Key: Int32); cdecl;
    KeyUp: procedure(Target: Pointer; Key: Int32); cdecl;
    KeySend: procedure(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Int32); cdecl;
    KeyPressed: function(Target: Pointer; Key: Int32): Boolean; cdecl;
  end;

function LoadPluginTarget(FileName, Args: String): TSimbaPluginTarget; overload;
function LoadPluginTarget(FileName, Args: String; out DebugImage: TSimbaExternalImage): TSimbaPluginTarget; overload;

procedure PluginTarget_GetDimensions(Target: Pointer; out W, H: Integer);
function PluginTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;

function PluginTarget_MousePressed(Target: Pointer; Button: EMouseButton): Boolean;
function PluginTarget_MousePosition(Target: Pointer): TPoint;
procedure PluginTarget_MouseTeleport(Target: Pointer; P: TPoint);
procedure PluginTarget_MouseUp(Target: Pointer; Button: EMouseButton);
procedure PluginTarget_MouseDown(Target: Pointer; Button: EMouseButton);
procedure PluginTarget_MouseScroll(Target: Pointer; Scrolls: Integer);

procedure PluginTarget_KeyDown(Target: Pointer; Key: EKeyCode);
procedure PluginTarget_KeyUp(Target: Pointer; Key: EKeyCode);
procedure PluginTarget_KeySend(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
function PluginTarget_KeyPressed(Target: Pointer; Key: EKeyCode): Boolean;

implementation

uses
  simba.env, simba.files, simba.script_pluginloader;

function Load(AFileName: String): TSimbaPluginTarget;
begin
  Result := Default(TSimbaPluginTarget);

  with Result do
  begin
    try
      Lib := LoadPlugin(AFileName);
    except
      on E: Exception do
        raise Exception.Create('LoadPluginTarget: ' + E.Message);
    end;

    FileName := TSimbaPath.PathExtractRelative(SimbaEnv.SimbaPath, AFileName);

    Pointer(Request)               := GetProcedureAddress(Lib, 'SimbaPluginTarget_Request');
    Pointer(RequestWithDebugImage) := GetProcedureAddress(Lib, 'SimbaPluginTarget_RequestWithDebugImage');
    Pointer(Release)               := GetProcedureAddress(Lib, 'SimbaPluginTarget_Release');

    Pointer(GetDimensions)         := GetProcedureAddress(Lib, 'SimbaPluginTarget_GetDimensions');
    Pointer(GetImageData)          := GetProcedureAddress(Lib, 'SimbaPluginTarget_GetImageData');

    Pointer(MousePressed)          := GetProcedureAddress(Lib, 'SimbaPluginTarget_MousePressed');
    Pointer(MousePosition)         := GetProcedureAddress(Lib, 'SimbaPluginTarget_MousePosition');
    Pointer(MouseTeleport)         := GetProcedureAddress(Lib, 'SimbaPluginTarget_MouseTeleport');
    Pointer(MouseUp)               := GetProcedureAddress(Lib, 'SimbaPluginTarget_MouseUp');
    Pointer(MouseDown)             := GetProcedureAddress(Lib, 'SimbaPluginTarget_MouseDown');
    Pointer(MouseScroll)           := GetProcedureAddress(Lib, 'SimbaPluginTarget_MouseScroll');

    Pointer(KeyDown)               := GetProcedureAddress(Lib, 'SimbaPluginTarget_KeyDown');
    Pointer(KeyUp)                 := GetProcedureAddress(Lib, 'SimbaPluginTarget_KeyUp');
    Pointer(KeySend)               := GetProcedureAddress(Lib, 'SimbaPluginTarget_KeySend');
    Pointer(KeyPressed)            := GetProcedureAddress(Lib, 'SimbaPluginTarget_KeyPressed');
  end;
end;

function LoadPluginTarget(FileName, Args: String): TSimbaPluginTarget;
begin
  Result := Load(FileName);
  Result.Target := Result.Request(PChar(Args));
end;

function LoadPluginTarget(FileName, Args: String; out DebugImage: TSimbaExternalImage): TSimbaPluginTarget;
begin
  Result := Load(FileName);
  Result.Target := Result.RequestWithDebugImage(PChar(Args), DebugImage);
end;

procedure PluginTarget_GetDimensions(Target: Pointer; out W, H: Integer);
begin
  with PSimbaPluginTarget(Target)^ do
    GetDimensions(Target, W, H);
end;

function PluginTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  with PSimbaPluginTarget(Target)^ do
    Result := GetImageData(Target, X, Y, Width, Height, Data, DataWidth);
end;

function PluginTarget_MousePressed(Target: Pointer; Button: EMouseButton): Boolean;
begin
  with PSimbaPluginTarget(Target)^ do
    Result := MousePressed(Target, Integer(Button));
end;

function PluginTarget_MousePosition(Target: Pointer): TPoint;
begin
  with PSimbaPluginTarget(Target)^ do
    Result := MousePosition(Target);
end;

procedure PluginTarget_MouseTeleport(Target: Pointer; P: TPoint);
begin
  with PSimbaPluginTarget(Target)^ do
    MouseTeleport(Target, P);
end;

procedure PluginTarget_MouseUp(Target: Pointer; Button: EMouseButton);
begin
  with PSimbaPluginTarget(Target)^ do
    MouseUp(Target, Integer(Button));
end;

procedure PluginTarget_MouseDown(Target: Pointer; Button: EMouseButton);
begin
  with PSimbaPluginTarget(Target)^ do
    MouseDown(Target, Integer(Button));
end;

procedure PluginTarget_MouseScroll(Target: Pointer; Scrolls: Integer);
begin
  with PSimbaPluginTarget(Target)^ do
    MouseScroll(Target, Scrolls);
end;

procedure PluginTarget_KeyDown(Target: Pointer; Key: EKeyCode);
begin
  with PSimbaPluginTarget(Target)^ do
    KeyDown(Target, Integer(Key));
end;

procedure PluginTarget_KeyUp(Target: Pointer; Key: EKeyCode);
begin
  with PSimbaPluginTarget(Target)^ do
    KeyUp(Target, Integer(Key));
end;

procedure PluginTarget_KeySend(Target: Pointer; Key: Char; KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime: Integer);
begin
  with PSimbaPluginTarget(Target)^ do
    KeySend(Target, Key, KeyDownTime, KeyUpTime, ModifierDownTime, ModifierUpTime);
end;

function PluginTarget_KeyPressed(Target: Pointer; Key: EKeyCode): Boolean;
begin
  with PSimbaPluginTarget(Target)^ do
    Result := KeyPressed(Target, Integer(Key));
end;

end.

