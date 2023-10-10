{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simba plugin target interface.
}
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
    KeySend: procedure(Target: Pointer; Text: PChar; TextLen: Int32; SleepTimes: PInt32); cdecl;
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
procedure PluginTarget_KeySend(Target: Pointer; Text: PChar; TextLen: Int32; SleepTimes: PInt32);
function PluginTarget_KeyPressed(Target: Pointer; Key: EKeyCode): Boolean;

function PluginTarget_IsValid(Target: Pointer): Boolean;

implementation

uses
  simba.env, simba.files, simba.script_pluginloader;

procedure CheckExported(const MethodName: String; const Method: Pointer); inline;
begin
  if (Method = nil) then
    SimbaException('SimbaPluginTarget: "' + MethodName + '" is not exported');
end;

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
  with Result do
  begin
    CheckExported('SimbaPluginTarget_Request', Request);

    Result.Target := Result.Request(PChar(Args));
  end;
end;

function LoadPluginTarget(FileName, Args: String; out DebugImage: TSimbaExternalImage): TSimbaPluginTarget;
begin
  Result := Load(FileName);
  with Result do
  begin
    CheckExported('SimbaPluginTarget_RequestWithDebugImage', RequestWithDebugImage);

    Target := Result.RequestWithDebugImage(PChar(Args), DebugImage);
  end;
end;

procedure PluginTarget_GetDimensions(Target: Pointer; out W, H: Integer);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_GetDimensions', GetDimensions);

    GetDimensions(Target, W, H);
  end;
end;

function PluginTarget_GetImageData(Target: Pointer; X, Y, Width, Height: Integer; var Data: PColorBGRA; var DataWidth: Integer): Boolean;
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_GetImageData', GetImageData);

    Result := GetImageData(Target, X, Y, Width, Height, Data, DataWidth);
  end;
end;

function PluginTarget_MousePressed(Target: Pointer; Button: EMouseButton): Boolean;
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_MousePressed', MousePressed);

    Result := MousePressed(Target, Integer(Button));
  end;
end;

function PluginTarget_MousePosition(Target: Pointer): TPoint;
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_MousePosition', MousePosition);

    Result := MousePosition(Target);
  end;
end;

procedure PluginTarget_MouseTeleport(Target: Pointer; P: TPoint);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_MouseTeleport', MouseTeleport);

    MouseTeleport(Target, P);
  end;
end;

procedure PluginTarget_MouseUp(Target: Pointer; Button: EMouseButton);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_MouseUp', MouseUp);

    MouseUp(Target, Integer(Button));
  end;
end;

procedure PluginTarget_MouseDown(Target: Pointer; Button: EMouseButton);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_MouseDown', MouseDown);

    MouseDown(Target, Integer(Button));
  end;
end;

procedure PluginTarget_MouseScroll(Target: Pointer; Scrolls: Integer);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_MouseScroll', MouseScroll);

    MouseScroll(Target, Scrolls);
  end;
end;

procedure PluginTarget_KeyDown(Target: Pointer; Key: EKeyCode);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_KeyDown', KeyDown);

    KeyDown(Target, Integer(Key));
  end;
end;

procedure PluginTarget_KeyUp(Target: Pointer; Key: EKeyCode);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_KeyUp', KeyUp);

    KeyUp(Target, Integer(Key));
  end;
end;

procedure PluginTarget_KeySend(Target: Pointer; Text: PChar; TextLen: Int32; SleepTimes: PInt32);
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_KeySend', KeySend);

    KeySend(Target, Text, TextLen, SleepTimes);
  end;
end;

function PluginTarget_KeyPressed(Target: Pointer; Key: EKeyCode): Boolean;
begin
  with PSimbaPluginTarget(Target)^ do
  begin
    CheckExported('SimbaPluginTarget_KeyPressed', KeyPressed);

    Result := KeyPressed(Target, Integer(Key));
  end;
end;

function PluginTarget_IsValid(Target: Pointer): Boolean;
begin
  Result := True;
end;

end.

