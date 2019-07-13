{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

           Input/Output manager for Mufasa Macro Library
}

{$mode objfpc}{$H+}

unit simba.target;

interface

uses
  classes, sysutils, dynlibs,
  mufasatypes, bitmaps, libloader, simba.oswindow, simba.eventhandlerlist;

type
  PTarget = ^TTarget;
  TTarget = class(TObject)
  protected
    FInvalidTargetHandlers: TNotifyEventHandlers;

    FMouseClientAreaSet: Boolean;
    FMouseClientArea: TBox;
    FImageClientAreaSet: Boolean;
    FImageClientArea: TBox;

    procedure InvalidTarget;

    function GetHandle: PtrUInt; virtual;
    procedure SetHandle(Value: PtrUInt); virtual;

    function GetAutoFocus: Boolean; virtual;
    procedure SetAutoFocus(Value: Boolean); virtual;
  public
    // Client area
    function MouseSetClientArea(X1, Y1, X2, Y2: Int32): Boolean; virtual;
    procedure MouseResetClientArea; virtual;
    function ImageSetClientArea(X1, Y1, X2, Y2: Int32): Boolean; virtual;
    procedure ImageResetClientArea; virtual;

    // Position, Dimensions
    procedure GetTargetDimensions(out Width, Height: Int32); virtual;
    procedure GetTargetPosition(out Left, Top: Int32); virtual;

    // Colors
    function GetColor(X, Y: Int32): Int32; virtual;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; virtual;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; virtual;
    procedure FreeReturnData; virtual;

    // Mouse
    procedure GetMousePosition(out x,y: Int32); virtual;
    procedure MoveMouse(X, Y: Int32); virtual;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); virtual;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); virtual;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); virtual;
    function IsMouseButtonHeld(Button: TClickType): Boolean; virtual;

    // Keyboard
    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); virtual;
    procedure HoldKey(key: Int32); virtual;
    procedure ReleaseKey(key: Int32); virtual;
    function IsKeyHeld(key: Int32): Boolean; virtual;
    function GetKeyCode(Character: Char) : Int32; virtual;

    // Activate
    procedure ActivateClient; virtual;

    // Valid
    function TargetValid: Boolean; virtual;

    // Events
    function AddHandlerInvalidTarget(Handler: TNotifyEvent): Int32; virtual;
    procedure RemoveHandlerInvalidTarget(Index: Int32); virtual;

    property Handle: PtrUInt read GetHandle write SetHandle;
    property AutoFocus: Boolean read GetAutoFocus write SetAutoFocus;

    constructor Create;
    destructor Destroy; override;
  end;

  PRawTarget = ^TRawTarget;
  TRawTarget = class(TTarget)
  protected
    FData: PRGB32;
    FManageData: Boolean;
    FWidth, FHeight: Int32;
  public
    procedure GetTargetDimensions(out Width, Height: Int32); override;

    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;

    constructor Create(Data: PRGB32; Width, Height: Int32; Copy: Boolean = False);
    destructor Destroy; override;
 end;

  PBitmapTarget = ^TBitmapTarget;
  TBitmapTarget = class(TTarget)
  protected
    FBitmap: TMufasaBitmap;
  public
    procedure GetTargetDimensions(out Width, Height: Int32); override;

    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;

    constructor Create(Bitmap: TMufasaBitmap);
    destructor Destroy; override;
  end;

  PWindowTarget = ^TWindowTarget;
  TWindowTarget = class(TTarget)
  protected
    FWindow: TOSWindow;
    FAutoFocus: Boolean;
  public
    procedure GetTargetDimensions(out W, H: Int32); override; abstract;
    procedure GetTargetPosition(out Left, Top: Int32); override; abstract;
    procedure GetTargetBounds(out Bounds: TBox); virtual; abstract;

    function CopyData(X, Y, Width, Height: Int32): PRGB32; override; abstract;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override; abstract;

    function TargetValid: Boolean; override; abstract;

    procedure ActivateClient; override; abstract;
    procedure GetMousePosition(out X, Y: Int32); override; abstract;
    procedure MoveMouse(X, Y: Int32); override; abstract;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); override; abstract;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override; abstract;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override; abstract;
    function IsMouseButtonHeld(Button: TClickType): Boolean;override; abstract;

    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); override; abstract;
    procedure HoldKey(Key: Int32); override; abstract;
    procedure ReleaseKey(Key: Int32); override; abstract;
    function IsKeyHeld(Key: Int32): Boolean; override; abstract;
    function GetKeyCode(Character: Char): Int32; override; abstract;
  end;

  PEIOS_Client = ^TEIOS_Client;
  TEIOS_Client = record
    RequestTarget: function(Data: PChar): Pointer; stdcall;
    ReleaseTarget: procedure(Target: Pointer); stdcall;

    GetTargetDimensions: procedure(Target: Pointer; var Width, Height: Int32); stdcall;
    GetTargetPosition: procedure(var Left, Top: Int32); stdcall;
    GetImageBuffer: function(Target: Pointer): PRGB32; stdcall;
    UpdateImageBuffer: procedure(Target: Pointer); stdcall;
    UpdateImageBufferEx: function(Target: Pointer): PRGB32; stdcall;
    UpdateImageBufferBounds: procedure(Target: Pointer; X1, Y1, X2, Y2: Int32); stdcall;

    GetMousePosition: procedure(Target: Pointer; var X, Y: Int32); stdcall;
    MoveMouse: procedure(Target: Pointer; X, Y: Int32); stdcall;
    ScrollMouse: procedure(target : pointer; X, Y: Int32; Lines: Int32); stdcall;
    HoldMouse: procedure(Target: Pointer; X, Y: Int32; Button: Int32); stdcall;
    ReleaseMouse: procedure(Target: Pointer; X, Y: Int32; Button: Int32); stdcall;
    IsMouseButtonHeld: function(Target: Pointer; Button: Int32): Boolean; stdcall;

    SendString: procedure(Target: Pointer; str: PChar; KeyWait, KeyModWait: Int32); stdcall;
    HoldKey: procedure(Target: Pointer; Key: Int32); stdcall;
    ReleaseKey: procedure(Target: Pointer; Key: Int32); stdcall;
    IsKeyHeld: function(Target: Pointer; Key: Int32): Boolean; stdcall;
    GetKeyCode :function(Target: Pointer; Character: Char): Int32; stdcall;
  end;

  PEIOS_Controller = ^TEIOS_Client;
  TEIOS_Controller = class(TGenericLoader)
  protected
    Clients: array of TEIOS_Client;

    function InitPlugin(Plugin: TLibHandle): Boolean; override;
    function FindClient(Name: String): Int32;
  public
    function ClientExists(Name: string): Boolean;
    function GetClient(Name: string): TEIOS_Client;
  end;

  PEIOS_Target = ^TEIOS_Target;
  TEIOS_Target = class(TTarget)
  protected
    FClient: TEIOS_Client;
    FTarget: Pointer;
    FBuffer: PRGB32;
    FWidth, FHeight: Int32;

    procedure UpdateBuffer(X, Y, Width, Height: Int32);
  public
    constructor Create(Client: TEIOS_Client; Data: String);
    destructor Destroy; override;

    procedure GetTargetDimensions(out Width, Height: Int32); override;
    procedure GetTargetPosition(out Left, Top: Int32); override;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; override;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; override;

    procedure GetMousePosition(out X, Y: Int32); override;
    procedure MoveMouse(X, Y: Int32); override;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); override;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); override;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); override;
    function  IsMouseButtonHeld(Button: TClickType) : Boolean;override;

    procedure SendString(Text: string; KeyWait, KeyModWait: Int32); override;
    procedure HoldKey(Key: Int32); override;
    procedure ReleaseKey(Key: Int32); override;
    function IsKeyHeld(Key: Int32): Boolean; override;
    function GetKeyCode(Character: Char) : Int32; override;
    function GetHandle: PtrUInt; override;
  end;

  PTarget_Exported = ^TTarget_Exported;
  TTarget_Exported = record
    Target: Pointer;

    GetTargetDimensions: procedure(Target: Pointer; var Width, Height: Int32); stdcall;
    GetTargetPosition: procedure(Target: Pointer; var Top, Left: Int32); stdcall;
    GetColor: function(Target: Pointer; X, Y: Int32) : Int32; stdcall;
    ReturnData: function(Target: Pointer; X, Y, Width, Height: Int32): TRetData; stdcall;
    FreeReturnData: procedure(Target: Pointer); stdcall;

    GetMousePosition: procedure(Target: Pointer; var X, Y: Int32); stdcall;
    MoveMouse: procedure(Target: Pointer; X, Y: Int32); stdcall;
    ScrollMouse: procedure(Target: Pointer; X, Y: Int32; Lines : Int32); stdcall;
    HoldMouse: procedure(Target: Pointer; X, Y: Int32; Left: Boolean); stdcall;
    ReleaseMouse: procedure(Target: Pointer; X, Y: Int32; left: Boolean); stdcall;
    IsMouseButtonHeld: function(Target: Pointer; Left : Boolean) : Boolean;stdcall;

    SendString: procedure(Target: Pointer; Text: PChar; KeyWait, KeyModWait: Int32); stdcall;
    HoldKey: procedure(Target: Pointer; key: Int32); stdcall;
    ReleaseKey: procedure(Target: Pointer; key: Int32); stdcall;
    IsKeyHeld: function(Target: Pointer; key: Int32): Boolean; stdcall;
    GetKeyCode: function(target : pointer; Character: Char): Int32; stdcall;
    CopyData: function(Target: Pointer; X, Y, Width, Height: Int32): PRGB32; stdcall;
  end;

  procedure TTarget_Exported_GetTargetDimensions(Target: Pointer; var Width, Height: Int32); stdcall;
  procedure TTarget_Exported_GetTargetPosition(Target: Pointer; var Left, Top: Int32); stdcall;
  function TTarget_Exported_GetColor(Target: Pointer; X, Y: Int32): Int32; stdcall;
  function TTarget_Exported_ReturnData(Target: Pointer; X, Y, Width, Height: Int32): TRetData; stdcall;
  procedure TTarget_Exported_FreeReturnData(Target: Pointer); stdcall;

  procedure TTarget_Exported_GetMousePosition(Target: Pointer; var X, Y: Int32); stdcall;
  procedure TTarget_Exported_MoveMouse(Target: Pointer; X, Y: Int32); stdcall;
  procedure TTarget_Exported_ScrollMouse(Target: Pointer; X, Y : Int32; Lines : Int32); stdcall;
  procedure TTarget_Exported_HoldMouse(Target: Pointer; X, Y: Int32; Left: Boolean); stdcall;
  procedure TTarget_Exported_ReleaseMouse(Target: Pointer; X, Y: Int32; Left: Boolean); stdcall;
  function TTarget_Exported_IsMouseButtonHeld(Target: Pointer; Left : Boolean): Boolean; stdcall;

  procedure TTarget_Exported_SendString(Target: Pointer; Text: PChar; KeyWait, KeyModWait: Int32); stdcall;
  procedure TTarget_Exported_HoldKey(Target: Pointer; Key: Int32); stdcall;
  procedure TTarget_Exported_ReleaseKey(Target: Pointer; Key: Int32); stdcall;
  function TTarget_Exported_IsKeyHeld(Target: Pointer; Key: Int32): Boolean; stdcall;
  function TTarget_Exported_GetKeyCode(Target : Pointer; Character: Char) : Int32; stdcall;
  function TTarget_Exported_CopyData(Target: Pointer; X, Y, Width, Height: Int32): PRGB32; stdcall;

var
  EIOSController: TEIOS_Controller;

implementation

uses
  colour_conv;

{$I target_raw.inc}
{$I target_bitmap.inc}
{$I target_eios.inc}
{$I target_exported.inc}

procedure TTarget.InvalidTarget;
begin
  FInvalidTargetHandlers.Call(Self);
end;

procedure TTarget.GetTargetPosition(out Left, Top: Int32);
begin
  raise Exception.Create('GetTargetPosition not available for this target');
end;

function TTarget.GetColor(X, Y: Int32): Int32;
begin
  with ReturnData(X + FImageClientArea.X1, Y + FImageClientArea.Y1, 1, 1) do
    Result := RGBToColor(Ptr[0].R, Ptr[0].G, Ptr[0].B);

  FreeReturnData();
end;

function TTarget.CopyData(X, Y, Width, Height: Int32): PRGB32;
begin
  raise Exception.Create('CopyData not availble for this target');
end;

function TTarget.ReturnData(X, Y, Width, Height: Int32): TRetData;
begin
  raise Exception.Create('ReturnData not available for this target');
end;

procedure TTarget.FreeReturnData;
begin
  {do nothing by default}
end;

procedure TTarget.ActivateClient;
begin
  raise Exception.Create('ActivateClient not available for this target');
end;

function TTarget.TargetValid: Boolean;
begin
  Result := True;
end;

procedure TTarget.GetTargetDimensions(out Width, Height: Int32);
begin
  raise Exception.Create('GetTargetDimensions not available for this target');
end;

function TTarget.MouseSetClientArea(X1, Y1, X2, Y2: Int32): Boolean;
begin
  Result := True;

  FMouseClientAreaSet := True;

  FMouseClientArea.X1 := X1;
  FMouseClientArea.Y1 := Y1;
  FMouseClientArea.X2 := X2;
  FMouseClientArea.Y2 := Y2;
end;

procedure TTarget.MouseResetClientArea;
begin
  FMouseClientAreaSet := False;

  FMouseClientArea.X1 := 0;
  FMouseClientArea.Y1 := 0;
  FMouseClientArea.X2 := 0;
  FMouseClientArea.Y2 := 0;
end;

function TTarget.ImageSetClientArea(X1, Y1, X2, Y2: Int32): Boolean;
begin
  Result := True;

  FImageClientAreaSet := True;

  FImageClientArea.X1 := X1;
  FImageClientArea.Y1 := Y1;
  FImageClientArea.X2 := X2;
  FImageClientArea.Y2 := Y2;
end;

procedure TTarget.ImageResetClientArea;
begin
  FImageClientAreaSet := False;

  FImageClientArea.X1 := 0;
  FImageClientArea.Y1 := 0;
  FImageClientArea.X2 := 0;
  FImageClientArea.Y2 := 0;
end;

procedure TTarget.GetMousePosition(out x, y: Int32);
begin
  raise Exception.Create('GetMousePosition not available for this target');
end;

procedure TTarget.MoveMouse(X, Y: Int32);
begin
  raise Exception.Create('MoveMouse not available for this target');
end;

procedure TTarget.ScrollMouse(X, Y: Int32; Lines: Int32);
begin
  raise Exception.Create('ScrollMouse is not available for this target');
end;

procedure TTarget.HoldMouse(X, Y: Int32; Button: TClickType);
begin
  raise Exception.Create('HoldMouse not available for this target');
end;

procedure TTarget.ReleaseMouse(X, Y: Int32; Button: TClickType);
begin
  raise Exception.Create('ReleaseMouse not available for this target');
end;

function TTarget.IsMouseButtonHeld(Button: TClickType): Boolean;
begin
  raise Exception.Create('IsMouseButtonHeld not available for this target');
end;

procedure TTarget.SendString(Text: String; KeyWait, KeyModWait: Int32);
begin
  raise Exception.Create('SendString not available for this target');
end;

procedure TTarget.HoldKey(key: Int32);
begin
  raise Exception.Create('HoldKey not available for this target');
end;

procedure TTarget.ReleaseKey(key: Int32);
begin
  raise Exception.Create('ReleaseKey not available for this target');
end;

function TTarget.IsKeyHeld(key: Int32): Boolean;
begin
  raise Exception.Create('IsKeyHeld not available for this target');
end;

function TTarget.GetKeyCode(Character: Char): Int32;
begin
  raise Exception.Create('GetKeyCode is not available for this target');
end;

function TTarget.GetHandle: PtrUInt;
begin
  raise Exception.Create('GetHandle is not available for this target');
end;

procedure TTarget.SetHandle(Value: PtrUInt);
begin
  raise Exception.Create('SetHandle is not available for this target');
end;

function TTarget.GetAutoFocus: Boolean;
begin
  raise Exception.Create('GetAutoFocus is not available for this target');
end;

procedure TTarget.SetAutoFocus(Value: Boolean);
begin
  raise Exception.Create('SetAutoFocus is not available for this target');
end;

function TTarget.AddHandlerInvalidTarget(Handler: TNotifyEvent): Int32;
begin
  Result := FInvalidTargetHandlers.Add(Handler);
end;

procedure TTarget.RemoveHandlerInvalidTarget(Index: Int32);
begin
  FInvalidTargetHandlers.Remove(Index);
end;

constructor TTarget.Create;
begin
  FInvalidTargetHandlers := TNotifyEventHandlers.Create();
end;

destructor TTarget.Destroy;
begin
  FInvalidTargetHandlers.Free();

  inherited Destroy();
end;

initialization
  EIOSController := TEIOS_Controller.Create();

finalization
  EIOSController.Free();

end.

