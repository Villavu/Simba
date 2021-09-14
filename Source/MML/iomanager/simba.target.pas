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
  classes, sysutils,
  simba.mufasatypes, simba.eventhandlerlist, simba.type_matrix;

type
  PTarget = ^TTarget;
  TTarget = class(TObject)
  protected
    FInvalidTargetHandlers: TNotifyEventHandlers;

    FMouseClientAreaSet: Boolean;
    FMouseClientArea: TBox;
    FImageClientAreaSet: Boolean;
    FImageClientArea: TBox;

    procedure InvalidTarget; virtual;

    function GetHandle: PtrUInt; virtual;
    procedure SetHandle(Value: PtrUInt); virtual;

    function GetAutoFocus: Boolean; virtual;
    procedure SetAutoFocus(Value: Boolean); virtual;

    procedure GetTargetBounds(out Bounds: TBox); virtual; // raw bounds. (no image client area etc)
  public
    // Client area
    procedure MouseClientAreaOffset(var X, Y: Int32); virtual;
    function MouseSetClientArea(X1, Y1, X2, Y2: Int32): Boolean; virtual;
    procedure MouseResetClientArea; virtual;
    procedure ImageClientAreaOffset(var X, Y: Int32); virtual;
    function ImageSetClientArea(X1, Y1, X2, Y2: Int32): Boolean; virtual;
    procedure ImageResetClientArea; virtual;

    // Position, Dimensions
    procedure GetTargetDimensions(out Width, Height: Int32); virtual;
    procedure GetTargetPosition(out Left, Top: Int32); virtual;

    // Colors
    function GetColor(X, Y: Int32): Int32; virtual;
    function CopyData(X, Y, Width, Height: Int32): PRGB32; virtual;
    function ReturnData(X, Y, Width, Height: Int32): TRetData; virtual;
    function ReturnMatrix(X, Y, Width, Height: Int32): TIntegerMatrix; virtual;
    procedure FreeReturnData; virtual;

    // Mouse
    procedure GetMousePosition(out X, Y: Int32); virtual;
    procedure MoveMouse(X, Y: Int32); virtual;
    procedure ScrollMouse(X, Y: Int32; Lines: Int32); virtual;
    procedure HoldMouse(X, Y: Int32; Button: TClickType); virtual;
    procedure ReleaseMouse(X, Y: Int32; Button: TClickType); virtual;
    function IsMouseButtonHeld(Button: TClickType): Boolean; virtual;

    // Keyboard
    procedure SendString(Text: String; KeyWait, KeyModWait: Int32); virtual;
    procedure HoldKey(Key: Int32); virtual;
    procedure ReleaseKey(Key: Int32); virtual;
    function IsKeyHeld(Key: Int32): Boolean; virtual;
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

implementation

uses
  simba.colormath;

procedure TTarget.InvalidTarget;
begin
  FInvalidTargetHandlers.Call(Self);
end;

procedure TTarget.GetTargetPosition(out Left, Top: Int32);
var
  Bounds: TBox;
begin
  if FImageClientAreaSet then
  begin
    Left := FImageClientArea.X1;
    Top := FImageClientArea.Y1;
  end else
  begin
    GetTargetBounds(Bounds);

    Left := Bounds.X1;
    Top := Bounds.Y1;
  end;
end;

procedure TTarget.GetTargetDimensions(out Width, Height: Int32);
var
  Bounds: TBox;
begin
  if FImageClientAreaSet then
  begin
    Width := FImageClientArea.Width;
    Height := FImageClientArea.Height;
  end else
  begin
    GetTargetBounds(Bounds);

    Width := Bounds.Width - 1;
    Height := Bounds.Height - 1;
  end;
end;

procedure TTarget.GetTargetBounds(out Bounds: TBox);
begin
  raise Exception.Create('GetTargetBounds not available for this target');
end;

function TTarget.GetColor(X, Y: Int32): Int32;
begin
  ImageClientAreaOffset(X, Y);
  with ReturnData(X, Y, 1, 1) do
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

function TTarget.ReturnMatrix(X, Y, Width, Height: Int32): TIntegerMatrix;
var
  Data: TRetData;
  Dest: PInt32;
  Source: PRGB32;
  SourceInc: Int32;
  LoopX, LoopY: Int32;
begin
  SetLength(Result, Height, Width);

  Data := ReturnData(X, Y, Width, Height);

  Source := Data.Ptr;
  SourceInc := Data.IncPtrWith;

  if (Data <> NullReturnData) then
  begin
    for LoopY := 0 to Height - 1 do
    begin
      Dest := @Result[LoopY][0];

      for LoopX := 0 to Width - 1 do
      begin
        Dest^ := BGRToRGB(Source^);

        Inc(Dest);
        Inc(Source);
      end;

      Inc(Source, SourceInc);
    end;

    FreeReturnData();
  end;
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

procedure TTarget.ImageClientAreaOffset(var X, Y: Int32);
begin
  if FImageClientAreaSet then
  begin
    X := X + FImageClientArea.X1;
    Y := Y + FImageClientArea.Y1;
  end;
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

procedure TTarget.GetMousePosition(out X, Y: Int32);
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

procedure TTarget.HoldKey(Key: Int32);
begin
  raise Exception.Create('HoldKey not available for this target');
end;

procedure TTarget.ReleaseKey(Key: Int32);
begin
  raise Exception.Create('ReleaseKey not available for this target');
end;

function TTarget.IsKeyHeld(Key: Int32): Boolean;
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
  raise Exception.Create('SetHandle is not available for this target: ' +Self.ClassName);
end;

function TTarget.GetAutoFocus: Boolean;
begin
  raise Exception.Create('GetAutoFocus is not available for this target');
end;

procedure TTarget.SetAutoFocus(Value: Boolean);
begin
  raise Exception.Create('SetAutoFocus is not available for this target');
end;

procedure TTarget.MouseClientAreaOffset(var X, Y: Int32);
begin
  if FMouseClientAreaSet then
  begin
    X := X + FMouseClientArea.X1;
    Y := Y + FMouseClientArea.Y1;
  end;
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

end.

