{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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
unit IOManager;

interface

  uses
    Classes, SysUtils, mufasatypes, graphics, LCLType, bitmaps, LCLIntf, libloader, dynlibs;

  type    

    { This is the base class for the target functionality. If it provides a target, it extends this.  
    | Some methods in heregratuitous provide default functionality as a convinence. Only override as nessessary }

    { TTarget }

    TTarget = class(TObject)
      public
        
        { ONLY override some of the following methods if the target provides image functions, defaults 
        | to raise exceptions. GetColor provides default functionality using ReturData of width 1.     
        | FreeReturnData provides default of doing nothing. }
        procedure GetTargetDimensions(var w, h: integer); virtual;
        function GetColor(x,y : integer) : TColor; virtual;
        function ReturnData(xs, ys, width, height: Integer): TRetData; virtual;
        procedure FreeReturnData; virtual;
        procedure ActivateClient; virtual;
        function TargetValid: boolean; virtual;

        { ONLY override the following methods if the target provides mouse functions, defaults to 
        | raise exceptions }
        procedure GetMousePosition(var x,y: integer); virtual;
        procedure MoveMouse(x,y: integer); virtual;
        procedure HoldMouse(x,y: integer; button: TClickType); virtual;
        procedure ReleaseMouse(x,y: integer; button: TClickType); virtual;

        { ONLY override the following methods if the target provides key functions, defaults to 
        | raise exceptions }
        procedure SendString(str: string); virtual;
        procedure HoldKey(key: integer); virtual;
        procedure ReleaseKey(key: integer); virtual;
        function IsKeyHeld(key: integer): boolean; virtual;
        function GetKeyCode(C : char) : integer; virtual;
    end;
   
    { Implements a target that is a raw pixel array, e.g. stuff from a bitmap or a frozen state.
    | Currently this uses the pointer as-is, but it might be needed to make a local copy... }
    TRawTarget = class(TTarget)
      public
        constructor Create(rgb: prgb32; w,h: integer; CopyData : boolean = false);
        destructor Destroy; override;
        
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;

      protected
        rgb: prgb32;
        freedata : boolean;
        w,h: integer;
    end;

    TBitmapTarget = class(TTarget)
      public
        constructor Create(bitmap: TMufasaBitmap);
        destructor Destroy; override;

        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;

      protected
        bitmap: TMufasaBitmap;
    end;
      
   { Implements a target that is a Window in the operating system. This class is abstract, i.e.,
   | the actual os-specific Implementation of TWindow is in one of the os units. }

    { TWindow_Abstract }

    TWindow_Abstract = class(TTarget)
      public
        procedure GetTargetDimensions(var w, h: integer); override; abstract;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override; abstract;

        function TargetValid: boolean; override; abstract;
        procedure ActivateClient; override; abstract;
        procedure GetMousePosition(var x,y: integer); override; abstract;
        procedure MoveMouse(x,y: integer); override; abstract;
        procedure HoldMouse(x,y: integer; button: TClickType); override; abstract;
        procedure ReleaseMouse(x,y: integer; button: TClickType); override; abstract;

        procedure SendString(str: string); override; abstract;
        procedure HoldKey(key: integer); override; abstract;
        procedure ReleaseKey(key: integer); override; abstract;
        function IsKeyHeld(key: integer): boolean; override; abstract;
        function GetKeyCode(C : char) : integer; override; abstract;
    end;
        
    { Contains the pointers to a non-internal target implementation using the EIOS specification.
    | N.B. this *is* the specification that I will finalize the speficication as... Once this is
    | finalized that is. Trust me, its >9000 times easier to use a buffer across the language
    | barrier. And the internal target implementation of EIOS will make that verry efficient. }
    TEIOS_Client = record
      RequestTarget: function(initdata: pointer): pointer; stdcall;
      ReleaseTarget: procedure(target: pointer); stdcall;

      GetTargetDimensions: procedure(target: pointer; var w, h: integer); stdcall;
      GetImageBuffer: function(target: pointer): prgb32; stdcall;
      UpdateImageBuffer: procedure(target: pointer); stdcall;
      UpdateImageBufferBounds: procedure(target: pointer; sx,sy,ex,ey: integer); stdcall;

      GetMousePosition: procedure(target: pointer; var x,y: integer); stdcall;
      MoveMouse: procedure(target: pointer; x,y: integer); stdcall;
      HoldMouse: procedure(target: pointer; x,y: integer; left: boolean); stdcall;
      ReleaseMouse: procedure(target: pointer; x,y: integer; left: boolean); stdcall;

      SendString: procedure(target: pointer; str: PChar); stdcall;
      HoldKey: procedure(target: pointer; key: integer); stdcall;
      ReleaseKey: procedure(target: pointer; key: integer); stdcall;
      IsKeyHeld: function(target: pointer; key: integer): boolean; stdcall;
      GetKeyCode : function(target : pointer; C : char) : integer; stdcall;
    end;
    
    { Implements a EIOS target. This is, for all intensive purposes, a TRawTarget with added
    | key and mouse methods, as well as the capability to request a buffer update. N.B. that 
    | some EIOS implementors can and will update the buffer on their own. In that case, the
    | UpdateImageBuffer call is just a call to an empty method, OR does not exist. In the case
    | of an EIOS client not needing a method defined, it will not be exported and will be set
    | to NIL here. I think. Will get back to that. }

    { TEIOS_Target }

    TEIOS_Target = class(TTarget)
      public
        constructor Create(client: TEIOS_Client; initval: pointer);
        destructor Destroy; override;
        
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;

        procedure GetMousePosition(var x,y: integer); override;
        procedure MoveMouse(x,y: integer); override;
        procedure HoldMouse(x,y: integer; button: TClickType); override;
        procedure ReleaseMouse(x,y: integer; button: TClickType); override;

        procedure SendString(str: string); override;
        procedure HoldKey(key: integer); override;
        procedure ReleaseKey(key: integer); override;
        function IsKeyHeld(key: integer): boolean; override;
        function GetKeyCode(C : char) : integer; override;
      
      private
        client: TEIOS_Client;
        target: pointer;
        buffer: prgb32;
        width,height: integer;
    end;
      
    { EIOS Clients will give an exported name, have a loaded library associated, and have
    | a TEIOS_Client with the method pointers set. }
    type TEIOS_LoadedPlugin = record
      name: string;
      client: TEIOS_Client;
    end;

    { This is just a class that loads EIOS clients (like SMART) and sets them up to be used
    | as targets. I hope to have a method like... 
    | function SetTarget(eios_name: pchar; init_args: variant): integer;
    | where the init_args variant will be implied to be anything that can be cast to a pointer
    | in the binary side after it gets out of PascalScript. So, the only thing to setup SMART
    | would be the single call....
    | SetTarget('SMART',SmartSetupRecord);
    | Sexy, right? ;}
    TEIOS_Controller = class(TGenericLoader)
      public
        constructor Create();
        destructor Destroy; override;

        function ClientExists(name: string): boolean;
        function GetClient(name: string): TEIOS_Client;

      protected
        function InitPlugin(plugin: TLibHandle): boolean; override;
      private
        plugs: array of TEIOS_LoadedPlugin;
        function FindClient(name:string): integer;
    end;

    { This class specifies the object that will go in the ThreadVar to give the script access
    | to targets. This class is abstract, i.e., the actual os-specific Implementation of 
    | TIOManager is in one of the os units. 
    | I assume you Simba people know what the methods you made do, and I'm mantaining 
    | Name -> Function compatibility from the TWindow and TMInput classes (e.g. key, image, 
    | and window functions). I decided to split targeting into input/output == image/keymouse, 
    | since they sometimes are treated as seperate entities. }

    { TIOManager_Abstract }

    TIOManager_Abstract = class(TObject)
      public
        constructor Create;
        constructor Create(plugin_dir: string);
        destructor Destroy; override;
        
        procedure SetDesktop; virtual; abstract;
        function SetTarget(ArrPtr: PRGB32; Size: TPoint): integer; overload;
        function SetTarget(bmp : TMufasaBitmap) : integer; overload;
        function SetTarget(name: string; initargs: pointer): integer; overload;
        function TargetValid: Boolean;
        procedure BitmapDestroyed(Bitmap : TMufasaBitmap);

        function GetColor(x,y : integer) : TColor;
        function ReturnData(xs, ys, width, height: Integer): TRetData;
        procedure FreeReturnData;

        procedure GetDimensions(var W, H: Integer);
        procedure ActivateClient;

        function IsFrozen: boolean;
        procedure SetFrozen(makefrozen: boolean);
        
        procedure GetMousePos(var X, Y: Integer);
        procedure SetMousePos(X, Y: Integer);
        procedure HoldMouse(x,y : integer; button: TClickType);
        procedure ReleaseMouse(x,y : integer; button: TClickType);
        procedure ClickMouse(X, Y: Integer; button: TClickType);

        procedure KeyUp(key: Word);
        procedure KeyDown(key: Word);
        procedure PressKey(key: Word);
        procedure SendText(text: string);
        function isKeyDown(key: Word): Boolean;
        function GetKeyCode(c : char) : integer;

        function GetImageTarget: TTarget; overload;
        function GetKeyMouseTarget: TTarget; overload;

        procedure GetImageTarget(var idx: integer); overload;
        procedure GetKeyMouseTarget(var idx: integer); overload;
        procedure SetImageTarget(idx: integer);
        procedure SetKeyMouseTarget(idx: integer);
        procedure FreeTarget(idx: integer);
        procedure SetState(val: Boolean);

      protected
        function SetImageTarget(target: TTarget): integer;
        function SetKeyMouseTarget(target: TTarget): integer;
        function SetBothTargets(target: TTarget): integer;
        procedure NativeInit; virtual; abstract;
        procedure NativeFree; virtual; abstract;

      private
        keymouse: TTarget;
        image: TTarget;
        frozen: TTarget;
        freezebuffer: prgb32;
        bothsame: boolean;
        FStopping: Boolean;

        idxarr: array of TTarget;

        function GetTargetIdx(target: TTarget): integer;
        function GetIdxTarget(idx: integer): TTarget;
      property Stopping: Boolean Read FStopping write SetState;
    end;

implementation

  uses FileUtil,
    {$IFDEF MSWINDOWS} os_windows {$ENDIF}
    {$IFDEF LINUX} os_linux {$ENDIF};

  var eios_controller: TEIOS_Controller;

//***implementation*** TIOManager

constructor TIOManager_Abstract.Create(plugin_dir: string);
begin
  inherited Create;
  SetLength(idxarr,0);
  eios_controller.AddPath(plugin_dir);
  keymouse:= nil;
  image:= nil;
  frozen:= nil;
  NativeInit;
  SetDesktop;
end;

constructor TIOManager_Abstract.Create;
begin
  inherited Create;
  keymouse:= nil;
  image:= nil;
  frozen:= nil;
  NativeInit;
  SetDesktop;
end;

destructor TIOManager_Abstract.Destroy;
var
  i: integer;
begin
  for i:= high(idxarr) downto 0 do
    idxarr[i].Free;
end;

procedure TIOManager_Abstract.FreeTarget(idx: integer);
begin
  if idx > high(idxarr) then
    raise Exception.Create('Invalid target index');
  if idxarr[idx] = nil then
    raise Exception.Create('Double free of target');
  idxarr[idx].Free;
  idxarr[idx]:= nil;
end;

function TIOManager_Abstract.GetTargetIdx(target: TTarget): integer;
var
  i: integer;
begin
  result:= -1;
  for i:= 0 to high(idxarr) do
  begin
    if idxarr[i] = target then
    begin
      result:= i;
      exit;
    end;
    if (idxarr[i] = nil) and (result = -1) then
      result:= i;
  end;
  if result = -1 then
  begin
    SetLength(idxarr,Length(idxarr) + 1);
    result:= high(idxarr);
  end;
  idxarr[result]:= target;
end;

function TIOManager_Abstract.GetIdxTarget(idx: integer): TTarget;
begin
  if idx > high(idxarr) then
    raise Exception.Create('Invalid target index');
  if idxarr[idx] = nil then
    raise Exception.Create('No target with specified index');
  result:= idxarr[idx];
end;

function TIOManager_Abstract.SetImageTarget(target: TTarget): integer;
begin
  if IsFrozen then
    raise Exception.Create('You cannot set a target when Frozen');
  result:= GetTargetIdx(target);
  image:= target;
end;

function TIOManager_Abstract.GetImageTarget: TTarget;
begin
  result := image;
end;

function TIOManager_Abstract.SetKeyMouseTarget(target: TTarget): integer;
begin
  result:= GetTargetIdx(target);
  keymouse:= target;
end;

function TIOManager_Abstract.GetKeyMouseTarget: TTarget;
begin
  result := keymouse;
end;

function TIOManager_Abstract.SetBothTargets(target: TTarget): integer;
begin
  if IsFrozen then
    raise Exception.Create('You cannot set a target when Frozen');
  result:= GetTargetIdx(target);
  image:= target;
  keymouse:= target;
end;

procedure TIOManager_Abstract.SetFrozen(makefrozen: boolean);
var
  w,h: integer;
  buffer: TRetData;
begin
  if (makefrozen) and (IsFrozen) then
    raise Exception.Create('The window is already frozen.');
  if makefrozen then //No need for the Frozen = nil check, already done above with the exception.
  begin
    frozen:= image;
    frozen.GetTargetDimensions(w,h);
    buffer:= frozen.ReturnData(0,0,w,h);
    image:= TRawTarget.Create(buffer.Ptr,w,h,true);
    frozen.FreeReturnData;
  end else
  begin
    image.Free();
    image:= frozen;
    frozen:= nil;
  end;
end;

function TIOManager_Abstract.IsFrozen: boolean;
begin
  result:= frozen <> nil;
end;

function TIOManager_Abstract.GetColor(x,y : integer) : TColor;
begin
  result:= image.GetColor(x,y);
end;
function TIOManager_Abstract.ReturnData(xs,ys,width,height: integer): TRetData;
begin
  result:= image.ReturnData(xs,ys,width,height);
end;
procedure TIOManager_Abstract.FreeReturnData;
begin
  image.freeReturnData();
end;

function TIOManager_Abstract.SetTarget(ArrPtr: PRGB32; Size: TPoint): integer;
begin
  result:= SetImageTarget(TRawTarget.Create(ArrPtr,Size.X,Size.Y));
end;

//Only checks the current image target, not targets that might be in the indexes...
procedure TIOManager_Abstract.BitmapDestroyed(Bitmap : TMufasaBitmap);
begin
  if image is TBitmapTarget then
    if (TBitmapTarget(image).bitmap = Bitmap) and (not FStopping) then
      raise Exception.Create('Target bitmap was destroyed!');
end;

function TIOManager_Abstract.SetTarget(bmp : TMufasaBitmap) : integer;
begin
  result:= SetImageTarget(TBitmapTarget.Create(bmp));
  bmp.OnDestroy:= @BitmapDestroyed;
end;

function TIOManager_Abstract.SetTarget(name: string; initargs: pointer): integer;
var
  client: TEIOS_Client;
begin
  if not eios_controller.ClientExists(name) then
    raise Exception.Create('EIOS Client by specified name does not exist');
  client:= eios_controller.GetClient(name);
  result:= SetBothTargets(TEIOS_Target.Create(client, initargs));
end;

procedure TIOManager_Abstract.SetImageTarget(idx: integer);
begin
  image:= GetIdxTarget(idx);
end;

procedure TIOManager_Abstract.SetKeyMouseTarget(idx: integer);
begin
  keymouse:= GetIdxTarget(idx);
end;

procedure TIOManager_Abstract.GetImageTarget(var idx: integer);
begin
  if IsFrozen then
    raise Exception.Create('Cannot get image target while frozen');
  idx:= GetTargetIdx(image);
end;

procedure TIOManager_Abstract.GetKeyMouseTarget(var idx: integer);
begin
  idx:= GetTargetIdx(keymouse);
end;

function TIOManager_Abstract.TargetValid: Boolean;
begin
  result:= false;
  if (keymouse <> nil) and (image <> nil) then
    result := (keymouse.TargetValid and image.TargetValid);
end;

procedure TIOManager_Abstract.GetDimensions(var W, H: Integer); begin image.GetTargetDimensions(w,h) end;
procedure TIOManager_Abstract.ActivateClient; 
begin 
  keymouse.ActivateClient(); 
  {not sure if image needs activation or not, if its a native window keymouse == image so it should be good.}
end;

procedure TIOManager_Abstract.GetMousePos(var X, Y: Integer);
begin
  keymouse.GetMousePosition(x,y)
end;
procedure TIOManager_Abstract.SetMousePos(X, Y: Integer);
begin
  keymouse.MoveMouse(x,y);
end;
procedure TIOManager_Abstract.HoldMouse(x,y : integer; button: TClickType);
begin
  keymouse.HoldMouse(x,y,button);
end;
procedure TIOManager_Abstract.ReleaseMouse(x,y : integer; button: TClickType);
begin
  keymouse.ReleaseMouse(x,y,button);
end;

procedure TIOManager_Abstract.ClickMouse(X, Y: Integer; button: TClickType);
begin
  HoldMouse(x,y,button);
  //BenLand100 note: probably should wait here
  ReleaseMouse(x,y,button);
end;

procedure TIOManager_Abstract.KeyUp(key: Word);
begin
  keymouse.ReleaseKey(key)
end;
procedure TIOManager_Abstract.KeyDown(key: Word);
begin
  keymouse.HoldKey(key)
end;
procedure TIOManager_Abstract.PressKey(key: Word);
begin
  keyup(key);
  keydown(key);
end;
procedure TIOManager_Abstract.SendText(text: string);
begin
  keymouse.SendString(text);
end;

function TIOManager_Abstract.isKeyDown(key: Word): Boolean;
begin
  result:= keymouse.IsKeyHeld(key);
end;

function TIOManager_Abstract.GetKeyCode(c: char): integer;
begin
  result := keymouse.GetKeyCode(c);
end;

// TRUE when STOPPING.
procedure TIOManager_Abstract.SetState(val: Boolean);
begin
  FStopping := val;
end;

//***implementation*** TTarget

procedure TTarget.GetTargetDimensions(var w, h: integer); begin raise Exception.Create('GetTargetDimensions not available for this target'); end;
function TTarget.GetColor(x,y : integer) : TColor;
begin
  with ReturnData(x,y,1,1) do
    Result := RGBToColor(Ptr[0].r,Ptr[0].g,Ptr[0].b);
  FreeReturnData;
end;
function TTarget.ReturnData(xs, ys, width, height: Integer): TRetData;  begin raise Exception.Create('ReturnData not available for this target'); end;
procedure TTarget.FreeReturnData; begin {do nothing by default} end;
procedure TTarget.ActivateClient; begin raise Exception.Create('ActivateClient not available for this target'); end;
function TTarget.TargetValid: boolean; begin result:= true; end;

procedure TTarget.GetMousePosition(var x,y: integer); begin raise Exception.Create('GetMousePosition not available for this target'); end;
procedure TTarget.MoveMouse(x,y: integer); begin raise Exception.Create('MoveMouse not available for this target'); end;
procedure TTarget.HoldMouse(x,y: integer; button: TClickType); begin raise Exception.Create('HoldMouse not available for this target'); end;
procedure TTarget.ReleaseMouse(x,y: integer; button: TClickType); begin raise Exception.Create('ReleaseMouse not available for this target'); end;

procedure TTarget.SendString(str: string); begin raise Exception.Create('SendString not available for this target'); end;
procedure TTarget.HoldKey(key: integer); begin raise Exception.Create('HoldKey not available for this target'); end;
procedure TTarget.ReleaseKey(key: integer); begin raise Exception.Create('ReleaseKey not available for this target'); end;
function TTarget.IsKeyHeld(key: integer): boolean; begin raise Exception.Create('IsKeyHeld not available for this target'); end;
function TTarget.GetKeyCode(C: char): integer;begin Exception.CreateFMT('GetKeyCode - char (%s) to key is not available for this target.',[c]); end;

//***implementation*** TEIOS_Target

constructor TEIOS_Target.Create(client: TEIOS_Client; initval: pointer); begin
  inherited Create;
  self.client:= client;
  if Pointer(client.RequestTarget) <> nil then
    self.target:= client.RequestTarget(initval);
  if Pointer(client.GetImageBuffer) <> nil then
     self.buffer:= client.GetImageBuffer(target)
  else
     self.buffer:= nil;
  GetTargetDimensions(self.width,self.height);
end;

destructor TEIOS_Target.Destroy; begin
  client.ReleaseTarget(self.target);
  inherited Destroy;
end;

procedure TEIOS_Target.GetTargetDimensions(var w, h: integer);
begin
  if Pointer(client.GetTargetDimensions) <> nil then
    client.GetTargetDimensions(target,w,h)
  else
    inherited GetTargetDimensions(w,h);
end;
function TEIOS_Target.ReturnData(xs, ys, width, height: Integer): TRetData;
begin
  if Pointer(client.UpdateImageBufferBounds) <> nil then
    client.UpdateImageBufferBounds(target,xs,ys,xs+width,ys+height)
  else if Pointer(client.UpdateImageBuffer) <> nil then
    client.UpdateImageBuffer(target)
  else begin
    {no update command exported}
  end;
  result.Ptr := buffer;
  result.RowLen:= self.width;
  result.IncPtrWith:= result.RowLen - width;
  Inc(result.Ptr, ys * result.RowLen + xs);
end;

procedure TEIOS_Target.GetMousePosition(var x,y: integer);
begin
  if Pointer(client.GetMousePosition) <> nil then
    client.GetMousePosition(target,x,y)
  else
    inherited GetMousePosition(x,y);
end;
procedure TEIOS_Target.MoveMouse(x,y: integer);
begin
  if Pointer(client.MoveMouse) <> nil then
    client.MoveMouse(target,x,y)
  else
    inherited MoveMouse(x,y);
end;
procedure TEIOS_Target.HoldMouse(x,y: integer; button: TClickType);
begin
  if Pointer(client.HoldMouse) <> nil then
  begin
    case button of
      mouse_Left:   client.HoldMouse(target,x,y,true);
      mouse_Middle: raise Exception.Create('EIOS does not implement the middle mouse button.');
      mouse_Right:  client.HoldMouse(target,x,y,false);
    end;
  end else
    inherited HoldMouse(x,y,button);
end;
procedure TEIOS_Target.ReleaseMouse(x,y: integer; button: TClickType);
begin
  if Pointer(client.ReleaseMouse) <> nil then
  begin
    case button of
      mouse_Left:   client.ReleaseMouse(target,x,y,true);
      mouse_Middle: raise Exception.Create('EIOS does not implement the middle mouse button.');
      mouse_Right:  client.ReleaseMouse(target,x,y,false);
    end;
  end else
    inherited ReleaseMouse(x,y,button);
end;

procedure TEIOS_Target.SendString(str: string);
begin
  if Pointer(client.SendString) <> nil then
    client.SendString(target,PChar(str))
  else
    inherited SendString(str);
end;
procedure TEIOS_Target.HoldKey(key: integer);
begin
  if Pointer(client.HoldKey) <> nil then
    client.HoldKey(target,key)
  else
    inherited HoldKey(key);
end;
procedure TEIOS_Target.ReleaseKey(key: integer);
begin
  if Pointer(client.ReleaseKey) <> nil then
    client.ReleaseKey(target,key)
  else
    inherited ReleaseKey(key);
end;
function TEIOS_Target.IsKeyHeld(key: integer): boolean;
begin
  if Pointer(client.IsKeyHeld) <> nil then
    result:= client.IsKeyHeld(target,key)
  else
    result:= inherited IsKeyHeld(key);
end;

function TEIOS_Target.GetKeyCode(C: char): integer;
begin
  if Pointer(client.GetKeyCode) <> nil then
    result:= client.GetKeyCode(target,C)
  else
    result:= inherited GetKeyCode(C);
end;

//***implementation*** TRawTarget

constructor TRawTarget.Create(rgb: prgb32; w,h: integer; CopyData : boolean = false);
begin
  inherited Create;
  self.w:= w;
  self.h:= h;
  self.freedata:= copydata;
  if CopyData then
  begin
    GetMem(self.rgb,w*h*sizeof(TRGB32));
    Move(rgb[0],self.rgb[0],w*h*sizeof(TRGB32));
  end else
    self.rgb:= rgb;
end;

destructor TRawTarget.Destroy;
begin
  if freedata then
    Freemem(self.rgb,w*h*sizeof(TRGB32));
  inherited Destroy;
end;

procedure TRawTarget.GetTargetDimensions(var w, h: integer);
begin
  w:= self.w;
  h:= self.h;
end;

function TRawTarget.ReturnData(xs, ys, width, height: Integer): TRetData;
begin
  result.Ptr := rgb;
  result.RowLen:= self.w;
  result.IncPtrWith:= result.RowLen - width;
  Inc(result.Ptr, ys * result.RowLen + xs);
end;

//***implementation*** TBitmapTarget

constructor TBitmapTarget.Create(bitmap: TMufasaBitmap);
begin
  inherited Create;
  self.bitmap:= bitmap;
end;

destructor TBitmapTarget.Destroy;
begin
  inherited Destroy;
end;

procedure TBitmapTarget.GetTargetDimensions(var w, h: integer);
begin
  h:= bitmap.Height;
  w:= bitmap.Width;
end;

function TBitmapTarget.ReturnData(xs, ys, width, height: Integer): TRetData;
begin
  result.Ptr := bitmap.FData;
  result.RowLen:= bitmap.Width;
  result.IncPtrWith:= result.RowLen - width;
  Inc(result.Ptr, ys * result.RowLen + xs);
end;

//***implementation*** TEIOS_Controller

constructor TEIOS_Controller.Create();
begin
  inherited Create;
end;

destructor TEIOS_Controller.Destroy;
begin
  SetLength(plugs,0);
  inherited Destroy;
end;

function TEIOS_Controller.InitPlugin(plugin: TLibHandle): boolean;
var
  GetName: procedure(name: pchar); stdcall;
  buffer: pchar;
  idx: integer;
begin
  Pointer(GetName) := GetProcAddress(plugin, PChar('EIOS_GetName'));
  if Pointer(GetName) = nil then begin result:= false; exit; end;
  idx:= Length(plugs);
  SetLength(plugs,idx+1);
  buffer:= stralloc(255);
  GetName(buffer);
  plugs[idx].name:= buffer;
  strdispose(buffer);
  {link in all eios methods that *might* exist}
  with plugs[idx].client do
  begin
    Pointer(RequestTarget):= GetProcAddress(plugin, PChar('EIOS_RequestTarget'));
    Pointer(ReleaseTarget):= GetProcAddress(plugin, PChar('EIOS_ReleaseTarget'));

    Pointer(GetTargetDimensions):= GetProcAddress(plugin, PChar('EIOS_GetTargetDimensions'));
    Pointer(GetImageBuffer):= GetProcAddress(plugin, PChar('EIOS_GetImageBuffer'));
    Pointer(UpdateImageBuffer):= GetProcAddress(plugin, PChar('EIOS_UpdateImageBuffer'));
    Pointer(UpdateImageBufferBounds):= GetProcAddress(plugin, PChar('EIOS_UpdateImageBufferBounds'));

    Pointer(GetMousePosition):= GetProcAddress(plugin, PChar('EIOS_GetMousePosition'));
    Pointer(MoveMouse):= GetProcAddress(plugin, PChar('EIOS_MoveMouse'));
    Pointer(HoldMouse):= GetProcAddress(plugin, PChar('EIOS_HoldMouse'));
    Pointer(ReleaseMouse):= GetProcAddress(plugin, PChar('EIOS_ReleaseMouse'));

    Pointer(SendString):= GetProcAddress(plugin, PChar('EIOS_SendString'));
    Pointer(HoldKey):= GetProcAddress(plugin, PChar('EIOS_HoldKey'));
    Pointer(ReleaseKey):= GetProcAddress(plugin, PChar('EIOS_ReleaseKey'));
    Pointer(IsKeyHeld):= GetProcAddress(plugin, PChar('EIOS_IsKeyHeld'));
  end;
  {done linking in methods}
  result:= true;
end;

function TEIOS_Controller.FindClient(name: string): integer;
var
  i: integer;
begin
  i:= LoadPlugin(name);
  result:= -1;
end;

function TEIOS_Controller.ClientExists(name: string): boolean;
begin
  result:= FindClient(name) >= 0;
end;

function TEIOS_Controller.GetClient(name: string): TEIOS_Client;
var
  i: integer;
begin
  i:= FindClient(name);
  if i >= 0 then
    result:= plugs[i].client
end;

initialization
  eios_controller:= TEIOS_Controller.Create;
finalization
  eios_controller.Free;
end.
