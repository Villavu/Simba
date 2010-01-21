unit IOManager;

interface

  uses
    Classes, SysUtils, mufasatypes, graphics, LCLType, bitmaps, LCLIntf;

  type    
  
    { This is the base class for the target functionality. If it provides a target, it extends this.  
    | Some methods in heregratuitous provide default functionality as a convinence. Only override as nessessary }
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
    end;
   
    { Implements a target that is a raw pixel array, e.g. stuff from a bitmap or a frozen state.
    | Currently this uses the pointer as-is, but it might be needed to make a local copy... }
    TRawTarget = class(TTarget)
      public
        constructor Create(rgb: prgb32; w,h: integer);
        destructor Destroy; override;
        
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;

      private
        rgb: prgb32;
        w,h: integer;
    end;
      
   { Implements a target that is a Window in the operating system. This class is abstract, i.e.,
   | the actual os-specific Implementation of TWindow is in one of the os units. }
    TWindow_Abstract = class(TTarget)
      public
        procedure GetTargetDimensions(var w, h: integer); override; abstract;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override; abstract;

        procedure ActivateClient; override; abstract;
        procedure GetMousePosition(var x,y: integer); override; abstract;
        procedure MoveMouse(x,y: integer); override; abstract;
        procedure HoldMouse(x,y: integer; button: TClickType); override; abstract;
        procedure ReleaseMouse(x,y: integer; button: TClickType); override; abstract;

        procedure SendString(str: string); override; abstract;
        procedure HoldKey(key: integer); override; abstract;
        procedure ReleaseKey(key: integer); override; abstract;
        function IsKeyHeld(key: integer): boolean; override; abstract;
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

      Initialize: procedure; stdcall;
      Finalize: procedure; stdcall;
    end;
    
    { Implements a EIOS target. This is, for all intensive purposes, a TRawTarget with added
    | key and mouse methods, as well as the capability to request a buffer update. N.B. that 
    | some EIOS implementors can and will update the buffer on their own. In that case, the
    | UpdateImageBuffer call is just a call to an empty method, OR does not exist. In the case
    | of an EIOS client not needing a method defined, it will not be exported and will be set
    | to NIL here. I think. Will get back to that. }
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
      plugin: pointer;
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
    TEIOS_Controller = class(TObject)
      public
        constructor Create(plugin_dir: string);
        destructor Destroy; override;

        function ClientExists(name: string): boolean;
        function GetClient(name: string): TEIOS_Client;
        
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
    TIOManager_Abstract = class(TObject)
      public
        constructor Create(plugin_dir: string);
        destructor Destroy; override;
        
        procedure SetDesktop; virtual; abstract;
        function SetTarget(ArrPtr: PRGB32; Size: TPoint): integer; overload;
        function SetTarget(bmp : TMufasaBitmap) : integer; overload;
        function SetTarget(name: string; initargs: pointer): integer; overload;
        function TargetValid: Boolean;

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
        
      protected
        controller: TEIOS_Controller;
        keymouse: TTarget;
        image: TTarget;
        frozen: TTarget;
        freezebuffer: prgb32;
        bothsame: boolean;
        
        procedure SetImageTarget(target: TTarget);
        procedure SetKeyMouseTarget(target: TTarget);
        procedure SetBothTargets(target: TTarget);
        procedure NativeInit; virtual; abstract;
        procedure NativeFree; virtual; abstract;
    end;

implementation

  uses 
    {$IFDEF MSWINDOWS} os_windows {$ENDIF}
    {$IFDEF LINUX} os_linux {$ENDIF};

//***implementation*** TIOManager

  constructor TIOManager_Abstract.Create(plugin_dir: string);
  begin
    inherited Create;
    controller:= nil;
    keymouse:= nil;
    image:= nil;
    frozen:= nil;
    NativeInit;
    SetDesktop;
  end;
  
  destructor TIOManager_Abstract.Destroy;
  begin
    if bothsame then keymouse.Destroy() else
    begin
      keymouse.Free();
      image.Free();
    end;
    if frozen <> nil then frozen.Destroy();
    if controller <> nil then controller.Destroy();
  end;
  
  procedure TIOManager_Abstract.SetImageTarget(target: TTarget);
  begin
    if frozen <> nil then 
      raise Exception.Create('You cannot set a target when Frozen');
    if not(bothsame) then image.Free();
    image:= target;
    bothsame:= false;
  end;
  procedure TIOManager_Abstract.SetKeyMouseTarget(target: TTarget);
  begin
    if not(bothsame) then keymouse.Free();
    keymouse:= target;
    bothsame:= false;
  end;
  procedure TIOManager_Abstract.SetBothTargets(target: TTarget);
  begin
    if frozen <> nil then 
      raise Exception.Create('You cannot set a target when Frozen');
    if bothsame then image.Destroy() else
    begin
      image.Free();
      keymouse.Free();
    end; 
    image:= target;
    keymouse:= target;
    bothsame:= true;
  end;
  
  procedure TIOManager_Abstract.SetFrozen(makefrozen: boolean);
  var
    w,h: integer;
    buffer: TRetData;
  begin
    if (makefrozen) and (frozen <> nil) then
      raise Exception.Create('The window is already frozen.');
    //BenLand100 edit: I say we leave this exception out. POLS
    //if not(isfrozen) and (frozen = nil) then
    //  raise Exception.Create('The window is not frozen.');
    if makefrozen then
    begin
      image.Destroy();
      image:= frozen;
    end else if frozen = nil then
    begin
      frozen:= image;
      frozen.GetTargetDimensions(w,h);
      buffer:= frozen.ReturnData(0,0,w,h);
      GetMem(freezebuffer, w * h * sizeof(TRGB32));
      Move(buffer.Ptr[0], freezebuffer[0], w*h*sizeof(TRGB32));
      frozen.FreeReturnData;
      image:= TRawTarget.Create(freezebuffer,w,h);
    end;
  end;

  function TIOManager_Abstract.IsFrozen: boolean;
  begin
    result:= frozen <> nil;
  end;

  function TIOManager_Abstract.GetColor(x,y : integer) : TColor; begin result:= image.GetColor(x,y); end;
  function TIOManager_Abstract.ReturnData(xs,ys,width,height: integer): TRetData; begin result:= image.ReturnData(xs,ys,width,height); end;
  procedure TIOManager_Abstract.FreeReturnData; begin image.freeReturnData(); end;
  
  function TIOManager_Abstract.SetTarget(ArrPtr: PRGB32; Size: TPoint): integer; begin SetImageTarget(TRawTarget.Create(ArrPtr,Size.X,Size.Y)); end;
  function TIOManager_Abstract.SetTarget(bmp : TMufasaBitmap) : integer; begin SetImageTarget(TRawTarget.Create(bmp.FData,bmp.width,bmp.height)); end;
  function TIOManager_Abstract.SetTarget(name: string; initargs: pointer): integer; 
  var
    client: TEIOS_Client;
  begin
    if not controller.ClientExists(name) then raise Exception.Create('EIOS Client by specified name does not exist');
    client:= controller.GetClient(name);
    SetBothTargets(TEIOS_Target.Create(client, initargs));
  end;
  
  function TIOManager_Abstract.TargetValid: Boolean;
  begin
    result:= (keymouse <> nil) and (image <> nil);
  end;

  procedure TIOManager_Abstract.GetDimensions(var W, H: Integer); begin image.GetTargetDimensions(w,h) end;
  procedure TIOManager_Abstract.ActivateClient; begin {lolwat} end;

  procedure TIOManager_Abstract.GetMousePos(var X, Y: Integer); begin keymouse.GetMousePosition(x,y) end;
  procedure TIOManager_Abstract.SetMousePos(X, Y: Integer); begin keymouse.MoveMouse(x,y); end;
  procedure TIOManager_Abstract.HoldMouse(x,y : integer; button: TClickType); begin keymouse.ReleaseMouse(x,y,button); end;
  procedure TIOManager_Abstract.ReleaseMouse(x,y : integer; button: TClickType); begin keymouse.ReleaseMouse(x,y,button); end;
  procedure TIOManager_Abstract.ClickMouse(X, Y: Integer; button: TClickType);
  begin
    HoldMouse(x,y,button);
    //BenLand100 note: probably should wait here
    ReleaseMouse(x,y,button);
  end;

  procedure TIOManager_Abstract.KeyUp(key: Word); begin keymouse.ReleaseKey(key) end;
  procedure TIOManager_Abstract.KeyDown(key: Word); begin keymouse.HoldKey(key) end;
  procedure TIOManager_Abstract.PressKey(key: Word); begin keyup(key); keydown(key); end;
  procedure TIOManager_Abstract.SendText(text: string); begin keymouse.SendString(PChar(@text[1])); end;
  function TIOManager_Abstract.isKeyDown(key: Word): Boolean; begin result:= keymouse.IsKeyHeld(key); end;
  
//***implementation*** TTarget
  
  procedure TTarget.GetTargetDimensions(var w, h: integer); begin raise Exception.Create('GetTargetDimensions not avaliable for this target'); end;
  function TTarget.GetColor(x,y : integer) : TColor; 
  begin
    with ReturnData(x,y,1,1) do
      Result := RGBToColor(Ptr[0].r,Ptr[0].g,Ptr[0].b);
    FreeReturnData;
  end;
  function TTarget.ReturnData(xs, ys, width, height: Integer): TRetData;  begin raise Exception.Create('ReturnData not avaliable for this target'); end;
  procedure TTarget.FreeReturnData; begin {do nothing by default} end;
  procedure TTarget.ActivateClient; begin raise Exception.Create('ActivateClient not avaliable for this target'); end;

  procedure TTarget.GetMousePosition(var x,y: integer); begin raise Exception.Create('GetMousePosition not avaliable for this target'); end;
  procedure TTarget.MoveMouse(x,y: integer); begin raise Exception.Create('MoveMouse not avaliable for this target'); end;
  procedure TTarget.HoldMouse(x,y: integer; button: TClickType); begin raise Exception.Create('HoldMouse not avaliable for this target'); end;
  procedure TTarget.ReleaseMouse(x,y: integer; button: TClickType); begin raise Exception.Create('ReleaseMouse not avaliable for this target'); end;

  procedure TTarget.SendString(str: string); begin raise Exception.Create('SendString not avaliable for this target'); end;
  procedure TTarget.HoldKey(key: integer); begin raise Exception.Create('HoldKey not avaliable for this target'); end;
  procedure TTarget.ReleaseKey(key: integer); begin raise Exception.Create('ReleaseKey not avaliable for this target'); end;
  function TTarget.IsKeyHeld(key: integer): boolean; begin raise Exception.Create('IsKeyHeld not avaliable for this target'); end;
  
//***implementation*** TEIOS_Target
  
  constructor TEIOS_Target.Create(client: TEIOS_Client; initval: pointer); begin
    inherited Create;
    self.client:= client;
    self.target:= client.RequestTarget(initval);
    self.buffer:= client.GetImageBuffer(target);
    client.GetTargetDimensions(target,self.width,self.height);
  end; 
  
  destructor TEIOS_Target.Destroy; begin 
    client.ReleaseTarget(self.target);
    inherited Destroy;
  end;
  
  procedure TEIOS_Target.GetTargetDimensions(var w, h: integer); begin client.GetTargetDimensions(target,w,h); end;
  function TEIOS_Target.ReturnData(xs, ys, width, height: Integer): TRetData;  
  begin
    client.UpdateImageBufferBounds(target,xs,ys,xs+width,ys+height);
    result.Ptr := buffer;
    result.RowLen:= self.width;
    result.IncPtrWith:= result.RowLen - width;
    Inc(result.Ptr, ys * result.RowLen + xs);
  end;

  procedure TEIOS_Target.GetMousePosition(var x,y: integer); begin client.GetMousePosition(target,x,y); end;
  procedure TEIOS_Target.MoveMouse(x,y: integer); begin client.MoveMouse(target,x,y); end;
  procedure TEIOS_Target.HoldMouse(x,y: integer; button: TClickType);
  begin
    case button of
      mouse_Left:   client.HoldMouse(target,x,y,true);
      mouse_Middle: raise Exception.Create('EIOS does not implement the middle mouse button.');
      mouse_Right:  client.HoldMouse(target,x,y,false);
    end;
  end;
  procedure TEIOS_Target.ReleaseMouse(x,y: integer; button: TClickType);
  begin
    case button of
      mouse_Left:   client.ReleaseMouse(target,x,y,true);
      mouse_Middle: raise Exception.Create('EIOS does not implement the middle mouse button.');
      mouse_Right:  client.ReleaseMouse(target,x,y,false);
    end;
  end;

  procedure TEIOS_Target.SendString(str: string); begin client.SendString(target,PChar(@str[1])); end;
  procedure TEIOS_Target.HoldKey(key: integer); begin client.HoldKey(target,key); end;
  procedure TEIOS_Target.ReleaseKey(key: integer); begin client.ReleaseKey(target,key); end;
  function TEIOS_Target.IsKeyHeld(key: integer): boolean; begin result:= client.IsKeyHeld(target,key); end;
  
//***implementation*** TRawTarget
  
  constructor TRawTarget.Create(rgb: prgb32; w,h: integer);
  begin
    inherited Create;
    self.rgb:= rgb;
    self.w:= w;
    self.h:= h;
  end;
  
  destructor TRawTarget.Destroy;
  begin
    {do nothing}
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

//***implementation*** TEIOS_Controller
  
  constructor TEIOS_Controller.Create(plugin_dir: string);
  begin
    inherited Create;
    SetLength(Plugs, 0);
    //Load plugins from plugins folder
  end;

  destructor TEIOS_Controller.Destroy;
  var
    i: integer;
  begin
    for i:= 0 to length(plugs) - 1 do
      if plugs[i].plugin <> nil then
      begin
        //Unload plugin that was loaded
      end;
    inherited Destroy;
  end;
  
  function TEIOS_Controller.FindClient(name: string): integer;
  var
    i: integer;
  begin
    for i:= 0 to length(plugs) - 1 do
      if plugs[i].name = name then
      begin
        result:= i;
        exit;
      end;
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

end.
