unit os_windows;

interface

  uses
    Classes, SysUtils, mufasatypes, windows, IOManager;
    
  type
    
    TWindow = class(TWindow_Abstract)
      public
        constructor Create(target: Hwnd); 
        destructor Destroy; override;
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;
        function GetColor(x,y : integer) : TColor; override;

        procedure GetMousePosition(var x,y: integer); override;
        procedure MoveMouse(x,y: integer); override;
        procedure HoldMouse(x,y: integer; left: boolean); override;
        procedure ReleaseMouse(x,y: integer; left: boolean); override;

        procedure SendString(str: PChar); override;
        procedure HoldKey(key: integer); override;
        procedure ReleaseKey(key: integer); override;
        function IsKeyHeld(key: integer): boolean; override;
      private
        procedure ValidateBuffer(w,h:integer);
        handle: Hwnd;
        dc: HDC;
        buffer: TBitmap;
        buffer_raw: prgb32;
        width,height: integer;
    end;
    
    TIOManager = class(TIOManager_Abstract)
      public
        constructor Create(plugin_dir: string);
        destructor Destroy; override
        function SetTarget(target: Hwnd): integer; overload;
        procedure SetDesktop;
      private
        procedure NativeInit; override;
        procedure NativeFree; override;
        DesktopHWND : Hwnd;
    end;
    
implementation

//***implementation*** TWindow

  constructor TWindow.Create(target: Hwnd); begin 
    inherited Create; 
    self.dc:= GetDC(hwnd);
    self.buffer:= TBitmap.Create;
    self.buffer.PixelFormat:= pf32bit;
  end; 
  
  destructor TWindow.Destroy; begin
    ReleaseDC(hwnd,dc);
    buffer.Free;
    inherited Destroy; 
  end;

  procedure TWindow.GetTargetDimensions(var w, h: integer);
  var
    Rect : TRect; 
  begin 
    GetWindowRect(self.hwnd, Rect);
    w:= Rect.Right - Rect.Left;
    h:= Rect.Bottom - Rect.Top;
  end;
  
  function TWindow.GetColor(x,y : integer) : TColor;
  begin
    result:= GetPixel(self.dc,x,y)
  end;
  
  procedure TWindow.ValidateBuffer(w,h:integer);
  begin
    if (w <> self.width) or (height <> self.height) then
    begin
      DrawBitmap.SetSize(w,h);
      self.width:= w;
      self.height:= h;
      GetObject(DrawBitmap.Handle, SizeOf(BmpInfo), @BmpInfo);
      self.buffer_raw := BmpInfo.bmBits;
    end;
  end;
  
  function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;
  var
    temp: PRGB32;
    w,h : integer;
  begin
    GetDimensions(w,h);
    ValidateBuffer(w,h);
    if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) then
      raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);
    BitBlt(self.buffer.Canvas.Handle,0,0, width, height, self.dc, xs,ys, SRCCOPY);
    Result.Ptr:= self.buffer_raw;
    Result.IncPtrWith:= w - width;
    Result.RowLen:= w;
  end;

  procedure TWindow.GetMousePosition(var x,y: integer); begin end;
  procedure TWindow.MoveMouse(x,y: integer); begin end;
  procedure TWindow.HoldMouse(x,y: integer; left: boolean); begin end;
  procedure TWindow.ReleaseMouse(x,y: integer; left: boolean); begin end;

  procedure TWindow.SendString(str: PChar); begin end;
  procedure TWindow.HoldKey(key: integer); begin end;
  procedure TWindow.ReleaseKey(key: integer); begin end;
  function TWindow.IsKeyHeld(key: integer): boolean; begin end;
  
  
//***implementation*** IOManager
  
  procedure TIOManager.NativeInit; 
  begin
     self.DesktopHWND:= GetDesktopWindow;
  end;
  
  procedure TIOManager.NativeFree; 
  begin
  end;
  
  procedure TIOManager.SetDesktop;
  begin
    SetBothTargets(TWindow.Create(DesktopHWND));
  end;
  
  function TIOManager.SetTarget(target: Hwnd): integer;
  begin
    SetBothTargets(TWindow.Create(hwnd));
  end;
  
  
