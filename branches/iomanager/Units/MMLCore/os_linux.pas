unit os_linux;

interface

  uses
    Classes, SysUtils, mufasatypes, xlib, x, xutil, IOManager;
  
  type

    TNativeWindow = x.TWindow;

    TWindow = class(TWindow_Abstract)
      public
        constructor Create(display: PDisplay; screennum: integer; window: x.TWindow); 
        destructor Destroy; override;
        procedure GetTargetDimensions(var w, h: integer); override;
        function ReturnData(xs, ys, width, height: Integer): TRetData; override;
        procedure FreeReturnData; override;

        procedure GetMousePosition(var x,y: integer); override;
        procedure MoveMouse(x,y: integer); override;
        procedure HoldMouse(x,y: integer; left: boolean); override;
        procedure ReleaseMouse(x,y: integer; left: boolean); override;

        procedure SendString(str: PChar); override;
        procedure HoldKey(key: integer); override;
        procedure ReleaseKey(key: integer); override;
        function IsKeyHeld(key: integer): boolean; override;
      private
        display: PDisplay;
        screennum: integer;
        window: x.TWindow;
        buffer: PXImage;
        dirty: Boolean;  //true if image loaded
    end;
    
    TIOManager = class(TIOManager_Abstract)
      public
        constructor Create(plugin_dir: string);
        function SetTarget(target: TNativeWindow): integer; overload;
        procedure SetDesktop; override;
      private
        procedure NativeInit; override;
        procedure NativeFree; override;
      public
        display: PDisplay;
        screennum: integer;
        desktop: x.TWindow;
    end;
    
    
implementation

  uses windowutil, GraphType;

//***implementation*** TWindow

  constructor TWindow.Create(display: PDisplay; screennum: integer; window: x.TWindow); 
  begin 
    inherited Create;
    self.display:= display;
    self.screennum:= screennum;
    self.window:= window;
    self.dirty:= false; 
  end; 
  
  destructor TWindow.Destroy; 
  begin
    FreeReturnData; 
    inherited Destroy; 
  end;
  
  procedure TWindow.GetTargetDimensions(var w, h: integer); 
  var
    Old_Handler: TXErrorHandler;
    Attrib: TXWindowAttributes;
    newx, newy: integer;
    childwindow: x.TWindow;
  begin
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    if XGetWindowAttributes(display, window, @Attrib) <> 0 Then
    begin
      { I don't think we need this XTranslateCoordinates... :D }
      XTranslateCoordinates(display, window, RootWindow(display, screennum), 0,0, @newx, @newy, @childwindow);
      W := Attrib.Width;
      H := Attrib.Height;
    end else
    begin
      { TODO: Raise Exception because the Window does not exist? }
      W := -1;
      H := -1;
    end;
    XSetErrorHandler(Old_Handler);
  end;
  
  function TWindow.ReturnData(xs, ys, width, height: Integer): TRetData;  
  var
    Old_Handler: TXErrorHandler;
    w,h: integer;
  begin
    GetTargetDimensions(w,h);
    if (xs < 0) or (xs + width > w) or (ys < 0) or (ys + height > h) then
      raise Exception.CreateFMT('TMWindow.ReturnData: The parameters passed are wrong; xs,ys %d,%d width,height %d,%d',[xs,ys,width,height]);
    if dirty then
      raise Exception.CreateFmt('ReturnData was called again without freeing'+
                                ' the previously used data. Do not forget to'+
                                ' call FreeReturnData', []);
    Old_Handler := XSetErrorHandler(@MufasaXErrorHandler);
    buffer := XGetImage(display, window, xs, ys, width, height, AllPlanes, ZPixmap);
    if buffer = nil then
    begin
      Writeln('ReturnData: XGetImage Error. Dumping data now:');
      Writeln('xs, ys, width, height: ' + inttostr(xs) + ', '  + inttostr(ys) +
              ', ' + inttostr(width) + ', ' + inttostr(height));
      Result.Ptr := nil;
      Result.IncPtrWith := 0;
      XSetErrorHandler(Old_Handler);
      raise Exception.CreateFMT('TMWindow.ReturnData: ReturnData: XGetImage Error', []);
      exit;
    end;
    Result.Ptr := PRGB32(buffer^.data);
    Result.IncPtrWith := 0;
    Result.RowLen := width;
    dirty:= true;
    XSetErrorHandler(Old_Handler);
  end;
  
  procedure TWindow.FreeReturnData; 
  begin
    if dirty then
    begin
      if (buffer <> nil) then
        XDestroyImage(buffer);
      buffer:= nil;
      dirty:= false;
    end;
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

  constructor TIOManager.Create(plugin_dir: string);
  begin
    inherited Create(plugin_dir);
  end;

  procedure TIOManager.NativeInit;
  begin
    display := XOpenDisplay(nil);
    if display = nil then
    begin
      // throw Exception
    end;
    screennum:= DefaultScreen(display);
    desktop:= RootWindow(display,screennum)
  end;
  
  procedure TIOManager.NativeFree; 
  begin
    XCloseDisplay(display);
  end;
  
  procedure TIOManager.SetDesktop;
  begin
    SetBothTargets(TWindow.Create(display, screennum, desktop));
  end;
  
  function TIOManager.SetTarget(target: x.TWindow): integer;
  begin
    SetBothTargets(TWindow.Create(display, screennum, target))
  end;
  
end.
