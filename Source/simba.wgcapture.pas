{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Capture a window using the Windows.Graphics.Capture API.
   - Should be able to capture any gpu-rendered application.
   - Child window capturing is done with cropping the root window.
}
unit simba.wgcapture;

{$i simba.inc}

interface

uses
  SysUtils, Classes, Windows,
  simba.base,
  simba.wgcapture_bindings;

type
  { One capture target (window or monitor) and its running session. }
  TWGCSource = record
    IsMonitor: Boolean;
    Wnd: HWND;                    // window root (when not IsMonitor)
    Mon: HMONITOR;                // monitor (when IsMonitor)
    L, T: Integer;                // virtual-screen top-left (monitor compositing)
    CapW, CapH: Integer;          // capture surface size
    Item: IGraphicsCaptureItem;
    Pool: IDirect3D11CaptureFramePool;
    Session: IGraphicsCaptureSession;
    Staging: ID3D11Texture2D;     // CPU-readable copy of the last frame (the cache)
    HasFrame: Boolean;
  end;

  TWGCCapture = class
  private
    FDevice: ID3D11Device;        // shared across all sources
    FContext: ID3D11DeviceContext;
    FRtDevice: IDirect3DDevice;
    FPoolStatics: IDirect3D11CaptureFramePoolStatics2;
    FSources: array of TWGCSource;
    FLastError: String;

    function CheckHR(const what: string; hr: HResult): Boolean;
    function CreateDevice: Boolean;
    function EnsureDevice: Boolean;
    function BuildSource(var S: TWGCSource): Boolean;
    function RefreshSource(var S: TWGCSource): Boolean;
    function EnsureWindowSource(ARoot: HWND): Boolean;
    function EnsureMonitorSources: Boolean;
    function CropFromSource(const S: TWGCSource; srcX, srcY, AWidth, AHeight: Integer; var Data: PColorBGRA): Boolean;
    function BlitMonitors(Dest: PColorBGRA; dstW, dstH, originVX, originVY: Integer): Boolean;
    function ComposeDesktop(originVX, originVY, dstW, dstH: Integer; var Data: PColorBGRA): Boolean;
  public
    destructor Destroy; override;

    function Capture(Window: HWND; X, Y, AWidth, AHeight: Integer; var Data: PColorBGRA): Boolean;
    function CaptureDesktop(var Data: PColorBGRA; out Width, Height: Integer): Boolean;

    property LastError: string read FLastError;
  end;

implementation

uses
  MultiMon, DwmApi;

const
  FRAME_POOL_BUFFERS     = 2;
  FIRST_FRAME_TIMEOUT_MS = 2000;

type
  TMonList = record
    Items: array of HMONITOR;
  end;
  PMonList = ^TMonList;

function MonEnumProc(hMon: HMONITOR; dc: HDC; lprc: PRect; data: LPARAM): BOOL; stdcall;
var
  lst: PMonList;
begin
  lst := PMonList(data);
  SetLength(lst^.Items, Length(lst^.Items) + 1);
  lst^.Items[High(lst^.Items)] := hMon;
  Result := True;
end;

function TWGCCapture.CheckHR(const what: string; hr: HResult): Boolean;
begin
  Result := hr >= 0;
  if not Result then
    FLastError := Format('%s failed (hr=0x%.8x)', [what, DWord(hr)]);
end;

destructor TWGCCapture.Destroy;
begin
  SetLength(FSources, 0);   { releases each source's COM objects / buffers }
  FPoolStatics := nil;
  FRtDevice := nil;
  FContext := nil;
  FDevice := nil;
  inherited Destroy;
end;

function TWGCCapture.CreateDevice: Boolean;
var
  dxgiDevice: IDXGIDevice;
  inspectable: IInspectable;
  hr: HResult;
begin
  Result := False;
  { Real GPU first, then WARP as the software fallback (headless / RDP / no GPU).
    WARP is Microsoft's built-in software rasterizer; D3D_DRIVER_TYPE_SOFTWARE is
    NOT - it needs a caller-supplied rasterizer DLL and returns E_INVALIDARG with
    the nil we pass, so WARP is the correct choice. }
  hr := D3D11CreateDevice(nil, D3D_DRIVER_TYPE_HARDWARE, nil, D3D11_CREATE_DEVICE_BGRA_SUPPORT, nil, 0, D3D11_SDK_VERSION, FDevice, nil, FContext);
  if hr < 0 then
    hr := D3D11CreateDevice(nil, D3D_DRIVER_TYPE_WARP, nil, D3D11_CREATE_DEVICE_BGRA_SUPPORT, nil, 0, D3D11_SDK_VERSION, FDevice, nil, FContext);
  if not CheckHR('D3D11CreateDevice', hr) then
    Exit;

  if not Supports(FDevice, IDXGIDevice, dxgiDevice) then
  begin
    FLastError := 'ID3D11Device does not expose IDXGIDevice.';
    Exit;
  end;
  if not CheckHR('CreateDirect3D11DeviceFromDXGIDevice', CreateDirect3D11DeviceFromDXGIDevice(dxgiDevice, inspectable)) then
    Exit;
  if not Supports(inspectable, IDirect3DDevice, FRtDevice) then
  begin
    FLastError := 'IDirect3DDevice not available.';
    Exit;
  end;
  Result := True;
end;

function TWGCCapture.EnsureDevice: Boolean;
begin
  if FRtDevice <> nil then
    Exit(True);

  { Init COM on this thread; ignore the result - any apartment is fine and we
    never RoUninitialize, so a GUI host's COM lifetime is left untouched. }
  RoInitialize(RO_INIT_MULTITHREADED);

  if not CreateDevice then
    Exit(False);

  Result := CheckHR('RoGetActivationFactory(Direct3D11CaptureFramePool)', RoGetActivationFactory(HS_FramePool, IID_IDirect3D11CaptureFramePoolStatics2, FPoolStatics));
end;

{ Build (or rebuild) one source's item/pool/session/staging on the shared device.
  The item comes from CreateForWindow or CreateForMonitor per S.IsMonitor. }
function TWGCCapture.BuildSource(var S: TWGCSource): Boolean;
var
  interop: IGraphicsCaptureItemInterop;
  size: TSizeInt32;
  packedSize: Int64;
  desc: TD3D11Texture2DDesc;
  session2: IGraphicsCaptureSession2;
  session3: IGraphicsCaptureSession3;
  hr: HResult;
begin
  Result := False;
  S.Staging := nil;
  S.Session := nil;
  S.Pool := nil;
  S.Item := nil;
  S.HasFrame := False;

  if not CheckHR('RoGetActivationFactory(GraphicsCaptureItem)', RoGetActivationFactory(HS_GraphicsCaptureItem, IID_IGraphicsCaptureItemInterop, interop)) then
    Exit;
  if S.IsMonitor then
    hr := interop.CreateForMonitor(S.Mon, IID_IGraphicsCaptureItem, S.Item)
  else
    hr := interop.CreateForWindow(S.Wnd, IID_IGraphicsCaptureItem, S.Item);
  if not CheckHR('CreateForWindow/Monitor', hr) then
    Exit;
  if not CheckHR('IGraphicsCaptureItem.get_Size', S.Item.get_Size(size)) then
    Exit;
  if (size.Width <= 0) or (size.Height <= 0) then
  begin
    FLastError := Format('Target has no capturable size (%dx%d).', [size.Width, size.Height]);
    Exit;
  end;

  S.CapW := size.Width;
  S.CapH := size.Height;

  packedSize := Int64(UInt32(S.CapW)) or (Int64(UInt32(S.CapH)) shl 32);
  if not CheckHR('CreateFreeThreaded', FPoolStatics.CreateFreeThreaded(FRtDevice, DXGI_FORMAT_B8G8R8A8_UNORM, FRAME_POOL_BUFFERS, packedSize, S.Pool)) then
    Exit;
  if not CheckHR('CreateCaptureSession', S.Pool.CreateCaptureSession(S.Item, S.Session)) then
    Exit;

  { Best-effort: hide the cursor and the yellow capture border. }
  if Supports(S.Session, IGraphicsCaptureSession2, session2) then
    session2.put_IsCursorCaptureEnabled(False);
  if Supports(S.Session, IGraphicsCaptureSession3, session3) then
    session3.put_IsBorderRequired(False);

  FillChar(desc, SizeOf(desc), 0);
  desc.Width := UInt32(S.CapW);
  desc.Height := UInt32(S.CapH);
  desc.MipLevels := 1;
  desc.ArraySize := 1;
  desc.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  desc.SampleDesc.Count := 1;
  desc.Usage := D3D11_USAGE_STAGING;
  desc.CPUAccessFlags := D3D11_CPU_ACCESS_READ;
  if not CheckHR('CreateTexture2D(staging)', FDevice.CreateTexture2D(desc, nil, S.Staging)) then
    Exit;

  Result := CheckHR('StartCapture', S.Session.StartCapture);
end;

{ Refresh one source's staging texture with its latest frame (rebuilding it on a
  size change). Non-blocking once primed. Returns True if the staging texture
  holds a usable frame (a freshly copied one, or the previously cached one). }
function TWGCCapture.RefreshSource(var S: TWGCSource): Boolean;
var
  size: TSizeInt32;
  f, newest: IDirect3D11CaptureFrame;
  i, waited, attempts: Integer;
  surface: IDirect3DSurface;
  access: IDirect3DDxgiInterfaceAccess;
  frameTex: ID3D11Texture2D;
  mapped: TD3D11MappedSubresource;
begin
  Result := False;

  attempts := 0;
  repeat
    Inc(attempts);

    { Drain to the newest already-delivered frame (pool holds FRAME_POOL_BUFFERS). }
    newest := nil;
    for i := 1 to FRAME_POOL_BUFFERS do
    begin
      f := nil;
      if not CheckHR('TryGetNextFrame', S.Pool.TryGetNextFrame(f)) then
        Exit;
      if f = nil then
        Break;
      newest := f;
    end;

    if (newest = nil) and (not S.HasFrame) then
    begin
      waited := 0;
      while (newest = nil) and (waited < FIRST_FRAME_TIMEOUT_MS) do
      begin
        Sleep(15);
        Inc(waited, 15);
        f := nil;
        if not CheckHR('TryGetNextFrame', S.Pool.TryGetNextFrame(f)) then
          Exit;
        if f <> nil then
          newest := f;
      end;
    end;

    if newest = nil then
    begin
      Result := S.HasFrame;   { no new frame - reuse the previous one if we have it }
      Exit;
    end;

    { Detect a resize from the frame we already hold (its ContentSize), avoiding a
      per-frame IGraphicsCaptureItem.get_Size call. On a change, rebuild the pool /
      staging at the new size and re-drain for a fresh frame; the one we just got
      belongs to the old pool. Bounded to a single rebuild so a window that is
      actively being dragged-resized still makes progress (copying is size-safe
      regardless: staging, pool buffer and frame surface are always equal sized -
      ContentSize only tells us the buffer no longer matches the window). }
    if (attempts = 1) and (newest.get_ContentSize(size) >= 0) and
       (size.Width > 0) and (size.Height > 0) and
       ((size.Width <> S.CapW) or (size.Height <> S.CapH)) then
    begin
      newest := nil;   { release the old-pool frame before recreating }
      if not BuildSource(S) then
        Exit;
      Continue;
    end;

    Break;
  until False;

  if not CheckHR('get_Surface', newest.get_Surface(surface)) then
    Exit;
  if not Supports(surface, IDirect3DDxgiInterfaceAccess, access) then
  begin
    FLastError := 'IDirect3DDxgiInterfaceAccess not available.';
    Exit;
  end;
  if not CheckHR('GetInterface(ID3D11Texture2D)', access.GetInterface(IID_ID3D11Texture2D, frameTex)) then
    Exit;

  { Snapshot the frame into our staging texture, which holds it (the cache) until
    the next copy. The Map/Unmap forces the copy to finish before we release the
    frame back to the pool, so the pool can't recycle the surface mid-copy. We
    don't read here - crop/blit map the staging later and copy only the pixels
    they need, instead of de-padding the whole surface every refresh. }
  FContext.CopyResource(S.Staging, frameTex);
  if not CheckHR('Map(staging)', FContext.Map(S.Staging, 0, D3D11_MAP_READ, 0, mapped)) then
    Exit;
  FContext.Unmap(S.Staging, 0);
  S.HasFrame := True;
  Result := True;
end;

{ Make FSources hold exactly one window source for ARoot (reuse if unchanged). }
function TWGCCapture.EnsureWindowSource(ARoot: HWND): Boolean;
begin
  if (Length(FSources) = 1) and (not FSources[0].IsMonitor) and
     (FSources[0].Wnd = ARoot) and (FSources[0].Pool <> nil) then
    Exit(True);

  SetLength(FSources, 0);
  SetLength(FSources, 1);
  FSources[0].IsMonitor := False;
  FSources[0].Wnd := ARoot;
  Result := BuildSource(FSources[0]);
end;

{ Make FSources hold one source per monitor (reuse the set if unchanged, just
  refreshing positions; rebuild only when the monitor set changes). }
function TWGCCapture.EnsureMonitorSources: Boolean;
var
  lst: TMonList;
  mi: TMonitorInfo;
  i: Integer;
  sameSet: Boolean;
begin
  Result := False;
  lst.Items := nil;
  EnumDisplayMonitors(0, nil, @MonEnumProc, LPARAM(@lst));
  if Length(lst.Items) = 0 then
  begin
    FLastError := 'No monitors enumerated.';
    Exit;
  end;

  sameSet := (Length(FSources) = Length(lst.Items));
  if sameSet then
    for i := 0 to High(lst.Items) do
      if (not FSources[i].IsMonitor) or (FSources[i].Mon <> lst.Items[i]) then
      begin
        sameSet := False;
        Break;
      end;

  if not sameSet then
  begin
    SetLength(FSources, 0);
    SetLength(FSources, Length(lst.Items));
    for i := 0 to High(lst.Items) do
    begin
      FSources[i].IsMonitor := True;
      FSources[i].Mon := lst.Items[i];
    end;
  end;

  for i := 0 to High(FSources) do
  begin
    mi.cbSize := SizeOf(mi);
    if not GetMonitorInfo(FSources[i].Mon, @mi) then
    begin
      FLastError := 'GetMonitorInfo failed.';
      Exit;
    end;
    FSources[i].L := mi.rcMonitor.Left;
    FSources[i].T := mi.rcMonitor.Top;
    if FSources[i].Pool = nil then
      if not BuildSource(FSources[i]) then
        Exit;
  end;
  Result := True;
end;

{ Copy the srcX,srcY,AWidth,AHeight rect out of S's staging texture into Data
  (top-down TColorBGRA). Pixels outside the source surface are zero-filled. }
function TWGCCapture.CropFromSource(const S: TWGCSource; srcX, srcY, AWidth, AHeight: Integer; var Data: PColorBGRA): Boolean;
var
  srcRowY, x0, x1, iy, iyStart, iyEnd: Integer;
  copyBytes, dstColOff, bufBytes: PtrUInt;
  mapped: TD3D11MappedSubresource;
  srcPtr: PByte;
begin
  Result := False;
  bufBytes := PtrUInt(AWidth) * PtrUInt(AHeight) * SizeOf(TColorBGRA);
  Data := ReAllocMem(Data, bufBytes);

  { Only zero-fill when the rect isn't fully inside the surface (else the copy
    overwrites every pixel and the fill is wasted). }
  if (srcX < 0) or (srcY < 0) or (srcX + AWidth > S.CapW) or (srcY + AHeight > S.CapH) then
    FillChar(Data^, bufBytes, 0);

  x0 := Max(srcX, 0);
  x1 := Min(srcX + AWidth, S.CapW);
  if x1 > x0 then
  begin
    iyStart := Max(0, -srcY);
    iyEnd := Min(AHeight - 1, S.CapH - 1 - srcY);

    { Copy just the visible rows of the rect straight from the staging texture
      (a cached frame maps instantly - no pending GPU writes). RowPitch is a byte
      stride that may include GPU padding. }
    if not CheckHR('Map(staging)', FContext.Map(S.Staging, 0, D3D11_MAP_READ, 0, mapped)) then
      Exit;

    try
      copyBytes := PtrUInt(x1 - x0) * SizeOf(TColorBGRA);
      dstColOff := PtrUInt(x0 - srcX);
      for iy := iyStart to iyEnd do
      begin
        srcRowY := srcY + iy;
        srcPtr := PByte(mapped.pData) + PtrUInt(srcRowY) * PtrUInt(mapped.RowPitch) + PtrUInt(x0) * SizeOf(TColorBGRA);
        Move(srcPtr^, (Data + PtrUInt(iy) * PtrUInt(AWidth) + dstColOff)^, copyBytes);
      end;
    finally
      FContext.Unmap(S.Staging, 0);
    end;
  end;
  Result := True;
end;

function TWGCCapture.Capture(Window: HWND; X, Y, AWidth, AHeight: Integer; var Data: PColorBGRA): Boolean;
var
  root: HWND;
  rcRoot, rcWin: TRect;
  srcX, srcY: Integer;
begin
  Result := False;
  FLastError := '';
  if (AWidth <= 0) or (AHeight <= 0) then
  begin
    FLastError := 'Invalid capture dimensions.';
    Exit;
  end;
  if not IsWindow(Window) then
  begin
    FLastError := 'Not a valid window handle.';
    Exit;
  end;

  { GetDesktopWindow() is not a capturable window; route to the desktop
    composite and return the requested rect, with X,Y relative to the virtual
    desktop's top-left (so 0,0,virtualW,virtualH gives the whole desktop). }
  if (Window = GetDesktopWindow) then
  begin
    if not EnsureDevice then
      Exit;
    if not EnsureMonitorSources then
      Exit;
    Result := ComposeDesktop(GetSystemMetrics(SM_XVIRTUALSCREEN) + X,
                             GetSystemMetrics(SM_YVIRTUALSCREEN) + Y,
                             AWidth, AHeight, Data);
    Exit;
  end;

  root := GetAncestor(Window, GA_ROOT);
  if root = 0 then
    root := Window;

  if not EnsureDevice then
    Exit;
  if not EnsureWindowSource(root) then
    Exit;
  if not RefreshSource(FSources[0]) then
  begin
    if FLastError = '' then
      FLastError := 'No frame captured (timed out).';
    Exit;
  end;

  { Map the requested rect's origin into the captured surface. For a top-level
    target the surface origin IS the window origin, a 1:1 mapping. For a child,
    offset by its position within the root - measured against the root's DWM
    extended frame bounds (the visible window, = the capture surface origin), NOT
    GetWindowRect, which includes the invisible resize border (~7-13px) and would
    shift the crop by that border width. }
  if (Window = root) then
  begin
    srcX := X;
    srcY := Y;
  end
  else
  begin
    if DwmGetWindowAttribute(root, DWMWA_EXTENDED_FRAME_BOUNDS, @rcRoot, SizeOf(rcRoot)) < 0 then
      if not GetWindowRect(root, rcRoot) then   { fallback if DWM is unavailable }
      begin
        FLastError := 'Cannot get root window bounds.';
        Exit;
      end;
    if not GetWindowRect(Window, rcWin) then
    begin
      FLastError := 'GetWindowRect failed.';
      Exit;
    end;
    srcX := (rcWin.Left - rcRoot.Left) + X;
    srcY := (rcWin.Top  - rcRoot.Top ) + Y;
  end;

  Result := CropFromSource(FSources[0], srcX, srcY, AWidth, AHeight, Data);
end;

{ Blit every monitor source into Dest (dstW*dstH) at its virtual position, where
  (originVX, originVY) is the virtual-screen coordinate of Dest's top-left. Each
  monitor is clamped to Dest on all sides, so it works for a full-desktop buffer
  or any sub-region of it. }
function TWGCCapture.BlitMonitors(Dest: PColorBGRA; dstW, dstH, originVX, originVY: Integer): Boolean;
var
  i, y, dstX, dstY, srcX0, srcY0, ddx, ddy, copyW, copyH: Integer;
  mapped: TD3D11MappedSubresource;
  srcPtr: PByte;
begin
  Result := False;
  for i := 0 to High(FSources) do
  begin
    dstX := FSources[i].L - originVX;
    dstY := FSources[i].T - originVY;
    srcX0 := Max(0, -dstX); ddx := Max(0, dstX);
    srcY0 := Max(0, -dstY); ddy := Max(0, dstY);
    copyW := Min(FSources[i].CapW - srcX0, dstW - ddx);
    copyH := Min(FSources[i].CapH - srcY0, dstH - ddy);

    if (copyW > 0) and (copyH > 0) then
    begin
      if not CheckHR('Map(staging)', FContext.Map(FSources[i].Staging, 0, D3D11_MAP_READ, 0, mapped)) then
        Exit;

      try
        for y := 0 to copyH - 1 do
        begin
          srcPtr := PByte(mapped.pData) + PtrUInt(srcY0 + y) * PtrUInt(mapped.RowPitch) + PtrUInt(srcX0) * SizeOf(TColorBGRA);
          Move(srcPtr^, (Dest + PtrUInt(ddy + y) * PtrUInt(dstW) + PtrUInt(ddx))^, PtrUInt(copyW) * SizeOf(TColorBGRA));
        end;
      finally
        FContext.Unmap(FSources[i].Staging, 0);
      end;
    end;
  end;
  Result := True;
end;

{ Refresh all monitor sources and composite them into Data (dstW*dstH), with
  Data's top-left at virtual coord (originVX, originVY). Callers must have run
  EnsureDevice + EnsureMonitorSources first. }
function TWGCCapture.ComposeDesktop(originVX, originVY, dstW, dstH: Integer; var Data: PColorBGRA): Boolean;
var
  i: Integer;
  bufBytes, covered: PtrUInt;
begin
  Result := False;
  for i := 0 to High(FSources) do
    if not RefreshSource(FSources[i]) then
    begin
      if FLastError = '' then
        FLastError := Format('Monitor %d: no frame captured.', [i]);
      Exit;
    end;

  bufBytes := PtrUInt(dstW) * PtrUInt(dstH) * SizeOf(TColorBGRA);
  Data := ReAllocMem(Data, bufBytes);

  { Only zero-fill when the monitors don't perfectly tile the virtual screen }
  covered := 0;
  for i := 0 to High(FSources) do
    covered := covered + PtrUInt(FSources[i].CapW) * PtrUInt(FSources[i].CapH);
  if covered <> PtrUInt(dstW) * PtrUInt(dstH) then
    FillChar(Data^, bufBytes, 0);

  Result := BlitMonitors(Data, dstW, dstH, originVX, originVY);
end;

function TWGCCapture.CaptureDesktop(var Data: PColorBGRA; out Width, Height: Integer): Boolean;
var
  vx, vy, vw, vh: Integer;
begin
  Result := False;
  FLastError := '';
  Width := 0;
  Height := 0;

  if not EnsureDevice then
    Exit;
  if not EnsureMonitorSources then
    Exit;

  vx := GetSystemMetrics(SM_XVIRTUALSCREEN);
  vy := GetSystemMetrics(SM_YVIRTUALSCREEN);
  vw := GetSystemMetrics(SM_CXVIRTUALSCREEN);
  vh := GetSystemMetrics(SM_CYVIRTUALSCREEN);
  if (vw <= 0) or (vh <= 0) then
  begin
    FLastError := 'Invalid virtual screen size.';
    Exit;
  end;

  if not ComposeDesktop(vx, vy, vw, vh, Data) then
    Exit;
  Width := vw;
  Height := vh;
  Result := True;
end;

end.
