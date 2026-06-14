{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Bindings for Windows.Graphics.Capture and Direct3D 11 needed to capture a window.
}
unit simba.wgcapture_bindings;

{$i simba.inc}

interface

uses
  Windows;

type
  THStringHeader = record
    Reserved1: Pointer;
    Reserved2: array[0..15] of Byte;
  end;

  TSizeInt32 = record
    Width: Int32;
    Height: Int32;
  end;

  TDXGISampleDesc = record
    Count: UInt32;
    Quality: UInt32;
  end;

  TD3D11Texture2DDesc = record
    Width: UInt32;
    Height: UInt32;
    MipLevels: UInt32;
    ArraySize: UInt32;
    Format: UInt32;
    SampleDesc: TDXGISampleDesc;
    Usage: UInt32;
    BindFlags: UInt32;
    CPUAccessFlags: UInt32;
    MiscFlags: UInt32;
  end;

  TD3D11MappedSubresource = record
    pData: Pointer;
    RowPitch: UInt32;
    DepthPitch: UInt32;
  end;

  ID3D11Texture2D = interface;

  IInspectable = interface(IUnknown)
    ['{AF86E2E0-B12D-4C6A-9C5A-D7AA65101E90}']
    function GetIids(out iidCount: UInt32; out iids: PGUID): HResult; stdcall;
    function GetRuntimeClassName(out className: Pointer): HResult; stdcall;
    function GetTrustLevel(out trustLevel: Int32): HResult; stdcall;
  end;

  ID3D11Texture2D = interface(IUnknown)
    ['{6f15aaf2-d208-4e89-9ab4-489535d34f9c}']
  end;

  ID3D11Device = interface(IUnknown)
    ['{db6f6ddb-ac77-4e88-8253-819df9bbf140}']
    function _r3_CreateBuffer: HResult; stdcall;
    function _r4_CreateTexture1D: HResult; stdcall;
    function CreateTexture2D(const pDesc: TD3D11Texture2DDesc;
      pInitialData: Pointer; out ppTexture2D: ID3D11Texture2D): HResult; stdcall;
  end;

  ID3D11DeviceContext = interface(IUnknown)
    ['{c0bfa96c-e089-44fb-8eaf-26f8796190da}']
    function _r3_GetDevice: HResult; stdcall;
    function _r4_GetPrivateData: HResult; stdcall;
    function _r5_SetPrivateData: HResult; stdcall;
    function _r6_SetPrivateDataInterface: HResult; stdcall;
    function _r7_VSSetConstantBuffers: HResult; stdcall;
    function _r8_PSSetShaderResources: HResult; stdcall;
    function _r9_PSSetShader: HResult; stdcall;
    function _r10_PSSetSamplers: HResult; stdcall;
    function _r11_VSSetShader: HResult; stdcall;
    function _r12_DrawIndexed: HResult; stdcall;
    function _r13_Draw: HResult; stdcall;
    function Map(pResource: IUnknown; Subresource: UInt32; MapType: UInt32;
      MapFlags: UInt32; out pMappedResource: TD3D11MappedSubresource): HResult; stdcall;
    procedure Unmap(pResource: IUnknown; Subresource: UInt32); stdcall;
    function _r16_PSSetConstantBuffers: HResult; stdcall;
    function _r17_IASetInputLayout: HResult; stdcall;
    function _r18_IASetVertexBuffers: HResult; stdcall;
    function _r19_IASetIndexBuffer: HResult; stdcall;
    function _r20_DrawIndexedInstanced: HResult; stdcall;
    function _r21_DrawInstanced: HResult; stdcall;
    function _r22_GSSetConstantBuffers: HResult; stdcall;
    function _r23_GSSetShader: HResult; stdcall;
    function _r24_IASetPrimitiveTopology: HResult; stdcall;
    function _r25_VSSetShaderResources: HResult; stdcall;
    function _r26_VSSetSamplers: HResult; stdcall;
    function _r27_Begin: HResult; stdcall;
    function _r28_End: HResult; stdcall;
    function _r29_GetData: HResult; stdcall;
    function _r30_SetPredication: HResult; stdcall;
    function _r31_GSSetShaderResources: HResult; stdcall;
    function _r32_GSSetSamplers: HResult; stdcall;
    function _r33_OMSetRenderTargets: HResult; stdcall;
    function _r34_OMSetRenderTargetsAndUAV: HResult; stdcall;
    function _r35_OMSetBlendState: HResult; stdcall;
    function _r36_OMSetDepthStencilState: HResult; stdcall;
    function _r37_SOSetTargets: HResult; stdcall;
    function _r38_DrawAuto: HResult; stdcall;
    function _r39_DrawIndexedInstancedIndirect: HResult; stdcall;
    function _r40_DrawInstancedIndirect: HResult; stdcall;
    function _r41_Dispatch: HResult; stdcall;
    function _r42_DispatchIndirect: HResult; stdcall;
    function _r43_RSSetState: HResult; stdcall;
    function _r44_RSSetViewports: HResult; stdcall;
    function _r45_RSSetScissorRects: HResult; stdcall;
    function _r46_CopySubresourceRegion: HResult; stdcall;
    procedure CopyResource(pDstResource: IUnknown; pSrcResource: IUnknown); stdcall;
  end;

  IDXGIDevice = interface(IUnknown)
    ['{54ec77fa-1377-44e6-8c32-88fd5f44c84c}']
  end;

  IDirect3DDevice = interface(IInspectable)
    ['{a37624ab-8d5f-4650-9d3e-9eae3d9bc670}']
  end;

  IDirect3DSurface = interface(IInspectable)
    ['{0bf4a146-13c1-4694-bee3-7abf15eaf586}']
  end;

  IDirect3DDxgiInterfaceAccess = interface(IUnknown)
    ['{a9b3d012-3df2-4ee3-b8d1-8695f457d3c1}']
    function GetInterface(const iid: TGUID; out p: ID3D11Texture2D): HResult; stdcall;
  end;

  IGraphicsCaptureItem = interface(IInspectable)
    ['{79c3f95b-31f7-4ec2-a464-632ef5d30760}']
    function get_DisplayName(out value: Pointer): HResult; stdcall;
    function get_Size(out value: TSizeInt32): HResult; stdcall;
  end;

  IGraphicsCaptureItemInterop = interface(IUnknown)
    ['{3628e81b-3cac-4c60-b7f4-23ce0e0c3356}']
    function CreateForWindow(window: HWND; const riid: TGUID;
      out outItem: IGraphicsCaptureItem): HResult; stdcall;
    function CreateForMonitor(monitor: HMONITOR; const riid: TGUID;
      out outItem: IGraphicsCaptureItem): HResult; stdcall;
  end;

  IGraphicsCaptureSession = interface(IInspectable)
    ['{814e42a9-f70f-4ad7-939b-fddcc6eb880d}']
    function StartCapture: HResult; stdcall;
  end;

  { Static side of GraphicsCaptureSession - exposes the IsSupported() capability
    check. Obtained from the GraphicsCaptureSession activation factory. }
  IGraphicsCaptureSessionStatics = interface(IInspectable)
    ['{2224a540-5974-49aa-b232-0882536f4cb5}']
    function IsSupported(out value: ByteBool): HResult; stdcall;
  end;

  IGraphicsCaptureSession2 = interface(IInspectable)
    ['{2c39ae40-7d2e-5044-804e-8b6799d4cf9e}']
    function get_IsCursorCaptureEnabled(out value: ByteBool): HResult; stdcall;
    function put_IsCursorCaptureEnabled(value: ByteBool): HResult; stdcall;
  end;

  IGraphicsCaptureSession3 = interface(IInspectable)
    ['{f2cdd966-22ae-5ea1-9596-3a289344c3be}']
    function get_IsBorderRequired(out value: ByteBool): HResult; stdcall;
    function put_IsBorderRequired(value: ByteBool): HResult; stdcall;
  end;

  IDirect3D11CaptureFrame = interface(IInspectable)
    ['{fa50c623-38da-4b32-acf3-fa9734ad800e}']
    function get_Surface(out value: IDirect3DSurface): HResult; stdcall;
    function _r7_get_SystemRelativeTime: HResult; stdcall;   { padding - keeps get_ContentSize at the right vtable offset }
    function get_ContentSize(out value: TSizeInt32): HResult; stdcall;
  end;

  IDirect3D11CaptureFramePool = interface(IInspectable)
    ['{24eb6d22-1975-422e-82e7-780dbd8ddf24}']
    function _r6_Recreate: HResult; stdcall;
    function TryGetNextFrame(out frame: IDirect3D11CaptureFrame): HResult; stdcall;
    function _r8_add_FrameArrived: HResult; stdcall;
    function _r9_remove_FrameArrived: HResult; stdcall;
    function CreateCaptureSession(item: IGraphicsCaptureItem;
      out session: IGraphicsCaptureSession): HResult; stdcall;
  end;

  IDirect3D11CaptureFramePoolStatics2 = interface(IInspectable)
    ['{589b103f-6bbc-5df5-a991-02e28b3b66d5}']
    function CreateFreeThreaded(device: IDirect3DDevice; pixelFormat: Int32; numberOfBuffers: Int32; size: Int64; out pool: IDirect3D11CaptureFramePool): HResult; stdcall;
  end;

const
  RO_INIT_MULTITHREADED            = 1;

  D3D_DRIVER_TYPE_HARDWARE         = 1;
  D3D_DRIVER_TYPE_WARP             = 5;
  D3D11_SDK_VERSION                = 7;
  D3D11_CREATE_DEVICE_BGRA_SUPPORT = $20;

  D3D11_USAGE_STAGING              = 3;
  D3D11_CPU_ACCESS_READ            = $20000;
  D3D11_MAP_READ                   = 1;

  DXGI_FORMAT_B8G8R8A8_UNORM       = 87;

  RC_GraphicsCaptureItem: UnicodeString = 'Windows.Graphics.Capture.GraphicsCaptureItem';
  RC_GraphicsCaptureSession: UnicodeString = 'Windows.Graphics.Capture.GraphicsCaptureSession';
  RC_FramePool: UnicodeString = 'Windows.Graphics.Capture.Direct3D11CaptureFramePool';

  IID_IGraphicsCaptureItem: TGUID = '{79c3f95b-31f7-4ec2-a464-632ef5d30760}';
  IID_IGraphicsCaptureItemInterop: TGUID = '{3628e81b-3cac-4c60-b7f4-23ce0e0c3356}';
  IID_IGraphicsCaptureSessionStatics: TGUID = '{2224a540-5974-49aa-b232-0882536f4cb5}';
  IID_IDirect3D11CaptureFramePoolStatics2: TGUID = '{589b103f-6bbc-5df5-a991-02e28b3b66d5}';
  IID_ID3D11Texture2D: TGUID = '{6f15aaf2-d208-4e89-9ab4-489535d34f9c}';

var
  RoInitialize: function(initType: Integer): HResult; stdcall;
  RoGetActivationFactory: function(activatableClassId: Pointer; const iid: TGUID; out factory): HResult; stdcall;
  WindowsCreateStringReference: function(sourceString: PWideChar; length: UInt32; out hstringHeader: THStringHeader; out hstr: Pointer): HResult; stdcall;
  D3D11CreateDevice: function(pAdapter: Pointer; DriverType: UInt32; Software: Pointer; Flags: UInt32; pFeatureLevels: Pointer; FeatureLevels: UInt32; SDKVersion: UInt32; out ppDevice: ID3D11Device; pFeatureLevel: Pointer; out ppImmediateContext: ID3D11DeviceContext): HResult; stdcall;
  CreateDirect3D11DeviceFromDXGIDevice: function(dxgiDevice: IUnknown; out graphicsDevice: IInspectable): HResult; stdcall;

var
  HS_GraphicsCaptureItem: Pointer;
  HS_GraphicsCaptureSession: Pointer;
  HS_FramePool: Pointer;

function CanUseWGC: Boolean;

implementation

var
  GCombase, GD3D11: HMODULE;
  GBindingsOk: Boolean;
  GHdrItem, GHdrSession, GHdrPool: THStringHeader;
  GCanUseWGC: Integer = -1;

function DetectWGCSupported: Boolean;
var
  statics: IGraphicsCaptureSessionStatics;
  poolStatics2: IDirect3D11CaptureFramePoolStatics2;
  supported: ByteBool;
begin
  Result := False;
  if not GBindingsOk then
    Exit;
  RoInitialize(RO_INIT_MULTITHREADED);
  if RoGetActivationFactory(HS_GraphicsCaptureSession, IID_IGraphicsCaptureSessionStatics, statics) < 0 then
    Exit;
  supported := False;
  if statics.IsSupported(supported) < 0 then
    Exit;
  if not supported then
    Exit;
  Result := RoGetActivationFactory(HS_FramePool, IID_IDirect3D11CaptureFramePoolStatics2, poolStatics2) >= 0;
end;

function CanUseWGC: Boolean;
begin
  if GCanUseWGC < 0 then
    GCanUseWGC := Ord(DetectWGCSupported());
  Result := GCanUseWGC = 1;
end;

initialization
  GCombase := LoadLibrary('combase.dll');
  GD3D11 := LoadLibrary('d3d11.dll');

  if (GCombase <> 0) and (GD3D11 <> 0) then
  begin
    Pointer(RoInitialize) := GetProcAddress(GCombase, 'RoInitialize');
    Pointer(RoGetActivationFactory) := GetProcAddress(GCombase, 'RoGetActivationFactory');
    Pointer(WindowsCreateStringReference) := GetProcAddress(GCombase, 'WindowsCreateStringReference');
    Pointer(D3D11CreateDevice) := GetProcAddress(GD3D11, 'D3D11CreateDevice');
    Pointer(CreateDirect3D11DeviceFromDXGIDevice) := GetProcAddress(GD3D11, 'CreateDirect3D11DeviceFromDXGIDevice');

    GBindingsOk := Assigned(RoInitialize) and
                   Assigned(RoGetActivationFactory) and
                   Assigned(WindowsCreateStringReference) and
                   Assigned(D3D11CreateDevice) and
                   Assigned(CreateDirect3D11DeviceFromDXGIDevice);
  end;

  if GBindingsOk then
  begin
    WindowsCreateStringReference(PWideChar(RC_GraphicsCaptureItem), Length(RC_GraphicsCaptureItem), GHdrItem, HS_GraphicsCaptureItem);
    WindowsCreateStringReference(PWideChar(RC_GraphicsCaptureSession), Length(RC_GraphicsCaptureSession), GHdrSession, HS_GraphicsCaptureSession);
    WindowsCreateStringReference(PWideChar(RC_FramePool), Length(RC_FramePool), GHdrPool, HS_FramePool);
  end;
end.
