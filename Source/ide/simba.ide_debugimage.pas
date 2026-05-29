{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_debugimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.ide_events,
  simba.component_debugimg,
  simba.component_imagebox;

type
  TSimbaDebugImage = class(TSimbaDebugImageForm)
  protected
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  TSimbaDebugMatrix = class(TSimbaDebugImageForm)
  protected
    FMatrix: TSingleMatrix;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
    procedure DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer);
  public
    procedure UpdateFromStream(Stream: TStream); override;

    constructor Create(TheOwner: TComponent); override;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImage;
  SimbaDebugMatrixForm: TSimbaDebugMatrix;

implementation

uses
  Forms,
  Menus,
  AnchorDocking,
  Graphics,
  simba.ide_docking,
  simba.initializations,
  simba.vartype_matrix,
  simba.image_lazbridge,
  simba.image_drawmatrix,
  simba.colormath,
  simba.threading;

procedure TSimbaDebugImage.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoViewDebugImage(Item: TMenuItem);
  begin
    DockMaster.Show(Self);
  end;

begin
  case Event of
    ESimbaEvent.ACTION_VIEW_DEBUGIMAGE: DoViewDebugImage(TMenuItem(Data));
  end;
end;

constructor TSimbaDebugImage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_VIEW_DEBUGIMAGE]);
end;

procedure TSimbaDebugMatrix.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

  procedure DoViewDebugMatrix(Item: TMenuItem);
  begin
    DockMaster.Show(Self);
  end;

begin
  case Event of
    ESimbaEvent.ACTION_VIEW_DEBUGMATRIX: DoViewDebugMatrix(TMenuItem(Data));
  end;
end;

procedure TSimbaDebugMatrix.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 0) and (X < FMatrix.Width) and (Y >= 0) and (Y < FMatrix.Height) then
    ImageBox.Status := Format('Matrix[%d,%d] := %.5f', [Y, X, FMatrix[Y,X]]);
end;

procedure TSimbaDebugMatrix.DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer);
begin
  if (X >= 0) and (X < FMatrix.Width) and (Y >= 0) and (Y < FMatrix.Height) then
  begin
    DebugLn('Matrix[%d,%d] := %.5f', [Y, X, FMatrix[Y,X]]);
    DebugLn(DEBUG_FOCUS);
  end;
end;

generic procedure DrawMatrix<PPixelType>(Matrix: TSingleMatrix; ColorMapType, Width, Height: Integer; Dest: PByte; DestBytesPerLine: Integer);
var
  X, Y: Integer;
  Ptr: PPixelType;
begin
  Dec(Height);
  Dec(Width);
  for Y := 0 to Height do
  begin
    Ptr := PPixelType(Dest);
    for X := 0 to Width do
    begin
      GetMatrixColor(Matrix[Y, X], ColorMapType, Ptr^.R, Ptr^.G, Ptr^.B);
      Inc(Ptr);
    end;
    Inc(Dest, DestBytesPerLine);
  end;
end;

procedure TSimbaDebugMatrix.UpdateFromStream(Stream: TStream);
type
  TParams = packed record
    Width, Height: Integer;
    Resize: Boolean;
    EnsureVisible: Boolean;
    ColorMapType: Integer;
  end;
var
  Params: TParams;
  Y: Integer;
begin
  FUpdating.Enter();

  Stream.Read(Params, SizeOf(TParams));
  FMatrix.SetSize(Params.Width, Params.Height);
  for Y := 0 to Params.Height - 1 do
    Stream.Read(FMatrix[Y, 0], Params.Width * SizeOf(Single));
  FMatrix := FMatrix.NormMinMax(0, 1);

  try
    if (FBackBuffer = nil) then
      FBackBuffer := TBitmap.Create();
    FBackBuffer.BeginUpdate();
    try
      FBackBuffer.SetSize(Params.Width, Params.Height);
      case FImageBox.PixelFormat of
        ELazPixelFormat.BGR:  specialize DrawMatrix<PColorBGR>(FMatrix,  Params.ColorMapType, Params.Width, Params.Height, FBackBuffer.RawImage.Data, FBackBuffer.RawImage.Description.BytesPerLine);
        ELazPixelFormat.BGRA: specialize DrawMatrix<PColorBGRA>(FMatrix, Params.ColorMapType, Params.Width, Params.Height, FBackBuffer.RawImage.Data, FBackBuffer.RawImage.Description.BytesPerLine);
        ELazPixelFormat.ARGB: specialize DrawMatrix<PColorARGB>(FMatrix, Params.ColorMapType, Params.Width, Params.Height, FBackBuffer.RawImage.Data, FBackBuffer.RawImage.Description.BytesPerLine);
        else
          SimbaException('Pixel format supported: %d', [Ord(FImageBox.PixelFormat)]);
      end;
    finally
      FBackBuffer.EndUpdate();
    end;

    SwapBuffers(Params.Resize, Params.EnsureVisible);
  finally
    FUpdating.Leave();
  end;
end;

constructor TSimbaDebugMatrix.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FImageBox.OnImgMouseMove := @DoImgMouseMove;
  FImageBox.OnImgDoubleClick := @DoImgDoubleClick;

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_VIEW_DEBUGMATRIX]);
end;

procedure DoCreate;
begin
  SimbaDebugImageForm := TSimbaDebugImage.Create(Application);
  SimbaDebugImageForm.Name := 'SimbaDebugImage';
  SimbaDebugImageForm.Caption := 'Debug Image';

  SimbaDebugMatrixForm := TSimbaDebugMatrix.Create(Application);
  SimbaDebugMatrixForm.Name := 'SimbaDebugMatrix';
  SimbaDebugMatrixForm.Caption := 'Debug Matrix';
end;

procedure DoDestroy;
begin
  SimbaDebugImageForm.Free();
  SimbaDebugMatrixForm.Free();
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'DebugImage');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'DebugImage');
end.

