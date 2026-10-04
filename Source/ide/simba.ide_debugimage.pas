{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  The debug image and debug matrix windows, which running scripts send
  frames to from simba.ide_scriptcommunication.
}
unit simba.ide_debugimage;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, syncobjs,
  simba.base,
  simba.image,
  simba.ide_events,
  simba.component_imagebox;

type
  TSimbaDebugImage = class(TForm)
  protected
    FImageBox: TSimbaImageBox;
    FMaxWidth, FMaxHeight: Integer;
    FLock: TCriticalSection;
    FBackBuffer: TSimbaImage;
    FBackBufferResize: Boolean;
    FBackBufferEnsureVisible: Boolean;
    FSwapBufferQueued: Boolean;

    function HostForm: TCustomForm;
    procedure DoViewEvent(Event: ESimbaEvent; Data: Pointer);
    procedure DoSwapBuffers;
    procedure DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer); virtual;
  public
    constructor Create(AOwner: TComponent; ViewEvent: ESimbaEvent); reintroduce;
    destructor Destroy; override;

    procedure BeginUpdate(AWidth, AHeight: Integer);
    procedure EndUpdate(AResize, AEnsureVisible: Boolean);

    procedure Display(AWidth, AHeight: Integer; AResize: Boolean = True; AEnsureVisible: Boolean = True); overload;
    procedure Display(X, Y, AWidth, AHeight: Integer); overload;
    // the largest the actual image part gets
    procedure SetMaxSize(AWidth, AHeight: Integer);
    procedure Close;

    property BackBuffer: TSimbaImage read FBackBuffer;
  end;

  TSimbaDebugMatrix = class(TSimbaDebugImage)
  protected
    FMatrix: TSingleMatrix;

    // False outside the matrix, and while it is not that of the image shown
    function GetValue(X, Y: Integer; out Value: Single): Boolean;

    procedure DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
    procedure DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent; ViewEvent: ESimbaEvent); reintroduce;

    property Matrix: TSingleMatrix read FMatrix write FMatrix;
  end;

var
  SimbaDebugImageForm: TSimbaDebugImage;
  SimbaDebugMatrixForm: TSimbaDebugMatrix;

implementation

uses
  Controls, Math,
  simba.ide_docking,
  simba.initializations,
  simba.threading,
  simba.colormath,
  simba.vartype_matrix;

function TSimbaDebugImage.HostForm: TCustomForm;
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Result := TSimbaAnchorDockHostSite(HostDockSite)
  else
    Result := Self;
end;

procedure TSimbaDebugImage.DoViewEvent(Event: ESimbaEvent; Data: Pointer);
begin
  SimbaDocking.Show(Self);
end;

procedure TSimbaDebugImage.DoSwapBuffers;
var
  DoResize, DoEnsureVisible: Boolean;
begin
  FLock.Enter();
  try
    FSwapBufferQueued := False;

    FBackBuffer := FImageBox.SwapBackground(FBackBuffer);

    DoResize := FBackBufferResize;
    DoEnsureVisible := FBackBufferEnsureVisible;

    FBackBufferResize := False;
    FBackBufferEnsureVisible := False;
  finally
    FLock.Leave();
  end;

  Display(FImageBox.Background.Width, FImageBox.Background.Height, DoResize, DoEnsureVisible);
end;

procedure TSimbaDebugImage.DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer);
begin
  if (X >= 0) and (X < FImageBox.Background.Width) and (Y >= 0) and (Y < FImageBox.Background.Height) then
  begin
    DebugLn('Pixels[%d,%d] := %s', [X, Y, ColorToStr(FImageBox.Background.Pixel[X, Y])]);
    DebugLn(DEBUG_FOCUS);
  end;
end;

constructor TSimbaDebugImage.Create(AOwner: TComponent; ViewEvent: ESimbaEvent);
begin
  inherited Create(AOwner);

  FMaxWidth := 1500;
  FMaxHeight := 1000;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.OnImgDoubleClick := @DoImgDoubleClick;

  FLock := TCriticalSection.Create();
  FBackBuffer := TSimbaImage.Create();

  SimbaEvents.Register(Self, @DoViewEvent, [ViewEvent]);
end;

destructor TSimbaDebugImage.Destroy;
begin
  TThread.RemoveQueuedEvents(@DoSwapBuffers);

  FreeAndNil(FBackBuffer);
  FreeAndNil(FLock);

  inherited Destroy();
end;

procedure TSimbaDebugImage.BeginUpdate(AWidth, AHeight: Integer);
begin
  FLock.Enter();
  try
    FBackBuffer.SetSize(AWidth, AHeight);
  except
    FLock.Leave();
    raise;
  end;
end;

procedure TSimbaDebugImage.EndUpdate(AResize, AEnsureVisible: Boolean);
begin
  FBackBufferResize := FBackBufferResize or AResize;
  FBackBufferEnsureVisible := FBackBufferEnsureVisible or AEnsureVisible;

  // ensure we only queue one at a time
  if not FSwapBufferQueued then
  begin
    FSwapBufferQueued := True;

    TThread.Queue(nil, @DoSwapBuffers);
  end;

  FLock.Leave();
end;

procedure TSimbaDebugImage.Display(AWidth, AHeight: Integer; AResize: Boolean; AEnsureVisible: Boolean);

  procedure Fit;
  var
    Form: TCustomForm;
    ViewWidth, ViewHeight: Integer;
    NewWidth, NewHeight: Integer;
  begin
    Form := HostForm();
    if (not AResize) or (Form.WindowState <> wsNormal) then
      Exit;

    if Form.Showing then
    begin
      ViewWidth := FImageBox.ViewWidth;
      ViewHeight := FImageBox.ViewHeight;
    end else
    begin
      ViewWidth := Form.Width;
      ViewHeight := Form.Height;
    end;

    // at least the image, at most the max size
    NewWidth := Min(Max(ViewWidth, AWidth), FMaxWidth);
    NewHeight := Min(Max(ViewHeight, AHeight), FMaxHeight);

    Form.SetBounds(
      Form.Left,
      Form.Top,
      Form.Width + (NewWidth - ViewWidth),
      Form.Height + (NewHeight - ViewHeight)
    );
  end;

  procedure Execute;
  begin
    Fit();

    if AEnsureVisible then
    begin
      SimbaDocking.Show(Self);

      Fit();
    end;
  end;

begin
  RunInMainThread(@Execute);
end;

procedure TSimbaDebugImage.Display(X, Y, AWidth, AHeight: Integer);

  procedure Execute;
  begin
    Display(AWidth, AHeight);

    HostForm().Left := X;
    HostForm().Top := Y;
  end;

begin
  RunInMainThread(@Execute);
end;

procedure TSimbaDebugImage.SetMaxSize(AWidth, AHeight: Integer);

  procedure Execute;
  begin
    FMaxWidth := AWidth;
    FMaxHeight := AHeight;

    Display(0, 0, True, False);
  end;

begin
  RunInMainThread(@Execute);
end;

procedure TSimbaDebugImage.Close;

  procedure Execute;
  begin
    HostForm().Close();
  end;

begin
  RunInMainThread(@Execute);
end;

function TSimbaDebugMatrix.GetValue(X, Y: Integer; out Value: Single): Boolean;
begin
  Value := 0;
  Result := False;

  // not while the next frame is being written
  if not FLock.TryEnter() then
    Exit;
  try
    // nor until it is swapped in
    Result := (not FSwapBufferQueued) and (X >= 0) and (X < FMatrix.Width) and (Y >= 0) and (Y < FMatrix.Height);
    if Result then
      Value := FMatrix[Y, X];
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugMatrix.DoImgMouseMove(Sender: TSimbaImageBox; Shift: TShiftState; X, Y: Integer);
var
  Value: Single;
begin
  if GetValue(X, Y, Value) then
    FImageBox.Status := Format('Matrix[%d,%d] := %.4f', [Y, X, Value]);
end;

procedure TSimbaDebugMatrix.DoImgDoubleClick(Sender: TSimbaImageBox; X, Y: Integer);
var
  Value: Single;
begin
  if GetValue(X, Y, Value) then
  begin
    DebugLn('Matrix[%d,%d] := %.4f', [Y, X, Value]);
    DebugLn(DEBUG_FOCUS);
  end;
end;

constructor TSimbaDebugMatrix.Create(AOwner: TComponent; ViewEvent: ESimbaEvent);
begin
  inherited Create(AOwner, ViewEvent);

  FImageBox.OnImgMouseMove := @DoImgMouseMove;
end;

procedure DoCreate;
begin
  SimbaDebugImageForm := TSimbaDebugImage.Create(Application, ESimbaEvent.ACTION_VIEW_DEBUGIMAGE);
  SimbaDebugImageForm.Caption := 'Debug Image';

  SimbaDebugMatrixForm := TSimbaDebugMatrix.Create(Application, ESimbaEvent.ACTION_VIEW_DEBUGMATRIX);
  SimbaDebugMatrixForm.Caption := 'Debug Matrix';
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaDebugImageForm);
  FreeAndNil(SimbaDebugMatrixForm);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_CREATE, @DoCreate, 'DebugImage');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'DebugImage');

end.
