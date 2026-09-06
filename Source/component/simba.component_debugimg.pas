{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_debugimg;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Forms, Graphics, syncobjs,
  simba.base,
  simba.component_imagebox;

type
  TSimbaDebugImageForm = class(TForm)
  protected type
    ESwapBufferFlags = set of (sbfResize, sbfEnsureVisible);
  protected
    FImageBox: TSimbaImageBox;
    FBackBuffer: TBitmap;
    FUpdating: TCriticalSection;

    FLastRepaint: Double;
    FNeedRepaint: Boolean;

    FMaxWidth, FMaxHeight: Integer;
    FSwapBufferFlags: ESwapBufferFlags;

    procedure SwapBuffers(DoResize, DoEnsureVisible: Boolean);

    procedure DoApplicationIsIdle(Sender: TObject; var Done: Boolean); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // Designed to be called off main thread to prevent lock ups.
    // Stream have the following format:
    //  - Width (Integer)
    //  - Height (Integer)
    //  - Resize (Boolean)
    //  - EnsureVisible (Boolean)
    //  - TColorBGRA image data equaling Width*Height*SizeOf(TColorBGRA)
    procedure UpdateFromStream(Stream: TStream); virtual;

    procedure Close; virtual;

    procedure SetMaxSize(AWidth, AHeight: Integer); virtual;
    procedure SetSize(AWidth, AHeight: Integer; AEnsureVisible: Boolean = True); virtual;

    property ImageBox: TSimbaImageBox read FImageBox;
  end;

implementation

uses
  simba.ide_docking,
  simba.image_lazbridge,
  simba.colormath,
  simba.datetime,
  simba.threading;

procedure TSimbaDebugImageForm.Close;
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  Form.Close();
end;

procedure TSimbaDebugImageForm.SwapBuffers(DoResize, DoEnsureVisible: Boolean);

  procedure DoSwapBuffers;
  var
    Temp: TBitmap;
  begin
    Temp := FImageBox.Background;
    FImageBox.Background := FBackBuffer;
    FBackBuffer := Temp;

    if (sbfResize in FSwapBufferFlags) then
      SetSize(FImageBox.Background.Width, FImageBox.Background.Height, sbfEnsureVisible in FSwapBufferFlags)
    else if (sbfEnsureVisible in FSwapBufferFlags) then
      SetSize(-1, -1, True);

    FNeedRepaint := True;
  end;

begin
  FSwapBufferFlags := [];
  if DoResize        then Include(FSwapBufferFlags, sbfResize);
  if DoEnsureVisible then Include(FSwapBufferFlags, sbfEnsureVisible);

  RunInMainThread(@DoSwapBuffers);
end;

procedure TSimbaDebugImageForm.DoApplicationIsIdle(Sender: TObject; var Done: Boolean);
begin
  if FNeedRepaint and ((HighResolutionTime() - FLastRepaint) >= 10) then // do not repaint yet if very recently done. not worth
  begin
    FImageBox.Invalidate();

    FNeedRepaint := False;
    FLastRepaint := HighResolutionTime();
  end;
end;

constructor TSimbaDebugImageForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FUpdating := TCriticalSection.Create();

  FMaxWidth := 1500;
  FMaxHeight := 1000;

  FImageBox := TSimbaImageBox.Create(Self);
  FImageBox.Parent := Self;
  FImageBox.Align := alClient;
  FImageBox.BackgroundOwner := False;

  Application.AddOnIdleHandler(@DoApplicationIsIdle);
end;

destructor TSimbaDebugImageForm.Destroy;
begin
  Application.RemoveOnIdleHandler(@DoApplicationIsIdle);

  if (FBackBuffer <> nil) then
    FreeAndNil(FBackBuffer);
  // The box holds the front buffer with BackgroundOwner=False. Freeing it here
  // would leave FImageBox.Background dangling, so hand ownership back and let the
  // box free it in its own destructor instead.
  if (FImageBox <> nil) then
    FImageBox.BackgroundOwner := True;
  if (FUpdating <> nil) then
    FreeAndNil(FUpdating);

  inherited Destroy();
end;

procedure TSimbaDebugImageForm.UpdateFromStream(Stream: TStream);
type
  TParams = packed record
    Width, Height: Integer;
    Resize: Boolean;
    EnsureVisible: Boolean;
  end;
var
  Params: TParams;

  Source, Dest: PByte;
  SourceUpper: PtrUInt;
  DestBytesPerLine, SourceBytesPerLine: Integer;

  procedure BGR;
  var
    Y: Integer;
  begin
    for Y := 0 to Params.Height - 1 do
    begin
      Stream.Read(Source^, SourceBytesPerLine);
      LazImage_CopyRow_BGR(PColorBGRA(Source), SourceUpper, PColorBGR(Dest));
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure BGRA;
  var
    Y: Integer;
  begin
    for Y := 0 to Params.Height - 1 do
    begin
      Stream.Read(Source^, SourceBytesPerLine);
      LazImage_CopyRow_BGRA(PColorBGRA(Source), SourceUpper, PColorBGRA(Dest));
      Inc(Dest, DestBytesPerLine);
    end;
  end;

  procedure ARGB;
  var
    Y: Integer;
  begin
    for Y := 0 to Params.Height - 1 do
    begin
      Stream.Read(Source^, SourceBytesPerLine);
      LazImage_CopyRow_ARGB(PColorBGRA(Source), SourceUpper, PColorARGB(Dest));
      Inc(Dest, DestBytesPerLine);
    end;
  end;

begin
  FUpdating.Enter();
  try
    Source := nil;

    if (FBackBuffer = nil) then
      FBackBuffer := TBitmap.Create();
    FBackBuffer.BeginUpdate();
    try
      Stream.Read(Params, SizeOf(TParams));

      FBackBuffer.SetSize(Params.Width, Params.Height);

      DestBytesPerLine := FBackBuffer.RawImage.Description.BytesPerLine;
      Dest             := FBackBuffer.RawImage.Data;

      SourceBytesPerLine := Params.Width * SizeOf(TColorBGRA);
      Source             := GetMem(SourceBytesPerLine);
      SourceUpper        := PtrUInt(Source + SourceBytesPerLine);

      case FImageBox.PixelFormat of
        ELazPixelFormat.BGR:  BGR();
        ELazPixelFormat.BGRA: BGRA();
        ELazPixelFormat.ARGB: ARGB();
        else
          SimbaException('Pixel format not supported: %d', [Ord(FImageBox.PixelFormat)]);
      end;
    finally
      FBackBuffer.EndUpdate();
      if (Source <> nil) then
        FreeMem(Source);
    end;

    SwapBuffers(Params.Resize, Params.EnsureVisible);
  finally
    FUpdating.Leave();
  end;
end;

procedure TSimbaDebugImageForm.SetSize(AWidth, AHeight: Integer; AEnsureVisible: Boolean);
var
  Form: TCustomForm;
begin
  Form := TCustomForm(Self);
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    Form := TSimbaAnchorDockHostSite(HostDockSite);

  if (AWidth > -1) and (AHeight > -1) then
  begin
    if (Form is TSimbaAnchorDockHostSite) and (TSimbaAnchorDockHostSite(Form).Header <> nil) then
    begin
      AHeight := AHeight + TSimbaAnchorDockHostSite(Form).Header.Height +
                           TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Top +
                           TSimbaAnchorDockHostSite(Form).Header.BorderSpacing.Bottom;
    end;
    AHeight := AHeight + FImageBox.StatusBar.Height;

    if (AWidth > Form.Width) then
      Form.Width := Min(AWidth, FMaxWidth);
    if (AHeight > Form.Height) then
      Form.Height := Min(AHeight, FMaxHeight);
  end;

  if AEnsureVisible then
    Form.EnsureVisible(True);
end;

procedure TSimbaDebugImageForm.SetMaxSize(AWidth, AHeight: Integer);
begin
  if (FMaxWidth = AWidth) and (FMaxHeight = AHeight) then
    Exit;

  // SetSize clamps against these whether docked or not, so they must always update
  FMaxWidth := AWidth;
  FMaxHeight := AHeight;

  // shrinking the live dock host only makes sense while actually docked
  if (HostDockSite is TSimbaAnchorDockHostSite) then
  begin
    if (HostDockSite.Width > FMaxWidth) then
      HostDockSite.Width := FMaxWidth;
    if (HostDockSite.Height > FMaxHeight) then
      HostDockSite.Height := FMaxHeight;
  end;
end;

end.

