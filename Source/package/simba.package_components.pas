unit simba.package_components;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, graphics, stdctrls, extctrls, grids,
  simba.package;

type
  TPackageListBox = class(TListBox)
  protected
    FOnInstallClick: TNotifyEvent;
    FOnAdvancedClick: TNotifyEvent;

    FImageList: TImageList;
    FBuffer: TBitmap;
    FHotInstallButtonIndex: Integer;
    FHotAdvancedButtonIndex: Integer;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FItemIndexFix: Integer; // https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/33618

    function GetSelected: TSimbaPackage;

    function GetInstallButtonRect(Rect: TRect): TRect;
    function GetAdvancedButtonRect(Rect: TRect): TRect;

    function GetInstallButtonRect(Index: Integer): TRect;
    function GetAdvancedButtonRect(Index: Integer): TRect;

    procedure SetEnabled(Value: Boolean); override;
    procedure FontChanged(Sender: TObject); override;
    procedure DoSelectionChange(User: Boolean); override;
    procedure DoMeasure;
    procedure DoDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; override;
    procedure Click; override;

    function Add(Package: TSimbaPackage): Integer;

    property Selected: TSimbaPackage read GetSelected;
    property OnInstallClick: TNotifyEvent read FOnInstallClick write FOnInstallClick;
    property OnAdvancedClick: TNotifyEvent read FOnAdvancedClick write FOnAdvancedClick;
    property ImageList: TImageList read FImageList write FImageList;
  end;

  TPackageVersionGrid = class(TStringGrid)
  protected
  type
    TVersionHeader = class
      Name: String;
      Age: String;
    end;
    TVersionInfo = class
      Notes: String;
      Wrapped: TStringArray;
    end;
  protected
    FOldWidth: Integer;

    FFixedCharWidth: Integer;
    FFixedCharHeight: Integer;

    procedure FontChanged(Sender: TObject); override;

    procedure DoOnResize; override;
    procedure DoLineWrapping(Foo: PtrInt);
    procedure DoFontCalculate;

    procedure DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState; aText: String); override;
    procedure DrawFocusRect(aCol, aRow: Integer; ARect: TRect); override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure Fill(Package: TSimbaPackage);
    procedure FillNoVersions;
  end;

  TPackageInfoGrid = class(TStringGrid)
  protected
    FHomepageHovered: Boolean;

    procedure GetAutoFillColumnInfo(const Index: Integer; var aMin, aMax, aPriority: Integer); override;
    procedure PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState); override;

    procedure Click; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure FontChanged(Sender: TObject); override;
    procedure Measure;
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetInfo(HomepageURL, InstalledVer, LatestVer: String);
  end;

implementation

uses
  lcltype, lclintf, themes, lazutf8, synedit,
  simba.helpers_string;

const
  pkgGridLineColor = $CCCCCC;
  pkgHeader        = $CCCCCC;
  pkgSubHeader     = $DEDEDE;

function TPackageListBox.GetSelected: TSimbaPackage;
begin
  if (ItemIndex = -1) then
    Result := nil
  else
    Result := Items.Objects[ItemIndex] as TSimbaPackage;
end;

function TPackageListBox.GetInstallButtonRect(Rect: TRect): TRect;
begin
  Result.Top    := Rect.CenterPoint.Y - 2 - FButtonHeight;
  Result.Bottom := Rect.CenterPoint.Y - 2;
  Result.Left   := Rect.Right - FButtonWidth - 10;
  Result.Right  := Rect.Right - 10;
end;

function TPackageListBox.GetAdvancedButtonRect(Rect: TRect): TRect;
begin
  Result.Top    := Rect.CenterPoint.Y + 2;
  Result.Bottom := Rect.CenterPoint.Y + 2 + FButtonHeight;
  Result.Left   := Rect.Right - FButtonWidth - 10;
  Result.Right  := Rect.Right - 10;
end;

function TPackageListBox.GetInstallButtonRect(Index: Integer): TRect;
begin
  Result := GetInstallButtonRect(ItemRect(Index));
end;

function TPackageListBox.GetAdvancedButtonRect(Index: Integer): TRect;
begin
  Result := GetAdvancedButtonRect(ItemRect(Index));
end;

procedure TPackageListBox.SetEnabled(Value: Boolean);
begin
  FHotAdvancedButtonIndex := -1;
  FHotInstallButtonIndex := -1;

  inherited SetEnabled(Value);
end;

procedure TPackageListBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  DoMeasure();
end;

procedure TPackageListBox.DoSelectionChange(User: Boolean);
begin
  if (ItemIndex = FItemIndexFix) then
    Exit;
  FItemIndexFix := ItemIndex;

  if Assigned(OnSelectionChange) then
    OnSelectionChange(Self, User);
end;

procedure TPackageListBox.DoMeasure;
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;

    ItemHeight := Round(Canvas.TextHeight('Fj') * 3.6);

    FButtonWidth := Round(Canvas.TextWidth('Custom Install') * 1.3);
    FButtonHeight := Round(Canvas.TextHeight('Fj') * 1.4);
  finally
    Free();
  end;
end;

procedure TPackageListBox.DoDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);

  procedure PaintButton(ButtonRect: TRect; ButtonText: String; ButtonType: TThemedButton; FontColor: TColor; FontStyles: TFontStyles);
  var
    TextStyle: TTextStyle;
  begin
    Dec(ButtonRect.Top, ARect.Top);
    Dec(ButtonRect.Bottom, ARect.Top);

    ThemeServices.DrawElement(FBuffer.Canvas.Handle, ThemeServices.GetElementDetails(ButtonType), ButtonRect);

    TextStyle := FBuffer.Canvas.TextStyle;
    TextStyle.Alignment := taCenter;
    TextStyle.Layout := tlCenter;
    TextStyle.Opaque := False;

    FBuffer.Canvas.Font.Color := FontColor;
    FBuffer.Canvas.Font.Style := FontStyles;
    FBuffer.Canvas.TextRect(ButtonRect, ButtonRect.Left, ButtonRect.Top, ButtonText, TextStyle);
  end;

  function CropText(Text: String; Area: TRect): String;
  var
    Test: TRect;
  begin
    Result := Text;

    while Length(Result) > 0 do
    begin
      Test := Area;
      DrawText(FBuffer.Canvas.Handle, PChar(Result), Length(Result), Test, DT_LEFT or DT_CALCRECT or DT_WORDBREAK);
      if Test.Bottom <= Area.Bottom then
        Break;

      SetLength(Result, Result.LastIndexOf(' ') - 1);
    end;
  end;

var
  Package: TSimbaPackage;
  TextStyle: TTextStyle;
  DescRect: TRect;
begin
  Package := Items.Objects[Index] as TSimbaPackage;

  FBuffer.SetSize(ARect.Width, ARect.Height);
  FBuffer.Canvas.Font := Self.Font;

  if (odSelected in State) then
    FBuffer.Canvas.Brush.Color := RGBToColor(159, 180, 208)
  else
    FBuffer.Canvas.Brush.Color := RGBToColor(240, 240, 240);

  FBuffer.Canvas.FillRect(0, 0, FBuffer.Width, FBuffer.Height);

  FImageList.Draw(FBuffer.Canvas, 10, (ItemHeight div 2) - (ImageList.Height div 2), 0);

  FBuffer.Canvas.Font.Color := clBlack;
  FBuffer.Canvas.Font.Bold := True;
  FBuffer.Canvas.TextOut(ImageList.Width + 20, 10, Package.Info.Name);

  if Package.HasUpdate() then
  begin
    if (FHotInstallButtonIndex = Index) then
      PaintButton(GetInstallButtonRect(ARect), 'Update', tbPushButtonHot, clBlack, [fsBold])
    else
      PaintButton(GetInstallButtonRect(ARect), 'Update', tbPushButtonNormal, clBlack, [fsBold]);

    if (FHotAdvancedButtonIndex = Index) then
      PaintButton(GetAdvancedButtonRect(ARect), 'Uninstall', tbPushButtonHot, clBlack, [])
    else
      PaintButton(GetAdvancedButtonRect(ARect), 'Uninstall', tbPushButtonNormal, clBlack, []);
  end
  else if Package.IsInstalled() then
  begin
    PaintButton(GetInstallButtonRect(ARect), 'Up to date', tbPushButtonDisabled, clGray, []);

    if (FHotAdvancedButtonIndex = Index) then
      PaintButton(GetAdvancedButtonRect(ARect), 'Uninstall', tbPushButtonHot, clBlack, [])
    else
      PaintButton(GetAdvancedButtonRect(ARect), 'Uninstall', tbPushButtonNormal, clBlack, []);
  end
  else
  begin
    if (FHotInstallButtonIndex = Index) then
      PaintButton(GetInstallButtonRect(ARect), 'Install', tbPushButtonHot, clBlack, [])
    else
      PaintButton(GetInstallButtonRect(ARect), 'Install', tbPushButtonNormal, clBlack, []);

    if (FHotAdvancedButtonIndex = Index) then
      PaintButton(GetAdvancedButtonRect(ARect), 'Custom Install', tbPushButtonHot, clBlack, [])
    else
      PaintButton(GetAdvancedButtonRect(ARect), 'Custom Install', tbPushButtonNormal, clBlack, []);
  end;

  DescRect.Top    := 10 + Canvas.TextHeight(Package.Info.Name);
  DescRect.Left   := 55+1;
  DescRect.Right  := FBuffer.Width - FButtonWidth - 25;
  DescRect.Bottom := FBuffer.Height - 10;

  FBuffer.Canvas.Font.Size := Canvas.Font.Size - 1;

  TextStyle := FBuffer.Canvas.TextStyle;
  TextStyle.Wordbreak := True;
  TextStyle.SingleLine := False;

  FBuffer.Canvas.TextRect(DescRect, DescRect.Left, DescRect.Top, CropText(Package.Info.Description, DescRect), TextStyle);
  FBuffer.Canvas.Font.Size := Canvas.Font.Size + 1;

  Canvas.Draw(ARect.Left, ARect.Top, FBuffer);
end;

procedure TPackageListBox.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  I, OldHotInstallButtonIndex, OldHotAdvancedButtonIndex: Integer;
begin
  OldHotInstallButtonIndex := FHotInstallButtonIndex;
  OldHotAdvancedButtonIndex := FHotAdvancedButtonIndex;

  FHotInstallButtonIndex := -1;
  FHotAdvancedButtonIndex := -1;

  for I := 0 to Items.Count - 1 do
  begin
    if GetInstallButtonRect(I).Contains(TPoint.Create(X, Y)) then
    begin
      FHotInstallButtonIndex := I;

      Break;
    end;

    if GetAdvancedButtonRect(I).Contains(TPoint.Create(X, Y)) then
    begin
      FHotAdvancedButtonIndex := I;

      Break;
    end;
  end;

  if (OldHotInstallButtonIndex <> FHotInstallButtonIndex) or (OldHotAdvancedButtonIndex <> FHotAdvancedButtonIndex) then
    Repaint();
end;

procedure TPackageListBox.Click;
begin
  inherited Click();

  if (FHotInstallButtonIndex > -1) and Assigned(FOnInstallClick) then
    FOnInstallClick(Self)
  else
  if (FHotAdvancedButtonIndex > -1) and Assigned(FOnAdvancedClick) then
    FOnAdvancedClick(Self);
end;

constructor TPackageListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBuffer := TBitmap.Create();
  FItemIndexFix := -1;

  FHotInstallButtonIndex := -1;
  FHotAdvancedButtonIndex := -1;

  Options := [];
  Style := lbOwnerDrawFixed;
  OnDrawItem := @DoDrawItem;
  OnMouseMove := @DoMouseMove;

  DoMeasure();
end;

destructor TPackageListBox.Destroy;
begin
  Clear();
  if (FBuffer <> nil) then
    FreeAndNil(FBuffer);

  inherited Destroy();
end;

procedure TPackageListBox.Clear;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    if Assigned(Items.Objects[I]) then
      Items.Objects[I].Free();

  inherited Clear();
end;

function TPackageListBox.Add(Package: TSimbaPackage): Integer;
begin
  Result := Items.AddObject('', Package);
end;

procedure TPackageInfoGrid.GetAutoFillColumnInfo(const Index: Integer; var aMin, aMax, aPriority: Integer);
begin
  inherited GetAutoFillColumnInfo(Index, aMin, aMax, aPriority);

  if (Index = 0) then
    aPriority := 0
  else
    aPriority := 1;
end;

procedure TPackageInfoGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  if (aRow = 0) then
    Canvas.Brush.Color := pkgHeader
  else
    Canvas.Brush.Color := clWhite;

  Canvas.Font.Bold := (aCol = 0);

  if (aCol = 1) and (aRow = 1) and FHomepageHovered then
    Canvas.Font.Color := clBlue
  else
    Canvas.Font.Color := clBlack;
end;

procedure TPackageInfoGrid.Click;
begin
  inherited Click();

  if FHomepageHovered then
    OpenURL(Cells[1, 1]);
end;

procedure TPackageInfoGrid.MouseLeave;
begin
  inherited MouseLeave();

  if FHomepageHovered then
  begin
    FHomepageHovered := False;

    Invalidate();
  end;
end;

procedure TPackageInfoGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ColIndex, RowIndex: Integer;
  WasHomepageHovered: Boolean;
begin
  inherited MouseMove(Shift, X, Y);

  MouseToCell(X, Y, ColIndex, RowIndex);

  WasHomepageHovered := FHomepageHovered;

  FHomepageHovered := (ColIndex = 1) and (RowIndex = 1);
  if (FHomepageHovered <> WasHomepageHovered) then
    Invalidate();
end;

procedure TPackageInfoGrid.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  Measure();
end;

procedure TPackageInfoGrid.Measure;
begin
  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Canvas.Font.Bold := True;

    ColWidths[0] := Canvas.TextWidth('Package Information :: ');
  finally
    Free();
  end;

  Height := CellRect(ColCount - 1, RowCount - 1).Bottom;
end;

constructor TPackageInfoGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FocusRectVisible := False;
  BorderStyle := bsNone;
  DisabledFontColor := clBlack;
  GridLineWidth := 2;
  GridLineColor := pkgGridLineColor;
  Scrollbars := ssNone;
  AutoFillColumns := True;

  BeginUpdate();

  FixedCols := 0;
  FixedRows := 0;

  RowCount := 4;
  ColCount := 2;

  Cells[0, 0] := 'Package Information';
  Cells[0, 1] := 'Homepage';
  Cells[0, 2] := 'Installed Version';
  Cells[0, 3] := 'Latest Version';

  EndUpdate();

  Measure();
end;

procedure TPackageInfoGrid.SetInfo(HomepageURL, InstalledVer, LatestVer: String);
begin
  if (InstalledVer = '') then
    InstalledVer := 'Not Installed';
  if (LatestVer = '') then
    LatestVer := 'Unknown';

  Cells[1, 1] := HomepageURL;
  Cells[1, 2] := InstalledVer;
  Cells[1, 3] := LatestVer;
end;

procedure TPackageVersionGrid.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  DoFontCalculate();
end;

procedure TPackageVersionGrid.DoOnResize;
begin
  inherited DoOnResize();

  Application.RemoveAsyncCalls(Self);
  Application.QueueAsyncCall(@DoLineWrapping, 0);
end;

procedure TPackageVersionGrid.DrawCellText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState; aText: String);
var
  Data: TObject;
  Info: TVersionInfo absolute Data;
  Header: TVersionHeader absolute Data;
  Line: String;
begin
  Data := Objects[aCol, aRow];

  if (Data is TVersionHeader) then
  begin
    Canvas.Font.Name := 'default';

    Inc(ARect.Top, varCellPadding);
    Inc(ARect.Left, varCellPadding);

    Canvas.Font.Bold := True;
    Canvas.TextOut(ARect.Left, ARect.Top, Header.Name);

    Inc(ARect.Left, Canvas.TextWidth(Header.Name + ' '));

    Canvas.Font.Bold := False;
    Canvas.Font.Italic := True;
    Canvas.TextOut(ARect.Left, ARect.Top, Header.Age);
    Canvas.Font.Italic := False;

    Exit;
  end;

  if (Data is TVersionInfo) then
  begin
    {$IFDEF WINDOWS}
    Canvas.Font.Name := 'Consolas';
    {$ELSE}
    Canvas.Font.Name := SynDefaultFontName;
    {$ENDIF}

    Inc(ARect.Top, varCellPadding div 2);
    Inc(ARect.Left, varCellPadding);

    for Line in Info.Wrapped do
    begin
      Canvas.TextOut(ARect.Left, ARect.Top, Line.TrimRight());

      Inc(ARect.Top, FFixedCharHeight);
    end;

    Exit;
  end;

  Canvas.Font.Name := 'default';

  inherited;
end;

procedure TPackageVersionGrid.DrawFocusRect(aCol, aRow: Integer; ARect: TRect);
begin
  { nothing }
end;

procedure TPackageVersionGrid.PrepareCanvas(aCol, aRow: Integer; aState: TGridDrawState);
var
  Data: TObject;
begin
  inherited PrepareCanvas(aCol, aRow, aState);

  Canvas.Brush.Color := clWhite;
  Canvas.Font.Color := clBlack;
  Canvas.Font.Bold := False;

  if (gdFixed in aState) then
  begin
    Canvas.Font.Bold := True;
    Canvas.Brush.Color := pkgHeader;

    Exit;
  end;

  Data := Objects[aCol, aRow];
  if (Data is TVersionHeader) then
    Canvas.Brush.Color := pkgSubHeader;
end;

constructor TPackageVersionGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Options := Options - [goEditing];
  BorderStyle := bsNone;
  DisabledFontColor := clBlack;
  GridLineWidth := 0;
  AutoFillColumns := True;
  ScrollBars := ssNone;
  Enabled := False;

  FixedCols := 0;
  FixedRows := 1;

  RowCount := 1;
  ColCount := 1;

  Cells[0, 0] := 'Package Version History';

  DoFontCalculate();
end;

destructor TPackageVersionGrid.Destroy;
begin
  Clear();

  inherited Destroy();
end;

procedure TPackageVersionGrid.Clear;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    if (Objects[0, I] <> nil) then
    begin
      Objects[0, I].Free();
      Objects[0, I] := nil;
    end;

  RowCount := FixedRows;
  Tag := Tag + 1;
  Height := GridHeight;
end;

procedure TPackageVersionGrid.Fill(Package: TSimbaPackage);
var
  Header: TVersionHeader;
  Info: TVersionInfo;
  Version: TSimbaPackageVersion;
  Versions: TSimbaPackageVersionArray;
  I: Integer;
begin
  Clear();

  Versions := Package.VersionsNoBranch;
  if Length(Versions) > 0 then
  begin
    RowCount := RowCount + Length(Versions) * 2;

    for I := FixedRows to RowCount - 1 do
    begin
      Version := Versions[(I - FixedRows) div 2];

      case Odd(I - FixedRows) of
        True:
          begin
            Info       := TVersionInfo.Create();
            Info.Notes := Version.Notes.Replace(#10, LineEnding).Trim();

            Objects[0, I] := Info;
          end;

        False:
          begin
            Header      := TVersionHeader.Create();
            Header.Name := Version.Name;
            Header.Age  := Version.Age;

            Objects[0, I] := Header;
          end;
      end;
    end;
  end else
    FillNoVersions();

  Application.QueueAsyncCall(@DoLineWrapping, 0);
end;

procedure TPackageVersionGrid.FillNoVersions;
begin
  Clear();

  RowCount := RowCount + 1;
  Cells[0, RowCount - 1] := '(no versions)';
  Height := GridHeight;
end;

procedure TPackageVersionGrid.DoLineWrapping(Foo: PtrInt);
var
  I, MaxCol: Integer;
  Data: TObject;
  Info: TVersionInfo absolute Data;
begin
  BeginUpdate();

  try
    MaxCol := (Width div FFixedCharWidth) - 2;

    for I := FixedRows to RowCount - 1 do
    begin
      Data := Objects[0, I];

      if Data is TVersionInfo then
      begin
        Info.Wrapped := UTF8WrapText(Info.Notes, MaxCol).Split(LineEnding);

        RowHeights[I] := (Length(Info.Wrapped) * FFixedCharHeight) + varCellPadding;
      end;
    end;
  finally
    EndUpdate();
  end;

  Height := GridHeight;
end;

procedure TPackageVersionGrid.DoFontCalculate;
begin
  with TBitmap.Create() do
  try
    {$IFDEF WINDOWS}
    Canvas.Font.Name := 'Consolas';
    {$ELSE}
    Canvas.Font.Name := SynDefaultFontName;
    {$ENDIF}

    FFixedCharWidth := Canvas.TextWidth('i');
    FFixedCharHeight := Canvas.TextHeight('i');
  finally
    Free();
  end;
end;

end.

