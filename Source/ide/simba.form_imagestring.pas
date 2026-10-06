{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Converting image to string.
}
unit simba.form_imagestring;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  simba.base,
  simba.ide_events,
  simba.image,
  simba.component_button;

type
  TSimbaImageStringForm = class(TForm)
  protected
    FString: String;
    FPreview: TImage;
    FPreviewHint: TLabel;
    FCountLabel: TLabel;
    FPadOutput: TSimbaLabeledCheckButton;
    FConvert: TSimbaButton;

    procedure SetImage(Image: TSimbaImage);
    procedure LoadImage(FileName: String);

    procedure DoOpenClick(Sender: TObject);
    procedure DoPasteClick(Sender: TObject);
    procedure DoConvertClick(Sender: TObject);
    procedure DoDropFiles(Sender: TObject; const FileNames: array of String);
    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create; reintroduce;
  end;

var
  SimbaImageStringForm: TSimbaImageStringForm;

implementation

uses
  Graphics, ExtDlgs, Clipbrd, LCLType,
  simba.initializations,
  simba.image_string,
  simba.image_lazbridge,
  simba.containers,
  simba.dialog,
  simba.component_theme;

procedure TSimbaImageStringForm.SetImage(Image: TSimbaImage);
begin
  FString := '';
  FCountLabel.Caption := '';
  try
    if Assigned(Image) and (Image.PixelCount > 0) then
    begin
      FString := SimbaImage_ToString(Image);

      // to the nearest thousand, once there are thousands
      if (Length(FString) < 1000) then
        FCountLabel.Caption := 'Character count: ' + IntToStr(Length(FString))
      else
        FCountLabel.Caption := 'Character count: ~' + FormatFloat('#,##0', ((Length(FString) + 500) div 1000) * 1000);
      FCountLabel.Caption := FCountLabel.Caption + Format(' (%d%% reduction)', [Trunc(100 - Length(FString) * 100 / (Image.PixelCount * SizeOf(TColorBGRA)))]);
    end;
  finally
    Image.Free();
  end;

  FPreviewHint.Visible := (FString = '');
  FConvert.Enabled := (FString <> '');
end;

procedure TSimbaImageStringForm.LoadImage(FileName: String);
begin
  try
    FPreview.Picture.LoadFromFile(FileName);

    SetImage(LazImage_ToSimbaImage(FPreview.Picture.Bitmap));
  except
    on E: Exception do
    begin
      FPreview.Picture.Clear();

      SetImage(nil);
      ShowErrorDialog('Image To String', 'Load image error: %s', [E.Message]);
    end;
  end;
end;

procedure TSimbaImageStringForm.DoOpenClick(Sender: TObject);
begin
  with TOpenPictureDialog.Create(Self) do
  try
    if Execute() then
      LoadImage(FileName);
  finally
    Free();
  end;
end;

procedure TSimbaImageStringForm.DoPasteClick(Sender: TObject);
begin
  if Clipboard.HasPictureFormat() then
  try
    FPreview.Picture.Bitmap.LoadFromClipboardFormat(Clipbrd.CF_Bitmap);

    SetImage(LazImage_ToSimbaImage(FPreview.Picture.Bitmap));
  except
    FPreview.Picture.Clear();

    SetImage(nil);
  end;
end;

// To the output box and the clipboard. The button is disabled while there is no image.
procedure TSimbaImageStringForm.DoConvertClick(Sender: TObject);
const
  PAD_WIDTH = 65;
var
  Code: TSimbaStringBuilder;
  I: Integer;
begin
  Code.AppendLine('Image := new TImage();');
  Code.Append('Image.FromString(');
  if FPadOutput.CheckButton.Down then
  begin
    I := 1;
    while (I <= Length(FString)) do
    begin
      if (I > 1) then
        Code.Append(' +');
      Code.AppendLine();
      Code.Append('  ' + #39 + Copy(FString, I, PAD_WIDTH) + #39);

      I := I + PAD_WIDTH;
    end;
  end else
    Code.Append(#39 + FString + #39);
  Code.Append(');');

  try
    Clipboard.AsText := Code.Str;
  except
  end;

  DebugLn(Code.Str);
  DebugLn(DEBUG_FOCUS);
end;

procedure TSimbaImageStringForm.DoDropFiles(Sender: TObject; const FileNames: array of String);
begin
  if (Length(FileNames) > 0) then
    LoadImage(FileNames[0]);
end;

procedure TSimbaImageStringForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.ACTION_IMG_TO_STRING:
      ShowOnTop();
  end;
end;

// Ctrl+V pastes
procedure TSimbaImageStringForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  if (Shift = [ssCtrl]) and (Key = VK_V) then
  begin
    Key := 0;
    DoPasteClick(nil);
  end;
end;

constructor TSimbaImageStringForm.Create;
var
  Buttons: TSimbaButtonGrid;
  Panel: TPanel;
  Row: TCustomControl;
begin
  inherited CreateNew(nil);

  Caption := 'Image To String';
  Color := SimbaComponentTheme.ColorFrame;
  Font.Color := SimbaComponentTheme.ColorFont;
  Position := poMainFormCenter;
  Width := Scale96ToScreen(500);
  Height := Scale96ToScreen(300);
  Constraints.MinWidth := Scale96ToScreen(280);
  Constraints.MinHeight := Scale96ToScreen(200);
  AllowDropFiles := True;
  OnDropFiles := @DoDropFiles;
  KeyPreview := True;

  Buttons := TSimbaButtonGrid.Create(Self);
  Buttons.Parent := Self;
  Buttons.Align := alTop;
  Buttons.BorderSpacing.Around := 5;
  Buttons.ChildSizing.EnlargeHorizontal := crsAnchorAligning; // their own width, not half the form each

  with TSimbaButton.Create(Self) do
  begin
    Parent := Buttons;
    Caption := 'Open Image';
    MeasureText := 'Paste Image'; // the same width as the other
    OnClick := @DoOpenClick;
  end;

  with TSimbaButton.Create(Self) do
  begin
    Parent := Buttons;
    Caption := 'Paste Image';
    OnClick := @DoPasteClick;
  end;

  // the bevel is the border
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alClient;
  Panel.BevelColor := SimbaComponentTheme.ColorScrollBarActive;
  Panel.BorderSpacing.Around := 5;
  Panel.Color := SimbaComponentTheme.ColorBackground;

  FPreviewHint := TLabel.Create(Self);
  FPreviewHint.Parent := Panel;
  FPreviewHint.Align := alClient;
  FPreviewHint.Alignment := taCenter;
  FPreviewHint.Layout := tlCenter;
  FPreviewHint.Caption := '(preview)';
  FPreviewHint.Font.Color := SimbaComponentTheme.ColorLine;

  FPreview := TImage.Create(Self);
  FPreview.Parent := Panel;
  FPreview.Align := alClient;
  FPreview.Center := True;
  FPreview.Proportional := True;
  FPreview.Stretch := True;

  // rows are made top to bottom
  Row := TCustomControl.Create(Self);
  Row.Parent := Self;
  Row.Align := alBottom;
  Row.AutoSize := True;
  Row.Color := SimbaComponentTheme.ColorFrame;

  FCountLabel := TLabel.Create(Self);
  FCountLabel.Parent := Row;
  FCountLabel.Align := alClient;
  FCountLabel.Layout := tlCenter;
  FCountLabel.BorderSpacing.Left := 5;

  FPadOutput := TSimbaLabeledCheckButton.Create(Self);
  FPadOutput.Parent := Row;
  FPadOutput.Align := alRight;
  FPadOutput.Caption := 'Pad output';

  FConvert := TSimbaButton.Create(Self);
  FConvert.Parent := Self;
  FConvert.Align := alBottom;
  FConvert.BorderSpacing.Around := 5;
  FConvert.Caption := 'Convert';
  FConvert.OnClick := @DoConvertClick;

  SetImage(nil);

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_IMG_TO_STRING]);
end;

procedure DoCreate;
begin
  SimbaImageStringForm := TSimbaImageStringForm.Create();
end;

procedure DoDestroy;
begin
  FreeAndNil(SimbaImageStringForm);
end;

initialization
  SimbaInitialization_Add(ESimbaInit.IDE_BEFORE_SHOW, @DoCreate, 'SimbaImageStringForm');
  SimbaInitialization_Add(ESimbaInit.IDE_DESTROY, @DoDestroy, 'SimbaImageStringForm');

end.
