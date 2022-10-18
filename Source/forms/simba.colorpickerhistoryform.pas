{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.colorpickerhistoryform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ColorBox, StdCtrls,
  ExtCtrls, Grids;

type
  TSimbaColorPickerHistoryForm = class(TForm)
    ButtonExit: TButton;
    ButtonDelete: TButton;
    ButtonClear: TButton;
    ButtonPickColor: TButton;
    ColorListBox: TColorListBox;
    ColorPanel: TPanel;
    StringGrid: TStringGrid;
    procedure HandleButtonExitClick(Sender: TObject);
    procedure HandleButtonDeleteClick(Sender: TObject);
    procedure HandleButtonPickColorClick(Sender: TObject);
    procedure HandleButtonClearClick(Sender: TObject);
    procedure ColorListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure FontChanged(Sender: TObject); override;

    procedure SizeComponents;

    function GetColor(Index: Integer): TColor;
    function GetColorCount: Integer;
    function GetPoint(Index: Integer): TPoint;
  public
    procedure Add(APoint: TPoint; AColor: TColor);

    property ColorCount: Integer read GetColorCount;
    property Color[Index: Integer]: TColor read GetColor;
    property Point[Index: Integer]: TPoint read GetPoint;
  end;

var
  SimbaColorPickerHistoryForm: TSimbaColorPickerHistoryForm;

implementation

{$R *.lfm}

uses
  lcltype, types,
  simba.mufasatypes, simba.main, simba.colormath, simba.settings;

type
  TColorHistoryValue = packed record
    Point: TPoint;
    Color: TColor;
  end;

procedure TSimbaColorPickerHistoryForm.ColorListBoxSelectionChange(Sender: TObject; User: Boolean);
var
  R, G, B: Integer;
  H, S, L: Extended;
  Selected: Integer;
begin
  Selected := ColorListBox.ItemIndex;
  if (Selected < 0) then
    Exit;

  ColorToRGB(Color[Selected], R, G, B);
  ColorToHSL(Color[Selected], H, S, L);

  StringGrid.Cells[1, 0] := Format('%d', [Color[Selected]]);
  StringGrid.Cells[1, 1] := Format('$%s', [HexStr(Color[Selected], 6)]);
  StringGrid.Cells[1, 2] := Format('%d, %d, %d', [R, G, B]);
  StringGrid.Cells[1, 3] := Format('%.2f, %.2f, %.2f', [H, S, L]);
  StringGrid.Cells[1, 4] := Format('%d, %d', [Point[Selected].X, Point[Selected].Y]);
end;

procedure TSimbaColorPickerHistoryForm.HandleButtonClearClick(Sender: TObject);
begin
  if MessageDlg('Clear the entire list?', mtConfirmation, mbYesNo, 0) = mrYes then
    ColorListBox.Clear();
end;

procedure TSimbaColorPickerHistoryForm.HandleButtonDeleteClick(Sender: TObject);
begin
  if (ColorListBox.ItemIndex > -1) then
    ColorListBox.Items.Delete(ColorListBox.ItemIndex);
end;

procedure TSimbaColorPickerHistoryForm.HandleButtonExitClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaColorPickerHistoryForm.HandleButtonPickColorClick(Sender: TObject);
begin
  SimbaForm.ToolbarButtonColorPicker.Click();
end;

procedure TSimbaColorPickerHistoryForm.FormCreate(Sender: TObject);
var
  Value: TColorHistoryValue;
  Stream: TMemoryStream;
begin
  Width := Scale96ToScreen(500);
  Height := Scale96ToScreen(350);

  StringGrid.FocusRectVisible := False;
  StringGrid.EditorBorderStyle := bsNone;
  StringGrid.Editor.Color := clForm;

  ColorListBox.Options := ColorListBox.Options - [lboDrawFocusRect];

  Stream := TStringStream.Create(AnsiString(SimbaSettings.General.ColorPickerHistory.Value));
  while Stream.Read(Value, SizeOf(TColorHistoryValue)) = SizeOf(TColorHistoryValue) do
    Add(Value.Point, Value.Color);
  Stream.Free();

  SizeComponents();
end;

procedure TSimbaColorPickerHistoryForm.FormDestroy(Sender: TObject);
var
  I: Integer;
  Value: TColorHistoryValue;
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(AnsiString(''));

  for I := 0 to ColorCount - 1 do
  begin
    Value.Color := Color[I];
    Value.Point := Point[I];

    Stream.Write(Value, SizeOf(TColorHistoryValue));
  end;

  SimbaSettings.General.ColorPickerHistory.Value := Stream.DataString;

  Stream.Free();
end;

procedure TSimbaColorPickerHistoryForm.StringGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key <> VK_C) and (not (ssModifier in Shift)) then
    Key := VK_UNKNOWN;
end;

function TSimbaColorPickerHistoryForm.GetColor(Index: Integer): TColor;
begin
  Result := ColorListBox.Colors[Index];
end;

function TSimbaColorPickerHistoryForm.GetColorCount: Integer;
begin
  Result := ColorListBox.Count;
end;

function TSimbaColorPickerHistoryForm.GetPoint(Index: Integer): TPoint;
var
  Arr: TStringArray;
begin
  Result.X := -1;
  Result.Y := -1;

  Arr := ColorListBox.Items[Index].Between('(', ')').Split([', ']);
  if (Length(Arr) = 2) then
  begin
    Result.X := StrToIntDef(Arr[0], -1);
    Result.Y := StrToIntDef(Arr[1], -1);
  end;
end;

procedure TSimbaColorPickerHistoryForm.SizeComponents;
var
  Size: TSize;
  I: Integer;
begin
  if (ControlCount = 0) then
    Exit;

  with TBitmap.Create() do
  try
    Canvas.Font := Self.Font;
    Size := Canvas.TextExtent('TaylorSwift');
  finally
    Free();
  end;

  ColorPanel.Width := Round(Size.Width * 2.75);
  ColorListBox.ItemHeight := Size.Height;
  ColorListBox.ColorRectWidth := Size.Height-3;

  StringGrid.ColWidths[0] := Round(Size.Width * 0.65);
  for I := 0 to StringGrid.RowCount - 1 do
    StringGrid.RowHeights[I] := Size.Height;
end;

procedure TSimbaColorPickerHistoryForm.Add(APoint: TPoint; AColor: TColor);
begin
  ColorListBox.ItemIndex := ColorListBox.Items.AddObject('%d at (%d, %d)', [AColor, APoint.X, APoint.Y], TObject(PtrUInt(AColor)));
end;

procedure TSimbaColorPickerHistoryForm.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  SizeComponents();
end;

end.

