{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.component_listbox;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, Graphics, ATListbox,
  simba.base;

type
  TSimbaListBox = class(TCustomControl)
  public type
    TPaintItemEvent = procedure(Sender: TObject; C: TCanvas; AIndex: Integer; const ARect: TRect) of object;
    TSelectionChangeEvent = procedure(Sender: TObject; ItemIndex: Integer) of object;
  protected
    FListComponent: TATListbox;
    FColumns: TStringArray;
    FPaintItemEvent: TPaintItemEvent;
    FSelectionChangeEvent: TSelectionChangeEvent;

    procedure DoDblClick(Sender: TObject);
    procedure DoPaintItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure DoSelectionChange(Sender: TObject);

    function GetItemHeight: Integer;
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetColumns(Titles: TStringArray);
    procedure SetColumnWidths(Widths: TIntegerArray);
    procedure AddRow(Row: TStringArray);
    procedure Add(S: String);

    property OnPaintItem: TPaintItemEvent read FPaintItemEvent write FPaintItemEvent;
    property OnSelectionChange: TSelectionChangeEvent read FSelectionChangeEvent write FSelectionChangeEvent;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemHeight: Integer read GetItemHeight;
    property OnDblClick;
  end;

implementation

uses
  Forms,
  simba.component_theme,
  simba.initializations;

function TSimbaListBox.GetItemHeight: Integer;
begin
  Result := FListComponent.ItemHeight;
end;

procedure TSimbaListBox.DoDblClick(Sender: TObject);
begin
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;

procedure TSimbaListBox.DoPaintItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
begin
  FListComponent.DoDefaultDrawItem(C, AIndex, ARect);
  if Assigned(FPaintItemEvent) then
    FPaintItemEvent(Self, C, AIndex, ARect);
end;

procedure TSimbaListBox.DoSelectionChange(Sender: TObject);
begin
  if Assigned(FSelectionChangeEvent) then
    FSelectionChangeEvent(Self, FListComponent.ItemIndex);
end;

function TSimbaListBox.GetItemIndex: Integer;
begin
  Result := FListComponent.ItemIndex;
end;

procedure TSimbaListBox.SetItemIndex(Value: Integer);
begin
  FListComponent.ItemIndex := Value;
end;

constructor TSimbaListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FListComponent := TATListbox.Create(Self);
  FListComponent.Parent := Self;
  FListComponent.Align := alClient;
  FListComponent.OwnerDrawn := True;
  FListComponent.VirtualMode := False;
  FListComponent.CanGetFocus := True;
  FListComponent.ColorBgListbox := SimbaComponentTheme.ColorBackground;
  FListComponent.ColorBgListboxHeader := SimbaComponentTheme.ColorFrame;
  FListComponent.ColorBgListboxSel := SimbaComponentTheme.ColorActive;
  FListComponent.ColorFontListbox := SimbaComponentTheme.ColorFont;
  FListComponent.ColorFontListboxHeader := SimbaComponentTheme.ColorFont;
  FListComponent.ColorFontListboxSel := SimbaComponentTheme.ColorFont;
  FListComponent.ColorSeparators := SimbaComponentTheme.ColorLine;
  FListComponent.OnDblClick := @DoDblClick;
  FListComponent.OnDrawItem := @DoPaintItem;
  FListComponent.OnChangedSel := @DoSelectionChange;
end;

procedure TSimbaListBox.SetColumns(Titles: TStringArray);
var
  Str: String;
  I: Integer;
  ColumnSizes: TIntegerArray;
begin
  SetLength(ColumnSizes, Length(Titles));

  Str := '';
  for I := 0 to High(Titles) do
    Str := Str + Titles[I] + FListComponent.ColumnSeparator;

  FListComponent.HeaderText := Str;
  FListComponent.ColumnSizes := ColumnSizes;
end;

procedure TSimbaListBox.SetColumnWidths(Widths: TIntegerArray);
begin
  FListComponent.ColumnSizes := Widths;
end;

procedure TSimbaListBox.AddRow(Row: TStringArray);
var
  I, Len, CurrPos, ItemLen: Integer;
  NewRow: String;
begin
  Len := 0;
  for I := 0 to High(Row) do
    Inc(Len, Length(Row[I]) + 1);
  Dec(Len); // remove trailing sep
  if (Len <= 0) then
    Exit;

  SetLength(NewRow, Len);
  CurrPos := 1;
  for I := 0 to High(Row) do
  begin
    ItemLen := Length(Row[I]);
    if (ItemLen > 0) then
    begin
      Move(Row[I][1], NewRow[CurrPos], ItemLen);
      Inc(CurrPos, ItemLen);
    end;

    if (I < High(Row)) then
    begin
      NewRow[CurrPos] := FListComponent.ColumnSeparator;
      Inc(CurrPos);
    end;
  end;

  FListComponent.Items.Add(NewRow);
end;

procedure TSimbaListBox.Add(S: String);
begin
  FListComponent.Items.Add(S);
end;

end.

