unit simba.showdeclarationform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, ImgList, ButtonPanel,
  simba.ide_codetools_parser, Types;

type
  TShowDeclarationForm = class(TForm)
    ButtonPanel: TButtonPanel;
    Grid: TStringGrid;

    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure GridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
    procedure GridPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure GridUserCheckboxImage(Sender: TObject; const aCol, aRow: Integer; const CheckedState: TCheckBoxState; var ImageList: TCustomImageList; var ImageIndex: TImageIndex);
    procedure OKButtonClick(Sender: TObject);
  public
    procedure Execute(Decls: TDeclarationArray);
  end;

var
  ShowDeclarationForm: TShowDeclarationForm;

implementation

uses
  simba.main, simba.ide_showdeclaration, simba.env;

{$R *.lfm}

procedure TShowDeclarationForm.GridGetCheckboxState(Sender: TObject; ACol, ARow: Integer; var Value: TCheckboxState);
begin
  if (ACol = 0) then
    Value := cbChecked;
end;

procedure TShowDeclarationForm.GridPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
begin
  if (gdRowHighlight in aState) then
  begin
    Grid.Canvas.Brush.Color := clHighlight;
    Grid.Canvas.Font.Color := clHighlightText;

    Grid.Canvas.FillRect(Grid.CellRect(aCol, aRow));
  end;
end;

procedure TShowDeclarationForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  { keep me for setting font color }
end;

procedure TShowDeclarationForm.GridDblClick(Sender: TObject);
begin
  ButtonPanel.OKButton.Click();
end;

procedure TShowDeclarationForm.GridUserCheckboxImage(Sender: TObject; const aCol, aRow: Integer; const CheckedState: TCheckBoxState; var ImageList: TCustomImageList; var ImageIndex: TImageIndex);
begin
  if (ACol = 0) then
  begin
    ImageList := SimbaForm.Images;

    if (Grid.Objects[aCol, aRow] is TDeclaration_Method) then
    begin
      if TDeclaration(Grid.Objects[aCol,aRow]).isFunction then
        ImageIndex := IMAGE_FUNCTION
      else
        ImageIndex := IMAGE_PROCEDURE;
    end;
  end;
end;

procedure TShowDeclarationForm.OKButtonClick(Sender: TObject);
begin
  if (Grid.Row >= 0) and (Grid.Row < Grid.RowCount) then
    ShowDeclaration(Grid.Objects[0, Grid.Row] as TDeclaration);
end;

procedure TShowDeclarationForm.Execute(Decls: TDeclarationArray);
var
  I: Integer;
begin
  Grid.BeginUpdate();
  Grid.FocusRectVisible := False;
  Grid.RowCount := Length(Decls) + Grid.FixedRows;
  for I := 0 to High(Decls) do
    if (Decls[I] is TDeclaration_Method) then
    begin
      Grid.Objects[0, Grid.FixedRows + I] := Decls[I]; // declarations are stored in [0, row]

      Grid.Cells[1, Grid.FixedRows + I] := TDeclaration_Method(Decls[I]).HeaderString;
      Grid.Cells[2, Grid.FixedRows + I] := TDeclaration_Method(Decls[I]).Line.ToString();
      Grid.Cells[3, Grid.FixedRows + I] := ExtractRelativePath(GetSimbaPath(), TDeclaration_Method(Decls[I]).Lexer.FileName);
    end;

  Grid.AutoSizeColumns();
  Grid.ColWidths[0] := 35;
  Grid.ColWidths[1] := Grid.ColWidths[1] + 70;
  Grid.ColWidths[2] := Grid.ColWidths[2] + 35;
  Grid.ColWidths[3] := Grid.ColWidths[3] + 70;
  Grid.EndUpdate();

  ShowModal();
end;

end.

