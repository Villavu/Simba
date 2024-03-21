{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_editor_modifiedlinegutter;

{$i simba.inc}

interface

uses
  classes, sysutils, graphics,
  synedit, syneditkeycmds, syngutterlineoverview;

type
  TSimbaEditorModifiedLinesGutter = class(TSynGutterLOvProviderModifiedLines)
  protected
    FPaintMarks: Boolean;
    FLineMarksCount: Integer;
    FLineMarks: array of record
      Line: Integer;
      Color: Integer;
    end;

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); override;
  public
    procedure ReCalc; reintroduce;

    procedure HideMarks;
    procedure ShowMarks;
    procedure ClearMarks;

    procedure AddMark(const ALine, AColor: Integer);
  end;

implementation

uses
  simba.ide_editor;

procedure TSimbaEditorModifiedLinesGutter.Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer);
var
  I, Y1, Y2: Integer;
begin
  if (SynEdit is TSimbaEditor) then
    AClip.Right := TSimbaEditor(SynEdit).RightGutter.Width*3;

  inherited Paint(Canvas, AClip, TopOffset);

  if FPaintMarks then
  begin
    for I := 0 to FLineMarksCount - 1 do
      with FLineMarks[I] do
      begin
        Y1 := TextLineToPixel(Line) - 2;
        Y2 := TextLineToPixelEnd(Line) + 2;

        Canvas.Brush.Color := Color;
        Canvas.FillRect(AClip.Left, Y1, AClip.Right, Y2);
      end;
  end;
end;

procedure TSimbaEditorModifiedLinesGutter.ReCalc;
begin
  inherited ReCalc();
end;

procedure TSimbaEditorModifiedLinesGutter.HideMarks;
begin
  FPaintMarks := False;

  if (SynEdit is TSimbaEditor) then
    TSimbaEditor(SynEdit).InvalidateGutter();
end;

procedure TSimbaEditorModifiedLinesGutter.ShowMarks;
begin
  FPaintMarks := True;

  if (SynEdit is TSimbaEditor) then
    TSimbaEditor(SynEdit).InvalidateGutter();
end;

procedure TSimbaEditorModifiedLinesGutter.AddMark(const ALine, AColor: Integer);
begin
  if (Length(FLineMarks) = FLineMarksCount) then
    SetLength(FLineMarks, 4 + (Length(FLineMarks) * 2));

  FLineMarks[FLineMarksCount].Color := AColor;
  FLineMarks[FLineMarksCount].Line := ALine;

  Inc(FLineMarksCount);
end;

procedure TSimbaEditorModifiedLinesGutter.ClearMarks;
begin
  FLineMarksCount := 0;
end;

end.

