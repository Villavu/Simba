{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_showdeclaration;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.ide_codetools_parser,
  simba.ide_codetools_insight;

procedure FindAndShowDeclaration(Script, ScriptFileName: String; CaretPos: Integer; What: String);

procedure ShowDeclaration(StartPos, EndPos, Line: Integer; FileName: String); overload;
procedure ShowDeclaration(Declaration: TDeclaration); overload;
procedure ShowSimbaDeclaration(Header: String; FileName: String);
procedure ShowPluginDeclaration(Header: String; FileName: String);

procedure ShowDeclarationDialog(Decls: TDeclarationArray);

implementation

uses
  Forms, Controls, Graphics, ATListbox,
  simba.env,
  simba.ide_theme,
  simba.component_buttonpanel,
  simba.form_main,
  simba.form_tabs;

type
  TShowDeclarationForm = class
  private
    FDecls: TDeclarationArray;
    FForm: TForm;
    FListBox: TATListBox;

    procedure DoOpenDeclaration(Sender: TObject);
    procedure DoDrawItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
  public
    constructor Create(Decls: TDeclarationArray);
  end;

procedure TShowDeclarationForm.DoOpenDeclaration(Sender: TObject);
begin
  if (FListBox.ItemIndex >= 0) and (FListBox.ItemIndex <= High(FDecls)) then
    ShowDeclaration(FDecls[FListBox.ItemIndex]);

  if (Sender = FListBox) then
    FForm.Close();
end;

procedure TShowDeclarationForm.DoDrawItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
var
  NIndentTop, NIndentLeft: Integer;
begin
  FListBox.DoDefaultDrawItem(C, AIndex, ARect);

  NIndentTop := (FListBox.ItemHeight - SimbaMainForm.Images.Height) div 2;
  NIndentLeft := ARect.Left - FListBox.ScrollHorz;
  NIndentLeft := NIndentLeft + (FListBox.ColumnWidth[0] - SimbaMainForm.Images.Width) div 2;

  SimbaMainForm.Images.Draw(C, NIndentLeft, ARect.Top + NIndentTop, DeclarationImage(FDecls[AIndex]));
end;

constructor TShowDeclarationForm.Create(Decls: TDeclarationArray);
var
  Items: array of record
    Decl: TDeclaration;
    Header: String;
    FileName: String;
    Line: Integer;
  end;
  MaxHeaderWidth, MaxFileWidth, MaxLineLength: Integer;

  procedure InitItems;
  var
    I, TextWidth: Integer;
    Bitmap: TBitmap;
  begin
    Bitmap := TBitmap.Create();
    Bitmap.Canvas.Font := FForm.Font;

    MaxHeaderWidth := Bitmap.Canvas.TextWidth('123456');
    MaxFileWidth   := MaxHeaderWidth;
    MaxLineLength  := MaxHeaderWidth;

    SetLength(Items, Length(FDecls));
    for I := 0 to High(FDecls) do
    begin
      Items[I].Decl := FDecls[I];
      Items[I].Header := FDecls[I].Header;
      Items[I].FileName := ExtractRelativePath(SimbaEnv.SimbaPath, FDecls[I].DocPos.FileName);
      Items[I].Line := FDecls[I].DocPos.Line;

      TextWidth := Bitmap.Canvas.TextWidth(Items[I].Header);
      if (TextWidth > MaxHeaderWidth) then
        MaxHeaderWidth := TextWidth;
      TextWidth := Bitmap.Canvas.TextWidth(Items[I].FileName);
      if (TextWidth > MaxFileWidth) then
        MaxFileWidth := TextWidth;
    end;

    Bitmap.Free();
  end;

var
  I: Integer;
  Cols: TATIntArray;
begin
  FDecls := Decls;

  FForm := TForm.Create(nil);
  FForm.Position := poMainFormCenter;
  FForm.Caption := 'Open Declaration';

  with TSimbaButtonPanel.Create(FForm) do
  begin
    Parent := FForm;
    ButtonOk.Caption := 'Open';
    ButtonOk.OnClick := @DoOpenDeclaration;
  end;

  FListBox := TATListbox.Create(FForm);
  FListBox.Parent := FForm;
  FListBox.Align := alClient;
  FListBox.CanGetFocus := True;
  FListBox.OwnerDrawn := True;
  FListBox.IndentLeft := 4;
  FListBox.OnDblClick := @DoOpenDeclaration;

  InitItems();

  Cols := [
    SimbaMainForm.Images.Width + FListBox.IndentLeft * 2,
    MaxHeaderWidth             + FListBox.IndentLeft * 4,
    MaxLineLength              + FListBox.IndentLeft * 4,
    MaxFileWidth               + FListBox.IndentLeft * 4
  ];

  FListBox.ColumnSizes := Cols;
  FListBox.ColumnSeparator := '|';
  FListBox.VirtualMode := False;
  FListBox.HeaderText := '|Header|Line|File';
  FListBox.OnDrawItem := @DoDrawItem;
  FListBox.ColorBgListbox := SimbaTheme.ColorBackground;
  FListBox.ColorBgListboxHeader := SimbaTheme.ColorFrame;
  FListBox.ColorBgListboxSel := SimbaTheme.ColorActive;
  FListBox.ColorFontListbox := SimbaTheme.ColorFont;
  FListBox.ColorFontListboxHeader := SimbaTheme.ColorFont;
  FListBox.ColorFontListboxSel := SimbaTheme.ColorFont;
  FListBox.ColorSeparators := SimbaTheme.ColorLine;

  for I := 0 to High(Items) do
    with Items[I] do
      FListBox.Items.Add('|' + Header + '|' + IntToStr(Line) + '|' + FileName);

  FForm.Width := FForm.Scale96ToScreen(650);
  FForm.Height := FForm.Scale96ToScreen(350);
  FForm.ShowModal();
  FForm.Free();
end;

procedure ShowDeclarationDialog(Decls: TDeclarationArray);
begin
  TShowDeclarationForm.Create(Decls).Free();
end;

procedure FindAndShowDeclaration(Script, ScriptFileName: String; CaretPos: Integer; What: String);
var
  Decl: TDeclaration;
  Decls: TDeclarationArray;
  Codeinsight: TCodeinsight;
begin
  Codeinsight := TCodeinsight.Create();

  try
    Codeinsight.SetScript(Script, ScriptFileName, CaretPos);
    Codeinsight.Run();

    Decl := Codeinsight.ParseExpr(What);
    if (Decl = nil) then
      Exit;

    // need to need to check for overloads
    if (Decl is TDeclaration_Method) then
    begin
      Decls := Codeinsight.Get(TDeclaration_MethodOfType(Decl).ObjectName).GetByClass(TDeclaration_Type);
      if (Length(Decls) > 0) then
        Decls := Codeinsight.GetTypeMembers(Decls[0] as TDeclaration_Type).GetByClassAndName(Decl.Name, TDeclaration_MethodOfType)
      else
        Decls := Codeinsight.Get(Decl.Name).GetByClassAndName(Decl.Name, TDeclaration_Method, True);

      if (Length(Decls) > 1) then
        ShowDeclarationDialog(Decls)
      else
      if (Length(Decls) = 1) then
        ShowDeclaration(Decls[0]);

      Exit;
    end;

    ShowDeclaration(Decl);
  finally
    Codeinsight.Free();
  end;
end;

procedure ShowDeclaration(StartPos, EndPos, Line: Integer; FileName: String);
begin
  if FileExists(FileName) then
    SimbaTabsForm.Open(FileName);

  with SimbaTabsForm.CurrentEditor do
  begin
    SelStart := StartPos;
    SelEnd := EndPos;
    TopLine := (Line + 1) - (LinesInWindow div 2);
    if CanSetFocus() then
      SetFocus();
  end;
end;

procedure ShowDeclaration(Declaration: TDeclaration);
begin
  if (Declaration.Parser.SourceType = EParserSourceType.PLUGIN) then
  begin
    DebugLn([EDebugLn.FOCUS], 'Declared internally in plugin: %s', [Declaration.DocPos.FileName]);
    DebugLn([EDebugLn.FOCUS], Declaration.Header);

    Exit;
  end;

  if (Declaration.DocPos.FileName = '') or FileExists(Declaration.DocPos.FileName) then
  begin
    if FileExists(Declaration.DocPos.FileName) then
      SimbaTabsForm.Open(Declaration.DocPos.FileName);

    with SimbaTabsForm.CurrentEditor do
    begin
      SelStart := Declaration.StartPos;
      SelEnd := Declaration.EndPos;
      TopLine := (Declaration.DocPos.Line + 1) - (LinesInWindow div 2);
      if CanSetFocus() then
        SetFocus();
    end;

    Exit;
  end;

  DebugLn([EDebugLn.FOCUS], 'Declared internally in Simba: %s', [Declaration.DocPos.FileName]);
  DebugLn([EDebugLn.FOCUS], Declaration.Header);
end;

procedure ShowSimbaDeclaration(Header: String; FileName: String);
begin
  if (Header = '') then
    Exit;

  DebugLn([EDebugLn.FOCUS], 'Declared internally in Simba: %s', [FileName]);
  DebugLn([EDebugLn.FOCUS], 'Declaration: %s', [Header]);
end;

procedure ShowPluginDeclaration(Header: String; FileName: String);
begin
  if (Header = '') then
    Exit;

  DebugLn([EDebugLn.FOCUS], 'Declared internally in plugin: %s', [FileName]);
  DebugLn([EDebugLn.FOCUS], 'Declaration: %s', [Header]);
end;

end.

