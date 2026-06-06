{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_selectdeclform;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base,
  simba.ide_codetools_parser;

function SelectDeclaration(Decls: TDeclarationArray): TDeclaration;

implementation

uses
  Controls, Forms, Graphics,
  simba.component_buttonpanel,
  simba.component_listbox,
  simba.component_images;

type
  TShowDeclarationForm = class
  private
    FDecls: TDeclarationArray;
    FForm: TForm;
    FListBox: TSimbaListBox;

    procedure DoOpenDeclaration(Sender: TObject);
    procedure DoPaintItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
  public
    constructor Create(Decls: TDeclarationArray);
    destructor Destroy; override;

    function Execute: TDeclaration;
  end;

procedure TShowDeclarationForm.DoOpenDeclaration(Sender: TObject);
begin
  FForm.Close();
end;

procedure TShowDeclarationForm.DoPaintItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
begin
  SimbaImages.Draw(
    C,
    4, ARect.Top + (FListBox.ItemHeight - SimbaImages.Height) div 2,
    DeclarationImage(FDecls[AIndex])
  );
end;

constructor TShowDeclarationForm.Create(Decls: TDeclarationArray);
var
  I: Integer;
begin
  FDecls := Decls;

  FForm := TForm.Create(nil);
  FForm.Position := poMainFormCenter;
  FForm.Caption := 'Select Declaration';
  FForm.Width := FForm.Scale96ToScreen(650);
  FForm.Height := FForm.Scale96ToScreen(350);

  with TSimbaButtonPanel.Create(FForm) do
  begin
    Parent := FForm;
    ButtonOk.Caption := 'Open';
  end;

  FListBox := TSimbaListBox.Create(FForm);
  FListBox.Parent := FForm;
  FListBox.Align := alClient;
  FListBox.OnPaintItem := @DoPaintItem;
  FListBox.OnDblClick := @DoOpenDeclaration;
  FListBox.SetColumns(['', 'Header', 'Line', 'File']);
  FListBox.SetColumnWidths([20, 0, -7, -25]);
  for I := 0 to High(FDecls) do
    FListBox.AddRow([
      '',
      FDecls[I].Header,
      IntToStr(FDecls[I].DocPos.Line),
      ExtractFileName(FDecls[I].DocPos.FileName)
    ]);
end;

destructor TShowDeclarationForm.Destroy;
begin
  FreeAndNil(FForm);

  inherited Destroy();
end;

function TShowDeclarationForm.Execute: TDeclaration;
begin
  FForm.ShowModal();

  if (FListBox.ItemIndex >= 0) and (FListBox.ItemIndex < Length(FDecls)) then
    Result := FDecls[FListBox.ItemIndex]
  else
    Result := nil;
end;

function SelectDeclaration(Decls: TDeclarationArray): TDeclaration;
begin
  Result := nil;

  with TShowDeclarationForm.Create(Decls) do
  try
    Result := Execute();
  finally
    Free();
  end;
end;

end.

