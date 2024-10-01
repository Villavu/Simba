{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dialog;

{$i simba.inc}

interface

uses
  Classes, SysUtils, StdCtrls, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLType;

type
  {$PUSH}
  {$SCOPEDENUMS ON}
  ESimbaDialogResult = (CANCEL, YES, NO);
  {$POP}

function SimbaQuestionDlg(Title: String; Question: String; Args: array of const): ESimbaDialogResult; overload;
function SimbaQuestionDlg(Title: String; Question: TStringArray; Args: array of const): ESimbaDialogResult; overload;

procedure SimbaErrorDlg(Title: String; Err: String; Args: array of const); overload;
procedure SimbaErrorDlg(Title: String; Err: TStringArray; Args: array of const); overload;

implementation

type
  TSimbaDialogForm = class(TForm)
    ButtonCancel: TButton;
    ButtonNo: TButton;
    ButtonYes: TButton;
    Image: TImage;
    QuestionLabel: TLabel;
    MessagePanel: TPanel;
    ButtonPanel: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
    procedure ButtonYesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean); override;
  public
    DialogResult: ESimbaDialogResult;
  end;
  TSimbaDialogClass = class of TSimbaDialogForm;

  TSimbaQuestionDialogForm = class(TSimbaDialogForm);
  TSimbaErrorDialogForm = class(TSimbaDialogForm);

function ShowDialog(Typ: TSimbaDialogClass; Title, Msg: String; Args: array of const): ESimbaDialogResult;
begin
  with Typ.Create(nil) do
  try
    Caption := Title;
    QuestionLabel.Caption := Format(Msg, Args);
    ShowModal();

    Result := DialogResult;
  finally
    Free();
  end;
end;

procedure TSimbaDialogForm.FormCreate(Sender: TObject);
begin
  if (Self is TSimbaErrorDialogForm) then
  begin
    Image.Picture.Bitmap := TBitmap(GetDialogIcon(idDialogError));

    ButtonCancel.Hide();
    ButtonNo.Hide();
    ButtonYes.Caption := 'Ok';
  end else
  if (Self is TSimbaQuestionDialogForm) then
    Image.Picture.Bitmap := TBitmap(GetDialogIcon(idDialogConfirm));

  DialogResult := ESimbaDialogResult.CANCEL; // default, if close button was clicked.
end;

type
  TLabelProtectedAccess = class(TLabel);

procedure TSimbaDialogForm.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; WithThemeSpace: Boolean);
var
  W, H: Integer;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

  W := 0; H := 0;
  TLabelProtectedAccess(QuestionLabel).CalculateSize(Monitor.Width div 2, W, H);
  PreferredWidth := QuestionLabel.Left + QuestionLabel.BorderSpacing.Right + W;
end;

procedure TSimbaDialogForm.ButtonNoClick(Sender: TObject);
begin
  DialogResult := ESimbaDialogResult.NO;
  Close();
end;

procedure TSimbaDialogForm.ButtonCancelClick(Sender: TObject);
begin
  DialogResult := ESimbaDialogResult.CANCEL;
  Close();
end;

procedure TSimbaDialogForm.ButtonYesClick(Sender: TObject);
begin
  DialogResult := ESimbaDialogResult.YES;
  Close();
end;

function SimbaQuestionDlg(Title: String; Question: String; Args: array of const): ESimbaDialogResult;
begin
  Result := ShowDialog(TSimbaQuestionDialogForm, Title, Question, Args);
end;

function SimbaQuestionDlg(Title: String; Question: TStringArray; Args: array of const): ESimbaDialogResult;
begin
  Result := ShowDialog(TSimbaQuestionDialogForm, Title, ''.Join(LineEnding, Question), Args);
end;

procedure SimbaErrorDlg(Title: String; Err: String; Args: array of const);
begin
  ShowDialog(TSimbaErrorDialogForm, Title, Err, Args);
end;

procedure SimbaErrorDlg(Title: String; Err: TStringArray; Args: array of const);
begin
  ShowDialog(TSimbaErrorDialogForm, Title, ''.Join(LineEnding, Err), Args);
end;

{$R *.lfm}

end.

