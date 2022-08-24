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
  ESimbaDialogResult = (
    CANCEL,
    YES,
    NO
  );
  {$POP}

  TSimbaDialogForm = class(TForm)
    ButtonCancel: TButton;
    ButtonNo: TButton;
    ButtonYes: TButton;
    Image1: TImage;
    QuestionLabel: TLabel;
    MessagePanel: TPanel;
    ButtonPanel: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonNoClick(Sender: TObject);
    procedure ButtonYesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    DialogResult: ESimbaDialogResult;
  end;

function SimbaQuestionDlg(Title, Question: String): ESimbaDialogResult; overload;
function SimbaQuestionDlg(Title: String; Question: TStringArray): ESimbaDialogResult; overload;

implementation

function SimbaQuestionDlg(Title, Question: String): ESimbaDialogResult;
begin
  with TSimbaDialogForm.Create(nil) do
  try
    Caption := Title;
    QuestionLabel.Caption := Question;

    ShowModal();

    Result := DialogResult;
  finally
    Free();
  end;
end;

function SimbaQuestionDlg(Title: String; Question: TStringArray): ESimbaDialogResult;
begin
  Result := SimbaQuestionDlg(Title, ''.Join(LineEnding, Question));
end;

procedure TSimbaDialogForm.FormCreate(Sender: TObject);
begin
  DialogResult := ESimbaDialogResult.CANCEL; // default, if close button was clicked.

  Image1.Picture.Bitmap := TBitmap(GetDialogIcon(idDialogConfirm));
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

{$R *.lfm}

end.

