{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.dialog;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

function SimbaQuestionDlg(Title, Question: String): Boolean;

implementation

uses
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLType;

type
  TSimbaDialogForm = class(TForm)
    ButtonNo: TButton;
    ButtonYes: TButton;
    Image1: TImage;
    QuestionLabel: TLabel;
    MessagePanel: TPanel;
    ButtonPanel: TPanel;
    procedure ButtonNoClick(Sender: TObject);
    procedure ButtonYesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    YesButtonClicked: Boolean;
  end;

function SimbaQuestionDlg(Title, Question: String): Boolean;
begin
  with TSimbaDialogForm.Create(nil) do
  try
    Caption := Title;
    QuestionLabel.Caption := Question;

    ShowModal();

    Result := YesButtonClicked;
  finally
    Free();
  end;
end;

procedure TSimbaDialogForm.FormCreate(Sender: TObject);
begin
  Image1.Picture.Bitmap := TBitmap(GetDialogIcon(idDialogConfirm));
end;

procedure TSimbaDialogForm.ButtonNoClick(Sender: TObject);
begin
  Close();
end;

procedure TSimbaDialogForm.ButtonYesClick(Sender: TObject);
begin
  YesButtonClicked := True;

  Close();
end;

{$R *.lfm}

end.

