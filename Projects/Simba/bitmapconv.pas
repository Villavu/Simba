unit bitmapconv;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, ExtDlgs;

type

  { TBitmapConvForm }

  TBitmapConvForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BitmapConvForm: TBitmapConvForm;

implementation

{$R *.lfm}

{ TBitmapConvForm }

procedure TBitmapConvForm.Button2Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    writeln(OpenPictureDialog1.FileName);
end;

end.

