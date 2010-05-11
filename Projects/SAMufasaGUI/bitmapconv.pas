unit bitmapconv;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TBitmapConvForm }

  TBitmapConvForm = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  BitmapConvForm: TBitmapConvForm;

implementation

{$R *.lfm}

end.

