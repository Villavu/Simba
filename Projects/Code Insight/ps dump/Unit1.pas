unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uPSComponentExt, uPSCompiler;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure CompImport(Sender: TObject; x: TPSPascalCompiler);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uPSC_std,
  uPSC_classes;

{$R *.dfm}

procedure TForm1.CompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x, true);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  a: TPSScriptExtension;
  InsertList, ItemList: TStrings;
begin
  a := TPSScriptExtension.Create(Self);
  a.OnCompImport := CompImport;

  ItemList := TStringList.Create;
  InsertList := TStringList.Create;

  try
    //a.Script.Text := 'const zzz=''test''; type a = record b: (test, jwz); end; begin end.';
    a.GetValueDefs(ItemList);
    Memo1.Lines.Text := ItemList.Text;
  finally
    FreeAndNil(a);
    FreeAndNil(ItemList);
    FreeAndNil(InsertList);
  end;
end;

end.
