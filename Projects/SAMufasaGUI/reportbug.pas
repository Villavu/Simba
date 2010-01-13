unit reportbug;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, LResources,
  ComCtrls;

type

  { TFormReportBug }

  TFormReportBug = class(TForm)
    ButtonCancel: TButton;
    ButtonReport: TButton;
    ComboBoxCategory: TComboBox;
    ComboBoxPriority: TComboBox;
    ComboBoxReproducibility: TComboBox;
    ComboBoxSeverity: TComboBox;
    GroupBoxReportData: TGroupBox;
    LabelSummary: TLabel;
    LabelDescription: TLabel;
    LabelTitle: TLabel;
    MemoSummary: TMemo;
    MemoDescription: TMemo;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonReportClick(Sender: TObject);
  private
    // mantis page
    PostPage: String;
    URLFormat: String;
    function PostReport: Boolean;
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormReportBug: TFormReportBug;

implementation
uses
  httpsend;
{ TFormReportBug }

procedure TFormReportBug.ButtonCancelClick(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
  Self.Hide;
end;

function TFormReportBug.postReport: Boolean;
begin
  // newinternets.pas will allow this to work! :D
  result := false;
end;

procedure TFormReportBug.ButtonReportClick(Sender: TObject);
begin
  { Send Report Data }
  {
    need to implement the sending of data, I am assuming that mantis uses POST
    variables when sending data, but I do not know. This needs to be found out.
  }
  Writeln('HAI YOU CLICKED THE BUTTON!');
  Self.postReport;

end;

initialization
  {$I reportbug.lrs}

end.

