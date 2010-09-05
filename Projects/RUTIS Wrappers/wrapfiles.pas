unit wrapfiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TWrapFilesForm }

  TWrapFilesForm = class(TForm)
    dbgMemo: TMemo;
    wrpBtn: TButton;
    SaveDirEdit: TDirectoryEdit;
    FileButton: TButton;
    FileBox: TListBox;
    FileDialog: TOpenDialog;
    SaveDirLabel: TLabel;
    procedure FileButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure wrpBtnClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  WrapFilesForm: TWrapFilesForm;

implementation

uses
  Main;

{$R *.lfm}

{ TWrapFilesForm }

procedure TWrapFilesForm.FileButtonClick(Sender: TObject);
begin
  if FileDialog.Execute then
  begin
    FileBox.Items.AddStrings(FileDialog.Files);
  end;
end;

procedure TWrapFilesForm.FormCreate(Sender: TObject);
begin
end;

procedure TWrapFilesForm.wrpBtnClick(Sender: TObject);
  procedure dbg(s : string);
  begin
    dbgMemo.Lines.Add(s);
  end;

var
  i : integer;
  Input, Output : TFileStream;
begin
  if not DirectoryExists(SaveDirEdit.Directory) then
  begin
    Writeln(format('Dir %s does not exist',[SaveDirEdit.Directory]));
    exit;
  end;
  if not FileBox.Items.Count < 1 then
  begin
    Writeln('No files loaded');
    exit;
  end;
  for i := 0 to FileBox.Items.Count - 1 do
  begin

  end;
end;

end.

