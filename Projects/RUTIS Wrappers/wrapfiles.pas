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
    SaveDirEdit.Directory := ExtractFileDir(FileDialog.FileName);
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
  Input, Output : TStringList;
  NewFile : string;
  YesToAll, NoToAll : boolean;
begin
  YesToAll:= false;
  NoToAll:= false;
  if not DirectoryExists(SaveDirEdit.Directory) then
  begin
    dbg(format('Dir %s does not exist',[SaveDirEdit.Directory]));
    exit;
  end;
  if FileBox.Items.Count < 1 then
  begin
    dbg('No files loaded');
    exit;
  end;
  for i := 0 to FileBox.Items.Count - 1 do
  begin
    if not FileExists(filebox.items[i]) then
    begin
      dbg(format('File[%s] does not exist',[FileBox.items[i]]));
      continue;
    end;
    NewFile := SaveDirEdit.Directory + DirectorySeparator + ExtractFileName(FileBox.Items[i]);
    if not YesToAll and FileExists(NewFile) then
    begin
      if NoToAll then
        Continue;
      case MessageDlg('File already exists',Format('Do you want to overwrite the file %s',[NewFile]),
                      mtConfirmation,[mbYes,mbYesToAll,mbNo,mbNoToAll],0) of
        mrNo : Continue;
        mrNoToAll : begin
                      NoToAll:= True;
                      Continue;
                    end;
        mrYesToAll : YesToAll:= true;
      end;
    end;
    dbg(NewFile);
    try
      Input := TStringList.Create;
      Input.LoadFromFile(filebox.items[i]);
      Output :=  TStringList.Create;
      ConvertRT(Input,dbgMemo.Lines,Output);
      Output.SaveToFile(NewFile);
      Input.free;
      Output.free;
    except
      dbg('Something went wrong');
    end;
  end;
end;

end.

