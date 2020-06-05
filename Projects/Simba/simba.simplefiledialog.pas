unit simba.simplefiledialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FileCtrl, ShellCtrls, ButtonPanel, ComCtrls;

type
  TSimbaFileDialog = class(TForm)
    Button1: TButton;
    ButtonGo: TButton;
    ButtonPanel1: TButtonPanel;
    FileEdit: TEdit;
    Label1: TLabel;
    PathEdit: TEdit;
    ShellView: TShellListView;

    procedure DoButtonUpClick(Sender: TObject);
    procedure DoButtonGoClick(Sender: TObject);
    procedure DoShellViewDoubleClick(Sender: TObject);
    procedure DoShellViewFileAdded(Sender: TObject; Item: TListItem);
    procedure DoShellViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  protected
    function GetFileName: String;
  public
    property FileName: String read GetFileName;

    procedure ChangeDirectory(Dir: String);
    function Execute: Boolean;

    constructor Create(TheOwner: TComponent); override;
  end;

var
  SimbaFileDialog: TSimbaFileDialog;

implementation

uses
  LazFileUtils,
  simba.main;

procedure TSimbaFileDialog.DoShellViewDoubleClick(Sender: TObject);
begin
  if (ShellView.Selected <> nil) then
    if DirectoryExists(ShellView.GetPathFromItem(ShellView.Selected)) then
      ChangeDirectory(ShellView.GetPathFromItem(ShellView.Selected))
    else
    begin
      FileEdit.Caption := ShellView.GetPathFromItem(ShellView.Selected);
      ModalResult := mrOK;
      CloseQuery;
    end;
end;

procedure TSimbaFileDialog.DoShellViewFileAdded(Sender: TObject; Item: TListItem);
begin
  if DirectoryExists(ShellView.GetPathFromItem(Item)) then
    Item.ImageIndex := IMAGE_DIRECTORY
  else
  if ExtractFileExt(ShellView.GetPathFromItem(Item)) = '.simba' then
    Item.ImageIndex := IMAGE_SIMBA;
end;

procedure TSimbaFileDialog.DoShellViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then
    FileEdit.Caption := ShellView.GetPathFromItem(Item);
end;

function TSimbaFileDialog.GetFileName: String;
begin
  Result := FileEdit.Text;
end;

procedure TSimbaFileDialog.ChangeDirectory(Dir: String);
begin
  if DirectoryExists(Dir) then
  begin
    PathEdit.Caption := Dir;
    FileEdit.Caption := '';

    ShellView.Root := PathEdit.Caption;
  end;
end;

function TSimbaFileDialog.Execute: Boolean;
begin
  Result := ShowModal() = mrOK;

  FileEdit.Text := ExpandFileNameUTF8(FileEdit.Text, ShellView.Root);
end;

constructor TSimbaFileDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ChangeDirectory(GetCurrentDir);
end;

procedure TSimbaFileDialog.DoButtonGoClick(Sender: TObject);
begin
  ChangeDirectory(PathEdit.Caption);
end;

procedure TSimbaFileDialog.DoButtonUpClick(Sender: TObject);
begin
  ChangeDirectory(ExtractFileDir(ExcludeTrailingPathDelimiter(ShellView.Root)));
end;

initialization
  {$I simba.simplefiledialog.lrs}

end.

