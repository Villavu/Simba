{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.shapeboxform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  simba.base, simba.shapebox, simba.env;

type
  TSimbaShapeBoxForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemLoadImage: TMenuItem;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItemLoadImageClick(Sender: TObject);
  protected
    HasBackground: Boolean;
    ShapeBox: TSimbaShapeBox;
  end;

var
  SimbaShapeBoxForm: TSimbaShapeBoxForm;

implementation

procedure TSimbaShapeBoxForm.FormCreate(Sender: TObject);
begin
  ShapeBox := TSimbaShapeBox.Create(Self);
  ShapeBox.Parent := Self;
  ShapeBox.Align := alClient;

  Width  := Scale96ToScreen(800);
  Height := Scale96ToScreen(600);
end;

procedure TSimbaShapeBoxForm.FormHide(Sender: TObject);
begin
  ShapeBox.PrintShapes();
  ShapeBox.SaveToFile(SimbaEnv.DataPath + 'shapes');
  if (not HasBackground) then
    ShapeBox.Background.SetSize(0, 0);
end;

procedure TSimbaShapeBoxForm.FormShow(Sender: TObject);
begin
  if (not HasBackground) then
  begin
    if (ShapeBox.Background = nil) then
      ShapeBox.Background := TBitmap.Create();
    ShapeBox.Background.SetSize(1000, 1000);
  end;

  ShapeBox.LoadFromFile(SimbaEnv.DataPath + 'shapes');
end;

procedure TSimbaShapeBoxForm.MenuItem3Click(Sender: TObject);
begin
  if OpenDialog.Execute() then
    ShapeBox.LoadFromFile(OpenDialog.FileName);
end;

procedure TSimbaShapeBoxForm.MenuItem4Click(Sender: TObject);
begin
  if OpenDialog.Execute() then
    ShapeBox.SaveToFile(OpenDialog.FileName);
end;

procedure TSimbaShapeBoxForm.MenuItemLoadImageClick(Sender: TObject);
begin
  if OpenDialog.Execute() then
  begin
    ShapeBox.SetBackgroundFromFile(OpenDialog.FileName);

    HasBackground := True;
  end;
end;

{$R *.lfm}

end.

