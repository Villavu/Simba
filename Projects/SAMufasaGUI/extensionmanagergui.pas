unit extensionmanagergui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, Grids,extensionmanager;

type

  { TExtensionsForm }

  TExtensionsForm = class(TForm)
    Button: TButton;
    ExtEnabled: TCheckBox;
    ExtensionsBox: TGroupBox;
    ExtensionsList: TListView;
    procedure ButtonClick(Sender: TObject);
    procedure ExtEnabledChange(Sender: TObject);
    procedure ExtensionsListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ExtensionsListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure Refresh;
  public
    procedure OnChangeExtensions(Sender:  TObject);
  end; 

var
  ExtensionsForm: TExtensionsForm;

implementation
uses
  virtualextension;

{ TExtensionsForm }

procedure TExtensionsForm.ButtonClick(Sender: TObject);
begin
  Refresh;
end;

procedure TExtensionsForm.ExtEnabledChange(Sender: TObject);
begin
  if ExtensionsList.Selected <> nil then
    with TVirtualSimbaExtension(ExtManager.Extensions[ExtensionsList.Selected.Index]) do
    begin;
      Enabled:= ExtEnabled.Checked;
      ExtEnabled.Checked := Enabled;
    end;
end;

procedure TExtensionsForm.ExtensionsListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if TVirtualSimbaExtension(ExtManager.Extensions[Item.Index]).Enabled then
    Sender.Canvas.Brush.Color := $CCFFCC//$7AFF95
  else
    sender.canvas.brush.color := $CCCCFF//$FF3D64;
end;

procedure TExtensionsForm.ExtensionsListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  str : string;
begin
  if Selected = true then
  begin
    extEnabled.Visible := true;
    with TVirtualSimbaExtension(ExtManager.Extensions[Item.Index]) do
    begin
      OnChange:= nil;
      ExtEnabled.Checked := Enabled;
      Onchange := @OnChangeExtensions;
      str := GetName;
      if str = '' then
        str := ExtractFileName(Filename);
      ExtEnabled.Caption:= 'Enable: ' + str;
    end;
  end else
    ExtEnabled.Visible := false;
end;

procedure TExtensionsForm.FormCreate(Sender: TObject);
begin
  ExtManager.OnChange:= @OnChangeExtensions;
  Refresh;
end;

procedure TExtensionsForm.Refresh;
var
  i , sel: integer;
  Extensions : TList;
  str : string;
begin
  sel := -1;
  if ExtensionsList.Selected <> nil then
    sel := ExtensionsList.Selected.Index;
  ExtensionsList.Clear;
  Extensions := ExtManager.Extensions;
  for i := 0 to Extensions.Count - 1 do
  begin
    with TVirtualSimbaExtension(Extensions[i]),ExtensionsList.Items.Add do
    begin
      Caption:= ExtractFileName(FileName);
      str := GetName;
      if str = '' then
        str := caption; //Could not retrieve the name
      SubItems.Add(str);
      str := GetVersion;
      if str = '' then
        str := 'NA'; //Could not retrieve the version
      SubItems.Add(str);
    end;
  end;
  if sel <> -1 then
    if sel < extensions.count then
      ExtensionsList.Selected := ExtensionsList.Items[sel];
end;

procedure TExtensionsForm.OnChangeExtensions(Sender: TObject);
begin
  Refresh;
end;

initialization
  {$I extensionmanagergui.lrs}

end.

