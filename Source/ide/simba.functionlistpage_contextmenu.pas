{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.functionlistpage_contextmenu;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Menus,
  simba.base,
  simba.functionlist_page;

type
  TFunctionListPage_ContextMenu = class(TPopupMenu)
  private
    FPage: TSimbaFunctionListPage;
    FHideShowSection: TMenuItem;
    FOpenSimbaDoc: TMenuItem;
    FTooltip: TMenuItem;
    FShowAll: TMenuItem;
    FHideAll: TMenuItem;

    procedure DoOpenSimbaDocClick(Sender: TObject);
    procedure DoShow(Sender: TObject);
    procedure DoUpdateHiddenSections(Sender: TObject);
    procedure DoMouseOverTooltipClick(Sender: TObject);
    procedure DoCollapseAllClick(Sender: TObject);
    procedure DoShowAllClick(Sender: TObject);
    procedure DoHideAllClick(Sender: TObject);
  public
    constructor Create(APage: TSimbaFunctionListPage); reintroduce;
  end;

implementation

uses
  ComCtrls,
  simba.settings,
  simba.nativeinterface;

procedure TFunctionListPage_ContextMenu.DoOpenSimbaDocClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  // the section the selection is in, at any depth
  Node := FPage.TreeView.Selected;
  while (Node <> nil) and (not (Node is TSimbaSectionNode)) do
    Node := Node.Parent;

  if (Node <> nil) then
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL + 'api/' + Node.Text)
  else
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL);
end;

procedure TFunctionListPage_ContextMenu.DoShow(Sender: TObject);
var
  I: Integer;
  NewItem: TMenuItem;
  Hidden: String;
  InSimba: Boolean;
begin
  FTooltip.Checked := SimbaSettings.FunctionList.ShowMouseoverHint.Value; // every page has a menu: another may have changed it

  InSimba := (FPage.SimbaNode <> nil) and (FPage.TreeView.Selected <> nil) and
             ((FPage.TreeView.Selected = FPage.SimbaNode) or FPage.TreeView.Selected.HasAsParent(FPage.SimbaNode));

  FOpenSimbaDoc.Enabled := InSimba;
  FHideShowSection.Enabled := InSimba;
  FShowAll.Enabled := InSimba;
  FHideAll.Enabled := InSimba;

  if InSimba then
  begin
    Hidden := SimbaSettings.FunctionList.HiddenSimbaSections.Value;

    FHideShowSection.Clear();
    for I := 0 to FPage.SimbaNode.Count - 1 do
    begin
      NewItem := TMenuItem.Create(FHideShowSection);
      NewItem.Caption := FPage.SimbaNode.Items[I].Text;
      NewItem.Checked := Pos('[' + FPage.SimbaNode.Items[I].Text + ']', Hidden) <= 0;
      NewItem.AutoCheck := True;
      NewItem.ShowAlwaysCheckable := True;
      NewItem.OnClick := @DoUpdateHiddenSections;

      FHideShowSection.Add(NewItem);
    end;
  end;
end;

procedure TFunctionListPage_ContextMenu.DoUpdateHiddenSections(Sender: TObject);
var
  I: Integer;
  Hidden: String;
begin
  Hidden := '';
  for I := 0 to FHideShowSection.Count - 1 do
    if (not FHideShowSection[I].Checked) then
      Hidden := Hidden + '[' + FHideShowSection[I].Caption + ']';
  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Hidden;
end;

procedure TFunctionListPage_ContextMenu.DoMouseOverTooltipClick(Sender: TObject);
begin
  SimbaSettings.FunctionList.ShowMouseoverHint.Value := TMenuItem(Sender).Checked;
end;

procedure TFunctionListPage_ContextMenu.DoCollapseAllClick(Sender: TObject);
begin
  FPage.CollapseAll();
end;

procedure TFunctionListPage_ContextMenu.DoShowAllClick(Sender: TObject);
begin
  SimbaSettings.FunctionList.HiddenSimbaSections.Value := '';
end;

procedure TFunctionListPage_ContextMenu.DoHideAllClick(Sender: TObject);
var
  Hidden: String;
  I: Integer;
begin
  Hidden := '';
  for I := 0 to FPage.SimbaNode.Count - 1 do
    Hidden := Hidden + '[' + FPage.SimbaNode.Items[I].Text + ']';

  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Hidden;
end;

constructor TFunctionListPage_ContextMenu.Create(APage: TSimbaFunctionListPage);

  function Add(ACaption: String; AOnClick: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AOnClick;

    Items.Add(Result);
  end;

begin
  inherited Create(APage);

  FPage := APage;

  FTooltip := Add('Show tooltip', @DoMouseOverTooltipClick);
  FTooltip.AutoCheck := True;
  FTooltip.ShowAlwaysCheckable := True;
  Add('Collapse all', @DoCollapseAllClick);
  Items.AddSeparator();
  FOpenSimbaDoc := Add('Open Simba Documentation', @DoOpenSimbaDocClick);
  FHideShowSection := Add('Hide/Show Section', nil);
  FShowAll := Add('Show all', @DoShowAllClick);
  FHideAll := Add('Hide all', @DoHideAllClick);

  OnPopup := @DoShow;
end;

end.

