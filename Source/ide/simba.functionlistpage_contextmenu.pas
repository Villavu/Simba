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
    FCollapseAll: TMenuItem;
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
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  ComCtrls,
  simba.settings,
  simba.nativeinterface;

procedure TFunctionListPage_ContextMenu.DoOpenSimbaDocClick(Sender: TObject);
var
  Node: TTreeNode;
  Section: String;
begin
  Section := '';

  Node := FPage.TreeView.Selected;
  if (Node is TSimbaSectionNode) then
    Section := Node.Text
  else if (Node is TDeclNode) and (Node.Parent is TSimbaSectionNode) then
    Section := Node.Parent.Text;

  if (Section <> '') then
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL + 'api/' + Section)
  else
    SimbaNativeInterface.OpenURL(SIMBA_DOCS_URL);
end;

procedure TFunctionListPage_ContextMenu.DoShow(Sender: TObject);
var
  I: Integer;
  NewItem: TMenuItem;
  Hidden: String;
begin
  if (FPage.SimbaNode = nil) then
    Exit;

  if (FPage.TreeView.Selected = nil) or (not FPage.TreeView.Selected.HasAsParent(FPage.SimbaNode)) then
  begin
    FOpenSimbaDoc.Enabled := False;
    FHideShowSection.Enabled := False;
    FShowAll.Enabled := False;
    FHideAll.Enabled := False;
  end else
  begin
    FOpenSimbaDoc.Enabled := True;
    FHideShowSection.Enabled := True;
    FShowAll.Enabled := True;
    FHideAll.Enabled := True;

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
  FPage.TreeView.FullCollapse();

  FPage.ScriptNode.Expanded := True;
  FPage.IncludesNode.Expanded := True;
  FPage.PluginsNode.Expanded := True;
  if (FPage.SimbaNode <> nil) then
    FPage.SimbaNode.Expanded := True;
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
  if (FPage.SimbaNode = nil) then
    Exit;

  Hidden := '';
  for I := 0 to FPage.SimbaNode.Count - 1 do
    Hidden := Hidden + '[' + FPage.SimbaNode.Items[I].Text + ']';

  SimbaSettings.FunctionList.HiddenSimbaSections.Value := Hidden;
end;

constructor TFunctionListPage_ContextMenu.Create(AOwner: TComponent);

  procedure AddLine;
  begin
    Items.Add(NewLine());
  end;

  function Add(ACaption: String; AOnClick: TNotifyEvent; Checkable: Boolean = False; Checked: Boolean = False; ImageIndex: Integer = -1): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AOnClick;
    Result.ImageIndex := ImageIndex;
    if Checkable then
    begin
      Result.AutoCheck := Checkable;
      Result.ShowAlwaysCheckable := Checkable;
      Result.Checked := Checked;
    end;

    Items.Add(Result);
  end;

begin
  inherited Create(AOwner);

  Assert(AOwner is TSimbaFunctionListPage);

  FPage := AOwner as TSimbaFunctionListPage;

  FTooltip := Add('Show Mouse-over tooltip', @DoMouseOverTooltipClick, True, SimbaSettings.FunctionList.ShowMouseoverHint.Value);
  FCollapseAll := Add('Collapse all', @DoCollapseAllClick);
  AddLine();
  FOpenSimbaDoc := Add('Open Simba Documentation', @DoOpenSimbaDocClick, False, False);
  FHideShowSection := Add('Hide/Show Section', nil);
  FShowAll := Add('Show all', @DoShowAllClick);
  FHideAll := Add('Hide all', @DoHideAllClick);

  OnPopup := @DoShow;
end;

end.

