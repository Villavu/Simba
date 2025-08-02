{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Notebook is like a tab control but without any tab "headers"
}
unit simba.component_notebook;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls,
  simba.containers;

type
  TSimbaPageClass = class of TSimbaPage;
  TSimbaPage = class(TCustomControl)
  protected
    procedure SetParent(NewParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSimbaNotebook = class(TCustomControl)
  protected type
    TSimbaPageList = specialize TSimbaList<TSimbaPage>;
  protected
    FPageClass: TSimbaPageClass;
    FPages: TSimbaPageList;
    FPageIndex: Integer;

    function IndexOf(Page: TSimbaPage): Integer;

    procedure ShowControl(AControl: TControl); override;

    function GetActivePage: TSimbaPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): TSimbaPage;
    procedure SetPageIndex(Value: Integer);
    procedure SetActivePage(Page: TSimbaPage);
  public
    property ActivePage: TSimbaPage read GetActivePage write SetActivePage;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;
    property Page[Index: Integer]: TSimbaPage read GetPage;

    function AddPage: TSimbaPage;

    constructor Create(AOwner: TComponent; PageClass: TSimbaPageClass = nil); reintroduce;
    destructor Destroy; override;
  end;

implementation

procedure TSimbaPage.SetParent(NewParent: TWinControl);
var
  OldParent: TWinControl;
  OldINdex: Integer;
begin
  OldParent := Parent;
  inherited SetParent(NewParent);

  if (OldParent is TSimbaNotebook) then
  begin
    OldIndex := TSimbaNotebook(OldParent).IndexOf(Self);
    if (OldIndex > -1) then
    begin
      TSimbaNotebook(OldParent).FPages.Delete(OldIndex);
      TSimbaNotebook(OldParent).PageIndex := OldIndex - 1;
    end;
  end;
  if (NewParent is TSimbaNotebook) then
    TSimbaNotebook(NewParent).FPages.Add(Self);
end;

constructor TSimbaPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csAcceptsControls, csDesignFixedBounds, csNoDesignVisible, csNoFocus];

  Caption := '';
  Align := alClient;
  Visible := False;
end;

function TSimbaNotebook.IndexOf(Page: TSimbaPage): Integer;
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    if (FPages[I] = Page) then
      Exit(I);
  Result := -1;
end;

procedure TSimbaNotebook.ShowControl(AControl: TControl);
begin
  inherited ShowControl(AControl);

  if (AControl is TSimbaPage) then
    PageIndex := IndexOf(TSimbaPage(AControl));
end;

function TSimbaNotebook.GetActivePage: TSimbaPage;
begin
  if (FPageIndex >= 0) and (FPageIndex < FPages.Count) then
    Result := FPages[FPageIndex]
  else
    Result := nil;
end;

function TSimbaNotebook.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TSimbaNotebook.GetPage(Index: Integer): TSimbaPage;
begin
  Result := FPages[Index];
end;

procedure TSimbaNotebook.SetPageIndex(Value: Integer);
var
  NewPage: TSimbaPage;
begin
  if (Value < -1) or (Value >= FPages.Count) then
    Exit;
  if (FPageIndex = Value) then
    Exit;

  // Hide the previously shown page
  if (FPageIndex >= 0) and (FPageIndex < FPages.Count) then
  begin
    NewPage := FPages[FPageIndex];
    NewPage.ControlStyle := NewPage.ControlStyle + [csNoDesignVisible];
    NewPage.Visible := False;
  end;

  // Update the property
  FPageIndex := Value;
  if (FPageIndex = -1) then
    Exit;

  // And show the new one
  NewPage := FPages[FPageIndex];
  NewPage.Visible := True;
  NewPage.ControlStyle := NewPage.ControlStyle - [csNoDesignVisible];
  NewPage.Align := alClient;
end;

procedure TSimbaNotebook.SetActivePage(Page: TSimbaPage);
var
  I: Integer;
begin
  for I := 0 to FPages.Count - 1 do
    if (FPages[I] = Page) then
    begin
      PageIndex := I;
      Exit;
    end;
end;

function TSimbaNotebook.AddPage: TSimbaPage;
begin
  Result := FPageClass.Create(Self);
  Result.Parent := Self;

  PageIndex := FPages.Count-1;
end;

constructor TSimbaNotebook.Create(AOwner: TComponent; PageClass: TSimbaPageClass);
begin
  inherited Create(AOwner);

  if (PageClass = nil) then
    FPageClass := TSimbaPage
  else
    FPageClass := PageClass;
  FPages := TSimbaPageList.Create();
  FPageIndex := -1;
end;

destructor TSimbaNotebook.Destroy;
begin
  if (FPages <> nil) then
    FreeAndNil(FPages);

  inherited Destroy();
end;

end.

