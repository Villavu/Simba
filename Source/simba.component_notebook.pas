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
  Classes, SysUtils, Controls, ExtCtrls;

type
  TSimbaPageClass = class of TSimbaPage;
  TSimbaPage = class(TPage);

  TSimbaNotebook = class(TCustomControl)
  protected
    FNotebook: TNotebook;
    FPageClass: TSimbaPageClass;

    function IndexOf(Page: TSimbaPage): Integer;

    procedure ShowControl(AControl: TControl); override;

    function GetActivePage: TSimbaPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): TSimbaPage;
    function GetPageIndex: Integer;
    procedure SetPageIndex(Value: Integer);
    procedure SetActivePage(Page: TSimbaPage);
  public
    property ActivePage: TSimbaPage read GetActivePage write SetActivePage;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;
    property Page[Index: Integer]: TSimbaPage read GetPage;

    function AddPage: TSimbaPage;

    constructor Create(AOwner: TComponent; PageClass: TSimbaPageClass = nil); reintroduce;
  end;

implementation

function TSimbaNotebook.IndexOf(Page: TSimbaPage): Integer;
begin
  Result := FNotebook.IndexOf(Page);
end;

procedure TSimbaNotebook.ShowControl(AControl: TControl);
begin
  if (AControl is TSimbaPage) then
    FNotebook.ShowControl(AControl)
  else
    inherited ShowControl(AControl);
end;

function TSimbaNotebook.GetActivePage: TSimbaPage;
begin
  Result := TSimbaPage(FNotebook.ActivePageComponent);
end;

function TSimbaNotebook.GetPageCount: Integer;
begin
  Result := FNotebook.PageCount;
end;

function TSimbaNotebook.GetPage(Index: Integer): TSimbaPage;
begin
  Result := TSimbaPage(FNotebook.Page[Index]);
end;

function TSimbaNotebook.GetPageIndex: Integer;
begin
  Result := FNotebook.PageIndex;
end;

procedure TSimbaNotebook.SetPageIndex(Value: Integer);
begin
  FNotebook.PageIndex := Value;
end;

procedure TSimbaNotebook.SetActivePage(Page: TSimbaPage);
begin
  FNotebook.ShowControl(Page);
end;

function TSimbaNotebook.AddPage: TSimbaPage;
begin
  Result := FPageClass.Create(Self);
  Result.Parent := FNotebook;
end;

constructor TSimbaNotebook.Create(AOwner: TComponent; PageClass: TSimbaPageClass);
begin
  inherited Create(AOwner);

  if (PageClass = nil) then
    FPageClass := TSimbaPage
  else
    FPageClass := PageClass;

  FNotebook := TNotebook.Create(Self);
  FNotebook.Parent := Self;
  FNotebook.Align := alClient;
end;

end.

