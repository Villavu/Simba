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
  TSimbaPage = class(TPage)
  public
    destructor Destroy; override;
  end;

  TSimbaNotebook = class(TCustomControl)
  protected
    FNotebook: TNotebook;
    FPageClass: TSimbaPageClass;

    function GetActivePage: TSimbaPage;
    function GetPageCount: Integer;
    function GetPage(Index: Integer): TSimbaPage;
    procedure SetActivePage(Page: TSimbaPage);
  public
    property ActivePage: TSimbaPage read GetActivePage write SetActivePage;
    property PageCount: Integer read GetPageCount;
    property Page[Index: Integer]: TSimbaPage read GetPage;

    function AddPage: TSimbaPage;

    constructor Create(AOwner: TComponent; PageClass: TSimbaPageClass = nil); reintroduce;
  end;

implementation

destructor TSimbaPage.Destroy;
begin
  Parent := nil; // out of the notebook first, else it processes messages with a stale page index

  inherited Destroy();
end;

function TSimbaNotebook.GetActivePage: TSimbaPage;
begin
  if (FNotebook.PageIndex > -1) then
    Result := TSimbaPage(FNotebook.ActivePageComponent)
  else
    Result := nil;
end;

function TSimbaNotebook.GetPageCount: Integer;
begin
  Result := FNotebook.PageCount;
end;

function TSimbaNotebook.GetPage(Index: Integer): TSimbaPage;
begin
  Result := TSimbaPage(FNotebook.Page[Index]);
end;

procedure TSimbaNotebook.SetActivePage(Page: TSimbaPage);
begin
  if (Page <> nil) then
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

