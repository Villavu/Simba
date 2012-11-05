unit sclist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics;
type TBitmapContainer = record
    switcher: boolean;
    imgcode: string;
  end;
type

  { TSimbaComponent }

  TSimbaComponent = class(TCollectionItem)

  Public
    clsname: string;
    caption: string;
    compname: string;
    img: TBitmapContainer;
    //ItemContainer: TStrings;
    left,top,width,heigth: integer;
    fontcolor: TColor;
    fontname: string;
    fontsize: integer;
  constructor Create(Col: TCollection); override;
  destructor Destroy; override;
  end;

{ TSimbaComponentList }

TSimbaComponentList = class (TCollection)
  private
    function GetItems(Index: Integer): TSimbaComponent;
  public
    function AddItem: TSimbaComponent;

    constructor Create;

    function FindByName(aName: string): TSimbaComponent;

    property Items[Index: Integer]: TSimbaComponent  read GetItems; default;
  end;


implementation

function TSimbaComponentList.GetItems(Index: Integer): TSimbaComponent;
begin
   Result := TSimbaComponent(inherited Items[Index]);
end;


function TSimbaComponentList.AddItem: TSimbaComponent;
begin
   Result := TSimbaComponent(inherited Add());
end;

constructor TSimbaComponentList.Create;
begin
   inherited Create(TSimbaComponent);
end;

function TSimbaComponentList.FindByName(aName: string): TSimbaComponent;
var I: Integer;
begin
 Result := nil;
  for I := 0 to Count - 1 do
    if AnsiCompareText(Trim(Items[i].compname),Trim(aName))=0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

{ TSimbaComponentList }



constructor TSimbaComponent.Create(Col: TCollection);
begin
  inherited Create(Col);
end;

destructor TSimbaComponent.Destroy;
begin
  inherited Destroy;
end;

end.


