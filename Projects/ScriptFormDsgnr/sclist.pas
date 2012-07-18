unit sclist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics;
type TBitmapContainer = record
    switcher: boolean;
    path: string;
  end;
type
  TSimbaComponent = record
    classname: string;
    caption: string;
    compname: string;
    img: TBitmapContainer;
    TItemContainer: TStrings;
    left,top,width,heigth: integer;
    fontcolor: TColor;
    fontname: string;
    fontsize: integer;
  end;
  type
  PSimbaComponent = ^TSimbaComponent;
type

{ TSimbaComponentList }

TSimbaComponentList = class
  private
    SimbaList: array of TSimbaComponent; // Simba components storage
  public
  constructor Create;
  destructor Destroy; override;
  procedure AddItem(Item: TSimbaComponent;index: integer);
  procedure ChangeElement(Item: TSimbaComponent;index: integer);
  procedure RemoveItem(Index: Integer);
  function GetComponent(Idx: integer): TSimbaComponent;
  function Count: integer;
end;

implementation

{ TSimbaComponentList }

constructor TSimbaComponentList.Create;
begin
  setlength(SimbaList,0);
  inherited Create;
end;

destructor TSimbaComponentList.Destroy;
begin
  SimbaList := nil; //free storage
  inherited Destroy;
end;

procedure TSimbaComponentList.AddItem(Item: TSimbaComponent;index: integer);
var
  len: integer;
begin
   Len:= Length(SimbaList);
   if Index >= Len then
     Index := Len+1;
   Inc(Len);
   setLength( SimbaList, Len);
   SimbaList[Len - 1] := Item;
end;

procedure TSimbaComponentList.ChangeElement(Item: TSimbaComponent;
  index: integer);
begin
  SimbaList[index]:=item;
end;

procedure TSimbaComponentList.RemoveItem(Index: Integer);
var
  last: integer;
begin
  last := high(SimbaList);
  if Index <  Last then move( SimbaList[Index+1], SimbaList[Index],
       (Last-Index) * sizeof( SimbaList[Index] )  );
   setLength(SimbaList, Last );
end;

function TSimbaComponentList.GetComponent(Idx: integer): TSimbaComponent;
begin
  result:= SimbaList[Idx];
end;


function TSimbaComponentList.Count: integer;
begin
  Result := Length(SimbaList);
end;

end.


