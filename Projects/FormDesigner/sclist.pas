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
  TSimbaComponent = class(TObject)
    private
      FBitmapContainer: TBitmapContainer;
      FCLSName: string;
      FCaption: string;
      fComponentName: string;
      FImage: TBitmapContainer;
      FLeft,FTop,FWidth,FHeight: integer;
      FFontColor: TColor;
      FFontName: string;
      FFontSize: integer;
      procedure Clear;
    public
      Constructor Create;
      Procedure Assign(smb: TSimbaComponent);
      property CLSName: string read FCLSName write FCLSName;
      property Caption: string read FCaption write FCaption;
      property CompName: string read FComponentName write FComponentName;
      property Img: TBitmapContainer read FBitmapContainer write FBitmapContainer;
      property Left: integer read FLeft write FLeft;
      property Top: integer read FTop write FTop;
      property Width: integer read FWidth write FWidth;
      property Height: integer read FHeight write FHeight;
      property FontColor: TColor read FFontColor write FFontColor;
      property FontName: string read FFontName write FFontName;
      property FontSize: integer read FFontSize write FFontSize;
  end;

{ TSimbaComponentList }

TSimbaComponentList = class
  private
    FSimbaComponents: TList;
    function GetCount: Integer;
    function GeTSimbaComponent(Index: Integer): TSimbaComponent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(aSimbaComponent: TSimbaComponent);
    procedure Assign(Src: TSimbaComponentList);
    function IndexOf(aItem: TSimbaComponent): Integer; overload;
    function IndexOf(Name: String): Integer; overload;
    procedure Delete(Index: Integer); overload;
    procedure Delete(aItem: TSimbaComponent); overload;
    property Count: Integer read GetCount;
    property SimbaComponent[Index: Integer]: TSimbaComponent read GeTSimbaComponent; default;
  end;

const
  ErrItemNotFound = 'Item not found!';
implementation

{ TSimbaComponent }

procedure TSimbaComponent.Clear;
begin
  CLSName:='';
  Caption:='';
  CompName:='';
  Left:=0;
  Top:=0;
  Width:=0;
  Height:=0;
  FontName:='';
  FontSize:=0;
end;

constructor TSimbaComponent.Create;
begin
  Clear;
end;

procedure TSimbaComponent.Assign(smb: TSimbaComponent);
begin
  clsname:=smb.clsname;
  compname:=smb.compname;
  caption:=smb.caption;
  fontcolor:=smb.fontcolor;
  fontname:=smb.fontname;
  img:=smb.img;
  height:=smb.height;
  width:=smb.width;
  left:=smb.left;
  top:=smb.top;
end;

{ TSimbaComponentList }

constructor TSimbaComponentList.Create;
begin
 FSimbaComponents := TList.Create;
end;

procedure TSimbaComponentList.Delete(Index: Integer);
begin
 if (Index < 0) or (Index >= Count) then
   raise Exception.Create(ErrItemNotFound);

 TSimbaComponent(FSimbaComponents[Index]).Free;
 FSimbaComponents.Delete(Index);
end;

procedure TSimbaComponentList.Delete(aItem: TSimbaComponent);
begin
 Delete(IndexOf(aItem));
end;

destructor TSimbaComponentList.Destroy;
begin
 Clear;
 FSimbaComponents.Free;
 inherited;
end;

procedure TSimbaComponentList.Add(aSimbaComponent: TSimbaComponent);
begin
 FSimbaComponents.Add(aSimbaComponent);
end;

procedure TSimbaComponentList.Assign(Src: TSimbaComponentList);
var
 I: Integer;
begin
 Clear;
 for I := 0 to Src.Count - 1 do
   Add(Src[I]);
end;

procedure TSimbaComponentList.Clear;
var
 I: Integer;
begin
 for I := 0 to FSimbaComponents.Count - 1 do
   SimbaComponent[I].Free;
 FSimbaComponents.Clear;
end;


function TSimbaComponentList.GetCount: Integer;
begin
 Result := FSimbaComponents.Count;
end;

function TSimbaComponentList.GeTSimbaComponent(Index: Integer): TSimbaComponent;
begin
 if (Index >= 0) and (Index < Count) then
   Result := TSimbaComponent(FSimbaComponents[Index])
 else
   Result := nil;
end;

function TSimbaComponentList.IndexOf(aItem: TSimbaComponent): Integer;
begin
Result := FSimbaComponents.IndexOf(aItem);
end;

function TSimbaComponentList.IndexOf(Name: String): Integer;
var
 I: Integer;
begin
 for I := 0 to Count - 1 do
   if SimbaComponent[I].CompName = Name then
   begin
     Result := I;
     Exit;
   end;
 Result := -1;
end;



end.


