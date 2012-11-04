unit sm_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, sm_utils;

type

   //reserved
  { TUpdateScript }

  TUpdateScript = class(TCollectionItem)
    Public
      ScriptName: string;
      URL: string;
      constructor Create(Col: TCollection); override;
      destructor Destroy; override;
  end;

  { TUpdateList }

  TUpdateList = class(TCollection)
     private
    function GetItems(Index: Integer): TUpdateScript;
  public
    function AddItem: TUpdateScript;

    constructor Create;

    property Items[Index: Integer]: TUpdateScript read GetItems; default;
  end;
  //end of reserved;

  { TSubItem }

  TSubItem = class(TCollectionItem)
  public
    FileName: string;
    UnpPath: string;
    constructor Create(Col: TCollection); override;
    destructor Destroy; override;
  end;

  { TSubitemList }

  TSubitemList = class(TCollection)
  private
    function GetItems(Index: Integer): TSubItem;
  public
    function AddItem: TSubItem;

    constructor Create;

    property Items[Index: Integer]: TSubItem read GetItems; default;
  end;

  TFileItem = class(TCollectionItem)
  public
    FileName: string;
    Author: string;
    EMail: string;
    DateModify : TDateTime;
    Version: extended;
    Description: string;

    SubFiles: TSubitemList;

    constructor Create(Col: TCollection); override;
    destructor Destroy; override;
  end;
  TFileItemEx = class(TFileItem)
    public
    Installed: integer;
    Update: integer;
  end;

  { TFileItemList }

  TFileItemList = class(TCollection)
  private
    function GetItems(Index: Integer): TFileItem;
    function GetItemsEx(Index: Integer): TFileItemEx;
  public
    function AddItem: TFileItem;
    function AddItemEx: TFileItemEx;
    function FindByName(aFileName: string): TFileItemEx;overload;
    function FindByName(aFileName: string): TFileItem;overload;
    constructor Create;
    property ItemsEx[Index: Integer]: TFileItemEx read GetItemsEx;
    property Items[Index: Integer]: TFileItem read GetItems; default;
  end;

  TPackageItem = class(TCollectionItem)
  public
    Name: string;
    Files: TFileItemList;
    Updates: TUpdateList;

    constructor Create(Col: TCollection); override;
    destructor Destroy; override;
  end;


  { TPackageList }

  TPackageList = class(TCollection)
  private
    function GetItems(Index: Integer): TPackageItem;
  public
    function AddItem: TPackageItem;
    
    constructor Create;
    procedure Clear();
    function FindByName(aName: string): TPackageItem;

    property Items[Index: Integer]: TPackageItem read GetItems; default;
  end;

implementation

{ TUpdateList }

function TUpdateList.GetItems(Index: Integer): TUpdateScript;
begin
  Result := TUpdateScript(inherited Items[Index]);
end;

function TUpdateList.AddItem: TUpdateScript;
begin
  Result := TUpdateScript(inherited Add());
end;

constructor TUpdateList.Create;
begin
  inherited Create(TUpdateScript);
end;

{ TUpdateScript }

constructor TUpdateScript.Create(Col: TCollection);
begin
  inherited Create(Col);
end;

destructor TUpdateScript.Destroy;
begin
  inherited Destroy;
end;

constructor TSubItem.Create(Col: TCollection);
begin
  inherited Create(Col);
end;

destructor TSubItem.Destroy;
begin
  inherited Destroy;
end;

{ TSubitemList }

function TSubitemList.GetItems(Index: Integer): TSubItem;
begin
  Result := TSubItem(inherited Items[Index]);
end;

function TSubitemList.AddItem: TSubItem;
begin
  Result := TSubItem(inherited Add());
end;

constructor TSubitemList.Create;
begin
  inherited Create(TSubItem);
end;

{ TFileItem }

constructor TFileItem.Create(Col: TCollection);
begin
  inherited Create(Col);
  SubFiles:=TSubitemList.Create;
 // SubFiles := TList.Create();
end;

destructor TFileItem.Destroy;
begin
  FreeAndNil(SubFiles);
  
 // inherited Destroy;
end;

{ TFileItemList }

function TFileItemList.AddItem: TFileItem;
begin
  Result := TFileItem(inherited Add());
end;

function TFileItemList.GetItems(Index: Integer): TFileItem;
begin
  Result := TFileItem(inherited Items[Index]);
end;

function TFileItemList.AddItemEx: TFileItemEx;
begin
  Result := TFileItemEx(inherited Add());
end;

function TFileItemList.FindByName(aFileName: string): TFileItemEx; overload;
var I: Integer;
begin
 Result := nil;

  for I := 0 to Count - 1 do
    if Eq(ItemsEx[i].FileName, aFileName) then
    begin
      Result := ItemsEx[i];
      Break;
    end;
end;

function TFileItemList.FindByName(aFileName: string): TFileItem; overload;
var I: Integer;
begin
 Result := nil;

  for I := 0 to Count - 1 do
    if Eq(Items[i].FileName, aFileName) then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TFileItemList.GetItemsEx(Index: Integer): TFileItemEx;
begin
  Result := TFileItemEx(inherited Items[Index]);
end;

constructor TFileItemList.Create;
begin
  inherited Create(TFileItem);
end;

{ TPackageItem }

constructor TPackageItem.Create(Col: TCollection);
begin
  inherited Create(Col);
  
  Files := TFileItemList.Create();
end;

destructor TPackageItem.Destroy;
begin
  FreeAndNil(Files);
  
  inherited Destroy;
end;

{ TPackageList }

function TPackageList.AddItem: TPackageItem;
begin
  Result := TPackageItem(inherited Add());
end;

function TPackageList.GetItems(Index: Integer): TPackageItem;
begin
  Result := TPackageItem(inherited Items[Index]);
end;

constructor TPackageList.Create;
begin
  inherited Create(TPackageItem);
end;

procedure TPackageList.Clear();
begin
  inherited Clear;
end;



function TPackageList.FindByName(aName: string): TPackageItem;
var I: Integer;
begin
 Result := nil;

  for I := 0 to Count - 1 do
    if Eq(Items[i].Name, aName) then
    begin
      Result := Items[i];
      Break;
    end;
end;

end.
