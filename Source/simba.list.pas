{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Simple generic list class.
}
unit simba.list;

{$i simba.inc}

interface

uses
  Classes, SysUtils;

type
  generic TSimbaList<_T> = class(TObject)
  private
    procedure SetItem(Index: Integer; AValue: _T);
  public type
    TArr = array of _T;
  protected
    FArr: TArr;
    FCount: Integer;

    function GetItem(Index: Integer): _T; virtual;
  public
    procedure Add(Item: _T); virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    function ToArray: TArr; virtual;

    property Count: Integer read FCount;
    property Items[Index: Integer]: _T read GetItem write SetItem; default;
  end;

  generic TSimbaObjectList<_T: class> = class(specialize TSimbaList<_T>)
  protected
    FFreeObjects: Boolean;
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;

    constructor Create(FreeObjects: Boolean = False); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaStringPair = record
    Name: String;
    Value: String;
  end;
  TSimbaStringPairList = specialize TSimbaList<TSimbaStringPair>;

implementation

procedure TSimbaList.SetItem(Index: Integer; AValue: _T);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('%s.SetItem: Index %d out of bounds', [ClassName, Index]);

  FArr[Index] := AValue;
end;

// List
function TSimbaList.GetItem(Index: Integer): _T;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('%s.GetItem: Index %d out of bounds', [ClassName, Index]);

  Result := FArr[Index];
end;

procedure TSimbaList.Add(Item: _T);
begin
  if (FCount >= Length(FArr)) then
    SetLength(FArr, 32 + (Length(FArr) * 2));

  FArr[FCount] := Item;
  Inc(FCount);
end;

procedure TSimbaList.Clear;
begin
  FCount := 0;
end;

procedure TSimbaList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('%s.Delete: Index %d out of bounds', [ClassName, Index]);

  FCount := FCount - 1;
  Move(FArr[Index + 1], FArr[Index], (FCount - Index) * SizeOf(_T));
end;

function TSimbaList.ToArray: TArr;
begin
  SetLength(Result, FCount);
  if (FCount > 0) then
    Move(FArr[0], Result[0], FCount * SizeOf(_T));
end;

// Object list
procedure TSimbaObjectList.Clear;
var
  I: Integer;
begin
  if FFreeObjects then
    for I := 0 to FCount - 1 do
      FArr[I].Free();

  inherited Clear();
end;

procedure TSimbaObjectList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('%s.Delete: Index %d out of bounds', [ClassName, Index]);
  FArr[Index].Free();

  inherited Delete(Index);
end;

constructor TSimbaObjectList.Create(FreeObjects: Boolean);
begin
  inherited Create();

  FFreeObjects := FreeObjects;
end;

destructor TSimbaObjectList.Destroy;
begin
  Clear();

  inherited Destroy();
end;

end.

