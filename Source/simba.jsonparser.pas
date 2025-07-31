{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------

  Reimplement FPC's TJSONParser but with tracking to report unfreed things
  like how TSimbaImage does.
}
unit simba.jsonparser;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.baseclass,
  fpjson, jsonscanner, jsonreader;

type
  TGarbageList = class(TSimbaBaseClass)
  protected
    FItems: TList;

    procedure NotifyUnfreed; override;

    function GetCount: Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure Add(Item: TJSONData);
    procedure Del(Item: TJSONData);

    property Count: Integer read GetCount;
  end;

  TSimbaJSONParser = class(TBaseJSONReader)
  protected
    FGarbage: TGarbageList;
    FStack: array of TJSONData;
    FStackPos: Integer;
    FStruct: TJSONData;
    FValue: TJSONData;
    FKey: TJSONStringType;

    procedure Pop(aType: TJSONType);
    procedure Push(AValue : TJSONData);
    function NewValue(AValue : TJSONData) : TJSONData;
  protected
    procedure KeyValue(Const AKey : TJSONStringType); override;
    procedure StringValue(Const AValue : TJSONStringType);override;
    procedure NullValue; override;
    procedure FloatValue(Const AValue : Double); override;
    procedure BooleanValue(Const AValue : Boolean); override;
    procedure NumberValue(Const AValue : TJSONStringType); override;
    procedure IntegerValue(Const AValue : integer); override;
    procedure Int64Value(Const AValue : int64); override;
    procedure QWordValue(Const AValue : QWord); override;
    procedure StartArray; override;
    procedure StartObject; override;
    procedure EndArray; override;
    procedure EndObject; override;
  public
    constructor Create(Stream: TStream; FreeStream: Boolean); reintroduce;
    destructor Destroy; override;

    function Parse: TJSONData;

    class function NewArray: TJSONArray; static;
    class function NewObject: TJSONObject; static;
  end;

implementation

type
  TSimbaJSONInteger = class(TJSONIntegerNumber)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(AValue: Integer; Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONInt64 = class(TJSONInt64Number)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(AValue: Int64; Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONQWord = class(TJSONQWordNumber)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(AValue: QWord; Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONFloat = class(TJSONFloatNumber)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(AValue: TJSONFloat; Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONString = class(TJSONString)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(const AValue: TJSONStringType; Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONBoolean = class(TJSONBoolean)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(AValue: Boolean; Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONArray = class(TJSONArray)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONObject = class(TJSONObject)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

  TSimbaJSONNull = class(TJSONNull)
  public
    FGarbage: TGarbageList;
    function Clone: TJSONData; override;
    constructor Create(Garbage: TGarbageList); reintroduce;
    destructor Destroy; override;
  end;

function TSimbaJSONNull.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONNull(Result).FGarbage := FGarbage;
  TSimbaJSONNull(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONNull.Create(Garbage: TGarbageList);
begin
  inherited Create;
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONNull.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONObject.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONObject(Result).FGarbage := FGarbage;
  TSimbaJSONObject(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONObject.Create(Garbage: TGarbageList);
begin
  inherited Create;
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONObject.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONArray.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONArray(Result).FGarbage := FGarbage;
  TSimbaJSONArray(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONArray.Create(Garbage: TGarbageList);
begin
  inherited Create;
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONArray.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONBoolean.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONBoolean(Result).FGarbage := FGarbage;
  TSimbaJSONBoolean(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONBoolean.Create(AValue: Boolean; Garbage: TGarbageList);
begin
  inherited Create(AValue);
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONBoolean.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONString.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONString(Result).FGarbage := FGarbage;
  TSimbaJSONString(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONString.Create(const AValue: TJSONStringType; Garbage: TGarbageList);
begin
  inherited Create(AValue);
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONString.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONFloat.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONFloat(Result).FGarbage := FGarbage;
  TSimbaJSONFloat(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONFloat.Create(AValue: TJSONFloat; Garbage: TGarbageList);
begin
  inherited Create(AValue);
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONFloat.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONQWord.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONQWord(Result).FGarbage := FGarbage;
  TSimbaJSONQWord(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONQWord.Create(AValue: QWord; Garbage: TGarbageList);
begin
  inherited Create(AValue);
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONQWord.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONInt64.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONQWord(Result).FGarbage := FGarbage;
  TSimbaJSONQWord(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONInt64.Create(AValue: Int64; Garbage: TGarbageList);
begin
  inherited Create(AValue);
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONInt64.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TSimbaJSONInteger.Clone: TJSONData;
begin
  Result:=inherited Clone;
  TSimbaJSONInteger(Result).FGarbage := FGarbage;
  TSimbaJSONInteger(Result).FGarbage.Add(Result);
end;

constructor TSimbaJSONInteger.Create(AValue: Integer; Garbage: TGarbageList);
begin
  inherited Create(AValue);
  FGarbage := Garbage;
  FGarbage.Add(Self);
end;

destructor TSimbaJSONInteger.Destroy;
begin
  FGarbage.Del(Self);
  inherited Destroy;
end;

function TGarbageList.GetCount: Integer;
begin
  if (FItems <> nil) then
    Result := FItems.Count
  else
    Result := 0;
end;

procedure TGarbageList.NotifyUnfreed;

  function Dump(Item: TJSONData): String;
  begin
    Result := '';
    case TJSONData(Item).JSONType of
      jtUnknown: Result := 'EJSONItemType.UNKNOWN';
      jtString:  Result := 'EJSONItemType.STR';
      jtBoolean: Result := 'EJSONItemType.BOOL';
      jtNull:    Result := 'EJSONItemType.NULL';
      jtArray:   Result := 'EJSONItemType.ARR';
      jtObject:  Result := 'EJSONItemType.OBJ';
      jtNumber:
        if (TJSONNumber(Item).NumberType = ntFloat) then
          Result := 'EJSONItemType.FLOAT'
        else
          Result := 'EJSONItemType.INT';
    end;
    Result := Result + ' ' + Item.AsJSON;
  end;

var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    DebugLn([EDebugLn.YELLOW], '  ' + Dump(TJSONData(FItems[I])));
end;

procedure TGarbageList.Add(Item: TJSONData);
begin
  FItems.Add(Item);
end;

procedure TGarbageList.Del(Item: TJSONData);
begin
  FItems.Remove(Item);
  if (FItems.Count = 0) then
    Self.Free();
end;

constructor TGarbageList.Create;
begin
  inherited Create();
  FItems := TList.Create();
end;

destructor TGarbageList.Destroy;
begin
  if (FItems <> nil) then
    FreeAndNil(FItems);
  inherited Destroy();
end;

procedure TSimbaJSONParser.Pop(aType: TJSONType);
begin
  if (FStackPos = 0) or (FStruct.JSONType <> aType) then
    DoError('Structural error');
  Dec(FStackPos);
  FStruct:=FStack[FStackPos];
end;

procedure TSimbaJSONParser.Push(AValue: TJSONData);
begin
  if (FStackPos=Length(FStack)) then
    SetLength(FStack, 10 + (FStackPos * 2));
  FStack[FStackPos]:=FStruct;
  Inc(FStackPos);
  FStruct:=AValue;
end;

function TSimbaJSONParser.NewValue(AValue: TJSONData): TJSONData;
begin
  Result := AValue;

  // Add to existing structural type
  if (FStruct is TJSONObject) then
  begin
    if (Not (joIgnoreDuplicates in options)) then
    try
      TJSONObject(FStruct).Add(FKey,AValue);
    except
      AValue.Free;
      raise;
    end
    else if (TJSONObject(FStruct).IndexOfName(FKey) = -1) then
      TJSONObject(FStruct).Add(FKey,AValue)
    else
      AValue.Free;
    FKey:='';
  end
  else if (FStruct is TJSONArray) then
    TJSONArray(FStruct).Add(AValue);
  // The first actual value is our result
  if (FValue=nil) then
    FValue:=AValue;
end;

procedure TSimbaJSONParser.KeyValue(const AKey: TJSONStringType);
begin
  if (FStruct is TJSONObject) and (FKey='') then
    FKey:=Akey
  else
    DoError('Duplicatekey or no object');
end;

procedure TSimbaJSONParser.StringValue(const AValue: TJSONStringType);
begin
  NewValue(TSimbaJSONString.Create(AValue, FGarbage));
end;

procedure TSimbaJSONParser.NullValue;
begin
  NewValue(TSimbaJSONNull.Create(FGarbage));
end;

procedure TSimbaJSONParser.FloatValue(const AValue: Double);
begin
  NewValue(TSimbaJSONFloat.Create(AValue, FGarbage));
end;

procedure TSimbaJSONParser.BooleanValue(const AValue: Boolean);
begin
  NewValue(TSimbaJSONBoolean.Create(AValue, FGarbage));
end;

procedure TSimbaJSONParser.NumberValue(const AValue: TJSONStringType);
begin
  // do nothing
end;

procedure TSimbaJSONParser.IntegerValue(const AValue: integer);
begin
  NewValue(TSimbaJSONInteger.Create(AValue, FGarbage));
end;

procedure TSimbaJSONParser.Int64Value(const AValue: int64);
begin
  NewValue(TSimbaJSONInt64.Create(AValue, FGarbage));
end;

procedure TSimbaJSONParser.QWordValue(const AValue: QWord);
begin
  NewValue(TSimbaJSONQWord.Create(AValue, FGarbage));
end;

procedure TSimbaJSONParser.StartArray;
begin
  Push(NewValue(TSimbaJSONArray.Create(FGarbage)))
end;

procedure TSimbaJSONParser.StartObject;
begin
  Push(NewValue(TSimbaJSONObject.Create(FGarbage)));
end;

procedure TSimbaJSONParser.EndArray;
begin
  Pop(jtArray);
end;

procedure TSimbaJSONParser.EndObject;
begin
  Pop(jtObject);
end;

function TSimbaJSONParser.Parse: TJSONData;
begin
  SetLength(FStack,0);
  FStackPos:=0;
  FValue:=nil;
  FStruct:=nil;
  try
    DoExecute;
    Result:=FValue;
  except
    on E: Exception do
    begin
      FreeAndNil(FValue);
      FStackPos:=0;
      SetLength(FStack,0);
      raise;
    end;
  end;
end;

class function TSimbaJSONParser.NewArray: TJSONArray;
begin
  Result := TSimbaJSONArray.Create(TGarbageList.Create());
end;

class function TSimbaJSONParser.NewObject: TJSONObject;
begin
  Result := TSimbaJSONObject.Create(TGarbageList.Create());
end;

constructor TSimbaJSONParser.Create(Stream: TStream; FreeStream: Boolean);
begin
  inherited Create(Stream, DefaultOptions);

  FGarbage := TGarbageList.Create();
  if FreeStream then
    Stream.Free();
end;

destructor TSimbaJSONParser.Destroy;
begin
  if (FGarbage.Count = 0) then
    FreeAndNil(FGarbage);

  inherited Destroy();
end;

end.

