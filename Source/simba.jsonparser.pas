{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  Change JSON datatype classes to report leaks like how TSimbaImage does.
  Also print floats without crazy float precision.
}
unit simba.jsonparser;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

implementation

uses
  fpjson,
  simba.baseclass;

var
  FormatSettingsDot: TFormatSettings;

type
  TJsonItemTracked = class(TSimbaBaseClass)
  protected
    Item: TJSONData;

    procedure NotifyUnfreed; override;
  public
    constructor Create(AItem: TJSONData); reintroduce;
  end;

  // note the constructors aren't virtual so need to new NewInstance to create the tracking
  TSimbaJSONInteger = class(TJSONIntegerNumber)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONInt64 = class(TJSONInt64Number)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONQWord = class(TJSONQWordNumber)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONFloat = class(TJSONFloatNumber)
  protected
    function GetAsString: TJSONStringType; override;
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONString = class(TJSONString)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONBoolean = class(TJSONBoolean)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONArray = class(TJSONArray)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONObject = class(TJSONObject)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  TSimbaJSONNull = class(TJSONNull)
  public
    FTracking: TJsonItemTracked;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

class function TSimbaJSONNull.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONNull(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONNull.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONObject.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONObject(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONObject.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONArray.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONArray(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONArray.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONBoolean.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONBoolean(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONBoolean.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONString.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONString(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONString.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

// can be improved later to use a more mathematical approach
function TSimbaJSONFloat.GetAsString: TJSONStringType;
var
  Len: Integer;
begin
  Result := FormatFloat('0.00000000', AsFloat);
  Len := Length(Result);
  while (Len > 3) and (Result[Len] = '0') and (Result[Len - 1] <> '.') do
    Dec(Len);
  SetLength(Result, Len);
end;

class function TSimbaJSONFloat.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONFloat(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONFloat.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONQWord.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONQWord(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONQWord.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONInt64.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONInt64(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONInt64.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

class function TSimbaJSONInteger.NewInstance: TObject;
begin
  Result := inherited;
  TSimbaJSONInteger(Result).FTracking := TJsonItemTracked.Create(TJSONData(Result));
end;

destructor TSimbaJSONInteger.Destroy;
begin
  FTracking.Free();
  inherited Destroy;
end;

procedure TJsonItemTracked.NotifyUnfreed;

  function Dump: String;
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

begin
  DebugLn(DEBUG_YELLOW + '  ' + Dump() + DEBUG_RESET);
end;

constructor TJsonItemTracked.Create(AItem: TJSONData);
begin
  inherited Create();

  Item := AItem;
end;

initialization
  FormatSettingsDot := FormatSettings;
  FormatSettingsDot.DecimalSeparator := '.';

  SetJSONInstanceType(jitNumberInteger, TSimbaJSONInteger);
  SetJSONInstanceType(jitNumberInt64, TSimbaJSONInt64);
  SetJSONInstanceType(jitNumberQWord, TSimbaJSONQWord);
  SetJSONInstanceType(jitNumberFloat, TSimbaJSONFloat);
  SetJSONInstanceType(jitString, TSimbaJSONString);
  SetJSONInstanceType(jitNumberFloat, TSimbaJSONFloat);
  SetJSONInstanceType(jitBoolean, TSimbaJSONBoolean);
  SetJSONInstanceType(jitArray, TSimbaJSONArray);
  SetJSONInstanceType(jitObject, TSimbaJSONObject);
  SetJSONInstanceType(jitNull, TSimbaJSONNull);

end.

