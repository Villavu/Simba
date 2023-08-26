{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Wraps FPC's json parser in a more scripting like way.
}
unit simba.json;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  fpjson,
  simba.mufasatypes, simba.baseclass;

type
  ESimbaJSONValueType = (UNKNOWN, NULL, INT, FLOAT, STR, BOOL);

  PSimbaJSONElement = ^TSimbaJSONElement;
  TSimbaJSONElement = record
  private
    FData: TJSONData;

    function GetAsString: String;
    function GetValue: Variant;
    function GetCount: Integer;
    function GetIsArray: Boolean;
    function GetIsObject: Boolean;
    function GetIsValue: Boolean;
    function GetItems(Index: Integer): TSimbaJSONElement;
    function GetKeys: TStringArray;
    function GetValueType: ESimbaJSONValueType;

    procedure SetValue(Value: Variant);
  public
    class operator := (Right: TJSONData): TSimbaJSONElement;
    class operator := (Right: TSimbaJSONElement): TJSONData;

    function Clone: TSimbaJSONElement;

    function Find(Key: String; out Element: TSimbaJSONElement): Boolean;
    function FindPath(Path: String; out Element: TSimbaJSONElement): Boolean;

    function HasKey(Key: String): Boolean; overload;
    function HasKey(Keys: TStringArray): Boolean; overload;
    function HasKeys(Keys: TStringArray): Boolean;

    function AddNull(Key: String): TSimbaJSONElement;
    function AddArray(Key: String): TSimbaJSONElement;
    function AddObject(Key: String): TSimbaJSONElement;
    procedure AddValue(Key: String; Value: Variant);
    procedure AddElement(Key: String; Element: TSimbaJSONElement);

    procedure Clear;
    procedure Delete(Key: String); overload;
    procedure Delete(Index: Integer); overload;

    property IsValue: Boolean read GetIsValue;
    property IsArray: Boolean read GetIsArray;
    property IsObject: Boolean read GetIsObject;

    property ValueType: ESimbaJSONValueType read GetValueType;
    property Value: Variant read GetValue write SetValue;

    property Keys: TStringArray read GetKeys;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSimbaJSONElement read GetItems;

    property AsString: String read GetAsString;
  end;

  PSimbaJSONParser = ^TSimbaJSONParser;
  TSimbaJSONParser = class(TSimbaBaseClass)
  protected
    FRoot: TSimbaJSONElement;

    function GetKeys: TStringArray;
    function GetAsString: String;
    function GetCount: Integer;
    function GetItems(Index: Integer): TSimbaJSONElement;
  public
    constructor Create(Str: String);
    constructor CreateFromFile(FileName: String);
    destructor Destroy; override;

    function SaveToFile(FileName: String): Boolean;

    // These all just wrap FRoot.XXX
    function Find(Key: String; out Element: TSimbaJSONElement): Boolean;
    function FindPath(Path: String; out Element: TSimbaJSONElement): Boolean;

    function HasKey(Key: String): Boolean; overload;
    function HasKey(Keys: TStringArray): Boolean; overload;
    function HasKeys(Keys: TStringArray): Boolean;

    function AddNull(Key: String): TSimbaJSONElement;
    function AddArray(Key: String): TSimbaJSONElement;
    function AddObject(Key: String): TSimbaJSONElement;
    procedure AddValue(Key: String; Value: Variant);
    procedure AddElement(Key: String; Element: TSimbaJSONElement);

    procedure Clear;
    procedure Delete(Key: String); overload;
    procedure Delete(Index: Integer); overload;

    property Keys: TStringArray read GetKeys;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSimbaJSONElement read GetItems;

    property AsString: String read GetAsString;
  end;

implementation

uses
  jsonreader, jsonscanner, jsonparser, Variants;

function TSimbaJSONElement.GetAsString: String;
begin
  Result := FData.FormatJSON();
end;

function TSimbaJSONElement.GetValue: Variant;
begin
  Result := FData.Value;
end;

function TSimbaJSONElement.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TSimbaJSONElement.GetIsArray: Boolean;
begin
  Result := FData is TJSONArray;
end;

function TSimbaJSONElement.GetIsObject: Boolean;
begin
  Result := FData is TJSONObject;
end;

function TSimbaJSONElement.GetIsValue: Boolean;
begin
  Result := (not IsArray) and (not IsObject);
end;

function TSimbaJSONElement.GetItems(Index: Integer): TSimbaJSONElement;
begin
  Result := FData.Items[Index];
end;

function TSimbaJSONElement.GetKeys: TStringArray;
var
  I: Integer;
begin
  Result := [];

  if (FData is TJSONObject) then
  begin
    SetLength(Result, FData.Count);
    for I := 0 to FData.Count - 1 do
      Result[I] := TJsonObject(FData).Names[I];
  end;
end;

function TSimbaJSONElement.GetValueType: ESimbaJSONValueType;
begin
  case FData.JSONType of
    jtNumber:
      begin
        if (TJSONNumber(FData).NumberType = ntFloat) then
          Result := ESimbaJSONValueType.FLOAT
        else
          Result := ESimbaJSONValueType.INT;
      end;
    jtString:  Result := ESimbaJSONValueType.STR;
    jtBoolean: Result := ESimbaJSONValueType.BOOL;
    jtNull:    Result := ESimbaJSONValueType.NULL;
    else
      Result := ESimbaJSONValueType.UNKNOWN;
  end;
end;

procedure TSimbaJSONElement.SetValue(Value: Variant);
begin
  FData.Value := Value;
end;

class operator TSimbaJSONElement.:=(Right: TJSONData): TSimbaJSONElement;
begin
  Result.FData := Right;
end;

class operator TSimbaJSONElement.:=(Right: TSimbaJSONElement): TJSONData;
begin
  Result := Right.FData;
end;

procedure TSimbaJSONElement.AddValue(Key: String; Value: Variant);
begin
  if IsValue then
    raise Exception.Create('Element is not json object or array');

  if VarIsStr(Value) then
    if IsArray then
      TJSONArray(FData).Add(TJSONStringType(Value))
    else
      TJSONObject(FData).Add(Key, TJSONStringType(Value))
  else
  if VarIsBool(Value) then
    if IsArray then
      TJSONArray(FData).Add(Boolean(Value))
    else
      TJSONObject(FData).Add(Key, Boolean(Value))
  else
  if VarIsOrdinal(Value) then
    if IsArray then
      TJSONArray(FData).Add(TJSONLargeInt(Value))
    else
      TJSONObject(FData).Add(Key, TJSONLargeInt(Value))
  else
  if VarIsFloat(Value) then
    if IsArray then
      TJSONArray(FData).Add(TJSONFloat(Value))
    else
      TJSONObject(FData).Add(Key, TJSONFloat(Value))
  else
    raise Exception.Create('Invalid JSON variant type: ' + VarTypeAsText(VarType(Value)));
end;

function TSimbaJSONElement.AddArray(Key: String): TSimbaJSONElement;
begin
  if IsValue then
    raise Exception.Create('Element is not json object or array');

  if (FData is TJSONArray) then
    Result := FData.Items[TJSONArray(FData).Add(TJSONArray.Create())]
  else
  if (FData is TJSONObject) then
    Result := FData.Items[TJSONObject(FData).Add(Key, TJSONArray.Create())];
end;

function TSimbaJSONElement.AddObject(Key: String): TSimbaJSONElement;
begin
  if IsValue then
    raise Exception.Create('Element is not json object or array');

  if (FData is TJSONArray) then
    Result := FData.Items[TJSONArray(FData).Add(TJSONObject.Create())]
  else
  if (FData is TJSONObject) then
    Result := FData.Items[TJSONObject(FData).Add(Key, TJSONObject.Create())];
end;

procedure TSimbaJSONElement.AddElement(Key: String; Element: TSimbaJSONElement);
begin
  if IsValue then
    raise Exception.Create('Element is not json object or array');

  if (FData is TJSONArray) then
    TJSONArray(FData).Add(Element)
  else
  if (FData is TJSONObject) then
    TJSONObject(FData).Add(Key, Element);
end;

function TSimbaJSONElement.AddNull(Key: String): TSimbaJSONElement;
begin
  if IsValue then
    raise Exception.Create('Element is not json object or array');

  if (FData is TJSONArray) then
    Result := FData.Items[TJSONArray(FData).Add(TJSONNull.Create())]
  else
  if (FData is TJSONObject) then
    Result := FData.Items[TJSONObject(FData).Add(Key, TJSONNull.Create())];
end;

procedure TSimbaJSONElement.Clear;
begin
  FData.Clear();
end;

procedure TSimbaJSONElement.Delete(Key: String);
begin
  if not IsObject then
    raise Exception.Create('Element is not a json object');

  TJSONObject(FData).Delete(Key);
end;

procedure TSimbaJSONElement.Delete(Index: Integer);
begin
  if IsValue then
    raise Exception.Create('Element is not json object or array');

  if (FData is TJSONArray) then
    TJSONArray(FData).Delete(Index)
  else
  if (FData is TJSONObject) then
    TJSONObject(FData).Delete(Index);
end;

function TSimbaJSONElement.Find(Key: String; out Element: TSimbaJSONElement): Boolean;
var
  Data: TJSONData;
begin
  if not IsObject then
    raise Exception.Create('Element is not json object');

  Result := TJSONObject(FData).Find(Key, Data);
  if Result then
    Element := Data;
end;

function TSimbaJSONElement.FindPath(Path: String; out Element: TSimbaJSONElement): Boolean;
var
  Data: TJSONData;
begin
  Data := FData.FindPath(Path);
  Result := Assigned(Data);
  if Result then
    Element := Data;
end;

function TSimbaJSONElement.HasKey(Key: String): Boolean;
begin
  if not IsObject then
    raise Exception.Create('Element is not json object');

  Result := Assigned(TJSONObject(FData).Find(Key));
end;

function TSimbaJSONElement.HasKey(Keys: TStringArray): Boolean;
var
  Key: String;
begin
  if not IsObject then
    raise Exception.Create('Element is not json object');

  for Key in Keys do
    if Assigned(TJSONObject(FData).Find(Key)) then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TSimbaJSONElement.HasKeys(Keys: TStringArray): Boolean;
var
  Key: String;
begin
  if not IsObject then
    raise Exception.Create('Element is not json object');

  for Key in Keys do
    if not Assigned(TJSONObject(FData).Find(Key)) then
    begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function TSimbaJSONElement.Clone: TSimbaJSONElement;
begin
  Result := FData.Clone;
end;

constructor TSimbaJSONParser.Create(Str: String);
var
  Parser: TJSONParser;
begin
  inherited Create();

  if (Str <> '') then
  begin
    Parser := TJSONParser.Create(Str, [joUTF8, joComments, joIgnoreTrailingComma]);
    try
      FRoot := Parser.Parse();
    finally
      Parser.Free();
    end;
  end;

  if (FRoot.FData = nil) then
    FRoot := TJSONObject.Create();
end;

constructor TSimbaJSONParser.CreateFromFile(FileName: String);
var
  Stream: TFileStream;
begin
  inherited Create();

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    FRoot := GetJSON(Stream);
  finally
    Stream.Free();
  end;
end;

destructor TSimbaJSONParser.Destroy;
begin
  if (FRoot.FData <> nil) then
    FreeAndNil(FRoot.FData);

  inherited Destroy();
end;

function TSimbaJSONParser.SaveToFile(FileName: String): Boolean;
var
  Stream: TFileStream;
  Str: String;
begin
  Result := False;

  Stream := nil;
  try
    Str := AsString;
    if FileExists(FileName) then
      Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
    else
      Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);

    Stream.WriteBuffer(Str[1], Length(Str));
  except
  end;

  if (Stream <> nil) then
    Stream.Free();
end;

procedure TSimbaJSONParser.Clear;
begin
  FRoot.Clear();
end;

procedure TSimbaJSONParser.AddElement(Key: String; Element: TSimbaJSONElement);
begin
  FRoot.AddElement(Key, Element);
end;

procedure TSimbaJSONParser.AddValue(Key: String; Value: Variant);
begin
  FRoot.AddValue(Key, Value);
end;

function TSimbaJSONParser.AddArray(Key: String): TSimbaJSONElement;
begin
  Result := FRoot.AddArray(Key);
end;

function TSimbaJSONParser.AddObject(Key: String): TSimbaJSONElement;
begin
  Result := FRoot.AddObject(Key);
end;

procedure TSimbaJSONParser.Delete(Key: String);
begin
  FRoot.Delete(Key);
end;

procedure TSimbaJSONParser.Delete(Index: Integer);
begin
  FRoot.Delete(Index);
end;

function TSimbaJSONParser.Find(Key: String; out Element: TSimbaJSONElement): Boolean;
begin
  Result := FRoot.Find(Key, Element);
end;

function TSimbaJSONParser.FindPath(Path: String; out Element: TSimbaJSONElement): Boolean;
begin
  Result := FRoot.FindPath(Path, Element);
end;

function TSimbaJSONParser.HasKey(Key: String): Boolean;
begin
  Result := FRoot.HasKey(Key);
end;

function TSimbaJSONParser.HasKey(Keys: TStringArray): Boolean;
begin
  Result := FRoot.HasKey(Keys);
end;

function TSimbaJSONParser.HasKeys(Keys: TStringArray): Boolean;
begin
  Result := FRoot.HasKeys(Keys);
end;

function TSimbaJSONParser.AddNull(Key: String): TSimbaJSONElement;
begin
  Result := FRoot.AddNull(Key);
end;

function TSimbaJSONParser.GetCount: Integer;
begin
  Result := FRoot.Count;
end;

function TSimbaJSONParser.GetKeys: TStringArray;
begin
  Result := FRoot.Keys;
end;

function TSimbaJSONParser.GetAsString: String;
begin
  Result := FRoot.AsString;
end;

function TSimbaJSONParser.GetItems(Index: Integer): TSimbaJSONElement;
begin
  Result := FRoot.Items[Index];
end;

end.

