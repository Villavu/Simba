{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_base;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base;

type
  // "Perfect Hashing"
  // A hashtable with no collisions.
  generic TKeywordDictionary<_T> = class(TObject)
  type
    TBucketArray = array of record Key: String; Value: _T; end;
  protected
    FBuckets: TBucketArray;
    FSeed: UInt32;
    FCount: Integer;
    FMaxKeyLength: Integer;

    procedure setSize(AValue: UInt32);
    function getValue(const Key: PChar): _T;
  public
    InvalidVal: _T;

    procedure Add(Key: String; Value: _T);

    property Seed: UInt32 read FSeed write FSeed;
    property Size: UInt32 write SetSize;
    property MaxKeyLength: Integer read FMaxKeyLength;
    property Value[Key: PChar]: _T read getValue; default;
  end;

  // not initialized, so only "safe" to use when used within a class
  generic TCache<_T> = record
  type
    TSelf = specialize TCache<_T>;
  private
    FValue: _T;
    FCached: Boolean;

    function getEmpty: Boolean;
    procedure setEmpty(Value: Boolean);
  public
    property Empty: Boolean read getEmpty write setEmpty;

    class operator := (AValue: _T): TSelf;
    class operator := (AValue: TSelf): _T;
  end;
  TStringCache = specialize TCache<String>;

  {$IFDEF PARSER_LEAK_CHECKS}
  TLeakChecker = class(TObject)
  protected
  class var
    FList: TList;
  public
    class constructor Create;
    class destructor Destroy;

    constructor Create; virtual;
    destructor Destroy; override;
  end;
  {$ENDIF}

  function HashStr(const Str: String; const Seed: UInt32 = $811C9DC5): UInt32; inline;

type
  TCodetoolsMessageHandler = procedure(const Message: String) of object;

  procedure CodetoolsMessage(Message: String); overload;
  procedure CodetoolsMessage(Message: String; Line, Col: Integer; FileName: String); overload;
  procedure SetCodetoolsMessageHandler(Handler: TCodetoolsMessageHandler);

implementation

var
  CodetoolsMessageHandler: TCodetoolsMessageHandler;

procedure TKeywordDictionary.setSize(AValue: UInt32);
begin
  SetLength(FBuckets, AValue);
end;

function TKeywordDictionary.getValue(const Key: PChar): _T;
var
  Bucket: Integer;
  Left, Right: PChar;
begin
  Bucket := HashStr(Key, FSeed) and High(FBuckets);

  Left := PChar(FBuckets[Bucket].Key);
  Right := PChar(Key);
  while (Left^ = Right^) and (Left^ <> #0) and (Right^ <> #0) do
  begin
    Inc(Left);
    Inc(Right);
  end;

  if (Left^ = Right^) then
    Result := FBuckets[Bucket].Value
  else
    Result := InvalidVal;
end;

procedure TKeywordDictionary.Add(Key: String; Value: _T);
var
  Bucket: Integer;
begin
  Key := LowerCase(Key);
  if (Length(Key) > FMaxKeyLength) then
    FMaxKeyLength := Length(Key);

  Bucket := HashStr(Key, FSeed) and High(FBuckets);
  if (FBuckets[Bucket].Key <> '') then
    raise Exception.Create('Different seed needed!');

  FBuckets[Bucket].Key := Key;
  FBuckets[Bucket].Value := Value;
end;

function TCache.getEmpty: Boolean;
begin
  Result := not FCached;
end;

procedure TCache.setEmpty(Value: Boolean);
begin
  FCached := Value;
end;

class operator TCache.:=(AValue: _T): TSelf;
begin
  Result.FValue := AValue;
  Result.FCached := True;
end;

class operator TCache.:=(AValue: TSelf): _T;
begin
  Result := AValue.FValue;
end;

{$IFDEF PARSER_LEAK_CHECKS}
class constructor TLeakChecker.Create;
begin
  FList := TList.Create();
end;

class destructor TLeakChecker.Destroy;
begin
  if (SimbaProcessType = ESimbaProcessType.IDE) then
  begin
    DebugLn('TLeakChecker.PrintLeaks: ' + IntToStr(FList.Count) + ' (should be zero)');
    DebugLn('Press enter to close...');

    ReadLn;
  end;

  FList.Free();
end;

constructor TLeakChecker.Create;
begin
  inherited Create();

  FList.Add(Self);
end;

destructor TLeakChecker.Destroy;
begin
  FList.Remove(Self);

  inherited Destroy();
end;
{$ENDIF}

// FNV1a
function HashStr(const Str: String; const Seed: UInt32): UInt32;
var
  Ptr: PChar;
begin
  {$PUSH}
  {$Q-}{$R-}
  Result := Seed;
  if (Length(Str) > 0) then
  begin
    Ptr := @Str[1];
    while (Ptr^ <> #0) do
    begin
      Result := (Result xor Byte(Ptr^)) * $01000193;

      Inc(Ptr);
    end;
  end;
  {$POP}
end;

procedure CodetoolsMessage(Message: String);
begin
  if Assigned(CodetoolsMessageHandler) then
    CodetoolsMessageHandler(Message)
  else
  begin
    {$PUSH}
    {$I-}
    WriteLn(Message);
    Flush(Output);
    {$POP}
  end;
end;

procedure CodetoolsMessage(Message: String; Line, Col: Integer; FileName: String);
begin
  Message := '"' + Message + '" at line ' + IntToStr(Line) + ', column ' + IntToStr(Col);
  if (FileName <> '') then
    Message := Message + ' in file "' + FileName + '"';

  CodetoolsMessage(Message);
end;

procedure SetCodetoolsMessageHandler(Handler: TCodetoolsMessageHandler);
begin
  CodetoolsMessageHandler := Handler;
end;

end.

