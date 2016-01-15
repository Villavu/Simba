unit mmlbasememoryworker;

{$mode objfpc}{$H+}
{$I Simba.inc}
interface

uses
  Classes, SysUtils;

type

  { TinternalMemoryWorker }

  TBaseMemoryWorker = class
  private
    FPID: integer;
  protected
    Procedure Clear;
    {$IFNDEF MSWINDOWS}
    function Attach: boolean; virtual;abstract;
    function Detach: boolean; virtual;abstract;
    {$ENDIF}
    function GetValue(Address: integer; ValueSize: integer; Value: Pointer)
      : boolean; virtual; abstract;
    function SetValue(Address: integer; ValueSize: integer; Value: Pointer)
      : boolean; virtual; abstract;
  public
    Constructor Create(aPID: integer); virtual;
    Destructor Destroy; override;
    function ReadInteger(const Address: integer): integer;
    function ReadFloat(const Address: integer): double;
    function ReadChar(const Address: integer): char;
    function ReadInt64(const Address: integer): int64;
    function ReadString(const Address: integer; Size: integer): string;
    function ReadDword(const Address: integer): DWord;
    function ReadCardinal(const Address: integer): cardinal;
    function ReadByte(const Address: integer): byte;
    function ReadRegion(const Address: integer; Size: integer): Pointer;
    procedure WriteInteger(const Address: integer; Value: integer);
    procedure WriteFloat(const Address: integer; Value: double);
    procedure WriteChar(const Address: integer; Value: char);
    procedure WriteInt64(const Address: integer; Value: int64);
    procedure WriteString(const Address: integer; Value: string);
    procedure WriteDword(const Address: integer; Value: DWord);
    procedure WriteCardinal(const Address: integer; Value: cardinal);
    procedure WriteByte(const Address: integer; Value: byte);
    procedure WriteRegion(const Address: integer; Value: Pointer;
      Size: integer);
    property PID: integer read FPID write FPID;
  end;

implementation

{ TBaseMemoryWorker }

procedure TBaseMemoryWorker.Clear;
begin
  FPID := -1;
end;

constructor TBaseMemoryWorker.Create(aPID: integer);
begin
  Clear;
  FPID := aPID;
end;

destructor TBaseMemoryWorker.Destroy;
begin
  inherited Destroy;
end;

function TBaseMemoryWorker.ReadInteger(const Address: integer): integer;
var
  Pt: array [0 .. 3] of byte;
begin
  if GetValue(Address, SizeOf(integer), @Pt[0]) then
    result := PInteger(@Pt)^
  else
    result := -1;
end;

function TBaseMemoryWorker.ReadFloat(const Address: integer): double;
var
  Pt: array [0 .. 7] of byte;
begin
  if GetValue(Address, SizeOf(double), @Pt[0]) then
    result := PDouble(@Pt)^
  else
    result := -1;
end;

function TBaseMemoryWorker.ReadChar(const Address: integer): char;
var
  Pt: array [0 .. 1] of byte;
begin
  if GetValue(Address, SizeOf(char), @Pt[0]) then
    result := PChar(@Pt)^
  else
    result := PChar('')^;
end;

function TBaseMemoryWorker.ReadInt64(const Address: integer): int64;
var
  Pt: array [0 .. 7] of byte;
begin
  if GetValue(Address, SizeOf(int64), @Pt[0]) then
    result := PInt64(@Pt)^
  else
    result := -1;
end;

function TBaseMemoryWorker.ReadString(const Address: integer;
  Size: integer): string;
var
  Pt: array of byte;
begin
  SetLength(Pt, Size);
  if GetValue(Address, Size, @Pt[0]) then
    result := PChar(@Pt)^
  else
    result := '';
end;

function TBaseMemoryWorker.ReadDword(const Address: integer): DWord;
var
  Pt: array [0 .. 1] of byte;
begin
  if GetValue(Address, SizeOf(DWord), @Pt[0]) then
    result := PDword(@Pt)^
  else
    result := 0;
end;

function TBaseMemoryWorker.ReadCardinal(const Address: integer): cardinal;
var
  Pt: array [0 .. 1] of byte;
begin
  if GetValue(Address, SizeOf(cardinal), @Pt[0]) then
    result := PCardinal(@Pt)^
  else
    result := 0;
end;

function TBaseMemoryWorker.ReadByte(const Address: integer): byte;
var
  Pt: array [0 .. 0] of byte;
begin
  if GetValue(Address, SizeOf(byte), @Pt[0]) then
    result := PByte(@Pt)^
  else
    result := 0;
end;

function TBaseMemoryWorker.ReadRegion(const Address: integer;
  Size: integer): Pointer;
var
  Pt: array of byte;
begin
  SetLength(Pt, Size);
  if GetValue(Address, Size, @Pt[0]) then
    result := @Pt[0]
  else
    result := nil;
end;

procedure TBaseMemoryWorker.WriteInteger(const Address: integer;
  Value: integer);
var
  Val: integer;
  Pt: array [0 .. 3] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(integer), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteFloat(const Address: integer; Value: double);
var
  Val: double;
  Pt: array [0 .. 7] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(double), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteChar(const Address: integer; Value: char);
var
  Val: char;
  Pt: array [0 .. 1] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(char), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteInt64(const Address: integer; Value: int64);
var
  Val: integer;
  Pt: array [0 .. 7] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(int64), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteString(const Address: integer; Value: string);
var
  Val: string;
  Pt: array of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, Length(Value), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteDword(const Address: integer; Value: DWord);
var
  Val: DWord;
  Pt: array [0 .. 1] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(DWord), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteCardinal(const Address: integer;
  Value: cardinal);
var
  Val: cardinal;
  Pt: array [0 .. 1] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(cardinal), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteByte(const Address: integer; Value: byte);
var
  Val: byte;
  Pt: array [0 .. 0] of byte absolute Val;
begin
  Val := Value;
  SetValue(Address, SizeOf(byte), @Pt[0]);
end;

procedure TBaseMemoryWorker.WriteRegion(const Address: integer; Value: Pointer;
  Size: integer);
begin
  SetValue(Address, Size, Value);
end;

end.
