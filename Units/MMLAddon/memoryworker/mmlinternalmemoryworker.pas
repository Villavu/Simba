unit mmlinternalmemoryworker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
type

  { TinternalMemoryWorker }

  TinternalMemoryWorker = class
    private
      FPID: integer;
    protected
      Procedure Clear;
      function Attach: boolean;virtual;abstract;
      function Detach: boolean;virtual;abstract;
      function GetValue(Address: integer; ValueSize: integer; Value: Pointer):boolean;virtual;abstract;
      function SetValue(Address: integer; ValueSize: integer; Value: Pointer):boolean;virtual;abstract;
    public
      Constructor Create(aPID: integer);virtual;
      Destructor Destroy;override;
      function ReadInteger(const Address: integer):integer;
      function ReadFloat(const Address: integer): double;
      function ReadChar(const Address: integer): char;
      function ReadInt64(const Address: integer):int64;
      function ReadString(const Address: integer;Size: integer): string;
      function ReadDword(const Address: integer): DWord;
      function ReadCardinal(const Address: integer):cardinal;
      function ReadByte(const Address: integer):byte;
      function ReadRegion(const Address: integer;Size: integer): pointer;
      procedure WriteInteger(const Address: integer; Value: integer);
      procedure WriteFloat(const Address: integer; Value: double);
      procedure WriteChar(const Address: integer; Value: char);
      procedure WriteInt64(const Address: integer; Value: int64);
      procedure WriteString(const Address: integer; Value: string);
      procedure WriteDword(const Address: integer; Value: dword);
      procedure WriteCardinal(const Address: integer; Value: cardinal);
      procedure WriteByte(const Address: integer; Value: byte);
      procedure WriteRegion(const Address: integer; Value: pointer; Size: integer);
      property PID:integer read FPid write FPID;
  end;

implementation

{ TinternalMemoryWorker }

procedure TinternalMemoryWorker.Clear;
begin
  FPid:=-1;
end;

constructor TinternalMemoryWorker.Create(aPID: integer);
begin
  Clear;
  FPid:=aPID;
end;

destructor TinternalMemoryWorker.Destroy;
begin
  inherited Destroy;
end;

function TinternalMemoryWorker.ReadInteger(const Address: integer): integer;
var
  Pt: array [0..3] of byte;
begin
  if GetValue(Address,SizeOf(integer),@Pt[0]) then
    result:= PInteger(@Pt)^ else
    result:= -1;
end;

function TinternalMemoryWorker.ReadFloat(const Address: integer): double;
var
  Pt: array [0..7] of byte;
begin
  if GetValue(Address,SizeOf(Double),@Pt[0]) then
    result:= PDouble(@Pt)^ else
    result:= -1;
end;

function TinternalMemoryWorker.ReadChar(const Address: integer): char;
var
  Pt: array [0..1] of byte;
begin
  if GetValue(Address,SizeOf(Char),@Pt[0]) then
    result:= PChar(@Pt)^ else
    result:= Pchar('')^;
end;

function TinternalMemoryWorker.ReadInt64(const Address: integer): int64;
var
  Pt: array [0..7] of byte;
begin
  if GetValue(Address,SizeOf(int64),@Pt[0]) then
    result:= PInt64(@Pt)^ else
    result:= -1;
end;

function TinternalMemoryWorker.ReadString(const Address: integer; Size: integer
  ): string;
var
  Pt: array of byte;
begin
  SetLength(Pt,Size);
  if GetValue(Address,Size,@Pt[0]) then
    result:= PChar(@Pt)^ else
    result:= '';
end;

function TinternalMemoryWorker.ReadDword(const Address: integer): DWord;
var
  Pt: array [0..1] of byte;
begin
  if GetValue(Address,SizeOf(DWord),@Pt[0]) then
    result:= PDword(@Pt)^ else
    result:= 0;
end;

function TinternalMemoryWorker.ReadCardinal(const Address: integer): cardinal;
var
  Pt: array [0..1] of byte;
begin
  if GetValue(Address,SizeOf(Cardinal),@Pt[0]) then
    result:= PCardinal(@Pt)^ else
    result:= 0;
end;

function TinternalMemoryWorker.ReadByte(const Address: integer): byte;
var
  Pt: array [0..0] of byte;
begin
  if GetValue(Address,SizeOf(Byte),@Pt[0]) then
    result:= PByte(@Pt)^ else
    result:= 0;
end;

function TinternalMemoryWorker.ReadRegion(const Address: integer; Size: integer
  ): pointer;
var
  Pt: array of byte;
begin
  SetLength(Pt,Size);
  if GetValue(Address,Size,@Pt[0]) then
    result:= @Pt[0] else
    result:= nil;
end;

procedure TinternalMemoryWorker.WriteInteger(const Address: integer;
  Value: integer);
var
  Val: integer;
  Pt: array [0..3] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(integer),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteFloat(const Address: integer; Value: double
  );
var
  Val: double;
  Pt: array [0..7] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(double),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteChar(const Address: integer; Value: char);
var
  Val: Char;
  Pt: array [0..1] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(char),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteInt64(const Address: integer; Value: int64
  );
var
  Val: integer;
  Pt: array [0..7] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(int64),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteString(const Address: integer;
  Value: string);
var
  Val: string;
  Pt: array of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,Length(Value),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteDword(const Address: integer; Value: dword
  );
var
  Val: Dword;
  Pt: array [0..1] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(Dword),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteCardinal(const Address: integer;
  Value: cardinal);
var
  Val: Cardinal;
  Pt: array [0..1] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(Cardinal),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteByte(const Address: integer; Value: byte);
var
  Val: byte;
  Pt: array [0..0] of byte absolute Val;
begin
  Val:=Value;
  SetValue(Address,SizeOf(Byte),@Pt[0]);
end;

procedure TinternalMemoryWorker.WriteRegion(const Address: integer;
  Value: pointer; Size: integer);
begin
  SetValue(Address,Size,Value);
end;

end.

