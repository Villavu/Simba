unit lptmemoryworker;

{$mode objfpc}{$H+}
{$I Simba.inc}
interface

uses
  Classes, SysUtils,lpcompiler, lptypes, lpClassHelper,mmlbasememoryworker,mmlmemoryworkerfactory;

procedure Register_TBaseMemoryWorker(Compiler: TLapeCompiler);

implementation

type
  PBaseMemoryWorker = ^TBaseMemoryWorker;

//Constructor Create(aPID: integer); virtual;
procedure TBaseMemoryWorker_Init(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^ := TMemoryWorkerFactory.GetInstance(Pinteger(Params^[1])^);
end;

//function ReadInteger(const Address: integer): integer;
procedure TBaseMemoryWorker_ReadInteger(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadInteger(Pinteger(Params^[1])^);
end;

//function ReadFloat(const Address: integer): double;
procedure TBaseMemoryWorker_ReadFloat(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pdouble(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadFloat(Pinteger(Params^[1])^);
end;

//function ReadChar(const Address: integer): char;
procedure TBaseMemoryWorker_ReadChar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pchar(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadChar(Pinteger(Params^[1])^);
end;

//function ReadInt64(const Address: integer): int64;
procedure TBaseMemoryWorker_ReadInt64(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pint64(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadInt64(Pinteger(Params^[1])^);
end;

//function ReadString(const Address: integer; Size: integer): string;
procedure TBaseMemoryWorker_ReadString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadString(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function ReadDword(const Address: integer): DWord;
procedure TBaseMemoryWorker_ReadDword(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PDWord(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadDword(Pinteger(Params^[1])^);
end;

//function ReadCardinal(const Address: integer): cardinal;
procedure TBaseMemoryWorker_ReadCardinal(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pcardinal(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadCardinal(Pinteger(Params^[1])^);
end;

//function ReadByte(const Address: integer): byte;
procedure TBaseMemoryWorker_ReadByte(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pbyte(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadByte(Pinteger(Params^[1])^);
end;

//function ReadRegion(const Address: integer; Size: integer): Pointer;
procedure TBaseMemoryWorker_ReadRegion(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPointer(Result)^ := PBaseMemoryWorker(Params^[0])^.ReadRegion(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure WriteInteger(const Address: integer; Value: integer);
procedure TBaseMemoryWorker_WriteInteger(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteInteger(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure WriteFloat(const Address: integer; Value: double);
procedure TBaseMemoryWorker_WriteFloat(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteFloat(Pinteger(Params^[1])^, Pdouble(Params^[2])^);
end;

//procedure WriteChar(const Address: integer; Value: char);
procedure TBaseMemoryWorker_WriteChar(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteChar(Pinteger(Params^[1])^, Pchar(Params^[2])^);
end;

//procedure WriteInt64(const Address: integer; Value: int64);
procedure TBaseMemoryWorker_WriteInt64(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteInt64(Pinteger(Params^[1])^, Pint64(Params^[2])^);
end;

//procedure WriteString(const Address: integer; Value: string);
procedure TBaseMemoryWorker_WriteString(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteString(Pinteger(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure WriteDword(const Address: integer; Value: DWord);
procedure TBaseMemoryWorker_WriteDword(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteDword(Pinteger(Params^[1])^, PDWord(Params^[2])^);
end;

//procedure WriteCardinal(const Address: integer; Value: cardinal);
procedure TBaseMemoryWorker_WriteCardinal(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteCardinal(Pinteger(Params^[1])^, Pcardinal(Params^[2])^);
end;

//procedure WriteByte(const Address: integer; Value: byte);
procedure TBaseMemoryWorker_WriteByte(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteByte(Pinteger(Params^[1])^, Pbyte(Params^[2])^);
end;

//procedure WriteRegion(const Address: integer; Value: Pointer;
procedure TBaseMemoryWorker_WriteRegion(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.WriteRegion(Pinteger(Params^[1])^, Pointer(Params^[2]),Pinteger(Params^[3])^);
end;

//Read: property PID: integer read FPID write FPID;
procedure TBaseMemoryWorker_PID_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PBaseMemoryWorker(Params^[0])^.PID;
end;

//Write: property PID: integer read FPID write FPID;
procedure TBaseMemoryWorker_PID_Write(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.PID := Pinteger(Params^[1])^;
end;

//procedure Free();
procedure TBaseMemoryWorker_Free(const Params: PParamArray); lape_extdecl
begin
  PBaseMemoryWorker(Params^[0])^.Free();
end;

procedure Register_TBaseMemoryWorker(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMemoryWorker', 'TObject');

    addGlobalFunc('procedure TMemoryWorker.Init(aPID: integer);', @TBaseMemoryWorker_Init);
    addGlobalFunc('function TMemoryWorker.ReadInteger(const Address: integer): integer;', @TBaseMemoryWorker_ReadInteger);
    addGlobalFunc('function TMemoryWorker.ReadFloat(const Address: integer): double;', @TBaseMemoryWorker_ReadFloat);
    addGlobalFunc('function TMemoryWorker.ReadChar(const Address: integer): char;', @TBaseMemoryWorker_ReadChar);
    addGlobalFunc('function TMemoryWorker.ReadInt64(const Address: integer): int64;', @TBaseMemoryWorker_ReadInt64);
    addGlobalFunc('function TMemoryWorker.ReadString(const Address: integer; Size: integer): string;', @TBaseMemoryWorker_ReadString);
    addGlobalFunc('function TMemoryWorker.ReadDword(const Address: integer): DWord;', @TBaseMemoryWorker_ReadDword);
    addGlobalFunc('function TMemoryWorker.ReadCardinal(const Address: integer): cardinal;', @TBaseMemoryWorker_ReadCardinal);
    addGlobalFunc('function TMemoryWorker.ReadByte(const Address: integer): byte;', @TBaseMemoryWorker_ReadByte);
    addGlobalFunc('function TMemoryWorker.ReadRegion(const Address: integer; Size: integer): Pointer;', @TBaseMemoryWorker_ReadRegion);
    addGlobalFunc('procedure TMemoryWorker.WriteInteger(const Address: integer; Value: integer);', @TBaseMemoryWorker_WriteInteger);
    addGlobalFunc('procedure TMemoryWorker.WriteFloat(const Address: integer; Value: double);', @TBaseMemoryWorker_WriteFloat);
    addGlobalFunc('procedure TMemoryWorker.WriteChar(const Address: integer; Value: char);', @TBaseMemoryWorker_WriteChar);
    addGlobalFunc('procedure TMemoryWorker.WriteInt64(const Address: integer; Value: int64);', @TBaseMemoryWorker_WriteInt64);
    addGlobalFunc('procedure TMemoryWorker.WriteString(const Address: integer; Value: string);', @TBaseMemoryWorker_WriteString);
    addGlobalFunc('procedure TMemoryWorker.WriteDword(const Address: integer; Value: DWord);', @TBaseMemoryWorker_WriteDword);
    addGlobalFunc('procedure TMemoryWorker.WriteCardinal(const Address: integer; Value: cardinal);', @TBaseMemoryWorker_WriteCardinal);
    addGlobalFunc('procedure TMemoryWorker.WriteByte(const Address: integer; Value: byte);', @TBaseMemoryWorker_WriteByte);
    addGlobalFunc('procedure TMemoryWorker.WriteRegion(const Address: integer; Value: Pointer; Size: integer);', @TBaseMemoryWorker_WriteRegion);
    addClassVar('TMemoryWorker', 'PID', 'integer', @TBaseMemoryWorker_PID_Read, @TBaseMemoryWorker_PID_Write);
    addGlobalFunc('procedure TMemoryWorker.Free();', @TBaseMemoryWorker_Free);
  end;
end;

end.

