unit simba.import_pointbuffer;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportPointBuffer(Compiler: TSimbaScript_Compiler);

implementation

uses
  Graphics, lptypes, lpvartypes,
  simba.arraybuffer;

type
  PSimbaPointBuffer = ^TSimbaPointBuffer;

procedure _LapePointBuffer_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaPointBuffer(Params^[0])^.Init(PInteger(Params^[1])^);
end;

procedure _LapePointBuffer_InitWith(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaPointBuffer(Params^[0])^.InitWith(PPointArray(Params^[1])^);
end;

procedure _LapePointBuffer_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaPointBuffer(Params^[0])^.Clear();
end;

procedure _LapePointBuffer_Add1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaPointBuffer(Params^[0])^.Add(PPoint(Params^[1])^);
end;

procedure _LapePointBuffer_Add2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaPointBuffer(Params^[0])^.Add(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePointBuffer_ToArray(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPointArray(Result)^ := PSimbaPointBuffer(Params^[0])^.ToArray(PBoolean(Params^[1])^);
end;

procedure _LapePointBuffer_First(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaPointBuffer(Params^[0])^.First();
end;

procedure _LapePointBuffer_Last(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaPointBuffer(Params^[0])^.Last();
end;

procedure _LapePointBuffer_Pop(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaPointBuffer(Params^[0])^.Pop();
end;

procedure _LapePointBuffer_Clear(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSimbaPointBuffer(Params^[0])^.Clear();
end;

procedure _LapePointBuffer_Size(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaPointBuffer(Params^[0])^.Size;
end;

procedure _LapePointBuffer_Count(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PSimbaPointBuffer(Params^[0])^.Count;
end;

procedure _LapePointBuffer_Point(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PSimbaPointBuffer(Params^[0])^.Item[PInteger(Params^[1])^];
end;

procedure ImportPointBuffer(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType([
      'record',
      '  {%CODETOOLS OFF}',
      '  __Length: Integer;',
      '  __Count: Integer;',
      '  __Arr: TPointArray;',
      '  {%CODETOOLS ON}',
      'end;'],
      'TPointBuffer'
    );
    if (getGlobalType('TPointBuffer').Size <> SizeOf(TSimbaPointBuffer)) then
      SimbaException('SizeOf(TPointBuffer) is wrong');

    addDelayedCode([
      'function ToString(constref PointBuffer: TPointBuffer): String; override;',
      'begin',
      '  Result := "TPointBuffer: Size=" + IntToStr(PointBuffer.Size()) + ", Count=" + IntToStr(PointBuffer.Count());',
      'end;'
    ]);

    addGlobalFunc('procedure TPointBuffer.Init(const InitialSize: Integer = 1024);', @_LapePointBuffer_Init);
    addGlobalFunc('procedure TPointBuffer.InitWith(const Values: TPointArray);', @_LapePointBuffer_InitWith);

    addGlobalFunc('procedure TPointBuffer.Clear;', @_LapePointBuffer_Clear);

    addGlobalFunc('procedure TPointBuffer.Add(Value: TPoint); overload;', @_LapePointBuffer_Add1);
    addGlobalFunc('procedure TPointBuffer.Add(X, Y: Integer); overload;', @_LapePointBuffer_Add2);
    addGlobalFunc('function TPointBuffer.ToArray(Copy: Boolean = True): TPointArray;', @_LapePointBuffer_ToArray);

    addGlobalFunc('function TPointBuffer.First: TPoint;', @_LapePointBuffer_First);
    addGlobalFunc('function TPointBuffer.Last: TPoint;', @_LapePointBuffer_Last);
    addGlobalFunc('function TPointBuffer.Pop: TPoint;', @_LapePointBuffer_Pop);

    addGlobalFunc('function TPointBuffer.Size: Integer;', @_LapePointBuffer_Size);
    addGlobalFunc('function TPointBuffer.Count: Integer;', @_LapePointBuffer_Count);
    addGlobalFunc('function TPointBuffer.Point(Index: Integer): TPoint;', @_LapePointBuffer_Point);
  end;
end;

end.

