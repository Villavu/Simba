unit simba.import_compress;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

procedure ImportCompress(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi, synlz, simba.compress;

procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := CompressString(PString(Params^[0])^);
end;

procedure _LapeDecompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pstring(Result)^ := DecompressString(PString(Params^[0])^);
end;

procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress1pas(PPAnsiChar(Params^[0])^, PInteger(Params^[1])^, PPAnsiChar(Params^[2])^);
end;

procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPAnsiChar(Params^[0])^);
end;

procedure _LapeLZCompressionThread_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Result)^ := TLZCompressionThread.Create();
end;

procedure _LapeLZCompressionThread_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.Free();
end;

procedure _LapeLZCompressionThread_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.Write(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeLZCompressionThread_WaitCompressing(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.WaitCompressing();
end;

procedure _LapeLZCompressionThread_IsCompressing(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLZCompressionThread(Params^[0])^.IsCompressing;
end;

procedure _LapeLZCompressionThreadOnCompressed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressedEvent(Result)^ := PLZCompressionThread(Params^[0])^.OnCompressed;
end;

procedure _LapeLZCompressionThreadOnCompressed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLZCompressionThread(Params^[0])^.OnCompressed := PLZCompressedEvent(Params^[1])^;
end;

procedure ImportCompress(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Compress';

    addGlobalFunc('function CompressString(S: String): String', @_LapeCompressString);
    addGlobalFunc('function DecompressString(S: String): String', @_LapeDecompressString);

    addGlobalFunc('function LZCompressDestLen(Len: Integer): Integer', @_LapeSynLZCompressDestLen);
    addGlobalFunc('function LZDecompressDestLen(Src: Pointer): Integer', @_LapeSynLZDecompressDestLen);
    addGlobalFunc('function LZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZCompress);
    addGlobalFunc('function LZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZDecompress);

    addGlobalType('type Pointer', 'TLZCompressionThread');
    addGlobalType('procedure(Sender: TLZCompressionThread; Data: Pointer; DataSize: Integer; TimeUsed: Double) of object', 'TLZCompressedEvent', FFI_DEFAULT_ABI);

    addGlobalFunc('function TLZCompressionThread.Create: TLZCompressionThread; static', @_LapeLZCompressionThread_Create);
    addGlobalFunc('procedure TLZCompressionThread.Free;', @_LapeLZCompressionThread_Free);
    addGlobalFunc('procedure TLZCompressionThread.Write(constref Data; DataSize: Integer)', @_LapeLZCompressionThread_Write);
    addGlobalFunc('procedure TLZCompressionThread.WaitCompressing', @_LapeLZCompressionThread_WaitCompressing);
    addGlobalFunc('procedure TLZCompressionThread.SetOnCompressed(Value: TLZCompressedEvent)', @_LapeLZCompressionThreadOnCompressed_Write);
    addGlobalFunc('function TLZCompressionThread.GetOnCompressed: TLZCompressedEvent', @_LapeLZCompressionThreadOnCompressed_Read);
    addGlobalFunc('function TLZCompressionThread.IsCompressing: Boolean', @_LapeLZCompressionThread_IsCompressing);

    ImportingSection := '';
  end;
end;

end.

