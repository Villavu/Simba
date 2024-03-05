unit simba.import_compress;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportCompress(Compiler: TSimbaScript_Compiler);

implementation

uses
  lptypes, ffi, SynLZ,
  simba.image, simba.image_fastcompress, simba.compress, simba.encoding;

(*
Compress
========
Compress data, strings, images.
*)

procedure _LapeImageCompressThread_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Result)^ := TImageCompressThread.Create();
end;

procedure _LapeImageCompressThread_Push(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Params^[0])^.Push(PSimbaImageArray(Params^[1])^);
end;

procedure _LapeImageCompressThread_SetOnCompressed(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Params^[0])^.OnCompressed := TImageCompressedEvent(Params^[1]^);
end;

procedure _LapeImageCompressThread_GetOnCompressed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TImageCompressedEvent(Result^) := PImageCompressThread(Params^[0])^.OnCompressed;
end;

procedure _LapeImageCompressThread_TimeUsed(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PImageCompressThread(Params^[0])^.TimeUsed;
end;

procedure _LapeImageCompressThread_IsCompressing(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PImageCompressThread(Params^[0])^.IsCompressing;
end;

procedure _LapeImageCompressThread_WaitCompressing(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PImageCompressThread(Params^[0])^.WaitCompressing();
end;

(*
CompressBytes
-------------
> function CompressBytes(Bytes: TByteArray): TByteArray;

```{note}
Zlib compression is used.
```
*)
procedure _LapeCompressBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TByteArray(Result^) := CompressBytes(TByteArray(Params^[0]^));
end;

(*
DecompressBytes
---------------
> function DecompressBytes(Bytes: TByteArray): TByteArray;

```{note}
Zlib compression is used.
```
*)
procedure _LapeDeCompressBytes(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TByteArray(Result^) := DeCompressBytes(TByteArray(Params^[0]^));
end;

(*
CompressString
--------------
> function CompressString(Data: String; Encoding: BaseEncoding = BaseEncoding.b64): String;

```{note}
Zlib compression is used.
```
*)
procedure _LapeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := CompressString(PString(Params^[0])^, BaseEncoding(Params^[1]^));
end;

(*
DecompressBytes
---------------
> function DeCompressString(Data: String; Encoding: BaseEncoding = BaseEncoding.b64): String;

```{note}
Zlib compression is used.
```
*)
procedure _LapeDeCompressString(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := DeCompressString(PString(Params^[0])^, BaseEncoding(Params^[1]^));
end;

(*
SynLZCompress
-------------
> function SynLZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZCompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
SynLZDecompress
---------------
> function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer;
*)
procedure _LapeSynLZDecompress(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompress(PPointer(Params^[0])^, PInteger(Params^[1])^, PPointer(Params^[2])^);
end;

(*
SynLZCompressDestLen
--------------------
> function SynLZCompressDestLen(Len: Integer): Integer;
*)
procedure _LapeSynLZCompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZcompressdestlen(PInteger(Params^[0])^);
end;

(*
SynLZDecompressDestLen
----------------------
> function SynLZDecompressDestLen(Src: Pointer): Integer;
*)
procedure _LapeSynLZDecompressDestLen(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := SynLZdecompressdestlen(PPointer(Params^[0])^);
end;

(*
FastCompressImage
-----------------
> procedure FastCompressImages(Images: TSimbaImageArray; var Data: Pointer; out DataSize: SizeUInt);
*)
procedure _LapeFastCompressImages(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  SimbaImage_FastCompress(TSimbaImageArray(Params^[0]^), PPointer(Params^[1])^, PSizeUInt(Params^[2])^);
end;

(*
FastDeCompressImages
--------------------
> function FastDeCompressImages(Data: Pointer; DataLen: SizeUInt): TSimbaImageArray;
*)
procedure _LapeFastDeCompressImages(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  TSimbaImageArray(Result^) := SimbaImage_FastDeCompress(PPointer(Params^[0])^, PSizeUInt(Params^[1])^);
end;

procedure ImportCompress(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    ImportingSection := 'Compress';

    addGlobalFunc('function CompressBytes(Bytes: TByteArray): TByteArray', @_LapeCompressBytes);
    addGlobalFunc('function DecompressBytes(Bytes: TByteArray): TByteArray', @_LapeDecompressBytes);

    addGlobalFunc('function CompressString(S: String; Encoding: BaseEncoding = BaseEncoding.b64): String', @_LapeCompressString);
    addGlobalFunc('function DecompressString(S: String; Encoding: BaseEncoding = BaseEncoding.b64): String', @_LapeDeCompressString);

    addGlobalFunc('function SynLZCompressDestLen(Len: Integer): Integer', @_LapeSynLZCompressDestLen);
    addGlobalFunc('function SynLZDecompressDestLen(Src: Pointer): Integer', @_LapeSynLZDecompressDestLen);
    addGlobalFunc('function SynLZCompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZCompress);
    addGlobalFunc('function SynLZDecompress(Src: Pointer; Size: Integer; Dest: Pointer): Integer', @_LapeSynLZDecompress);

    addGlobalFunc('procedure FastCompressImages(Images: TImageArray; var Data: Pointer; out DataSize: SizeUInt);', @_LapeFastCompressImages);
    addGlobalFunc('function FastDeCompressImages(Data: Pointer; out DataLen: SizeUInt): TImageArray', @_LapeFastDeCompressImages);

    addClass('TImageCompressThread');
    addClassConstructor('TImageCompressThread', '', @_LapeImageCompressThread_Create);
    addGlobalType('procedure(Sender: TImageCompressThread; Images: TImageArray; Data: Pointer; DataSize: SizeUInt) of object', 'TImageCompressedEvent', FFI_DEFAULT_ABI);
    addGlobalFunc('procedure TImageCompressThread.Push(Images: TImageArray)', @_LapeImageCompressThread_Push);
    addGlobalFunc('procedure TImageCompressThread.SetOnCompressed(Value: TImageCompressedEvent)', @_LapeImageCompressThread_SetOnCompressed);
    addGlobalFunc('function TImageCompressThread.GetOnCompressed: TImageCompressedEvent', @_LapeImageCompressThread_GetOnCompressed);
    addGlobalFunc('function TImageCompressThread.TimeUsed: Double', @_LapeImageCompressThread_TimeUsed);
    addGlobalFunc('function TImageCompressThread.IsCompressing: Boolean', @_LapeImageCompressThread_IsCompressing);
    addGlobalFunc('procedure TImageCompressThread.WaitCompressing;', @_LapeImageCompressThread_WaitCompressing);

    ImportingSection := '';
  end;
end;

end.

