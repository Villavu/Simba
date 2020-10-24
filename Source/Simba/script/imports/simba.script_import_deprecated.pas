unit simba.script_import_deprecated;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Deprecated(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  DCPcrypt2, DCPmd4, DCPmd5, DCPtiger, DCPsha1, DCPsha256, DCPsha512, DCPhaval, DCPripemd128, DCPripemd160, DCPrc2,
  forms, simba.tpa, math {$IFDEF WINDOWS}, windows{$ENDIF},
  simba.iomanager;

type
  THashType = (htHaval, htMD4, htMD5, htRIPEMD128, htRIPEMD160,
               htSHA1, htSHA256, htSHA384, htSHA512, htTiger);
  PHashType = ^THashType;

procedure _Encrypt(CipherType: TDCP_cipherclass; HashType: TDCP_hashclass; const Key: string; var Data: string);
begin
  with CipherType.Create(nil) do
  try
    InitStr(Key, HashType);
    Data := EncryptString(Data);
    Burn;
  finally
    Free;
  end;
end;

procedure _Decrypt(CipherType: TDCP_cipherclass; HashType: TDCP_hashclass; const Key: string; var Data: string);
begin
  with CipherType.Create(nil) do
  try
    InitStr(Key, HashType);
    Data := DecryptString(Data);
    Burn;
  finally
    Free;
  end;
end;

function _Hash(HashType: TDCP_hashclass; Data: string): string;
var
  Digest: Pointer;
  I, Size: LongInt;
begin
  with HashType.Create(nil) do
  try
    Init();
    UpdateStr(Data);

    Size := (GetHashSize() div 8);
    Digest := GetMem(Size);
    FillChar(Digest^, Size, $00);
    Final(Digest^);
  finally
    Free;
  end;

  Dec(Size);
  Result := '';
  for I := 0 to Size do
    Result := Result + IntToHex(PByte(Digest)[I], 2);

  FreeMem(Digest, Size + 1);
end;

function _HashType2Class(const HashType: THashType): TDCP_hashclass; inline;
begin
  case (HashType) of
    htHaval: Result := TDCP_Haval;
    htMD4: Result := TDCP_MD4;
    htMD5: Result := TDCP_MD5;
    htRIPEMD128: Result := TDCP_RIPEMD128;
    htRIPEMD160: Result := TDCP_RIPEMD160;
    htSHA1: Result := TDCP_SHA1;
    htSHA256: Result := TDCP_SHA256;
    htSHA384: Result := TDCP_SHA384;
    htSHA512: Result := TDCP_SHA512;
    htTiger: Result := TDCP_Tiger;
  else
    Result := TDCP_SHA1;
  end;
end;

procedure Lape_Hash(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(_HashType2Class(PHashType(Params^[0])^), PString(Params^[1])^);
end;

procedure Lape_haval(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_Haval, PString(Params^[0])^);
end;

procedure Lape_md4(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_MD4, PString(Params^[0])^);
end;

procedure Lape_md5(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_MD5, PString(Params^[0])^);
end;

procedure Lape_ripemd128(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_RipeMD128, PString(Params^[0])^);
end;

procedure Lape_ripemd160(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_RipeMD160, PString(Params^[0])^);
end;

procedure Lape_sha1(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_SHA1, PString(Params^[0])^);
end;

procedure Lape_sha256(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_SHA256, PString(Params^[0])^);
end;

procedure Lape_sha384(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_SHA384, PString(Params^[0])^);
end;

procedure Lape_sha512(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_SHA512, PString(Params^[0])^);
end;

procedure Lape_tiger(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := _Hash(TDCP_Tiger, PString(Params^[0])^);
end;

procedure Lape_rc2_encrypt(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Encrypt(TDCP_RC2, _HashType2Class(PHashType(Params^[0])^), PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_rc2_decrypt(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  _Decrypt(TDCP_RC2, _HashType2Class(PHashType(Params^[0])^), PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_rs_GetUpText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MOCR.GetUpTextAt(7, 7, True);
end;

procedure Lape_rs_GetUpTextAt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MOCR.GetUpTextAt(PInt32(Params^[0])^, PInt32(Params^[1])^, True);
end;

procedure Lape_rs_GetUpTextAtEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := SimbaScript.Client.MOCR.GetUpTextAtEx(PInt32(Params^[0])^, PInt32(Params^[1])^, True, PString(Params^[2])^);
end;

type
  TMessageBox = class
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TMessageBox.Execute;
begin
  PInt32(Result)^ := Application.MessageBox(PChar(PString(Params^[0])^), PChar(PString(Params^[1])^), PInt32(Params^[2])^);
end;

procedure Lape_MessageBox(const Params: PParamArray; const Result: Pointer);
var
  MessageBox: TMessageBox;
begin
  MessageBox := TMessageBox.Create();
  MessageBox.Params := Params;
  MessageBox.Result := Result;

  TThread.Synchronize(nil, @MessageBox.Execute);

  MessageBox.Free();
end;

procedure Lape_tSwap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  tSwap(PPoint(Params^[0])^, PPoint(Params^[1])^);
end;

procedure Lape_tpaSwap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  tpaSwap(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_SwapE(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  SwapE(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_MinE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Math.Min(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

procedure Lape_MaxE(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PExtended(Result)^ := Math.Max(PExtended(Params^[0])^, PExtended(Params^[1])^);
end;

{$IFDEF WINDOWS}
threadvar
  ProcArr: TSysProcArr;

function EnumProcess(WindowHandle: HWND; Param: LPARAM): WINBOOL; stdcall;
var
  I: integer;
  PID: DWORD;
  R: TRect;
begin
  R := Default(TRect);
  PID := 0;

  Result := (not ((WindowHandle = 0) or (WindowHandle = null)));
  if ((Result) and (IsWindowVisible(WindowHandle))) then
  begin
    I := Length(ProcArr);
    SetLength(ProcArr, I + 1);
    ProcArr[I].Handle := WindowHandle;
    SetLength(ProcArr[I].Title, 255);
    SetLength(ProcArr[I].Title, GetWindowTextW(WindowHandle, PWideChar(ProcArr[I].Title), Length(ProcArr[I].Title)));
    GetWindowRect(WindowHandle, R);
    ProcArr[i].Width := R.Right - R.Left;
    ProcArr[i].Height := R.Bottom - R.Top;
    GetWindowThreadProcessId(WindowHandle, PID);
    ProcArr[I].Pid := PID;
  end;
end;
{$ENDIF}

procedure Lape_GetProcesses(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
{$IFDEF WINDOWS}
  SetLength(ProcArr, 0);
  EnumWindows(@EnumProcess, 0);
  TSysProcArr(Result^) := ProcArr;
{$ELSE}
  raise Exception.Create('GetProcesses is not available on this platform');
{$ENDIF}
end;

//function SetTarget(target: TNativeWindow): integer; overload;
procedure TIOManager_SetTargetHandle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := TIOManager(Params^[0]^).SetTarget(PPtrUInt(Params^[1])^);
end;

procedure Lape_TPAPosNext(const Params: PParamArray;const Result: pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PInt32(Result)^:= TPAPosNext(PPoint(Params^[0])^, PPointArray(Params^[1])^, PInt32(Params^[2])^, System.PBoolean(Params^[3])^);
end;

procedure Lape_GlueTPAs(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PPointArray(Result)^:= GlueTPAs(PPointArray(Params^[0])^, PPointArray(Params^[1])^, System.PBoolean(Params^[2])^, System.PBoolean(Params^[3])^);
end;

procedure Lape_CombineIntArray(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PIntegerArray(Result)^ := CombineIntArray(PIntegerArray(Params^[0])^, PIntegerArray(Params^[1])^);
end;

procedure Lape_CopyTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CopyTPA(PPointArray(Params^[0])^);
end;

procedure Lape_CopyATPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  P2DPointArray(Result)^ := CopyATPA(P2DPointArray(Params^[0])^);
end;

procedure Lape_AppendTPA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  AppendTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_CombineTPA(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPointArray(Result)^ := CombineTPA(PPointArray(Params^[0])^, PPointArray(Params^[1])^);
end;

procedure Lape_Import_Deprecated(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

  function Deprecated(Message: String): String;
  begin
    Result := ' deprecated ' + #39 + Message + #39 + ';';
  end;

begin
  with Compiler do
  begin
    Section := 'Deprecated';

    addGlobalType('(htHaval, htMD4, htMD5, htRIPEMD128, htRIPEMD160, htSHA1, htSHA256, htSHA384, htSHA512, htTiger)', 'THashType');

    addGlobalFunc('function GetProcesses: TSysProcArr;' + Deprecated('Use TOSWindow'), @Lape_GetProcesses);
    addGlobalFunc('function TIOManager.GetProcesses: TSysProcArr; constref;' + Deprecated('Use TOSWindow'), @Lape_GetProcesses);
    addGlobalFunc('function TIOManager.SetTarget2(target: TNativeWindow): integer; constref;' + Deprecated('TIOManager.SetTarget'), @TIOManager_SetTargetHandle);

    addGlobalFunc('procedure tSwap(var a, b: TPoint); ' + Deprecated('Use Swap'), @Lape_tSwap);
    addGlobalFunc('procedure tpaSwap(var a, b: TPointArray); ' + Deprecated('Use Swap'), @Lape_tpaSwap);
    addGlobalFunc('procedure SwapE(var a, b: Extended); ' + Deprecated('Use Swap'), @Lape_SwapE);

    addGlobalFunc('function CopyTPA(const TPA: TPointArray): TPointArray;' + Deprecated('Use Copy'), @Lape_CopyTPA);
    addGlobalFunc('function CopyATPA(const ATPA: T2DPointArray): T2DPointArray;' + Deprecated('Use Copy'), @Lape_CopyATPA);

    addGlobalFunc('function TPAPosNext(const Find: TPoint; const V: TPointArray; const PrevPos: Int32;const IsSortedAscending: Boolean): Int32; deprecated;', @Lape_TPAPosNext);
    addGlobalFunc('function GlueTPAs(const V1, V2: TPointArray; const IsSortedAscending, byDifference: Boolean): TPointArray; deprecated;', @Lape_GlueTPAs);

    addGlobalFunc('function CombineTPA(const Ar1, Ar2: TPointArray): TPointArray;' + Deprecated('Use + operator'), @Lape_CombineTPA);
    addGlobalFunc('function CombineIntArray(const Arr1, Arr2: TIntegerArray): TIntegerArray;' + Deprecated('Use + operator'), @Lape_CombineIntArray);
    addGlobalFunc('procedure AppendTPA(var TPA: TPointArray; const ToAppend: TPointArray);' + Deprecated('Use + operator'), @Lape_AppendTPA);

    addGlobalFunc('function MinE(a, b: Extended): Extended; ' + Deprecated('Use Min'), @Lape_MinE);
    addGlobalFunc('function MaxE(a, b: Extended): Extended; ' + Deprecated('Use Min'), @Lape_MaxE);

    addGlobalFunc('function hash(const HashType: THashType; const Data: string): string; deprecated;', @Lape_hash);
    addGlobalFunc('function haval(const Data: string): string; deprecated;', @Lape_haval);
    addGlobalFunc('function md4(const Data: string): string; deprecated;', @Lape_md4);
    addGlobalFunc('function md5(const Data: string): string; deprecated;', @Lape_md5);
    addGlobalFunc('function ripemd128(const Data: string): string; deprecated;', @Lape_ripemd128);
    addGlobalFunc('function ripemd160(const Data: string): string; deprecated;', @Lape_ripemd160);
    addGlobalFunc('function sha1(const Data: string): string; deprecated;', @Lape_sha1);
    addGlobalFunc('function sha256(const Data: string): string; deprecated;', @Lape_sha256);
    addGlobalFunc('function sha384(const Data: string): string; deprecated;', @Lape_sha384);
    addGlobalFunc('function sha512(const Data: string): string; deprecated;', @Lape_sha512);
    addGlobalFunc('function tiger(const Data: string): string; deprecated;', @Lape_tiger);
    addGlobalFunc('procedure rc2_encrypt(const Key: string; const HashType: THashType; var Data: string); deprecated;', @Lape_rc2_encrypt);
    addGlobalFunc('procedure rc2_decrypt(const Key: string; const HashType: THashType; var Data: string); deprecated;', @Lape_rc2_decrypt);

    addGlobalFunc('function rs_GetUpText: string; deprecated;', @Lape_rs_GetUpText);
    addGlobalFunc('function rs_GetUpTextAt(x, y : integer): string; deprecated;', @Lape_rs_GetUpTextAt);
    addGlobalFunc('function rs_GetUpTextAtEx(x, y: integer; shadow: boolean; fontname: string): string; deprecated;', @Lape_rs_GetUpTextAtEx);

    addGlobalFunc('function MessageBox(Text, Caption: String; Flags: Int32): Int32;' + Deprecated('Use MessageDlg'), @Lape_MessageBox);

    addDelayedCode('{$HINTS OFF}'                                                                                                                             + LineEnding +
                   'type'                                                                                                                                     + LineEnding +
                   '  TSP_Property = (SP_OnTerminate, SP_WriteTimeStamp);'                                                                                    + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure SetScriptProp(Prop: TSP_Property; Value: TVariantArray); deprecated;'                                                           + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  case Prop of'                                                                                                                           + LineEnding +
                   '    SP_OnTerminate:'                                                                                                                      + LineEnding +
                   '      AddOnTerminate(Value[0]);'                                                                                                          + LineEnding +
                   '    SP_WriteTimeStamp:'                                                                                                                   + LineEnding +
                   '      ;'                                                                                                                                  + LineEnding +
                   '  end;'                                                                                                                                   + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'function GetTClient: TClient; deprecated ' + #39 + 'Replace with `Client` variable' + #39 + ';'                                           + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Result := Client;'                                                                                                                      + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   'procedure SplitTPAExWrap(const arr: TPointArray; w, h: Integer; var res : T2DPointArray); deprecated;'                                    + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  res := SplitTPAEx(arr,w,h);'                                                                                                            + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure SplitTPAWrap(const arr: TPointArray; Dist: Integer; var res: T2DPointArray); deprecated;'                                       + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  res := SplitTPA(arr,dist);'                                                                                                             + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure FindGapsTPAWrap(const TPA: TPointArray; MinPixels: Integer; var Res: T2DPointArray); deprecated;'                               + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := FindGapsTPA(TPA,MinPixels);'                                                                                                     + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure RemoveDistTPointArrayWrap(X, Y, Dist: Int32; TPA: TPointArray; Higher: Boolean; var Res: TPointArray); deprecated;'             + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res :=  RemoveDistTPointArray(X,Y,Dist,TPA,Higher);'                                                                                    + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure CombineTPAWrap(const Ar1, Ar2: TPointArray; var Res: TPointArray); deprecated;'                                                 + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := CombineTPA(Ar1,Ar2);'                                                                                                            + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure ReArrangeandShortenArrayExWrap(const a: TPointArray; w, h: Integer; var Res :  TPointArray); deprecated;'                       + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := ReArrangeandShortenArrayEx(a,w,h);'                                                                                              + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure ReArrangeandShortenArrayWrap(const a: TPointArray; Dist: Integer; var Res :  TPointArray); deprecated;'                         + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := ReArrangeandShortenArray(a,dist);'                                                                                               + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAtoATPAExWrap(const TPA: TPointArray; w, h: Integer; var Res :  T2DPointArray); deprecated;'                                  + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := TPAtoATPAEx(TPA,w,h);'                                                                                                           + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAtoATPAWrap(const TPA: TPointArray; Dist: Integer; var Res :  T2DPointArray); deprecated;'                                    + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
'                     Res := TPAtoATPA(TPA,Dist);'                                                                                                            + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure MiddleTPAWrap(const TPA: TPointArray; var Res: TPoint); deprecated;'                                                            + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := MiddleTPA(TPA);'                                                                                                                 + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure CombineIntArrayWrap(const Ar1, Ar2: TIntegerArray; var Res :  TIntegerArray); deprecated;'                                      + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := CombineIntArray(Ar1,Ar2);'                                                                                                       + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure MergeATPAWrap(const ATPA : T2DPointArray; var Res: TPointArray); deprecated;'                                                   + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := MergeATPA(ATPA);'                                                                                                                + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAFromLineWrap(const x1, y1, x2, y2: Integer; var Res: TPointArray); deprecated;'                                              + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := TPAFromLine(x1, y1, x2, y2);'                                                                                                    + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure EdgeFromBoxWrap(const Box: TBox; var Res: TPointArray); deprecated;'                                                            + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := EdgeFromBox(Box);'                                                                                                               + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAFromBoxWrap(const Box : TBox; var Res : TPointArray); deprecated;'                                                           + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := TPAFromBox(Box);'                                                                                                                + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAFromEllipseWrap(const CX, CY, XRadius, YRadius : Integer; var Res : TPointArray); deprecated;'                               + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := TPAFromEllipse(CX, CY, XRadius, YRadius);'                                                                                       + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAFromCircleWrap(const CX, CY, Radius: Integer; var Res : TPointArray); deprecated;'                                           + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := TPAFromCircle(CX, CY, Radius);'                                                                                                  + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure RotatePointsWrap(Const P: TPointArray; A, cx, cy: Extended; var Res :  TPointArray); deprecated;'                               + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := RotatePoints(P,a,cx,cy);'                                                                                                        + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure FindTPAEdgesWrap(const p: TPointArray; var Res:  TPointArray); deprecated;'                                                     + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := FindTPAEdges(p);'                                                                                                                + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure ClearTPAFromTPAWrap(const arP, ClearPoints: TPointArray; var Res :  TPointArray); deprecated;'                                  + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := ClearTPAFromTPA(arP, clearpoints);'                                                                                              + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure ReturnPointsNotInTPAWrap(Const TotalTPA: TPointArray; const Box: TBox; var Res :  TPointArray); deprecated;'                    + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  Res := ReturnPointsNotInTPA(TotalTPA,box);'                                                                                             + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure ExplodeWrap(del, str: string; var res : TStringArray); deprecated;'                                                             + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  res := Explode(del,str);'                                                                                                               + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure GetColorsWrap(Coords: TPointArray; var res: TIntegerArray); deprecated;'                                                        + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  res := GetColors(Coords);'                                                                                                              + LineEnding +
                   'end;'                                                                                                                                     + LineEnding +
                   ''                                                                                                                                         + LineEnding +
                   'procedure TPAFromTextWrap(const text, font: String; var w,h: integer; out res: TPointArray); deprecated;'                                 + LineEnding +
                   'begin'                                                                                                                                    + LineEnding +
                   '  res := TPAFromText(text, font, w, h);'                                                                                                  + LineEnding +
                   'end;',
                   'Deprecated');
  end;
end;

end.

