unit lpblockserial;
//Depends: TBlockSerial, TObject, Tdcb, termios, string; virtual, boolean); virtual, string, pointer, integer, byte, AnsiString, TStream, Integer, boolean, Boolean, integer): string, THandle, THookSerialStatus

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils,synaser, lpcompiler, lptypes, lplclsystem, script_imports;

procedure Register_TBlockSerial(Compiler: TLapeCompiler);

implementation

type
  PBlockSerial = ^TBlockSerial;

//constructor Create;
procedure TBlockSerial_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^ := TBlockSerial.Create();
end;

//function GetVersion: string;
procedure TBlockSerial_GetVersion(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PBlockSerial(Params^[0])^.GetVersion();
end;

//procedure CloseSocket;
procedure TBlockSerial_CloseSocket(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.CloseSocket();
end;

//procedure Config(baud, bits: integer; parity: char; stop: integer;softflow, hardflow: boolean);
procedure TBlockSerial_Config(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Config(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pchar(Params^[3])^, Pinteger(Params^[4])^, Pboolean(Params^[5])^, Pboolean(Params^[6])^);
end;

//procedure Connect(comport: string);
procedure TBlockSerial_Connect(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Connect(PlpString(Params^[1])^);
end;

//procedure SetCommState;
procedure TBlockSerial_SetCommState(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SetCommState();
end;

//procedure GetCommState;
procedure TBlockSerial_GetCommState(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.GetCommState();
end;

//function SendBuffer(buffer: pointer; length: integer): integer;
procedure TBlockSerial_SendBuffer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.SendBuffer(Ppointer(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure SendByte(data: byte);
procedure TBlockSerial_SendByte(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendByte(Pbyte(Params^[1])^);
end;

//procedure SendString(data: AnsiString);
procedure TBlockSerial_SendString(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendString(PAnsiString(Params^[1])^);
end;

//procedure SendInteger(Data: integer);
procedure TBlockSerial_SendInteger(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendInteger(Pinteger(Params^[1])^);
end;

//procedure SendBlock(const Data: AnsiString);
procedure TBlockSerial_SendBlock(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendBlock(PAnsiString(Params^[1])^);
end;

//procedure SendStreamRaw(const Stream: TStream);
procedure TBlockSerial_SendStreamRaw(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendStreamRaw(PStream(Params^[1])^);
end;

//procedure SendStream(const Stream: TStream);
procedure TBlockSerial_SendStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendStream(PStream(Params^[1])^);
end;

//procedure SendStreamIndy(const Stream: TStream);
procedure TBlockSerial_SendStreamIndy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SendStreamIndy(PStream(Params^[1])^);
end;

//function RecvBuffer(buffer: pointer; length: integer): integer;
procedure TBlockSerial_RecvBuffer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.RecvBuffer(Ppointer(Params^[1])^, Pinteger(Params^[2])^);
end;

//function RecvBufferEx(buffer: pointer; length: integer; timeout: integer): integer;
procedure TBlockSerial_RecvBufferEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.RecvBufferEx(Ppointer(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^);
end;

//function RecvBufferStr(Length: Integer; Timeout: Integer): AnsiString;
procedure TBlockSerial_RecvBufferStr(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.RecvBufferStr(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function RecvPacket(Timeout: Integer): AnsiString;
procedure TBlockSerial_RecvPacket(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.RecvPacket(PInteger(Params^[1])^);
end;

//function RecvByte(timeout: integer): byte;
procedure TBlockSerial_RecvByte(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pbyte(Result)^ := PBlockSerial(Params^[0])^.RecvByte(Pinteger(Params^[1])^);
end;

//function RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString;
procedure TBlockSerial_RecvTerminated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.RecvTerminated(PInteger(Params^[1])^, PAnsiString(Params^[2])^);
end;

//function Recvstring(timeout: integer): AnsiString;
procedure TBlockSerial_Recvstring(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.Recvstring(Pinteger(Params^[1])^);
end;

//function RecvInteger(Timeout: Integer): Integer;
procedure TBlockSerial_RecvInteger(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBlockSerial(Params^[0])^.RecvInteger(PInteger(Params^[1])^);
end;

//function RecvBlock(Timeout: Integer): AnsiString;
procedure TBlockSerial_RecvBlock(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.RecvBlock(PInteger(Params^[1])^);
end;

//procedure RecvStreamRaw(const Stream: TStream; Timeout: Integer);
procedure TBlockSerial_RecvStreamRaw(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RecvStreamRaw(PStream(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer);
procedure TBlockSerial_RecvStreamSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RecvStreamSize(PStream(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure RecvStream(const Stream: TStream; Timeout: Integer);
procedure TBlockSerial_RecvStream(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RecvStream(PStream(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure RecvStreamIndy(const Stream: TStream; Timeout: Integer);
procedure TBlockSerial_RecvStreamIndy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RecvStreamIndy(PStream(Params^[1])^, PInteger(Params^[2])^);
end;

//function WaitingData: integer;
procedure TBlockSerial_WaitingData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.WaitingData();
end;

//function WaitingDataEx: integer;
procedure TBlockSerial_WaitingDataEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.WaitingDataEx();
end;

//function SendingData: integer;
procedure TBlockSerial_SendingData(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.SendingData();
end;

//procedure EnableRTSToggle(value: boolean);
procedure TBlockSerial_EnableRTSToggle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.EnableRTSToggle(Pboolean(Params^[1])^);
end;

//procedure Flush;
procedure TBlockSerial_Flush(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Flush();
end;

//procedure Purge;
procedure TBlockSerial_Purge(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Purge();
end;

//function CanRead(Timeout: integer): boolean;
procedure TBlockSerial_CanRead(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.CanRead(Pinteger(Params^[1])^);
end;

//function CanWrite(Timeout: integer): boolean;
procedure TBlockSerial_CanWrite(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.CanWrite(Pinteger(Params^[1])^);
end;

//function CanReadEx(Timeout: integer): boolean;
procedure TBlockSerial_CanReadEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.CanReadEx(Pinteger(Params^[1])^);
end;

//function ModemStatus: integer;
procedure TBlockSerial_ModemStatus(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.ModemStatus();
end;

//procedure SetBreak(Duration: integer);
procedure TBlockSerial_SetBreak(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SetBreak(Pinteger(Params^[1])^);
end;

//function ATCommand(value: AnsiString): AnsiString;
procedure TBlockSerial_ATCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.ATCommand(PAnsiString(Params^[1])^);
end;

//function ATConnect(value: AnsiString): AnsiString;
procedure TBlockSerial_ATConnect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.ATConnect(PAnsiString(Params^[1])^);
end;

//function SerialCheck(SerialResult: integer): integer;
procedure TBlockSerial_SerialCheck(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.SerialCheck(Pinteger(Params^[1])^);
end;

//procedure ExceptCheck;
procedure TBlockSerial_ExceptCheck(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.ExceptCheck();
end;

//procedure SetSynaError(ErrNumber: integer);
procedure TBlockSerial_SetSynaError(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SetSynaError(Pinteger(Params^[1])^);
end;

//procedure RaiseSynaError(ErrNumber: integer);
procedure TBlockSerial_RaiseSynaError(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RaiseSynaError(Pinteger(Params^[1])^);
end;

//Read: property Device: string read FDevice;
procedure TBlockSerial_Device_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PBlockSerial(Params^[0])^.Device;
end;

//Read: property LastError: integer read FLastError;
procedure TBlockSerial_LastError_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.LastError;
end;

//Read: property LastErrorDesc: string read FLastErrorDesc;
procedure TBlockSerial_LastErrorDesc_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PBlockSerial(Params^[0])^.LastErrorDesc;
end;

//Read: property ATResult: Boolean read FATResult;
procedure TBlockSerial_ATResult_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBlockSerial(Params^[0])^.ATResult;
end;

//Write: property RTS: Boolean write SetRTSF;
procedure TBlockSerial_RTS_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RTS := PBoolean(Params^[1])^;
end;

//Read: property CTS: boolean read GetCTS;
procedure TBlockSerial_CTS_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.CTS;
end;

//Write: property DTR: Boolean write SetDTRF;
procedure TBlockSerial_DTR_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.DTR := PBoolean(Params^[1])^;
end;

//Read: property DSR: boolean read GetDSR;
procedure TBlockSerial_DSR_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.DSR;
end;

//Read: property Carrier: boolean read GetCarrier;
procedure TBlockSerial_Carrier_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.Carrier;
end;

//Read: property Ring: boolean read GetRing;
procedure TBlockSerial_Ring_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.Ring;
end;

//Read: property MaxSendBandwidth: Integer read FMaxSendBandwidth Write FMaxSendBandwidth;
procedure TBlockSerial_MaxSendBandwidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBlockSerial(Params^[0])^.MaxSendBandwidth;
end;

//Write: property MaxSendBandwidth: Integer read FMaxSendBandwidth Write FMaxSendBandwidth;
procedure TBlockSerial_MaxSendBandwidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.MaxSendBandwidth := PInteger(Params^[1])^;
end;

//Read: property MaxRecvBandwidth: Integer read FMaxRecvBandwidth Write FMaxRecvBandwidth;
procedure TBlockSerial_MaxRecvBandwidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBlockSerial(Params^[0])^.MaxRecvBandwidth;
end;

//Write: property MaxRecvBandwidth: Integer read FMaxRecvBandwidth Write FMaxRecvBandwidth;
procedure TBlockSerial_MaxRecvBandwidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.MaxRecvBandwidth := PInteger(Params^[1])^;
end;

//Write: property MaxBandwidth: Integer Write SetBandwidth;
procedure TBlockSerial_MaxBandwidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.MaxBandwidth := PInteger(Params^[1])^;
end;

//Read: property SizeRecvBuffer: integer read FRecvBuffer write SetSizeRecvBuffer;
procedure TBlockSerial_SizeRecvBuffer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.SizeRecvBuffer;
end;

//Write: property SizeRecvBuffer: integer read FRecvBuffer write SetSizeRecvBuffer;
procedure TBlockSerial_SizeRecvBuffer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.SizeRecvBuffer := Pinteger(Params^[1])^;
end;

//function GetErrorDesc(ErrorCode: integer): string;
procedure TBlockSerial_GetErrorDesc(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PBlockSerial(Params^[0])^.GetErrorDesc(Pinteger(Params^[1])^);
end;

//Read: property Tag: integer read FTag write FTag;
procedure TBlockSerial_Tag_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.Tag;
end;

//Write: property Tag: integer read FTag write FTag;
procedure TBlockSerial_Tag_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Tag := Pinteger(Params^[1])^;
end;

//Read: property Handle: THandle read Fhandle write FHandle;
procedure TBlockSerial_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PBlockSerial(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read Fhandle write FHandle;
procedure TBlockSerial_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//Read: property LineBuffer: AnsiString read FBuffer write FBuffer;
procedure TBlockSerial_LineBuffer_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAnsiString(Result)^ := PBlockSerial(Params^[0])^.LineBuffer;
end;

//Write: property LineBuffer: AnsiString read FBuffer write FBuffer;
procedure TBlockSerial_LineBuffer_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.LineBuffer := PAnsiString(Params^[1])^;
end;

//Read: property RaiseExcept: boolean read FRaiseExcept write FRaiseExcept;
procedure TBlockSerial_RaiseExcept_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.RaiseExcept;
end;

//Write: property RaiseExcept: boolean read FRaiseExcept write FRaiseExcept;
procedure TBlockSerial_RaiseExcept_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.RaiseExcept := Pboolean(Params^[1])^;
end;

//Read: property TestDSR: boolean read FTestDSR write FTestDSR;
procedure TBlockSerial_TestDSR_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.TestDSR;
end;

//Write: property TestDSR: boolean read FTestDSR write FTestDSR;
procedure TBlockSerial_TestDSR_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.TestDSR := Pboolean(Params^[1])^;
end;

//Read: property TestCTS: boolean read FTestCTS write FTestCTS;
procedure TBlockSerial_TestCTS_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PBlockSerial(Params^[0])^.TestCTS;
end;

//Write: property TestCTS: boolean read FTestCTS write FTestCTS;
procedure TBlockSerial_TestCTS_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.TestCTS := Pboolean(Params^[1])^;
end;

//Read: property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;
procedure TBlockSerial_MaxLineLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBlockSerial(Params^[0])^.MaxLineLength;
end;

//Write: property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;
procedure TBlockSerial_MaxLineLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.MaxLineLength := PInteger(Params^[1])^;
end;

//Read: property DeadlockTimeout: Integer read FDeadlockTimeout Write FDeadlockTimeout;
procedure TBlockSerial_DeadlockTimeout_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PBlockSerial(Params^[0])^.DeadlockTimeout;
end;

//Write: property DeadlockTimeout: Integer read FDeadlockTimeout Write FDeadlockTimeout;
procedure TBlockSerial_DeadlockTimeout_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.DeadlockTimeout := PInteger(Params^[1])^;
end;

//Read: property LinuxLock: Boolean read FLinuxLock write FLinuxLock;
procedure TBlockSerial_LinuxLock_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBlockSerial(Params^[0])^.LinuxLock;
end;

//Write: property LinuxLock: Boolean read FLinuxLock write FLinuxLock;
procedure TBlockSerial_LinuxLock_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.LinuxLock := PBoolean(Params^[1])^;
end;

//Read: property ConvertLineEnd: Boolean read FConvertLineEnd Write FConvertLineEnd;
procedure TBlockSerial_ConvertLineEnd_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBlockSerial(Params^[0])^.ConvertLineEnd;
end;

//Write: property ConvertLineEnd: Boolean read FConvertLineEnd Write FConvertLineEnd;
procedure TBlockSerial_ConvertLineEnd_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.ConvertLineEnd := PBoolean(Params^[1])^;
end;

//Read: property AtTimeout: integer read FAtTimeout Write FAtTimeout;
procedure TBlockSerial_AtTimeout_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PBlockSerial(Params^[0])^.AtTimeout;
end;

//Write: property AtTimeout: integer read FAtTimeout Write FAtTimeout;
procedure TBlockSerial_AtTimeout_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.AtTimeout := Pinteger(Params^[1])^;
end;

//Read: property InterPacketTimeout: Boolean read FInterPacketTimeout Write FInterPacketTimeout;
procedure TBlockSerial_InterPacketTimeout_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PBlockSerial(Params^[0])^.InterPacketTimeout;
end;

//Write: property InterPacketTimeout: Boolean read FInterPacketTimeout Write FInterPacketTimeout;
procedure TBlockSerial_InterPacketTimeout_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.InterPacketTimeout := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TBlockSerial_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBlockSerial(Params^[0])^.Free();
end;

procedure Register_TBlockSerial(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TBlockSerial', 'TObject');
    addGlobalFunc('procedure TBlockSerial.Init();', @TBlockSerial_Init);
    addGlobalFunc('function TBlockSerial.GetVersion(): string; constref;', @TBlockSerial_GetVersion);
    addGlobalFunc('procedure TBlockSerial.CloseSocket(); constref;', @TBlockSerial_CloseSocket);
    addGlobalFunc('procedure TBlockSerial.Config(baud, bits: integer; parity: char; stop: integer;softflow, hardflow: boolean); constref;', @TBlockSerial_Config);
    addGlobalFunc('procedure TBlockSerial.Connect(comport: string); constref;', @TBlockSerial_Connect);
    addGlobalFunc('procedure TBlockSerial.SetCommState(); constref;', @TBlockSerial_SetCommState);
    addGlobalFunc('procedure TBlockSerial.GetCommState(); constref;', @TBlockSerial_GetCommState);
    addGlobalFunc('function TBlockSerial.SendBuffer(buffer: pointer; length: integer): integer; constref;', @TBlockSerial_SendBuffer);
    addGlobalFunc('procedure TBlockSerial.SendByte(data: byte); constref;', @TBlockSerial_SendByte);
    addGlobalFunc('procedure TBlockSerial.SendString(data: AnsiString); constref;', @TBlockSerial_SendString);
    addGlobalFunc('procedure TBlockSerial.SendInteger(Data: integer); constref;', @TBlockSerial_SendInteger);
    addGlobalFunc('procedure TBlockSerial.SendBlock(const Data: AnsiString); constref;', @TBlockSerial_SendBlock);
    addGlobalFunc('procedure TBlockSerial.SendStreamRaw(const Stream: TStream); constref;', @TBlockSerial_SendStreamRaw);
    addGlobalFunc('procedure TBlockSerial.SendStream(const Stream: TStream); constref;', @TBlockSerial_SendStream);
    addGlobalFunc('procedure TBlockSerial.SendStreamIndy(const Stream: TStream); constref;', @TBlockSerial_SendStreamIndy);
    addGlobalFunc('function TBlockSerial.RecvBuffer(buffer: pointer; length: integer): integer; constref;', @TBlockSerial_RecvBuffer);
    addGlobalFunc('function TBlockSerial.RecvBufferEx(buffer: pointer; length: integer; timeout: integer): integer; constref;', @TBlockSerial_RecvBufferEx);
    addGlobalFunc('function TBlockSerial.RecvBufferStr(Length: Integer; Timeout: Integer): AnsiString; constref;', @TBlockSerial_RecvBufferStr);
    addGlobalFunc('function TBlockSerial.RecvPacket(Timeout: Integer): AnsiString; constref;', @TBlockSerial_RecvPacket);
    addGlobalFunc('function TBlockSerial.RecvByte(timeout: integer): byte; constref;', @TBlockSerial_RecvByte);
    addGlobalFunc('function TBlockSerial.RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString; constref;', @TBlockSerial_RecvTerminated);
    addGlobalFunc('function TBlockSerial.Recvstring(timeout: integer): AnsiString; constref;', @TBlockSerial_Recvstring);
    addGlobalFunc('function TBlockSerial.RecvInteger(Timeout: Integer): Integer; constref;', @TBlockSerial_RecvInteger);
    addGlobalFunc('function TBlockSerial.RecvBlock(Timeout: Integer): AnsiString; constref;', @TBlockSerial_RecvBlock);
    addGlobalFunc('procedure TBlockSerial.RecvStreamRaw(const Stream: TStream; Timeout: Integer); constref;', @TBlockSerial_RecvStreamRaw);
    addGlobalFunc('procedure TBlockSerial.RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer); constref;', @TBlockSerial_RecvStreamSize);
    addGlobalFunc('procedure TBlockSerial.RecvStream(const Stream: TStream; Timeout: Integer); constref;', @TBlockSerial_RecvStream);
    addGlobalFunc('procedure TBlockSerial.RecvStreamIndy(const Stream: TStream; Timeout: Integer); constref;', @TBlockSerial_RecvStreamIndy);
    addGlobalFunc('function TBlockSerial.WaitingData(): integer; constref;', @TBlockSerial_WaitingData);
    addGlobalFunc('function TBlockSerial.WaitingDataEx(): integer; constref;', @TBlockSerial_WaitingDataEx);
    addGlobalFunc('function TBlockSerial.SendingData(): integer; constref;', @TBlockSerial_SendingData);
    addGlobalFunc('procedure TBlockSerial.EnableRTSToggle(value: boolean); constref;', @TBlockSerial_EnableRTSToggle);
    addGlobalFunc('procedure TBlockSerial.Flush(); constref;', @TBlockSerial_Flush);
    addGlobalFunc('procedure TBlockSerial.Purge(); constref;', @TBlockSerial_Purge);
    addGlobalFunc('function TBlockSerial.CanRead(Timeout: integer): boolean; constref;', @TBlockSerial_CanRead);
    addGlobalFunc('function TBlockSerial.CanWrite(Timeout: integer): boolean; constref;', @TBlockSerial_CanWrite);
    addGlobalFunc('function TBlockSerial.CanReadEx(Timeout: integer): boolean; constref;', @TBlockSerial_CanReadEx);
    addGlobalFunc('function TBlockSerial.ModemStatus(): integer; constref;', @TBlockSerial_ModemStatus);
    addGlobalFunc('procedure TBlockSerial.SetBreak(Duration: integer); constref;', @TBlockSerial_SetBreak);
    addGlobalFunc('function TBlockSerial.ATCommand(value: AnsiString): AnsiString; constref;', @TBlockSerial_ATCommand);
    addGlobalFunc('function TBlockSerial.ATConnect(value: AnsiString): AnsiString; constref;', @TBlockSerial_ATConnect);
    addGlobalFunc('function TBlockSerial.SerialCheck(SerialResult: integer): integer; constref;', @TBlockSerial_SerialCheck);
    addGlobalFunc('procedure TBlockSerial.ExceptCheck(); constref;', @TBlockSerial_ExceptCheck);
    addGlobalFunc('procedure TBlockSerial.SetSynaError(ErrNumber: integer); constref;', @TBlockSerial_SetSynaError);
    addGlobalFunc('procedure TBlockSerial.RaiseSynaError(ErrNumber: integer); constref;', @TBlockSerial_RaiseSynaError);
    addClassVar('TBlockSerial', 'Device', 'string', @TBlockSerial_Device_Read, nil);
    addClassVar('TBlockSerial', 'LastError', 'integer', @TBlockSerial_LastError_Read, nil);
    addClassVar('TBlockSerial', 'LastErrorDesc', 'string', @TBlockSerial_LastErrorDesc_Read, nil);
    addClassVar('TBlockSerial', 'ATResult', 'Boolean', @TBlockSerial_ATResult_Read, nil);
    addClassVar('TBlockSerial', 'RTS', 'Boolean', nil, @TBlockSerial_RTS_Write);
    addClassVar('TBlockSerial', 'CTS', 'boolean', @TBlockSerial_CTS_Read, nil);
    addClassVar('TBlockSerial', 'DTR', 'Boolean', nil, @TBlockSerial_DTR_Write);
    addClassVar('TBlockSerial', 'DSR', 'boolean', @TBlockSerial_DSR_Read, nil);
    addClassVar('TBlockSerial', 'Carrier', 'boolean', @TBlockSerial_Carrier_Read, nil);
    addClassVar('TBlockSerial', 'Ring', 'boolean', @TBlockSerial_Ring_Read, nil);
    addClassVar('TBlockSerial', 'MaxSendBandwidth', 'Integer', @TBlockSerial_MaxSendBandwidth_Read, @TBlockSerial_MaxSendBandwidth_Write);
    addClassVar('TBlockSerial', 'MaxRecvBandwidth', 'Integer', @TBlockSerial_MaxRecvBandwidth_Read, @TBlockSerial_MaxRecvBandwidth_Write);
    addClassVar('TBlockSerial', 'MaxBandwidth', 'Integer', nil, @TBlockSerial_MaxBandwidth_Write);
    addClassVar('TBlockSerial', 'SizeRecvBuffer', 'integer', @TBlockSerial_SizeRecvBuffer_Read, @TBlockSerial_SizeRecvBuffer_Write);
    addGlobalFunc('function TBlockSerial.GetErrorDesc(ErrorCode: integer): string; constref;', @TBlockSerial_GetErrorDesc);
    addClassVar('TBlockSerial', 'Tag', 'integer', @TBlockSerial_Tag_Read, @TBlockSerial_Tag_Write);
    addClassVar('TBlockSerial', 'Handle', 'THandle', @TBlockSerial_Handle_Read, @TBlockSerial_Handle_Write);
    addClassVar('TBlockSerial', 'LineBuffer', 'AnsiString', @TBlockSerial_LineBuffer_Read, @TBlockSerial_LineBuffer_Write);
    addClassVar('TBlockSerial', 'RaiseExcept', 'boolean', @TBlockSerial_RaiseExcept_Read, @TBlockSerial_RaiseExcept_Write);
    addClassVar('TBlockSerial', 'TestDSR', 'boolean', @TBlockSerial_TestDSR_Read, @TBlockSerial_TestDSR_Write);
    addClassVar('TBlockSerial', 'TestCTS', 'boolean', @TBlockSerial_TestCTS_Read, @TBlockSerial_TestCTS_Write);
    addClassVar('TBlockSerial', 'MaxLineLength', 'Integer', @TBlockSerial_MaxLineLength_Read, @TBlockSerial_MaxLineLength_Write);
    addClassVar('TBlockSerial', 'DeadlockTimeout', 'Integer', @TBlockSerial_DeadlockTimeout_Read, @TBlockSerial_DeadlockTimeout_Write);
    addClassVar('TBlockSerial', 'LinuxLock', 'Boolean', @TBlockSerial_LinuxLock_Read, @TBlockSerial_LinuxLock_Write);
    addClassVar('TBlockSerial', 'ConvertLineEnd', 'Boolean', @TBlockSerial_ConvertLineEnd_Read, @TBlockSerial_ConvertLineEnd_Write);
    addClassVar('TBlockSerial', 'AtTimeout', 'integer', @TBlockSerial_AtTimeout_Read, @TBlockSerial_AtTimeout_Write);
    addClassVar('TBlockSerial', 'InterPacketTimeout', 'Boolean', @TBlockSerial_InterPacketTimeout_Read, @TBlockSerial_InterPacketTimeout_Write);
    addGlobalFunc('procedure TBlockSerial.Free();', @TBlockSerial_Free);
  end;
end;

end.

