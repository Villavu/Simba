{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{******************************************************************************
                     Text File Writeln/ReadLn Support
******************************************************************************}
{******************************************************************************
                               InetSock
******************************************************************************}

Function DoAccept(Sock:longint;Var addr:TInetSockAddr):longint;

Var AddrLen : Longint;

begin
  AddrLEn:=SizeOf(Addr);
  repeat
    DoAccept:=fpaccept(Sock,@Addr,@AddrLen);
  until (DoAccept<>-1) or (SocketError <> EsockEINTR);
end;

Function DoConnect(Sock:longint;const addr: TInetSockAddr): Boolean;

var
  res: longint;
begin
  repeat
    res:=fpconnect(Sock,@Addr,SizeOF(TInetSockAddr));
  until (res<>-1) or (SocketError <> EsockEINTR);
  DoConnect:= res = 0;
end;

{$warnings off}

{$warnings on}

type thostaddr= packed array[1..4] of byte;

function htonl( host : cardinal):cardinal; inline;overload;
begin
{$ifdef FPC_BIG_ENDIAN}
  htonl:=host;
{$else}
  htonl:=SwapEndian(host);
{$endif}
end;

Function NToHl (Net : cardinal) : cardinal; inline;overload;
begin
{$ifdef FPC_BIG_ENDIAN}
  ntohl:=net;
{$else}
  ntohl:=SwapEndian(net);
{$endif}
end;

function htons( host : word):word; inline;

begin
{$ifdef FPC_BIG_ENDIAN}
  htons:=host;
{$else}
  htons:=SwapEndian(host);
{$endif}
end;

Function NToHs (Net : word):word; inline;

begin
{$ifdef FPC_BIG_ENDIAN}
  ntohs:=net;
{$else}
  ntohs:=SwapEndian(net);
{$endif}
end;

Type array4int = array[1..4] of byte;

function NetAddrToStr (Entry : in_addr) : AnsiString;

Var Dummy : Ansistring;
    i,j   : Longint;

begin
  NetAddrToStr:='';
  j:=entry.s_addr;
  For I:=1 to 4 do
   begin
     Str(array4int(j)[i],Dummy);
     NetAddrToStr:=NetAddrToStr+Dummy;
     If I<4 Then
       NetAddrToStr:=NetAddrToStr+'.';
   end;
end;

function HostAddrToStr (Entry : in_addr) : AnsiString;

Var x: in_addr;

begin
  x.s_addr:=htonl(entry.s_addr);
  HostAddrToStr:=NetAddrToStr(x);
end;

function StrToHostAddr(IP : AnsiString) : in_addr ;

Var
    Dummy : AnsiString;
    I,j,k     : Longint;
    Temp : in_addr;

begin
  strtohostaddr.s_addr:=0;              //:=NoAddress;
  For I:=1 to 4 do
    begin
      If I<4 Then
        begin
          J:=Pos('.',IP);
          If J=0 then
            exit;
          Dummy:=Copy(IP,1,J-1);
          Delete (IP,1,J);
        end
       else
         Dummy:=IP;
      Val (Dummy,k,J);
      array4int(temp.s_addr)[i]:=k;
      If J<>0 then Exit;
   end;
   strtohostaddr.s_addr:=ntohl(Temp.s_addr);
end;

function StrToNetAddr(IP : AnsiString) : in_addr;

begin
  StrToNetAddr.s_addr:=htonl(StrToHostAddr(IP).s_addr);
end;

Function HostToNet (Host : in_addr):in_addr;

begin
  HostToNet.s_addr:=htonl(host.s_addr);
end;

Function NetToHost (Net : in_addr) : in_addr;

begin
  NetToHost.s_addr:=ntohl(net.s_addr);
end;

Function HostToNet (Host : Longint) : Longint;

begin
  HostToNet:=htonl(host);
end;

Function NetToHost (Net : Longint) : Longint;

begin
  NetToHost:=ntohl(net);
end;

Function ShortHostToNet (Host : Word) : Word;

begin
  ShortHostToNet:=htons(host);
end;

Function ShortNetToHost (Net : Word) : Word;

begin
  ShortNEtToHost:=ntohs(net);
end;

const digittab : shortstring = ('0123456789ABCDEF');

function lclinttohex (i:integer;digits:longint): ansistring;

begin
  SetLength(lclinttohex,4);
  lclinttohex[4]:=digittab[1+(i and 15)];
  lclinttohex[3]:=digittab[1+((i shr 4) and 15)];
  lclinttohex[2]:=digittab[1+((i shr 8) and 15)];
  lclinttohex[1]:=digittab[1+((i shr 12) and 15)];;
end;

function HostAddrToStr6 (Entry : TIn6_Addr) :ansiString;
var
  i: byte;
  zr1,zr2: set of byte;
  zc1,zc2: byte;
  have_skipped: boolean;
begin
  zr1 := [];
  zr2 := [];
  zc1 := 0;
  zc2 := 0;
  for i := 0 to 7 do begin
    if Entry.u6_addr16[i] = 0 then begin
      include(zr2, i);
      inc(zc2);
    end else begin
      if zc1 < zc2 then begin
        zc1 := zc2;
        zr1 := zr2;
        zc2 := 0; zr2 := [];
      end;
    end;
  end;
  if zc1 < zc2 then begin
    zc1 := zc2;
    zr1 := zr2;
  end;
  SetLength(HostAddrToStr6, 8*5-1);
  SetLength(HostAddrToStr6, 0);
  have_skipped := false;
  for i := 0 to 7 do begin
    if not (i in zr1) then begin
      if have_skipped then begin
        if HostAddrToStr6 = ''
          then HostAddrToStr6 := '::'
          else HostAddrToStr6 := HostAddrToStr6 + ':';
        have_skipped := false;
      end;
      // FIXME: is that shortnettohost really proper there? I wouldn't be too sure...
      HostAddrToStr6 := HostAddrToStr6 +lclIntToHex(ShortNetToHost(Entry.u6_addr16[i]), 1) + ':';
    end else begin
      have_skipped := true;
    end;
  end;
  if have_skipped then
    if HostAddrToStr6 = ''
      then HostAddrToStr6 := '::'
      else HostAddrToStr6 := HostAddrToStr6 + ':';

  if HostAddrToStr6 = '' then HostAddrToStr6 := '::';
  if not (7 in zr1) then
    SetLength(HostAddrToStr6, Length(HostAddrToStr6)-1);
end;


function StrToHostAddr6(IP : String) : TIn6_addr;
Var Part   : String;
    IPv6   : TIn6_addr;
    P,J    : Integer;
    W      : Word;
    Index  : Integer;
    ZeroAt : Integer;
Begin
  FillChar(IPv6,SizeOf(IPv6),0);
  { Every 16-bit block is converted at its own and stored into Result. When }
  { the '::' zero-spacer is found, its location is stored. Afterwards the   }
  { address is shifted and zero-filled.                                     }
  Index := 0; ZeroAt := -1;
  J := 0;
  P := Pos(':',IP);
  While (P > 0) and (Length(IP) > 0) and (Index < 8) do
    Begin
      Part := '$'+Copy(IP,1,P-1);
      Delete(IP,1,P);
      if Length(Part) > 1 then  { is there a digit after the '$'? }
        Val(Part,W,J)
      else W := 0;
      IPv6.u6_addr16[Index] := HtoNS(W);
      if J <> 0 then
        Begin
          FillChar(IPv6,SizeOf(IPv6),0);
          Exit;
        End;
      if IP[1] = ':' then
        Begin
          ZeroAt := Index;
          Delete(IP,1,1);
        End;
      Inc(Index);
      P := Pos(':',IP); if P = 0 then P := Length(IP)+1;
    End;
  { address      a:b:c::f:g:h }
  { Result now   a : b : c : f : g : h : 0 : 0, ZeroAt = 2, Index = 6 }
  { Result after a : b : c : 0 : 0 : f : g : h }
  if ZeroAt >= 0 then
    Begin
      Move(IPv6.u6_addr16[ZeroAt+1],IPv6.u6_addr16[(8-Index)+ZeroAt+1],2*(Index-ZeroAt-1));
      FillChar(IPv6.u6_addr16[ZeroAt+1],2*(8-Index),0);
    End;

  StrToHostAddr6:=IPv6;
End;


function NetAddrToStr6 (Entry : TIn6_Addr) : ansiString;
begin
  netaddrtostr6 := HostAddrToStr6((Entry));
end;

function StrToNetAddr6(IP : ansiString) : TIn6_Addr;
begin
  StrToNetAddr6 := StrToHostAddr6(IP);
end;


