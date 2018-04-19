uses
  initc;

const
  { Net type }
  socklib = 'c';
  AF_INET = 2;

  { Error constants. Returned by LastError method of THost, TNet}

  NETDB_INTERNAL= -1;       { see errno }
  NETDB_SUCCESS = 0;        { no problem }
  HOST_NOT_FOUND= 1;        { Authoritative Answer Host not found }
  TRY_AGAIN     = 2;        { Non-Authoritive Host not found, or SERVERFAIL }
  NO_RECOVERY   = 3;        { Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  NO_DATA       = 4;        { Valid name, no data record of requested type }
  NO_ADDRESS    = NO_DATA;  { no address, look for MX record }


Type

  { THostEnt Object }
  THostEnt = record
    H_Name     : pchar;   { Official name }
    H_Aliases  : ppchar;  { Null-terminated list of aliases}
    H_Addrtype : longint;   { Host address type }
    H_length  : longint;   { Length of address }
    H_Addr : ppchar;    { null-terminated list of adresses }
  end;
  PHostEntry = ^THostEnt;

  { TNetEnt object }
  TNetEnt = record
    N_Name     : pchar;   { Official name }
    N_Aliases  : ppchar;  { Nill-terminated alias list }
    N_AddrType : longint; { Net address type }
    N_net      : Cardinal; { Network number }
  end;
  PNetEntry = ^TNetEnt;

  TServEnt = record
    s_name    : pchar;    { Service name }
    s_aliases : ppchar;   { Null-terminated alias list }
    s_port    : longint;  { Port number }
    s_proto   : pchar;    { Protocol to use }
  end;
  PServEntry = ^TServEnt;

{ C style calls, linked in from Libc }

function gethostent : PHostEntry; cdecl; external socklib;
procedure sethostent (stayopen : longint); cdecl; external socklib;
procedure endhostent; cdecl; external socklib;

function getnetent : PNetEntry; cdecl; external socklib;
procedure setnetent ( Stayopen : Longint);  cdecl; external socklib;
procedure endnetent; cdecl; external socklib;

function getservent : PServEntry; cdecl; external socklib;
procedure setservent (StayOpen : longint); cdecl; external socklib;
procedure endservent; cdecl; external socklib;

function getnetbyaddr ( Net : Longint; nettype : Longint) : PNetEntry; cdecl; external socklib;
function gethostbyname ( Name : Pchar) : PHostEntry; cdecl; external socklib;
function gethostbyaddr ( Addr : PChar; Len : Longint; HType : Longint) : PHostentry ; cdecl; external socklib;
function getnetbyname ( Name : pchar) : PNetEntry; cdecl; external socklib;
function getservbyname (name : pchar  ; protocol : pchar) : PServEntry; cdecl; external socklib;
function getservbyport (port : longint; protocol : pchar) : PServEntry; cdecl; external socklib;

function  GetDNSError : libcint;

begin
  GetDNSError:=fpgetCerrno;
end;

Function InitResolve : Boolean;

begin
  Result:=True;
end;

Function FinalResolve : Boolean;

begin
  Result:=True;
end;
