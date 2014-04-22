{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  Files:      dpaddr.h dplobby8.h dplay8.h dvoice.h                         *}
{*  Content:    DirectPlay8 include files                                     *}
{*                                                                            *}
{*  DirectX 9.0 Delphi / FreePascal adaptation by Alexey Barkovoy             *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 22-Aug-2004                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://clootie.ru                                                       *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{$I DirectX.inc}

unit DirectPlay8;

interface


uses
  Windows, WinSock, DirectSound;

(*==========================================================================;
 *
 *  Copyright (C) 2000-2002 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dpaddr.h
 *  Content:   DirectPlayAddress include file
 ***************************************************************************)


const
(****************************************************************************
 *
 * DirectPlay8Address CLSIDs
 *
 ****************************************************************************)

  // {934A9523-A3CA-4bc5-ADA0-D6D95D979421}
  CLSID_DirectPlay8Address: TGUID = '{934a9523-a3ca-4bc5-ada0-d6d95d979421}';

  
(****************************************************************************
 *
 * DirectPlay8Address Interface IIDs
 *
 ****************************************************************************)
type
  DPNAREFIID = TGUID;
  TDPNARefIID = DPNAREFIID;


(****************************************************************************
 *
 * DirectPlay8Address Constants
 *
 ****************************************************************************)
const
  //
  // Asynchronous operation flags
  //
  DPNA_DATATYPE_STRING        = $00000001;
  DPNA_DATATYPE_DWORD         = $00000002;
  DPNA_DATATYPE_GUID          = $00000003;
  DPNA_DATATYPE_BINARY        = $00000004;
  DPNA_DATATYPE_STRING_ANSI   = $00000005;

  DPNA_DPNSVR_PORT            = 6073;

  DPNA_INDEX_INVALID          = $FFFFFFFF;

(****************************************************************************
 *
 * DirectPlay8Address Address Elements
 *
 ****************************************************************************)

  DPNA_SEPARATOR_KEYVALUE       = WideChar('=');
  DPNA_SEPARATOR_USERDATA       = WideChar('#');
  DPNA_SEPARATOR_COMPONENT      = WideChar(';');
  DPNA_ESCAPECHAR               = WideChar('%');

  // Header
  DPNA_HEADER		                    = 'x-directplay:/';

  // key names for address components
  DPNA_KEY_NAT_RESOLVER     = 'natresolver';
  DPNA_KEY_NAT_RESOLVER_USER_STRING = 'natresolveruserstring';
  DPNA_KEY_APPLICATION_INSTANCE = 'applicationinstance';
  DPNA_KEY_DEVICE           = 'device';
  DPNA_KEY_HOSTNAME         = 'hostname';
  DPNA_KEY_PORT             = 'port';
  DPNA_KEY_NAMEINFO         = 'nameinfo';
  DPNA_KEY_PROCESSOR        = 'processor';
  DPNA_KEY_PROGRAM          = 'program';
  DPNA_KEY_PROVIDER         = 'provider';
  DPNA_KEY_SCOPE            = 'scope';
  DPNA_KEY_TRAVERSALMODE    = 'traversalmode';
  DPNA_KEY_BAUD             = 'baud';
  DPNA_KEY_FLOWCONTROL      = 'flowcontrol';
  DPNA_KEY_PARITY           = 'parity';
  DPNA_KEY_PHONENUMBER      = 'phonenumber';
  DPNA_KEY_STOPBITS         = 'stopbits';

  // values for baud rate
  DPNA_BAUD_RATE_9600         = 9600;
  DPNA_BAUD_RATE_14400        = 14400;
  DPNA_BAUD_RATE_19200        = 19200;
  DPNA_BAUD_RATE_38400        = 38400;
  DPNA_BAUD_RATE_56000        = 56000;
  DPNA_BAUD_RATE_57600        = 57600;
  DPNA_BAUD_RATE_115200       = 115200;

  // values for stop bits
  DPNA_STOP_BITS_ONE          = '1';
  DPNA_STOP_BITS_ONE_FIVE     = '1.5';
  DPNA_STOP_BITS_TWO          = '2';

  // values for parity
  DPNA_PARITY_NONE					= 'NONE';
  DPNA_PARITY_EVEN					= 'EVEN';
  DPNA_PARITY_ODD						= 'ODD';
  DPNA_PARITY_MARK					= 'MARK';
  DPNA_PARITY_SPACE					= 'SPACE';

  // values for flow control
  DPNA_FLOW_CONTROL_NONE			= 'NONE';
  DPNA_FLOW_CONTROL_XONXOFF		= 'XONXOFF';
  DPNA_FLOW_CONTROL_RTS				= 'RTS';
  DPNA_FLOW_CONTROL_DTR				= 'DTR';
  DPNA_FLOW_CONTROL_RTSDTR		= 'RTSDTR';
  // values for traversal mode
  DPNA_TRAVERSALMODE_NONE             = 0;
  DPNA_TRAVERSALMODE_PORTREQUIRED     = 1;
  DPNA_TRAVERSALMODE_PORTRECOMMENDED  = 2;
  // Shortcut values
  //
  // These can be used instead of the corresponding CLSID_DP8SP_XXXX guids
  //
  DPNA_VALUE_TCPIPPROVIDER            = 'IP';
  DPNA_VALUE_IPXPROVIDER              = 'IPX';
  DPNA_VALUE_MODEMPROVIDER            = 'MODEM';
  DPNA_VALUE_SERIALPROVIDER           = 'SERIAL';

//// ANSI DEFINITIONS

  // Header
  DPNA_HEADER_A						      = 'x-directplay:/';
  DPNA_SEPARATOR_KEYVALUE_A			= '=';
  DPNA_SEPARATOR_USERDATA_A			= '#';
  DPNA_SEPARATOR_COMPONENT_A		= ';';
  DPNA_ESCAPECHAR_A					    = '%';

  // key names for address components
  DPNA_KEY_NAT_RESOLVER_A					= 'natresolver';
  DPNA_KEY_NAT_RESOLVER_USER_STRING_A		= 'natresolveruserstring';
  DPNA_KEY_APPLICATION_INSTANCE_A		= 'applicationinstance';
  DPNA_KEY_DEVICE_A					= 'device';
  DPNA_KEY_HOSTNAME_A					= 'hostname';
  DPNA_KEY_PORT_A						= 'port';
  DPNA_KEY_NAMEINFO_A					= 'nameinfo';
  DPNA_KEY_PROCESSOR_A				= 'processor';
  DPNA_KEY_PROGRAM_A					= 'program';
  DPNA_KEY_PROVIDER_A					= 'provider';
  DPNA_KEY_SCOPE_A					= 'scope';
  DPNA_KEY_TRAVERSALMODE_A			= 'traversalmode';
  DPNA_KEY_BAUD_A						= 'baud';
  DPNA_KEY_FLOWCONTROL_A				= 'flowcontrol';
  DPNA_KEY_PARITY_A					= 'parity';
  DPNA_KEY_PHONENUMBER_A				= 'phonenumber';
  DPNA_KEY_STOPBITS_A					= 'stopbits';

  // values for stop bits
  DPNA_STOP_BITS_ONE_A				= '1';
  DPNA_STOP_BITS_ONE_FIVE_A			= '1.5';
  DPNA_STOP_BITS_TWO_A				= '2';

  // values for parity
  DPNA_PARITY_NONE_A					= 'NONE';
  DPNA_PARITY_EVEN_A					= 'EVEN';
  DPNA_PARITY_ODD_A	          = 'ODD';
  DPNA_PARITY_MARK_A					= 'MARK';
  DPNA_PARITY_SPACE_A					= 'SPACE';

  // values for flow control
  DPNA_FLOW_CONTROL_NONE_A			= 'NONE';
  DPNA_FLOW_CONTROL_XONXOFF_A   = 'XONXOFF';
  DPNA_FLOW_CONTROL_RTS_A				= 'RTS';
  DPNA_FLOW_CONTROL_DTR_A				= 'DTR';
  DPNA_FLOW_CONTROL_RTSDTR_A		= 'RTSDTR';
  // Shortcut values
  //
  // These can be used instead of the corresponding CLSID_DP8SP_XXXX guids
  //
  DPNA_VALUE_TCPIPPROVIDER_A          = 'IP';
  DPNA_VALUE_IPXPROVIDER_A            = 'IPX';
  DPNA_VALUE_MODEMPROVIDER_A          = 'MODEM';
  DPNA_VALUE_SERIALPROVIDER_A         = 'SERIAL';

type
(****************************************************************************
 *
 * DirectPlay8Address Forward Declarations For External Types
 *
 ****************************************************************************)

  SOCKADDR = TSockAddr;

(****************************************************************************
 *
 * DirectPlay8Address Functions
 *
 ****************************************************************************)


(*
 *
 * This function is no longer supported.  It is recommended that CoCreateInstance be used to create
 * DirectPlay8 address objects.
 *
 * HRESULT WINAPI DirectPlay8AddressCreate( const GUID * pcIID, void **ppvInterface, IUnknown *pUnknown);
 *
 *)

(****************************************************************************
 *
 * DirectPlay8Address Application Interfaces
 *
 ****************************************************************************)

  //
  // COM definition for IDirectPlay8Address Generic Interface
  //
  IDirectPlay8Address = interface(IUnknown)
    ['{83783300-4063-4c8a-9db3-82830a7feb31}']
    (*** IDirectPlay8Address methods ***)
    function BuildFromURLW(pwszSourceURL: PWideChar): HResult; stdcall;
    function BuildFromURLA(pszSourceURL: PAnsiChar): HResult; stdcall;
    function Duplicate(out ppdpaNewAddress: IDirectPlay8Address): HResult; stdcall;
    function SetEqual(pdpaAddress: IDirectPlay8Address): HResult; stdcall;
    function IsEqual(pdpaAddress: IDirectPlay8Address): HResult; stdcall;
    function Clear: HResult; stdcall;
    function GetURLW(pwszURL: PWideChar; var pdwNumChars: DWORD): HResult; stdcall;
    function GetURLA(pszURL: PAnsiChar; var pdwNumChars: DWORD): HResult; stdcall;
    function GetSP(out pguidSP: TGUID): HResult; stdcall;
    function GetUserData(pvUserData: Pointer; var pdwBufferSize: DWORD): HResult; stdcall;
    function SetSP(const pguidSP: TGUID): HResult; stdcall;
    function SetUserData(pvUserData: Pointer; dwDataSize: DWORD): HResult; stdcall;
    function GetNumComponents(out pdwNumComponents: DWORD): HResult; stdcall;
    function GetComponentByName(pwszName: PWideChar; pvBuffer: Pointer; var pdwBufferSize: DWORD; out pdwDataType: DWORD): HResult; stdcall;
    function GetComponentByIndex(dwComponentID: DWORD; pwszName: PWideChar; var pdwNameLen: DWORD; pvBuffer: Pointer; var pdwBufferSize: DWORD; out pdwDataType: DWORD): HResult; stdcall;
    function AddComponent(pwszName: PWideChar; lpvData: Pointer; dwDataSize, dwDataType: DWORD): HResult; stdcall;
    function GetDevice(out pguidDevice: TGUID): HResult; stdcall;
    function SetDevice(const pguidDevice: TGUID): HResult; stdcall;
    function BuildFromDPADDRESS(pvAddress: Pointer; dwDataSize: DWORD): HResult; stdcall;
  end;
  PIDirectPlay8Address = ^IDirectPlay8Address;

  //
  // COM definition for IDirectPlay8AddressIP Generic Interface
  //
  IDirectPlay8AddressIP = interface(IUnknown)
    ['{e5a0e990-2bad-430b-87da-a142cf75de58}']
    (*** IDirectPlay8AddressIP methods ***)
    // Constructs a IDirectPlay8 TCP Address from a SOCKADDR structure
    function BuildFromSockAddr(const pSockAddr: TSockAddr): HResult; stdcall;

    // Constructs a TCP Address from a string (hostname) and port
    function BuildAddress(wszAddress: PWideChar; usPort: Word): HResult; stdcall;

    // Builds a local TCP Address
    function BuildLocalAddress(const pguidAdapter: TGUID; usPort: Word): HResult; stdcall;

    // Gets the address from the structure in SOCKADR format
    function GetSockAddress(psockAddress: PSockAddr; var pdwAddressBufferSize: DWORD): HResult; stdcall;

    // Gets the local afddress
    function GetLocalAddress(out pguidAdapter: TGUID; out pusPort: Word): HResult; stdcall;

    // Gets the remote address
    function GetAddress(wszAddress: PWideChar; var pdwAddressLength: DWORD; out psPort: Word): HResult; stdcall;
  end;


(****************************************************************************
 *
 * DirectPlay8Address Interface IIDs
 *
 ****************************************************************************)

type
  // {83783300-4063-4c8a-9DB3-82830A7FEB31}
  IID_IDirectPlay8Address = IDirectPlay8Address;
  // {E5A0E990-2BAD-430b-87DA-A142CF75DE58}
  IID_IDirectPlay8AddressIP = IDirectPlay8AddressIP; 



(*==========================================================================;
 *
 *  Copyright (C) 1998-2002 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:   DPlay8.h
 *  Content: DirectPlay8 include file
 *
 ***************************************************************************)

const
(****************************************************************************
 *
 * DirectPlay8 CLSIDs
 *
 ****************************************************************************)

  // {743F1DC6-5ABA-429f-8BDF-C54D03253DC2}
  CLSID_DirectPlay8Client: TGUID = '{743f1dc6-5aba-429f-8bdf-c54d03253dc2}';

  // {DA825E1B-6830-43d7-835D-0B5AD82956A2}
  CLSID_DirectPlay8Server: TGUID = '{da825e1b-6830-43d7-835d-0b5ad82956a2}';

  // {286F484D-375E-4458-A272-B138E2F80A6A}
  CLSID_DirectPlay8Peer: TGUID = '{286f484d-375e-4458-a272-b138e2f80a6a}';

  // CLSIDs added for DirectX 9

  // {FC47060E-6153-4b34-B975-8E4121EB7F3C}
  CLSID_DirectPlay8ThreadPool: TGUID = '{fc47060e-6153-4b34-b975-8e4121eb7f3c}';

  // {E4C1D9A2-CBF7-48bd-9A69-34A55E0D8941}
  CLSID_DirectPlay8NATResolver: TGUID = '{e4c1d9a2-cbf7-48bd-9a69-34a55e0d8941}';

(****************************************************************************
 *
 * DirectPlay8 Service Provider GUIDs
 *
 ****************************************************************************)

  // {53934290-628D-11D2-AE0F-006097B01411}
  CLSID_DP8SP_IPX: TGUID = '{53934290-628d-11d2-ae0f-006097b01411}';

  // {6D4A3650-628D-11D2-AE0F-006097B01411}
  CLSID_DP8SP_MODEM: TGUID = '{6d4a3650-628d-11d2-ae0f-006097b01411}';

  // {743B5D60-628D-11D2-AE0F-006097B01411}
  CLSID_DP8SP_SERIAL: TGUID = '{743b5d60-628d-11d2-ae0f-006097b01411}';

  // {EBFE7BA0-628D-11D2-AE0F-006097B01411}
  CLSID_DP8SP_TCPIP: TGUID = '{ebfe7ba0-628d-11d2-ae0f-006097b01411}';


  // Service providers added for DirectX 9


  // {995513AF-3027-4b9a-956E-C772B3F78006}
  CLSID_DP8SP_BLUETOOTH: TGUID = '{995513af-3027-4b9a-956e-c772b3f78006}';


type
(****************************************************************************
 *
 * DirectPlay8 Callback Functions
 *
 ****************************************************************************)

  //
  // Callback Function Type Definition
  //
  TFNDPNMessageHandler = function (pvUserContext: Pointer; dwMessageType: DWORD; pMessage: Pointer): HRESULT; stdcall;

(****************************************************************************
 *
 * DirectPlay8 Datatypes (Non-Structure / Non-Message)
 *
 ****************************************************************************)

  //
  // Player IDs.  Used to uniquely identify a player in a session
  //
  PDPNID = ^DPNID;
  DPNID = DWORD;
  TDPNID = DPNID;

  //
  // Used as identifiers for operations
  //
  PDPNHandle = ^TDPNHandle;
  DPNHANDLE = DWORD;
  TDPNHandle = DPNHANDLE;

(****************************************************************************
 *
 * DirectPlay8 Message Identifiers
 *
 ****************************************************************************)

const
  DPN_MSGID_OFFSET                    = $FFFF0000;
  DPN_MSGID_ADD_PLAYER_TO_GROUP       = (DPN_MSGID_OFFSET or $0001);
  DPN_MSGID_APPLICATION_DESC          = (DPN_MSGID_OFFSET or $0002);
  DPN_MSGID_ASYNC_OP_COMPLETE         = (DPN_MSGID_OFFSET or $0003);
  DPN_MSGID_CLIENT_INFO               = (DPN_MSGID_OFFSET or $0004);
  DPN_MSGID_CONNECT_COMPLETE          = (DPN_MSGID_OFFSET or $0005);
  DPN_MSGID_CREATE_GROUP              = (DPN_MSGID_OFFSET or $0006);
  DPN_MSGID_CREATE_PLAYER             = (DPN_MSGID_OFFSET or $0007);
  DPN_MSGID_DESTROY_GROUP             = (DPN_MSGID_OFFSET or $0008);
  DPN_MSGID_DESTROY_PLAYER            = (DPN_MSGID_OFFSET or $0009);
  DPN_MSGID_ENUM_HOSTS_QUERY          = (DPN_MSGID_OFFSET or $000a);
  DPN_MSGID_ENUM_HOSTS_RESPONSE       = (DPN_MSGID_OFFSET or $000b);
  DPN_MSGID_GROUP_INFO                = (DPN_MSGID_OFFSET or $000c);
  DPN_MSGID_HOST_MIGRATE              = (DPN_MSGID_OFFSET or $000d);
  DPN_MSGID_INDICATE_CONNECT          = (DPN_MSGID_OFFSET or $000e);
  DPN_MSGID_INDICATED_CONNECT_ABORTED = (DPN_MSGID_OFFSET or $000f);
  DPN_MSGID_PEER_INFO                 = (DPN_MSGID_OFFSET or $0010);
  DPN_MSGID_RECEIVE                   = (DPN_MSGID_OFFSET or $0011);
  DPN_MSGID_REMOVE_PLAYER_FROM_GROUP  = (DPN_MSGID_OFFSET or $0012);
  DPN_MSGID_RETURN_BUFFER             = (DPN_MSGID_OFFSET or $0013);
  DPN_MSGID_SEND_COMPLETE             = (DPN_MSGID_OFFSET or $0014);
  DPN_MSGID_SERVER_INFO               = (DPN_MSGID_OFFSET or $0015);
  DPN_MSGID_TERMINATE_SESSION         = (DPN_MSGID_OFFSET or $0016);

  // Messages added for DirectX 9
  DPN_MSGID_CREATE_THREAD             = (DPN_MSGID_OFFSET or $0017);
  DPN_MSGID_DESTROY_THREAD            = (DPN_MSGID_OFFSET or $0018);
  DPN_MSGID_NAT_RESOLVER_QUERY        = (DPN_MSGID_OFFSET or $0101);

(****************************************************************************
 *
 * DirectPlay8 Constants
 *
 ****************************************************************************)

  DPNID_ALL_PLAYERS_GROUP                 = 0;

  //
  // DESTROY_GROUP reasons
  //
  DPNDESTROYGROUPREASON_NORMAL            = $0001;
  DPNDESTROYGROUPREASON_AUTODESTRUCTED    = $0002;
  DPNDESTROYGROUPREASON_SESSIONTERMINATED = $0003;

  //
  // DESTROY_PLAYER reasons
  //
  DPNDESTROYPLAYERREASON_NORMAL              = $0001;
  DPNDESTROYPLAYERREASON_CONNECTIONLOST      = $0002;
  DPNDESTROYPLAYERREASON_SESSIONTERMINATED   = $0003;
  DPNDESTROYPLAYERREASON_HOSTDESTROYEDPLAYER = $0004;


  DPN_MAX_APPDESC_RESERVEDDATA_SIZE          = 64;


(****************************************************************************
 *
 * DirectPlay8 Flags
 *
 ****************************************************************************)

  //
  // Asynchronous operation flags (For Async Ops)
  //
  DPNOP_SYNC                   = $80000000;

  //
  // Add player to group flags (For AddPlayerToGroup)
  //
  DPNADDPLAYERTOGROUP_SYNC     = DPNOP_SYNC;

  //
  // Cancel flags
  //
  DPNCANCEL_CONNECT           = $0001;
  DPNCANCEL_ENUM              = $0002;
  DPNCANCEL_SEND              = $0004;
  DPNCANCEL_ALL_OPERATIONS    = $8000;

  // Flags added for DirectX 9
  DPNCANCEL_PLAYER_SENDS					= $80000000;
  DPNCANCEL_PLAYER_SENDS_PRIORITY_HIGH	= (DPNCANCEL_PLAYER_SENDS or $00010000);
  DPNCANCEL_PLAYER_SENDS_PRIORITY_NORMAL	= (DPNCANCEL_PLAYER_SENDS or $00020000);
  DPNCANCEL_PLAYER_SENDS_PRIORITY_LOW		= (DPNCANCEL_PLAYER_SENDS or $00040000);

  //
  // Close flags (for Close, added for DirectX 9)
  //
  DPNCLOSE_IMMEDIATE						= $00000001;

  //
  // Connect flags (For Connect)
  //
  DPNCONNECT_SYNC              = DPNOP_SYNC;
  DPNCONNECT_OKTOQUERYFORADDRESSING = $0001;

  //
  // Create group flags (For CreateGroup)
  //
  DPNCREATEGROUP_SYNC          = DPNOP_SYNC;

  //
  // Destroy group flags (For DestroyGroup)
  //
  DPNDESTROYGROUP_SYNC         = DPNOP_SYNC;

  //
  // Enumerate clients and groups flags (For EnumPlayersAndGroups)
  //
  DPNENUM_PLAYERS              = $0001;
  DPNENUM_GROUPS               = $0010;

  //
  // Enum hosts flags (For EnumHosts)
  //
  DPNENUMHOSTS_SYNC                   = DPNOP_SYNC;
  DPNENUMHOSTS_OKTOQUERYFORADDRESSING = $0001;
  DPNENUMHOSTS_NOBROADCASTFALLBACK    = $0002;

  //
  // Enum service provider flags (For EnumSP)
  //
  DPNENUMSERVICEPROVIDERS_ALL         = $0001;

  //
  // GetLocalHostAddresses flags (added for DirectX 9)
  //
  DPNGETLOCALHOSTADDRESSES_COMBINED   = $0001;

  //
  // Get send queue info flags (For GetSendQueueInfo)
  //
  DPNGETSENDQUEUEINFO_PRIORITY_NORMAL = $0001;
  DPNGETSENDQUEUEINFO_PRIORITY_HIGH   = $0002;
  DPNGETSENDQUEUEINFO_PRIORITY_LOW    = $0004;

  //
  // Group information flags (For Group Info)
  //
  DPNGROUP_AUTODESTRUCT               = $0001;

  //
  // Host flags (For Host)
  //
  DPNHOST_OKTOQUERYFORADDRESSING      = $0001;

  //
  // Set info
  //
  DPNINFO_NAME                        = $0001;
  DPNINFO_DATA                        = $0002;

  //
  // Initialize flags (For Initialize)
  //
  DPNINITIALIZE_DISABLEPARAMVAL       = $0001;
  // Flags added for DirectX 9
  DPNINITIALIZE_HINT_LANSESSION       = $0002;
  DPNINITIALIZE_DISABLELINKTUNING     = $0004;


  //
  // Register Lobby flags
  //
  DPNLOBBY_REGISTER                   = $0001;
  DPNLOBBY_UNREGISTER                 = $0002;

  //
  // Player information flags (For Player Info / Player Messages)
  //
  DPNPLAYER_LOCAL       = $0002;
  DPNPLAYER_HOST        = $0004;

  //
  // Receive indication flags (added for DirectX 9)
  //
  DPNRECEIVE_GUARANTEED       = $0001;
  DPNRECEIVE_COALESCED        = $0002;

  //
  // Remove player from group flags (For RemovePlayerFromGroup)
  //
  DPNREMOVEPLAYERFROMGROUP_SYN = DPNOP_SYNC;

  //
  // Send flags (For Send/SendTo)
  //
  DPNSEND_SYNC                = DPNOP_SYNC;
  DPNSEND_NOCOPY              = $0001;
  DPNSEND_NOCOMPLETE          = $0002;
  DPNSEND_COMPLETEONPROCESS   = $0004;
  DPNSEND_GUARANTEED          = $0008;
  DPNSEND_NONSEQUENTIAL       = $0010;
  DPNSEND_NOLOOPBACK          = $0020;
  DPNSEND_PRIORITY_LOW        = $0040;
  DPNSEND_PRIORITY_HIGH       = $0080;
  // Flag added for DirectX 9
  DPNSEND_COALESCE						= $0100;

  //
  // Send complete indication flags (added for DirectX 9)
  //
  DPNSENDCOMPLETE_GUARANTEED				= $0001;
  DPNSENDCOMPLETE_COALESCED				= $0002;

  //
  // Session Flags (for DPN_APPLICATION_DESC)
  //
  DPNSESSION_CLIENT_SERVER    = $0001;
  DPNSESSION_MIGRATE_HOST     = $0004;
  DPNSESSION_NODPNSVR         = $0040;
  DPNSESSION_REQUIREPASSWORD  = $0080;
  // Flag added for DirectX 9
  DPNSESSION_NOENUMS						= $0100;
  DPNSESSION_FAST_SIGNED					= $0200;
  DPNSESSION_FULL_SIGNED					= $0400;

  //
  // Set client info flags (For SetClientInfo)
  //
  DPNSETCLIENTINFO_SYNC       = DPNOP_SYNC;

  //
  // Set group info flags (For SetGroupInfo)
  //
  DPNSETGROUPINFO_SYNC        = DPNOP_SYNC;

  //
  // Set peer info flags (For SetPeerInfo)
  //
  DPNSETPEERINFO_SYNC         = DPNOP_SYNC;

  //
  // Set server info flags (For SetServerInfo)
  //
  DPNSETSERVERINFO_SYNC       = DPNOP_SYNC;

  //
  // SP capabilities flags
  //
  DPNSPCAPS_SUPPORTSDPNSVR      = $0001;
  DPNSPCAPS_SUPPORTSDPNSRV      = DPNSPCAPS_SUPPORTSDPNSVR;
  DPNSPCAPS_SUPPORTSBROADCAST   = $0002;
  DPNSPCAPS_SUPPORTSALLADAPTERS = $0004;
  // Flags added for DirectX 9
  DPNSPCAPS_SUPPORTSTHREADPOOL			= $0008;
  DPNSPCAPS_NETWORKSIMULATOR				= $0010;

  //
  // SP information flags (added for DirectX 9)
  //
  DPNSPINFO_NETWORKSIMULATORDEVICE		= $0001;

(****************************************************************************
 *
 * DirectPlay8 Structures (Non-Message)
 *
 ****************************************************************************)

type
  //
  // Application description
  //

  PDPNApplicationDesc = ^TDPNApplicationDesc;
  _DPN_APPLICATION_DESC = packed record
    dwSize: DWORD;                    // Size of this structure
    dwFlags: DWORD;                   // Flags (DPNSESSION_...)
    guidInstance: TGUID;              // Instance GUID
    guidApplication: TGUID;           // Application GUID
    dwMaxPlayers: DWORD;              // Maximum # of players allowed (0=no limit)
    dwCurrentPlayers: DWORD;          // Current # of players allowed
    pwszSessionName: PWideChar;       // Name of the session
    pwszPassword: PWideChar;          // Password for the session
    pvReservedData: Pointer;
    dwReservedDataSize: DWORD;
    pvApplicationReservedData: Pointer;
    dwApplicationReservedDataSize: DWORD;
  end;
  DPN_APPLICATION_DESC = _DPN_APPLICATION_DESC;
  TDPNApplicationDesc = _DPN_APPLICATION_DESC;

  //
  // Generic Buffer Description
  //
  PBufferDesc = ^TBufferDesc;
  _BUFFERDESC = packed record
    wBufferSize:DWORD;
    pBufferData: PByte;
  end;
  BUFFERDESC = _BUFFERDESC;
  TBufferDesc = _BUFFERDESC;

  PDPNBufferDesc = ^TDPNBufferDesc;
  DPN_BUFFER_DESC = _BUFFERDESC;
  TDPNBufferDesc = DPN_BUFFER_DESC;

  //
  // DirectPlay8 capabilities
  //
  PDPNCaps = ^TDPNCaps;
  _DPN_CAPS = packed record
    dwSize: DWORD;							// Size of this structure
    dwFlags: DWORD;						// Flags
    dwConnectTimeout: DWORD;				// ms before a connect request times out
    dwConnectRetries: DWORD;				// # of times to attempt the connection
    dwTimeoutUntilKeepAlive: DWORD;		// ms of inactivity before a keep alive is sent
  end;
  DPN_CAPS = _DPN_CAPS;
  TDPNCaps = _DPN_CAPS;

  //
  // Extended capabilities structures (added for DirectX 9)
  //
  PDPNCapsEx = ^TDPNCapsEx;
  _DPN_CAPS_EX = packed record
    dwSize: DWORD;						// Size of this structure
    dwFlags: DWORD;						// Flags
    dwConnectTimeout: DWORD;			// ms before a connect request times out
    dwConnectRetries: DWORD;				// # of times to attempt the connection
    dwTimeoutUntilKeepAlive: DWORD;		// ms of inactivity before a keep alive is sent
    dwMaxRecvMsgSize: DWORD;			// maximum size in bytes of message that can be received
    dwNumSendRetries: DWORD;			// maximum number of send retries before link is considered dead
    dwMaxSendRetryInterval: DWORD;		// maximum period in msec between send retries
    dwDropThresholdRate: DWORD;			// percentage of dropped packets before throttling
    dwThrottleRate: DWORD;				// percentage amount to reduce send window when throttling
    dwNumHardDisconnectSends: DWORD;	// number of hard disconnect frames to send when close immediate flag is specified
    dwMaxHardDisconnectPeriod: DWORD;	// maximum period between hard disconnect sends
  end;
  DPN_CAPS_EX = _DPN_CAPS_EX;
  TDPNCapsEx = _DPN_CAPS_EX;

  // Connection Statistics information

  PDPNConnectionInfo = ^TDPNConnectionInfo;
  _DPN_CONNECTION_INFO = packed record
    dwSize: DWORD;
    dwRoundTripLatencyMS: DWORD;
    dwThroughputBPS: DWORD;
    dwPeakThroughputBPS: DWORD;

    dwBytesSentGuaranteed: DWORD;
    dwPacketsSentGuaranteed: DWORD;
    dwBytesSentNonGuaranteed: DWORD;
    dwPacketsSentNonGuaranteed: DWORD;

    dwBytesRetried: DWORD;    // Guaranteed only
    dwPacketsRetried: DWORD;  // Guaranteed only
    dwBytesDropped: DWORD;    // Non Guaranteed only
    dwPacketsDropped: DWORD;  // Non Guaranteed only

    dwMessagesTransmittedHighPriority: DWORD;
    dwMessagesTimedOutHighPriority: DWORD;
    dwMessagesTransmittedNormalPriority: DWORD;
    dwMessagesTimedOutNormalPriority: DWORD;
    dwMessagesTransmittedLowPriority: DWORD;
    dwMessagesTimedOutLowPriority: DWORD;

    dwBytesReceivedGuaranteed: DWORD;
    dwPacketsReceivedGuaranteed: DWORD;
    dwBytesReceivedNonGuaranteed: DWORD;
    dwPacketsReceivedNonGuaranteed: DWORD;
    dwMessagesReceived: DWORD;
  end;
  DPN_CONNECTION_INFO = _DPN_CONNECTION_INFO;
  TDPNConnectionInfo = _DPN_CONNECTION_INFO;


  //
  // Group information strucutre
  //
  PDPNGroupInfo = ^TDPNGroupInfo;
  _DPN_GROUP_INFO = packed record
    dwSize: DWORD;          // size of this structure
    dwInfoFlags: DWORD;     // information contained
    pwszName: PWideChar;    // Unicode Name
    pvData: Pointer;        // data block
    dwDataSize: DWORD;      // size in BYTES of data block
    dwGroupFlags: DWORD;    // group flags (DPNGROUP_...)
  end;
  DPN_GROUP_INFO = _DPN_GROUP_INFO;
  TDPNGroupInfo = _DPN_GROUP_INFO;

  //
  // Player information structure
  //
  PDPNPlayerInfo = ^TDPNPlayerInfo;
  _DPN_PLAYER_INFO = packed record
    dwSize: DWORD;          // size of this structure
    dwInfoFlags: DWORD;     // information contained
    pwszName: PWideChar;    // Unicode Name
    pvData: Pointer;        // data block
    dwDataSize: DWORD;      // size in BYTES of data block
    dwPlayerFlags: DWORD;   // player flags (DPNPLAYER_...)
  end;
  DPN_PLAYER_INFO = _DPN_PLAYER_INFO;
  TDPNPlayerInfo = _DPN_PLAYER_INFO;

  PDPNSecurityCredentials = ^TDPNSecurityCredentials;
  _DPN_SECURITY_CREDENTIALS = record
  end;
  DPN_SECURITY_CREDENTIALS = _DPN_SECURITY_CREDENTIALS;
  TDPNSecurityCredentials = _DPN_SECURITY_CREDENTIALS;

  PDPNSecurityDesc = ^TDPNSecurityDesc;
  _DPN_SECURITY_DESC = record
  end;
  DPN_SECURITY_DESC = _DPN_SECURITY_DESC;
  TDPNSecurityDesc = _DPN_SECURITY_DESC;

  //
  // Service provider & adapter enumeration structure
  //
  PDPNServiceProviderInfo = ^TDPNServiceProviderInfo;
  _DPN_SERVICE_PROVIDER_INFO = packed record
     dwFlags: DWORD;
     guid: TGUID;            // SP Guid
     pwszName: PWideChar;    // Friendly Name
     pvReserved: Pointer;
     dwReserved: DWORD;
  end;
  DPN_SERVICE_PROVIDER_INFO = _DPN_SERVICE_PROVIDER_INFO;
  TDPNServiceProviderInfo = _DPN_SERVICE_PROVIDER_INFO;

  //
  // Service provider caps structure
  //
  PDPNSpCaps = ^TDPNSpCaps;
  _DPN_SP_CAPS = packed record
    dwSize: DWORD;                   // Size of this structure
    dwFlags: DWORD;                  // Flags ((DPNSPCAPS_...)
    dwNumThreads: DWORD;             // # of worker threads to use
    dwDefaultEnumCount: DWORD;       // default # of enum requests
    dwDefaultEnumRetryInterval: DWORD; // default ms between enum requests
    dwDefaultEnumTimeout: DWORD;     // default enum timeout
    dwMaxEnumPayloadSize: DWORD;     // maximum size in bytes for enum payload data
    dwBuffersPerThread: DWORD;       // number of receive buffers per thread
    dwSystemBufferSize: DWORD;       // amount of buffering to do in addition to posted receive buffers
  end;
  DPN_SP_CAPS = _DPN_SP_CAPS;
  TDPNSpCaps = _DPN_SP_CAPS;


(****************************************************************************
 *
 * IDirectPlay8 message handler call back structures
 *
 ****************************************************************************)

  //
  // Add player to group strucutre for message handler
  // (DPN_MSGID_ADD_PLAYER_TO_GROUP)
  //
  PDPNMsgAddPlayerToGroup = ^TDPNMsgAddPlayerToGroup;
  _DPNMSG_ADD_PLAYER_TO_GROUP = packed record
    dwSize: DWORD;            // Size of this structure
    dpnidGroup: TDPNID;       // DPNID of group
    pvGroupContext: Pointer;  // Group context value
    dpnidPlayer: TDPNID;      // DPNID of added player
    pvPlayerContext: Pointer; // Player context value
  end;
  DPNMSG_ADD_PLAYER_TO_GROUP = _DPNMSG_ADD_PLAYER_TO_GROUP;
  TDPNMsgAddPlayerToGroup = _DPNMSG_ADD_PLAYER_TO_GROUP;

  //
  // Async operation completion structure for message handler
  // (DPN_MSGID_ASYNC_OP_COMPLETE)
  //
  PDPNMsgAsyncOpComplete = ^TDPNMsgAsyncOpComplete;
  _DPNMSG_ASYNC_OP_COMPLETE = packed record
    dwSize: DWORD;          // Size of this structure
    hAsyncOp: TDPNHandle;   // DirectPlay8 async operation handle
    pvUserContext: Pointer; // User context supplied
    hResultCode: HRESULT;   // HRESULT of operation
  end;
  DPNMSG_ASYNC_OP_COMPLETE = _DPNMSG_ASYNC_OP_COMPLETE;
  TDPNMsgAsyncOpComplete = _DPNMSG_ASYNC_OP_COMPLETE;

  //
  // Client info structure for message handler
  // (DPN_MSGID_CLIENT_INFO)
  //
  PDPNMsgClientInfo = ^TDPNMsgClientInfo;
  _DPNMSG_CLIENT_INFO = packed record
    dwSize: DWORD;            // Size of this structure
    dpnidClient: TDPNID;      // DPNID of client
    pvPlayerContext: Pointer; // Player context value
  end;
  DPNMSG_CLIENT_INFO = _DPNMSG_CLIENT_INFO;
  TDPNMsgClientInfo = _DPNMSG_CLIENT_INFO;

  //
  // Connect complete structure for message handler
  // (DPN_MSGID_CONNECT_COMPLETE)
  //
  PDPNMsgConnectComplete = ^TDPNMsgConnectComplete;
  _DPNMSG_CONNECT_COMPLETE = packed record
    dwSize: DWORD;                     // Size of this structure
    hAsyncOp: TDPNHandle;              // DirectPlay8 Async operation handle
    pvUserContext: Pointer;            // User context supplied at Connect
    hResultCode: HRESULT;              // HRESULT of connection attempt
    pvApplicationReplyData: Pointer;   // Connection reply data from Host/Server
    dwApplicationReplyDataSize: DWORD; // Size (in bytes) of pvApplicationReplyData

    // Fields added for DirectX 9
    dpnidLocal: TDPNID;                // DPNID of local player
  end;
  DPNMSG_CONNECT_COMPLETE = _DPNMSG_CONNECT_COMPLETE;
  TDPNMsgConnectComplete = _DPNMSG_CONNECT_COMPLETE;

  //
  // Create group structure for message handler
  // (DPN_MSGID_CREATE_GROUP)
  //
  PDPNMsgCreateGroup = ^TDPNMsgCreateGroup;
  _DPNMSG_CREATE_GROUP = packed record
    dwSize: DWORD;           // Size of this structure
    dpnidGroup: TDPNID;      // DPNID of new group
    dpnidOwner: TDPNID;      // Owner of newgroup
    pvGroupContext: Pointer; // Group context value

    // Fields added for DirectX 9
    pvOwnerContext: Pointer; // Owner context value
  end;
  DPNMSG_CREATE_GROUP = _DPNMSG_CREATE_GROUP;
  TDPNMsgCreateGroup = _DPNMSG_CREATE_GROUP;

  //
  // Create player structure for message handler
  // (DPN_MSGID_CREATE_PLAYER)
  //
  PDPNMsgCreatePlayer = ^TDPNMsgCreatePlayer;
  _DPNMSG_CREATE_PLAYER = packed record
    dwSize: DWORD;            // Size of this structure
    dpnidPlayer: DPNID;       // DPNID of new player
    pvPlayerContext: Pointer; // Player context value
  end;
  DPNMSG_CREATE_PLAYER = _DPNMSG_CREATE_PLAYER;
  TDPNMsgCreatePlayer = _DPNMSG_CREATE_PLAYER;

  //
  // Destroy group structure for message handler
  // (DPN_MSGID_DESTROY_GROUP)
  //
  PDPNMsgDestroyGroup = ^TDPNMsgDestroyGroup;
  _DPNMSG_DESTROY_GROUP = packed record
    dwSize: DWORD;           // Size of this structure
    dpnidGroup: TDPNID;      // DPNID of destroyed group
    pvGroupContext: Pointer; // Group context value
    dwReason: DWORD;         // Information only
  end;
  DPNMSG_DESTROY_GROUP = _DPNMSG_DESTROY_GROUP;
  TDPNMsgDestroyGroup = _DPNMSG_DESTROY_GROUP;

  //
  // Destroy player structure for message handler
  // (DPN_MSGID_DESTROY_PLAYER)
  //
  PDPNMsgDestroyPlayer = ^TDPNMsgDestroyPlayer;
  _DPNMSG_DESTROY_PLAYER = packed record
    dwSize: DWORD;            // Size of this structure
    dpnidPlayer: TDPNID;      // DPNID of leaving player
    pvPlayerContext: Pointer; // Player context value
    dwReason: DWORD;          // Information only
  end;
  DPNMSG_DESTROY_PLAYER = _DPNMSG_DESTROY_PLAYER;
  TDPNMsgDestroyPlayer = _DPNMSG_DESTROY_PLAYER;

  //
  // Enumeration request received structure for message handler
  // (DPN_MSGID_ENUM_HOSTS_QUERY)
  //
  PDPNMsgEnumHostsQuery = ^TDPNMsgEnumHostsQuery;
  _DPNMSG_ENUM_HOSTS_QUERY = packed record
    dwSize: DWORD;                        // Size of this structure.
    pAddressSender: IDirectPlay8Address;  // Address of client who sent the request
    pAddressDevice: IDirectPlay8Address;  // Address of device request was received on
    pvReceivedData: Pointer;              // Request data (set on client)
    dwReceivedDataSize: DWORD;            // Request data size (set on client)
    dwMaxResponseDataSize: DWORD;         // Max allowable size of enum response
    pvResponseData: Pointer;              // Optional query repsonse (user set)
    dwResponseDataSize: DWORD;            // Optional query response size (user set)
    pvResponseContext: Pointer;           // Optional query response context (user set)
  end;
  DPNMSG_ENUM_HOSTS_QUERY = _DPNMSG_ENUM_HOSTS_QUERY;
  TDPNMsgEnumHostsQuery = _DPNMSG_ENUM_HOSTS_QUERY;

  //
  // Enumeration response received structure for message handler
  // (DPN_MSGID_ENUM_HOSTS_RESPONSE)
  //
  PDPNMsgEnumHostsResponse = ^TDPNMsgEnumHostsResponse;
  _DPNMSG_ENUM_HOSTS_RESPONSE = packed record
    dwSize: DWORD;                                // Size of this structure
    pAddressSender: IDirectPlay8Address;          // Address of host who responded
    pAddressDevice: IDirectPlay8Address;          // Device response was received on
    pApplicationDescription: PDPNApplicationDesc; // Application description for the session
    pvResponseData: Pointer;                      // Optional response data (set on host)
    dwResponseDataSize: DWORD;                    // Optional response data size (set on host)
    pvUserContext: Pointer;                       // Context value supplied for enumeration
    dwRoundTripLatencyMS: DWORD;                  // Round trip latency in MS
  end;
  DPNMSG_ENUM_HOSTS_RESPONSE = _DPNMSG_ENUM_HOSTS_RESPONSE;
  TDPNMsgEnumHostsResponse = _DPNMSG_ENUM_HOSTS_RESPONSE;

  //
  // Group info structure for message handler
  // (DPN_MSGID_GROUP_INFO)
  //
  PDPNMsgGroupInfo = ^TDPNMsgGroupInfo;
  _DPNMSG_GROUP_INFO = packed record
    dwSize: DWORD;              // Size of this structure
    dpnidGroup: TDPNID;         // DPNID of group
    pvGroupContext: Pointer;    // Group context value
  end;
  DPNMSG_GROUP_INFO = _DPNMSG_GROUP_INFO;
  TDPNMsgGroupInfo = _DPNMSG_GROUP_INFO;

  //
  // Migrate host structure for message handler
  // (DPN_MSGID_HOST_MIGRATE)
  //
  PDPNMsgHostMigrate = ^TDPNMsgHostMigrate;
  _DPNMSG_HOST_MIGRATE = packed record
    dwSize: DWORD;             // Size of this structure
    dpnidNewHost: TDPNID;      // DPNID of new Host player
    pvPlayerContext: Pointer;  // Player context value
  end;
  DPNMSG_HOST_MIGRATE = _DPNMSG_HOST_MIGRATE;
  TDPNMsgHostMigrate = _DPNMSG_HOST_MIGRATE;

  //
  // Indicate connect structure for message handler
  // (DPN_MSGID_INDICATE_CONNECT)
  //
  PDPNMsgIndicateConnect = ^TDPNMsgIndicateConnect;
  _DPNMSG_INDICATE_CONNECT = packed record
    dwSize: DWORD;                        // Size of this structure
    pvUserConnectData: Pointer;           // Connecting player data
    dwUserConnectDataSize: DWORD;         // Size (in bytes) of pvUserConnectData
    pvReplyData: Pointer;                 // Connection reply data
    dwReplyDataSize: DWORD;               // Size (in bytes) of pvReplyData
    pvReplyContext: Pointer;              // Buffer context for pvReplyData
    pvPlayerContext: Pointer;             // Player context preset
    pAddressPlayer: IDirectPlay8Address;  // Address of connecting player
    pAddressDevice: IDirectPlay8Address;  // Address of device receiving connect attempt
  end;
  DPNMSG_INDICATE_CONNECT = _DPNMSG_INDICATE_CONNECT;
  TDPNMsgIndicateConnect = _DPNMSG_INDICATE_CONNECT;

  //
  // Indicated connect aborted structure for message handler
  // (DPN_MSGID_INDICATED_CONNECT_ABORTED)
  //
  PDPNMsgIndicatedConnectAborted = ^TDPNMsgIndicatedConnectAborted;
  _DPNMSG_INDICATED_CONNECT_ABORTED = packed record
    dwSize: DWORD;            // Size of this structure
    pvPlayerContext: Pointer; // Player context preset from DPNMSG_INDICATE_CONNECT
  end;
  DPNMSG_INDICATED_CONNECT_ABORTED = _DPNMSG_INDICATED_CONNECT_ABORTED;
  TDPNMsgIndicatedConnectAborted = _DPNMSG_INDICATED_CONNECT_ABORTED;

  //
  // Peer info structure for message handler
  // (DPN_MSGID_PEER_INFO)
  //
  PDPNMsgPeerInfo = ^TDPNMsgPeerInfo;
  _DPNMSG_PEER_INFO = packed record
    dwSize: DWORD;              // Size of this structure
    dpnidPeer: TDPNID;          // DPNID of peer
    pvPlayerContext: Pointer;   // Player context value
  end;
  DPNMSG_PEER_INFO = _DPNMSG_PEER_INFO;
  TDPNMsgPeerInfo = _DPNMSG_PEER_INFO;

  //
  // Receive structure for message handler
  // (DPN_MSGID_RECEIVE)
  //
  PDPNMsgReceive = ^TDPNMsgReceive;
  _DPNMSG_RECEIVE = packed record
    dwSize: DWORD;             // Size of this structure
    dpnidSender: TDPNID;       // DPNID of sending player
    pvPlayerContext: Pointer;  // Player context value of sending player
    pReceiveData: PByte;       // Received data
    dwReceiveDataSize: DWORD;  // Size (in bytes) of pReceiveData
    hBufferHandle: TDPNHandle; // Buffer handle for pReceiveData

    // Fields added for DirectX 9
    dwReceiveFlags: DWORD;     // Flags describing how message was received
  end;
  DPNMSG_RECEIVE = _DPNMSG_RECEIVE;
  TDPNMsgReceive = _DPNMSG_RECEIVE;

  //
  // Remove player from group structure for message handler
  // (DPN_MSGID_REMOVE_PLAYER_FROM_GROUP)
  //
  PDPNMsgRemovePlayerFromGroup = ^TDPNMsgRemovePlayerFromGroup;
  _DPNMSG_REMOVE_PLAYER_FROM_GROUP = packed record
    dwSize: DWORD;               // Size of this structure
    dpnidGroup: TDPNID;          // DPNID of group
    pvGroupContext: Pointer;     // Group context value
    dpnidPlayer: TDPNID;         // DPNID of deleted player
    pvPlayerContext: Pointer;    // Player context value
  end;
  DPNMSG_REMOVE_PLAYER_FROM_GROUP = _DPNMSG_REMOVE_PLAYER_FROM_GROUP;
  TDPNMsgRemovePlayerFromGroup = _DPNMSG_REMOVE_PLAYER_FROM_GROUP;

  //
  // Returned buffer structure for message handler
  // (DPN_MSGID_RETURN_BUFFER)
  //
  PDPNMsgReturnBuffer = ^TDPNMsgReturnBuffer;
  _DPNMSG_RETURN_BUFFER = packed record
    dwSize: DWORD;          // Size of this structure
    hResultCode: HRESULT;   // Return value of operation
    pvBuffer: Pointer;      // Buffer being returned
    pvUserContext: Pointer; // Context associated with buffer
  end;
  DPNMSG_RETURN_BUFFER = _DPNMSG_RETURN_BUFFER;
  TDPNMsgReturnBuffer = _DPNMSG_RETURN_BUFFER;

  //
  // Send complete structure for message handler
  // (DPN_MSGID_SEND_COMPLETE)
  //
  PDPNMsgSendComplete = ^TDPNMsgSendComplete;
  _DPNMSG_SEND_COMPLETE = packed record
    dwSize: DWORD;          // Size of this structure
    hAsyncOp: TDPNHandle;   // DirectPlay8 Async operation handle
    pvUserContext: Pointer; // User context supplied at Send/SendTo
    hResultCode: HRESULT;   // HRESULT of send
    dwSendTime: DWORD;      // Send time in ms

    // Fields added for DirectX 9
    dwFirstFrameRTT: DWORD;		// RTT of the first frame in the message
    dwFirstFrameRetryCount: DWORD;	// Retry count of the first frame
    dwSendCompleteFlags: DWORD;	// Flags describing how message was sent
    pBuffers: PDPNBufferDesc;				// Pointer to array of buffers sent, if DirectPlay did not make a copy
    dwNumBuffers: DWORD;			// Number of buffers in previous array
  end;
  DPNMSG_SEND_COMPLETE = _DPNMSG_SEND_COMPLETE;
  TDPNMsgSendComplete = _DPNMSG_SEND_COMPLETE;

  //
  // Server info structure for message handler
  // (DPN_MSGID_SERVER_INFO)
  //
  PDPNMsgServerInfo = ^TDPNMsgServerInfo;
  _DPNMSG_SERVER_INFO = packed record
    dwSize: DWORD;              // Size of this structure
    dpnidServer: TDPNID;        // DPNID of server
    pvPlayerContext: Pointer;   // Player context value
  end;
  DPNMSG_SERVER_INFO = _DPNMSG_SERVER_INFO;
  TDPNMsgServerInfo = _DPNMSG_SERVER_INFO;

  //
  // Terminated session structure for message handler
  // (DPN_MSGID_TERMINATE_SESSION)
  //
  PDPNMsgTerminateSession = ^TDPNMsgTerminateSession;
  _DPNMSG_TERMINATE_SESSION = packed record
    dwSize: DWORD;              // Size of this structure
    hResultCode: HRESULT;       // Reason
    pvTerminateData: Pointer;   // Data passed from Host/Server
    dwTerminateDataSize: DWORD; // Size (in bytes) of pvTerminateData
  end;
  DPNMSG_TERMINATE_SESSION = _DPNMSG_TERMINATE_SESSION;
  TDPNMsgTerminateSession = _DPNMSG_TERMINATE_SESSION;

  //
  // Message structures added for DirectX 9
  //

  //
  // Create thread info structure for message handler
  // (DPN_MSGID_CREATE_THREAD)
  //
  PDPNMsgCreateThread = ^TDPNMsgCreateThread;
  _DPNMSG_CREATE_THREAD = packed record
    dwSize: DWORD;				// Size of this structure
    dwFlags: DWORD;			// Flags describing this thread
    dwProcessorNum: DWORD;		// Index of processor to which thread is bound
    pvUserContext: Pointer;		// Thread context value
  end;
  DPNMSG_CREATE_THREAD = _DPNMSG_CREATE_THREAD;
  TDPNMsgCreateThread = _DPNMSG_CREATE_THREAD;

  //
  // Destroy thread info structure for message handler
  // (DPN_MSGID_DESTROY_THREAD)
  //
  PDPNMsgDestroyThread = ^TDPNMsgDestroyThread;
  _DPNMSG_DESTROY_THREAD = packed record
    dwSize:	DWORD;				// Size of this structure
    dwProcessorNum:	DWORD;		// Index of processor to which thread was bound
    pvUserContext:	Pointer;		// Thread context value
  end;
  DPNMSG_DESTROY_THREAD = _DPNMSG_DESTROY_THREAD;
  TDPNMsgDestroyThread = _DPNMSG_DESTROY_THREAD;


  //
  // Query-to-resolve-NAT-address structure for message handler
  // (DPN_MSGID_NAT_RESOLVER_QUERY)
  //
  PDPNMsgNatResolverQuery = ^TDPNMsgNatResolverQuery;
  _DPNMSG_NAT_RESOLVER_QUERY = packed record
    dwSize: DWORD;				// Size of this structure.
    pAddressSender: IDirectPlay8Address;	// Address of client that sent the query
    pAddressDevice: IDirectPlay8Address;	// Address of device on which query was received
    pwszUserString: PWideChar;	// User specified string, or NULL if none
  end;
  DPNMSG_NAT_RESOLVER_QUERY = _DPNMSG_NAT_RESOLVER_QUERY;
  TDPNMsgNatResolverQuery = _DPNMSG_NAT_RESOLVER_QUERY;


(****************************************************************************
 *
 * DirectPlay8 Functions
 *
 ****************************************************************************)

(*
 * This function is no longer supported.  It is recommended that CoCreateInstance be used to create
 * DirectPlay8 objects.
 *
 * extern HRESULT WINAPI DirectPlay8Create( const GUID * pcIID, void **ppvInterface, IUnknown *pUnknown);
 *
 *)
 

(****************************************************************************
 *
 * DirectPlay8Lobby Interface Pointer
 *
 ****************************************************************************)

  IDirectPlay8LobbiedApplication = interface;

(****************************************************************************
 *
 * DirectPlay8 Application Interfaces
 *
 ****************************************************************************)

  //
  // COM definition for DirectPlay8 Client interface
  //
  IDirectPlay8Client = interface(IUnknown)
    ['{5102dacd-241b-11d3-aea7-006097b01411}']
    (*** IDirectPlay8Client methods ***)
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; dwFlags: DWORD): HResult; stdcall;
    function EnumServiceProviders(pguidServiceProvider, pguidApplication: PGUID;
      pSPInfoBuffer: PDPNServiceProviderInfo; out pcbEnumData, pcReturned: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function EnumHosts(const pApplicationDesc: TDPNApplicationDesc;
      pAddrHost, pDeviceInfo: IDirectPlay8Address; pUserEnumData: Pointer;
      dwUserEnumDataSize, dwEnumCount, dwRetryInterval, dwTimeOut: DWORD;
      pvUserContext: Pointer; pAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function CancelAsyncOperation(hAsyncHandle: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function Connect(const pdnAppDesc: TDPNApplicationDesc;
      pHostAddr: IDirectPlay8Address; pDeviceInfo: IDirectPlay8Address;
      pdnSecurity: PDPNSecurityDesc; pdnCredentials: PDPNSecurityCredentials;
      pvUserConnectData: Pointer; dwUserConnectDataSize: DWORD; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function Send(const prgBufferDesc: TDPNBufferDesc; cBufferDesc, dwTimeOut: DWORD;
      pvAsyncContext: Pointer; phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetSendQueueInfo(pdwNumMsgs, pdwNumBytes: PDWORD; dwFlags: DWORD): HResult; stdcall;
    function GetApplicationDesc(pAppDescBuffer: PDPNApplicationDesc; var pcbDataSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetClientInfo(const pdpnPlayerInfo: TDPNPlayerInfo; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetServerInfo(pdpnPlayerInfo: PDPNPlayerInfo; var pdwSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function GetServerAddress(out pAddress: IDirectPlay8Address; dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function ReturnBuffer(hBufferHandle: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetCaps(pdpCaps: PDPNCaps; dwFlags: DWORD): HResult; stdcall; //Translator: pdpCaps can be either PDPNCaps or PDPNCapsEx
    function SetCaps(pdpCaps: PDPNCaps; dwFlags: DWORD): HResult; stdcall; //Translator: pdpCaps can be either PDPNCaps or PDPNCapsEx
    function SetSPCaps(const pguidSP: TGUID; const pdpspCaps: TDPNSpCaps; dwFlags: DWORD): HResult; stdcall;
    function GetSPCaps(const pguidSP: TGUID; var pdpspCaps: TDPNSpCaps; dwFlags: DWORD): HResult; stdcall;
    function GetConnectionInfo(var pdpConnectionInfo: TDPNConnectionInfo; dwFlags: DWORD): HResult; stdcall;
    function RegisterLobby(dpnHandle: TDPNHandle; pIDP8LobbiedApplication: IDirectPlay8LobbiedApplication; dwFlags: DWORD): HResult; stdcall;
  end;

  //
  // COM definition for DirectPlay8 Server interface
  //
  IDirectPlay8Server = interface(IUnknown)
    ['{5102dace-241b-11d3-aea7-006097b01411}']
    (*** IDirectPlay8Server methods ***)
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; dwFlags: DWORD): HResult; stdcall;
    function EnumServiceProviders(pguidServiceProvider, pguidApplication: PGUID;
      pSPInfoBuffer: PDPNServiceProviderInfo; out pcbEnumData, pcReturned: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function CancelAsyncOperation(hAsyncHandle: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetSendQueueInfo(dpnid: TDPNID; pdwNumMsgs, pdwNumBytes: PDWORD; dwFlags: DWORD): HResult; stdcall;
    function GetApplicationDesc(pAppDescBuffer: PDPNApplicationDesc; var pcbDataSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetServerInfo(pdpnPlayerInfo: PDPNPlayerInfo; pvAsyncContext: Pointer; phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetClientInfo(dpnid: TDPNID; pdpnPlayerInfo: PDPNPlayerInfo; pdwSize: PDWORD; dwFlags: DWORD): HResult; stdcall;
    function GetClientAddress(dpnid: TDPNID; out pAddress: IDirectPlay8Address; dwFlags: DWORD): HResult; stdcall;
    function GetLocalHostAddresses(out prgpAddress: IDirectPlay8Address; var pcAddress: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetApplicationDesc(const pad: TDPNApplicationDesc; dwFlags: DWORD): HResult; stdcall;
    function Host(const pdnAppDesc: TDPNApplicationDesc; prgpDeviceInfo: PIDirectPlay8Address;
      cDeviceInfo: DWORD; pdnSecurity: PDPNSecurityDesc; pdnCredentials: PDPNSecurityCredentials;
      pvPlayerContext: Pointer; dwFlags: DWORD): HResult; stdcall;
    function SendTo(dpnid: TDPNID; const prgBufferDesc: TDPNBufferDesc; cBufferDesc, dwTimeOut: DWORD;
      pvAsyncContext: Pointer; phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function CreateGroup(const pdpnGroupInfo: TDPNGroupInfo; pvGroupContext, pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function DestroyGroup(idGroup: TDPNID; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function AddPlayerToGroup(idGroup, idClient: TDPNID; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function RemovePlayerFromGroup(idGroup, idClient: TDPNID; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function SetGroupInfo(dpnid: TDPNID; const pdpnGroupInfo: TDPNGroupInfo; pvAsyncContext: Pointer; phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetGroupInfo(dpnid: TDPNID; pdpnGroupInfo: PDPNGroupInfo; pdwSize: PDWORD; dwFlags: DWORD): HResult; stdcall; //todo: Changed
    function EnumPlayersAndGroups(prgdpnid: PDPNID; var pcdpnid: DWORD; dwFlags: DWORD): HResult; stdcall;
    function EnumGroupMembers(dpnid: TDPNID; prgdpnid: PDPNID; var pcdpnid: DWORD; dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function DestroyClient(dpnidClient: TDPNID; pvDestroyData: Pointer; dwDestroyDataSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function ReturnBuffer(hBufferHandle: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetPlayerContext(dpnid: TDPNID; out ppvPlayerContext: Pointer; dwFlags: DWORD): HResult; stdcall;
    function GetGroupContext(dpnid: TDPNID; out ppvGroupContext: Pointer; dwFlags: DWORD): HResult; stdcall;
    function GetCaps(pdpCaps: PDPNCaps; dwFlags: DWORD): HResult; stdcall; //Translator: pdpCaps can be either PDPNCaps or PDPNCapsEx
    function SetCaps(pdpCaps: PDPNCaps; dwFlags: DWORD): HResult; stdcall; //Translator: pdpCaps can be either PDPNCaps or PDPNCapsEx
    function SetSPCaps(const pguidSP: TGUID; const pdpspCaps: TDPNSpCaps; dwFlags: DWORD): HResult; stdcall;
    function GetSPCaps(const pguidSP: TGUID; var pdpspCaps: TDPNSpCaps; dwFlags: DWORD): HResult; stdcall;
    function GetConnectionInfo(dpnid: TDPNID; var pdpConnectionInfo: TDPNConnectionInfo; dwFlags: DWORD): HResult; stdcall;
    function RegisterLobby(dpnHandle: TDPNHandle; pIDP8LobbiedApplication: IDirectPlay8LobbiedApplication; dwFlags: DWORD): HResult; stdcall;
  end;

  //
  // COM definition for DirectPlay8 Peer interface
  //
  IDirectPlay8Peer = interface(IUnknown)
    ['{5102dacf-241b-11d3-aea7-006097b01411}']
    (*** IDirectPlay8Peer methods ***)
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; dwFlags: DWORD): HResult; stdcall;
    function EnumServiceProviders(pguidServiceProvider, pguidApplication: PGUID;
      pSPInfoBuffer: PDPNServiceProviderInfo; out pcbEnumData, pcReturned: DWORD;
      dwFlags: DWORD): HResult; stdcall;
    function CancelAsyncOperation(hAsyncHandle: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function Connect(const pdnAppDesc: TDPNApplicationDesc;
      pHostAddr: IDirectPlay8Address; pDeviceInfo: IDirectPlay8Address;
      pdnSecurity: PDPNSecurityDesc; pdnCredentials: PDPNSecurityCredentials;
      pvUserConnectData: Pointer; dwUserConnectDataSize: DWORD;
      pvPlayerContext, pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function SendTo(dpnid: TDPNID; const prgBufferDesc: TDPNBufferDesc; cBufferDesc, dwTimeOut: DWORD;
      pvAsyncContext: Pointer; phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetSendQueueInfo(dpnid: TDPNID; pdwNumMsgs, pdwNumBytes: PDWORD; dwFlags: DWORD): HResult; stdcall;
    function Host(const pdnAppDesc: TDPNApplicationDesc; prgpDeviceInfo: PIDirectPlay8Address;
      cDeviceInfo: DWORD; pdnSecurity: PDPNSecurityDesc; pdnCredentials: PDPNSecurityCredentials;
      pvPlayerContext: Pointer; dwFlags: DWORD): HResult; stdcall;
    function GetApplicationDesc(pAppDescBuffer: PDPNApplicationDesc; var pcbDataSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetApplicationDesc(const pad: TDPNApplicationDesc; dwFlags: DWORD): HResult; stdcall;
    function CreateGroup(const pdpnGroupInfo: TDPNGroupInfo; pvGroupContext, pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function DestroyGroup(idGroup: TDPNID; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function AddPlayerToGroup(idGroup, idClient: TDPNID; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function RemovePlayerFromGroup(idGroup, idClient: TDPNID; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function SetGroupInfo(dpnid: TDPNID; const pdpnGroupInfo: TDPNGroupInfo; pvAsyncContext: Pointer; phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetGroupInfo(dpnid: TDPNID; pdpnGroupInfo: PDPNGroupInfo; pdwSize: PDWORD; dwFlags: DWORD): HResult; stdcall; //todo: Changed
    function EnumPlayersAndGroups(prgdpnid: PDPNID; var pcdpnid: DWORD; dwFlags: DWORD): HResult; stdcall;
    function EnumGroupMembers(dpnid: TDPNID; prgdpnid: PDPNID; var pcdpnid: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetPeerInfo(const pdpnPlayerInfo: TDPNPlayerInfo; pvAsyncContext: Pointer;
      phAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetPeerInfo(dpnid: TDPNID; var pdpnPlayerInfo: TDPNPlayerInfo; pdwSize: PDWORD; dwFlags: DWORD): HResult; stdcall; //todo: Changed
    function GetPeerAddress(dpnid: TDPNID; out pAddress: IDirectPlay8Address; dwFlags: DWORD): HResult; stdcall;
    function GetLocalHostAddresses(prgpAddress: PIDirectPlay8Address; var pcAddress: DWORD; dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function EnumHosts(const pApplicationDesc: TDPNApplicationDesc;
      pAddrHost, pDeviceInfo: IDirectPlay8Address; pUserEnumData: Pointer;
      dwUserEnumDataSize, dwEnumCount, dwRetryInterval, dwTimeOut: DWORD;
      pvUserContext: Pointer; pAsyncHandle: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function DestroyPeer(dpnidClient: TDPNID; pvDestroyData: Pointer; dwDestroyDataSize, dwFlags: DWORD): HResult; stdcall;
    function ReturnBuffer(hBufferHandle: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function GetPlayerContext(dpnid: TDPNID; out ppvPlayerContext: Pointer; dwFlags: DWORD): HResult; stdcall;
    function GetGroupContext(dpnid: TDPNID; out ppvGroupContext: Pointer; dwFlags: DWORD): HResult; stdcall;
    function GetCaps(pdpCaps: PDPNCaps; dwFlags: DWORD): HResult; stdcall; //Translator: pdpCaps can be either PDPNCaps or PDPNCapsEx
    function SetCaps(pdpCaps: PDPNCaps; dwFlags: DWORD): HResult; stdcall; //Translator: pdpCaps can be either PDPNCaps or PDPNCapsEx
    function SetSPCaps(const pguidSP: TGUID; const pdpspCaps: TDPNSpCaps; dwFlags: DWORD): HResult; stdcall;
    function GetSPCaps(const pguidSP: TGUID; var pdpspCaps: TDPNSpCaps; dwFlags: DWORD): HResult; stdcall;
    function GetConnectionInfo(dpnid: TDPNID; var pdpConnectionInfo: TDPNConnectionInfo; dwFlags: DWORD): HResult; stdcall;
    function RegisterLobby(dpnHandle: TDPNHandle; pIDP8LobbiedApplication: IDirectPlay8LobbiedApplication; dwFlags: DWORD): HResult; stdcall; 
    function TerminateSession(pvTerminateData: Pointer; dwTerminateDataSize, dwFlags: DWORD): HResult; stdcall;
  end;


  //
  // COM definition for DirectPlay8 Thread Pool interface
  //
  IDirectPlay8ThreadPool = interface(IUnknown)
    ['{0d22ee73-4a46-4a0d-89b2-045b4d666425}']
    (*** IDirectPlay8ThreadPool methods ***)
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function GetThreadCount(dwProcessorNum: DWORD; out pdwNumThreads: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetThreadCount(dwProcessorNum: DWORD; dwNumThreads: DWORD; dwFlags: DWORD): HResult; stdcall;
    function DoWork(dwAllowedTimeSlice: DWORD; dwFlags: DWORD): HResult; stdcall;
  end;


  //
  // COM definition for DirectPlay8 NAT Resolver interface
  //
  IDirectPlay8NATResolver = interface(IUnknown)
    ['{a9e213f2-9a60-486f-bf3b-53408b6d1cbb}']
    (*** IDirectPlay8NATResolver methods ***)
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; dwFlags: DWORD): HResult; stdcall;
    function Start(ppDevices: PIDirectPlay8Address; dwNumDevices: DWORD; dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function EnumDevices(pSPInfoBuffer: PDPNServiceProviderInfo; var pdwBufferSize: DWORD; out pdwNumDevices: DWORD; dwFlags: DWORD): HResult; stdcall;
    function GetAddresses(ppAddresses: PIDirectPlay8Address; out pdwNumAddresses: DWORD; dwFlags: DWORD): HResult; stdcall;
  end;


(****************************************************************************
 *
 * DirectPlay8 Interface IIDs
 *
 ****************************************************************************)

  // {5102DACD-241B-11d3-AEA7-006097B01411}
  IID_IDirectPlay8Client = IDirectPlay8Client;

  // {5102DACE-241B-11d3-AEA7-006097B01411}
  IID_IDirectPlay8Server = IDirectPlay8Server;

  // {5102DACF-241B-11d3-AEA7-006097B01411}
  IID_IDirectPlay8Peer = IDirectPlay8Peer;


  // IIDs added for DirectX 9

  // {0D22EE73-4A46-4a0d-89B2-045B4D666425}
  IID_IDirectPlay8ThreadPool = IDirectPlay8ThreadPool;

  // {A9E213F2-9A60-486f-BF3B-53408B6D1CBB}
  IID_IDirectPlay8NATResolver = IDirectPlay8NATResolver;



(*==========================================================================
 *
 *  Copyright (C) 2000 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       DPLobby.h
 *  Content:    DirectPlay8 Lobby Include File
 *
 ***************************************************************************)


(****************************************************************************
 *
 * DirectPlay8Lobby Structures (Non-Message)
 *
 ****************************************************************************)

  //
  // Information on a registered game
  //
  PDPLApplicationInfo = ^TDPLApplicationInfo;
  _DPL_APPLICATION_INFO = packed record
    guidApplication: TGUID;            // GUID of the application
    pwszApplicationName: PWideChar;    // Name of the application
    dwNumRunning: DWORD;               // # of instances of this application running
    dwNumWaiting: DWORD;               // # of instances of this application waiting
    dwFlags: DWORD;                    // Flags
  end;
  DPL_APPLICATION_INFO = _DPL_APPLICATION_INFO;
  TDPLApplicationInfo = _DPL_APPLICATION_INFO;

  //
  // Settings to be used for connecting / hosting a game session
  //
  PDPLConnectionSettings = ^TDPLConnectionSettings;
  _DPL_CONNECTION_SETTINGS = packed record
    dwSize: DWORD;                    // Size of this structure
    dwFlags: DWORD;                // Connection settings flags (DPLCONNECTSETTINGS_...)
    dpnAppDesc: TDPNApplicationDesc;             // Application desc for the associated DirectPlay session
    pdp8HostAddress: IDirectPlay8Address;       // Address of host to connect to
    ppdp8DeviceAddresses: IDirectPlay8Address; // Address of device to connect from / host on
    cNumDeviceAddresses: DWORD;    // # of addresses specified in ppdp8DeviceAddresses
    pwszPlayerName: PWideChar;         // Name to give the player
  end;
  DPL_CONNECTION_SETTINGS = _DPL_CONNECTION_SETTINGS;
  TDPLConnectionSettings = _DPL_CONNECTION_SETTINGS;

  //
  // Information for performing a lobby connect
  // (ConnectApplication)
  //
  PDPLConnectInfo = ^TDPLConnectInfo;
  _DPL_CONNECT_INFO = packed record
    dwSize: DWORD;                                  // Size of this structure
    dwFlags: DWORD;                                 // Flags (DPLCONNECT_...)
    guidApplication: TGUID;                         // GUID of application to launch
    pdplConnectionSettings: PDPLConnectionSettings; // Settings application should use
    pvLobbyConnectData: Pointer;                    // User defined data block
    dwLobbyConnectDataSize: DWORD;                  // Size of user defined data block
  end;
  DPL_CONNECT_INFO = _DPL_CONNECT_INFO;
  TDPLConnectInfo = _DPL_CONNECT_INFO;

  //
  // Information for registering an application
  // (RegisterApplication)
  //
  PDPLProgramDesc = ^TDPLProgramDesc;
  _DPL_PROGRAM_DESC = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    guidApplication: TGUID;               // Application GUID
    pwszApplicationName: PWideChar;       // Unicode application name
    pwszCommandLine: PWideChar;           // Unicode command line arguments
    pwszCurrentDirectory: PWideChar;      // Unicode current directory
    pwszDescription: PWideChar;           // Unicode application description
    pwszExecutableFilename: PWideChar;    // Unicode filename of application executable
    pwszExecutablePath: PWideChar;        // Unicode path of application executable
    pwszLauncherFilename: PWideChar;      // Unicode filename of launcher executable
    pwszLauncherPath: PWideChar;          // Unicode path of launcher executable
  end;
  DPL_PROGRAM_DESC = _DPL_PROGRAM_DESC;
  TDPLProgramDesc = _DPL_PROGRAM_DESC;

(****************************************************************************
 *
 * DirectPlay8 Lobby Message Structures
 *
 ****************************************************************************)

  //
  // A connection was established
  // (DPL_MSGID_CONNECT)
  //
  PDPLMessageConnect = ^TDPLMessageConnect;
  _DPL_MESSAGE_CONNECT = packed record
    dwSize: DWORD;                 // Size of this structure
    hConnectId: TDPNHandle;        // Handle of new connection
    pdplConnectionSettings: PDPLConnectionSettings; // Connection settings for this connection
    pvLobbyConnectData: Pointer;   // User defined lobby data block
    dwLobbyConnectDataSize: DWORD; // Size of user defined lobby data block
    pvConnectionContext: Pointer;  // Context value for this connection (user set)
  end;
  DPL_MESSAGE_CONNECT = _DPL_MESSAGE_CONNECT;
  TDPLMessageConnect = _DPL_MESSAGE_CONNECT;

  //
  // Connection settings have been updated
  // (DPL_MSGID_CONNECTION_SETTINGS)
  //
  PDPLMessageConnectionSettings = ^TDPLMessageConnectionSettings;
  _DPL_MESSAGE_CONNECTION_SETTINGS = packed record
    dwSize: DWORD;                // Size of this structure
    hSender: TDPNHandle;          // Handle of the connection for these settings
    pdplConnectionSettings: PDPLConnectionSettings;     // Connection settings
    pvConnectionContext: Pointer; // Context value for this connection
  end;
  DPL_MESSAGE_CONNECTION_SETTINGS = _DPL_MESSAGE_CONNECTION_SETTINGS;
  TDPLMessageConnectionSettings = _DPL_MESSAGE_CONNECTION_SETTINGS;

  //
  // A connection has been disconnected
  // (DPL_MSGID_DISCONNECT)
  //
  PDPLMessageDisconnect = ^TDPLMessageDisconnect;
  _DPL_MESSAGE_DISCONNECT = packed record
    dwSize: DWORD;                // Size of this structure
    hDisconnectId: TDPNHandle;    // Handle of the connection that was terminated
    hrReason: HRESULT;            // Reason the connection was broken
    pvConnectionContext: Pointer; // Context value for this connection
  end;
  DPL_MESSAGE_DISCONNECT = _DPL_MESSAGE_DISCONNECT;
  TDPLMessageDisconnect = _DPL_MESSAGE_DISCONNECT;

  //
  // Data was received through a connection
  // (DPL_MSGID_RECEIVE)
  //
  PDPLMessageReceive = ^TDPLMessageReceive;
  _DPL_MESSAGE_RECEIVE = packed record
    dwSize: DWORD;                  // Size of this structure
    hSender: TDPNHandle;            // Handle of the connection that is from
    pBuffer: PByte;                 // Contents of the message
    dwBufferSize: DWORD;            // Size of the message context
    pvConnectionContext: Pointer;   // Context value for this connection
  end;
  DPL_MESSAGE_RECEIVE = _DPL_MESSAGE_RECEIVE;
  TDPLMessageReceive = _DPL_MESSAGE_RECEIVE;

  //
  // Current status of the associated connection
  // (DPL_MSGID_SESSION_STATUS)
  //
  PDPLMessageSessionStatus = ^TDPLMessageSessionStatus;
  _DPL_MESSAGE_SESSION_STATUS = packed record
    dwSize: DWORD;                // Size of this structure
    hSender: TDPNHandle;          // Handle of the connection that this is from
    dwStatus: DWORD;              // Status (DPLSESSION_...)
    pvConnectionContext: Pointer; // Context value for this connection
  end;
  DPL_MESSAGE_SESSION_STATUS = _DPL_MESSAGE_SESSION_STATUS;
  TDPLMessageSessionStatus = _DPL_MESSAGE_SESSION_STATUS;

(****************************************************************************
 *
 * DirectPlay8Lobby Create
 *
 ****************************************************************************)
 
(*
 * This function is no longer supported.  It is recommended that CoCreateInstance be used to create
 * DirectPlay8 lobby objects.
 *
 * extern HRESULT WINAPI DirectPlay8LobbyCreate( const GUID * pcIID, void **ppvInterface, IUnknown *pUnknown);
 *
 *)

(****************************************************************************
 *
 * DirectPlay8 Functions
 *
 ****************************************************************************)

  //
  // COM definition for DirectPlayLobbyClient
  //
  IDirectPlay8LobbyClient = interface(IUnknown)
    ['{819074a2-016c-11d3-ae14-006097b01411}']
    // IDirectPlayLobbyClient methods
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; dwFlags: DWORD): HResult; stdcall;
    function EnumLocalPrograms(pGuidApplication: PGUID; pEnumData: PByte; var pdwEnumData: DWORD; out pdwItems: DWORD; dwFlags: DWORD): HResult; stdcall;
    function ConnectApplication(const pdplConnectionInfo: PDPLConnectInfo;
      pvConnectionContext: Pointer; hApplication: PDPNHandle; dwTimeOut, dwFlags: DWORD): HResult; stdcall;
    function Send(hConnection: TDPNHandle; pBuffer: PByte; pBufferSize, dwFlags: DWORD): HResult; stdcall;
    function ReleaseApplication(hConnection: TDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function GetConnectionSettings(hConnection: TDPNHandle; pdplSessionInfo: PDPLConnectionSettings; var pdwInfoSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetConnectionSettings(hConnection: TDPNHandle; const pdplSessionInfo: TDPLConnectionSettings; dwFlags: DWORD): HResult; stdcall;
  end;


  //
  // COM definition for DirectPlayLobbiedApplication
  //
  IDirectPlay8LobbiedApplication = interface(IUnknown)
    ['{819074a3-016c-11d3-ae14-006097b01411}']
    // IDirectPlayLobbiedApplication methods
    function Initialize(pvUserContext: Pointer; pfn: TFNDPNMessageHandler; pdpnhConnection: PDPNHandle; dwFlags: DWORD): HResult; stdcall;
    function RegisterProgram(const pdplProgramDesc: TDPLProgramDesc; dwFlags: DWORD): HResult; stdcall;
    function UnRegisterProgram(const pguidApplication: TGUID; dwFlags: DWORD): HResult; stdcall;
    function Send(hConnection: TDPNHandle; pBuffer: PByte; pBufferSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetAppAvailable(fAvailable: BOOL;  dwFlags: DWORD): HResult; stdcall;
    function UpdateStatus(hConnection: TDPNHandle; dwStatus, dwFlags: DWORD): HResult; stdcall;
    function Close(dwFlags: DWORD): HResult; stdcall;
    function GetConnectionSettings(hConnection: TDPNHandle; pdplSessionInfo: PDPLConnectionSettings; var pdwInfoSize: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetConnectionSettings(hConnection: TDPNHandle; const pdplSessionInfo: TDPLConnectionSettings; dwFlags: DWORD): HResult; stdcall;
  end;


const
(****************************************************************************
 *
 * DirectPlay8Lobby CLSIDs
 *
 ****************************************************************************)

  // {667955AD-6B3B-43ca-B949-BC69B5BAFF7F}
  CLSID_DirectPlay8LobbiedApplication: TGUID = '{667955ad-6b3b-43ca-b949-bc69b5baff7f}';

  // {3B2B6775-70B6-45af-8DEA-A209C69559F3}
  CLSID_DirectPlay8LobbyClient: TGUID = '{3b2b6775-70b6-45af-8dea-a209c69559f3}';

(****************************************************************************
 *
 * DirectPlay8Lobby Interface IIDs
 *
 ****************************************************************************)

type
  // {819074A3-016C-11d3-AE14-006097B01411}
  IID_IDirectPlay8LobbiedApplication = IDirectPlay8LobbiedApplication;

  // {819074A2-016C-11d3-AE14-006097B01411}
  IID_IDirectPlay8LobbyClient = IDirectPlay8LobbyClient;

(****************************************************************************
 *
 * DirectPlay8 Lobby Message IDs
 *
 ****************************************************************************)

const
  DPL_MSGID_LOBBY                     = $8000;
  DPL_MSGID_RECEIVE                   = ($0001 or DPL_MSGID_LOBBY);
  DPL_MSGID_CONNECT                   = ($0002 or DPL_MSGID_LOBBY);
  DPL_MSGID_DISCONNECT                = ($0003 or DPL_MSGID_LOBBY);
  DPL_MSGID_SESSION_STATUS            = ($0004 or DPL_MSGID_LOBBY);
  DPL_MSGID_CONNECTION_SETTINGS       = ($0005 or DPL_MSGID_LOBBY);

(****************************************************************************
 *
 * DirectPlay8Lobby Constants
 *
 ****************************************************************************)

  //
  // Specifies that operation should be performed on all open connections
  //
  DPLHANDLE_ALLCONNECTIONS     = $FFFFFFFF;

  //
  // The associated game session has suceeded in connecting / hosting
  //
  DPLSESSION_CONNECTED         = $0001;

  // The associated game session failed connecting / hosting
  //
  DPLSESSION_COULDNOTCONNECT   = $0002;

  //
  // The associated game session has disconnected
  //
  DPLSESSION_DISCONNECTED      = $0003;

  //
  // The associated game session has terminated
  //
  DPLSESSION_TERMINATED        = $0004;

  //
  // The associated game session's host has migrated
  //
  DPLSESSION_HOSTMIGRATED      = $0005;

  //
  // The associated game session's host has migrated to the local client
  //
  DPLSESSION_HOSTMIGRATEDHERE  = $0006;


(****************************************************************************
 *
 * DirectPlay8 Lobby Flags
 *
 ****************************************************************************)

  //
  // Do not automatically make the lobby app unavailable when a connection is established
  //
  DPLAVAILABLE_ALLOWMULTIPLECONNECT   = $0001;

  //
  // Launch a new instance of the application to connect to
  //
  DPLCONNECT_LAUNCHNEW                = $0001;

  //
  // Launch a new instance of the application if one is not waiting
  //
  DPLCONNECT_LAUNCHNOTFOUND           = $0002;

  //
  // When starting the associated game session, start it as a host
  //
  DPLCONNECTSETTINGS_HOST             = $0001;

  //
  // Disable parameter validation
  //
  DPLINITIALIZE_DISABLEPARAMVAL       = $0001;





///// Part 2 of dplay8.h /////
(*==========================================================================;
 *
 *  Copyright (C) 1998-2000 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:		DPlay8.h
 *  Content:	DirectPlay8 include file
 *
 ***************************************************************************)

(****************************************************************************
 *
 * DIRECTPLAY8 ERRORS
 *
 * Errors are represented by negative values and cannot be combined.
 *
 ****************************************************************************)

const
  _DPN_FACILITY_CODE = $015;
  _DPNHRESULT_BASE   = $8000;

  MAKE_DPNHRESULT_R  = (1 shl 31) or (_DPN_FACILITY_CODE shl 16) + _DPNHRESULT_BASE;

// #define MAKE_DPNHRESULT( code )  MAKE_HRESULT( 1, _DPN_FACILITY_CODE, ( code + _DPNHRESULT_BASE ) )
function MAKE_DPNHRESULT(Code: DWORD): DWORD;

const
  DPN_OK                    = S_OK;

  DPNSUCCESS_EQUAL          = HRESULT((0 shl 31) or (_DPN_FACILITY_CODE shl 16) or ($5 + _DPNHRESULT_BASE)); // MAKE_HRESULT( 0, _DPN_FACILITY_CODE, ( 0x5 + _DPNHRESULT_BASE ) )
  DPNSUCCESS_NOPLAYERSINGROUP = HRESULT((0 shl 31) or (_DPN_FACILITY_CODE shl 16) or ($8 + _DPNHRESULT_BASE)); // MAKE_HRESULT( 0, _DPN_FACILITY_CODE, ( 0x8 + _DPNHRESULT_BASE ) )	// added for DirectX 9
  DPNSUCCESS_NOTEQUAL       = HRESULT((0 shl 31) or (_DPN_FACILITY_CODE shl 16) or ($A + _DPNHRESULT_BASE)); // MAKE_HRESULT( 0, _DPN_FACILITY_CODE, (0x0A + _DPNHRESULT_BASE ) )
  DPNSUCCESS_PENDING        = HRESULT((0 shl 31) or (_DPN_FACILITY_CODE shl 16) or ($e + _DPNHRESULT_BASE)); // MAKE_HRESULT( 0, _DPN_FACILITY_CODE, (0x0e + _DPNHRESULT_BASE ) )

  DPNERR_ABORTED                 = HRESULT(MAKE_DPNHRESULT_R + $30); // MAKE_DPNHRESULT( 0x30 )
  DPNERR_ADDRESSING              = HRESULT(MAKE_DPNHRESULT_R + $40); // MAKE_DPNHRESULT( 0x40 )
  DPNERR_ALREADYCLOSING          = HRESULT(MAKE_DPNHRESULT_R + $50); // MAKE_DPNHRESULT( 0x50 )
  DPNERR_ALREADYCONNECTED        = HRESULT(MAKE_DPNHRESULT_R + $60); // MAKE_DPNHRESULT( 0x60 )
  DPNERR_ALREADYDISCONNECTING    = HRESULT(MAKE_DPNHRESULT_R + $70); // MAKE_DPNHRESULT( 0x70 )
  DPNERR_ALREADYINITIALIZED      = HRESULT(MAKE_DPNHRESULT_R + $80); // MAKE_DPNHRESULT(  0x80 )
  DPNERR_ALREADYREGISTERED       = HRESULT(MAKE_DPNHRESULT_R + $90); // MAKE_DPNHRESULT(  0x90 )
  DPNERR_BUFFERTOOSMALL          = HRESULT(MAKE_DPNHRESULT_R + $100); // MAKE_DPNHRESULT( 0x100 )
  DPNERR_CANNOTCANCEL            = HRESULT(MAKE_DPNHRESULT_R + $110); // MAKE_DPNHRESULT( 0x110 )
  DPNERR_CANTCREATEGROUP         = HRESULT(MAKE_DPNHRESULT_R + $120); // MAKE_DPNHRESULT( 0x120 )
  DPNERR_CANTCREATEPLAYER        = HRESULT(MAKE_DPNHRESULT_R + $130); // MAKE_DPNHRESULT( 0x130 )
  DPNERR_CANTLAUNCHAPPLICATION   = HRESULT(MAKE_DPNHRESULT_R + $140); // MAKE_DPNHRESULT( 0x140 )
  DPNERR_CONNECTING              = HRESULT(MAKE_DPNHRESULT_R + $150); // MAKE_DPNHRESULT( 0x150 )
  DPNERR_CONNECTIONLOST          = HRESULT(MAKE_DPNHRESULT_R + $160); // MAKE_DPNHRESULT( 0x160 )
  DPNERR_CONVERSION              = HRESULT(MAKE_DPNHRESULT_R + $170); // MAKE_DPNHRESULT( 0x170 )
  DPNERR_DATATOOLARGE            = HRESULT(MAKE_DPNHRESULT_R + $175); // MAKE_DPNHRESULT( 0x175 )
  DPNERR_DOESNOTEXIST            = HRESULT(MAKE_DPNHRESULT_R + $180); // MAKE_DPNHRESULT( 0x180 )
  DPNERR_DPNSVRNOTAVAILABLE      = HRESULT(MAKE_DPNHRESULT_R + $185); // MAKE_DPNHRESULT( 0x185 )
  DPNERR_DUPLICATECOMMAND        = HRESULT(MAKE_DPNHRESULT_R + $190); // MAKE_DPNHRESULT( 0x190 )
  DPNERR_ENDPOINTNOTRECEIVING    = HRESULT(MAKE_DPNHRESULT_R + $200); // MAKE_DPNHRESULT( 0x200 )
  DPNERR_ENUMQUERYTOOLARGE       = HRESULT(MAKE_DPNHRESULT_R + $210); // MAKE_DPNHRESULT( 0x210 )
  DPNERR_ENUMRESPONSETOOLARGE    = HRESULT(MAKE_DPNHRESULT_R + $220); // MAKE_DPNHRESULT( 0x220 )
  DPNERR_EXCEPTION               = HRESULT(MAKE_DPNHRESULT_R + $230); // MAKE_DPNHRESULT( 0x230 )
  DPNERR_GENERIC                 = E_FAIL;
  DPNERR_GROUPNOTEMPTY           = HRESULT(MAKE_DPNHRESULT_R + $240); // MAKE_DPNHRESULT( 0x240 )
  DPNERR_HOSTING                 = HRESULT(MAKE_DPNHRESULT_R + $250); // MAKE_DPNHRESULT( 0x250 )
  DPNERR_HOSTREJECTEDCONNECTION  = HRESULT(MAKE_DPNHRESULT_R + $260); // MAKE_DPNHRESULT( 0x260 )
  DPNERR_HOSTTERMINATEDSESSION   = HRESULT(MAKE_DPNHRESULT_R + $270); // MAKE_DPNHRESULT( 0x270 )
  DPNERR_INCOMPLETEADDRESS       = HRESULT(MAKE_DPNHRESULT_R + $280); // MAKE_DPNHRESULT( 0x280 )
  DPNERR_INVALIDADDRESSFORMAT    = HRESULT(MAKE_DPNHRESULT_R + $290); // MAKE_DPNHRESULT( 0x290 )
  DPNERR_INVALIDAPPLICATION      = HRESULT(MAKE_DPNHRESULT_R + $300); // MAKE_DPNHRESULT( 0x300 )
  DPNERR_INVALIDCOMMAND          = HRESULT(MAKE_DPNHRESULT_R + $310); // MAKE_DPNHRESULT( 0x310 )
  DPNERR_INVALIDDEVICEADDRESS    = HRESULT(MAKE_DPNHRESULT_R + $320); // MAKE_DPNHRESULT( 0x320 )
  DPNERR_INVALIDENDPOINT         = HRESULT(MAKE_DPNHRESULT_R + $330); // MAKE_DPNHRESULT( 0x330 )
  DPNERR_INVALIDFLAGS            = HRESULT(MAKE_DPNHRESULT_R + $340); // MAKE_DPNHRESULT( 0x340 )
  DPNERR_INVALIDGROUP            = HRESULT(MAKE_DPNHRESULT_R + $350); // MAKE_DPNHRESULT( 0x350 )
  DPNERR_INVALIDHANDLE           = HRESULT(MAKE_DPNHRESULT_R + $360); // MAKE_DPNHRESULT( 0x360 )
  DPNERR_INVALIDHOSTADDRESS      = HRESULT(MAKE_DPNHRESULT_R + $370); // MAKE_DPNHRESULT( 0x370 )
  DPNERR_INVALIDINSTANCE         = HRESULT(MAKE_DPNHRESULT_R + $380); // MAKE_DPNHRESULT( 0x380 )
  DPNERR_INVALIDINTERFACE        = HRESULT(MAKE_DPNHRESULT_R + $390); // MAKE_DPNHRESULT( 0x390 )
  DPNERR_INVALIDOBJECT           = HRESULT(MAKE_DPNHRESULT_R + $400); // MAKE_DPNHRESULT( 0x400 )
  DPNERR_INVALIDPARAM            = E_INVALIDARG;
  DPNERR_INVALIDPASSWORD         = HRESULT(MAKE_DPNHRESULT_R + $410); // MAKE_DPNHRESULT( 0x410 )
  DPNERR_INVALIDPLAYER           = HRESULT(MAKE_DPNHRESULT_R + $420); // MAKE_DPNHRESULT( 0x420 )
  DPNERR_INVALIDPOINTER          = E_POINTER;
  DPNERR_INVALIDPRIORITY         = HRESULT(MAKE_DPNHRESULT_R + $430); // MAKE_DPNHRESULT( 0x430 )
  DPNERR_INVALIDSTRING           = HRESULT(MAKE_DPNHRESULT_R + $440); // MAKE_DPNHRESULT( 0x440 )
  DPNERR_INVALIDURL              = HRESULT(MAKE_DPNHRESULT_R + $450); // MAKE_DPNHRESULT( 0x450 )
  DPNERR_INVALIDVERSION          = HRESULT(MAKE_DPNHRESULT_R + $460); // MAKE_DPNHRESULT( 0x460 )
  DPNERR_NOCAPS                  = HRESULT(MAKE_DPNHRESULT_R + $470); // MAKE_DPNHRESULT( 0x470 )
  DPNERR_NOCONNECTION            = HRESULT(MAKE_DPNHRESULT_R + $480); // MAKE_DPNHRESULT( 0x480 )
  DPNERR_NOHOSTPLAYER            = HRESULT(MAKE_DPNHRESULT_R + $490); // MAKE_DPNHRESULT( 0x490 )
  DPNERR_NOINTERFACE             = E_NOINTERFACE;
  DPNERR_NOMOREADDRESSCOMPONENTS = HRESULT(MAKE_DPNHRESULT_R + $500); // MAKE_DPNHRESULT( 0x500 )
  DPNERR_NORESPONSE              = HRESULT(MAKE_DPNHRESULT_R + $510); // MAKE_DPNHRESULT( 0x510 )
  DPNERR_NOTALLOWED              = HRESULT(MAKE_DPNHRESULT_R + $520); // MAKE_DPNHRESULT( 0x520 )
  DPNERR_NOTHOST                 = HRESULT(MAKE_DPNHRESULT_R + $530); // MAKE_DPNHRESULT( 0x530 )
  DPNERR_NOTREADY                = HRESULT(MAKE_DPNHRESULT_R + $540); // MAKE_DPNHRESULT( 0x540 )
  DPNERR_NOTREGISTERED           = HRESULT(MAKE_DPNHRESULT_R + $550); // MAKE_DPNHRESULT( 0x550 )
  DPNERR_OUTOFMEMORY             = E_OUTOFMEMORY;
  DPNERR_PENDING                 = DPNSUCCESS_PENDING;
  DPNERR_PLAYERALREADYINGROUP    = HRESULT(MAKE_DPNHRESULT_R + $560); // MAKE_DPNHRESULT( 0x560 )
  DPNERR_PLAYERLOST              = HRESULT(MAKE_DPNHRESULT_R + $570); // MAKE_DPNHRESULT( 0x570 )
  DPNERR_PLAYERNOTINGROUP        = HRESULT(MAKE_DPNHRESULT_R + $580); // MAKE_DPNHRESULT( 0x580 )
  DPNERR_PLAYERNOTREACHABLE      = HRESULT(MAKE_DPNHRESULT_R + $590); // MAKE_DPNHRESULT( 0x590 )
  DPNERR_SENDTOOLARGE            = HRESULT(MAKE_DPNHRESULT_R + $600); // MAKE_DPNHRESULT( 0x600 )
  DPNERR_SESSIONFULL             = HRESULT(MAKE_DPNHRESULT_R + $610); // MAKE_DPNHRESULT( 0x610 )
  DPNERR_TABLEFULL               = HRESULT(MAKE_DPNHRESULT_R + $620); // MAKE_DPNHRESULT( 0x620 )
  DPNERR_TIMEDOUT                = HRESULT(MAKE_DPNHRESULT_R + $630); // MAKE_DPNHRESULT( 0x630 )
  DPNERR_UNINITIALIZED           = HRESULT(MAKE_DPNHRESULT_R + $640); // MAKE_DPNHRESULT( 0x640 )
  DPNERR_UNSUPPORTED             = E_NOTIMPL;
  DPNERR_USERCANCEL              = HRESULT(MAKE_DPNHRESULT_R + $650); // MAKE_DPNHRESULT( 0x650 )




(*==========================================================================;
 *
 *  Copyright (C) 1999 Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       dpvoice.h
 *  Content:    DirectPlayVoice include file
 ***************************************************************************)


const
(****************************************************************************
 *
 * DirectPlayVoice CLSIDs
 *
 ****************************************************************************)


  // {B9F3EB85-B781-4ac1-8D90-93A05EE37D7D}
  CLSID_DirectPlayVoiceClient: TGUID = '{b9f3eb85-b781-4ac1-8d90-93a05ee37d7d}';

  // {D3F5B8E6-9B78-4a4c-94EA-CA2397B663D3}
  CLSID_DirectPlayVoiceServer: TGUID = '{d3f5b8e6-9b78-4a4c-94ea-ca2397b663d3}';

  // {0F0F094B-B01C-4091-A14D-DD0CD807711A}
  CLSID_DirectPlayVoiceTest: TGUID = '{0f0f094b-b01c-4091-a14d-dd0cd807711a}';

(****************************************************************************
 *
 * DirectPlayVoice Compression Type GUIDs
 *
 ****************************************************************************)

  // MS-ADPCM 32.8 kbit/s
  //
  // {699B52C1-A885-46a8-A308-97172419ADC7}
  DPVCTGUID_ADPCM: TGUID = '{699b52c1-a885-46a8-a308-97172419adc7}';

  // Microsoft GSM 6.10 13 kbit/s
  //
  // {24768C60-5A0D-11d3-9BE4-525400D985E7}
  DPVCTGUID_GSM: TGUID = '{24768c60-5a0d-11d3-9be4-525400d985e7}';

  // MS-PCM 64 kbit/s
  //
  // {8DE12FD4-7CB3-48ce-A7E8-9C47A22E8AC5}
  DPVCTGUID_NONE: TGUID = '{8de12fd4-7cb3-48ce-a7e8-9c47a22e8ac5}';

  // Voxware SC03 3.2kbit/s
  //
  // {7D82A29B-2242-4f82-8F39-5D1153DF3E41}
  DPVCTGUID_SC03: TGUID = '{7d82a29b-2242-4f82-8f39-5d1153df3e41}';

  // Voxware SC06 6.4kbit/s
  //
  // {53DEF900-7168-4633-B47F-D143916A13C7}
  DPVCTGUID_SC06: TGUID = '{53def900-7168-4633-b47f-d143916a13c7}';

  // TrueSpeech(TM) 8.6 kbit/s
  //
  // {D7954361-5A0B-11d3-9BE4-525400D985E7}
  DPVCTGUID_TRUESPEECH: TGUID = '{d7954361-5a0b-11d3-9be4-525400d985e7}';

  // Voxware VR12 1.4kbit/s
  //
  // {FE44A9FE-8ED4-48bf-9D66-1B1ADFF9FF6D}
  DPVCTGUID_VR12: TGUID = '{fe44a9fe-8ed4-48bf-9d66-1b1adff9ff6d}';

  // Define the default compression type
  DPVCTGUID_DEFAULT: TGUID = '{7d82a29b-2242-4f82-8f39-5d1153df3e41}'; // DPVCTGUID_SC03;

(****************************************************************************
 *
 * DirectPlayVoice Callback Functions
 *
 ****************************************************************************)

type
  TDVMessageHandler = function(pvUserContext: Pointer; dwMessageType: DWORD; lpMessage: Pointer): HResult; stdcall;

(****************************************************************************
 *
 * DirectPlayVoice Datatypes (Non-Structure / Non-Message)
 *
 ****************************************************************************)

  PDVID = ^DVID;
  DVID = DWORD;
  TDVID = DVID;

(****************************************************************************
 *
 * DirectPlayVoice Message Types
 *
 ****************************************************************************)

const
  DVMSGID_BASE                        = $0000;

  DVMSGID_CREATEVOICEPLAYER           = (DVMSGID_BASE+$0001);
  DVMSGID_DELETEVOICEPLAYER           = (DVMSGID_BASE+$0002);
  DVMSGID_SESSIONLOST                 = (DVMSGID_BASE+$0003);
  DVMSGID_PLAYERVOICESTART            = (DVMSGID_BASE+$0004);
  DVMSGID_PLAYERVOICESTOP             = (DVMSGID_BASE+$0005);
  DVMSGID_RECORDSTART                 = (DVMSGID_BASE+$0006);
  DVMSGID_RECORDSTOP                  = (DVMSGID_BASE+$0007);
  DVMSGID_CONNECTRESULT               = (DVMSGID_BASE+$0008);
  DVMSGID_DISCONNECTRESULT            = (DVMSGID_BASE+$0009);
  DVMSGID_INPUTLEVEL                  = (DVMSGID_BASE+$000A);
  DVMSGID_OUTPUTLEVEL                 = (DVMSGID_BASE+$000B);
  DVMSGID_HOSTMIGRATED                = (DVMSGID_BASE+$000C);
  DVMSGID_SETTARGETS                  = (DVMSGID_BASE+$000D);
  DVMSGID_PLAYEROUTPUTLEVEL           = (DVMSGID_BASE+$000E);
  DVMSGID_LOSTFOCUS                   = (DVMSGID_BASE+$0010);
  DVMSGID_GAINFOCUS                   = (DVMSGID_BASE+$0011);
  DVMSGID_LOCALHOSTSETUP              = (DVMSGID_BASE+$0012);
  DVMSGID_MAXBASE                     = (DVMSGID_LOCALHOSTSETUP);
  DVMSGID_MINBASE                     = (DVMSGID_CREATEVOICEPLAYER);

(****************************************************************************
 *
 * DirectPlayVoice Constants
 *
 ****************************************************************************)

  //
  // Buffer Aggresiveness Value Ranges
  //
  DVBUFFERAGGRESSIVENESS_MIN          = $00000001;
  DVBUFFERAGGRESSIVENESS_MAX          = $00000064;
  DVBUFFERAGGRESSIVENESS_DEFAULT      = $00000000;

  //
  // Buffer Quality Value Ranges
  //
  DVBUFFERQUALITY_MIN                 = $00000001;
  DVBUFFERQUALITY_MAX                 = $00000064;
  DVBUFFERQUALITY_DEFAULT             = $00000000;

  DVID_SYS                = 0;

  //
  // Used to identify the session host in client/server
  //
  DVID_SERVERPLAYER       = 1;

  //
  // Used to target all players
  //
  DVID_ALLPLAYERS         = 0;

  //
  // Used to identify the main buffer
  //
  DVID_REMAINING          = $FFFFFFFF;

  //
  // Input level range
  //
  DVINPUTLEVEL_MIN                    = $00000000;
  DVINPUTLEVEL_MAX                    = $00000063; // 99 decimal

  DVNOTIFYPERIOD_MINPERIOD            = 20;


  DVPLAYBACKVOLUME_DEFAULT            = DSBVOLUME_MAX;

  DVRECORDVOLUME_LAST                 = $00000001;


  //
  // Use the default value
  //
  DVTHRESHOLD_DEFAULT               = $FFFFFFFF;

  //
  // Threshold Ranges
  //
  DVTHRESHOLD_MIN                   = $00000000;
  DVTHRESHOLD_MAX                   = $00000063;	// 99 decimal

  //
  // Threshold field is not used
  //
  DVTHRESHOLD_UNUSED                = $FFFFFFFE;

  //
  // Session Types
  //
  DVSESSIONTYPE_PEER                  = $00000001;
  DVSESSIONTYPE_MIXING                = $00000002;
  DVSESSIONTYPE_FORWARDING            = $00000003;
  DVSESSIONTYPE_ECHO                  = $00000004;

(****************************************************************************
 *
 * DirectPlayVoice Flags
 *
 ****************************************************************************)


  //
  // Enable automatic adjustment of the recording volume
  //
  DVCLIENTCONFIG_AUTORECORDVOLUME     = $00000008;

  //
  // Enable automatic voice activation
  //
  DVCLIENTCONFIG_AUTOVOICEACTIVATED   = $00000020;

  //
  // Enable echo suppression
  //
  DVCLIENTCONFIG_ECHOSUPPRESSION      = $08000000;

  //
  // Voice Activation manual mode
  //
  DVCLIENTCONFIG_MANUALVOICEACTIVATED = $00000004;

  //
  // Only playback voices that have buffers created for them
  //
  DVCLIENTCONFIG_MUTEGLOBAL           = $00000010;

  //
  // Mute the playback
  //
  DVCLIENTCONFIG_PLAYBACKMUTE         = $00000002;

  //
  // Mute the recording
  //
  DVCLIENTCONFIG_RECORDMUTE           = $00000001;

  //
  // Complete the operation before returning
  //
  DVFLAGS_SYNC                        = $00000001;

  //
  // Just check to see if wizard has been run, and if so what it's results were
  //
  DVFLAGS_QUERYONLY                   = $00000002;

  //
  // Shutdown the voice session without migrating the host
  //
  DVFLAGS_NOHOSTMIGRATE               = $00000008;

  //
  // Allow the back button to be enabled in the wizard
  //
  DVFLAGS_ALLOWBACK                   = $00000010;

  //
  // Disable host migration in the voice session
  //
  DVSESSION_NOHOSTMIGRATION           = $00000001;

  //
  // Server controlled targetting
  //
  DVSESSION_SERVERCONTROLTARGET       = $00000002;

  //
  // Use DirectSound Normal Mode instead of priority
  //
  DVSOUNDCONFIG_NORMALMODE            = $00000001;

  //
  // Automatically select the microphone
  //
  DVSOUNDCONFIG_AUTOSELECT            = $00000002;

  //
  // Run in half duplex mode
  //
  DVSOUNDCONFIG_HALFDUPLEX            = $00000004;

  //
  // No volume controls are available for the recording device
  //
  DVSOUNDCONFIG_NORECVOLAVAILABLE     = $00000010;

  //
  // Disable capture sharing
  //
  DVSOUNDCONFIG_NOFOCUS               = $20000000;

  //
  // Set system conversion quality to high
  //
  DVSOUNDCONFIG_SETCONVERSIONQUALITY	= $00000008;

  //
  // Enable strict focus mode
  //
  DVSOUNDCONFIG_STRICTFOCUS           = $40000000;

  //
  // Player is in half duplex mode
  //
  DVPLAYERCAPS_HALFDUPLEX             = $00000001;

  //
  // Specifies that player is the local player
  //
  DVPLAYERCAPS_LOCAL                  = $00000002;


type
(****************************************************************************
 *
 * DirectPlayVoice Interface Pointer definitions
 *
 ****************************************************************************)

  IDirectPlayVoiceServer = interface;

(****************************************************************************
 *
 * DirectPlayVoice Structures (Non-Message)
 *
 ****************************************************************************)


  //
  // DirectPlayVoice Caps
  // (GetCaps / SetCaps)
  //
  PDVCaps = ^TDVCaps;
  DVCAPS = packed record
    dwSize:   DWORD;                 // Size of this structure
    dwFlags:   DWORD;                // Caps flags
  end;
  TDVCaps = DVCAPS;

  //
  // DirectPlayVoice Client Configuration
  // (Connect / GetClientConfig)
  //
  PDVClientConfig = ^TDVClientConfig;
  DVCLIENTCONFIG = packed record
    dwSize: DWORD;                 // Size of this structure
    dwFlags: DWORD;                // Flags for client config (DVCLIENTCONFIG_...)
    lRecordVolume: Longint;        // Recording volume
    lPlaybackVolume: Longint;      // Playback volume
    dwThreshold: DWORD;            // Voice Activation Threshold
    dwBufferQuality: DWORD;        // Buffer quality
    dwBufferAggressiveness: DWORD; // Buffer aggressiveness
    dwNotifyPeriod: DWORD;         // Period of notification messages (ms)
  end;
  TDVClientConfig = DVCLIENTCONFIG;

  //
  // DirectPlayVoice Compression Type Information
  // (GetCompressionTypes)
  //
  PDVCompressionInfo = ^TDVCompressionInfo;
  DVCOMPRESSIONINFO = packed record
    dwSize: DWORD;                 // Size of this structure
    guidType: TGUID;               // GUID that identifies this compression type
    lpszName: PWideChar;           // String name of this compression type
    lpszDescription: PWideChar;    // Description for this compression type
    dwFlags: DWORD;                // Flags for this compression type
    dwMaxBitsPerSecond: DWORD;     // Maximum # of bit/s this compression type uses
  end;
  TDVCompressionInfo = DVCOMPRESSIONINFO;

  //
  // DirectPlayVoice Session Description
  // (Host / GetSessionDesc)
  //
  PDVSessionDesc = ^TDVSessionDesc;
  DVSESSIONDESC = packed record
    dwSize: DWORD;                 // Size of this structure
    dwFlags: DWORD;                // Session flags (DVSESSION_...)
    dwSessionType: DWORD;          // Session type (DVSESSIONTYPE_...)
    guidCT: TGUID;                 // Compression Type to use
    dwBufferQuality: DWORD;        // Buffer quality
    dwBufferAggressiveness: DWORD; // Buffer aggresiveness
  end;
  TDVSessionDesc = DVSESSIONDESC;

  //
  // DirectPlayVoice Client Sound Device Configuration
  // (Connect / GetSoundDeviceConfig)
  //
  PDVSoundDeviceConfig = ^TDVSoundDeviceConfig;
  DVSOUNDDEVICECONFIG = packed record
    dwSize: DWORD;                 // Size of this structure
    dwFlags: DWORD;                // Flags for sound config (DVSOUNDCONFIG_...)
    guidPlaybackDevice: TGUID;     // GUID of the playback device to use
    lpdsPlaybackDevice: IDirectSound; // DirectSound Object to use (optional)
    guidCaptureDevice: TGUID;      // GUID of the capture device to use
    lpdsCaptureDevice: IDirectSoundCapture; // DirectSoundCapture Object to use (optional)
    hwndAppWindow: HWND;           // HWND of your application's top-level window
    lpdsMainBuffer: IDirectSoundBuffer; // DirectSoundBuffer to use for playback (optional)
    dwMainBufferFlags: DWORD;      // Flags to pass to Play() on the main buffer
    dwMainBufferPriority: DWORD;   // Priority to set when calling Play() on the main buffer
  end;
  TDVSoundDeviceConfig = DVSOUNDDEVICECONFIG;

(****************************************************************************
 *
 * DirectPlayVoice message handler call back structures
 *
 ****************************************************************************)

  //
  // Result of the Connect() call.  (If it wasn't called Async)
  // (DVMSGID_CONNECTRESULT)
  //
  PDVMsgConnectResult = ^TDVMsgConnectResult;
  DVMSG_CONNECTRESULT = packed record
    dwSize: DWORD;                         // Size of this structure
    hrResult: HRESULT;                     // Result of the Connect() call
  end;
  TDVMsgConnectResult = DVMSG_CONNECTRESULT;

  //
  // A new player has entered the voice session
  // (DVMSGID_CREATEVOICEPLAYER)
  //
  PDVMsgCreateVoicePlayer = ^TDVMsgCreateVoicePlayer;
  DVMSG_CREATEVOICEPLAYER = packed record
    dwSize: DWORD;                         // Size of this structure
    dvidPlayer: TDVID;                     // DVID of the player who joined
    dwFlags: DWORD;                        // Player flags (DVPLAYERCAPS_...)
    pvPlayerContext: Pointer;              // Context value for this player (user set)
  end;
  TDVMsgCreateVoicePlayer = DVMSG_CREATEVOICEPLAYER;

  //
  // A player has left the voice session
  // (DVMSGID_DELETEVOICEPLAYER)
  //
  PDVMsgDeleteVoicePlayer = ^TDVMsgDeleteVoicePlayer;
  DVMSG_DELETEVOICEPLAYER = packed record
    dwSize: DWORD;                         // Size of this structure
    dvidPlayer: TDVID;                     // DVID of the player who left
    pvPlayerContext: Pointer;              // Context value for the player
  end;
  TDVMsgDeleteVoicePlayer = DVMSG_DELETEVOICEPLAYER;

  //
  // Result of the Disconnect() call.  (If it wasn't called Async)
  // (DVMSGID_DISCONNECTRESULT)
  //
  PDVMsgDisconnectResult = ^TDVMsgDisconnectResult;
  DVMSG_DISCONNECTRESULT = packed record
    dwSize: DWORD;                         // Size of this structure
    hrResult: HRESULT;                     // Result of the Disconnect() call
  end;
  TDVMsgDisconnectResult = DVMSG_DISCONNECTRESULT;

  //
  // The voice session host has migrated.
  // (DVMSGID_HOSTMIGRATED)
  //
  PDVMsgHostMigrated = ^TDVMsgHostMigrated;
  DVMSG_HOSTMIGRATED = packed record
    dwSize: DWORD;         // Size of this structure
    dvidNewHostID: TDVID;  // DVID of the player who is now the host
    pdvServerInterface: IDirectPlayVoiceServer; // Pointer to the new host object (if local player is now host)
  end;
  TDVMsgHostMigrated = DVMSG_HOSTMIGRATED;

  //
  // The current input level / recording volume on the local machine
  // (DVMSGID_INPUTLEVEL)
  //
  PDVMsgInputlevel = ^TDVMsgInputlevel;
  DVMSG_INPUTLEVEL = packed record
    dwSize: DWORD;                         // Size of this structure
    dwPeakLevel: DWORD;                    // Current peak level of the audio
    lRecordVolume: Longint;                // Current recording volume
    pvLocalPlayerContext: Pointer;         // Context value for the local player
  end;
  TDVMsgInputlevel = DVMSG_INPUTLEVEL;

  //
  // The local client is about to become the new host
  // (DVMSGID_LOCALHOSTSETUP)
  //
  PDVMsgLocalHostSetup = ^TDVMsgLocalHostSetup;
  DVMSG_LOCALHOSTSETUP = packed record
    dwSize: DWORD;             // Size of this structure
    pvContext: Pointer;        // Context value to be passed to Initialize() of new host object
    pMessageHandler: TDVMessageHandler; // Message handler to be used by new host object
  end;
  TDVMsgLocalHostSetup = DVMSG_LOCALHOSTSETUP;

  //
  // The current output level for the combined output of all incoming streams.
  // (DVMSGID_OUTPUTLEVEL)
  //
  PDVMsgOutputLevel = ^TDVMsgOutputLevel;
  DVMSG_OUTPUTLEVEL = packed record
    dwSize: DWORD;                         // Size of this structure
    dwPeakLevel: DWORD;                    // Current peak level of the output
    lOutputVolume: Longint;                // Current playback volume
    pvLocalPlayerContext: Pointer;         // Context value for the local player
  end;
  TDVMsgOutputLevel = DVMSG_OUTPUTLEVEL;

  //
  // The current peak level of an individual player's incoming audio stream as it is
  // being played back.
  // (DVMSGID_PLAYEROUTPUTLEVEL)
  //
  PDVMsgPlayerOutputLevel = ^TDVMsgPlayerOutputLevel;
  DVMSG_PLAYEROUTPUTLEVEL = packed record
    dwSize: DWORD;                         // Size of this structure
    dvidSourcePlayerID: TDVID;             // DVID of the player
    dwPeakLevel: DWORD;                    // Peak level of the player's stream
    pvPlayerContext: Pointer;              // Context value for the player
  end;
  TDVMsgPlayerOutputLevel = DVMSG_PLAYEROUTPUTLEVEL;

  //
  // An audio stream from the specified player has started playing back on the local client.
  // (DVMSGID_PLAYERVOICESTART).
  //
  PDVMsgPlayerVoiceStart = ^TDVMsgPlayerVoiceStart;
  DVMSG_PLAYERVOICESTART = packed record
    dwSize: DWORD;                         // Size of this structure
    dvidSourcePlayerID: TDVID;             // DVID of the Player
    pvPlayerContext: Pointer;              // Context value for this player
  end;
  TDVMsgPlayerVoiceStart = DVMSG_PLAYERVOICESTART;

  //
  // The audio stream from the specified player has stopped playing back on the local client.
  // (DVMSGID_PLAYERVOICESTOP)
  //
  PDVMsgPlayerVoiceStop = ^TDVMsgPlayerVoiceStop;
  DVMSG_PLAYERVOICESTOP = packed record
    dwSize:   DWORD;                         // Size of this structure
    dvidSourcePlayerID:    DVID;             // DVID of the player
    pvPlayerContext:	Pointer;                // Context value for this player
  end;
  TDVMsgPlayerVoiceStop = DVMSG_PLAYERVOICESTOP;

  //
  // Transmission has started on the local machine
  // (DVMSGID_RECORDSTART)
  //
  PDVMsgRecordStart = ^TDVMsgRecordStart;
  DVMSG_RECORDSTART = packed record
    dwSize:   DWORD;                         // Size of this structure
    dwPeakLevel:   DWORD;                    // Peak level that caused transmission to start
    pvLocalPlayerContext:	Pointer;           // Context value for the local player
  end;
  TDVMsgRecordStart = DVMSG_RECORDSTART;

  //
  // Transmission has stopped on the local machine
  // (DVMSGID_RECORDSTOP)
  //
  PDVMsgRecordStop = ^TDVMsgRecordStop;
  DVMSG_RECORDSTOP = packed record
    dwSize: DWORD;                         // Size of this structure
    dwPeakLevel: DWORD;                    // Peak level that caused transmission to stop
    pvLocalPlayerContext: Pointer;         // Context value for the local player
  end;
  TDVMsgRecordStop = DVMSG_RECORDSTOP;

  //
  // The voice session has been lost
  // (DVMSGID_SESSIONLOST)
  //
  PDVMsgSessionLost = ^TDVMsgSessionLost;
  DVMSG_SESSIONLOST = packed record
    dwSize: DWORD;                         // Size of this structure
    hrResult: HRESULT;                     // Reason the session was disconnected
  end;
  TDVMsgSessionLost = DVMSG_SESSIONLOST;

  //
  // The target list has been updated for the local client
  // (DVMSGID_SETTARGETS)
  //
  PDVMsgSetTargets = ^TDVMsgSetTargets;
  DVMSG_SETTARGETS = packed record
    dwSize: DWORD;                         // Size of this structure
    dwNumTargets: DWORD;                   // # of targets
    pdvidTargets: PDVID;                   // An array of DVIDs specifying the current targets
  end;
  TDVMsgSetTargets = DVMSG_SETTARGETS;


(****************************************************************************
 *
 * DirectPlayVoice Functions
 *
 ****************************************************************************)

(*
 *
 * This function is no longer supported.  It is recommended that CoCreateInstance be used to create
 * DirectPlay voice objects.
 *
 * extern HRESULT WINAPI DirectPlayVoiceCreate( const GUID * pcIID, void **ppvInterface, IUnknown *pUnknown);
 *
 *)

(****************************************************************************
 *
 * DirectPlay8 Application Interfaces
 *
 ****************************************************************************)

  IDirectPlayVoiceClient = interface(IUnknown)
    ['{1dfdc8ea-bcf7-41d6-b295-ab64b3b23306}']
    (*** IDirectPlayVoiceClient methods ***)
    function Initialize(pVoid: IUnknown; pMessageHandler: TDVMessageHandler;
      pUserContext: Pointer; pdwMessageMask: PDWORD; dwMessageMaskElements: DWORD): HResult; stdcall;
    function Connect(const pSoundDeviceConfig: TDVSoundDeviceConfig;
      const pdvClientConfig: TDVClientConfig; dwFlags: DWORD): HResult; stdcall;
    function Disconnect(dwFlags: DWORD): HResult; stdcall;
    function GetSessionDesc(out pvSessionDesc: TDVSessionDesc): HResult; stdcall;
    function GetClientConfig(out pClientConfig: TDVClientConfig): HResult; stdcall;
    function SetClientConfig(const pClientConfig: TDVClientConfig): HResult; stdcall;
    function GetCaps(out pDVCaps: TDVCaps): HResult; stdcall;
    function GetCompressionTypes(pData: Pointer; var pdwDataSize: DWORD;
      out pdwNumElements: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetTransmitTargets(pdvIDTargets: PDVID; dwNumTargets, dwFlags: DWORD): HResult; stdcall;
    function GetTransmitTargets(pdvIDTargets: PDVID; var dwNumTargets: DWORD; dwFlags: DWORD): HResult; stdcall;
    function Create3DSoundBuffer(dvID: TDVID; lpdsSourceBuffer: IDirectSoundBuffer;
      dwPriority, dwFlags: DWORD; out lpUserBuffer: IDirectSound3DBuffer): HResult; stdcall;
    function Delete3DSoundBuffer(dvID: TDVID; lpUserBuffer: IDirectSound3DBuffer): HResult; stdcall;
    function SetNotifyMask(pdwMessageMask: PDWORD; dwMessageMaskElements: DWORD): HResult; stdcall;
    function GetSoundDeviceConfig(pSoundDeviceConfig: PDVSoundDeviceConfig; var pdwSize: DWORD): HResult; stdcall;
  end;


  IDirectPlayVoiceServer = interface(IUnknown)
    ['{faa1c173-0468-43b6-8a2a-ea8a4f2076c9}']
    (*** IDirectPlayVoiceServer methods ***)
    function Initialize(pVoid: IUnknown; pMessageHandler: TDVMessageHandler;
      pUserContext: Pointer; pdwMessageMask: PDWORD; dwMessageMaskElements: DWORD): HResult; stdcall;
    function StartSession(const pSessionDesc: TDVSessionDesc; dwFlags: DWORD): HResult; stdcall;
    function StopSession(dwFlags: DWORD): HResult; stdcall;
    function GetSessionDesc(out pvSessionDesc: TDVSessionDesc): HResult; stdcall;
    function SetSessionDesc(const pvSessionDesc: TDVSessionDesc): HResult; stdcall;
    function GetCaps(out pDVCaps: TDVCaps): HResult; stdcall;
    function GetCompressionTypes(pData: Pointer; var pdwDataSize: DWORD;
      out pdwNumElements: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetTransmitTargets(dvSource: TDVID; pdvIDTargets: PDVID;
      dwNumTargets, dwFlags: DWORD): HResult; stdcall;
    function GetTransmitTargets(dvSource: TDVID; pdvIDTargets: PDVID;
      var dwNumTargets: DWORD; dwFlags: DWORD): HResult; stdcall;
    function SetNotifyMask(pdwMessageMask: PDWORD; dwMessageMaskElements: DWORD): HResult; stdcall;
  end;

  IDirectPlayVoiceTest = interface(IUnknown)
    ['{d26af734-208b-41da-8224-e0ce79810be1}']
    (*** IDirectPlayVoiceTest methods ***)
    function CheckAudioSetup(pguidPlaybackDevice, pguidCaptureDevice: PGUID;
      hwndParent: hWND; dwFlags: DWORD): HResult; stdcall;
  end;

(****************************************************************************
 *
 * DirectPlayVoice Interface IIDs
 *
 ****************************************************************************)

  // {1DFDC8EA-BCF7-41d6-B295-AB64B3B23306}
  IID_IDirectPlayVoiceClient = IDirectPlayVoiceClient;

  // {FAA1C173-0468-43b6-8A2A-EA8A4F2076C9}
  IID_IDirectPlayVoiceServer = IDirectPlayVoiceServer;

  // {D26AF734-208B-41da-8224-E0CE79810BE1}
  IID_IDirectPlayVoiceTest = IDirectPlayVoiceTest;


(****************************************************************************
 *
 * DIRECTPLAYVOICE ERRORS
 *
 * Errors are represented by negative values and cannot be combined.
 *
 ****************************************************************************)

const
  _FACDPV  = $15;

//#define MAKE_DVHRESULT( code )          MAKE_HRESULT( 1, _FACDPV, code )

(*
#define DV_FULLDUPLEX                   MAKE_HRESULT( 0, _FACDPV,  0x0005 )
#define DV_HALFDUPLEX                   MAKE_HRESULT( 0, _FACDPV,  0x000A )
#define DV_PENDING						MAKE_HRESULT( 0, _FACDPV,  0x0010 )

#define DVERR_BUFFERTOOSMALL            MAKE_DVHRESULT(  0x001E )
#define DVERR_EXCEPTION                 MAKE_DVHRESULT(  0x004A )
#define DVERR_GENERIC                   E_FAIL
#define DVERR_INVALIDFLAGS              MAKE_DVHRESULT( 0x0078 )
#define DVERR_INVALIDOBJECT             MAKE_DVHRESULT( 0x0082 )
#define DVERR_INVALIDPARAM              E_INVALIDARG
#define DVERR_INVALIDPLAYER             MAKE_DVHRESULT( 0x0087 )
#define DVERR_INVALIDGROUP              MAKE_DVHRESULT( 0x0091 )
#define DVERR_INVALIDHANDLE             MAKE_DVHRESULT( 0x0096 )
#define DVERR_OUTOFMEMORY               E_OUTOFMEMORY
#define DVERR_PENDING                   DV_PENDING
#define DVERR_NOTSUPPORTED              E_NOTIMPL
#define DVERR_NOINTERFACE               E_NOINTERFACE
#define DVERR_SESSIONLOST               MAKE_DVHRESULT( 0x012C )
#define DVERR_NOVOICESESSION            MAKE_DVHRESULT( 0x012E )
#define DVERR_CONNECTIONLOST            MAKE_DVHRESULT( 0x0168 )
#define DVERR_NOTINITIALIZED            MAKE_DVHRESULT( 0x0169 )
#define DVERR_CONNECTED                 MAKE_DVHRESULT( 0x016A )
#define DVERR_NOTCONNECTED              MAKE_DVHRESULT( 0x016B )
#define DVERR_CONNECTABORTING           MAKE_DVHRESULT( 0x016E )
#define DVERR_NOTALLOWED                MAKE_DVHRESULT( 0x016F )
#define DVERR_INVALIDTARGET             MAKE_DVHRESULT( 0x0170 )
#define DVERR_TRANSPORTNOTHOST          MAKE_DVHRESULT( 0x0171 )
#define DVERR_COMPRESSIONNOTSUPPORTED   MAKE_DVHRESULT( 0x0172 )
#define DVERR_ALREADYPENDING            MAKE_DVHRESULT( 0x0173 )
#define DVERR_SOUNDINITFAILURE          MAKE_DVHRESULT( 0x0174 )
#define DVERR_TIMEOUT                   MAKE_DVHRESULT( 0x0175 )
#define DVERR_CONNECTABORTED            MAKE_DVHRESULT( 0x0176 )
#define DVERR_NO3DSOUND                 MAKE_DVHRESULT( 0x0177 )
#define DVERR_ALREADYBUFFERED	        MAKE_DVHRESULT( 0x0178 )
#define DVERR_NOTBUFFERED               MAKE_DVHRESULT( 0x0179 )
#define DVERR_HOSTING                   MAKE_DVHRESULT( 0x017A )
#define DVERR_NOTHOSTING                MAKE_DVHRESULT( 0x017B )
#define DVERR_INVALIDDEVICE             MAKE_DVHRESULT( 0x017C )
#define DVERR_RECORDSYSTEMERROR         MAKE_DVHRESULT( 0x017D )
#define DVERR_PLAYBACKSYSTEMERROR       MAKE_DVHRESULT( 0x017E )
#define DVERR_SENDERROR                 MAKE_DVHRESULT( 0x017F )
#define DVERR_USERCANCEL                MAKE_DVHRESULT( 0x0180 )
#define DVERR_RUNSETUP                  MAKE_DVHRESULT( 0x0183 )
#define DVERR_INCOMPATIBLEVERSION       MAKE_DVHRESULT( 0x0184 )
#define DVERR_INITIALIZED               MAKE_DVHRESULT( 0x0187 )
#define DVERR_INVALIDPOINTER            E_POINTER
#define DVERR_NOTRANSPORT               MAKE_DVHRESULT( 0x0188 )
#define DVERR_NOCALLBACK                MAKE_DVHRESULT( 0x0189 )
#define DVERR_TRANSPORTNOTINIT          MAKE_DVHRESULT( 0x018A )
#define DVERR_TRANSPORTNOSESSION        MAKE_DVHRESULT( 0x018B )
#define DVERR_TRANSPORTNOPLAYER         MAKE_DVHRESULT( 0x018C )
#define DVERR_USERBACK                  MAKE_DVHRESULT( 0x018D )
#define DVERR_NORECVOLAVAILABLE         MAKE_DVHRESULT( 0x018E )
#define DVERR_INVALIDBUFFER				MAKE_DVHRESULT( 0x018F )
#define DVERR_LOCKEDBUFFER				MAKE_DVHRESULT( 0x0190 )
*)

  MAKE_DVHRESULT_R                = (1 shl 31) or (_FACDPV shl 16);

  DV_OK                           = S_OK;
  DV_FULLDUPLEX                   = (0 shl 31) or (_FACDPV shl 16) or $0005;
  DV_HALFDUPLEX                   = (0 shl 31) or (_FACDPV shl 16) or $000A;
  DV_PENDING		      	  = (0 shl 31) or (_FACDPV shl 16) or $0010;

  DVERR_BUFFERTOOSMALL            = MAKE_DVHRESULT_R + $001E ;
  DVERR_EXCEPTION                 = MAKE_DVHRESULT_R + $004A ;
  DVERR_GENERIC                   = E_FAIL;
  DVERR_INVALIDFLAGS              = MAKE_DVHRESULT_R + $0078 ;
  DVERR_INVALIDOBJECT             = MAKE_DVHRESULT_R + $0082 ;
  DVERR_INVALIDPARAM              = E_INVALIDARG;
  DVERR_INVALIDPLAYER             = MAKE_DVHRESULT_R + $0087 ;
  DVERR_INVALIDGROUP              = MAKE_DVHRESULT_R + $0091 ;
  DVERR_INVALIDHANDLE             = MAKE_DVHRESULT_R + $0096 ;
  DVERR_OUTOFMEMORY               = E_OUTOFMEMORY;
  DVERR_PENDING                   = DV_PENDING;
  DVERR_NOTSUPPORTED              = E_NOTIMPL;
  DVERR_NOINTERFACE               = E_NOINTERFACE;
  DVERR_SESSIONLOST               = MAKE_DVHRESULT_R + $012C ;
  DVERR_NOVOICESESSION            = MAKE_DVHRESULT_R + $012E ;
  DVERR_CONNECTIONLOST            = MAKE_DVHRESULT_R + $0168 ;
  DVERR_NOTINITIALIZED            = MAKE_DVHRESULT_R + $0169 ;
  DVERR_CONNECTED                 = MAKE_DVHRESULT_R + $016A ;
  DVERR_NOTCONNECTED              = MAKE_DVHRESULT_R + $016B ;
  DVERR_CONNECTABORTING           = MAKE_DVHRESULT_R + $016E ;
  DVERR_NOTALLOWED                = MAKE_DVHRESULT_R + $016F ;
  DVERR_INVALIDTARGET             = MAKE_DVHRESULT_R + $0170 ;
  DVERR_TRANSPORTNOTHOST          = MAKE_DVHRESULT_R + $0171 ;
  DVERR_COMPRESSIONNOTSUPPORTED   = MAKE_DVHRESULT_R + $0172 ;
  DVERR_ALREADYPENDING            = MAKE_DVHRESULT_R + $0173 ;
  DVERR_SOUNDINITFAILURE          = MAKE_DVHRESULT_R + $0174 ;
  DVERR_TIMEOUT                   = MAKE_DVHRESULT_R + $0175 ;
  DVERR_CONNECTABORTED            = MAKE_DVHRESULT_R + $0176 ;
  DVERR_NO3DSOUND                 = MAKE_DVHRESULT_R + $0177 ;
  DVERR_ALREADYBUFFERED	          = MAKE_DVHRESULT_R + $0178 ;
  DVERR_NOTBUFFERED               = MAKE_DVHRESULT_R + $0179 ;
  DVERR_HOSTING                   = MAKE_DVHRESULT_R + $017A ;
  DVERR_NOTHOSTING                = MAKE_DVHRESULT_R + $017B ;
  DVERR_INVALIDDEVICE             = MAKE_DVHRESULT_R + $017C ;
  DVERR_RECORDSYSTEMERROR         = MAKE_DVHRESULT_R + $017D ;
  DVERR_PLAYBACKSYSTEMERROR       = MAKE_DVHRESULT_R + $017E ;
  DVERR_SENDERROR                 = MAKE_DVHRESULT_R + $017F ;
  DVERR_USERCANCEL                = MAKE_DVHRESULT_R + $0180 ;
  DVERR_RUNSETUP                  = MAKE_DVHRESULT_R + $0183 ;
  DVERR_INCOMPATIBLEVERSION       = MAKE_DVHRESULT_R + $0184 ;
  DVERR_INITIALIZED               = MAKE_DVHRESULT_R + $0187 ;
  DVERR_INVALIDPOINTER            = E_POINTER;
  DVERR_NOTRANSPORT               = MAKE_DVHRESULT_R + $0188 ;
  DVERR_NOCALLBACK                = MAKE_DVHRESULT_R + $0189 ;
  DVERR_TRANSPORTNOTINIT          = MAKE_DVHRESULT_R + $018A ;
  DVERR_TRANSPORTNOSESSION        = MAKE_DVHRESULT_R + $018B ;
  DVERR_TRANSPORTNOPLAYER         = MAKE_DVHRESULT_R + $018C ;
  DVERR_USERBACK                  = MAKE_DVHRESULT_R + $018D ;
  DVERR_NORECVOLAVAILABLE         = MAKE_DVHRESULT_R + $018E ;
  DVERR_INVALIDBUFFER             = MAKE_DVHRESULT_R + $018F ;
  DVERR_LOCKEDBUFFER              = MAKE_DVHRESULT_R + $0190 ;



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// Compatibility types for non-Borland compliant translations of DirectPlay8 //
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

type
  PDvmsg_Connectresult = PDVMsgConnectResult;
  TDvmsg_Connectresult = TDVMsgConnectResult;
  PDvmsg_Createvoiceplayer = PDVMsgCreateVoicePlayer;
  TDvmsg_Createvoiceplayer = TDVMsgCreateVoicePlayer;
  PDvmsg_Deletevoiceplayer = PDVMsgDeleteVoicePlayer;
  TDvmsg_Deletevoiceplayer = TDVMsgDeleteVoicePlayer;
  PDvmsg_Disconnectresult = PDVMsgDisconnectResult;
  TDvmsg_Disconnectresult = TDVMsgDisconnectResult;
  PDvmsg_Hostmigrated = PDVMsgHostMigrated;
  TDvmsg_Hostmigrated = TDVMsgHostMigrated;
  PDvmsg_Inputlevel = PDVMsgInputlevel;
  TDvmsg_Inputlevel = TDVMsgInputlevel;
  PDvmsg_Localhostsetup = PDVMsgLocalHostSetup;
  TDvmsg_Localhostsetup = TDVMsgLocalHostSetup;
  PDvmsg_Outputlevel = PDVMsgOutputLevel;
  TDvmsg_Outputlevel = TDVMsgOutputLevel;
  PDvmsg_Playeroutputlevel = PDVMsgPlayerOutputLevel;
  TDvmsg_Playeroutputlevel = TDVMsgPlayerOutputLevel;
  PDvmsg_Playervoicestart = PDVMsgPlayerVoiceStart;
  TDvmsg_Playervoicestart = TDVMsgPlayerVoiceStart;
  PDvmsg_Playervoicestop = PDVMsgPlayerVoiceStop;
  TDvmsg_Playervoicestop = TDVMsgPlayerVoiceStop;
  PDvmsg_Recordstart = PDVMsgRecordStart;
  TDvmsg_Recordstart = TDVMsgRecordStart;
  PDvmsg_Recordstop = PDVMsgRecordStop;
  TDvmsg_Recordstop = TDVMsgRecordStop;
  PDvmsg_Sessionlost = PDVMsgSessionLost;
  TDvmsg_Sessionlost = TDVMsgSessionLost;
  PDvmsg_Settargets = PDVMsgSetTargets;
  TDvmsg_Settargets = TDVMsgSetTargets;

  PDPN_Application_Desc = PDPNApplicationDesc;
  TDPN_Application_Desc = TDPNApplicationDesc;
  PDPN_Caps = PDPNCaps;
  TDPN_Caps = TDPNCaps;
  PDPN_Caps_Ex = PDPNCapsEx;
  TDPN_Caps_Ex = TDPNCapsEx;
  PDPN_Connection_Info = PDPNConnectionInfo;
  TDPN_Connection_Info = TDPNConnectionInfo;
  PDPN_Group_Info = PDPNGroupInfo;
  TDPN_Group_Info = TDPNGroupInfo;
  PDPN_Player_Info = PDPNPlayerInfo;
  TDPN_Player_Info = TDPNPlayerInfo;
  PDPN_Security_Credentials = PDPNSecurityCredentials;
  TDPN_Security_Credentials = TDPNSecurityCredentials;
  PDPN_Security_Desc = PDPNSecurityDesc;
  TDPN_Security_Desc = TDPNSecurityDesc;
  PDPN_Service_Provider_Info = PDPNServiceProviderInfo;
  TDPN_Service_Provider_Info = TDPNServiceProviderInfo;
  PDPN_Sp_Caps = PDPNSpCaps;
  TDPN_Sp_Caps = TDPNSpCaps;
  PDPNMsg_Add_Player_To_Group = PDPNMsgAddPlayerToGroup;
  TDPNMsg_Add_Player_To_Group = TDPNMsgAddPlayerToGroup;
  PDPNMsg_Async_Op_Complete = PDPNMsgAsyncOpComplete;
  TDPNMsg_Async_Op_Complete = TDPNMsgAsyncOpComplete;
  PDPNMsg_Client_Info = PDPNMsgClientInfo;
  TDPNMsg_Client_Info = TDPNMsgClientInfo;
  PDPNMsg_Connect_Complete = PDPNMsgConnectComplete;
  TDPNMsg_Connect_Complete = TDPNMsgConnectComplete;
  PDPNMsg_Create_Group = PDPNMsgCreateGroup;
  TDPNMsg_Create_Group = TDPNMsgCreateGroup;
  PDPNMsg_Create_Player = PDPNMsgCreatePlayer;
  TDPNMsg_Create_Player = TDPNMsgCreatePlayer;
  PDPNMsg_Destroy_Group = PDPNMsgDestroyGroup;
  TDPNMsg_Destroy_Group = TDPNMsgDestroyGroup;
  PDPNMsg_Destroy_Player = PDPNMsgDestroyPlayer;
  TDPNMsg_Destroy_Player = TDPNMsgDestroyPlayer;
  PDPNMsg_Enum_Hosts_Query = PDPNMsgEnumHostsQuery;
  TDPNMsg_Enum_Hosts_Query = TDPNMsgEnumHostsQuery;
  PDPNMsg_Enum_Hosts_Response = PDPNMsgEnumHostsResponse;
  TDPNMsg_Enum_Hosts_Response = TDPNMsgEnumHostsResponse;
  PDPNMsg_Group_Info = PDPNMsgGroupInfo;
  TDPNMsg_Group_Info = TDPNMsgGroupInfo;
  PDPNMsg_Host_Migrate = PDPNMsgHostMigrate;
  TDPNMsg_Host_Migrate = TDPNMsgHostMigrate;
  PDPNMsg_Indicate_Connect = PDPNMsgIndicateConnect;
  TDPNMsg_Indicate_Connect = TDPNMsgIndicateConnect;
  PDPNMsg_Indicated_Connect_Aborted = PDPNMsgIndicatedConnectAborted;
  TDPNMsg_Indicated_Connect_Aborted = TDPNMsgIndicatedConnectAborted;
  PDPNMsg_Peer_Info = PDPNMsgPeerInfo;
  TDPNMsg_Peer_Info = TDPNMsgPeerInfo;
  PDPNMsg_Receive = PDPNMsgReceive;
  TDPNMsg_Receive = TDPNMsgReceive;
  PDPNMsg_Remove_Player_From_Group = PDPNMsgRemovePlayerFromGroup;
  TDPNMsg_Remove_Player_From_Group = TDPNMsgRemovePlayerFromGroup;
  PDPNMsg_Return_Buffer = PDPNMsgReturnBuffer;
  TDPNMsg_Return_Buffer = TDPNMsgReturnBuffer;
  PDPNMsg_Send_Complete = PDPNMsgSendComplete;
  TDPNMsg_Send_Complete = TDPNMsgSendComplete;
  PDPNMsg_Server_Info = PDPNMsgServerInfo;
  TDPNMsg_Server_Info = TDPNMsgServerInfo;
  PDPNMsg_Terminate_Session = PDPNMsgTerminateSession;
  TDPNMsg_Terminate_Session = TDPNMsgTerminateSession;
  PDPNMsg_Create_Thread = PDPNMsgCreateThread;
  TDPNMsg_Create_Thread = TDPNMsgCreateThread;
  PDPNMsg_Destroy_Thread = PDPNMsgDestroyThread;
  TDPNMsg_Destroy_Thread = TDPNMsgDestroyThread;
  PDPNMsg_Nat_Resolver_Query = PDPNMsgNatResolverQuery;
  TDPNMsg_Nat_Resolver_Query = TDPNMsgNatResolverQuery;


implementation

// #define MAKE_DPNHRESULT( code )  MAKE_HRESULT( 1, _DPN_FACILITY_CODE, ( code + _DPNHRESULT_BASE ) )
function MAKE_DPNHRESULT(Code: DWORD): DWORD;
begin
  Result:= DWord((1 shl 31) or (_DPN_FACILITY_CODE shl 16)) or (Code + _DPNHRESULT_BASE);
end;

end.
