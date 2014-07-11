{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) 1995-1997 Microsoft Corporation.  All Rights Reserved.      *}
{*                                                                            *}
{*  Files:      dsetup.h                                                      *}
{*  Content:    DirectXSetup, error codes and flags                           *}
{*                                                                            *}
{*  DirectX 9.0 Delphi / FreePascal adaptation by Alexey Barkovoy             *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://www.clootie.ru                                                   *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*----------------------------------------------------------------------------*}
{*  $Id: DirectSetup.pas,v 1.1 2005/10/10 21:11:07 clootie Exp $ }
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

unit DirectSetup;

interface


uses
  Windows;

////////////////////////////////////////////////////////////////////////
// Global level dynamic loading support
{$IFDEF DYNAMIC_LINK_ALL}
  {$DEFINE DIRECTSETUP_DYNAMIC_LINK}
{$ENDIF}
{$IFDEF DYNAMIC_LINK_EXPLICIT_ALL}
  {$DEFINE DIRECTSETUP_DYNAMIC_LINK_EXPLICIT}
{$ENDIF}

// Remove "dots" below to force some kind of dynamic linking
{.$DEFINE DIRECTSETUP_DYNAMIC_LINK}
{.$DEFINE DIRECTSETUP_DYNAMIC_LINK_EXPLICIT}


const
  // FOURCC_VERS                 = mmioFOURCC('v','e','r','s')
  FOURCC_VERS                 = Byte('v') or (Byte('e') shl 8) or (Byte('r') shl 16) or (Byte('s') shl 24);

  // DSETUP Error Codes, must remain compatible with previous setup.
  DSETUPERR_SUCCESS_RESTART        = 1;
  DSETUPERR_SUCCESS                = 0;
  DSETUPERR_BADWINDOWSVERSION     = -1;
  DSETUPERR_SOURCEFILENOTFOUND    = -2;
  DSETUPERR_NOCOPY                = -5;
  DSETUPERR_OUTOFDISKSPACE        = -6;
  DSETUPERR_CANTFINDINF           = -7;
  DSETUPERR_CANTFINDDIR           = -8;
  DSETUPERR_INTERNAL              = -9;
  DSETUPERR_UNKNOWNOS             = -11;
  DSETUPERR_NEWERVERSION          = -14;
  DSETUPERR_NOTADMIN              = -15;
  DSETUPERR_UNSUPPORTEDPROCESSOR  = -16;
  DSETUPERR_MISSINGCAB_MANAGEDDX  = -17;
  DSETUPERR_NODOTNETFRAMEWORKINSTALLED = -18;
  DSETUPERR_CABDOWNLOADFAIL       = -19;
  DSETUPERR_DXCOMPONENTFILEINUSE  = -20;
  DSETUPERR_UNTRUSTEDCABINETFILE  = -21;

  // DSETUP flags. DirectX 5.0 apps should use these flags only.
  DSETUP_DDRAWDRV         = $00000008;      (* install DirectDraw Drivers           *)
  DSETUP_DSOUNDDRV        = $00000010;      (* install DirectSound Drivers          *)
  DSETUP_DXCORE           = $00010000;      (* install DirectX runtime              *)
  DSETUP_DIRECTX  = (DSETUP_DXCORE or DSETUP_DDRAWDRV or DSETUP_DSOUNDDRV);
  DSETUP_MANAGEDDX        = $00004000;      (* OBSOLETE. install managed DirectX    *)
  DSETUP_TESTINSTALL      = $00020000;      (* just test install, don't do anything *)

  // These OBSOLETE flags are here for compatibility with pre-DX5 apps only.
  // They are present to allow DX3 apps to be recompiled with DX5 and still work.
  // DO NOT USE THEM for DX5. They will go away in future DX releases.
  DSETUP_DDRAW            = $00000001;      (* OBSOLETE. install DirectDraw           *)
  DSETUP_DSOUND           = $00000002;      (* OBSOLETE. install DirectSound          *)
  DSETUP_DPLAY            = $00000004;      (* OBSOLETE. install DirectPlay           *)
  DSETUP_DPLAYSP          = $00000020;      (* OBSOLETE. install DirectPlay Providers *)
  DSETUP_DVIDEO           = $00000040;      (* OBSOLETE. install DirectVideo          *)
  DSETUP_D3D              = $00000200;      (* OBSOLETE. install Direct3D             *)
  DSETUP_DINPUT           = $00000800;      (* OBSOLETE. install DirectInput          *)
  DSETUP_DIRECTXSETUP     = $00001000;      (* OBSOLETE. install DirectXSetup DLL's   *)
  DSETUP_NOUI             = $00002000;      (* OBSOLETE. install DirectX with NO UI   *)
  DSETUP_PROMPTFORDRIVERS = $10000000;      (* OBSOLETE. prompt when replacing display/audio drivers *)
  DSETUP_RESTOREDRIVERS   = $20000000;      (* OBSOLETE. restore display/audio drivers *)


  //******************************************************************
  // DirectX Setup Callback mechanism
  //******************************************************************

  // DSETUP Message Info Codes, passed to callback as Reason parameter.
  DSETUP_CB_MSG_NOMESSAGE                     = 0;
  DSETUP_CB_MSG_INTERNAL_ERROR                = 10;
  DSETUP_CB_MSG_BEGIN_INSTALL                 = 13;
  DSETUP_CB_MSG_BEGIN_INSTALL_RUNTIME         = 14;
  DSETUP_CB_MSG_PROGRESS                      = 18;
  DSETUP_CB_MSG_WARNING_DISABLED_COMPONENT    = 19;


type
  PDSetupCBProgress = ^TDSetupCBProgress;
  _DSETUP_CB_PROGRESS = record
    dwPhase: DWORD;
    dwInPhaseMaximum: DWORD;
    dwInPhaseProgress: DWORD;
    dwOverallMaximum: DWORD;
    dwOverallProgress: DWORD;
  end;
  DSETUP_CB_PROGRESS = _DSETUP_CB_PROGRESS;
  TDSetupCBProgress = _DSETUP_CB_PROGRESS;


  _DSETUP_CB_PROGRESS_PHASE = (
    DSETUP_INITIALIZING,
    DSETUP_EXTRACTING,
    DSETUP_COPYING,
    DSETUP_FINALIZING
  );
  TDSetupCBProgressPhase = _DSETUP_CB_PROGRESS_PHASE;


  //
  // Data Structures
  //

  PDirectXRegisterAppA = ^TDirectXRegisterAppA;
  _DIRECTXREGISTERAPPA = record
    dwSize:               DWORD;
    dwFlags:              DWORD;
    lpszApplicationName:  PAnsiChar;
    lpGUID:               PGUID;
    lpszFilename:         PAnsiChar;
    lpszCommandLine:      PAnsiChar;
    lpszPath:             PAnsiChar;
    lpszCurrentDirectory: PAnsiChar;
  end;
  DIRECTXREGISTERAPPA = _DIRECTXREGISTERAPPA;
  TDirectXRegisterAppA = _DIRECTXREGISTERAPPA;

  PDirectXRegisterApp2A = ^TDirectXRegisterApp2A;
  _DIRECTXREGISTERAPP2A = record
    dwSize:               DWORD;
    dwFlags:              DWORD;
    lpszApplicationName:  PAnsiChar;
    lpGUID:               PGUID;
    lpszFilename:         PAnsiChar;
    lpszCommandLine:      PAnsiChar;
    lpszPath:             PAnsiChar;
    lpszCurrentDirectory: PAnsiChar;
    lpszLauncherName:     PAnsiChar;
  end;
  DIRECTXREGISTERAPP2A = _DIRECTXREGISTERAPP2A;
  TDirectXRegisterApp2A = _DIRECTXREGISTERAPP2A;

  PDirectXRegisterAppW = ^TDirectXRegisterAppW;
  _DIRECTXREGISTERAPPW = record
    dwSize:               DWORD;
    dwFlags:              DWORD;
    lpszApplicationName:  PWideChar;
    lpGUID:               PGUID;
    lpszFilename:         PWideChar;
    lpszCommandLine:      PWideChar;
    lpszPath:             PWideChar;
    lpszCurrentDirectory: PWideChar;
  end;
  DIRECTXREGISTERAPPW = _DIRECTXREGISTERAPPW;
  TDirectXRegisterAppW = _DIRECTXREGISTERAPPW;

  PDirectXRegisterApp2W = ^TDirectXRegisterApp2W;
  _DIRECTXREGISTERAPP2W = record
    dwSize:               DWORD;
    dwFlags:              DWORD;
    lpszApplicationName:  PWideChar;
    lpGUID:               PGUID;
    lpszFilename:         PWideChar;
    lpszCommandLine:      PWideChar;
    lpszPath:             PWideChar;
    lpszCurrentDirectory: PWideChar;
    lpszLauncherName:     PWideChar;
  end;
  DIRECTXREGISTERAPP2W = _DIRECTXREGISTERAPP2W;
  TDirectXRegisterApp2W = _DIRECTXREGISTERAPP2W;

  
  PDirectXRegisterApp = ^TDirectXRegisterApp;
  PDirectXRegisterApp2 = ^TDirectXRegisterApp2;
{$IFDEF UNICODE}
  TDirectXRegisterApp = TDirectXRegisterAppW;
  TDirectXRegisterApp2 = TDirectXRegisterAppW2;
{$ELSE}
  TDirectXRegisterApp = TDirectXRegisterAppA;
  TDirectXRegisterApp2 = TDirectXRegisterApp2A;
{$ENDIF}


//
// API
//

var
  DirectXSetupW: function (hWnd: HWND; lpszRootPath: PWideChar; dwFlags: DWORD): Integer; stdcall;
  DirectXSetupA: function (hWnd: HWND; lpszRootPath: PAnsiChar; dwFlags: DWORD): Integer; stdcall;
  DirectXSetup: function (hWnd: HWND; lpszRootPath: PChar; dwFlags: DWORD): Integer; stdcall;

  DirectXRegisterApplicationW: function (hWnd: HWND; const lpDXRegApp: TDirectXRegisterAppW): Integer; stdcall;
  DirectXRegisterApplicationA: function (hWnd: HWND; const lpDXRegApp: TDirectXRegisterAppA): Integer; stdcall;
  DirectXRegisterApplication: function (hWnd: HWND; const lpDXRegApp: TDirectXRegisterApp): Integer; stdcall;

  DirectXUnRegisterApplication: function (hWnd: HWND; const lpGUID: TGUID): Integer; stdcall;

type
  TDSetupCallback = function (Reason: DWORD; MsgType: DWORD; (* Same as flags to MessageBox *)
      szMessage: PChar; szName: PChar; pInfo: Pointer): DWORD; stdcall;

var
  DirectXSetupSetCallback: function (Callback: TDSetupCallback): Integer; stdcall;
  DirectXSetupGetVersion: function (out lpdwVersion, lpdwMinorVersion: DWORD): Integer; stdcall;
  DirectXSetupShowEULA: function(hWndParent: HWND): Integer; stdcall;

  DirectXSetupGetEULAA: function(lpszEULA: PAnsiChar; cchEULA: LongWord; LangID: Word): LongWord; stdcall;
  DirectXSetupGetEULAW: function(lpszEULA: PWideChar; cchEULA: LongWord; LangID: Word): LongWord; stdcall;
  DirectXSetupGetEULA: function(lpszEULA: PChar; cchEULA: LongWord; LangID: Word): LongWord; stdcall;

function DirectSetupLoaded: Boolean;
function UnLoadDirectSetup: Boolean;
function LoadDirectSetup: Boolean;


implementation

const
  DirectSetupDll = 'dsetup.dll';

var
  DirectSetupLib: THandle = 0;

function DirectSetupLoaded: Boolean;
begin
  Result:= (DirectSetupLib <> 0);
end;

function UnLoadDirectSetup: Boolean;
begin
  Result:= True;
  if (DirectSetupLib <> 0) then
  begin
    Result:= Result and FreeLibrary(DirectSetupLib);

    DirectXSetupA := nil;
    DirectXSetupW := nil;
    DirectXSetup := nil;

    DirectXRegisterApplicationA := nil;
    DirectXRegisterApplicationW := nil;
    DirectXRegisterApplication := nil;

    DirectXUnRegisterApplication := nil;

    DirectXSetupSetCallback := nil;
    DirectXSetupGetVersion := nil;
    DirectXSetupShowEULA := nil;

    DirectXSetupGetEULAA := nil;
    DirectXSetupGetEULAW := nil;
    DirectXSetupGetEULA := nil;

    DirectSetupLib:= 0;
  end;
end;

function LoadDirectSetup: Boolean;
begin
  Result:= DirectSetupLoaded;
  if (not Result) then
  begin
    DirectSetupLib:= LoadLibrary(DirectSetupDll);
    if (DirectSetupLib <> 0) then
    begin
      DirectXSetupA := GetProcAddress(DirectSetupLib, 'DirectXSetupA');
      DirectXSetupW := GetProcAddress(DirectSetupLib, 'DirectXSetupW');
      {$IFDEF UNICODE}
      DirectXSetup := DirectXSetupW;
      {$ELSE}
      DirectXSetup := DirectXSetupA;
      {$ENDIF}

      DirectXRegisterApplicationA := GetProcAddress(DirectSetupLib, 'DirectXRegisterApplicationA');
      DirectXRegisterApplicationW := GetProcAddress(DirectSetupLib, 'DirectXRegisterApplicationW');
      {$IFDEF UNICODE}
      DirectXRegisterApplication := DirectXRegisterApplicationW;
      {$ELSE}
      DirectXRegisterApplication := DirectXRegisterApplicationA;
      {$ENDIF}

      DirectXUnRegisterApplication := GetProcAddress(DirectSetupLib, 'DirectXUnRegisterApplication');

      DirectXSetupSetCallback := GetProcAddress(DirectSetupLib, 'DirectXSetupSetCallback');
      DirectXSetupGetVersion := GetProcAddress(DirectSetupLib, 'DirectXSetupGetVersion');
      DirectXSetupShowEULA := GetProcAddress(DirectSetupLib, 'DirectXSetupShowEULA');;


      DirectXSetupGetEULAA := GetProcAddress(DirectSetupLib, 'DirectXSetupGetEULAA');;
      DirectXSetupGetEULAW := GetProcAddress(DirectSetupLib, 'DirectXSetupGetEULAW');;
      {$IFDEF UNICODE}
      DirectXSetupGetEULA := DirectXSetupGetEULAW;
      {$ELSE}
      DirectXSetupGetEULA := DirectXSetupGetEULAA;
      {$ENDIF}
    end;

    // At least basic procedure is found!
    Result:= Assigned(DirectXSetup);
    if not Result then UnLoadDirectSetup;
  end;
end;

initialization
{$IFNDEF DIRECTSETUP_DYNAMIC_LINK_EXPLICIT}
  LoadDirectSetup;
{$ENDIF}
finalization
  UnLoadDirectSetup;
end.

