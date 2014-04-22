{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  Files:      XInput.h                                                      *}
{*  Content:    This module defines XBOX controller APIs                      *}
{*              and constansts for the Windows platform.                      *}
{*                                                                            *}
{*  DirectX 9.0 Delphi / FreePascal adaptation by Alexey Barkovoy             *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://www.clootie.ru                                                   *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*----------------------------------------------------------------------------*}
{*  $Id: XInput.pas,v 1.3 2006/10/21 21:30:10 clootie Exp $ }
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

unit XInput;

interface


uses
  Windows;

const
  // Current name of the DLL shipped in the same SDK as this header.
  // The name reflects the current version
  XINPUT_DLL_A  = 'xinput1_3.dll';
  XINPUT_DLL_W  = 'xinput1_3.dll';
  {$IFDEF UNICODE}
  XINPUT_DLL = XINPUT_DLL_W;
  {$ELSE}
  XINPUT_DLL = XINPUT_DLL_A;
  {$ENDIF}

  //
  // Device types available in XINPUT_CAPABILITIES
  //
  XINPUT_DEVTYPE_GAMEPAD          = $01;
  XINPUT_DEVSUBTYPE_WHEEL         = $02;
  XINPUT_DEVSUBTYPE_ARCADE_STICK  = $03;
  XINPUT_DEVSUBTYPE_FLIGHT_SICK   = $04;
  XINPUT_DEVSUBTYPE_DANCE_PAD     = $05;

  //
  // Device subtypes available in XINPUT_CAPABILITIES
  //
  XINPUT_DEVSUBTYPE_GAMEPAD       = $01;

  //
  // Flags for XINPUT_CAPABILITIES
  //
  XINPUT_CAPS_VOICE_SUPPORTED     = $0004;

  //
  // Constants for gamepad buttons
  //
  XINPUT_GAMEPAD_DPAD_UP          = $0001;
  XINPUT_GAMEPAD_DPAD_DOWN        = $0002;
  XINPUT_GAMEPAD_DPAD_LEFT        = $0004;
  XINPUT_GAMEPAD_DPAD_RIGHT       = $0008;
  XINPUT_GAMEPAD_START            = $0010;
  XINPUT_GAMEPAD_BACK             = $0020;
  XINPUT_GAMEPAD_LEFT_THUMB       = $0040;
  XINPUT_GAMEPAD_RIGHT_THUMB      = $0080;
  XINPUT_GAMEPAD_LEFT_SHOULDER    = $0100;
  XINPUT_GAMEPAD_RIGHT_SHOULDER   = $0200;
  XINPUT_GAMEPAD_A                = $1000;
  XINPUT_GAMEPAD_B                = $2000;
  XINPUT_GAMEPAD_X                = $4000;
  XINPUT_GAMEPAD_Y                = $8000;


  //
  // Gamepad thresholds
  //
  XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE  = 7849;
  XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE = 8689;
  XINPUT_GAMEPAD_TRIGGER_THRESHOLD    = 30;

  //
  // Flags to pass to XInputGetCapabilities
  //
  XINPUT_FLAG_GAMEPAD             = $00000001;

  //
  // Devices that support batteries
  //
  BATTERY_DEVTYPE_GAMEPAD         = $00;
  BATTERY_DEVTYPE_HEADSET         = $01;

  //
  // Flags for battery status level
  //
  BATTERY_TYPE_DISCONNECTED       = $00;    // This device is not connected
  BATTERY_TYPE_WIRED              = $01;    // Wired device, no battery
  BATTERY_TYPE_ALKALINE           = $02;    // Alkaline battery source
  BATTERY_TYPE_NIMH               = $03;    // Nickel Metal Hydride battery source
  BATTERY_TYPE_UNKNOWN            = $FF;    // Cannot determine the battery type

  // These are only valid for wireless, connected devices, with known battery types
  // The amount of use time remaining depends on the type of device.
  BATTERY_LEVEL_EMPTY             = $00;
  BATTERY_LEVEL_LOW               = $01;
  BATTERY_LEVEL_MEDIUM            = $02;
  BATTERY_LEVEL_FULL              = $03;

  // User index definitions
  XUSER_MAX_COUNT                 = 4;

  XUSER_INDEX_ANY                 = $000000FF;


  //
  // Codes returned for the gamepad keystroke
  //

  VK_PAD_A                        = $5800;
  VK_PAD_B                        = $5801;
  VK_PAD_X                        = $5802;
  VK_PAD_Y                        = $5803;
  VK_PAD_RSHOULDER                = $5804;
  VK_PAD_LSHOULDER                = $5805;
  VK_PAD_LTRIGGER                 = $5806;
  VK_PAD_RTRIGGER                 = $5807;

  VK_PAD_DPAD_UP                  = $5810;
  VK_PAD_DPAD_DOWN                = $5811;
  VK_PAD_DPAD_LEFT                = $5812;
  VK_PAD_DPAD_RIGHT               = $5813;
  VK_PAD_START                    = $5814;
  VK_PAD_BACK                     = $5815;
  VK_PAD_LTHUMB_PRESS             = $5816;
  VK_PAD_RTHUMB_PRESS             = $5817;

  VK_PAD_LTHUMB_UP                = $5820;
  VK_PAD_LTHUMB_DOWN              = $5821;
  VK_PAD_LTHUMB_RIGHT             = $5822;
  VK_PAD_LTHUMB_LEFT              = $5823;
  VK_PAD_LTHUMB_UPLEFT            = $5824;
  VK_PAD_LTHUMB_UPRIGHT           = $5825;
  VK_PAD_LTHUMB_DOWNRIGHT         = $5826;
  VK_PAD_LTHUMB_DOWNLEFT          = $5827;

  VK_PAD_RTHUMB_UP                = $5830;
  VK_PAD_RTHUMB_DOWN              = $5831;
  VK_PAD_RTHUMB_RIGHT             = $5832;
  VK_PAD_RTHUMB_LEFT              = $5833;
  VK_PAD_RTHUMB_UPLEFT            = $5834;
  VK_PAD_RTHUMB_UPRIGHT           = $5835;
  VK_PAD_RTHUMB_DOWNRIGHT         = $5836;
  VK_PAD_RTHUMB_DOWNLEFT          = $5837;

  //
  // Flags used in XINPUT_KEYSTROKE
  //
  XINPUT_KEYSTROKE_KEYDOWN        = $0001;
  XINPUT_KEYSTROKE_KEYUP          = $0002;
  XINPUT_KEYSTROKE_REPEAT         = $0004;

type

  //
  // Structures used by XInput APIs
  //
  PXInputGamepad = ^TXInputGamepad;
  _XINPUT_GAMEPAD = record
    wButtons:         Word;
    bLeftTrigger:     Byte;
    bRightTrigger:    Byte;
    sThumbLX:         Smallint;
    sThumbLY:         Smallint;
    sThumbRX:         Smallint;
    sThumbRY:         Smallint;
  end;
  XINPUT_GAMEPAD = _XINPUT_GAMEPAD;
  TXInputGamepad = XINPUT_GAMEPAD;

  PXInputState = ^TXInputState;
  _XINPUT_STATE = record
    dwPacketNumber:   DWORD;
    Gamepad:          TXInputGamepad;
  end;
  XINPUT_STATE = _XINPUT_STATE;
  TXInputState = XINPUT_STATE;

  PXInputVibration = ^TXInputVibration;
  _XINPUT_VIBRATION = record
    wLeftMotorSpeed:  Word;
    wRightMotorSpeed: Word;
  end;
  XINPUT_VIBRATION = _XINPUT_VIBRATION;
  TXInputVibration = _XINPUT_VIBRATION;

  PXInputCapabilities = ^TXInputCapabilities;
  _XINPUT_CAPABILITIES = record
    _Type:            Byte;
    SubType:          Byte;
    Flags:            Word;
    Gamepad:          TXInputGamepad;
    Vibration:        TXInputVibration;
  end;
  XINPUT_CAPABILITIES = _XINPUT_CAPABILITIES;
  TXInputCapabilities = _XINPUT_CAPABILITIES;

  PXInputBatteryInformation = ^TXInputBatteryInformation;
  _XINPUT_BATTERY_INFORMATION = record
    BatteryType: Byte;
    BatteryLevel: Byte;
  end;
  XINPUT_BATTERY_INFORMATION = _XINPUT_BATTERY_INFORMATION;
  TXInputBatteryInformation = _XINPUT_BATTERY_INFORMATION;

  PXInputKeystroke = ^TXInputKeystroke;
  _XINPUT_KEYSTROKE = record
    VirtualKey: Word;
    Unicode: WideChar;
    Flags: Word;
    UserIndex: Byte;
    HidCode: Byte;
  end;
  XINPUT_KEYSTROKE = _XINPUT_KEYSTROKE;
  TXInputKeystroke = _XINPUT_KEYSTROKE;


//
// XInput APIs
//

function XInputGetState(
    dwUserIndex: DWORD;      // [in] Index of the gamer associated with the device
    out pState: TXInputState // [out] Receives the current state
 ): DWORD; stdcall; external XINPUT_DLL;

function XInputSetState(
    dwUserIndex: DWORD;                 // [in] Index of the gamer associated with the device
    const pVibration: TXInputVibration  // [in, out] The vibration information to send to the controller
 ): DWORD; stdcall; external XINPUT_DLL;

function XInputGetCapabilities(
    dwUserIndex: DWORD;                     // [in] Index of the gamer associated with the device
    dwFlags: DWORD;                         // [in] Input flags that identify the device type
    out pCapabilities: TXInputCapabilities  // [out] Receives the capabilities
 ): DWORD; stdcall; external XINPUT_DLL;

procedure XInputEnable(
    enable: BOOL     // [in] Indicates whether xinput is enabled or disabled.
); stdcall; external XINPUT_DLL;

function XInputGetDSoundAudioDeviceGuids(
    dwUserIndex: DWORD;           // [in] Index of the gamer associated with the device
    out pDSoundRenderGuid: TGUID; // [out] DSound device ID for render
    out pDSoundCaptureGuid: TGUID // [out] DSound device ID for capture
 ): DWORD; stdcall; external XINPUT_DLL;

function XInputGetBatteryInformation(
    dwUserIndex: DWORD;          // [in]  Index of the gamer associated with the device
    devType: Byte;               // [in]  Which device on this user index
    out pBatteryInformation: TXInputBatteryInformation // [out] Contains the level and types of batteries
 ): DWORD; stdcall; external XINPUT_DLL;

function XInputGetKeystroke(
    dwUserIndex: DWORD;               // [in]  Index of the gamer associated with the device
    dwReserved: DWORD;                // [in]  Reserved for future use
    var pKeystroke: TXInputKeystroke  // [out] Pointer to an XINPUT_KEYSTROKE structure that receives an input event.
 ): DWORD; stdcall; external XINPUT_DLL;


implementation

end.
