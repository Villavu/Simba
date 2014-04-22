{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  Files:      dxsdkver.h, extracts from various DirectX SDK include files   *}
{*  Content:    DirectX 9.0 headers common types                              *}
{*                                                                            *}
{*  DirectX 9.0 Delphi / FreePascal adaptation by Alexey Barkovoy             *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://www.clootie.ru                                                   *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*----------------------------------------------------------------------------*}
{*  $Id: DXTypes.pas,v 1.23 2007/04/14 20:57:43 clootie Exp $  }
{******************************************************************************}
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

unit DXTypes;

interface


uses Windows;

(*==========================================================================;
 *
 *  File:   dxsdkver.h
 *  Content:    DirectX SDK Version Include File
 *
 ****************************************************************************)
const
  _DXSDK_PRODUCT_MAJOR  = 9;
  _DXSDK_PRODUCT_MINOR  = 18;
  _DXSDK_BUILD_MAJOR    = 944;
  _DXSDK_BUILD_MINOR    = 0000;



(****************************************************************************
 *  Other files
 ****************************************************************************)
type
  // TD3DValue is the fundamental Direct3D fractional data type
  D3DVALUE = Single;
  TD3DValue = D3DVALUE;
  PD3DValue = ^TD3DValue;

  D3DCOLOR = DWord;
  TD3DColor = D3DCOLOR;
  PD3DColor = ^TD3DColor;

  _D3DVECTOR = packed record
    x: Single;
    y: Single;
    z: Single;
  end {_D3DVECTOR};
  D3DVECTOR = _D3DVECTOR;
  TD3DVector = _D3DVECTOR;
  PD3DVector = ^TD3DVector;

  REFERENCE_TIME = LONGLONG;
  TReferenceTime = REFERENCE_TIME;
  PReferenceTime = ^TReferenceTime;


// ==================================================================
// Here comes generic Windows types for Win32 / Win64 compatibility
//


  //
  // The INT_PTR is guaranteed to be the same size as a pointer.  Its
  // size with change with pointer size (32/64).  It should be used
  // anywhere that a pointer is cast to an integer type. UINT_PTR is
  // the unsigned variation.
  //
  {$IFDEF WIN64}
  INT_PTR = Int64;
  UINT_PTR = UInt64;
  LONG_PTR = Int64;
  ULONG_PTR = UInt64;
  DWORD_PTR = UInt64;
  {$ELSE}
  INT_PTR = Longint;
  UINT_PTR = LongWord;
  LONG_PTR = Longint;
  ULONG_PTR = LongWord;
  DWORD_PTR = LongWord;
  {$ENDIF}
  PINT_PTR = ^INT_PTR;
  PUINT_PTR = ^UINT_PTR;
  PLONG_PTR = ^LONG_PTR;
  PULONG_PTR = ^ULONG_PTR;


  //
  // SIZE_T used for counts or ranges which need to span the range of
  // of a pointer.  SSIZE_T is the signed variation.
  //
  SIZE_T = ULONG_PTR;
  SSIZE_T = LONG_PTR;
  PSIZE_T = ^SIZE_T;
  PSSIZE_T = ^SSIZE_T;

implementation

end.

