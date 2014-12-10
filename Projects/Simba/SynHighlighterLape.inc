(******************************************************************************)
(* SynEdit Include File.  This file was adapted from Brad Stowers' DFS.INC    *)
(* file and used with permission.  This will help reduce headaches when new   *)
(* versions of Delphi and C++Builder are released, among other things.        *)
(******************************************************************************)
(* Brad Stowers: bstowers@pobox.com                                           *)
(* Delphi Free Stuff: http://delphifreestuff.com/                             *)
(* February 24, 1999                                                          *)
(******************************************************************************)
(*                                                                            *)
(* Complete Boolean Evaluation compiler directive is turned off by including  *)
(*   this file.                                                               *)
(* The $ObjExportAll directive is turned on if compiling with C++Builder 3 or *)
(*   higher.  This is required for Delphi components built in Builder with    *)
(*   run-time packages.                                                       *)
(*                                                                            *)
(* Here is a brief explanation of what each of the defines mean:              *)
(* SYN_WIN32            : Compilation target is 32-bit Windows                *)
(* SYN_COMPILER_2       : Delphi 2.0x or C++B 1.0x is the compiler.           *)
(* SYN_COMPILER_2_UP    : Delphi 2.0x or higher, or C++B 1.0x or higher is the*)
(*                        compiler.                                           *)
(* SYN_COMPILER_3       : Delphi 3.0x or C++B 3.0x is the compiler.           *)
(* SYN_COMPILER_3_UP    : Delphi 3.0x or higher, or C++B 3.0x or higher is the*)
(*                        compiler.                                           *)
(* SYN_COMPILER_4       : Delphi 4.0x or C++B 4.0x is the compiler.           *)
(* SYN_COMPILER_4_UP    : Delphi 4.0x or higher, or C++B 4.0x or higher is    *)
(*                        the compiler.                                       *)
(* SYN_COMPILER_5       : Delphi 5.0x or C++B 5.0x is the compiler.           *)
(* SYN_COMPILER_5_UP    : Delphi 5.0x or higher, or C++B 5.0x or higher is    *)
(*                        the compiler.                                       *)
(* SYN_CPPB             : Any version of C++B is being used.                  *)
(* SYN_CPPB_1           : C++B v1.0x is being used.                           *)
(* SYN_CPPB_3           : C++B v3.0x is being used.                           *)
(* SYN_CPPB_3_UP        : C++B v3.0x or higher is being used.                 *)
(* SYN_CPPB_4           : C++B v4.0x is being used.                           *)
(* SYN_CPPB_4_UP        : C++B v4.0x or higher is being used.                 *)
(* SYN_CPPB_5           : C++B v5.0x is being used.                           *)
(* SYN_CPPB_5_UP        : C++B v5.0x or higher is being used.                 *)
(* SYN_DELPHI           : Any version of Delphi is being used.                *)
(* SYN_DELPHI_2         : Delphi v2.0x is being used.                         *)
(* SYN_DELPHI_2_UP      : Delphi v2.0x or higher is being used.               *)
(* SYN_DELPHI_3         : Delphi v3.0x is being used.                         *)
(* SYN_DELPHI_3_UP      : Delphi v3.0x or higher is being used.               *)
(* SYN_DELPHI_4         : Delphi v4.0x is being used.                         *)
(* SYN_DELPHI_4_UP      : Delphi v4.0x or higher is being used.               *)
(* SYN_DELPHI_5         : Delphi v5.0x is being used.                         *)
(* SYN_DELPHI_5_UP      : Delphi v5.0x or higher is being used.               *)
(******************************************************************************)

{$IFDEF FPC}
  {$MODE OBJFPC}
{$ENDIF}

{$DEFINE SYNEDIT_INCLUDE}

{$IFdef MSWindows}
  {$DEFINE SYN_WIN32}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SYN_COMPILER_5}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_5}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SYN_COMPILER_4}
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SYN_COMPILER_4}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SYN_COMPILER_3}
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE SYN_COMPILER_3}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_3}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE SYN_COMPILER_2}  { C++B v1 compiler is really v2 }
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_1}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE SYN_COMPILER_2}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2}
{$ENDIF}

{$IFDEF SYN_COMPILER_2}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_3}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_4}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_5}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2}
  {$DEFINE SYN_DELPHI_2_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_3}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_4}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_5}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
{$ENDIF}

{$IFDEF SYN_CPPB_3}
  {$DEFINE SYN_CPPB_3_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
  {$DEFINE SYN_NO_COM_CLEANUP}
{$ENDIF}

{$IFDEF SYN_CPPB_3_UP}
  // C++Builder requires this if you use Delphi components in run-time packages.
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_LAZARUS}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}

// defaults are short evaluation of boolean values and long strings

// lazarus change   no $B-
{$H+}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}

// support for multibyte character sets
{$IFDEF SYN_COMPILER_3_UP}
{$IFNDEF SYN_LAZARUS}
{$DEFINE SYN_MBCSSUPPORT}
{$ENDIF}
{$ENDIF}

// additional tests for debugging

{.$DEFINE SYN_DEVELOPMENT_CHECKS}

{$IFDEF SYN_DEVELOPMENT_CHECKS}

{$R+,Q+,S+,T+}

{$ENDIF}

// $Id: synedit.inc 9222 2006-05-01 21:47:48Z micha $         