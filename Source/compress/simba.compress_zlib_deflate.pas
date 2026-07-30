{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
  --------------------------------------------------------------------------
  zlib by Jean-loup Gailly and Mark Adler https://github.com/madler/zlib

    HashAt            UPDATE_HASH       deflate.c
    LongestMatch      longest_match     deflate.c
    Run               deflate_slow      deflate.c
    HeapDown          pqdownheap        trees.c
    BuildTree         build_tree        trees.c
    ScanTree          scan_tree         trees.c
    SendTree          send_tree         trees.c
    SendCode          send_code         trees.c
    CompressSymbols   compress_block    trees.c
    FlushBlock        _tr_flush_block   trees.c
    InflateTable      inflate_table     inftrees.c
    TInflateState.Run inflate           inflate.c
}
unit simba.compress_zlib_deflate;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.compress_codec;

function DeflateBound(InSize: PtrUInt): PtrUInt;

function Deflate(InData: PByte; InSize: PtrUInt; OutData: PByte): PtrUInt;
procedure Inflate(InData: PByte; InSize: PtrUInt; var OutData: PByte; OutOffset: PtrUInt; out OutSize: Int64; out InUsed: PtrUInt);

implementation

{$R-}{$Q-}

const
  MAX_BITS    = 15;   // longest huffman code in a literal/length or distance tree
  MAX_CL_BITS = 7;    // longest code in the tree that describes the trees
  LIT_CODES   = 286;  // literal/length alphabet actually in use (0..285)
  DIST_CODES  = 30;
  CL_CODES    = 19;
  END_BLOCK   = 256;
  HEAP_SIZE   = 2 * LIT_CODES + 1;

  MIN_MATCH = 3;
  MAX_MATCH = 258;
  WSIZE     = 32768;
  WMASK     = WSIZE - 1;

  MIN_LOOKAHEAD = MAX_MATCH + MIN_MATCH + 1;
  MAX_DIST      = WSIZE - MIN_LOOKAHEAD;
  NIL_POS       = 0;   // zlib's NIL: position 0 is never a match source

  HASH_BITS  = 15;
  HASH_SIZE  = 1 shl HASH_BITS;
  HASH_MASK  = HASH_SIZE - 1;
  HASH_SHIFT = (HASH_BITS + MIN_MATCH - 1) div MIN_MATCH;

  // zlib's level 6 row of configuration_table
  GOOD_MATCH = 8;
  MAX_LAZY   = 16;
  NICE_MATCH = 128;
  MAX_CHAIN  = 128;
  TOO_FAR    = 4096;

  SYM_BUF    = 16384;
  SYM_END    = SYM_BUF - 1;  // zlib flushes here, one short of the buffer
  MAX_STORED = 65535;

  STORED_OVERHEAD = 8;
  BOUND_SLACK     = 64; // headroom over the worst case, which is never reached
  GROW_MIN        = 4096; // first size a growing output buffer takes

  CL_ORDER: array[0..CL_CODES-1] of Byte = (16,17,18,0,8,7,9,6,10,5,11,4,12,3,13,2,14,1,15);
  CL_EXTRA: array[0..CL_CODES-1] of Byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,3,7);

  LEN_BASE: array[0..28] of UInt16 = (3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258);
  LEN_EXTRA: array[0..28] of Byte = (0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0);

  DIST_BASE: array[0..29] of UInt16 = (1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577);
  DIST_EXTRA: array[0..29] of Byte = (0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13);

type
  TTreeNode = record
    Freq: UInt32;
    Code: UInt16;
    Len: UInt16;
    Dad: UInt16;
  end;

const
  // (length - MIN_MATCH) -> length code, and (distance - 1) -> distance code.
  // trees.h's _length_code and _dist_code.
  LenCodeTab: array[0..255] of Byte = (
      0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  9,  9, 10, 10, 11, 11,
     12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15,
     16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17,
     18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19,
     20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
     21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
     22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
     23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
     25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
     26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
     26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
     27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
     27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28
  );
  DistCodeTab: array[0..511] of Byte = (
      0,  1,  2,  3,  4,  4,  5,  5,  6,  6,  6,  6,  7,  7,  7,  7,
      8,  8,  8,  8,  8,  8,  8,  8,  9,  9,  9,  9,  9,  9,  9,  9,
     10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
     11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
     12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
     12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
     13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
     14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
     14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
     14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
     15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
     15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
     15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
     15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
      0,  0, 16, 17, 18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21,
     22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23,
     24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
     25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
     26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
     26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
     27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
     27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
     28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
     28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
     28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
     28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
     29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
     29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
     29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
     29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29
  );

  // the fixed trees of RFC 1951 3.2.6 - trees.h's static_ltree and static_dtree.
  // Freq and Dad are only there because a dynamic tree needs them.
  StaticLitLen: array[0..287] of Byte = (
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
      7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
      7,  7,  7,  7,  7,  7,  7,  7,  8,  8,  8,  8,  8,  8,  8,  8
  );
  StaticLitTree: array[0..287] of TTreeNode = (
    (Freq:0; Code: 12; Len:8; Dad:0), (Freq:0; Code:140; Len:8; Dad:0), (Freq:0; Code: 76; Len:8; Dad:0), (Freq:0; Code:204; Len:8; Dad:0),
    (Freq:0; Code: 44; Len:8; Dad:0), (Freq:0; Code:172; Len:8; Dad:0), (Freq:0; Code:108; Len:8; Dad:0), (Freq:0; Code:236; Len:8; Dad:0),
    (Freq:0; Code: 28; Len:8; Dad:0), (Freq:0; Code:156; Len:8; Dad:0), (Freq:0; Code: 92; Len:8; Dad:0), (Freq:0; Code:220; Len:8; Dad:0),
    (Freq:0; Code: 60; Len:8; Dad:0), (Freq:0; Code:188; Len:8; Dad:0), (Freq:0; Code:124; Len:8; Dad:0), (Freq:0; Code:252; Len:8; Dad:0),
    (Freq:0; Code:  2; Len:8; Dad:0), (Freq:0; Code:130; Len:8; Dad:0), (Freq:0; Code: 66; Len:8; Dad:0), (Freq:0; Code:194; Len:8; Dad:0),
    (Freq:0; Code: 34; Len:8; Dad:0), (Freq:0; Code:162; Len:8; Dad:0), (Freq:0; Code: 98; Len:8; Dad:0), (Freq:0; Code:226; Len:8; Dad:0),
    (Freq:0; Code: 18; Len:8; Dad:0), (Freq:0; Code:146; Len:8; Dad:0), (Freq:0; Code: 82; Len:8; Dad:0), (Freq:0; Code:210; Len:8; Dad:0),
    (Freq:0; Code: 50; Len:8; Dad:0), (Freq:0; Code:178; Len:8; Dad:0), (Freq:0; Code:114; Len:8; Dad:0), (Freq:0; Code:242; Len:8; Dad:0),
    (Freq:0; Code: 10; Len:8; Dad:0), (Freq:0; Code:138; Len:8; Dad:0), (Freq:0; Code: 74; Len:8; Dad:0), (Freq:0; Code:202; Len:8; Dad:0),
    (Freq:0; Code: 42; Len:8; Dad:0), (Freq:0; Code:170; Len:8; Dad:0), (Freq:0; Code:106; Len:8; Dad:0), (Freq:0; Code:234; Len:8; Dad:0),
    (Freq:0; Code: 26; Len:8; Dad:0), (Freq:0; Code:154; Len:8; Dad:0), (Freq:0; Code: 90; Len:8; Dad:0), (Freq:0; Code:218; Len:8; Dad:0),
    (Freq:0; Code: 58; Len:8; Dad:0), (Freq:0; Code:186; Len:8; Dad:0), (Freq:0; Code:122; Len:8; Dad:0), (Freq:0; Code:250; Len:8; Dad:0),
    (Freq:0; Code:  6; Len:8; Dad:0), (Freq:0; Code:134; Len:8; Dad:0), (Freq:0; Code: 70; Len:8; Dad:0), (Freq:0; Code:198; Len:8; Dad:0),
    (Freq:0; Code: 38; Len:8; Dad:0), (Freq:0; Code:166; Len:8; Dad:0), (Freq:0; Code:102; Len:8; Dad:0), (Freq:0; Code:230; Len:8; Dad:0),
    (Freq:0; Code: 22; Len:8; Dad:0), (Freq:0; Code:150; Len:8; Dad:0), (Freq:0; Code: 86; Len:8; Dad:0), (Freq:0; Code:214; Len:8; Dad:0),
    (Freq:0; Code: 54; Len:8; Dad:0), (Freq:0; Code:182; Len:8; Dad:0), (Freq:0; Code:118; Len:8; Dad:0), (Freq:0; Code:246; Len:8; Dad:0),
    (Freq:0; Code: 14; Len:8; Dad:0), (Freq:0; Code:142; Len:8; Dad:0), (Freq:0; Code: 78; Len:8; Dad:0), (Freq:0; Code:206; Len:8; Dad:0),
    (Freq:0; Code: 46; Len:8; Dad:0), (Freq:0; Code:174; Len:8; Dad:0), (Freq:0; Code:110; Len:8; Dad:0), (Freq:0; Code:238; Len:8; Dad:0),
    (Freq:0; Code: 30; Len:8; Dad:0), (Freq:0; Code:158; Len:8; Dad:0), (Freq:0; Code: 94; Len:8; Dad:0), (Freq:0; Code:222; Len:8; Dad:0),
    (Freq:0; Code: 62; Len:8; Dad:0), (Freq:0; Code:190; Len:8; Dad:0), (Freq:0; Code:126; Len:8; Dad:0), (Freq:0; Code:254; Len:8; Dad:0),
    (Freq:0; Code:  1; Len:8; Dad:0), (Freq:0; Code:129; Len:8; Dad:0), (Freq:0; Code: 65; Len:8; Dad:0), (Freq:0; Code:193; Len:8; Dad:0),
    (Freq:0; Code: 33; Len:8; Dad:0), (Freq:0; Code:161; Len:8; Dad:0), (Freq:0; Code: 97; Len:8; Dad:0), (Freq:0; Code:225; Len:8; Dad:0),
    (Freq:0; Code: 17; Len:8; Dad:0), (Freq:0; Code:145; Len:8; Dad:0), (Freq:0; Code: 81; Len:8; Dad:0), (Freq:0; Code:209; Len:8; Dad:0),
    (Freq:0; Code: 49; Len:8; Dad:0), (Freq:0; Code:177; Len:8; Dad:0), (Freq:0; Code:113; Len:8; Dad:0), (Freq:0; Code:241; Len:8; Dad:0),
    (Freq:0; Code:  9; Len:8; Dad:0), (Freq:0; Code:137; Len:8; Dad:0), (Freq:0; Code: 73; Len:8; Dad:0), (Freq:0; Code:201; Len:8; Dad:0),
    (Freq:0; Code: 41; Len:8; Dad:0), (Freq:0; Code:169; Len:8; Dad:0), (Freq:0; Code:105; Len:8; Dad:0), (Freq:0; Code:233; Len:8; Dad:0),
    (Freq:0; Code: 25; Len:8; Dad:0), (Freq:0; Code:153; Len:8; Dad:0), (Freq:0; Code: 89; Len:8; Dad:0), (Freq:0; Code:217; Len:8; Dad:0),
    (Freq:0; Code: 57; Len:8; Dad:0), (Freq:0; Code:185; Len:8; Dad:0), (Freq:0; Code:121; Len:8; Dad:0), (Freq:0; Code:249; Len:8; Dad:0),
    (Freq:0; Code:  5; Len:8; Dad:0), (Freq:0; Code:133; Len:8; Dad:0), (Freq:0; Code: 69; Len:8; Dad:0), (Freq:0; Code:197; Len:8; Dad:0),
    (Freq:0; Code: 37; Len:8; Dad:0), (Freq:0; Code:165; Len:8; Dad:0), (Freq:0; Code:101; Len:8; Dad:0), (Freq:0; Code:229; Len:8; Dad:0),
    (Freq:0; Code: 21; Len:8; Dad:0), (Freq:0; Code:149; Len:8; Dad:0), (Freq:0; Code: 85; Len:8; Dad:0), (Freq:0; Code:213; Len:8; Dad:0),
    (Freq:0; Code: 53; Len:8; Dad:0), (Freq:0; Code:181; Len:8; Dad:0), (Freq:0; Code:117; Len:8; Dad:0), (Freq:0; Code:245; Len:8; Dad:0),
    (Freq:0; Code: 13; Len:8; Dad:0), (Freq:0; Code:141; Len:8; Dad:0), (Freq:0; Code: 77; Len:8; Dad:0), (Freq:0; Code:205; Len:8; Dad:0),
    (Freq:0; Code: 45; Len:8; Dad:0), (Freq:0; Code:173; Len:8; Dad:0), (Freq:0; Code:109; Len:8; Dad:0), (Freq:0; Code:237; Len:8; Dad:0),
    (Freq:0; Code: 29; Len:8; Dad:0), (Freq:0; Code:157; Len:8; Dad:0), (Freq:0; Code: 93; Len:8; Dad:0), (Freq:0; Code:221; Len:8; Dad:0),
    (Freq:0; Code: 61; Len:8; Dad:0), (Freq:0; Code:189; Len:8; Dad:0), (Freq:0; Code:125; Len:8; Dad:0), (Freq:0; Code:253; Len:8; Dad:0),
    (Freq:0; Code: 19; Len:9; Dad:0), (Freq:0; Code:275; Len:9; Dad:0), (Freq:0; Code:147; Len:9; Dad:0), (Freq:0; Code:403; Len:9; Dad:0),
    (Freq:0; Code: 83; Len:9; Dad:0), (Freq:0; Code:339; Len:9; Dad:0), (Freq:0; Code:211; Len:9; Dad:0), (Freq:0; Code:467; Len:9; Dad:0),
    (Freq:0; Code: 51; Len:9; Dad:0), (Freq:0; Code:307; Len:9; Dad:0), (Freq:0; Code:179; Len:9; Dad:0), (Freq:0; Code:435; Len:9; Dad:0),
    (Freq:0; Code:115; Len:9; Dad:0), (Freq:0; Code:371; Len:9; Dad:0), (Freq:0; Code:243; Len:9; Dad:0), (Freq:0; Code:499; Len:9; Dad:0),
    (Freq:0; Code: 11; Len:9; Dad:0), (Freq:0; Code:267; Len:9; Dad:0), (Freq:0; Code:139; Len:9; Dad:0), (Freq:0; Code:395; Len:9; Dad:0),
    (Freq:0; Code: 75; Len:9; Dad:0), (Freq:0; Code:331; Len:9; Dad:0), (Freq:0; Code:203; Len:9; Dad:0), (Freq:0; Code:459; Len:9; Dad:0),
    (Freq:0; Code: 43; Len:9; Dad:0), (Freq:0; Code:299; Len:9; Dad:0), (Freq:0; Code:171; Len:9; Dad:0), (Freq:0; Code:427; Len:9; Dad:0),
    (Freq:0; Code:107; Len:9; Dad:0), (Freq:0; Code:363; Len:9; Dad:0), (Freq:0; Code:235; Len:9; Dad:0), (Freq:0; Code:491; Len:9; Dad:0),
    (Freq:0; Code: 27; Len:9; Dad:0), (Freq:0; Code:283; Len:9; Dad:0), (Freq:0; Code:155; Len:9; Dad:0), (Freq:0; Code:411; Len:9; Dad:0),
    (Freq:0; Code: 91; Len:9; Dad:0), (Freq:0; Code:347; Len:9; Dad:0), (Freq:0; Code:219; Len:9; Dad:0), (Freq:0; Code:475; Len:9; Dad:0),
    (Freq:0; Code: 59; Len:9; Dad:0), (Freq:0; Code:315; Len:9; Dad:0), (Freq:0; Code:187; Len:9; Dad:0), (Freq:0; Code:443; Len:9; Dad:0),
    (Freq:0; Code:123; Len:9; Dad:0), (Freq:0; Code:379; Len:9; Dad:0), (Freq:0; Code:251; Len:9; Dad:0), (Freq:0; Code:507; Len:9; Dad:0),
    (Freq:0; Code:  7; Len:9; Dad:0), (Freq:0; Code:263; Len:9; Dad:0), (Freq:0; Code:135; Len:9; Dad:0), (Freq:0; Code:391; Len:9; Dad:0),
    (Freq:0; Code: 71; Len:9; Dad:0), (Freq:0; Code:327; Len:9; Dad:0), (Freq:0; Code:199; Len:9; Dad:0), (Freq:0; Code:455; Len:9; Dad:0),
    (Freq:0; Code: 39; Len:9; Dad:0), (Freq:0; Code:295; Len:9; Dad:0), (Freq:0; Code:167; Len:9; Dad:0), (Freq:0; Code:423; Len:9; Dad:0),
    (Freq:0; Code:103; Len:9; Dad:0), (Freq:0; Code:359; Len:9; Dad:0), (Freq:0; Code:231; Len:9; Dad:0), (Freq:0; Code:487; Len:9; Dad:0),
    (Freq:0; Code: 23; Len:9; Dad:0), (Freq:0; Code:279; Len:9; Dad:0), (Freq:0; Code:151; Len:9; Dad:0), (Freq:0; Code:407; Len:9; Dad:0),
    (Freq:0; Code: 87; Len:9; Dad:0), (Freq:0; Code:343; Len:9; Dad:0), (Freq:0; Code:215; Len:9; Dad:0), (Freq:0; Code:471; Len:9; Dad:0),
    (Freq:0; Code: 55; Len:9; Dad:0), (Freq:0; Code:311; Len:9; Dad:0), (Freq:0; Code:183; Len:9; Dad:0), (Freq:0; Code:439; Len:9; Dad:0),
    (Freq:0; Code:119; Len:9; Dad:0), (Freq:0; Code:375; Len:9; Dad:0), (Freq:0; Code:247; Len:9; Dad:0), (Freq:0; Code:503; Len:9; Dad:0),
    (Freq:0; Code: 15; Len:9; Dad:0), (Freq:0; Code:271; Len:9; Dad:0), (Freq:0; Code:143; Len:9; Dad:0), (Freq:0; Code:399; Len:9; Dad:0),
    (Freq:0; Code: 79; Len:9; Dad:0), (Freq:0; Code:335; Len:9; Dad:0), (Freq:0; Code:207; Len:9; Dad:0), (Freq:0; Code:463; Len:9; Dad:0),
    (Freq:0; Code: 47; Len:9; Dad:0), (Freq:0; Code:303; Len:9; Dad:0), (Freq:0; Code:175; Len:9; Dad:0), (Freq:0; Code:431; Len:9; Dad:0),
    (Freq:0; Code:111; Len:9; Dad:0), (Freq:0; Code:367; Len:9; Dad:0), (Freq:0; Code:239; Len:9; Dad:0), (Freq:0; Code:495; Len:9; Dad:0),
    (Freq:0; Code: 31; Len:9; Dad:0), (Freq:0; Code:287; Len:9; Dad:0), (Freq:0; Code:159; Len:9; Dad:0), (Freq:0; Code:415; Len:9; Dad:0),
    (Freq:0; Code: 95; Len:9; Dad:0), (Freq:0; Code:351; Len:9; Dad:0), (Freq:0; Code:223; Len:9; Dad:0), (Freq:0; Code:479; Len:9; Dad:0),
    (Freq:0; Code: 63; Len:9; Dad:0), (Freq:0; Code:319; Len:9; Dad:0), (Freq:0; Code:191; Len:9; Dad:0), (Freq:0; Code:447; Len:9; Dad:0),
    (Freq:0; Code:127; Len:9; Dad:0), (Freq:0; Code:383; Len:9; Dad:0), (Freq:0; Code:255; Len:9; Dad:0), (Freq:0; Code:511; Len:9; Dad:0),
    (Freq:0; Code:  0; Len:7; Dad:0), (Freq:0; Code: 64; Len:7; Dad:0), (Freq:0; Code: 32; Len:7; Dad:0), (Freq:0; Code: 96; Len:7; Dad:0),
    (Freq:0; Code: 16; Len:7; Dad:0), (Freq:0; Code: 80; Len:7; Dad:0), (Freq:0; Code: 48; Len:7; Dad:0), (Freq:0; Code:112; Len:7; Dad:0),
    (Freq:0; Code:  8; Len:7; Dad:0), (Freq:0; Code: 72; Len:7; Dad:0), (Freq:0; Code: 40; Len:7; Dad:0), (Freq:0; Code:104; Len:7; Dad:0),
    (Freq:0; Code: 24; Len:7; Dad:0), (Freq:0; Code: 88; Len:7; Dad:0), (Freq:0; Code: 56; Len:7; Dad:0), (Freq:0; Code:120; Len:7; Dad:0),
    (Freq:0; Code:  4; Len:7; Dad:0), (Freq:0; Code: 68; Len:7; Dad:0), (Freq:0; Code: 36; Len:7; Dad:0), (Freq:0; Code:100; Len:7; Dad:0),
    (Freq:0; Code: 20; Len:7; Dad:0), (Freq:0; Code: 84; Len:7; Dad:0), (Freq:0; Code: 52; Len:7; Dad:0), (Freq:0; Code:116; Len:7; Dad:0),
    (Freq:0; Code:  3; Len:8; Dad:0), (Freq:0; Code:131; Len:8; Dad:0), (Freq:0; Code: 67; Len:8; Dad:0), (Freq:0; Code:195; Len:8; Dad:0),
    (Freq:0; Code: 35; Len:8; Dad:0), (Freq:0; Code:163; Len:8; Dad:0), (Freq:0; Code: 99; Len:8; Dad:0), (Freq:0; Code:227; Len:8; Dad:0)
  );
  StaticDistTree: array[0..29] of TTreeNode = (
    (Freq:0; Code:  0; Len:5; Dad:0), (Freq:0; Code: 16; Len:5; Dad:0), (Freq:0; Code:  8; Len:5; Dad:0), (Freq:0; Code: 24; Len:5; Dad:0),
    (Freq:0; Code:  4; Len:5; Dad:0), (Freq:0; Code: 20; Len:5; Dad:0), (Freq:0; Code: 12; Len:5; Dad:0), (Freq:0; Code: 28; Len:5; Dad:0),
    (Freq:0; Code:  2; Len:5; Dad:0), (Freq:0; Code: 18; Len:5; Dad:0), (Freq:0; Code: 10; Len:5; Dad:0), (Freq:0; Code: 26; Len:5; Dad:0),
    (Freq:0; Code:  6; Len:5; Dad:0), (Freq:0; Code: 22; Len:5; Dad:0), (Freq:0; Code: 14; Len:5; Dad:0), (Freq:0; Code: 30; Len:5; Dad:0),
    (Freq:0; Code:  1; Len:5; Dad:0), (Freq:0; Code: 17; Len:5; Dad:0), (Freq:0; Code:  9; Len:5; Dad:0), (Freq:0; Code: 25; Len:5; Dad:0),
    (Freq:0; Code:  5; Len:5; Dad:0), (Freq:0; Code: 21; Len:5; Dad:0), (Freq:0; Code: 13; Len:5; Dad:0), (Freq:0; Code: 29; Len:5; Dad:0),
    (Freq:0; Code:  3; Len:5; Dad:0), (Freq:0; Code: 19; Len:5; Dad:0), (Freq:0; Code: 11; Len:5; Dad:0), (Freq:0; Code: 27; Len:5; Dad:0),
    (Freq:0; Code:  7; Len:5; Dad:0), (Freq:0; Code: 23; Len:5; Dad:0)
  );

function BitReverse(Value, Bits: Int32): Int32;
begin
  Result := 0;
  while (Bits > 0) do
  begin
    Result := (Result shl 1) or (Value and 1);
    Value := Value shr 1;
    Dec(Bits);
  end;
end;

function DistCodeOf(Dist: Int32): Int32; inline;
begin
  Dec(Dist);
  if (Dist < 256) then
    Result := DistCodeTab[Dist]
  else
    Result := DistCodeTab[256 + (Dist shr 7)];
end;

// worst case is every block falling back to stored
function DeflateBound(InSize: PtrUInt): PtrUInt;
begin
  Result := InSize + ((InSize div SYM_BUF) + (InSize div MAX_STORED) + 2) * STORED_OVERHEAD + BOUND_SLACK;
end;

type
  PDeflateState = ^TDeflateState;
  TDeflateState = record
    Src: PByte;
    SrcSize: Int32;

    Dst, DstStart: PByte;
    BitBuf: UInt32;
    BitCount: Int32;

    Head: array[0..HASH_SIZE-1] of Int32;
    Prev: array[0..WSIZE-1] of Int32;

    LitBuf: array[0..SYM_BUF-1] of Byte;
    DistBuf: array[0..SYM_BUF-1] of UInt16;
    SymCount: Int32;
    BlockStart: Int32;

    LitTree: array[0..HEAP_SIZE-1] of TTreeNode;
    DistTree: array[0..2*DIST_CODES] of TTreeNode;
    ClTree: array[0..2*CL_CODES] of TTreeNode;

    Heap: array[0..HEAP_SIZE-1] of Int32;
    Depth: array[0..HEAP_SIZE-1] of Byte;
    HeapLen, HeapMax: Int32;
    BLCount: array[0..MAX_BITS] of Int32;

    MaxLitCode, MaxDistCode, MaxClIndex: Int32;

    ForcedList: array[0..1] of Int32;
    ForcedCount: Int32;
    AdjOpt, AdjStatic: Int64;

    procedure PutBits(Value: UInt32; Count: Int32); inline;
    procedure AlignByte;
    procedure SendCode(Code: Int32; const Tree: array of TTreeNode); inline;

    procedure ClearFreqs;
    function HashAt(Pos: Int32): UInt32; inline;
    function LongestMatch(Pos, Cur, PrevLen: Int32; var MatchStart: Int32): Int32;
    procedure Tally(Dist, Len, EndPos: Int32; MayFlush: Boolean = True);

    procedure HeapDown(const Tree: array of TTreeNode; K: Int32);
    procedure BuildTree(var Tree: array of TTreeNode; Elems, MaxLen: Int32; out MaxCode: Int32);
    procedure ScanTree(var Tree: array of TTreeNode; MaxCode: Int32);
    procedure SendTree(const Tree: array of TTreeNode; MaxCode: Int32);
    function TreeCost(const Tree: array of TTreeNode; MaxCode: Int32; Extra: PByte; ExtraBase: Int32): Int64;
    function StaticCost: Int64;

    procedure CompressSymbols(const LitCodes, DistCodes: array of TTreeNode);
    procedure FlushBlock(EndPos: Int32; Last: Boolean);

    procedure Run(AIn: PByte; AInSize: PtrUInt; AOut: PByte);
    function Finish: PtrUInt;
  end;

procedure TDeflateState.PutBits(Value: UInt32; Count: Int32);
begin
  BitBuf := BitBuf or (Value shl BitCount);
  Inc(BitCount, Count);
  while (BitCount >= 8) do
  begin
    Dst^ := Byte(BitBuf);
    Inc(Dst);
    BitBuf := BitBuf shr 8;
    Dec(BitCount, 8);
  end;
end;

procedure TDeflateState.AlignByte;
begin
  if (BitCount > 0) then
  begin
    Dst^ := Byte(BitBuf);
    Inc(Dst);
    BitBuf := 0;
    BitCount := 0;
  end;
end;

procedure TDeflateState.SendCode(Code: Int32; const Tree: array of TTreeNode);
begin
  PutBits(Tree[Code].Code, Tree[Code].Len);
end;

procedure TDeflateState.ClearFreqs;
var
  I: Int32;
begin
  for I := 0 to High(LitTree) do
    LitTree[I].Freq := 0;
  for I := 0 to High(DistTree) do
    DistTree[I].Freq := 0;
  for I := 0 to High(ClTree) do
    ClTree[I].Freq := 0;
end;

// zlib's UPDATE_HASH rolled over the three bytes at Pos; the first mask is a
// no-op at these widths, so it reduces to this
function TDeflateState.HashAt(Pos: Int32): UInt32;
begin
  Result := ((((UInt32(Src[Pos]) shl HASH_SHIFT) xor Src[Pos+1]) shl HASH_SHIFT) xor Src[Pos+2]) and HASH_MASK;
end;

function TDeflateState.LongestMatch(Pos, Cur, PrevLen: Int32; var MatchStart: Int32): Int32;
var
  Chain, Best, Limit, MaxLen, Len: Int32;
begin
  Chain := MAX_CHAIN;
  if (PrevLen >= GOOD_MATCH) then
    Chain := Chain shr 2;

  Best := PrevLen;

  if (Pos > MAX_DIST) then
    Limit := Pos - MAX_DIST
  else
    Limit := NIL_POS;

  MaxLen := SrcSize - Pos;
  if (MaxLen > MAX_MATCH) then
    MaxLen := MAX_MATCH;
  if (MaxLen <= Best) then
    Exit(Best);

  repeat
    // cheap reject before the full compare
    if (Src[Cur + Best] = Src[Pos + Best]) and (Src[Cur + Best - 1] = Src[Pos + Best - 1]) and
       (Src[Cur] = Src[Pos]) and (Src[Cur + 1] = Src[Pos + 1]) then
    begin
      Len := 2;
      while (Len < MaxLen) and (Src[Cur + Len] = Src[Pos + Len]) do
        Inc(Len);

      if (Len > Best) then
      begin
        Best := Len;
        MatchStart := Cur;
        if (Len >= NICE_MATCH) or (Len >= MaxLen) then
          Break;
      end;
    end;

    Cur := Prev[Cur and WMASK];
    Dec(Chain);
  until (Chain = 0) or (Cur <= Limit);

  Result := Best;
end;

procedure TDeflateState.Tally(Dist, Len, EndPos: Int32; MayFlush: Boolean);
begin
  LitBuf[SymCount] := Len;
  DistBuf[SymCount] := Dist;
  Inc(SymCount);

  if (Dist = 0) then
    Inc(LitTree[Len].Freq)
  else
  begin
    Inc(LitTree[LenCodeTab[Len] + 257].Freq);
    Inc(DistTree[DistCodeOf(Dist)].Freq);
  end;

  if MayFlush and (SymCount = SYM_END) then
    FlushBlock(EndPos, False);
end;

// ties break on subtree depth, which keeps the longest code shorter
procedure TDeflateState.HeapDown(const Tree: array of TTreeNode; K: Int32);
var
  V, J: Int32;
begin
  V := Heap[K];
  J := K shl 1;
  while (J <= HeapLen) do
  begin
    if (J < HeapLen) and
       ((Tree[Heap[J+1]].Freq < Tree[Heap[J]].Freq) or
        ((Tree[Heap[J+1]].Freq = Tree[Heap[J]].Freq) and (Depth[Heap[J+1]] <= Depth[Heap[J]]))) then
      Inc(J);

    if (Tree[V].Freq < Tree[Heap[J]].Freq) or
       ((Tree[V].Freq = Tree[Heap[J]].Freq) and (Depth[V] <= Depth[Heap[J]])) then
      Break;

    Heap[K] := Heap[J];
    K := J;
    J := J shl 1;
  end;
  Heap[K] := V;
end;

procedure TDeflateState.BuildTree(var Tree: array of TTreeNode; Elems, MaxLen: Int32; out MaxCode: Int32);
var
  N, M, Node, Bits, H, Overflow, Code: Int32;
  NextCode: array[0..MAX_BITS] of Int32;
begin
  MaxCode := -1;
  HeapLen := 0;
  HeapMax := HEAP_SIZE;

  for N := 0 to Elems - 1 do
    if (Tree[N].Freq <> 0) then
    begin
      Inc(HeapLen);
      Heap[HeapLen] := N;
      MaxCode := N;
      Depth[N] := 0;
    end else
      Tree[N].Len := 0;

  // a tree needs two codes to be decodable. zlib cancels their cost as it forces
  // them; ForcedList lets the caller do the same afterwards.
  ForcedCount := 0;
  while (HeapLen < 2) do
  begin
    Inc(HeapLen);
    if (MaxCode < 2) then
    begin
      Inc(MaxCode);
      Node := MaxCode;
    end else
      Node := 0;
    Heap[HeapLen] := Node;
    Tree[Node].Freq := 1;
    Depth[Node] := 0;
    ForcedList[ForcedCount] := Node;
    Inc(ForcedCount);
  end;

  for N := HeapLen div 2 downto 1 do
    HeapDown(Tree, N);

  // join the two rarest repeatedly; Heap[HeapMax..] ends up parent-before-child
  Node := Elems;
  repeat
    N := Heap[1];
    Heap[1] := Heap[HeapLen];
    Dec(HeapLen);
    HeapDown(Tree, 1);
    M := Heap[1];

    Dec(HeapMax);
    Heap[HeapMax] := N;
    Dec(HeapMax);
    Heap[HeapMax] := M;

    Tree[Node].Freq := Tree[N].Freq + Tree[M].Freq;
    if (Depth[N] >= Depth[M]) then
      Depth[Node] := Depth[N] + 1
    else
      Depth[Node] := Depth[M] + 1;
    Tree[N].Dad := Node;
    Tree[M].Dad := Node;

    Heap[1] := Node;
    Inc(Node);
    HeapDown(Tree, 1);
  until (HeapLen < 2);

  Dec(HeapMax);
  Heap[HeapMax] := Heap[1];

  // a node's depth in the tree is its code length
  for Bits := 0 to MAX_BITS do
    BLCount[Bits] := 0;
  Overflow := 0;

  Tree[Heap[HeapMax]].Len := 0; // the root
  for H := HeapMax + 1 to HEAP_SIZE - 1 do
  begin
    N := Heap[H];
    Bits := Tree[Tree[N].Dad].Len + 1;
    if (Bits > MaxLen) then
    begin
      Bits := MaxLen;
      Inc(Overflow);
    end;
    Tree[N].Len := Bits;
    if (N > MaxCode) then
      Continue; // an internal node has no code of its own
    Inc(BLCount[Bits]);
  end;

  if (Overflow > 0) then
  begin
    // too deep: move a leaf down a level, freeing two slots at the bottom
    repeat
      Bits := MaxLen - 1;
      while (BLCount[Bits] = 0) do
        Dec(Bits);
      Dec(BLCount[Bits]);
      Inc(BLCount[Bits + 1], 2);
      Dec(BLCount[MaxLen]);
      Dec(Overflow, 2);
    until (Overflow <= 0);

      H := HEAP_SIZE;
    for Bits := MaxLen downto 1 do
    begin
      N := BLCount[Bits];
      while (N <> 0) do
      begin
        Dec(H);
        M := Heap[H];
        if (M > MaxCode) then
          Continue;
        if (Tree[M].Len <> Bits) then
          Tree[M].Len := Bits;
        Dec(N);
      end;
    end;
  end;

  // stored bit reversed: codes go out most significant bit first, into a stream
  // filled least significant bit first
  Code := 0;
  for Bits := 1 to MAX_BITS do
  begin
    Code := (Code + BLCount[Bits - 1]) shl 1;
    NextCode[Bits] := Code;
  end;

  for N := 0 to MaxCode do
  begin
    Bits := Tree[N].Len;
    if (Bits = 0) then
      Continue;
    Tree[N].Code := BitReverse(NextCode[Bits], Bits);
    Inc(NextCode[Bits]);
  end;
end;

procedure TDeflateState.ScanTree(var Tree: array of TTreeNode; MaxCode: Int32);
var
  N, PrevLen, CurLen, NextLen, Count, MaxCount, MinCount: Int32;
begin
  PrevLen := -1;
  NextLen := Tree[0].Len;
  Count := 0;
  MaxCount := 7;
  MinCount := 4;
  if (NextLen = 0) then
  begin
    MaxCount := 138;
    MinCount := 3;
  end;
  Tree[MaxCode + 1].Len := $FFFF; // never equal to a real length, forces a final flush

  for N := 0 to MaxCode do
  begin
    CurLen := NextLen;
    NextLen := Tree[N + 1].Len;
    Inc(Count);

    if (Count < MaxCount) and (CurLen = NextLen) then
      Continue
    else if (Count < MinCount) then
      Inc(ClTree[CurLen].Freq, Count)
    else if (CurLen <> 0) then
    begin
      if (CurLen <> PrevLen) then
        Inc(ClTree[CurLen].Freq);
      Inc(ClTree[16].Freq);
    end
    else if (Count <= 10) then
      Inc(ClTree[17].Freq)
    else
      Inc(ClTree[18].Freq);

    Count := 0;
    PrevLen := CurLen;
    if (NextLen = 0) then
    begin
      MaxCount := 138;
      MinCount := 3;
    end
    else if (CurLen = NextLen) then
    begin
      MaxCount := 6;
      MinCount := 3;
    end else
    begin
      MaxCount := 7;
      MinCount := 4;
    end;
  end;
end;

procedure TDeflateState.SendTree(const Tree: array of TTreeNode; MaxCode: Int32);
var
  N, PrevLen, CurLen, NextLen, Count, MaxCount, MinCount: Int32;
begin
  PrevLen := -1;
  NextLen := Tree[0].Len;
  Count := 0;
  MaxCount := 7;
  MinCount := 4;
  if (NextLen = 0) then
  begin
    MaxCount := 138;
    MinCount := 3;
  end;

  for N := 0 to MaxCode do
  begin
    CurLen := NextLen;
    NextLen := Tree[N + 1].Len;
    Inc(Count);

    if (Count < MaxCount) and (CurLen = NextLen) then
      Continue
    else if (Count < MinCount) then
    begin
      repeat
        SendCode(CurLen, ClTree);
        Dec(Count);
      until (Count = 0);
    end
    else if (CurLen <> 0) then
    begin
      if (CurLen <> PrevLen) then
      begin
        SendCode(CurLen, ClTree);
        Dec(Count);
      end;
      SendCode(16, ClTree);
      PutBits(Count - 3, 2);
    end
    else if (Count <= 10) then
    begin
      SendCode(17, ClTree);
      PutBits(Count - 3, 3);
    end else
    begin
      SendCode(18, ClTree);
      PutBits(Count - 11, 7);
    end;

    Count := 0;
    PrevLen := CurLen;
    if (NextLen = 0) then
    begin
      MaxCount := 138;
      MinCount := 3;
    end
    else if (CurLen = NextLen) then
    begin
      MaxCount := 6;
      MinCount := 3;
    end else
    begin
      MaxCount := 7;
      MinCount := 4;
    end;
  end;
end;

function TDeflateState.TreeCost(const Tree: array of TTreeNode; MaxCode: Int32; Extra: PByte; ExtraBase: Int32): Int64;
var
  N, Bits: Int32;
begin
  Result := 0;
  for N := 0 to MaxCode do
    if (Tree[N].Freq <> 0) then
    begin
      Bits := Tree[N].Len;
      if (Extra <> nil) and (N >= ExtraBase) then
        Inc(Bits, Extra[N - ExtraBase]);
      Inc(Result, Int64(Tree[N].Freq) * Bits);
    end;
end;

function TDeflateState.StaticCost: Int64;
var
  N, Bits: Int32;
begin
  Result := 0;
  for N := 0 to LIT_CODES - 1 do
    if (LitTree[N].Freq <> 0) then
    begin
      Bits := StaticLitLen[N];
      if (N >= 257) then
        Inc(Bits, LEN_EXTRA[N - 257]);
      Inc(Result, Int64(LitTree[N].Freq) * Bits);
    end;

  for N := 0 to DIST_CODES - 1 do
    if (DistTree[N].Freq <> 0) then
      Inc(Result, Int64(DistTree[N].Freq) * (5 + DIST_EXTRA[N]));
end;

procedure TDeflateState.CompressSymbols(const LitCodes, DistCodes: array of TTreeNode);
var
  I, Dist, Lit, Code, Extra: Int32;
begin
  for I := 0 to SymCount - 1 do
  begin
    Dist := DistBuf[I];
    Lit := LitBuf[I];

    if (Dist = 0) then
      SendCode(Lit, LitCodes)
    else
    begin
      Code := LenCodeTab[Lit];
      SendCode(Code + 257, LitCodes);
      Extra := LEN_EXTRA[Code];
      if (Extra <> 0) then
        PutBits((Lit + MIN_MATCH) - LEN_BASE[Code], Extra);

      Code := DistCodeOf(Dist);
      SendCode(Code, DistCodes);
      Extra := DIST_EXTRA[Code];
      if (Extra <> 0) then
        PutBits(Dist - DIST_BASE[Code], Extra);
    end;
  end;

  SendCode(END_BLOCK, LitCodes);
end;

procedure TDeflateState.FlushBlock(EndPos: Int32; Last: Boolean);
var
  RawSize, Chunk, Pos, N, Sub, I: Int32;
  DynCost, StatCost, DynBytes, StatBytes: Int64;
begin
  RawSize := EndPos - BlockStart;

  Inc(LitTree[END_BLOCK].Freq);

  AdjOpt := 0;
  AdjStatic := 0;

  BuildTree(LitTree, LIT_CODES, MAX_BITS, MaxLitCode);
  for N := 0 to ForcedCount - 1 do
  begin
    Dec(AdjOpt);
    Dec(AdjStatic, StaticLitLen[ForcedList[N]]);
  end;

  BuildTree(DistTree, DIST_CODES, MAX_BITS, MaxDistCode);
  for N := 0 to ForcedCount - 1 do
  begin
    Dec(AdjOpt);
    Dec(AdjStatic, 5);
  end;

  ScanTree(LitTree, MaxLitCode);
  ScanTree(DistTree, MaxDistCode);
  BuildTree(ClTree, CL_CODES, MAX_CL_BITS, I);
  Dec(AdjOpt, ForcedCount); // no static tree describes the code length tree

  MaxClIndex := CL_CODES - 1;
  while (MaxClIndex >= 3) and (ClTree[CL_ORDER[MaxClIndex]].Len = 0) do
    Dec(MaxClIndex);

  DynCost := 3 + 5 + 5 + 4 + 3 * Int64(MaxClIndex + 1) +
             TreeCost(ClTree, CL_CODES - 1, @CL_EXTRA[0], 0) +
             TreeCost(LitTree, MaxLitCode, @LEN_EXTRA[0], 257) +
             TreeCost(DistTree, MaxDistCode, @DIST_EXTRA[0], 0) +
             AdjOpt;

  StatCost := 3 + StaticCost() + AdjStatic;

  // zlib compares whole bytes, so static can be up to seven bits worse and win
  DynBytes := (DynCost + 7) shr 3;
  StatBytes := (StatCost + 7) shr 3;
  if (StatBytes <= DynBytes) then
    DynBytes := StatBytes;

  Sub := (RawSize + MAX_STORED - 1) div MAX_STORED;
  if (Sub = 0) then
    Sub := 1;

  if (Int64(RawSize) + 4 <= DynBytes) then
  begin
    Pos := BlockStart;
    for N := 1 to Sub do
    begin
      Chunk := EndPos - Pos;
      if (Chunk > MAX_STORED) then
        Chunk := MAX_STORED;

      PutBits(Ord(Last and (N = Sub)), 1);
      PutBits(0, 2);
      AlignByte();
      PUInt16(Dst)^ := UInt16(Chunk);
      PUInt16(Dst + 2)^ := UInt16(not Chunk);
      Inc(Dst, 4);
      if (Chunk > 0) then
      begin
        Move(Src[Pos], Dst^, Chunk);
        Inc(Dst, Chunk);
        Inc(Pos, Chunk);
      end;
    end;
  end
  else if (StatBytes = DynBytes) then
  begin
    PutBits(Ord(Last), 1);
    PutBits(1, 2);
    CompressSymbols(StaticLitTree, StaticDistTree);
  end else
  begin
    PutBits(Ord(Last), 1);
    PutBits(2, 2);

    PutBits(MaxLitCode + 1 - 257, 5);
    PutBits(MaxDistCode + 1 - 1, 5);
    PutBits(MaxClIndex + 1 - 4, 4);
    for I := 0 to MaxClIndex do
      PutBits(ClTree[CL_ORDER[I]].Len, 3);

    SendTree(LitTree, MaxLitCode);
    SendTree(DistTree, MaxDistCode);

    CompressSymbols(LitTree, DistTree);
  end;

  SymCount := 0;
  BlockStart := EndPos;
  ClearFreqs();
end;

procedure TDeflateState.Run(AIn: PByte; AInSize: PtrUInt; AOut: PByte);
var
  Pos, HashHead, N: Int32;
  H: UInt32;
  CurLen, CurMatch, PrevLen, PrevMatch: Int32;
  MatchAvailable: Boolean;
begin
  Src := AIn;
  SrcSize := AInSize;
  Dst := AOut;
  DstStart := AOut;
  BitBuf := 0;
  BitCount := 0;
  SymCount := 0;
  BlockStart := 0;

  FillChar(LitTree, SizeOf(LitTree), 0);
  FillChar(DistTree, SizeOf(DistTree), 0);
  FillChar(ClTree, SizeOf(ClTree), 0);
  FillDWord(Head, HASH_SIZE, NIL_POS);

  Pos := 0;
  CurLen := MIN_MATCH - 1;
  CurMatch := 0;
  MatchAvailable := False;

  while (Pos < SrcSize) do
  begin
    HashHead := NIL_POS;
    if (Pos + MIN_MATCH <= SrcSize) then
    begin
      H := HashAt(Pos);
      HashHead := Head[H];
      Prev[Pos and WMASK] := HashHead;
      Head[H] := Pos;
    end;

    PrevLen := CurLen;
    PrevMatch := CurMatch;
    CurLen := MIN_MATCH - 1;

    if (HashHead <> NIL_POS) and (PrevLen < MAX_LAZY) and (Pos - HashHead <= MAX_DIST) then
    begin
      CurLen := LongestMatch(Pos, HashHead, PrevLen, CurMatch);
      if (CurLen = MIN_MATCH) and (Pos - CurMatch > TOO_FAR) then
        CurLen := MIN_MATCH - 1;
    end;

    if (PrevLen >= MIN_MATCH) and (CurLen <= PrevLen) then
    begin
      // the match found one byte back is at least as good, take that one
      Tally(Pos - 1 - PrevMatch, PrevLen - MIN_MATCH, Pos - 1 + PrevLen);

      // hash every position the match covers bar its last two
      N := PrevLen - 2;
      repeat
        Inc(Pos);
        if (Pos + MIN_MATCH <= SrcSize) then
        begin
          H := HashAt(Pos);
          Prev[Pos and WMASK] := Head[H];
          Head[H] := Pos;
        end;
        Dec(N);
      until (N = 0);

      MatchAvailable := False;
      CurLen := MIN_MATCH - 1;
      Inc(Pos);
    end
    else if MatchAvailable then
    begin
      Tally(0, Src[Pos - 1], Pos);
      Inc(Pos);
    end else
    begin
      MatchAvailable := True;
      Inc(Pos);
    end;
  end;

  // zlib tallies this one without acting on the flush flag, so its last block
  // may hold a full SYM_BUF symbols rather than splitting at SYM_END
  if MatchAvailable then
    Tally(0, Src[Pos - 1], Pos, False);

  FlushBlock(SrcSize, True);
end;

function TDeflateState.Finish: PtrUInt;
begin
  AlignByte();
  Result := Dst - DstStart;
end;

function Deflate(InData: PByte; InSize: PtrUInt; OutData: PByte): PtrUInt;
var
  State: PDeflateState;
begin
  New(State);
  try
    State^.Run(InData, InSize, OutData);
    Result := State^.Finish();
  finally
    Dispose(State);
  end;
end;

const
  ENOUGH_LENS  = 852;  // "enough 286 9 15", see inftrees.h
  ENOUGH_DISTS = 592;  // "enough 30 6 15"
  ENOUGH       = ENOUGH_LENS + ENOUGH_DISTS;

  LEN_ROOT  = 9;  // root table index bits, which the two bounds above depend on
  DIST_ROOT = 6;

  // inftrees.c's lbase/lext and dbase/dext. An extra count doubles as the op
  // byte, so 16 marks a length or distance and 64 an invalid code; the tail
  // entries are the codes deflate never assigns.
  LBASE: array[0..30] of UInt16 = (3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,0,0);
  LEXT: array[0..30] of UInt16 = (16,16,16,16,16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,16,68,193);
  DBASE: array[0..31] of UInt16 = (1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,1025,1537,2049,3073,4097,6145,8193,12289,16385,24577,0,0);
  DEXT: array[0..31] of UInt16 = (16,16,16,16,17,17,18,18,19,19,20,20,21,21,22,22,23,23,24,24,25,25,26,26,27,27,28,28,29,29,64,64);

type
  TCodeType = (ctCodes, ctLens, ctDists);

  // Op is 0 for a literal, 0000tttt for a sub-table of tttt index bits,
  // 0001eeee for a length or distance with eeee extra, 96 end of block, 64 invalid
  TCode = record
    Op, Bits: Byte;
    Val: UInt16;
  end;
  PCode = ^TCode;

{$POINTERMATH ON}

const
  // inffixed.h, entry for entry. What InflateTable builds from the static tree,
  // then makefixed's last step: op 64 wherever (index and 127) = 99. That is the
  // two slots each for symbols 286 and 287, the length codes deflate never
  // assigns, which would otherwise carry LEXT's own 68 and 193.
  LenFix: array[0..511] of TCode = (
    (Op: 96; Bits: 7; Val:    0), (Op:  0; Bits: 8; Val:   80), (Op:  0; Bits: 8; Val:   16), (Op: 20; Bits: 8; Val:  115),
    (Op: 18; Bits: 7; Val:   31), (Op:  0; Bits: 8; Val:  112), (Op:  0; Bits: 8; Val:   48), (Op:  0; Bits: 9; Val:  192),
    (Op: 16; Bits: 7; Val:   10), (Op:  0; Bits: 8; Val:   96), (Op:  0; Bits: 8; Val:   32), (Op:  0; Bits: 9; Val:  160),
    (Op:  0; Bits: 8; Val:    0), (Op:  0; Bits: 8; Val:  128), (Op:  0; Bits: 8; Val:   64), (Op:  0; Bits: 9; Val:  224),
    (Op: 16; Bits: 7; Val:    6), (Op:  0; Bits: 8; Val:   88), (Op:  0; Bits: 8; Val:   24), (Op:  0; Bits: 9; Val:  144),
    (Op: 19; Bits: 7; Val:   59), (Op:  0; Bits: 8; Val:  120), (Op:  0; Bits: 8; Val:   56), (Op:  0; Bits: 9; Val:  208),
    (Op: 17; Bits: 7; Val:   17), (Op:  0; Bits: 8; Val:  104), (Op:  0; Bits: 8; Val:   40), (Op:  0; Bits: 9; Val:  176),
    (Op:  0; Bits: 8; Val:    8), (Op:  0; Bits: 8; Val:  136), (Op:  0; Bits: 8; Val:   72), (Op:  0; Bits: 9; Val:  240),
    (Op: 16; Bits: 7; Val:    4), (Op:  0; Bits: 8; Val:   84), (Op:  0; Bits: 8; Val:   20), (Op: 21; Bits: 8; Val:  227),
    (Op: 19; Bits: 7; Val:   43), (Op:  0; Bits: 8; Val:  116), (Op:  0; Bits: 8; Val:   52), (Op:  0; Bits: 9; Val:  200),
    (Op: 17; Bits: 7; Val:   13), (Op:  0; Bits: 8; Val:  100), (Op:  0; Bits: 8; Val:   36), (Op:  0; Bits: 9; Val:  168),
    (Op:  0; Bits: 8; Val:    4), (Op:  0; Bits: 8; Val:  132), (Op:  0; Bits: 8; Val:   68), (Op:  0; Bits: 9; Val:  232),
    (Op: 16; Bits: 7; Val:    8), (Op:  0; Bits: 8; Val:   92), (Op:  0; Bits: 8; Val:   28), (Op:  0; Bits: 9; Val:  152),
    (Op: 20; Bits: 7; Val:   83), (Op:  0; Bits: 8; Val:  124), (Op:  0; Bits: 8; Val:   60), (Op:  0; Bits: 9; Val:  216),
    (Op: 18; Bits: 7; Val:   23), (Op:  0; Bits: 8; Val:  108), (Op:  0; Bits: 8; Val:   44), (Op:  0; Bits: 9; Val:  184),
    (Op:  0; Bits: 8; Val:   12), (Op:  0; Bits: 8; Val:  140), (Op:  0; Bits: 8; Val:   76), (Op:  0; Bits: 9; Val:  248),
    (Op: 16; Bits: 7; Val:    3), (Op:  0; Bits: 8; Val:   82), (Op:  0; Bits: 8; Val:   18), (Op: 21; Bits: 8; Val:  163),
    (Op: 19; Bits: 7; Val:   35), (Op:  0; Bits: 8; Val:  114), (Op:  0; Bits: 8; Val:   50), (Op:  0; Bits: 9; Val:  196),
    (Op: 17; Bits: 7; Val:   11), (Op:  0; Bits: 8; Val:   98), (Op:  0; Bits: 8; Val:   34), (Op:  0; Bits: 9; Val:  164),
    (Op:  0; Bits: 8; Val:    2), (Op:  0; Bits: 8; Val:  130), (Op:  0; Bits: 8; Val:   66), (Op:  0; Bits: 9; Val:  228),
    (Op: 16; Bits: 7; Val:    7), (Op:  0; Bits: 8; Val:   90), (Op:  0; Bits: 8; Val:   26), (Op:  0; Bits: 9; Val:  148),
    (Op: 20; Bits: 7; Val:   67), (Op:  0; Bits: 8; Val:  122), (Op:  0; Bits: 8; Val:   58), (Op:  0; Bits: 9; Val:  212),
    (Op: 18; Bits: 7; Val:   19), (Op:  0; Bits: 8; Val:  106), (Op:  0; Bits: 8; Val:   42), (Op:  0; Bits: 9; Val:  180),
    (Op:  0; Bits: 8; Val:   10), (Op:  0; Bits: 8; Val:  138), (Op:  0; Bits: 8; Val:   74), (Op:  0; Bits: 9; Val:  244),
    (Op: 16; Bits: 7; Val:    5), (Op:  0; Bits: 8; Val:   86), (Op:  0; Bits: 8; Val:   22), (Op: 64; Bits: 8; Val:    0),
    (Op: 19; Bits: 7; Val:   51), (Op:  0; Bits: 8; Val:  118), (Op:  0; Bits: 8; Val:   54), (Op:  0; Bits: 9; Val:  204),
    (Op: 17; Bits: 7; Val:   15), (Op:  0; Bits: 8; Val:  102), (Op:  0; Bits: 8; Val:   38), (Op:  0; Bits: 9; Val:  172),
    (Op:  0; Bits: 8; Val:    6), (Op:  0; Bits: 8; Val:  134), (Op:  0; Bits: 8; Val:   70), (Op:  0; Bits: 9; Val:  236),
    (Op: 16; Bits: 7; Val:    9), (Op:  0; Bits: 8; Val:   94), (Op:  0; Bits: 8; Val:   30), (Op:  0; Bits: 9; Val:  156),
    (Op: 20; Bits: 7; Val:   99), (Op:  0; Bits: 8; Val:  126), (Op:  0; Bits: 8; Val:   62), (Op:  0; Bits: 9; Val:  220),
    (Op: 18; Bits: 7; Val:   27), (Op:  0; Bits: 8; Val:  110), (Op:  0; Bits: 8; Val:   46), (Op:  0; Bits: 9; Val:  188),
    (Op:  0; Bits: 8; Val:   14), (Op:  0; Bits: 8; Val:  142), (Op:  0; Bits: 8; Val:   78), (Op:  0; Bits: 9; Val:  252),
    (Op: 96; Bits: 7; Val:    0), (Op:  0; Bits: 8; Val:   81), (Op:  0; Bits: 8; Val:   17), (Op: 21; Bits: 8; Val:  131),
    (Op: 18; Bits: 7; Val:   31), (Op:  0; Bits: 8; Val:  113), (Op:  0; Bits: 8; Val:   49), (Op:  0; Bits: 9; Val:  194),
    (Op: 16; Bits: 7; Val:   10), (Op:  0; Bits: 8; Val:   97), (Op:  0; Bits: 8; Val:   33), (Op:  0; Bits: 9; Val:  162),
    (Op:  0; Bits: 8; Val:    1), (Op:  0; Bits: 8; Val:  129), (Op:  0; Bits: 8; Val:   65), (Op:  0; Bits: 9; Val:  226),
    (Op: 16; Bits: 7; Val:    6), (Op:  0; Bits: 8; Val:   89), (Op:  0; Bits: 8; Val:   25), (Op:  0; Bits: 9; Val:  146),
    (Op: 19; Bits: 7; Val:   59), (Op:  0; Bits: 8; Val:  121), (Op:  0; Bits: 8; Val:   57), (Op:  0; Bits: 9; Val:  210),
    (Op: 17; Bits: 7; Val:   17), (Op:  0; Bits: 8; Val:  105), (Op:  0; Bits: 8; Val:   41), (Op:  0; Bits: 9; Val:  178),
    (Op:  0; Bits: 8; Val:    9), (Op:  0; Bits: 8; Val:  137), (Op:  0; Bits: 8; Val:   73), (Op:  0; Bits: 9; Val:  242),
    (Op: 16; Bits: 7; Val:    4), (Op:  0; Bits: 8; Val:   85), (Op:  0; Bits: 8; Val:   21), (Op: 16; Bits: 8; Val:  258),
    (Op: 19; Bits: 7; Val:   43), (Op:  0; Bits: 8; Val:  117), (Op:  0; Bits: 8; Val:   53), (Op:  0; Bits: 9; Val:  202),
    (Op: 17; Bits: 7; Val:   13), (Op:  0; Bits: 8; Val:  101), (Op:  0; Bits: 8; Val:   37), (Op:  0; Bits: 9; Val:  170),
    (Op:  0; Bits: 8; Val:    5), (Op:  0; Bits: 8; Val:  133), (Op:  0; Bits: 8; Val:   69), (Op:  0; Bits: 9; Val:  234),
    (Op: 16; Bits: 7; Val:    8), (Op:  0; Bits: 8; Val:   93), (Op:  0; Bits: 8; Val:   29), (Op:  0; Bits: 9; Val:  154),
    (Op: 20; Bits: 7; Val:   83), (Op:  0; Bits: 8; Val:  125), (Op:  0; Bits: 8; Val:   61), (Op:  0; Bits: 9; Val:  218),
    (Op: 18; Bits: 7; Val:   23), (Op:  0; Bits: 8; Val:  109), (Op:  0; Bits: 8; Val:   45), (Op:  0; Bits: 9; Val:  186),
    (Op:  0; Bits: 8; Val:   13), (Op:  0; Bits: 8; Val:  141), (Op:  0; Bits: 8; Val:   77), (Op:  0; Bits: 9; Val:  250),
    (Op: 16; Bits: 7; Val:    3), (Op:  0; Bits: 8; Val:   83), (Op:  0; Bits: 8; Val:   19), (Op: 21; Bits: 8; Val:  195),
    (Op: 19; Bits: 7; Val:   35), (Op:  0; Bits: 8; Val:  115), (Op:  0; Bits: 8; Val:   51), (Op:  0; Bits: 9; Val:  198),
    (Op: 17; Bits: 7; Val:   11), (Op:  0; Bits: 8; Val:   99), (Op:  0; Bits: 8; Val:   35), (Op:  0; Bits: 9; Val:  166),
    (Op:  0; Bits: 8; Val:    3), (Op:  0; Bits: 8; Val:  131), (Op:  0; Bits: 8; Val:   67), (Op:  0; Bits: 9; Val:  230),
    (Op: 16; Bits: 7; Val:    7), (Op:  0; Bits: 8; Val:   91), (Op:  0; Bits: 8; Val:   27), (Op:  0; Bits: 9; Val:  150),
    (Op: 20; Bits: 7; Val:   67), (Op:  0; Bits: 8; Val:  123), (Op:  0; Bits: 8; Val:   59), (Op:  0; Bits: 9; Val:  214),
    (Op: 18; Bits: 7; Val:   19), (Op:  0; Bits: 8; Val:  107), (Op:  0; Bits: 8; Val:   43), (Op:  0; Bits: 9; Val:  182),
    (Op:  0; Bits: 8; Val:   11), (Op:  0; Bits: 8; Val:  139), (Op:  0; Bits: 8; Val:   75), (Op:  0; Bits: 9; Val:  246),
    (Op: 16; Bits: 7; Val:    5), (Op:  0; Bits: 8; Val:   87), (Op:  0; Bits: 8; Val:   23), (Op: 64; Bits: 8; Val:    0),
    (Op: 19; Bits: 7; Val:   51), (Op:  0; Bits: 8; Val:  119), (Op:  0; Bits: 8; Val:   55), (Op:  0; Bits: 9; Val:  206),
    (Op: 17; Bits: 7; Val:   15), (Op:  0; Bits: 8; Val:  103), (Op:  0; Bits: 8; Val:   39), (Op:  0; Bits: 9; Val:  174),
    (Op:  0; Bits: 8; Val:    7), (Op:  0; Bits: 8; Val:  135), (Op:  0; Bits: 8; Val:   71), (Op:  0; Bits: 9; Val:  238),
    (Op: 16; Bits: 7; Val:    9), (Op:  0; Bits: 8; Val:   95), (Op:  0; Bits: 8; Val:   31), (Op:  0; Bits: 9; Val:  158),
    (Op: 20; Bits: 7; Val:   99), (Op:  0; Bits: 8; Val:  127), (Op:  0; Bits: 8; Val:   63), (Op:  0; Bits: 9; Val:  222),
    (Op: 18; Bits: 7; Val:   27), (Op:  0; Bits: 8; Val:  111), (Op:  0; Bits: 8; Val:   47), (Op:  0; Bits: 9; Val:  190),
    (Op:  0; Bits: 8; Val:   15), (Op:  0; Bits: 8; Val:  143), (Op:  0; Bits: 8; Val:   79), (Op:  0; Bits: 9; Val:  254),
    (Op: 96; Bits: 7; Val:    0), (Op:  0; Bits: 8; Val:   80), (Op:  0; Bits: 8; Val:   16), (Op: 20; Bits: 8; Val:  115),
    (Op: 18; Bits: 7; Val:   31), (Op:  0; Bits: 8; Val:  112), (Op:  0; Bits: 8; Val:   48), (Op:  0; Bits: 9; Val:  193),
    (Op: 16; Bits: 7; Val:   10), (Op:  0; Bits: 8; Val:   96), (Op:  0; Bits: 8; Val:   32), (Op:  0; Bits: 9; Val:  161),
    (Op:  0; Bits: 8; Val:    0), (Op:  0; Bits: 8; Val:  128), (Op:  0; Bits: 8; Val:   64), (Op:  0; Bits: 9; Val:  225),
    (Op: 16; Bits: 7; Val:    6), (Op:  0; Bits: 8; Val:   88), (Op:  0; Bits: 8; Val:   24), (Op:  0; Bits: 9; Val:  145),
    (Op: 19; Bits: 7; Val:   59), (Op:  0; Bits: 8; Val:  120), (Op:  0; Bits: 8; Val:   56), (Op:  0; Bits: 9; Val:  209),
    (Op: 17; Bits: 7; Val:   17), (Op:  0; Bits: 8; Val:  104), (Op:  0; Bits: 8; Val:   40), (Op:  0; Bits: 9; Val:  177),
    (Op:  0; Bits: 8; Val:    8), (Op:  0; Bits: 8; Val:  136), (Op:  0; Bits: 8; Val:   72), (Op:  0; Bits: 9; Val:  241),
    (Op: 16; Bits: 7; Val:    4), (Op:  0; Bits: 8; Val:   84), (Op:  0; Bits: 8; Val:   20), (Op: 21; Bits: 8; Val:  227),
    (Op: 19; Bits: 7; Val:   43), (Op:  0; Bits: 8; Val:  116), (Op:  0; Bits: 8; Val:   52), (Op:  0; Bits: 9; Val:  201),
    (Op: 17; Bits: 7; Val:   13), (Op:  0; Bits: 8; Val:  100), (Op:  0; Bits: 8; Val:   36), (Op:  0; Bits: 9; Val:  169),
    (Op:  0; Bits: 8; Val:    4), (Op:  0; Bits: 8; Val:  132), (Op:  0; Bits: 8; Val:   68), (Op:  0; Bits: 9; Val:  233),
    (Op: 16; Bits: 7; Val:    8), (Op:  0; Bits: 8; Val:   92), (Op:  0; Bits: 8; Val:   28), (Op:  0; Bits: 9; Val:  153),
    (Op: 20; Bits: 7; Val:   83), (Op:  0; Bits: 8; Val:  124), (Op:  0; Bits: 8; Val:   60), (Op:  0; Bits: 9; Val:  217),
    (Op: 18; Bits: 7; Val:   23), (Op:  0; Bits: 8; Val:  108), (Op:  0; Bits: 8; Val:   44), (Op:  0; Bits: 9; Val:  185),
    (Op:  0; Bits: 8; Val:   12), (Op:  0; Bits: 8; Val:  140), (Op:  0; Bits: 8; Val:   76), (Op:  0; Bits: 9; Val:  249),
    (Op: 16; Bits: 7; Val:    3), (Op:  0; Bits: 8; Val:   82), (Op:  0; Bits: 8; Val:   18), (Op: 21; Bits: 8; Val:  163),
    (Op: 19; Bits: 7; Val:   35), (Op:  0; Bits: 8; Val:  114), (Op:  0; Bits: 8; Val:   50), (Op:  0; Bits: 9; Val:  197),
    (Op: 17; Bits: 7; Val:   11), (Op:  0; Bits: 8; Val:   98), (Op:  0; Bits: 8; Val:   34), (Op:  0; Bits: 9; Val:  165),
    (Op:  0; Bits: 8; Val:    2), (Op:  0; Bits: 8; Val:  130), (Op:  0; Bits: 8; Val:   66), (Op:  0; Bits: 9; Val:  229),
    (Op: 16; Bits: 7; Val:    7), (Op:  0; Bits: 8; Val:   90), (Op:  0; Bits: 8; Val:   26), (Op:  0; Bits: 9; Val:  149),
    (Op: 20; Bits: 7; Val:   67), (Op:  0; Bits: 8; Val:  122), (Op:  0; Bits: 8; Val:   58), (Op:  0; Bits: 9; Val:  213),
    (Op: 18; Bits: 7; Val:   19), (Op:  0; Bits: 8; Val:  106), (Op:  0; Bits: 8; Val:   42), (Op:  0; Bits: 9; Val:  181),
    (Op:  0; Bits: 8; Val:   10), (Op:  0; Bits: 8; Val:  138), (Op:  0; Bits: 8; Val:   74), (Op:  0; Bits: 9; Val:  245),
    (Op: 16; Bits: 7; Val:    5), (Op:  0; Bits: 8; Val:   86), (Op:  0; Bits: 8; Val:   22), (Op: 64; Bits: 8; Val:    0),
    (Op: 19; Bits: 7; Val:   51), (Op:  0; Bits: 8; Val:  118), (Op:  0; Bits: 8; Val:   54), (Op:  0; Bits: 9; Val:  205),
    (Op: 17; Bits: 7; Val:   15), (Op:  0; Bits: 8; Val:  102), (Op:  0; Bits: 8; Val:   38), (Op:  0; Bits: 9; Val:  173),
    (Op:  0; Bits: 8; Val:    6), (Op:  0; Bits: 8; Val:  134), (Op:  0; Bits: 8; Val:   70), (Op:  0; Bits: 9; Val:  237),
    (Op: 16; Bits: 7; Val:    9), (Op:  0; Bits: 8; Val:   94), (Op:  0; Bits: 8; Val:   30), (Op:  0; Bits: 9; Val:  157),
    (Op: 20; Bits: 7; Val:   99), (Op:  0; Bits: 8; Val:  126), (Op:  0; Bits: 8; Val:   62), (Op:  0; Bits: 9; Val:  221),
    (Op: 18; Bits: 7; Val:   27), (Op:  0; Bits: 8; Val:  110), (Op:  0; Bits: 8; Val:   46), (Op:  0; Bits: 9; Val:  189),
    (Op:  0; Bits: 8; Val:   14), (Op:  0; Bits: 8; Val:  142), (Op:  0; Bits: 8; Val:   78), (Op:  0; Bits: 9; Val:  253),
    (Op: 96; Bits: 7; Val:    0), (Op:  0; Bits: 8; Val:   81), (Op:  0; Bits: 8; Val:   17), (Op: 21; Bits: 8; Val:  131),
    (Op: 18; Bits: 7; Val:   31), (Op:  0; Bits: 8; Val:  113), (Op:  0; Bits: 8; Val:   49), (Op:  0; Bits: 9; Val:  195),
    (Op: 16; Bits: 7; Val:   10), (Op:  0; Bits: 8; Val:   97), (Op:  0; Bits: 8; Val:   33), (Op:  0; Bits: 9; Val:  163),
    (Op:  0; Bits: 8; Val:    1), (Op:  0; Bits: 8; Val:  129), (Op:  0; Bits: 8; Val:   65), (Op:  0; Bits: 9; Val:  227),
    (Op: 16; Bits: 7; Val:    6), (Op:  0; Bits: 8; Val:   89), (Op:  0; Bits: 8; Val:   25), (Op:  0; Bits: 9; Val:  147),
    (Op: 19; Bits: 7; Val:   59), (Op:  0; Bits: 8; Val:  121), (Op:  0; Bits: 8; Val:   57), (Op:  0; Bits: 9; Val:  211),
    (Op: 17; Bits: 7; Val:   17), (Op:  0; Bits: 8; Val:  105), (Op:  0; Bits: 8; Val:   41), (Op:  0; Bits: 9; Val:  179),
    (Op:  0; Bits: 8; Val:    9), (Op:  0; Bits: 8; Val:  137), (Op:  0; Bits: 8; Val:   73), (Op:  0; Bits: 9; Val:  243),
    (Op: 16; Bits: 7; Val:    4), (Op:  0; Bits: 8; Val:   85), (Op:  0; Bits: 8; Val:   21), (Op: 16; Bits: 8; Val:  258),
    (Op: 19; Bits: 7; Val:   43), (Op:  0; Bits: 8; Val:  117), (Op:  0; Bits: 8; Val:   53), (Op:  0; Bits: 9; Val:  203),
    (Op: 17; Bits: 7; Val:   13), (Op:  0; Bits: 8; Val:  101), (Op:  0; Bits: 8; Val:   37), (Op:  0; Bits: 9; Val:  171),
    (Op:  0; Bits: 8; Val:    5), (Op:  0; Bits: 8; Val:  133), (Op:  0; Bits: 8; Val:   69), (Op:  0; Bits: 9; Val:  235),
    (Op: 16; Bits: 7; Val:    8), (Op:  0; Bits: 8; Val:   93), (Op:  0; Bits: 8; Val:   29), (Op:  0; Bits: 9; Val:  155),
    (Op: 20; Bits: 7; Val:   83), (Op:  0; Bits: 8; Val:  125), (Op:  0; Bits: 8; Val:   61), (Op:  0; Bits: 9; Val:  219),
    (Op: 18; Bits: 7; Val:   23), (Op:  0; Bits: 8; Val:  109), (Op:  0; Bits: 8; Val:   45), (Op:  0; Bits: 9; Val:  187),
    (Op:  0; Bits: 8; Val:   13), (Op:  0; Bits: 8; Val:  141), (Op:  0; Bits: 8; Val:   77), (Op:  0; Bits: 9; Val:  251),
    (Op: 16; Bits: 7; Val:    3), (Op:  0; Bits: 8; Val:   83), (Op:  0; Bits: 8; Val:   19), (Op: 21; Bits: 8; Val:  195),
    (Op: 19; Bits: 7; Val:   35), (Op:  0; Bits: 8; Val:  115), (Op:  0; Bits: 8; Val:   51), (Op:  0; Bits: 9; Val:  199),
    (Op: 17; Bits: 7; Val:   11), (Op:  0; Bits: 8; Val:   99), (Op:  0; Bits: 8; Val:   35), (Op:  0; Bits: 9; Val:  167),
    (Op:  0; Bits: 8; Val:    3), (Op:  0; Bits: 8; Val:  131), (Op:  0; Bits: 8; Val:   67), (Op:  0; Bits: 9; Val:  231),
    (Op: 16; Bits: 7; Val:    7), (Op:  0; Bits: 8; Val:   91), (Op:  0; Bits: 8; Val:   27), (Op:  0; Bits: 9; Val:  151),
    (Op: 20; Bits: 7; Val:   67), (Op:  0; Bits: 8; Val:  123), (Op:  0; Bits: 8; Val:   59), (Op:  0; Bits: 9; Val:  215),
    (Op: 18; Bits: 7; Val:   19), (Op:  0; Bits: 8; Val:  107), (Op:  0; Bits: 8; Val:   43), (Op:  0; Bits: 9; Val:  183),
    (Op:  0; Bits: 8; Val:   11), (Op:  0; Bits: 8; Val:  139), (Op:  0; Bits: 8; Val:   75), (Op:  0; Bits: 9; Val:  247),
    (Op: 16; Bits: 7; Val:    5), (Op:  0; Bits: 8; Val:   87), (Op:  0; Bits: 8; Val:   23), (Op: 64; Bits: 8; Val:    0),
    (Op: 19; Bits: 7; Val:   51), (Op:  0; Bits: 8; Val:  119), (Op:  0; Bits: 8; Val:   55), (Op:  0; Bits: 9; Val:  207),
    (Op: 17; Bits: 7; Val:   15), (Op:  0; Bits: 8; Val:  103), (Op:  0; Bits: 8; Val:   39), (Op:  0; Bits: 9; Val:  175),
    (Op:  0; Bits: 8; Val:    7), (Op:  0; Bits: 8; Val:  135), (Op:  0; Bits: 8; Val:   71), (Op:  0; Bits: 9; Val:  239),
    (Op: 16; Bits: 7; Val:    9), (Op:  0; Bits: 8; Val:   95), (Op:  0; Bits: 8; Val:   31), (Op:  0; Bits: 9; Val:  159),
    (Op: 20; Bits: 7; Val:   99), (Op:  0; Bits: 8; Val:  127), (Op:  0; Bits: 8; Val:   63), (Op:  0; Bits: 9; Val:  223),
    (Op: 18; Bits: 7; Val:   27), (Op:  0; Bits: 8; Val:  111), (Op:  0; Bits: 8; Val:   47), (Op:  0; Bits: 9; Val:  191),
    (Op:  0; Bits: 8; Val:   15), (Op:  0; Bits: 8; Val:  143), (Op:  0; Bits: 8; Val:   79), (Op:  0; Bits: 9; Val:  255)
  );
  DistFix: array[0..31] of TCode = (
    (Op: 16; Bits: 5; Val:    1), (Op: 23; Bits: 5; Val:  257), (Op: 19; Bits: 5; Val:   17), (Op: 27; Bits: 5; Val: 4097),
    (Op: 17; Bits: 5; Val:    5), (Op: 25; Bits: 5; Val: 1025), (Op: 21; Bits: 5; Val:   65), (Op: 29; Bits: 5; Val:16385),
    (Op: 16; Bits: 5; Val:    3), (Op: 24; Bits: 5; Val:  513), (Op: 20; Bits: 5; Val:   33), (Op: 28; Bits: 5; Val: 8193),
    (Op: 18; Bits: 5; Val:    9), (Op: 26; Bits: 5; Val: 2049), (Op: 22; Bits: 5; Val:  129), (Op: 64; Bits: 5; Val:    0),
    (Op: 16; Bits: 5; Val:    2), (Op: 23; Bits: 5; Val:  385), (Op: 19; Bits: 5; Val:   25), (Op: 27; Bits: 5; Val: 6145),
    (Op: 17; Bits: 5; Val:    7), (Op: 25; Bits: 5; Val: 1537), (Op: 21; Bits: 5; Val:   97), (Op: 29; Bits: 5; Val:24577),
    (Op: 16; Bits: 5; Val:    4), (Op: 24; Bits: 5; Val:  769), (Op: 20; Bits: 5; Val:   49), (Op: 28; Bits: 5; Val:12289),
    (Op: 18; Bits: 5; Val:   13), (Op: 26; Bits: 5; Val: 3073), (Op: 22; Bits: 5; Val:  193), (Op: 64; Bits: 5; Val:    0)
  );

// 0 on success, -1 for an invalid code set, 1 when the tables would not fit
function InflateTable(CodeType: TCodeType; Lens: PUInt16; Codes: Int32; var Table: PCode; var Bits: Int32; Work: PUInt16): Int32;
var
  Len, Sym, Max, Left: Int32;
  Min, Root, Curr, Drop, Used, Huff, Incr, Fill, Low, Mask, Match: UInt32;
  Here: TCode;
  Next, Base: PCode;
  Vals, Extra: PUInt16;
  Count, Offs: array[0..MAX_BITS] of UInt16;
begin
  for Len := 0 to MAX_BITS do
    Count[Len] := 0;
  for Sym := 0 to Codes - 1 do
    Inc(Count[Lens[Sym]]);

  // bound code lengths, force root to be within code lengths
  Root := Bits;
  Max := MAX_BITS;
  while (Max >= 1) and (Count[Max] = 0) do
    Dec(Max);
  if (Root > UInt32(Max)) then
    Root := Max;
  if (Max = 0) then
  begin
    // no symbols to code at all, so make a table that forces an error
    Here.Op := 64;
    Here.Bits := 1;
    Here.Val := 0;
    Table[0] := Here;
    Table[1] := Here;
    Inc(Table, 2);
    Bits := 1;
    Exit(0);
  end;
  Min := 1;
  while (Min < UInt32(Max)) and (Count[Min] = 0) do
    Inc(Min);
  if (Root < Min) then
    Root := Min;

  Left := 1;
  for Len := 1 to MAX_BITS do
  begin
    Left := Left shl 1;
    Dec(Left, Count[Len]);
    if (Left < 0) then
      Exit(-1); // over-subscribed
  end;
  if (Left > 0) and ((CodeType = ctCodes) or (Max <> 1)) then
    Exit(-1); // incomplete set

  // sort symbols by length, by symbol order within each length
  Offs[1] := 0;
  for Len := 1 to MAX_BITS - 1 do
    Offs[Len + 1] := Offs[Len] + Count[Len];
  for Sym := 0 to Codes - 1 do
    if (Lens[Sym] <> 0) then
    begin
      Work[Offs[Lens[Sym]]] := Sym;
      Inc(Offs[Lens[Sym]]);
    end;

  Vals := nil;
  Extra := nil;
  Match := 0;
  case CodeType of
    ctCodes: Match := 20;
    ctLens:  begin Vals := @LBASE[0]; Extra := @LEXT[0]; Match := 257; end;
    ctDists: begin Vals := @DBASE[0]; Extra := @DEXT[0]; end;
  end;

  Huff := 0;
  Sym := 0;
  Len := Min;
  Base := Table;
  Next := Table;
  Curr := Root;
  Drop := 0;
  Low := UInt32(-1);   // trigger a new sub-table when Len > Root
  Used := UInt32(1) shl Root;
  Mask := Used - 1;

  if ((CodeType = ctLens) and (Used > ENOUGH_LENS)) or
     ((CodeType = ctDists) and (Used > ENOUGH_DISTS)) then
    Exit(1);

  repeat
    Here.Bits := Len - Drop;
    if (UInt32(Work[Sym]) + 1 < Match) then
    begin
      Here.Op := 0;
      Here.Val := Work[Sym];
    end
    else if (Work[Sym] >= Match) then
    begin
      Here.Op := Byte(Extra[Work[Sym] - Match]);
      Here.Val := Vals[Work[Sym] - Match];
    end else
    begin
      Here.Op := 32 + 64;
      Here.Val := 0;
    end;

    // replicate for those indices with low Len bits equal to Huff
    Incr := UInt32(1) shl (Len - Drop);
    Fill := UInt32(1) shl Curr;
    Min := Fill; // save offset to next table
    repeat
      Dec(Fill, Incr);
      Next[(Huff shr Drop) + Fill] := Here;
    until (Fill = 0);

    // backwards increment the Len bit code Huff
    Incr := UInt32(1) shl (Len - 1);
    while (Huff and Incr <> 0) do
      Incr := Incr shr 1;
    if (Incr <> 0) then
    begin
      Huff := Huff and (Incr - 1);
      Inc(Huff, Incr);
    end else
      Huff := 0;

    Inc(Sym);
    Dec(Count[Len]);
    if (Count[Len] = 0) then
    begin
      if (Len = Max) then
        Break;
      Len := Lens[Work[Sym]];
    end;

    if (UInt32(Len) > Root) and ((Huff and Mask) <> Low) then
    begin
      if (Drop = 0) then
        Drop := Root;

      Inc(Next, Min); // here Min is 1 shl Curr

      // look ahead at the length counts for the size the sub-table needs
      Curr := UInt32(Len) - Drop;
      Left := 1 shl Curr;
      while (Curr + Drop < UInt32(Max)) do
      begin
        Dec(Left, Count[Curr + Drop]);
        if (Left <= 0) then
          Break;
        Inc(Curr);
        Left := Left shl 1;
      end;

      Inc(Used, UInt32(1) shl Curr);
      if ((CodeType = ctLens) and (Used > ENOUGH_LENS)) or
         ((CodeType = ctDists) and (Used > ENOUGH_DISTS)) then
        Exit(1);

      Low := Huff and Mask;
      Base[Low].Op := Curr;
      Base[Low].Bits := Root;
      Base[Low].Val := (PtrUInt(Next) - PtrUInt(Base)) div SizeOf(TCode);
    end;
  until False;

  // an incomplete code has at most one entry left over, since one bit is the
  // longest length that gets this far - which also means Drop is still 0 here
  if (Huff <> 0) then
  begin
    Here.Op := 64;
    Here.Bits := Len - Drop;
    Here.Val := 0;
    Next[Huff] := Here;
  end;

  Inc(Table, Used);
  Bits := Root;
  Result := 0;
end;

type
  TInflateMode = (imType, imStored, imCopy, imTable, imLenLens, imCodeLens,
                  imLen, imLenExt, imDist, imDistExt, imMatch, imLit, imDone);

  TInflateState = record
    Src, SrcEnd: PByte;
    BitBuf: UInt32;
    BitCount: Int32;

    Dst: PByte;
    DstPos, DstStart, DstCap: PtrUInt;

    Mode: TInflateMode;
    Last: Boolean;
    Len, Dist, Extra: UInt32;
    NLen, NDist, NCode, Have: Int32;

    LenCode, DistCode, Next: PCode;
    LenBits, DistBits: Int32;
    Lens: array[0..319] of UInt16;
    Work: array[0..287] of UInt16;
    Codes: array[0..ENOUGH-1] of TCode;

    procedure PullByte; inline;
    procedure NeedBits(Count: Int32); inline;
    function BitsOf(Count: Int32): UInt32; inline;
    procedure DropBits(Count: Int32); inline;
    procedure ByteBits; inline;
    procedure EnsureOut(Room: PtrUInt);
    procedure Run;
  end;

// zlib returns for more input here; a whole buffer is all the input there is
procedure TInflateState.PullByte;
begin
  if (Src >= SrcEnd) then
    CompressCodecException('deflate stream is truncated');
  BitBuf := BitBuf or (UInt32(Src^) shl BitCount);
  Inc(Src);
  Inc(BitCount, 8);
end;

procedure TInflateState.NeedBits(Count: Int32);
begin
  while (BitCount < Count) do
    PullByte();
end;

function TInflateState.BitsOf(Count: Int32): UInt32;
begin
  Result := BitBuf and ((UInt32(1) shl Count) - 1);
end;

procedure TInflateState.DropBits(Count: Int32);
begin
  BitBuf := BitBuf shr Count;
  Dec(BitCount, Count);
end;

procedure TInflateState.ByteBits;
begin
  BitBuf := BitBuf shr (BitCount and 7);
  Dec(BitCount, BitCount and 7);
end;

procedure TInflateState.EnsureOut(Room: PtrUInt);
begin
  if (DstPos + Room > DstCap) then
  begin
    if (DstCap < GROW_MIN) then
      DstCap := GROW_MIN;
    while (DstCap < DstPos + Room) do
      DstCap := DstCap * 2;
    ReAllocMem(Dst, DstCap);
  end;
end;

procedure TInflateState.Run;
var
  Here, Prev: TCode;
  Val, Count: UInt32;
  I: Int32;
  From, Put: PByte;
begin
  Mode := imType;
  Last := False;

  while (Mode <> imDone) do
    case Mode of
      imType:
        if Last then
        begin
          ByteBits();
          Mode := imDone;
        end else
        begin
          NeedBits(3);
          Last := BitsOf(1) <> 0;
          DropBits(1);
          case BitsOf(2) of
            0: Mode := imStored;
            1: begin
                 LenCode := @LenFix[0];
                 LenBits := 9;
                 DistCode := @DistFix[0];
                 DistBits := 5;
                 Mode := imLen;
               end;
            2: Mode := imTable;
          else
            CompressCodecException('invalid block type');
          end;
          DropBits(2);
        end;

      imStored:
        begin
          ByteBits();
          NeedBits(32);
          if ((BitBuf and $FFFF) <> ((BitBuf shr 16) xor $FFFF)) then
            CompressCodecException('invalid stored block lengths');
          Len := BitBuf and $FFFF;
          BitBuf := 0;
          BitCount := 0;
          Mode := imCopy;
        end;

      imCopy:
        begin
          if (Len > PtrUInt(SrcEnd - Src)) then
            CompressCodecException('deflate stream is truncated');
          if (Len > 0) then
          begin
            EnsureOut(Len);
            Move(Src^, (Dst + DstPos)^, Len);
            Inc(Src, Len);
            Inc(DstPos, Len);
          end;
          Mode := imType;
        end;

      imTable:
        begin
          NeedBits(14);
          NLen := BitsOf(5) + 257;
          DropBits(5);
          NDist := BitsOf(5) + 1;
          DropBits(5);
          NCode := BitsOf(4) + 4;
          DropBits(4);
          if (NLen > 286) or (NDist > 30) then
            CompressCodecException('too many length or distance symbols');
          Have := 0;
          Mode := imLenLens;
        end;

      imLenLens:
        begin
          while (Have < NCode) do
          begin
            NeedBits(3);
            Lens[CL_ORDER[Have]] := BitsOf(3);
            Inc(Have);
            DropBits(3);
          end;
          while (Have < CL_CODES) do
          begin
            Lens[CL_ORDER[Have]] := 0;
            Inc(Have);
          end;

          Next := @Codes[0];
          LenCode := Next;
          DistCode := Next;
          LenBits := 7;
          if (InflateTable(ctCodes, @Lens[0], CL_CODES, Next, LenBits, @Work[0]) <> 0) then
            CompressCodecException('invalid code lengths set');
          Have := 0;
          Mode := imCodeLens;
        end;

      imCodeLens:
        begin
          while (Have < NLen + NDist) do
          begin
            repeat
              Here := LenCode[BitsOf(LenBits)];
              if (Here.Bits <= BitCount) then
                Break;
              PullByte();
            until False;

            if (Here.Val < 16) then
            begin
              DropBits(Here.Bits);
              Lens[Have] := Here.Val;
              Inc(Have);
            end else
            begin
              if (Here.Val = 16) then
              begin
                NeedBits(Here.Bits + 2);
                DropBits(Here.Bits);
                if (Have = 0) then
                  CompressCodecException('invalid bit length repeat');
                Val := Lens[Have - 1];
                Count := 3 + BitsOf(2);
                DropBits(2);
              end
              else if (Here.Val = 17) then
              begin
                NeedBits(Here.Bits + 3);
                DropBits(Here.Bits);
                Val := 0;
                Count := 3 + BitsOf(3);
                DropBits(3);
              end else
              begin
                NeedBits(Here.Bits + 7);
                DropBits(Here.Bits);
                Val := 0;
                Count := 11 + BitsOf(7);
                DropBits(7);
              end;

              if (Have + Int32(Count) > NLen + NDist) then
                CompressCodecException('invalid bit length repeat');
              while (Count > 0) do
              begin
                Lens[Have] := Val;
                Inc(Have);
                Dec(Count);
              end;
            end;
          end;

          // a block with no way to say "end of block" can only run off the end
          if (Lens[256] = 0) then
            CompressCodecException('invalid code -- missing end-of-block');

          Next := @Codes[0];
          LenCode := Next;
          LenBits := LEN_ROOT;
          if (InflateTable(ctLens, @Lens[0], NLen, Next, LenBits, @Work[0]) <> 0) then
            CompressCodecException('invalid literal/lengths set');
          DistCode := Next;
          DistBits := DIST_ROOT;
          if (InflateTable(ctDists, @Lens[NLen], NDist, Next, DistBits, @Work[0]) <> 0) then
            CompressCodecException('invalid distances set');
          Mode := imLen;
        end;

      imLen:
        begin
          repeat
            Here := LenCode[BitsOf(LenBits)];
            if (Here.Bits <= BitCount) then
              Break;
            PullByte();
          until False;

          if (Here.Op <> 0) and (Here.Op and $F0 = 0) then
          begin
            Prev := Here;
            repeat
              Here := LenCode[Prev.Val + (BitsOf(Prev.Bits + Prev.Op) shr Prev.Bits)];
              if (Prev.Bits + Here.Bits <= BitCount) then
                Break;
              PullByte();
            until False;
            DropBits(Prev.Bits);
          end;
          DropBits(Here.Bits);

          Len := Here.Val;
          if (Here.Op = 0) then
            Mode := imLit
          else if (Here.Op and 32 <> 0) then
            Mode := imType
          else if (Here.Op and 64 <> 0) then
            CompressCodecException('invalid literal/length code')
          else
          begin
            Extra := Here.Op and 15;
            Mode := imLenExt;
          end;
        end;

      imLenExt:
        begin
          if (Extra <> 0) then
          begin
            NeedBits(Extra);
            Inc(Len, BitsOf(Extra));
            DropBits(Extra);
          end;
          Mode := imDist;
        end;

      imDist:
        begin
          repeat
            Here := DistCode[BitsOf(DistBits)];
            if (Here.Bits <= BitCount) then
              Break;
            PullByte();
          until False;

          if (Here.Op and $F0 = 0) then
          begin
            Prev := Here;
            repeat
              Here := DistCode[Prev.Val + (BitsOf(Prev.Bits + Prev.Op) shr Prev.Bits)];
              if (Prev.Bits + Here.Bits <= BitCount) then
                Break;
              PullByte();
            until False;
            DropBits(Prev.Bits);
          end;
          DropBits(Here.Bits);

          if (Here.Op and 64 <> 0) then
            CompressCodecException('invalid distance code');
          Dist := Here.Val;
          Extra := Here.Op and 15;
          Mode := imDistExt;
        end;

      imDistExt:
        begin
          if (Extra <> 0) then
          begin
            NeedBits(Extra);
            Inc(Dist, BitsOf(Extra));
            DropBits(Extra);
          end;
          Mode := imMatch;
        end;

      imMatch:
        begin
          // whave is zero when a stream is decoded in one pass, so zlib's window
          // branch is only ever a distance reaching before this stream's output
          if (Dist > DstPos - DstStart) then
            CompressCodecException('invalid distance too far back');

          EnsureOut(Len);
          Put := Dst + DstPos;
          From := Put - Dist;
          Inc(DstPos, Len);
          if (Dist >= Len) then
            Move(From^, Put^, Len)
          else
            for I := 0 to Len - 1 do
              Put[I] := From[I];
          Mode := imLen;
        end;

      imLit:
        begin
          EnsureOut(1);
          (Dst + DstPos)^ := Byte(Len);
          Inc(DstPos);
          Mode := imLen;
        end;
    end;
end;

procedure Inflate(InData: PByte; InSize: PtrUInt; var OutData: PByte; OutOffset: PtrUInt; out OutSize: Int64; out InUsed: PtrUInt);
var
  State: TInflateState;
begin
  State := Default(TInflateState);
  State.Src := InData;
  State.SrcEnd := InData + InSize;
  State.Dst := OutData;
  State.DstPos := OutOffset;
  State.DstStart := OutOffset;
  if (OutData <> nil) then
    State.DstCap := MemSize(OutData);

  try
    State.EnsureOut(InSize * 2);
    State.Run();
  finally
    OutData := State.Dst;
  end;

  OutSize := State.DstPos;

  // whole bytes still sitting in the bit buffer were read but never consumed
  InUsed := PtrUInt(State.Src - InData) - PtrUInt(State.BitCount div 8);
  if (InUsed > InSize) then
    InUsed := InSize;
end;

end.
