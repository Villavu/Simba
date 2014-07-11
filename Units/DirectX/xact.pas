{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  Files:      xact.h, xact2wb.h, xact3D.h                                   *}
{*  Content:    XACT public interfaces, functions and data types;             *}
{*              XACT 2 wave bank definitions; XACT 3D support.                *}
{*                                                                            *}
{*  DirectX 9.0 Delphi / FreePascal adaptation by Alexey Barkovoy             *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://www.clootie.ru                                                   *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*----------------------------------------------------------------------------*}
{*  $Id: xact.pas,v 1.14 2007/04/14 20:57:43 clootie Exp $ }
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

unit xact;

interface


uses
  {$IFDEF XBOX}XAudio, xauddefs{$ELSE}Windows, ActiveX{$ENDIF}, X3DAudio;

type
  UINT32 = Longword;

(***************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       xact2wb.h
 *  Content:    XACT 2 wave bank definitions.
 *
 ****************************************************************************)

{$MINENUMSIZE 1}

const
  WAVEBANK_HEADER_SIGNATURE   = 'DNBW';     // WaveBank  RIFF chunk signature
  WAVEBANK_HEADER_VERSION     = 42;         // Current wavebank file version

  WAVEBANK_BANKNAME_LENGTH    = 64;         // Wave bank friendly name length, in characters
  WAVEBANK_ENTRYNAME_LENGTH   = 64;         // Wave bank entry friendly name length, in characters

  WAVEBANK_MAX_DATA_SEGMENT_SIZE          = $FFFFFFFF;  // Maximum wave bank data segment size, in bytes
  WAVEBANK_MAX_COMPACT_DATA_SEGMENT_SIZE  = $001FFFFF;  // Maximum compact wave bank data segment size, in bytes

type
  WAVEBANKOFFSET = DWORD;
  TWavebankOffset = WAVEBANKOFFSET;

const
  //
  // Bank flags
  //

  WAVEBANK_TYPE_BUFFER         = $00000000;      // In-memory buffer
  WAVEBANK_TYPE_STREAMING      = $00000001;      // Streaming
  WAVEBANK_TYPE_MASK           = $00000001;

  WAVEBANK_FLAGS_ENTRYNAMES    = $00010000;      // Bank includes entry names
  WAVEBANK_FLAGS_COMPACT       = $00020000;      // Bank uses compact format
  WAVEBANK_FLAGS_SYNC_DISABLED = $00040000;      // Bank is disabled for audition sync
  WAVEBANK_FLAGS_SEEKTABLES    = $00080000;      // Bank includes seek tables.
  WAVEBANK_FLAGS_MASK          = $000F0000;

  //
  // Entry flags
  //

  WAVEBANKENTRY_FLAGS_READAHEAD       = $00000001;  // Enable stream read-ahead
  WAVEBANKENTRY_FLAGS_LOOPCACHE       = $00000002;  // One or more looping sounds use this wave
  WAVEBANKENTRY_FLAGS_REMOVELOOPTAIL  = $00000004;  // Remove data after the end of the loop region
  WAVEBANKENTRY_FLAGS_IGNORELOOP      = $00000008;  // Used internally when the loop region can't be used
  WAVEBANKENTRY_FLAGS_MASK            = $00000008;

  //
  // Entry wave format identifiers
  //

  WAVEBANKMINIFORMAT_TAG_PCM      = $0;     // PCM data
  WAVEBANKMINIFORMAT_TAG_XMA      = $1;     // XMA data
  WAVEBANKMINIFORMAT_TAG_ADPCM    = $2;     // ADPCM data

  WAVEBANKMINIFORMAT_BITDEPTH_8   = $0;     // 8-bit data (PCM only)
  WAVEBANKMINIFORMAT_BITDEPTH_16  = $1;     // 16-bit data (PCM only)

  //
  // Arbitrary fixed sizes
  //
  WAVEBANKENTRY_XMASTREAMS_MAX          = 3;   // enough for 5.1 channel audio
  WAVEBANKENTRY_XMACHANNELS_MAX         = 6;   // enough for 5.1 channel audio (cf. XAUDIOCHANNEL_SOURCEMAX)

  //
  // DVD data sizes
  //

  WAVEBANK_DVD_SECTOR_SIZE    = 2048;
  WAVEBANK_DVD_BLOCK_SIZE     = (WAVEBANK_DVD_SECTOR_SIZE * 16);

  //
  // Bank alignment presets
  //

  WAVEBANK_ALIGNMENT_MIN  = 4;                           // Minimum alignment
  WAVEBANK_ALIGNMENT_DVD  = WAVEBANK_DVD_SECTOR_SIZE;    // DVD-optimized alignment

type
  //
  // Wave bank segment identifiers
  //

  //todo: What is size of this ENUM ???
  PWavebankSegIDX = ^TWavebankSegIDX;
  WAVEBANKSEGIDX =
  (
    WAVEBANK_SEGIDX_BANKDATA{= 0},      // Bank data
    WAVEBANK_SEGIDX_ENTRYMETADATA,      // Entry meta-data
    WAVEBANK_SEGIDX_SEEKTABLES,         // Storage for seek tables for the encoded waves.
    WAVEBANK_SEGIDX_ENTRYNAMES,         // Entry friendly names
    WAVEBANK_SEGIDX_ENTRYWAVEDATA       // Entry wave data
    // WAVEBANK_SEGIDX_COUNT
  );
  TWavebankSegIDX = WAVEBANKSEGIDX;

const
  WAVEBANK_SEGIDX_COUNT = Ord(High(TWavebankSegIDX))+1;


  //
  // Endianness
  //

procedure SwapBytes(_dw_: PDWORD); overload;
procedure SwapBytes(w: PWORD); overload;


//
// Wave bank region in bytes.
//
type
  PWavebankRegion = ^TWavebankRegion;
  WAVEBANKREGION = record
    dwOffset:       DWORD;               // Region offset, in bytes.
    dwLength:       DWORD;               // Region length, in bytes.
  end;
  TWavebankRegion = WAVEBANKREGION;

//procedure TWavebankRegion_SwapBytes(var wbr: TWavebankRegion);


//
// Wave bank region in samples.
//

  PWavebankSampleRegion = ^TWavebankSampleRegion;
  WAVEBANKSAMPLEREGION = record
    dwStartSample:       DWORD;         // Start sample for the region.
    dwTotalSamples:      DWORD;         // Region length in samples.
(*
    void SwapBytes(void)
    {
        XACTWaveBank::SwapBytes(dwStartSample);
        XACTWaveBank::SwapBytes(dwTotalSamples);
    } *)
  end;
  TWavebankSampleRegion = WAVEBANKSAMPLEREGION;

//
// Wave bank file header
//
type
  PWavebankHeader = ^TWavebankHeader;
  WAVEBANKHEADER = record
    dwSignature: DWORD;                         // File signature
    dwVersion: DWORD;                           // Version of the tool that created the file
    dwHeaderVersion: DWORD;                     // Version of the file format
    Segments: array[0..WAVEBANK_SEGIDX_COUNT-1] of TWavebankRegion; // Segment lookup table
  end;
  TWavebankHeader = WAVEBANKHEADER;

//procedure TWavebankHeader_SwapBytes(var wbh: TWavebankHeader);


//
// Entry compressed data format
//
type
  PWavebankMiniWaveFormat = ^TWavebankMiniWaveFormat;
  WAVEBANKMINIWAVEFORMAT = record
    //struct
    {
        DWORD       wFormatTag      : 2;        // Format tag
        DWORD       nChannels       : 3;        // Channel count (1 - 6)
        DWORD       nSamplesPerSec  : 18;       // Sampling rate
        DWORD       wBlockAlign     : 8;        // Block alignment
        DWORD       wBitsPerSample  : 1;        // Bits per sample (8 vs. 16, PCM only)
    }
    dwValue:           DWORD;
  end;
  TWavebankMiniWaveFormat = WAVEBANKMINIWAVEFORMAT;

//procedure TWavebankMiniWaveFormat_SwapBytes(var wbmwf: TWavebankMiniWaveFormat);
//function TWavebankMiniWaveFormat_BitsPerSample(): Word;
//function TWavebankMiniWaveFormat_BlockAlign(): DWPRD;


//
// Entry meta-data
//

  PWavebankEntry = ^TWavebankEntry;
  WAVEBANKENTRY = record
    //union
        //struct
        {
            // Entry flags
            DWORD                   dwFlags  :  4;

            // Duration of the wave, in units of one sample.
            // For instance, a ten second long wave sampled
            // at 48KHz would have a duration of 480,000.
            // This value is not affected by the number of
            // channels, the number of bits per sample, or the
            // compression format of the wave.
            DWORD                   Duration : 28;
        }
        dwFlagsAndDuration: DWORD;
    //};

    Format: TWavebankMiniWaveFormat;  // Entry format
    PlayRegion: TWavebankRegion;      // Region within the wave data segment that contains this entry
    LoopRegion: TWavebankSampleRegion;// Region within the wave data (in samples) that should loop.
  end;
  TWavebankEntry = WAVEBANKENTRY;


//
// Compact entry meta-data
//

  PWavebankEntryCompact = ^TWavebankEntryCompact;
  WAVEBANKENTRYCOMPACT = record
    {DWORD       dwOffset            : 21;       // Data offset, in sectors
    DWORD       dwLengthDeviation   : 11;       // Data length deviation, in bytes}
    dwValue: DWORD;
  end;
  TWavebankEntryCompact = WAVEBANKENTRYCOMPACT;


//
// Bank data segment
//

  PWavebankData = ^TWavebankData;
  WAVEBANKDATA = record
    dwFlags: DWORD;                                // Bank flags
    dwEntryCount: DWORD;                           // Number of entries in the bank
    szBankName: array [0..WAVEBANK_BANKNAME_LENGTH-1] of Char;   // Bank friendly name
    dwEntryMetaDataElementSize: DWORD;             // Size of each entry meta-data element, in bytes
    dwEntryNameElementSize: DWORD;                 // Size of each entry name element, in bytes
    dwAlignment: DWORD;                            // Entry alignment, in bytes
    CompactFormat: TWavebankMiniWaveFormat;        // Format data for compact bank
    BuildTime: FILETIME;                           // Build timestamp
  end;
  TWavebankData = WAVEBANKDATA;

{$MINENUMSIZE 4}




(***************************************************************************
 *
 *  Copyright (C) Microsoft Corporation.  All Rights Reserved.
 *
 *  File:       xact.h
 *  Content:    XACT public interfaces, functions and data types
 *
 ****************************************************************************)

{$IFDEF XBOX}
type
  //XBox compatibility types - not needed in real XBox SDK
  PXAudioVoiceOutput = Pointer;
  PXAudioVoiceOutputVolume = Pointer;
  PXACTCueProperties = Pointer;
{$ENDIF}


//------------------------------------------------------------------------------
// XACT class and interface IDs (Version 2.7)
//------------------------------------------------------------------------------
{$IFNDEF XBOX} // XACT COM support only exists on Windows
const
  CLSID_XACTEngine: TGUID = '{cd0d66ec-8057-43f5-acbd-66dfb36fd78c}';
  CLSID_XACTAuditionEngine: TGUID = '{9b94bf7a-ce0f-4c68-8b5e-d062cb3730c3}';
  CLSID_XACTDebugEngine: TGUID = '{1bd54a4b-a1dc-4e4c-92a2-73ed33552148}';
  // will be defined later -> IID_IXACTEngine: TGUID = '{c2f0af68-1f6d-40ed-964f-26256842edc4}';

{$ENDIF}


//------------------------------------------------------------------------------
// Forward Declarations
//------------------------------------------------------------------------------

{$IFNDEF XBOX}
const
  XACT_RENDERER_ID_LENGTH    = $ff;   // Maximum number of characters allowed in the renderer ID
  XACT_RENDERER_NAME_LENGTH  = $ff;   // Maximum number of characters allowed in the renderer display name.

// -----------------------------------------------------------------------------
// Engine Look-Ahead Time
// -----------------------------------------------------------------------------
  XACT_ENGINE_LOOKAHEAD_DEFAULT   = 250;         // Default look-ahead time of 250ms can be used during XACT engine initialization.
{$ENDIF}

// -----------------------------------------------------------------------------
// Cue friendly name length
// -----------------------------------------------------------------------------
  XACT_CUE_NAME_LENGTH        = $FF;
  

type
  IXACTSoundBank = class;
  IXACTWaveBank = class;
  IXACTWave = class;
  IXACTCue = class;

  PXACT_Notification = ^TXACT_Notification;
  PIXACTCue = ^IXACTCue;


//------------------------------------------------------------------------------
// Typedefs
//------------------------------------------------------------------------------

  TXACTIndex = Word;            // All normal indices
  TXACTNotificationType = Byte; // Notification type
  TXACTVariableValue = Single;  // Variable value
  TXACTVariableIndex = Word;    // Variable index
  TXACTCategory = Word;         // Sound category
  TXACTChannel = Byte;          // Audio channel
  TXACTVolume = Single;         // Volume value
  TXACTTime = Longint;          // Time (in ms)
  TXACTPitch = Smallint;        // Pitch value
  TXACTLoopCount = Byte;        // For all loops / recurrences
  TXACTVariationWeight = Byte;  // Variation weight
  TXACTPriority = Byte;         // Sound priority
  TXACTInstanceLimit = Byte;    // Instance limitations

  PXACTIndex = ^TXACTIndex;
  PXACTNotificationType = ^TXACTNotificationType;
  PXACTVariableValue = ^TXACTVariableValue;
  PXACTVariableIndex = ^TXACTVariableIndex;
  PXACTCategory = ^TXACTCategory;
  PXACTChannel = ^TXACTChannel;
  PXACTVolume = ^TXACTVolume;
  PXACTTime = ^TXACTTime;
  PXACTPitch = ^TXACTPitch;
  PXACTLoopCount = ^TXACTLoopCount;
  PXACTVariationWeight = ^TXACTVariationWeight;
  PXACTPriority = ^TXACTPriority;
  PXACTInstanceLimit = ^TXACTInstanceLimit;

  XACTINDEX = TXACTIndex;
  XACTNOTIFICATIONTYPE = TXACTNotificationType;
  XACTVARIABLEVALUE = TXACTVariableValue;
  XACTVARIABLEINDEX = TXACTVariableIndex;
  XACTCATEGORY = TXACTCategory;
  XACTCHANNEL = TXACTChannel;
  XACTVOLUME = TXACTVolume;
  XACTTIME = TXACTTime;
  XACTPITCH = TXACTPitch;
  XACTLOOPCOUNT = TXACTLoopCount;
  XACTVARIATIONWEIGHT = TXACTVariationWeight;
  XACTPRIORITY = TXACTPriority;
  XACTINSTANCELIMIT = TXACTInstanceLimit;


//------------------------------------------------------------------------------
// Standard win32 multimedia definitions
//------------------------------------------------------------------------------

  PWaveFormatEx = ^TWaveFormatEx;
  tWAVEFORMATEX = packed record
    wFormatTag: Word;         { format type }
    nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD;  { sample rate }
    nAvgBytesPerSec: DWORD; { for buffer estimation }
    nBlockAlign: Word;      { block size of data }
    wBitsPerSample: Word;   { number of bits per sample of mono data }
    cbSize: Word;           { the count in bytes of the size of }
  end;

  //
  // The WAVEFORMATEXTENSIBLE structure defines the format of waveform-audio data for formats having more than two channels.
  // This structure is part of the Platform SDK and is not declared in Dsound.h. It is included here for convenience.
  //
  TWaveFormatExtensibleSamples = record
     case byte of
      0: (wValidBitsPerSample : Word);   // bits of precision
      1: (wSamplesPerBlock    : Word);   // valid if wBitsPerSample = 0
      2: (wReserved           : Word);   // If neither applies, set to zero.
  end;

  //Clootie: TWaveFormatExtensible definition is borrowed from DirectShow.pas
  PWaveFormatExtensible = ^TWaveFormatExtensible;
  WAVEFORMATEXTENSIBLE = record
    Format: TWaveFormatEx;
    Samples: TWaveFormatExtensibleSamples;
    dwChannelMask       : DWORD;  // which channels are present in stream
    SubFormat           : TGUID;
  end;
  TWaveFormatExtensible = WAVEFORMATEXTENSIBLE;


// -----------------------------------------------------------------------------
// File IO Callbacks
// -----------------------------------------------------------------------------
  TXACT_ReadFile_Callback = function (hFile: THandle; lpBuffer: Pointer; nNumberOfBytesToRead: DWORD;
      lpNumberOfBytesRead: PDWORD; lpOverlapped: POverlapped): BOOL; stdcall;
  TXACT_GetOverlappedResult_Callback = function (hFile: THandle; lpOverlapped: POverlapped;
      lpNumberOfBytesTransferred: PDWORD; bWait: BOOL): BOOL; stdcall;

  PXACT_FileIO_Callbacks = ^TXACT_FileIO_Callbacks;
  XACT_FILEIO_CALLBACKS = record
    readFileCallback: TXACT_ReadFile_Callback;
    getOverlappedResultCallback: TXACT_GetOverlappedResult_Callback;
  end;
  TXACT_FileIO_Callbacks = XACT_FILEIO_CALLBACKS;

// -----------------------------------------------------------------------------
// Notification Callback
// -----------------------------------------------------------------------------
  TXACT_Notification_Callback = procedure (const pNotification: PXACT_Notification); stdcall;

{$IFNDEF XBOX}
// -----------------------------------------------------------------------------
// Renderer Details
// -----------------------------------------------------------------------------
  PXACT_Renderer_Details = ^TXACT_Renderer_Details;
  XACT_RENDERER_DETAILS = record
    rendererID: array[0..XACT_RENDERER_ID_LENGTH-1] of WideChar;       // The string ID for the rendering device.
    displayName: array[0..XACT_RENDERER_NAME_LENGTH-1] of WideChar;    // A friendly name suitable for display to a human.
    defaultDevice: BOOL;                                               // Set to TRUE if this device is the primary audio device on the system.
  end;
  TXACT_Renderer_Details = XACT_RENDERER_DETAILS;
  TXACTRendererDetails = XACT_RENDERER_DETAILS;
  PXACTRendererDetails = PXACT_Renderer_Details;
{$ENDIF}

// -----------------------------------------------------------------------------
// Runtime (engine) parameters
// -----------------------------------------------------------------------------
  PXACT_Runtime_Parameters = ^TXACT_Runtime_Parameters;
  XACT_RUNTIME_PARAMETERS = record
    lookAheadTime:                 DWORD;   // Time in ms
    pGlobalSettingsBuffer:         Pointer; // Buffer containing the global settings file
    globalSettingsBufferSize:      DWORD;   // Size of global settings buffer
    globalSettingsFlags:           DWORD;   // Flags for global settings
    globalSettingsAllocAttributes: DWORD;   // Global settings buffer allocation attributes (see XMemAlloc)
    fileIOCallbacks:               TXACT_FileIO_Callbacks;      // File I/O callbacks
    fnNotificationCallback:        TXACT_Notification_Callback; // Callback that receives notifications.
  {$IFNDEF XBOX}
    pRendererID:                   PWideChar;                   // Ptr to the ID for the audio renderer the engine should connect to.
  {$ENDIF}
  end;
  TXACT_Runtime_Parameters = XACT_RUNTIME_PARAMETERS;

//------------------------------------------------------------------------------
// Streaming Parameters
//------------------------------------------------------------------------------

  PXACT_Streaming_Parameters = ^TXACT_Streaming_Parameters;
  PXACT_Wavebank_Streaming_Parameters = ^TXACT_Wavebank_Streaming_Parameters;
  XACT_STREAMING_PARAMETERS = record
    file_: THandle;         // File handle associated with wavebank data
    offset: DWORD;          // Offset within file of wavebank header (must be sector aligned)
    flags: DWORD;           // Flags (none currently)
    packetSize: Word;       // Stream packet size (in sectors) to use for each stream (min = 2)
                            //   number of sectors (DVD = 2048 bytes: 2 = 4096, 3 = 6144, 4 = 8192 etc.)
                            //   optimal DVD size is a multiple of 16 (DVD block = 16 DVD sectors)
  end;
  XACT_WAVEBANK_STREAMING_PARAMETERS = XACT_STREAMING_PARAMETERS;
  TXACT_Streaming_Parameters = XACT_STREAMING_PARAMETERS;
  TXACT_Wavebank_Streaming_Parameters = XACT_STREAMING_PARAMETERS;

  // Structure used to report cue properties back to the client.
  PXACT_Cue_Properties = ^TXACT_Cue_Properties;
  XACT_CUE_PROPERTIES = record
    friendlyName: array [0..XACT_CUE_NAME_LENGTH-1] of Char; // Empty if the soundbank doesn't contain any friendly names
    interactive: BOOL;                    // TRUE if an IA cue; FALSE otherwise
    iaVariableIndex: TXACTIndex;          // Only valid for IA cues; XACTINDEX_INVALID otherwise
    numVariations: TXACTIndex;            // Number of variations in the cue
    maxInstances: TXACTInstanceLimit;     // Number of maximum instances for this cue
    currentInstances: TXACTInstanceLimit; // Current active instances of this cue
  end;
  TXACT_Cue_Properties = XACT_CUE_PROPERTIES;

  // Strucutre used to return the track properties.
  PXACT_Track_Properties = ^TXACT_Track_Properties;
  XACT_TRACK_PROPERTIES = record
    duration: TXACTTime;                  // Duration of the track in ms
    numVariations: TXACTIndex;            // Number of wave variations in the track
    numChannels: TXACTChannel;            // Number of channels for the active wave variation on this track
    waveVariation: TXACTIndex;            // Index of the active wave variation
    loopCount: TXACTLoopCount;            // Current loop count on this track
  end;
  TXACT_Track_Properties = XACT_TRACK_PROPERTIES;

  // Structure used to return the properties of a variation.
  PXACT_Variation_Properties = ^TXACT_Variation_Properties;
  XACT_VARIATION_PROPERTIES = record
    index: TXACTIndex;                    // Index of the variation in the cue's variation list
    weight: TXACTVariationWeight;         // Weight for the active variation. Valid only for complex cues
    iaVariableMin: TXACTVariableValue;    // Valid only for IA cues
    iaVariableMax: TXACTVariableValue;    // Valid only for IA cues
    linger: BOOL;                         // Valid only for IA cues
  end;
  TXACT_Variation_Properties = XACT_VARIATION_PROPERTIES;

  // Structure used to return the properties of the sound referenced by a variation.
  PXACT_Sound_Properties = ^TXACT_Sound_Properties;
  XACT_SOUND_PROPERTIES = record
    category: TXACTCategory;              // Category this sound belongs to
    priority: Byte;                       // Priority of this variation
    pitch: TXACTPitch;                    // Current pitch set on the active variation
    volume: TXACTVolume;                  // Current volume set on the active variation
    numTracks: TXACTIndex;                // Number of tracks in the active variation
    arrTrackProperties: array [0..0] of TXACT_Track_Properties; // Array of active track properties (has numTracks number of elements)
  end;
  TXACT_Sound_Properties = XACT_SOUND_PROPERTIES;

  // Structure used to return the properties of the active variation and the sound referenced.
  PXACT_Sound_Variation_Properties = ^TXACT_Sound_Variation_Properties;
  XACT_SOUND_VARIATION_PROPERTIES = record
    variationProperties: TXACT_Variation_Properties; // Properties for this variation
    soundProperties: TXACT_Sound_Properties;         // Proeprties for the sound referenced by this variation
  end;
  TXACT_Sound_Variation_Properties = XACT_SOUND_VARIATION_PROPERTIES;

  // Structure used to return the properties of an active cue instance.
  PXACT_Cue_Instance_Properties = ^TXACT_Cue_Instance_Properties;
  XACT_CUE_INSTANCE_PROPERTIES = record
    allocAttributes: DWORD;                          // Buffer allocation attributes (see XMemAlloc)
    cueProperties: TXACT_Cue_Properties;             // Properties of the cue that are shared by all instances.
    activeVariationProperties: TXACT_Sound_Variation_Properties; // Properties if the currently active variation.
  end;
  TXACT_Cue_Instance_Properties = XACT_CUE_INSTANCE_PROPERTIES;

  // Structure used to return the common wave properties.
  PXACT_Wave_Properties = ^TXACT_Wave_Properties;
  XACT_WAVE_PROPERTIES = record
    friendlyName: array[0..WAVEBANK_ENTRYNAME_LENGTH-1] of Char; // Friendly name for the wave; empty if the wavebank doesn't contain friendly names.
    format: TWavebankMiniWaveFormat;                 // Format for the wave.
    durationInSamples: DWORD;                        // Duration of the wave in units of one sample
    loopRegion: TWavebankSampleRegion;               // Loop region defined in samples.
    streaming: BOOL;                                 // Set to TRUE if the wave is streaming; FALSE otherwise.
  end;
  TXACT_Wave_Properties = XACT_WAVE_PROPERTIES;

  // Structure used to return the properties specific to a wave instance.
  PXACT_Wave_Instance_Properties = ^TXACT_Wave_Instance_Properties;
  XACT_WAVE_INSTANCE_PROPERTIES = record
    properties:  TXACT_Wave_Properties;              // Static properties common to all the wave instances.
    backgroundMusic: BOOL;                           // Set to TRUE if the wave is tagged as background music; FALSE otherwise.
  end;
  TXACT_Wave_Instance_Properties = XACT_WAVE_INSTANCE_PROPERTIES;


//------------------------------------------------------------------------------
// Channel Mapping / Speaker Panning
//------------------------------------------------------------------------------

  PXACTChannelMapEntry = ^TXACTChannelMapEntry;
  XACTCHANNELMAPENTRY = record
    InputChannel: TXACTChannel;
    OutputChannel: TXACTChannel;
    Volume: TXACTVolume;
  end;
  TXACTChannelMapEntry = XACTCHANNELMAPENTRY;


  PXACTChannelMap = ^TXACTChannelMap;
  XACTCHANNELMAP = record
    EntryCount: TXACTChannel;
    paEntries: PXACTChannelMapEntry;
  end;
  TXACTChannelMap = XACTCHANNELMAP;

  PXACTChannelVolumeEntry = ^TXACTChannelVolumeEntry;
  XACTCHANNELVOLUMEENTRY = record
    EntryIndex: TXACTChannel;
    Volume: TXACTVolume;
  end;
  TXACTChannelVolumeEntry = XACTCHANNELVOLUMEENTRY;

  PXACTChannelVolume = ^TXACTChannelVolume;
  XACTCHANNELVOLUME = record
    EntryCount: TXACTChannel;
    paEntries: PXACTChannelVolumeEntry;
  end;
  TXACTChannelVolume = XACTCHANNELVOLUME;


//------------------------------------------------------------------------------
// Notifications
//------------------------------------------------------------------------------

// Pack the notification structures
// Original C++ header: #pragma pack(push, 1)

  // Notification description used for registering, un-registering and flushing notifications
  PXACT_Notification_Description = ^TXACT_Notification_Description;
  XACT_NOTIFICATION_DESCRIPTION = packed record
    type_: TXACTNotificationType;          // Notification type
    flags: Byte;                           // Flags
    pSoundBank: IXACTSoundBank;            // SoundBank instance
    pWaveBank: IXACTWaveBank;              // WaveBank instance
    pCue: IXACTCue;                        // Cue instance
    pWave: IXACTWave;                      // Wave instance
    cueIndex: TXACTIndex;                  // Cue index
    waveIndex: TXACTIndex;                 // Wave index
    pvContext: Pointer;                    // User context (optional)
  end;
  TXACT_Notification_Description = XACT_NOTIFICATION_DESCRIPTION;

  // Notification structure for all XACTNOTIFICATIONTYPE_CUE* notifications
  PXACT_Notification_Cue = ^TXACT_Notification_Cue;
  XACT_NOTIFICATION_CUE = packed record
    cueIndex:   TXACTIndex;     // Cue index
    pSoundBank: IXACTSoundBank; // SoundBank instance
    pCue:       IXACTCue;       // Cue instance
  end;
  TXACT_Notification_Cue = XACT_NOTIFICATION_CUE;


  // Notification structure for all XACTNOTIFICATIONTYPE_MARKER* notifications
  PXACT_Notification_Marker = ^TXACT_Notification_Marker;
  XACT_NOTIFICATION_MARKER = packed record
    cueIndex:   TXACTIndex;     // Cue index
    pSoundBank: IXACTSoundBank; // SoundBank instance
    pCue:       IXACTCue;       // Cue instance
    marker:     DWORD;          // Marker value
  end;
  TXACT_Notification_Marker = XACT_NOTIFICATION_MARKER;

  // Notification structure for all XACTNOTIFICATIONTYPE_SOUNDBANK* notifications
  PXACT_Notification_SoundBank = ^TXACT_Notification_SoundBank;
  XACT_NOTIFICATION_SOUNDBANK = packed record
    pSoundBank: IXACTSoundBank; // SoundBank instance
  end;
  TXACT_Notification_SoundBank = XACT_NOTIFICATION_SOUNDBANK;


  // Notification structure for all XACTNOTIFICATIONTYPE_WAVEBANK* notifications
  PXACT_Notification_WaveBank = ^TXACT_Notification_WaveBank;
  XACT_NOTIFICATION_WAVEBANK = packed record
    pWaveBank: IXACTWaveBank;  // WaveBank instance
  end;
  TXACT_Notification_WaveBank = XACT_NOTIFICATION_WAVEBANK;


  // Notification structure for all XACTNOTIFICATIONTYPE_*VARIABLE* notifications
  PXACT_Notification_Variable = ^TXACT_Notification_Variable;
  XACT_NOTIFICATION_VARIABLE = packed record
    cueIndex:        TXACTIndex;         // Cue index
    pSoundBank:      IXACTSoundBank;     // SoundBank instance
    pCue:            IXACTCue;           // Cue instance
    variableIndex:   TXACTVariableIndex; // Variable index
    variableValue:   TXACTVariableValue; // Variable value
    local:           BOOL;               // TRUE if a local variable
  end;
  TXACT_Notification_Variable = XACT_NOTIFICATION_VARIABLE;

  // Notification structure for all XACTNOTIFICATIONTYPE_GUI* notifications
  PXACT_Notification_GUI = ^TXACT_Notification_GUI;
  XACT_NOTIFICATION_GUI = packed record
    reserved:   DWORD; // Reserved
  end;
  TXACT_Notification_GUI = XACT_NOTIFICATION_GUI;


  // Notification structure for all XACTNOTIFICATIONTYPE_WAVE* notifications
  PXACT_Notification_Wave = ^TXACT_Notification_Wave;
  XACT_NOTIFICATION_WAVE = packed record
    pWaveBank:  IXACTWaveBank;  // WaveBank
    waveIndex:  TXACTIndex;     // Wave index
    cueIndex:   TXACTIndex;     // Cue index
    pSoundBank: IXACTSoundBank; // SoundBank instance
    pCue:       IXACTCue;       // Cue instance
    pWave:      IXACTWave;      // Wave instance
  end;
  TXACT_Notification_Wave = XACT_NOTIFICATION_WAVE;


  // General notification structure
  XACT_NOTIFICATION = packed record
    type_: TXACTNotificationType;        // Notification type
    timeStamp: Longint;                  // Timestamp of notification (milliseconds)
    pvContext: Pointer;                  // User context (optional)
    case Byte of
      1: (cue:       TXACT_Notification_Cue);        // XACTNOTIFICATIONTYPE_CUE*
      2: (marker:    TXACT_Notification_Marker);     // XACTNOTIFICATIONTYPE_MARKER*
      3: (soundBank: TXACT_Notification_SoundBank);  // XACTNOTIFICATIONTYPE_SOUNDBANK*
      4: (waveBank:  TXACT_Notification_WaveBank);   // XACTNOTIFICATIONTYPE_WAVEBANK*
      5: (variable:  TXACT_Notification_Variable);   // XACTNOTIFICATIONTYPE_VARIABLE*
      6: (gui:       TXACT_Notification_GUI);        // XACTNOTIFICATIONTYPE_GUI*
      7: (wave:      TXACT_Notification_Wave);       // XACTNOTIFICATIONTYPE_WAVE*
  end;
  TXACT_Notification = XACT_NOTIFICATION;

//Original C++ header: #pragma pack(pop)


//------------------------------------------------------------------------------
// IXACTSoundBank
//------------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // To expose FreePascal class as abstract C++ class
  {$INTERFACES CORBA}
  IXACTSoundBank_FPC = interface
    function GetCueIndex(szFriendlyName: PAnsiChar): TXACTIndex; stdcall;
    function GetNumCues(pnNumCues: PXACTIndex): HResult; stdcall;
    function GetCueProperties(nCueIndex: TXACTIndex; pProperties: PXACT_Cue_Properties): HResult; stdcall;
    function Prepare(nCueIndex: TXACTIndex; dwFlags: DWORD; timeOffset: TXACTTime; out ppCue: IXACTCue): HResult; stdcall;
    function Play(nCueIndex: TXACTIndex; dwFlags: DWORD; timeOffset: TXACTTime; ppCue: PIXACTCue): HResult; stdcall;
    function Stop(nCueIndex: TXACTIndex; dwFlags: DWORD): HResult; stdcall;
    function Destroy: HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
  end;
  {$INTERFACES DEFAULT}

  IXACTSoundBank = class(IXACTSoundBank_FPC)
    function GetCueIndex(szFriendlyName: PAnsiChar): TXACTIndex; virtual; stdcall; abstract;
    function GetNumCues(pnNumCues: PXACTIndex): HResult; virtual; stdcall; abstract;
    function GetCueProperties(nCueIndex: TXACTIndex; pProperties: PXACT_Cue_Properties): HResult; virtual; stdcall; abstract;
    function Prepare(nCueIndex: TXACTIndex; dwFlags: DWORD; timeOffset: TXACTTime; out ppCue: IXACTCue): HResult; virtual; stdcall; abstract;
    function Play(nCueIndex: TXACTIndex; dwFlags: DWORD; timeOffset: TXACTTime; ppCue: PIXACTCue): HResult; virtual; stdcall; abstract;
    function Stop(nCueIndex: TXACTIndex; dwFlags: DWORD): HResult; virtual; stdcall; abstract;
    {$WARNINGS OFF}
    function Destroy: HResult; virtual; stdcall; abstract; //Clootie: This method is not related to Delphi TObject.Destroy
    {$WARNINGS ON}
    function GetState(out pdwState: DWORD): HResult; virtual; stdcall; abstract;
  end;


//------------------------------------------------------------------------------
// IXACTWaveBank
//------------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // To expose FreePascal class as abstract C++ class
  {$INTERFACES CORBA}
  IXACTWaveBank_FPC = interface
    function Destroy: HResult; stdcall;
    function GetNumWaves(pnNumWaves: PXACTIndex): HResult; stdcall;
    function GetWaveIndex(szFriendlyName: PAnsiChar): XACTINDEX; stdcall;
    function GetWaveProperties(nWaveIndex: TXACTIndex; pWaveProperties: PXACT_Wave_Properties): HResult; stdcall;
    function Prepare(nWaveIndex: TXACTIndex; dwFlags: DWORD; dwPlayOffset: DWORD; nLoopCount: TXACTLoopCount; out ppWave: IXACTWave): HResult; stdcall;
    function Play(nWaveIndex: TXACTIndex; dwFlags: DWORD; dwPlayOffset: DWORD; nLoopCount: TXACTLoopCount; out ppWave: IXACTWave): HResult; stdcall;
    function Stop(nWaveIndex: TXACTIndex; dwFlags: DWORD): HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
  end;
  {$INTERFACES DEFAULT}

  IXACTWaveBank = class(IXACTWaveBank_FPC)
    {$WARNINGS OFF}
    function Destroy: HResult; virtual; stdcall; abstract; //Clootie: This method is not related to Delphi TObject.Destroy
    {$WARNINGS ON}
    function GetNumWaves(pnNumWaves: PXACTIndex): HResult; virtual; stdcall; abstract;
    function GetWaveIndex(szFriendlyName: PAnsiChar): XACTINDEX; virtual; stdcall; abstract;
    function GetWaveProperties(nWaveIndex: TXACTIndex; pWaveProperties: PXACT_Wave_Properties): HResult; virtual; stdcall; abstract;
    function Prepare(nWaveIndex: TXACTIndex; dwFlags: DWORD; dwPlayOffset: DWORD; nLoopCount: TXACTLoopCount; out ppWave: IXACTWave): HResult; virtual; stdcall; abstract;
    function Play(nWaveIndex: TXACTIndex; dwFlags: DWORD; dwPlayOffset: DWORD; nLoopCount: TXACTLoopCount; out ppWave: IXACTWave): HResult; virtual; stdcall; abstract;
    function Stop(nWaveIndex: TXACTIndex; dwFlags: DWORD): HResult; virtual; stdcall; abstract;
    function GetState(out pdwState: DWORD): HResult; virtual; stdcall; abstract;
  end;


//------------------------------------------------------------------------------
// IXACTWave
//------------------------------------------------------------------------------


  //----------------------------------------------------------------------------
  // To expose FreePascal class as abstract C++ class
  {$INTERFACES CORBA}
  IXACTWave_FPC = interface
    function Destroy: HResult; stdcall;
    function Play: HResult; stdcall;
    function Stop(dwFlags: DWORD): HResult; stdcall;
    function Pause(fPause: BOOL): HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
    function SetPitch(pitch: TXACTPitch): HResult; stdcall;
    function SetVolume(volume: TXACTVolume): HResult; stdcall;
    function SetMatrixCoefficients(uSrcChannelCount: UINT32; uDstChannelCount: UINT32; pMatrixCoefficients: PSingle): HResult; stdcall;
    function GetProperties(out pProperties: TXACT_Wave_Instance_Properties): HResult; stdcall;
  end;
  {$INTERFACES DEFAULT}

  IXACTWave = class(IXACTWave_FPC)
    {$WARNINGS OFF}
    function Destroy: HResult; virtual; stdcall; abstract; //Clootie: This method is not related to Delphi TObject.Destroy
    {$WARNINGS ON}
    function Play: HResult; virtual; stdcall; abstract;
    function Stop(dwFlags: DWORD): HResult; virtual; stdcall; abstract;
    function Pause(fPause: BOOL): HResult; virtual; stdcall; abstract;
    function GetState(out pdwState: DWORD): HResult; virtual; stdcall; abstract;
    function SetPitch(pitch: TXACTPitch): HResult; virtual; stdcall; abstract;
    function SetVolume(volume: TXACTVolume): HResult; virtual; stdcall; abstract;
    function SetMatrixCoefficients(uSrcChannelCount: UINT32; uDstChannelCount: UINT32; pMatrixCoefficients: PSingle): HResult; virtual; stdcall; abstract;
    function GetProperties(out pProperties: TXACT_Wave_Instance_Properties): HResult; virtual; stdcall; abstract;
  end;


//------------------------------------------------------------------------------
// IXACTCue
//------------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // To expose FreePascal class as abstract C++ class
  {$INTERFACES CORBA}
  IXACTCue_FPC = interface
    function Play: HResult; stdcall;
    function Stop(dwFlags: DWORD): HResult; stdcall;
    function GetState(out pdwState: DWORD): HResult; stdcall;
    function Destroy: HResult; stdcall;
    function GetChannelMap(pChannelMap: PXACTChannelMap; BufferSize: DWORD; pRequiredSize: PDWORD): HResult; stdcall;
    function SetChannelMap(const pChannelMap: PXACTChannelMap): HResult; stdcall;
    function GetChannelVolume(const pVolume: PXACTChannelVolume): HResult; stdcall;
    function SetChannelVolume(const pVolume: PXACTChannelVolume): HResult; stdcall;
    function SetMatrixCoefficients(uSrcChannelCount: LongWord; uDstChannelCount: LongWord; pMatrixCoefficients: PSingle): HResult; stdcall;
    function GetVariableIndex(szFriendlyName: PAnsiChar): TXACTVariableIndex; stdcall;
    function SetVariable(nIndex: TXACTVariableIndex; nValue: TXACTVariableValue): HResult; stdcall;
    function GetVariable(nIndex: TXACTVariableIndex; out nValue: TXACTVariableValue): HResult; stdcall;
    function Pause(fPause: BOOL): HResult; stdcall;
    function GetProperties(out ppProperties: TXACT_Cue_Instance_Properties): HResult; stdcall;
  {$IFDEF XBOX}
    function SetVoiceOutput(pVoiceOutput: PXAudioVoiceOutput): HResult; stdcall;
    function SetVoiceOutputVolume(pVolume: PXAudioVoiceOutputVolume): HResult; stdcall;
  {$ENDIF} // _XBOX
  end;
  {$INTERFACES DEFAULT}

  IXACTCue = class(IXACTCue_FPC)
    function Play: HResult; virtual; stdcall; abstract;
    function Stop(dwFlags: DWORD): HResult; virtual; stdcall; abstract;
    function GetState(out pdwState: DWORD): HResult; virtual; stdcall; abstract;
    {$WARNINGS OFF}
    function Destroy: HResult; virtual; stdcall; abstract; //Clootie: This method is not related to Delphi TObject.Destroy
    {$WARNINGS ON}
    function GetChannelMap(pChannelMap: PXACTChannelMap; BufferSize: DWORD; pRequiredSize: PDWORD): HResult; virtual; stdcall; abstract;
    function SetChannelMap(const pChannelMap: PXACTChannelMap): HResult; virtual; stdcall; abstract;
    function GetChannelVolume(const pVolume: PXACTChannelVolume): HResult; virtual; stdcall; abstract;
    function SetChannelVolume(const pVolume: PXACTChannelVolume): HResult; virtual; stdcall; abstract;
    function SetMatrixCoefficients(uSrcChannelCount: LongWord; uDstChannelCount: LongWord; pMatrixCoefficients: PSingle): HResult; virtual; stdcall; abstract;
    function GetVariableIndex(szFriendlyName: PAnsiChar): TXACTVariableIndex; virtual; stdcall; abstract;
    function SetVariable(nIndex: TXACTVariableIndex; nValue: TXACTVariableValue): HResult; virtual; stdcall; abstract;
    function GetVariable(nIndex: TXACTVariableIndex; out nValue: TXACTVariableValue): HResult; virtual; stdcall; abstract;
    function Pause(fPause: BOOL): HResult; virtual; stdcall; abstract;
    function GetProperties(out ppProperties: TXACT_Cue_Instance_Properties): HResult; virtual; stdcall; abstract;
  {$IFDEF XBOX}
    function SetVoiceOutput(pVoiceOutput: PXAudioVoiceOutput): HResult; virtual; stdcall; abstract;
    function SetVoiceOutputVolume(pVolume: PXAudioVoiceOutputVolume): HResult; virtual; stdcall; abstract;
  {$ENDIF} // _XBOX
  end;


//------------------------------------------------------------------------------
// IXACTEngine
//------------------------------------------------------------------------------

  {$IFDEF XBOX}
  IXACTEngine = interface // !CORBA!
    function AddRef: Cardinal; stdcall;
    function Release: Cardinal; stdcall;
  {$ELSE}
  IXACTEngine = interface(IUnknown)
    ['{c2f0af68-1f6d-40ed-964f-26256842edc4}']
  {$ENDIF}
  {$IFNDEF XBOX}
    function GetRendererCount(out pnRendererCount: TXACTIndex): HResult; stdcall;
    function GetRendererDetails(nRendererIndex: TXACTIndex; out pRendererDetails: TXACTRendererDetails): HResult; stdcall;
  {$ENDIF}

    function GetFinalMixFormat(out pFinalMixFormat: TWaveFormatExtensible): HResult; stdcall;
    function Initialize(const pParams: TXACT_Runtime_Parameters): HResult; stdcall;
    function ShutDown: HResult; stdcall;

    function DoWork: HResult; stdcall;

    function CreateSoundBank(const pvBuffer: Pointer; dwSize: DWORD; dwFlags: DWORD; dwAllocAttributes: DWORD; out ppSoundBank: IXACTSoundBank): HResult; stdcall;
    function CreateInMemoryWaveBank(const pvBuffer: Pointer; dwSize: DWORD; dwFlags: DWORD; dwAllocAttributes: DWORD; out ppWaveBank: IXACTWaveBank): HResult; stdcall;
    function CreateStreamingWaveBank(const pParms: TXACT_Wavebank_Streaming_Parameters; out ppWaveBank: IXACTWaveBank): HResult; stdcall;

    function PrepareWave(dwFlags: DWORD; szWavePath: PAnsiChar; wStreamingPacketSize: Word; dwAlignment: DWORD; dwPlayOffset: DWORD; nLoopCount: XACTLOOPCOUNT; out ppWave: IXACTWave): HResult; stdcall;
    function PrepareInMemoryWave(dwFlags: DWORD; entry: TWavebankEntry; pdwSeekTable: PDWORD; pbWaveData: PByte; dwPlayOffset: DWORD; nLoopCount: XACTLOOPCOUNT; out ppWave: IXACTWave): HResult; stdcall;
    function PrepareStreamingWave(dwFlags: DWORD; entry: TWavebankEntry; streamingParams: TXACT_Streaming_Parameters; dwAlignment: DWORD; pdwSeekTable: PDWORD; dwPlayOffset: DWORD; nLoopCount: TXACTLoopCount; out ppWave: IXACTWave): HResult; stdcall;

    function RegisterNotification(const pNotificationDesc: TXACT_Notification_Description): HResult; stdcall;
    function UnRegisterNotification(const pNotificationDesc: TXACT_Notification_Description): HResult; stdcall;

    function GetCategory(szFriendlyName: PAnsiChar): TXACTCategory; stdcall;
    function Stop(nCategory: TXACTCategory; dwFlags: DWORD): HResult; stdcall;
    function SetVolume(nCategory: TXACTCategory; nVolume: TXACTVolume): HResult; stdcall;
    function Pause(nCategory: TXACTCategory; fPause: BOOL): HResult; stdcall;

    function GetGlobalVariableIndex(szFriendlyName: PAnsiChar): TXACTVariableIndex; stdcall;
    function SetGlobalVariable(nIndex: TXACTVariableIndex; nValue: TXACTVariableValue): HResult; stdcall;
    function GetGlobalVariable(nIndex: TXACTVariableIndex; out nValue: TXACTVariableValue): HResult; stdcall;
  end;
  IID_IXACTEngine = IXACTEngine;


//------------------------------------------------------------------------------
// XACT API's (these are deprecated and will be removed in a future release)
//------------------------------------------------------------------------------

{$IFDEF XBOX}
(*
#define XACT_FLAG_API_CREATE_MANAGEDATA     XACT_FLAG_MANAGEDATA
#define XACT_FLAG_API_STOP_IMMEDIATE        XACT_FLAG_STOP_IMMEDIATE

STDAPI XACTInitialize(const XACT_RUNTIME_PARAMETERS* pParams);
STDAPI XACTShutDown(void);
STDAPI XACTDoWork(void);

STDAPI XACTCreateSoundBank(const void* pvBuffer, DWORD dwSize, DWORD dwFlags, DWORD dwAllocAttributes, IXACTSoundBank** ppSoundBank);
STDAPI XACTCreateInMemoryWaveBank(const void* pvBuffer, DWORD dwSize, DWORD dwFlags, DWORD dwAllocAttributes, IXACTWaveBank** ppWaveBank);
STDAPI XACTCreateStreamingWaveBank(const XACT_WAVEBANK_STREAMING_PARAMETERS* pParms, IXACTWaveBank** ppWaveBank);

STDAPI XACTRegisterNotification(const XACT_NOTIFICATION_DESCRIPTION* pNotificationDesc);
STDAPI XACTUnRegisterNotification(const XACT_NOTIFICATION_DESCRIPTION* pNotificationDesc);

STDAPI_(XACTCATEGORY) XACTGetCategory(PCSTR szFriendlyName);
STDAPI XACTStop(XACTCATEGORY nCategory, DWORD dwFlags);
STDAPI XACTSetVolume(XACTCATEGORY nCategory, XACTVOLUME nVolume);
STDAPI XACTPause(XACTCATEGORY nCategory, BOOL fPause);

STDAPI_(XACTVARIABLEINDEX) XACTGetGlobalVariableIndex(PCSTR szFriendlyName);
STDAPI XACTSetGlobalVariable(XACTVARIABLEINDEX nIndex, XACTVARIABLEVALUE nValue);
STDAPI XACTGetGlobalVariable(XACTVARIABLEINDEX nIndex, XACTVARIABLEVALUE* pnValue);
*)
{$ENDIF XBOX}

//------------------------------------------------------------------------------
// Create Engine
//------------------------------------------------------------------------------

// Flags used only in XACTCreateEngine below.  These flags are valid but ignored
// when building for Xbox 360; to enable auditioning on that platform you must
// link explicitly to an auditioning version of the XACT static library.
const
  XACT_FLAG_API_AUDITION_MODE = DWORD($00000001);
  XACT_FLAG_API_DEBUG_MODE    = DWORD($00000002);

function XACTCreateEngine(dwCreationFlags: DWORD; out ppEngine: IXACTEngine): HRESULT; stdcall; // inline

{$IFNDEF XBOX}
const
  XACT_DEBUGENGINE_REGISTRY_KEY   = 'Software\Microsoft\XACT';
  XACT_DEBUGENGINE_REGISTRY_VALUE = 'DebugEngine';
{$ENDIF}

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Standard win32 multimedia definitions
//------------------------------------------------------------------------------
const
  WAVE_FORMAT_IEEE_FLOAT = $0003;

  WAVE_FORMAT_EXTENSIBLE = $FFFE;

//------------------------------------------------------------------------------
// Constants
//------------------------------------------------------------------------------

const
  LONG_MIN = (-2147483647-1);
  LONG_MAX = 2147483647;

const
  XACTTIME_MIN                    = TXACTTime(LONG_MIN);
  XACTTIME_MAX                    = TXACTTime(LONG_MAX); // 24 days 20:31:23.647
  XACTTIME_INFINITE               = TXACTTime(LONG_MAX);
  XACTINSTANCELIMIT_INFINITE      = TXACTInstanceLimit($ff);
  XACTINSTANCELIMIT_MIN           = TXACTInstanceLimit($00); // == 1 instance total (0 additional instances)
  XACTINSTANCELIMIT_MAX           = TXACTInstanceLimit($fe); // == 255 instances total (254 additional instances)

  XACTINDEX_MIN = TXACTIndex($0);
  XACTINDEX_MAX = TXACTIndex($fffe);
  XACTINDEX_INVALID = TXACTIndex($ffff);

  XACTNOTIFICATIONTYPE_MIN = TXACTNotificationType($00);
  XACTNOTIFICATIONTYPE_MAX = TXACTNotificationType($ff);

  XACTVARIABLEVALUE_MIN: TXACTVariableValue = -3.402823466e+38; // -FLT_MAX;
  XACTVARIABLEVALUE_MAX: TXACTVariableValue = 3.402823466e+38; // FLT_MAX;

  XACTVARIABLEINDEX_MIN               = TXACTVariableIndex($0000);
  XACTVARIABLEINDEX_MAX               = TXACTVariableIndex($fffe);
  XACTVARIABLEINDEX_INVALID           = TXACTVariableIndex($ffff);

  XACTCATEGORY_MIN     = TXACTCategory($0);
  XACTCATEGORY_MAX     = TXACTCategory($fffe);
  XACTCATEGORY_INVALID = TXACTCategory($ffff);

  XACTCHANNEL_MIN = TXACTChannel(0);
  XACTCHANNEL_MAX = TXACTChannel($FF);

  XACTPITCH_MIN                   = TXACTPitch(-1200);
  XACTPITCH_MAX                   = TXACTPitch( 1200);

  XACTVOLUME_MIN = {TXACTVolume}(0.0);
  XACTVOLUME_MAX = {TXACTVolume}(3.402823466e+38); // FLT_MAX;

  XACTPARAMETERVALUE_MIN: TXACTVariableValue = -3.402823466e+38; // -FLT_MAX;
  XACTPARAMETERVALUE_MAX: TXACTVariableValue = 3.402823466e+38; // FLT_MAX;

  XACTLOOPCOUNT_MIN               = TXACTLoopCount(0);
  XACTLOOPCOUNT_MAX               = TXACTLoopCount($fe);
  XACTLOOPCOUNT_INFINITE          = TXACTLoopCount($ff);
  XACTWAVEALIGNMENT_MIN           = 2048;

{$IFDEF XBOX}
  XACTMAXOUTPUTVOICECOUNT: TXAudioVoiceIndex = 3;
{$ENDIF} // XBOX

// -----------------------------------------------------------------------------
// Current Content Tool Version
// -----------------------------------------------------------------------------
  XACT_CONTENT_VERSION        = 42;

// -----------------------------------------------------------------------------
// XACT Stop Flags
// -----------------------------------------------------------------------------
  XACT_FLAG_STOP_RELEASE       = DWORD($00000000); // Stop with release envelope (or as authored), for looping waves this acts as break loop.
  XACT_FLAG_STOP_IMMEDIATE     = DWORD($00000001); // Stop immediately

// -----------------------------------------------------------------------------
// XACT Manage Data Flag - XACT will manage the lifetime of this data
// -----------------------------------------------------------------------------
  XACT_FLAG_MANAGEDATA         = DWORD($00000001);

// -----------------------------------------------------------------------------
// XACT Content Preparation Flags
// -----------------------------------------------------------------------------
  XACT_FLAG_BACKGROUND_MUSIC   = DWORD($00000002); // Marks the waves as background music.
  XACT_FLAG_UNITS_MS           = DWORD($00000004); // Indicates that the units passed in are in milliseconds.
  XACT_FLAG_UNITS_SAMPLES      = DWORD($00000008); // Indicates that the units passed in are in samples.

// -----------------------------------------------------------------------------
// XACT State flags
// -----------------------------------------------------------------------------
  XACT_STATE_CREATED           = DWORD($00000001); // Created, but nothing else
  XACT_STATE_PREPARING         = DWORD($00000002); // In the middle of preparing
  XACT_STATE_PREPARED          = DWORD($00000004); // Prepared, but not yet played
  XACT_STATE_PLAYING           = DWORD($00000008); // Playing (though could be paused)
  XACT_STATE_STOPPING          = DWORD($00000010); // Stopping
  XACT_STATE_STOPPED           = DWORD($00000020); // Stopped
  XACT_STATE_PAUSED            = DWORD($00000040); // Paused (Can be combined with some of the other state flags above)
  XACT_STATE_INUSE             = DWORD($00000080); // Object is in use (used by wavebanks and soundbanks).
  XACT_STATE_PREPAREFAILED     = DWORD($80000000); // Object preparation failed.

//------------------------------------------------------------------------------
// XACT Parameters
//------------------------------------------------------------------------------

  XACT_FLAG_GLOBAL_SETTINGS_MANAGEDATA    = XACT_FLAG_MANAGEDATA;

//------------------------------------------------------------------------------
// Notifications
//------------------------------------------------------------------------------

  XACTNOTIFICATIONTYPE_CUEPREPARED           = TXACTNotificationType(1);  // None, SoundBank, SoundBank & cue index, cue instance
  XACTNOTIFICATIONTYPE_CUEPLAY               = TXACTNotificationType(2);  // None, SoundBank, SoundBank & cue index, cue instance
  XACTNOTIFICATIONTYPE_CUESTOP               = TXACTNotificationType(3);  // None, SoundBank, SoundBank & cue index, cue instance
  XACTNOTIFICATIONTYPE_CUEDESTROYED          = TXACTNotificationType(4);  // None, SoundBank, SoundBank & cue index, cue instance
  XACTNOTIFICATIONTYPE_MARKER                = TXACTNotificationType(5);  // None, SoundBank, SoundBank & cue index, cue instance
  XACTNOTIFICATIONTYPE_SOUNDBANKDESTROYED    = TXACTNotificationType(6);  // None, SoundBank
  XACTNOTIFICATIONTYPE_WAVEBANKDESTROYED     = TXACTNotificationType(7);  // None, WaveBank
  XACTNOTIFICATIONTYPE_LOCALVARIABLECHANGED  = TXACTNotificationType(8);  // None, SoundBank, SoundBank & cue index, cue instance
  XACTNOTIFICATIONTYPE_GLOBALVARIABLECHANGED = TXACTNotificationType(9);  // None
  XACTNOTIFICATIONTYPE_GUICONNECTED          = TXACTNotificationType(10); // None
  XACTNOTIFICATIONTYPE_GUIDISCONNECTED       = TXACTNotificationType(11); // None
  XACTNOTIFICATIONTYPE_WAVEPREPARED          = TXACTNotificationType(12); // None, WaveBank & wave index, wave instance.
  XACTNOTIFICATIONTYPE_WAVEPLAY              = TXACTNotificationType(13); // None, SoundBank, SoundBank & cue index, cue instance, WaveBank, wave instance
  XACTNOTIFICATIONTYPE_WAVESTOP              = TXACTNotificationType(14); // None, SoundBank, SoundBank & cue index, cue instance, WaveBank, wave instance
  XACTNOTIFICATIONTYPE_WAVELOOPED            = TXACTNotificationType(15); // None, SoundBank, SoundBank & cue index, cue instance, WaveBank, wave instance
  XACTNOTIFICATIONTYPE_WAVEDESTROYED         = TXACTNotificationType(16); // None, WaveBank & wave index, wave instance.
  XACTNOTIFICATIONTYPE_WAVEBANKPREPARED      = TXACTNotificationType(17); // None, WaveBank
  XACTNOTIFICATIONTYPE_WAVEBANKSTREAMING_INVALIDCONTENT = TXACTNotificationType(18); // None, WaveBank

  XACT_FLAG_NOTIFICATION_PERSIST = $01;

//------------------------------------------------------------------------------
// IXACTSoundBank
//------------------------------------------------------------------------------

  XACT_FLAG_SOUNDBANK_STOP_IMMEDIATE = XACT_FLAG_STOP_IMMEDIATE;
  XACT_SOUNDBANKSTATE_INUSE           = XACT_STATE_INUSE;

//------------------------------------------------------------------------------
// IXACTWaveBank
//------------------------------------------------------------------------------

  XACT_WAVEBANKSTATE_INUSE            = XACT_STATE_INUSE;         // Currently in-use
  XACT_WAVEBANKSTATE_PREPARED         = XACT_STATE_PREPARED;      // Prepared
  XACT_WAVEBANKSTATE_PREPAREFAILED    = XACT_STATE_PREPAREFAILED; // Prepare failed.


//------------------------------------------------------------------------------
// IXACTCue
//------------------------------------------------------------------------------

  // Cue Flags
  XACT_FLAG_CUE_STOP_RELEASE      = XACT_FLAG_STOP_RELEASE;
  XACT_FLAG_CUE_STOP_IMMEDIATE    = XACT_FLAG_STOP_IMMEDIATE;

  // Mutually exclusive states
  XACT_CUESTATE_CREATED           = XACT_STATE_CREATED;   // Created, but nothing else
  XACT_CUESTATE_PREPARING         = XACT_STATE_PREPARING; // In the middle of preparing
  XACT_CUESTATE_PREPARED          = XACT_STATE_PREPARED;  // Prepared, but not yet played
  XACT_CUESTATE_PLAYING           = XACT_STATE_PLAYING;   // Playing (though could be paused)
  XACT_CUESTATE_STOPPING          = XACT_STATE_STOPPING;  // Stopping
  XACT_CUESTATE_STOPPED           = XACT_STATE_STOPPED;   // Stopped
  XACT_CUESTATE_PAUSED            = XACT_STATE_PAUSED;    // Paused (can be combined with other states)


//------------------------------------------------------------------------------
// IXACTEngine
//------------------------------------------------------------------------------

  // Engine flags
  XACT_FLAG_ENGINE_CREATE_MANAGEDATA    = XACT_FLAG_MANAGEDATA;
  XACT_FLAG_ENGINE_STOP_IMMEDIATE       = XACT_FLAG_STOP_IMMEDIATE;


//------------------------------------------------------------------------------
// XACT specific error codes
//------------------------------------------------------------------------------

const
  FACILITY_XACTENGINE = $AC7;

// #define XACTENGINEERROR(n) MAKE_HRESULT(SEVERITY_ERROR, FACILITY_XACTENGINE, n)
// XACTENGINEERROR(n) = DWord((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or n;

  XACTENGINE_E_OUTOFMEMORY               = E_OUTOFMEMORY;      // Out of memory
  XACTENGINE_E_INVALIDARG                = E_INVALIDARG;       // Invalid arg
  XACTENGINE_E_NOTIMPL                   = E_NOTIMPL;          // Not implemented
  XACTENGINE_E_FAIL                      = E_FAIL;             // Unknown error

  XACTENGINE_E_ALREADYINITIALIZED        = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $001);   // The engine is already initialized
  XACTENGINE_E_NOTINITIALIZED            = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $002);   // The engine has not been initialized
  XACTENGINE_E_EXPIRED                   = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $003);   // The engine has expired (demo or pre-release version)
  XACTENGINE_E_NONOTIFICATIONCALLBACK    = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $004);   // No notification callback
  XACTENGINE_E_NOTIFICATIONREGISTERED    = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $005);   // Notification already registered
  XACTENGINE_E_INVALIDUSAGE              = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $006);   // Invalid usage
  XACTENGINE_E_INVALIDDATA               = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $007);   // Invalid data
  XACTENGINE_E_INSTANCELIMITFAILTOPLAY   = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $008);   // Fail to play due to instance limit
  XACTENGINE_E_NOGLOBALSETTINGS          = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $009);   // Global Settings not loaded
  XACTENGINE_E_INVALIDVARIABLEINDEX      = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $00a);   // Invalid variable index
  XACTENGINE_E_INVALIDCATEGORY           = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $00b);   // Invalid category
  XACTENGINE_E_INVALIDCUEINDEX           = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $00c);   // Invalid cue index
  XACTENGINE_E_INVALIDWAVEINDEX          = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $00d);   // Invalid wave index
  XACTENGINE_E_INVALIDTRACKINDEX         = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $00e);   // Invalid track index
  XACTENGINE_E_INVALIDSOUNDOFFSETORINDEX = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $00f);   // Invalid sound offset or index
  XACTENGINE_E_READFILE                  = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $010);   // Error reading a file
  XACTENGINE_E_UNKNOWNEVENT              = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $011);   // Unknown event type
  XACTENGINE_E_INCALLBACK                = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $012);   // Invalid call of method of function from callback
  XACTENGINE_E_NOWAVEBANK                = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $013);   // No wavebank exists for desired operation
  XACTENGINE_E_SELECTVARIATION           = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $014);   // Unable to select a variation
  XACTENGINE_E_MULTIPLEAUDITIONENGINES   = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $015);   // There can be only one audition engine
  XACTENGINE_E_WAVEBANKNOTPREPARED       = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $016);   // The wavebank is not prepared
  XACTENGINE_E_NORENDERER                = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $017);   // No audio device found on.
  XACTENGINE_E_INVALIDENTRYCOUNT         = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $018);   // Invalid entry count for channel maps
  XACTENGINE_E_SEEKTIMEBEYONDCUEEND      = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $019);   // Time offset for seeking is beyond the cue end.
  XACTENGINE_E_SEEKTIMEBEYONDWAVEEND     = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $019);   // Time offset for seeking is beyond the wave end.
  XACTENGINE_E_NOFRIENDLYNAMES           = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $01a);   // Friendly names are not included in the bank.

  XACTENGINE_E_AUDITION_WRITEFILE        = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $101);  // Error writing a file during auditioning
  XACTENGINE_E_AUDITION_NOSOUNDBANK      = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $102);  // Missing a soundbank
  XACTENGINE_E_AUDITION_INVALIDRPCINDEX  = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $103);  // Missing an RPC curve
  XACTENGINE_E_AUDITION_MISSINGDATA      = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $104);  // Missing data for an audition command
  XACTENGINE_E_AUDITION_UNKNOWNCOMMAND   = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $105);  // Unknown command
  XACTENGINE_E_AUDITION_INVALIDDSPINDEX  = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $106);  // Missing a DSP parameter
  XACTENGINE_E_AUDITION_MISSINGWAVE      = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $107);  // Wave does not exist in auditioned wavebank
  XACTENGINE_E_AUDITION_CREATEDIRECTORYFAILED = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $108);  // Failed to create a directory for streaming wavebank data
  XACTENGINE_E_AUDITION_INVALIDSESSION   = DWord(((1 shl 31) or (FACILITY_XACTENGINE shl 16)) or $109);  // Invalid audition session




{*-========================================================================-_
 |                                - XACT3D -                                |
 |        Copyright (c) Microsoft Corporation.  All rights reserved.        |
 |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
 |VERSION:  0.1                         MODEL:   Unmanaged User-mode        |
 |CONTRACT: N / A                       EXCEPT:  No Exceptions              |
 |PARENT:   N / A                       MINREQ:  Win2000, Xbox360           |
 |PROJECT:  XACT3D                      DIALECT: MS Visual C++ 7.0          |
 |>------------------------------------------------------------------------<|
 | DUTY: XACT 3D support                                                    |
 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^
  NOTES:
    1.  See X3DAudio.h for information regarding X3DAudio types.            *}


//--------------<D-E-F-I-N-I-T-I-O-N-S>-------------------------------------//
const
  // Supported speaker positions, represented as azimuth angles.
  //
  // Here's a picture of the azimuth angles for the 8 cardinal points,
  // seen from above.  The emitter's base position is at the origin 0.
  //
  //           FRONT
  //             | 0  <-- azimuth
  //             |
  //    7pi/4 \  |  / pi/4
  //           \ | /
  // LEFT       \|/      RIGHT
  // 3pi/2-------0-------pi/2
  //            /|\
  //           / | \
  //    5pi/4 /  |  \ 3pi/4
  //             |
  //             | pi
  //           BACK
  //
  LEFT_AZIMUTH                    = (3*X3DAUDIO_PI/2);
  RIGHT_AZIMUTH                   = (X3DAUDIO_PI/2);
  FRONT_LEFT_AZIMUTH              = (7*X3DAUDIO_PI/4);
  FRONT_RIGHT_AZIMUTH             = (X3DAUDIO_PI/4);
  FRONT_CENTER_AZIMUTH            = 0.0;
  LOW_FREQUENCY_AZIMUTH           = X3DAUDIO_2PI;
  BACK_LEFT_AZIMUTH               = (5*X3DAUDIO_PI/4);
  BACK_RIGHT_AZIMUTH              = (3*X3DAUDIO_PI/4);
  BACK_CENTER_AZIMUTH             = X3DAUDIO_PI;
  FRONT_LEFT_OF_CENTER_AZIMUTH    = (15*X3DAUDIO_PI/8);
  FRONT_RIGHT_OF_CENTER_AZIMUTH   = (X3DAUDIO_PI/8);


//--------------<D-A-T-A---T-Y-P-E-S>---------------------------------------//

  // Supported emitter channel layouts:
  aStereoLayout: array[0..1] of Single =
  (
    LEFT_AZIMUTH,
    RIGHT_AZIMUTH
  );
  a2Point1Layout: array[0..2] of Single =
  (
    LEFT_AZIMUTH,
    RIGHT_AZIMUTH,
    LOW_FREQUENCY_AZIMUTH
  );
  aQuadLayout: array[0..3] of Single =
  (
    FRONT_LEFT_AZIMUTH,
    FRONT_RIGHT_AZIMUTH,
    BACK_LEFT_AZIMUTH,
    BACK_RIGHT_AZIMUTH
  );
  a4Point1Layout: array[0..4] of Single =
  (
    FRONT_LEFT_AZIMUTH,
    FRONT_RIGHT_AZIMUTH,
    LOW_FREQUENCY_AZIMUTH,
    BACK_LEFT_AZIMUTH,
    BACK_RIGHT_AZIMUTH
  );
  a5Point1Layout: array[0..5] of Single =
  (
    FRONT_LEFT_AZIMUTH,
    FRONT_RIGHT_AZIMUTH,
    FRONT_CENTER_AZIMUTH,
    LOW_FREQUENCY_AZIMUTH,
    BACK_LEFT_AZIMUTH,
    BACK_RIGHT_AZIMUTH
  );
  a7Point1Layout: array[0..7] of Single =
  (
    FRONT_LEFT_AZIMUTH,
    FRONT_RIGHT_AZIMUTH,
    FRONT_CENTER_AZIMUTH,
    LOW_FREQUENCY_AZIMUTH,
    BACK_LEFT_AZIMUTH,
    BACK_RIGHT_AZIMUTH,
    LEFT_AZIMUTH,
    RIGHT_AZIMUTH
  );

const
  DefaultCurvePoints: array[0..1] of TX3DAudioDistanceCurvePoint =
    ((Distance: 0.0; DSPSetting: 1.0), (Distance: 1.0; DSPSetting: 1.0));
  DefaultCurve: TX3DAudioDistanceCurve = (pPoints: @DefaultCurvePoints; PointCount: 2);

//--------------<F-U-N-C-T-I-O-N-S>-----------------------------------------//

////
// DESCRIPTION:
//  Initializes the 3D API's:
//
// REMARKS:
//  This method only needs to be called once.
//  X3DAudio will be initialized such that its speaker channel mask
//  matches the format of the given XACT engine's final mix.
//
// PARAMETERS:
//  pEngine     - [in]  XACT engine
//  X3DInstance - [out] X3DAudio instance handle
//
// RETURN VALUE:
//  HResult error code
////
function XACT3DInitialize(const pEngine: IXACTEngine; out X3DInstance: TX3DAudioHandle): HRESULT; cdecl;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}


////
// DESCRIPTION:
//  Calculates DSP settings with respect to 3D parameters:
//
// REMARKS:
//  Note the following flags are always specified for XACT3D calculation:
//  X3DAUDIO_CALCULATE_MATRIX | X3DAUDIO_CALCULATE_DOPPLER | X3DAUDIO_CALCULATE_EMITTER_ANGLE
//
//  This means the caller must set at least the following fields:
//    X3DAUDIO_LISTENER.OrientFront
//    X3DAUDIO_LISTENER.OrientTop
//    X3DAUDIO_LISTENER.Position
//    X3DAUDIO_LISTENER.Velocity
//
//    X3DAUDIO_EMITTER.OrientFront
//    X3DAUDIO_EMITTER.OrientTop, if emitter is multi-channel
//    X3DAUDIO_EMITTER.Position
//    X3DAUDIO_EMITTER.Velocity
//    X3DAUDIO_EMITTER.ChannelCount
//    X3DAUDIO_EMITTER.CurveDistanceScaler
//    X3DAUDIO_EMITTER.DopplerScaler
//
//    X3DAUDIO_DSP_SETTINGS.pMatrixCoefficients, the caller need only allocate space for SrcChannelCount*DstChannelCount elements
//    X3DAUDIO_DSP_SETTINGS.SrcChannelCount
//    X3DAUDIO_DSP_SETTINGS.DstChannelCount
//
//  If X3DAUDIO_EMITTER.pChannelAzimuths is left NULL for multi-channel emitters,
//  a default channel radius and channel azimuth array will be applied below.
//  Distance curves such as X3DAUDIO_EMITTER.pVolumeCurve should be
//  left NULL as XACT's native RPCs will be used to define DSP behaviour
//  with respect to normalized distance.
//
//  See X3DAudio.h for information regarding X3DAudio types.
//
// PARAMETERS:
//  X3DInstance  - [in]  X3DAudio instance handle, returned from XACT3DInitialize()
//  pListener    - [in]  point of 3D audio reception
//  pEmitter     - [in]  3D audio source
//  pDSPSettings - [out] receives calculation results, applied to an XACT cue via XACT3DApply()
//
// RETURN VALUE:
//  HResult error code
////
function XACT3DCalculate (const X3DInstance: TX3DAudioHandle; const pListener: TX3DAudioListener;
  var pEmitter: TX3DAudioEmitter; var pDSPSettings: TX3DAudioDspSettings): HRESULT; cdecl;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}


////
// DESCRIPTION:
//  Applies results from a call to XACT3DCalculate() to a cue.
//
// PARAMETERS:
//  pDSPSettings - [in] calculation results generated by XACT3DCalculate()
//  pCue         - [in] cue to which to apply pDSPSettings
//
// RETURN VALUE:
//  HResult error code
////
function XACT3DApply(const pDSPSettings: TX3DAudioDspSettings; const pCue: IXACTCue;
  ChannelCount: LongWord): HRESULT; cdecl;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}


implementation


{.$IFDEF XBOX}
function XACTCreateEngine(dwCreationFlags: DWORD; out ppEngine: IXACTEngine): HRESULT; stdcall; // inline
var
  key: HKEY;
  data: DWORD;
  type_: DWORD;
  dataSize: DWORD;
  debug: Boolean;
  audition: Boolean;
  guid: TGUID;
begin
  type_    := REG_DWORD;
  dataSize := SizeOf(DWORD);
  debug    := (dwCreationFlags and XACT_FLAG_API_DEBUG_MODE) <> 0;
  audition := (dwCreationFlags and XACT_FLAG_API_AUDITION_MODE) <> 0;

  // If neither the debug nor audition flags are set, see if the debug registry key is set
  if (not debug and not audition and
     (RegOpenKeyEx(HKEY_LOCAL_MACHINE, XACT_DEBUGENGINE_REGISTRY_KEY, 0, KEY_READ, key) = ERROR_SUCCESS)) then
  begin
    if RegQueryValueEx(key, XACT_DEBUGENGINE_REGISTRY_VALUE, nil, @type_, @data, @dataSize) = ERROR_SUCCESS
    then debug := (data <> 0);
    RegCloseKey(key);
  end;

  if audition then guid := CLSID_XACTAuditionEngine
  else if debug then guid := CLSID_XACTDebugEngine
  else guid := CLSID_XACTEngine;

  // Priority order: Audition, Debug, Retail
  Result:= CoCreateInstance(guid, nil, CLSCTX_INPROC_SERVER, IID_IXACTEngine, ppEngine);

  // If debug engine does not exist fallback to retail version
  if FAILED(Result) and debug and not audition then
  begin
    Result := CoCreateInstance(CLSID_XACTEngine, nil, CLSCTX_INPROC_SERVER, IID_IXACTEngine, ppEngine);
  end;
end;
{.$ENDIF XBOX}


(***************************************************************************
 *  File:       xact2wb.h
 *  Content:    XACT 2 wave bank definitions.
 ****************************************************************************)
procedure SwapBytes(_dw_: PDWORD); overload;
asm
  mov edi, _dw_
  // mov edi, dw
  mov eax, [edi]
  bswap eax
  mov [edi], eax
end;
//        dw = _byteswap_ulong(dw); //todo: add inline in pure pascal case

procedure SwapBytes(w: PWORD); overload;
asm
  mov edi, w
  mov ax, [edi]
  xchg ah, al
  mov [edi], ax
end;
//        w = _byteswap_ushort(w); //todo: add inline in pure pascal case




(***************************************************************************
 *  File:       xact3d.h
 *  Content:    XACT 3D support.
 ****************************************************************************)

////
// DESCRIPTION:
//  Initializes the 3D API's:
//
// REMARKS:
//  This method only needs to be called once
//  The number of bits set in SpeakerChannelMask should equal the number of
//  channels expected on the final mix.
//
// PARAMETERS:
//  SpeakerChannelMask - [in]  speaker geometry configuration on the final mix, specifies assignment of channels to speaker positions, defined as per WAVEFORMATEXTENSIBLE.dwChannelMask, must be != 0
//                             Currently only SPEAKER_STEREO and SPEAKER_5POINT1 is supported by X3DAudio.
//  pEngine            - [in]  pointer to the XACT engine
//  X3DInstance        - [out] Handle to the X3DAudio instance
//
// RETURN VALUE:
//  HResult error code
////
function XACT3DInitialize(const pEngine: IXACTEngine; out X3DInstance: TX3DAudioHandle): HRESULT; cdecl;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}
var
  xactSpeedOfSoundID: TXACTVariableIndex;
  nSpeedOfSound: TXACTVariableValue;
  wfxFinalMixFormat: TWaveFormatExtensible;
begin
  Result := S_OK;
  if (pEngine = nil) then Result := E_POINTER;

  if SUCCEEDED(Result) then
  begin
    xactSpeedOfSoundID := pEngine.GetGlobalVariableIndex('SpeedOfSound');
    Result := pEngine.GetGlobalVariable(xactSpeedOfSoundID, nSpeedOfSound);
  end;

  if SUCCEEDED(Result) then
  begin
    Result := pEngine.GetFinalMixFormat(wfxFinalMixFormat);
    if SUCCEEDED(Result)
    then X3DAudioInitialize(wfxFinalMixFormat.dwChannelMask, nSpeedOfSound, X3DInstance);
  end;
end;


////
// DESCRIPTION:
//  Calculates DSP settings with respect to 3D parameters:
//
// PARAMETERS:
//  X3DInstance        - [in]  X3DAudio instance (returned from XACT3DInitialize)
//  pListener          - [in]  point of 3D audio reception
//  pEmitter           - [in]  3D audio source
//  pDSPSettings       - [out] receives calculation results, applied to an XACT cue via XACT3DApply
//
// RETURN VALUE:
//  HResult error code
////
function XACT3DCalculate(const X3DInstance: TX3DAudioHandle; const pListener: TX3DAudioListener;
  var pEmitter: TX3DAudioEmitter; var pDSPSettings: TX3DAudioDspSettings): HRESULT; cdecl;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}
begin
  Result := S_OK;
  { if (pListener = nil) or (pEmitter = nil) or (pDSPSettings = nil)
  then Result := E_POINTER; }

  // if(SUCCEEDED(Result) then
  if (pEmitter.ChannelCount > 1) and (pEmitter.pChannelAzimuths = nil) then
  begin
    pEmitter.ChannelRadius := 1.0;

    case pEmitter.ChannelCount of
      2: pEmitter.pChannelAzimuths := @aStereoLayout[0];
      3: pEmitter.pChannelAzimuths := @a2Point1Layout[0];
      4: pEmitter.pChannelAzimuths := @aQuadLayout[0];
      5: pEmitter.pChannelAzimuths := @a4Point1Layout[0];
      6: pEmitter.pChannelAzimuths := @a5Point1Layout[0];
      8: pEmitter.pChannelAzimuths := @a7Point1Layout[0];
     else
      Result := E_FAIL;
    end;
  end;

  if SUCCEEDED(Result) then
  begin
    if (pEmitter.pVolumeCurve = nil) then pEmitter.pVolumeCurve := @DefaultCurve;
    if (pEmitter.pLFECurve = nil) then pEmitter.pLFECurve := @DefaultCurve;

    X3DAudioCalculate(X3DInstance, pListener, pEmitter,
      X3DAUDIO_CALCULATE_MATRIX or X3DAUDIO_CALCULATE_DOPPLER or X3DAUDIO_CALCULATE_EMITTER_ANGLE,
      pDSPSettings);
  end;
end;


////
// DESCRIPTION:
//  Applies a 3D calculation returned by XACT3DCalculate to a cue:
//
// PARAMETERS:
//  pDSPSettings - [in] calculation results generated by XACT3DCalculate
//  pCue         - [in] cue to which to apply pDSPSettings
//  ChannelCount - [in] should be set equal to pDSPSettings->SrcChannelCount.
//
// RETURN VALUE:
//  HResult error code
////
function XACT3DApply(const pDSPSettings: TX3DAudioDspSettings; const pCue: IXACTCue;
  ChannelCount: LongWord): HRESULT; cdecl;{$IFDEF SUPPORTS_INLINE} inline;{$ENDIF}
var
  xactDistanceID: TXACTVariableIndex;
  xactDopplerID: TXACTVariableIndex;
  xactOrientationID: TXACTVariableIndex;
begin
  { Result := S_OK;
  if (pDSPSettings = nil) or )pCue = nil) then Result := E_POINTER;

  if SUCCEEDED(Result) then }
  Result := pCue.SetMatrixCoefficients(pDSPSettings.SrcChannelCount, pDSPSettings.DstChannelCount, pDSPSettings.pMatrixCoefficients);

  if SUCCEEDED(Result) then
  begin
    xactDistanceID := pCue.GetVariableIndex('Distance');
    Result := pCue.SetVariable(xactDistanceID, pDSPSettings.EmitterToListenerDistance);
  end;

  if SUCCEEDED(Result) then
  begin
    xactDopplerID := pCue.GetVariableIndex('DopplerPitchScalar');
    Result := pCue.SetVariable(xactDopplerID, pDSPSettings.DopplerFactor);
  end;

  if SUCCEEDED(Result) then   
  begin
    xactOrientationID := pCue.GetVariableIndex('OrientationAngle');
    Result := pCue.SetVariable(xactOrientationID, pDSPSettings.EmitterToListenerAngle * (180.0 / X3DAUDIO_PI));
  end;
end;

end.






