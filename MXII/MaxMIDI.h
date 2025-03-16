//-----------------------------------------------------------------------------
// Maximum MIDI Programmer's ToolKit
// 32-bit App Header File
//
// Copyright (c) Paul A. Messick, 1994-1996
//
// Written by Paul A. Messick
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  Definitions
//-----------------------------------------------------------------------------
#ifndef __MAXMIDI__
#define __MAXMIDI__   // include header only once per file

#include <mmsystem.h>

#ifdef __cplusplus
extern "C" {        // Assume C declarations for C++
#endif

#ifndef __WIN32__
    #ifdef _WIN32
        #define __WIN32__
    #else
        #ifdef WIN32
            #define __WIN32__
        #endif
    #endif
#endif

#ifdef __WIN32__
    #define PREFIX __declspec(dllexport)
    #define EXPORT
#else
    #define PREFIX
    #define EXPORT __export
#endif

//  midi buffer sizes
#define QUEUE_64    (0 << 12)
#define QUEUE_128   (1 << 12)
#define QUEUE_256   (2 << 12)
#define QUEUE_512   (3 << 12)
#define QUEUE_1K    (4 << 12)
#define QUEUE_2K    (5 << 12)
#define QUEUE_4K    (6 << 12)
#define QUEUE_8K    (7 << 12)
#define QUEUE_16K   (8 << 12)
#define QUEUE_32K   (9 << 12)

#define SXBUF_64    (0 << 8)
#define SXBUF_128   (1 << 8)
#define SXBUF_256   (2 << 8)
#define SXBUF_512   (3 << 8)
#define SXBUF_1K    (4 << 8)
#define SXBUF_2K    (5 << 8)
#define SXBUF_4K    (6 << 8)
#define SXBUF_8K    (7 << 8)
#define SXBUF_16K   (8 << 8)
#define SXBUF_32K   (9 << 8)

//  midi sync status messages
#define NOTEOFF     0x80
#define NOTEON          0x90
#define  SYSEX          0xF0
#define MTC_QFRAME      0xF1
#define  EOX                0xF7
#define MIDI_CLOCK      0xF8
#define MIDI_START      0xFA
#define MIDI_CONTINUE   0xFB




Page 303

#define MIDI_STOP       0xFC

//  maxmidi messages
#define WM_MAXMIDI          (WM_USER + 0x504D)  // 'PM'
#define MOM_LONGDATA        (WM_MAXMIDI+3)
#define MIDI_BEAT           (WM_MAXMIDI+5)
#define OUTBUFFER_READY     (WM_MAXMIDI+6)
#define SYNC_DONE           (WM_MAXMIDI+10)
#define MIDI_DATA            MIM_DATA

//  external (user settable) flags
#define ENABLE_SYSEX        0x00010000L
#define DISABLE_SYSEX       (~ENABLE_SYSEX)
#define SYNC_INPUT          0x00020000L
#define SYNC_OUTPUT         0x00020000L

// MxMidi DLL error values
#define MXMIDIERR_NOERROR       0
#define MXMIDIERR_BADDEVICEID   MMSYSERR_BADDEVICEID
#define MXMIDIERR_NOMEM         MMSYSERR_NOMEM
#define MXMIDIERR_BADHANDLE     MMSYSERR_ALLOCATED
#define MXMIDIERR_BADTEMPO      30
#define MXMIDIERR_MAXERR        32
#define ERR_NOMATCH             0xFFF0

//-----------------------------------------------------------------------------
//  Event structures
//-----------------------------------------------------------------------------
#pragma pack(1)
typedef struct {
    DWORD   time;       // time in ticks since last event
    BYTE    status;     // status byte of this midi message
    BYTE    data1;      // first data byte of message
    BYTE    data2;      // second data byte of message
    BYTE    data3;      // third data byte, used for tempo changes
} MidiEvent;
#pragma pack()

typedef MidiEvent* LPMIDIEVENT;

//-----------------------------------------------------------------------------
//  Midi In Definitions
//-----------------------------------------------------------------------------
#define MIDIIN_DEFAULT (QUEUE_512|SXBUF_512|ENABLE_SYSEX|32)
typedef DWORD HMIN;

//-----------------------------------------------------------------------------
//  MIDI Out Definitions
//-----------------------------------------------------------------------------
#define MIDIOUT_DEFAULT (QUEUE_512|SXBUF_512|ENABLE_SYSEX|32)
typedef DWORD HMOUT;

//-----------------------------------------------------------------------------
//  Sync Timer definitions
//-----------------------------------------------------------------------------
#define USE_CURRENT 0xFFFF
#define DEFAULT_TIMERPERIOD 10
#define MAX_RESOLUTION 960
#define S_INT       0
#define S_MIDI      1
#define POS_TICKS       0
#define POS_MS      1
typedef DWORD HSYNC;

//-----------------------------------------------------------------------------
//  Standard MIDI File definitions
//-----------------------------------------------------------------------------
#define META                    0xFF
#define META_SEQUENCE_NUMBER    0x00
#define META_TEXT               0x01
#define META_COPYRIGHT          0x02
#define META_NAME               0x03
#define META_INST_NAME          0x04
#define META_LYRIC              0x05
#define META_MARKER             0x06
#define META_CUE_POINT          0x07
#define META_CHAN_PREFIX        0x20
#define META_EOT                0x2F




Page 304

#define META_TEMPO          0x51
#define META_SMPTE_OFFSET       0x54
#define META_TIME_SIG       0x58
#define META_KEY_SIG            0x59
#define META_SEQ_SPECIFIC       0x7F
#define MAX_META_EVENT      0x80

typedef DWORD HSMF;

//-----------------------------------------------------------------------------
//  Exported MaxMidi DLL entry point function prototypes
//-----------------------------------------------------------------------------
PREFIX WORD WINAPI EXPORT GetMaxMidiVersion(void);

PREFIX UINT WINAPI EXPORT GetNumOutDevices(void);
PREFIX BOOL WINAPI EXPORT GetMidiOutDescription(WORD wDeviceID, LPSTR lpzDesc);
PREFIX HMOUT WINAPI EXPORT OpenMidiOut(HWND hWnd, WORD wDeviceID, HSYNC hSync, 
DWORD dwFlags);
PREFIX WORD WINAPI EXPORT ResetMidiOut(HMOUT hMidiOut);
PREFIX void WINAPI EXPORT FlushMidiOut(HMOUT hMidiOut);
PREFIX WORD WINAPI EXPORT CloseMidiOut(HMOUT hMidiOut);
PREFIX WORD WINAPI EXPORT PutMidiOut(HMOUT hMidiOut, LPMIDIEVENT lpMidiEvent);

PREFIX UINT WINAPI EXPORT GetNumInDevices(void);
PREFIX BOOL WINAPI EXPORT GetMidiInDescription(WORD wDeviceID, LPSTR lpzDesc);
PREFIX HMIN WINAPI EXPORT OpenMidiIn(HWND hWnd, WORD wDeviceID, HSYNC hSync, 
DWORD dwFlags);
PREFIX WORD WINAPI EXPORT StartMidiIn(HMIN lpMidiIn);
PREFIX WORD WINAPI EXPORT StopMidiIn(HMIN lpMidiIn);
PREFIX WORD WINAPI EXPORT CloseMidiIn(HMIN lpMidiIn);
PREFIX LPMIDIEVENT WINAPI EXPORT GetMidiIn(HMIN lpMidiIn);

PREFIX HSYNC WINAPI EXPORT OpenSync(HSYNC hSync, HWND hWnd, WORD mode, 
WORD timerPeriod);
PREFIX WORD WINAPI EXPORT CloseSync(HSYNC hSync);
PREFIX void WINAPI EXPORT StopSync(HSYNC hSync);
PREFIX void WINAPI EXPORT StartSync(HSYNC hSync);
PREFIX void WINAPI EXPORT PauseSync(HSYNC hSync, BOOL reset);
PREFIX void WINAPI EXPORT ReStartSync(HSYNC hSync);
PREFIX WORD WINAPI EXPORT SetTempo(HSYNC hSync, DWORD uSPerBeat);
PREFIX void WINAPI EXPORT SetResolution(HSYNC hSync, WORD resolution);
PREFIX DWORD WINAPI EXPORT GetTempo(HSYNC hSync);
PREFIX WORD WINAPI EXPORT GetResolution(HSYNC hSync);
PREFIX DWORD WINAPI EXPORT GetPosition(HSYNC hSync, WORD units);

PREFIX HSMF WINAPI EXPORT OpenSMF(LPSTR filename, int *Format, const char mode, 
int *nTracks);
PREFIX void WINAPI EXPORT CloseSMF(HSMF hSMF);
PREFIX BOOL WINAPI EXPORT RewindSMF(HSMF hSMF);
PREFIX DWORD WINAPI EXPORT ReadSMF(HSMF hSMF, int wTrack, LPMIDIEVENT 
lpMidiEventBuffer, DWORD dwBufferLen);
PREFIX DWORD WINAPI EXPORT WriteSMF(HSMF hSMF, int wTrack, 
LPMIDIEVENT lpMidiEventBuffer, DWORD dwBufferLen);
PREFIX WORD WINAPI EXPORT GetSMFResolution(HSMF hSMF);
PREFIX WORD WINAPI EXPORT SetSMFResolution(HSMF hSMF, WORD resolution);
PREFIX DWORD WINAPI EXPORT ReadMetaEvent(HSMF hSMF, int wTrack, BYTE MetaEvent, 
LPSTR *EventValue, DWORD *EventSize);
PREFIX int WINAPI EXPORT WriteMetaEvent(HSMF hSMF, int wTrack, BYTE MetaEvent, 
LPSTR EventValue, DWORD dwTime);

#ifdef __cplusplus
}                   // End of extern "C" {
#endif

#ifdef __cplusplus      // Include MaxMidi Classes
#ifdef __AFX_H__        // MFC Only!
#ifdef _SMF
    #ifndef _TRACK      // SMF requires TRACK
        #define _TRACK
    #endif
#endif
#ifdef _TRACK
    class CMaxMidiTrack;
#endif
#ifdef _SMF
    #include "CMaxMidiSMF.h"
#endif
#ifdef _SYNC
    #include "CMaxMidiSync.h"
#endif




Page 305

#ifdef _MIDIIN
    #include "CMaxMidiIn.h"
    #ifdef _MENUS
        #include "CMidiInDeviceMenu.h"
    #endif
#endif
#ifdef _MIDIOUT
    #include "CMaxMidiOut.h"
    #ifdef _MENUS
        #include "CMidiOutDeviceMenu.h"
    #endif
#endif
#ifdef _TRACK
    #include "CMaxMidiTrack.h"
#endif
#endif //__AFX_H__
#endif
#endif //!__MAXMIDI__
