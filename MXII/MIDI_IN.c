// Maximum MIDI Programmer's ToolKit - MxMidi16.DLL
// MIDI Input Module
//
// Copyright (c) Paul A. Messick, 1994-1996
//
// Written by Paul A. Messick
//
// Provides a 16-bit API to open, close, and receive midi messages,
// with system exclusive messages merged into the normal midi stream.
//-----------------------------------------------------------------------------
#include <windows.h>
#include "MxDLL.h"

//-----------------------------------------------------------------------------
//  DllEntryPoint definitions
//-----------------------------------------------------------------------------
#define DLL_PROCESS_DETACH 0
#define DLL_PROCESS_ATTACH 1
BOOL FAR PASCAL _export MxMidi_ThunkConnect16(LPSTR pszDll16, LPSTR pszDll32, 
        HINSTANCE hInst, DWORD dwReason);

//-----------------------------------------------------------------------------
// Function Prototypes used in this DLL module
//-----------------------------------------------------------------------------
WORD AllocateMidiInQueue(LPMIDIIN lpMidiIn, DWORD dwEvents);
HWND CreateMidiInWindow(LPMIDIIN lpMidiIn);
LPMIDIHDR AllocateMidiInSysexBuffer(LPMIDIIN lpMidiIn, WORD wSize);
void MidiClock(LPSYNC lpSync);

//-----------------------------------------------------------------------------
// Global Variables
//-----------------------------------------------------------------------------
HINSTANCE ghInstance;   // global instance handle for this dll
                    // buffer size table
WORD bufSize[10] = { 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768 };
WORD wQueueSize = 8;    // the size of the windows message queue

#define MSHORT 0
#define MSYSEX 1





Page 309

//-----------------------------------------------------------------------------
//  DllEntryPoint
//
//  Connects this 16-bit Windows 4.0 (Windows 95) DLL to its 32-bit counterpart
//  through the flat thunk layer.  This entry point is called (in addition to
//  LibMain) for DLLs that are tagged as version 4.0 (using the special rc.exe
//  resource compiler in the Win95 SDK: 'rc -40 mxmidi16.dll mxmidi16.res').
//-----------------------------------------------------------------------------
BOOL FAR PASCAL _export DllEntryPoint(DWORD dwReason, HINSTANCE hInst, WORD wDS,
         WORD wHeapSize, DWORD dwReserved1, WORD wReserved2)
{
    if(!(MxMidi_ThunkConnect16("MXMIDI16.DLL", "MXMIDI32.DLL", hInst, dwReason)))
        return FALSE;

    switch(dwReason)
    {
        // DLL is attaching to the address space of the current process.
        case DLL_PROCESS_ATTACH:
            break;

        // The calling process is detaching the DLL from its address space.
        case DLL_PROCESS_DETACH:
            break;
    }

    return TRUE;
}

//------------------------------------------------------------------------
// LibMain
//
// Only one LibMain may exist in a dll, so it appears only in midiin.cpp.
// This function registers the window classes used in this DLL.
//------------------------------------------------------------------------
int WINAPI _export LibMain(HINSTANCE hInst, WORD wDS, WORD cbHeapSize,
         LPSTR lpCmdLine)
{
    WNDCLASS wndclass;      // Structure used to register class

    ghInstance = hInst;     // the instance handle for this dll

    // setup window class parameters. Notice that this window is NEVER
    // displayed, so the display-specific parameters are set to NULL.
    wndclass.style       = CS_GLOBALCLASS;
    wndclass.lpfnWndProc   = MidiInProc;
    wndclass.cbClsExtra    = 0;
    wndclass.cbWndExtra    = 0;
    wndclass.hInstance  = ghInstance;
    wndclass.hIcon       = NULL;
    wndclass.hCursor      = NULL;
    wndclass.hbrBackground = NULL;
    wndclass.lpszMenuName  = NULL;
    wndclass.lpszClassName = MIDIINCLASS;
    RegisterClass(&wndclass);

    // setup hidden midi out window class parameters.  Notice that
    // this window is NEVER displayed, so the display-specific
    // parameters are set to NULL.
    wndclass.style       = CS_GLOBALCLASS;
    wndclass.lpfnWndProc   = MidiOutProc;
    wndclass.cbClsExtra    = 0;
    wndclass.cbWndExtra    = 0;
    wndclass.hInstance  = ghInstance;
    wndclass.hIcon       = NULL;
    wndclass.hCursor      = NULL;
    wndclass.hbrBackground = NULL;
    wndclass.lpszMenuName  = NULL;
    wndclass.lpszClassName = MIDIOUTCLASS;
    RegisterClass(&wndclass);
    return 1;
}

//------------------------------------------------------------------------
// Windows Exit Procedure
//
// Unregister the hidden windows that are used by this DLL.
//------------------------------------------------------------------------
int WINAPI _export WEP(int nParameter)
{
    UnregisterClass(MIDIINCLASS, ghInstance);




Page 310


    UnregisterClass(MIDIOUTCLASS, ghInstance);
    return 0;
}

//-----------------------------------------------------------------------------
// GetMxMidiVersion
//
// Returns the current dll version number as a word with the major version
// number in the high byte and the minor version number in the low byte.
// This function must be called before the message loop of the
// application that is to use midi can begin to use functions in this dll,
// since the dll must set the message queue list size.
//-----------------------------------------------------------------------------
WORD WINAPI _export GetMaxMidiVersion16(void)
{
    // the queue size determines the upper limit on the number
    // of queue-able sysex buffers.  If this function is not called
    // the QueueSize is the default 8 messages.
    wQueueSize = WINDOWS_QUEUE_SIZE;
    SetMessageQueue(wQueueSize);
    return (1 << 8) | 0;
}

//-----------------------------------------------------------------------------
// GetMidiInDescription
//
// Gets the text string description for the midi in device designated by
// wDeviceID.  If there is no device by that ID the function returns FALSE
// Otherwise, it returns TRUE and sets the string lpzDesc to the device
// description.
//-----------------------------------------------------------------------------
BOOL WINAPI _export GetMidiInDescription16(WORD wDeviceID, LPSTR lpzDesc)
{
    MIDIINCAPS Caps;

    if(midiInGetDevCaps(wDeviceID, (LPMIDIINCAPS) &Caps, sizeof(Caps)) == 
            MMSYSERR_BADDEVICEID)
        return FALSE;

    lstrcpy(lpzDesc, (LPSTR)Caps.szPname);
    return TRUE;
}

//-----------------------------------------------------------------------------
// OpenMidiIn
//
// Opens the device wDevice for midi input.
//
// hWnd is the handle to the window that is to receive any messages from
//  this device.
//
// wDeviceID ranges from 0 to one less than the number of available
//  devices and indicates which device is to be opened.
//
// hSync is the handle to the previously opened sync device that will
//  handle syncronization for this input.  If no sync is to be used
//  with this input, hSync must be zero.
//
// dwFlags are used to affect the particular operation of the midi in
//  device and can be combined using the OR operator.
//
// Current flags are:
//   SYSEX_ENABLED  if set, system exclusive messages are merged into
//              the normal midi stream that is sent to the app
// *SYSEX_DISABLED  if set, system exclusive messages are filtered (default)
//   SYNC_INPUT this input serves as the sync input for MTC or
//              CLOCK sync for the previously opened sync
//              device lpSync.  It is ignored if lpSync
//              is NULL or the sync mode has been set to
//              S_STOPPED or S_INT.
//
// The function returns the handle to the device if midi in was correctly
// opened, or an error value (that is less than MXMIDIERR_MAXERR) if there
// was an error opening the device.
//-----------------------------------------------------------------------------
HMIN WINAPI _export OpenMidiIn16(HWND hWnd, WORD wDeviceID, HSYNC hSync,
        DWORD dwFlags)
{
    LPMIDIIN lpMI;      // local pointer to allocated MidiIn structure




Page 311

    WORD i;             // loop counter
    LPSYNC lpSync = (LPSYNC)hSync;
    WORD wLastError;

    // if an attempt to open more than then max number of devices return bad
    // ID error
    if(midiInGetNumDevs() <= wDeviceID)
        return MXMIDIERR_BADDEVICEID;

    // attempt to allocate memory for this device
    lpMI = (LPMIDIIN)GlobalLock(GlobalAlloc(GPTR, sizeof(MidiInStruct)));

    // check for an allocation error
    if(lpMI == NULL)
        return MXMIDIERR_NOMEM;

    // this is the handle to the window that is to receive messages
    lpMI->hWnd = hWnd;

    // the device id associated with this device
    lpMI->wDeviceID = wDeviceID;

    // attempt to open the device
    wLastError = midiInOpen(&lpMI->hMidiIn, wDeviceID, (DWORD)MidiInCallback,
             (DWORD)lpMI, CALLBACK_FUNCTION);

    // if there is an error opening midi in, return a null handle and
    // free the allocated structure
    if(wLastError != 0)
    {
        FreeGlobalMem16(lpMI);
        return wLastError;
    }

    // setup the necessary flags and default values
    lpMI->dwFlags = (dwFlags & MIDI_IN_INTERNAL);    // set external flags only
    lpMI->nSysexBufSize = bufSize[(dwFlags & SXMASK) >> 8];

    // if this is the sync input device, adjust the pointer in lpSync
    // and insert this device into lpSync as the sync in device
    lpMI->lpSync = lpSync;

    if((dwFlags & SYNC_INPUT) && (lpSync != NULL))
        lpSync->lpSyncIn = lpMI;

    // allocate the queue for non-sysex messages
    if(AllocateMidiInQueue(lpMI, bufSize[(dwFlags & QMASK) >> 12]) != 0)
    {
        FreeGlobalMem16(lpMI);      // delete structure
        return MXMIDIERR_NOMEM;
    }

    // initialize the pointer to the round-robin list of sysex buffers
    lpMI->lpHeaderList = NULL;

    // if ENABLE_SYSEX flag is set, setup driver to process sysex messages
    if(dwFlags & ENABLE_SYSEX)
    {
        // sysex buffer offset starts at zero
        lpMI->lpSysexData = 0;

        // get the number of sysex buffers that will be allocated
        lpMI->nSysexBuffs = (int)(dwFlags & NMASK);

        // allocate buffer for round-robin sysex header list
        if( (lpMI->lpHeaderList = (LPDWORD*)GlobalLock(GlobalAlloc(GMEM_MOVEABLE
                 | GMEM_SHARE | GMEM_ZEROINIT, (DWORD)(sizeof(DWORD) * 
                 (lpMI->nSysexBuffs)) )))  != NULL)
        {
            // allocate the actual sysex buffers
            for(i = 0; i < lpMI->nSysexBuffs; i++)
                AllocateMidiInSysexBuffer(lpMI, lpMI->nSysexBufSize);
        }
        else
            lpMI->dwFlags &= DISABLE_SYSEX;

        // point to the first header in the list
        lpMI->nHeaderIn = lpMI->nHeaderOut = 0;
    }

    // create the (invisible) window that processes midi-in action messages
    CreateMidiInWindow(lpMI);

    return (HMIN)lpMI;




Page 312

}

//-----------------------------------------------------------------------------
// StartMidiIn
//
// Must be called after the device is opened in order to enable receiving
// midi messages.
//-----------------------------------------------------------------------------
WORD WINAPI _export StartMidiIn16(HMIN hMidiIn)
{
    LPMIDIIN lpMidiIn = (LPMIDIIN)hMidiIn;
    WORD wLastError;

    // check for null pointer
    if(hMidiIn < MXMIDIERR_MAXERR)
        return 1;

    // if midi in was already started, do nothing
    if(lpMidiIn->dwFlags & MIDI_IN_STARTED)
        return 0;

    // reset the pointers to the queue
    lpMidiIn->pMidiInDataIn = lpMidiIn->pMidiInDataOut = 0;

    // start the midi in
    wLastError = midiInStart(lpMidiIn->hMidiIn);
    if(wLastError == 0)
        lpMidiIn->dwFlags |= MIDI_IN_STARTED;

    // clear the last event time to calculate delta times
    lpMidiIn->dwLastEventTicks = 0L;

    return wLastError;
}

//-----------------------------------------------------------------------------
// StopMidiIn
//
// Must be called before closing a device in order to stop midi messages
// from being sent by the midi driver.
//-----------------------------------------------------------------------------
WORD WINAPI _export StopMidiIn16(HMIN hMidiIn)
{
    LPMIDIIN lpMidiIn = (LPMIDIIN)hMidiIn;
    WORD wLastError;

    // check for null pointer
    if(hMidiIn < MXMIDIERR_MAXERR)
        return 1;

    // try to stop the midi in
    wLastError = midiInStop(lpMidiIn->hMidiIn);

    // if stopped, adjust flag
    if(wLastError == 0)
        lpMidiIn->dwFlags &= ~MIDI_IN_STARTED;

    return wLastError;
}

//-----------------------------------------------------------------------------
// CloseMidiIn
//
// Closes the midi device lpMidiIn.
//-----------------------------------------------------------------------------
WORD WINAPI _export CloseMidiIn16(HMIN hMidiIn)
{
    LPMIDIHDR lpMidiHdr;
    LPMIDIIN lpMidiIn = (LPMIDIIN)hMidiIn;

    // just in case the user didn't follow directions
    if(hMidiIn < MXMIDIERR_MAXERR)
        return 0;

    if(lpMidiIn->dwFlags & MIDI_IN_STARTED)
        StopMidiIn16(hMidiIn);

    // reset midi in
    midiInReset(lpMidiIn->hMidiIn);

    // if sysex was enabled, go through the list of sysex buffer
    // headers and unprepare and free each one.
    if(lpMidiIn->dwFlags & ENABLE_SYSEX)
    {




Page 313

        while(lpMidiIn->nHeaderIn != lpMidiIn->nHeaderOut)
        {
            // get the header
            lpMidiHdr = (LPMIDIHDR)lpMidiIn->lpHeaderList[lpMidiIn->nHeaderOut];

            // this sysex buffer is to be removed: unprepare it
            midiInUnprepareHeader(lpMidiIn->hMidiIn, (LPMIDIHDR)lpMidiHdr,
                (WORD)sizeof(MIDIHDR));

            // free the memory associated with this item
            FreeGlobalMem16(lpMidiHdr->lpData);
            FreeGlobalMem16(lpMidiHdr);

            // point to next header, wrap if necessary
            if(++lpMidiIn->nHeaderOut == lpMidiIn->nSysexBuffs)
                lpMidiIn->nHeaderOut = 0;
        }

        // free the buffer list
        FreeGlobalMem16(lpMidiIn->lpHeaderList);
    }
                                   
    // now that all the buffers are removed and the driver is reset
    // we can close the device for midi in
    midiInClose(lpMidiIn->hMidiIn);
    return 1;
}

//-----------------------------------------------------------------------------
// GetMidiIn
//
// Returns a pointer to a midi event, if one is available.
//-----------------------------------------------------------------------------
LPMIDIEVENT WINAPI _export GetMidiIn16(HMIN hMidiIn)
{
    LPMIDIEVENT rc;
    LPMIDIHDR lpMidiHdr;        // local pointer to MidiHdr
    LPMIDIIN lpMidiIn = (LPMIDIIN)hMidiIn;

    // check for null pointer
    if(hMidiIn < MXMIDIERR_MAXERR)
        return NULL;

    // are we receiving a sysex? (i.e. any buffers in the list?)
    if(lpMidiIn->nHeaderOut != lpMidiIn->nHeaderIn)
    {
        // yes, point to the event (that will be in the lpMidiIn header)
        rc = (LPMIDIEVENT) &(lpMidiIn->sysexEvent);
        lpMidiHdr = (LPMIDIHDR)lpMidiIn->lpHeaderList[lpMidiIn->nHeaderOut];

        // setup the event values
        lpMidiIn->sysexEvent.status = SYSEX;
        lpMidiIn->sysexEvent.data1 = *(lpMidiHdr->lpData + lpMidiIn->lpSysexData);

        // time is only valid for sysex message status, data gets zero timestamp
        // time is in dwUser of the MidiHdr structure
        if(lpMidiIn->sysexEvent.data1 == SYSEX)
            lpMidiIn->sysexEvent.time = lpMidiHdr->dwUser;
        else
            lpMidiIn->sysexEvent.time = 0L;

        lpMidiIn->sysexEvent.data2 = 0;
        lpMidiIn->lpSysexData++;

        // are we at the end of the buffer?
        if(lpMidiHdr->dwBytesRecorded == lpMidiIn->lpSysexData)
        {
            // one less buffer in the sysex queue
            lpMidiIn->lpSysexData = 0L;

            // this sysex buffer is empty: re-prepare it
            // setup midihdr structure
            lpMidiHdr->dwBytesRecorded = 0L; // number of bytes in this buffer
            lpMidiHdr->dwFlags = 0L;         // initialize flags
            lpMidiHdr->dwUser = 0L;          // user data - will be used for
                                            // sysex time

            // prepare the header
            midiInPrepareHeader(lpMidiIn->hMidiIn, (LPMIDIHDR)lpMidiHdr,
                    (WORD)sizeof(MIDIHDR));

            // add the sysex buffer to the driver
            midiInAddBuffer(lpMidiIn->hMidiIn, lpMidiHdr, sizeof(MIDIHDR));





Page 314

            // point to next output buffer
            if(++lpMidiIn->nHeaderOut == lpMidiIn->nSysexBuffs)
                lpMidiIn->nHeaderOut = 0;
        }

        return rc;
    }

    // else its normal midi data
    // any data to get?
    if(lpMidiIn->pMidiInDataOut == lpMidiIn->pMidiInDataIn)
        return NULL;

    // get the address from the buffer
    rc = lpMidiIn->lpMidiInDataHead + lpMidiIn->pMidiInDataOut;

    // if necessary wrap the pointer
    if(lpMidiIn->pMidiInDataOut++ == lpMidiIn->nMidiInSize)
        lpMidiIn->pMidiInDataOut = 0;

    return rc;
}

//-----------------------------------------------------------------------------
// AllocateMidiInQueue
//
// Allocates a queue of dwEvents size to midi-in device lpMidiIn.
// Returns 0 if successful, MXMIDIERR_NOMEM if there is not enough global
// memory to allocate the queue.
//-----------------------------------------------------------------------------
WORD AllocateMidiInQueue(LPMIDIIN lpMidiIn, DWORD dwEvents)
{
    LPMIDIEVENT lpQueue;    // pointer to the newly allocated queue
    DWORD dwBytes;      // number of bytes to allocate

    // calculate number of bytes to allocate
    dwBytes = (dwEvents + 1) * sizeof(MidiEvent);

    // abort if not enough memory
    if((lpQueue = (LPMIDIEVENT) GlobalLock(GlobalAlloc(GPTR, dwBytes))) == NULL)
        return MXMIDIERR_NOMEM;

    // queue exists, setup pointers
    lpMidiIn->lpMidiInDataHead = lpQueue;
    lpMidiIn->pMidiInDataIn = lpMidiIn->pMidiInDataOut = 0;
    lpMidiIn->nMidiInSize = (int)dwEvents;
    return 0;
}

//-----------------------------------------------------------------------------
// AllocateMidiInSysexBuffer
//
// Adds a buffer of wSize bytes to the previously opened midi in, pointed
// to by lpMidiIn.  Returns the LPMIDIHDR for the new buffer, or NULL
// if not successful.
//-----------------------------------------------------------------------------
LPMIDIHDR AllocateMidiInSysexBuffer(LPMIDIIN lpMidiIn, WORD wSize)
{
    LPMIDIHDR lpMidiHdr;    // pointer to header structure for this buffer
    LPSTR lpData;       // pointer to sysex buffer
    WORD wLastError;

    // allocate MIDIHDR structure
    // it will be fixed and pagelocked by the midiInPrepareHeader call
    lpMidiHdr = (LPMIDIHDR)GlobalLock(GlobalAlloc(GMEM_MOVEABLE | 
GMEM_SHARE, sizeof(MIDIHDR)));

    if(lpMidiHdr == NULL)
        return NULL;

    // allocate buffer
    // it will be fixed and pagelocked by the midiInPrepareHeader call
    lpData = (LPSTR)GlobalLock(GlobalAlloc(GMEM_MOVEABLE | GMEM_SHARE, (DWORD)wSize));

    // if not enough memory for buffer, free header memory
    if(lpData == NULL)
    {
        FreeGlobalMem16(lpMidiHdr);
        return NULL;
    }

    // setup midihdr structure
    lpMidiHdr->lpData = lpData;          // pointer to start of buffer




Page 315

    lpMidiHdr->dwBufferLength = wSize;       // buffer size
    lpMidiHdr->dwBytesRecorded = 0L;     // number of bytes in this buffer
    lpMidiHdr->dwFlags = 0L;             // initialize flags
    lpMidiHdr->dwUser = 0L;              // user data - will be used for sysex time

    // prepare the header
    wLastError = midiInPrepareHeader(lpMidiIn->hMidiIn, lpMidiHdr, sizeof(MIDIHDR));

    if(wLastError != 0)
    {
        FreeGlobalMem16(lpMidiHdr->lpData);
        FreeGlobalMem16(lpMidiHdr);
        return NULL;
    }

    // add the sysex buffer to the driver
    wLastError = midiInAddBuffer(lpMidiIn->hMidiIn, lpMidiHdr, sizeof(MIDIHDR));

    if(wLastError != 0)
    {
        FreeGlobalMem16(lpMidiHdr->lpData);
        FreeGlobalMem16(lpMidiHdr);
        return NULL;
    }

    return lpMidiHdr;
}

//-----------------------------------------------------------------------------
// CreateMidiInWindow
//
// Creates a window that will receive notification messages from the
// MidiInCallback function.  Returns a handle to the new window, or NULL
// on error.
//-----------------------------------------------------------------------------
HWND CreateMidiInWindow(LPMIDIIN lpMidiIn)
{
    HWND hWnd;      // handle to the new window

    // create the (invisible) window
    hWnd = CreateWindow( MIDIINCLASS,       // lpClassName
        MIDIINCLASS,                        // lpWindowName
        WS_DISABLED,                        // dwStyle
        CW_USEDEFAULT,                     // X position
        CW_USEDEFAULT,                     // Y position
        CW_USEDEFAULT,                     // nWidth
        CW_USEDEFAULT,                     // nHeight
        NULL,                              // hWinParent
        NULL,                              // hMenu
        ghInstance,                        // hInstance
        NULL );

    // this is the handle to the sysex window now
    lpMidiIn->hMWnd = hWnd;
    return hWnd;
}

//-----------------------------------------------------------------------------
// GetIndex
//
// Returns the index corresponding to the message status
// index = 0 for 0x80, 1 for 0x90 ... 8 for 0xF0, 9 for 0xF1 ... 21
// for 0xFF.  index is -1 for EOX.
//-----------------------------------------------------------------------------
WORD GetIndex(BYTE msg)
{
    WORD index;

    if(msg == EOX)
        return (WORD)-1;                // skip eox

    if((msg & 0xF0) < SYSEX)         // is it a channel message?
        index = ((msg & 0xF0) >> 4) - 8;
    else                                // or a system message?
    {
        if(msg > EOX)
            index = (msg & 0x0F) + 6;
        else
            index = (msg & 0x0F) + 7;
    }

    return index;




Page 316

}

//-----------------------------------------------------------------------------
// GetEventTimeInTicks
//
// Returns the event time in ticks at the current tempo and resolution.
//-----------------------------------------------------------------------------
DWORD GetEventTimeInTicks(LPMIDIIN lpMidiIn, DWORD time, LPMIDIHDR lpMidiHdr, WORD type)
{
    DWORD rt;           // returned time in ticks
    LPSYNC lpSync;      // local pointer to sync device
    DWORD tmpo;         // tempo in uS/beat
    DWORD dur;          // sysex buffer duration in ticks

    // if sync is enabled, calculate time in ticks, else return time in mS
    if(lpMidiIn->lpSync == NULL)
        return time;

    lpSync = (LPSYNC)lpMidiIn->lpSync;

    if(type == MSYSEX)
    {
        // get tempo in uS/beat
        tmpo = lpSync->dwTempo / SCALE;

        // since sysex notification occurs at the end of a buffer calculate the
        // duration of the buffer in ticks and subtract from elaspsed ticks.
        // This works for buffers up to 4.473 seconds long at 960 bpm, or about
        // 13K long.
        dur = (320L * lpMidiHdr->dwBytesRecorded * (DWORD)lpSync->wResolution)/tmpo;
    }
    else
        dur = 0L;

    // calculate the elaspsed ticks
    rt = lpSync->dwTicks - lpMidiIn->dwLastEventTicks - dur;
    lpMidiIn->dwLastEventTicks = lpSync->dwTicks - dur;
    return rt;
}

//-----------------------------------------------------------------------------
// MidiInCallback
//
// This function is called at interrupt time.  It saves the message in the
// queue, or if a sysex, it puts the sysex header in the list and posts a
// MIDI_DATA message to the client window.
//
// dwInstance is the lpMidiIn
//
// For MIM_DATA
//   dwParam1 is the midi event with status in the low byte
//   dwParam2 is the event time in ms since the start of midi in
//
// For MIM_LONGDATA
//   dwParam1 is the lpMidiHdr
//   dwParam2 is the event time in ms since the start of midi in
//       Note that this time is for the last byte in the buffer _not_ the first.
//-----------------------------------------------------------------------------
void CALLBACK _export MidiInCallback(HMIDIIN hMI, UINT wMsg, DWORD dwInstance,
        DWORD dwParam1, DWORD dwParam2)
{
    LPMIDIIN lpMidiIn;      // pointer to MidiIn structure
    LPMIDIHDR lpMidiHdr;        // pointer to MidiHdr structure
    DWORD FAR* lpB;         // pointer to buffer event as dwords

    // the pointer to the midi in structure is passed in the instance data
    lpMidiIn = (LPMIDIIN) dwInstance;

    // don't process this message if the queue is not initialized yet
    // this is necessary in case midi-in occurs while the output device
    // is being opened
    if(lpMidiIn->lpMidiInDataHead == NULL)
        return;

    if(wMsg == MIM_DATA)
    {
        // if this is the sync in device, is this an mtc or midi
        // clock message?  If so, call the corresponding function
        // to process the sync
        if(lpMidiIn->dwFlags & SYNC_INPUT)
            switch(LOBYTE(LOWORD(dwParam1)))




Page 317

            {
                case MIDI_CLOCK:
                    if(((LPSYNC)lpMidiIn->lpSync)->wSyncMode == S_MIDI)
                        if(((LPSYNC)lpMidiIn->lpSync)->wFlags & SYNC_ENABLED)
                        {
                            MidiClock((LPSYNC)lpMidiIn->lpSync);
                            return;
                        }
                    break;

                case MIDI_START:
                case MIDI_CONTINUE:
                    if(((LPSYNC)lpMidiIn->lpSync)->wSyncMode == S_MIDI)
                        if(((LPSYNC)lpMidiIn->lpSync)->wFlags & SYNC_ENABLED)
                        {
                            ((LPSYNC)lpMidiIn->lpSync)->wFlags |= SYNC_RUNNING;
                            return;
                        }
                    break;

                case MIDI_STOP:
                    if(((LPSYNC)lpMidiIn->lpSync)->wSyncMode == S_MIDI)
                        if(((LPSYNC)lpMidiIn->lpSync)->wFlags & SYNC_ENABLED)
                        {
                            ((LPSYNC)lpMidiIn->lpSync)->wFlags &= ~SYNC_RUNNING;
                            return;
                        }
                    break;
            }

        // get the address of the start of the queue
        lpB = (DWORD FAR*)(lpMidiIn->lpMidiInDataHead + lpMidiIn->pMidiInDataIn);

        // store the data in the buffer
        // first, get the delta time for this event
        *lpB = GetEventTimeInTicks(lpMidiIn, dwParam2, NULL, MSHORT);
        *(lpB + 1) = dwParam1;

        // if necessary wrap the pointer
        if(++lpMidiIn->pMidiInDataIn == lpMidiIn->nMidiInSize)
                lpMidiIn->pMidiInDataIn = 0;

        // send a message to the application
        PostMessage(lpMidiIn->hWnd, MIDI_DATA, 0, (DWORD)lpMidiIn);
        return;
    }

    if((wMsg == MIM_LONGDATA) || (wMsg == MIM_LONGERROR))
    {
        // for MIM_LONG... dwParam1 is lpMidiHdr
        lpMidiHdr = (LPMIDIHDR)dwParam1;

        // add the buffer to the header list
        lpMidiIn->lpHeaderList[lpMidiIn->nHeaderIn] = (LPDWORD)lpMidiHdr;

        // point to the next slot in the list
        if(++lpMidiIn->nHeaderIn == lpMidiIn->nSysexBuffs)
            lpMidiIn->nHeaderIn = 0;

        if(lpMidiHdr->dwBytesRecorded != 0)
        {
            // get the time for this message -- this is stored in
            // dwUser in the MidiHdr structure
            if((BYTE)*(lpMidiHdr->lpData) == SYSEX)
                lpMidiHdr->dwUser = GetEventTimeInTicks(lpMidiIn, 
timeGetTime(), lpMidiHdr, MSYSEX);
            else
                lpMidiHdr->dwUser = 0L;

            // send MIDI_DATA to the app in there was any data in the
            // buffer (it will be of zero length if returned as a result
            // of a call to midiInClose
            PostMessage(lpMidiIn->hWnd, MIDI_DATA, 0, (DWORD)lpMidiIn);
        }

        return;
    }

    if(wMsg == MIM_CLOSE)
    {
        // send a message to the sysex proc indicating that the
        // device is closed and to free any remaining memory




Page 318

        PostMessage(lpMidiIn->hMWnd, MIM_CLOSE, 0, (DWORD)MAKELONG(HIWORD(lpMidiHdr),     
                      HIWORD(lpMidiIn)));
    }
}

//-----------------------------------------------------------------------------
// MidiInProc
//
// Processes the notification messages for MidiIn window.  lpMidiIn (in
// the lParam parameter) is a pointer to the midi in header that is
// receiving this sysex.
//-----------------------------------------------------------------------------
LRESULT CALLBACK _export MidiInProc(HWND hWnd, UINT iMessage, WPARAM wParam,
        LPARAM lParam)
{
    LPMIDIIN lpMidiIn;      // pointer to MidiIn structure

    lpMidiIn = (LPMIDIIN) MAKELONG(0, HIWORD(lParam));

    if(iMessage == MIM_CLOSE)
    {
        // destroy the sysex window
        DestroyWindow(lpMidiIn->hMWnd);

        // free the midi in queue
        FreeGlobalMem16(lpMidiIn->lpMidiInDataHead);

        // send a close message to the application
        SendMessage(lpMidiIn->hWnd, MIM_CLOSE, 0, 0L);

        // and the rest of the MidiIn structure
        FreeGlobalMem16(lpMidiIn);

        return 0;
    }

    // all messages that are not completely processed above must be processed here.
    return DefWindowProc( hWnd, iMessage, wParam, lParam );
}

//-----------------------------------------------------------------------------
// FreeGlobalMem
//
// Frees the global memory, gMem, that was previously allocated.
// This function expects a pointer to previously allocated global memory.
//-----------------------------------------------------------------------------
void WINAPI _export FreeGlobalMem16(void FAR* gMem)
{
    HANDLE hMem;

    hMem = (HANDLE)LOWORD(GlobalHandle(SELECTOROF(gMem)));
    if(hMem != NULL)
    {
        GlobalUnlock(hMem);
        GlobalFree(hMem);
    }
}
