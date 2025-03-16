//-----------------------------------------------------------------------------
// Maximum MIDI Programmer's ToolKit - MxMidi16.DLL
// MIDI Output Module
//
// Copyright (c) Paul A. Messick, 1994-1996
//
// Written by Paul A. Messick
//
// Provides a 16-bit API to open, close, and send midi messages,
// with system exclusive messages merged into the normal midi stream.
//-----------------------------------------------------------------------------
#include <windows.h>
#include "MxDLL.h"

//-----------------------------------------------------------------------------
// Function Prototypes used in this DLL
//-----------------------------------------------------------------------------
WORD AllocateMidiOutQueue(LPMIDIOUT lpMidiOut, DWORD dwEvents);
LPMIDIHDR AllocateMidiOutSysexBuffer(LPMIDIOUT lpMidiOut, WORD wSize);
HWND CreateMidiOutWindow(LPMIDIOUT lpMidiOut);




Page 319

WORD TurnNotesOff(LPMIDIOUT lpMidiOut);

//-----------------------------------------------------------------------------
// Global Variables
//-----------------------------------------------------------------------------
extern HINSTANCE ghInstance;    // global instance handle for this dll
extern UINT bufSize[10];            // buffer size table
extern UINT wQueueSize;         // the size of the windows message queue

// note on masks for each channel
UINT NtOn[16] = { 0x0001, 0x0002, 0x0004, 0x0008,
                 0x0010, 0x0020, 0x0040, 0x0080,
                 0x0100, 0x0200, 0x0400, 0x0800,
                 0x1000, 0x2000, 0x4000, 0x8000 };

// note off masks for each channel
UINT NtOff[16] = { 0xFFFE, 0xFFFD, 0xFFFB, 0xFFF7,
                  0xFFEF, 0xFFDF, 0xFFBF, 0xFF7F,
                  0xFEFF, 0xFDFF, 0xFBFF, 0xF7FF,
                  0xEFFF, 0xDFFF, 0xBFFF, 0x7FFF };

//-----------------------------------------------------------------------------
// GetMidiOutDescription
//
// Gets the text string description for the midi out device designated by
// wDeviceID.  If there is no device by that ID the function returns FALSE
// Otherwise, it returns TRUE and sets the string lpzDesc to the device
// description.
//-----------------------------------------------------------------------------
BOOL WINAPI _export GetMidiOutDescription16(WORD wDeviceID, LPSTR lpzDesc)
{
    MIDIOUTCAPS Caps;

    if(midiOutGetDevCaps(wDeviceID, (LPMIDIOUTCAPS) &Caps, sizeof(Caps)) ==      
                           MMSYSERR_BADDEVICEID)
        return FALSE;

    lstrcpy(lpzDesc, (LPSTR)Caps.szPname);
    return TRUE;
}

//-----------------------------------------------------------------------------
// OpenMidiOut
//
// Opens the device wDevice for midi output.
//
// hWnd is the handle to the window that is to receive any messages from
//   this device.
//
// wDeviceID ranges from 0 to one less than the number of available
//   devices and indicates which device is to be opened.
//
// hSync is the handle to the previously opened sync device that will
//   handle synchronization for this output.  If no sync is to be used
//    with this output, hSync must be zero.
//
// dwFlags are used to affect the operation of this particular midi out
//   device and can be combined using the OR operator.
//
// Current flags are:
//   ENABLE_SYSEX   if set, system exclusive messages are merged
//               into the normal midi stream that is sent
//               to the device.
//
// *DISABLE_SYSEX   if set, system exclusive messages are ignored
//
//   SYNC_OUTPUT    this output serves as a sync output for midi clock
//               sync messages for the previously opened
//              sync device lpSync.  It is ignored if
//              lpSync is NULL or the sync mode has been
//              set to S_STOPPED or S_MIDI (clock output
//               messages are not sent when receiving clock
//              sync.)
//
// The function returns the handle to the device if midi out was correctly
// opened, or an error value (less than MXMIDIERR_MAXERR) if there was an
// error opening the device.
//-----------------------------------------------------------------------------
HMOUT WINAPI _export OpenMidiOut16(HWND hWnd, WORD wDeviceID, HSYNC hSync,
        DWORD dwFlags)




Page 320

{
    LPMIDIOUT lpMO;     // pointer to allocated MidiOut structure
    WORD i;              // loop counter
    LPMIDIOUT FAR* newlpMidiOutList;    // pointer to new (larger) midi out device list
    HGLOBAL hg;         // handle to global memory
    LPSYNC lpSync = (LPSYNC)hSync;
    WORD wLastError;

    // if an attempt to open more than then max number of devices,
    // return bad ID error
    if((midiOutGetNumDevs() <= wDeviceID) && ((int)wDeviceID != MIDI_MAPPER))
        return MXMIDIERR_BADDEVICEID;

    // attempt to allocate memory for this device
    lpMO = (LPMIDIOUT)GlobalLock(GlobalAlloc(GPTR, sizeof(MidiOutStruct)));

    // check for an allocation error
    if(lpMO == NULL)
        return MXMIDIERR_NOMEM;

    // this is the handle to client window that is to receive messages
    lpMO->hWnd = hWnd;

    // the device id associated with this device
    lpMO->wDeviceID = wDeviceID;

    // is there a sync device specified?
    if(lpSync != NULL)
    {
        // yes, is this device ID already open?
        i = 0;
        while(*(lpSync->lpMidiOutList + i) != NULL)
        {
            if( ((LPMIDIOUT)*(lpSync->lpMidiOutList + i))->wDeviceID == wDeviceID)
            {
                // its already open, don't open it again
                lpMO->hMidiOut = ((LPMIDIOUT)*(lpSync->lpMidiOutList + i))->hMidiOut;
            }
        }
    }

    // attempt to open the device
    wLastError = midiOutOpen(&lpMO->hMidiOut, wDeviceID, (DWORD)MidiOutCallback,
            (DWORD)lpMO, CALLBACK_FUNCTION);

    // if there is an error opening midi out, return a null handle and
    // free the allocated structure
    if(wLastError != 0)
    {
        FreeGlobalMem16(lpMO);
        return MXMIDIERR_BADDEVICEID;
    }

    // get the queue sizes from dwFlags
    lpMO->nSysexBufSize = bufSize[(dwFlags & SXMASK) >> 8];

    // the number of (possibly) available sysex buffers that can be queued
    // at a time is equal to the size of the windows message queue/2
    lpMO->nSysexBuffsAllowed = lpMO->nAvailableBuffs = min(wQueueSize/2, (WORD)(dwFlags 
                                                    & NMASK));

    // allocate the queue for non-sysex messages
    if(AllocateMidiOutQueue(lpMO, bufSize[(dwFlags & QMASK) >> 12]) != 0)
    {
        FreeGlobalMem16(lpMO);
        return MXMIDIERR_NOMEM;
    }

    // setup the necessary flags and internal values
    lpMO->dwFlags = (dwFlags & MIDI_OUT_INTERNAL);   // set external flags only
    lpMO->lpSync = lpSync;                   // sync device, if not NULL

    // allocate an extra slot for a pointer to this midi out device in
    // the list of devices held in the lpSync device
    if(lpSync != NULL)
    {
        hg = (HGLOBAL)GlobalHandle(SELECTOROF(lpSync->lpMidiOutList));

        newlpMidiOutList = (LPMIDIOUT FAR*)GlobalLock(GlobalReAlloc(hg,
                            GlobalSize(hg) + sizeof(LPMIDIOUT), GMEM_MOVEABLE |  
                                GMEM_ZEROINIT));





Page 321

        if(newlpMidiOutList != NULL)
        {
            lpSync->lpMidiOutList = newlpMidiOutList;

            // find the last entry (before the last terminating null)
            while(*newlpMidiOutList != NULL)
                newlpMidiOutList++;

            // insert new entry
            *newlpMidiOutList = lpMO;
        }
    }

    // initialize the pointers to the round-robin list of sysex
    // buffers that will be removed later (on close)
    lpMO->lpHeaderList = NULL;

    // if ENABLE_SYSEX flag is set, setup driver to process sysex messages
    if(dwFlags & ENABLE_SYSEX)
    {
        // allocate buffer for round-robin sysex header list
        // with an extra null end-of-list entry
        if((lpMO->lpHeaderList = (LPDWORD*)GlobalLock(GlobalAlloc(GMEM_MOVEABLE |     
                    GMEM_SHARE | GMEM_ZEROINIT, (DWORD)(sizeof(DWORD) *
                ( (dwFlags & NMASK) + 1 )) )))  != NULL)
        {
            // allocate the actual sysex buffers
            for(i = 0; i < (WORD)(dwFlags & NMASK); i++)
                lpMO->lpHeaderList[i] = (LPDWORD)AllocateMidiOutSysexBuffer(lpMO,
                        lpMO->nSysexBufSize);

            // point to the first header in the list
            lpMO->nThisHeader = 0;
        }
        else
            lpMO->dwFlags &= DISABLE_SYSEX;
    }

    // create the (invisible) window that processes midi-out action messages
    CreateMidiOutWindow(lpMO);

    return (HMOUT)lpMO;
}

//-----------------------------------------------------------------------------
// FlushMidiOut
//
// Flushes any data that may be in the midiOut buffer for this device.
//-----------------------------------------------------------------------------
void WINAPI _export FlushMidiOut16(HMOUT hMidiOut)
{
    LPMIDIOUT lpMidiOut = (LPMIDIOUT)hMidiOut;

    // check for null pointer
    if(hMidiOut < MXMIDIERR_MAXERR)
        return;

    // flush any pending messages in the buffer
    lpMidiOut->pMidiOutDataOut = lpMidiOut->pMidiOutDataIn = lpMidiOut->wSpan = 0;
}

//-----------------------------------------------------------------------------
// ResetMidiOut
//
// Sends all notes off to midi device and releases any pending sysex
// buffers.  Flushes the output buffer.
// Returns 0 if successful, non-zero on error.
//-----------------------------------------------------------------------------
WORD WINAPI _export ResetMidiOut16(HMOUT hMidiOut)
{
    LPMIDIOUT lpMidiOut = (LPMIDIOUT)hMidiOut;

    // check for null pointer
    if(hMidiOut < MXMIDIERR_MAXERR)
        return 1;

    // flush the output buffer
    FlushMidiOut16(hMidiOut);

    // turn off any playing notes
    return TurnNotesOff(lpMidiOut);
}





Page 322

//-----------------------------------------------------------------------------
// TurnNotesOff
//
// Sends all notes off to midi device and releases any pending sysex
// buffers.
//-----------------------------------------------------------------------------
WORD TurnNotesOff(LPMIDIOUT lpMidiOut)
{
    int n;          // note number
    int chan;       // channel number
    WORD mask;      // channel mask
    DWORD note;     // note off message

    // send note offs for any notes that are currently on
    for(n = 0; n < 128; n++)
        if(lpMidiOut->wNotes[n] != 0)
        {
            // search through all channels to see if this note is on
            mask = 1;
            for(chan = 0; chan < 16; chan++)
            {
                if(lpMidiOut->wNotes[n] & mask)
                {
                    // build and send the note off
                    note = 0x00000090 | (n << 8) | chan;
                    midiOutShortMsg(lpMidiOut->hMidiOut, note);
                }

                // next channel
                mask <<= 1;
            }

            // clear the value in the array so it is not sent again if reset
            // is called again
            lpMidiOut->wNotes[n] = 0;
        }

    // send all notes off messages in case the above didn't do it for all notes
    // i.e. some synths require exact pairing of note on/offs
    for(n = 0; n < 16; n++)
        midiOutShortMsg(lpMidiOut->hMidiOut, 0x00007BB0 | (DWORD)n);

    // have driver send note-offs for any notes that are currently on
    // and flush any pending sysex blocks
    return midiOutReset(lpMidiOut->hMidiOut);
}

//-----------------------------------------------------------------------------
// TrackMidiOut
//
// Tracks note ons and note offs in the Note[] array of the MidiOutStruc
// structure.  This array is used to turn any still-sounding note off when
// ResetMidiOut() is called.  The array is organized as 128 words, one
// word per note number.  Each bit of the words corresponds to the channel
// of the note, with the lsb corresponding to channel 1.  A one in a bit
// position indicates that that note on that channel is currently on.
//-----------------------------------------------------------------------------
void TrackMidiOut(LPMIDIOUT lpMidiOut, DWORD dwMsg)
{
    void FAR *n;            // pointer to the note data
    BYTE FAR *status;       // status byte
    BYTE FAR *note;     // note number
    BYTE FAR *vel;      // velocity
    BYTE noChanStatus;  // status with channel masked

    // cast pointers to the double word message to access the message as bytes
    n = &dwMsg;
    status = (BYTE FAR*)n;
    note = (BYTE FAR*)n + 1;
    vel = (BYTE FAR*)n + 2;
    noChanStatus = *status & 0xF0;

    // set the bit in the array if this is a note on or clear it if a note off
    // make sure that the two data bytes are valid data by forcing
    // the upper bits of each byte to a zero
    if((noChanStatus == NOTEON) && (*vel != 0))
        lpMidiOut->wNotes[0x7f & *note] |= NtOn[*status & 0x0F];
    else
        if((noChanStatus == NOTEOFF) || (noChanStatus == NOTEON))
            lpMidiOut->wNotes[0x7f & *note] &= NtOff[*status & 0x0F];




Page 323

}

//-----------------------------------------------------------------------------
// CloseMidiOut
//
// Closes the midi device lpMidiOut and frees the lpMidiOut structure.
// Returns 0 if successful, non-zero on error.
//-----------------------------------------------------------------------------
WORD WINAPI _export CloseMidiOut16(HMOUT hMidiOut)
{
    LPMIDIOUT FAR* newlpMidiOutList;     // pointer to midi out device list
    HGLOBAL hg;                     // handle to global memory
    WORD nDev;                      // device counter
    LPSYNC lpS;                     // local pointer to sync device
    int i;                          // loop counter
    LPMIDIOUT lpMidiOut = (LPMIDIOUT)hMidiOut;

    // if not open, ignore request
    if(hMidiOut < MXMIDIERR_MAXERR)
        return 0;

    // reset the output device, forcing it to return any pending buffers
    ResetMidiOut16(hMidiOut);

    // if sysex was enabled, remove sysex buffers from list and free memory
    if(lpMidiOut->dwFlags & ENABLE_SYSEX)
    {
        // the list is terminated by a null entry
        i = 0;
        while(lpMidiOut->lpHeaderList[i] != NULL)
        {
            // this sysex buffer is to be removed: unprepare it
            midiOutUnprepareHeader(lpMidiOut->hMidiOut, 
                (LPMIDIHDR)lpMidiOut->lpHeaderList[i], (WORD)sizeof(MIDIHDR));

            // free the memory
            FreeGlobalMem16(((LPMIDIHDR)lpMidiOut->lpHeaderList[i])->lpData);
            FreeGlobalMem16(lpMidiOut->lpHeaderList[i]);

            // next item in list
            i++;
        }

        // free the (empty) list
        FreeGlobalMem16(lpMidiOut->lpHeaderList);
    }

    // close midi out
    midiOutClose(lpMidiOut->hMidiOut);

    // if sync is enabled, remove the device from the sync device list
    if(lpMidiOut->lpSync != NULL)
    {
        // create a convenient pointer to lpSync
        lpS = (LPSYNC)(lpMidiOut->lpSync);

        // search through the list to find this device pointer
        nDev = 0;
        while((*(lpS->lpMidiOutList + nDev) != NULL) && (*(lpS->lpMidiOutList +
                nDev) != lpMidiOut))
            nDev++;

        // remove it from the list by copying all of the other
        // pointers up one slot
        while(*(lpS->lpMidiOutList + nDev) != NULL)
        {
            *(lpS->lpMidiOutList + nDev) = *(lpS->lpMidiOutList + nDev + 1);
            nDev++;
        }

        // resize the memory block
        hg = (HGLOBAL)GlobalHandle(SELECTOROF(lpS->lpMidiOutList));

        newlpMidiOutList = (LPMIDIOUT FAR*)GlobalLock(GlobalReAlloc(hg,
                    GlobalSize(hg) - sizeof(LPMIDIOUT), GMEM_MOVEABLE |      
                        GMEM_SHARE));

        if(newlpMidiOutList != NULL)
            lpS->lpMidiOutList = newlpMidiOutList;
    }

    return 1;
}





Page 324

//-----------------------------------------------------------------------------
// InsertInSysexBuffer
//
// Insert this midi event into the sysex buffer.  If the buffer is full
// send a message to have it sent to the driver, then find the next buffer.
//-----------------------------------------------------------------------------
void InsertInSysexBuffer(LPMIDIOUT lpMidiOut, LPMIDIEVENT thisEvent)
{
    LPMIDIHDR lpMidiHdr;

    // insert the event into the sysex buffer
    lpMidiHdr = (LPMIDIHDR)lpMidiOut->lpHeaderList[lpMidiOut->nThisHeader];

    *(lpMidiHdr->lpData) = thisEvent->data1;
    lpMidiHdr->lpData++;

    // if buffer is full, send a message to have it sent to the driver
    if((++lpMidiHdr->dwBytesRecorded == lpMidiHdr->dwBufferLength) ||
            (thisEvent->data1 == EOX))
    {
        // point to the next buffer in the list
        // if this is the end of the list (i.e. a null entry) then wrap around
        // to the head of the list
        lpMidiOut->nThisHeader++;
        if(lpMidiOut->lpHeaderList[lpMidiOut->nThisHeader] == NULL)
            lpMidiOut->nThisHeader = 0;

        // one less buffer is available now
        lpMidiOut->nAvailableBuffs--;

        // we are now sending a sysex message
        lpMidiOut->dwFlags |= SENDING_SYSEX;

        // one more buffer is active
        if(lpMidiOut->lpSync != NULL)
            ((LPSYNC)lpMidiOut->lpSync)->nSysexBuffsActive++;

        // post a message so that the header will be sent to the driver.
        // This has to be done since the driver may not return before the buffer
        // has been sent.
        PostMessage(lpMidiOut->hMWnd, MOM_LONGDATA, 0, (LPARAM)MAKELONG(
                HIWORD(lpMidiHdr), HIWORD(lpMidiOut)));
    }
}

//-----------------------------------------------------------------------------
// PutMidiOut
//
// Puts a midi event into the outgoing queue.  If no sync device is attached,
// the event is then removed from the queue and sent to the MIDI output driver.
// For sysex data, the event is added to the the current buffer; when the buffer
// is full, it is sent to the driver as a long message.
//
// If sync is being used, the event, sysex or short, is always queued.  It will
// then be output at the proper time by the sync engine.
//
// Returns 0 if event accepted, -1 if queue is full.
//-----------------------------------------------------------------------------
WORD WINAPI _export PutMidiOut16(HMOUT hMidiOut, LPMIDIEVENT lpMidiEvent)
{
    DWORD dwMsg;                // echo message
    DWORD FAR* lpE;         // pointer to event structure
    DWORD FAR* lpB;         // pointer to buffer event
    LPMIDIOUT lpMidiOut = (LPMIDIOUT)hMidiOut;

    // check for null pointer
    if(hMidiOut < MXMIDIERR_MAXERR)
        return (WORD)-1;

        // get pointer to event for access as dword
    lpE = (DWORD FAR*)lpMidiEvent;

    // The message can be sent out immediately, without buffering,
    // by setting the time to -1L and calling MidiOut().
    // This can be used during syncronized record or playback to echo
    // incoming midi data to a particular output.  Since this is
    // unbuffered output, system exclusive messages are filtered and
    // cannot be echoed through this method.
    if(lpMidiEvent->time == (DWORD)-1L)
    {





Page 325

        // filter sysex from echo out
        if(lpMidiEvent->status == SYSEX)
            return 0;

        // build the event as a dword
        dwMsg = (DWORD)*(lpE + 1);

        // send the message to the driver
        // the driver could return MIDIERR_NOTREADY here, but no
        // credible drivers will do so.  If desired, this return
        // value could be handled here.
        midiOutShortMsg(lpMidiOut->hMidiOut, dwMsg);
        TrackMidiOut(lpMidiOut, dwMsg);
        return 0;
    }

    // if this is a sysex message and no more buffers are available for
    // queueing (because the "windows messaging queue size - 1" worth
    // of buffers have been sent) then refuse the message
    if((lpMidiEvent->status == SYSEX) && (lpMidiOut->nAvailableBuffs == 0))
        return (WORD)-1;

    // is this part of a sysex message?
    // if sync is enabled, treat the sysex as a regular midi message
    // Otherwise, insert the sysex in an outgoing buffer
    if(lpMidiEvent->status == SYSEX)
        if(lpMidiOut->dwFlags & ENABLE_SYSEX)
            if((lpMidiOut->lpSync == NULL) || (((LPSYNC)lpMidiOut->lpSync)->wFlags
                    & SYNC_ENABLED) == 0)
            {
                // if sync not enabled, insert the event in the
                // outgoing sysex buffer
                InsertInSysexBuffer(lpMidiOut, lpMidiEvent);
                return 0;
            }

    // just a normal midi event
    // check the wSpan to see if queue is full
    if(lpMidiOut->wSpan == lpMidiOut->nMidiOutSize)
        return (WORD)-1;

    // save event in the queue - 1st dword is time, 2nd dword is event
    lpB = (DWORD FAR*)(lpMidiOut->pMidiOutDataIn + lpMidiOut->lpMidiOutDataHead);
    *lpB = *lpE;
    *(lpB + 1) = *(lpE + 1);

    // if necessary, wrap the pointer
    if(lpMidiOut->pMidiOutDataIn++ == lpMidiOut->nMidiOutSize)
        lpMidiOut->pMidiOutDataIn = 0;

    // one more in the wSpan
    lpMidiOut->wSpan++;

    // send any queued messages to the driver
    if((LPSYNC)lpMidiOut->lpSync == NULL)
        while(lpMidiOut->wSpan != 0)
        {
            // read the next event - 2nd dword is event data
            lpE = (DWORD FAR*)(lpMidiOut->lpMidiOutDataHead + 
                    lpMidiOut->pMidiOutDataOut);
            dwMsg = *(lpE + 1);

            // increment the out pointer, wrapping if necessary
            if(lpMidiOut->pMidiOutDataOut++ == lpMidiOut->nMidiOutSize)
                lpMidiOut->pMidiOutDataOut = 0;

            lpMidiOut->wSpan--;

            // send the event to the driver
            midiOutShortMsg(lpMidiOut->hMidiOut, dwMsg);
            TrackMidiOut(lpMidiOut, dwMsg);
        }

    return 0;
}

//-----------------------------------------------------------------------------
// AllocateMidiOutQueue
//
// allocates a queue of dwEvents size to midi-out device lpMidiOut.
// Returns 0 if successful, MXMIDIERR_NOMEM if there is not enough global
// memory to allocate the queue.
//-----------------------------------------------------------------------------




Page 326

WORD AllocateMidiOutQueue(LPMIDIOUT lpMidiOut, DWORD dwEvents)
{
    LPMIDIEVENT lpQueue;            // pointer to the newly allocated queue
    DWORD dwBytes;              // number of bytes to allocate

    // calculate number of bytes to allocate
    dwBytes = (dwEvents + 1) * sizeof(MidiEvent);

    // abort if not enough memory
    if((lpQueue = (LPMIDIEVENT)GlobalLock(GlobalAlloc(GPTR | GMEM_SHARE, dwBytes))) 
== NULL)
        return MXMIDIERR_NOMEM;

    // queue exists, setup pointers
    lpMidiOut->lpMidiOutDataHead = lpQueue;
    lpMidiOut->pMidiOutDataIn = lpMidiOut->pMidiOutDataOut = 0;
    lpMidiOut->nMidiOutSize = (int)dwEvents;
    lpMidiOut->wSpan = 0;

    return 0;
}

//-----------------------------------------------------------------------------
// AllocateMidiOutSysexBuffer
//
// Adds a buffer of wSize bytes to the previously opened midi out, pointed
// to by lpMidiOut.  Returns a pointer to the midi header structure on
// success or NULL on failure.
//-----------------------------------------------------------------------------
LPMIDIHDR AllocateMidiOutSysexBuffer(LPMIDIOUT lpMidiOut, WORD wSize)
{
    LPMIDIHDR lpMidiHdr;        // pointer to header structure for this buffer
    LPSTR lpData;           // pointer to sysex buffer

    // allocate MIDIHDR structure
    lpMidiHdr = (LPMIDIHDR)GlobalLock(GlobalAlloc(GMEM_MOVEABLE | GMEM_SHARE,    
                                                sizeof(MIDIHDR)));

    if(lpMidiHdr == NULL)
        return NULL;

    // allocate buffer
    lpData = (LPSTR)GlobalLock(GlobalAlloc(GMEM_MOVEABLE | GMEM_SHARE, (DWORD)wSize));

    // if not enough memory for buffer, free header memory
    if(lpData == NULL)
    {
        FreeGlobalMem16(lpMidiHdr);
        return NULL;
    }

    // setup midihdr structure
    lpMidiHdr->lpData = lpData;          // pointer to start of buffer
    lpMidiHdr->dwBytesRecorded = 0L;     // number of bytes in buffer
    lpMidiHdr->dwBufferLength = wSize;       // buffer size
    lpMidiHdr->dwFlags = 0L;             // initialize flags
    lpMidiHdr->dwUser = 0L;              // user data

    // prepare the header
    if(midiOutPrepareHeader(lpMidiOut->hMidiOut, lpMidiHdr, sizeof(MIDIHDR)) != 0)
    {
        FreeGlobalMem16(lpMidiHdr->lpData);
        FreeGlobalMem16(lpMidiHdr);
        return NULL;
    }

    return lpMidiHdr;
}

//-----------------------------------------------------------------------------
// CreateMidiOutWindow
//
// Registers and creates a window that will receive messages from the
// MidiOutCallback function.  Returns a handle to the new window,
// or NULL on error.
//-----------------------------------------------------------------------------
HWND CreateMidiOutWindow(LPMIDIOUT lpMidiOut)
{
    HWND hWnd;      // handle to the new window

    // create the (invisible) window
    hWnd = CreateWindow(MIDIOUTCLASS, // lpClassName
        MIDIOUTCLASS,               // lpWindowName




Page 327

        WS_DISABLED,                    // dwStyle
        CW_USEDEFAULT,              // X position
        CW_USEDEFAULT,              // Y position
        CW_USEDEFAULT,              // nWidth
        CW_USEDEFAULT,              // nHeight
        NULL,                       // hWinParent
        NULL,                       // hMenu
        ghInstance,                 // hInstance
        NULL);

    // this is the handle to the sysex window now
    lpMidiOut->hMWnd = hWnd;
    return hWnd;
}

//-----------------------------------------------------------------------------
// MidiOutCallback
//
// This function is called at interrupt time.  It processes MOM_DONE and
// MOM_CLOSE messages from the driver.  MOM_DONE indicates that the driver
// has finished sending the last sysex block and is ready for another one.
// MOM_CLOSE is sent by the driver when it has finished any cleanup that
// resulted from a midiOutClose().
//-----------------------------------------------------------------------------
void CALLBACK _export MidiOutCallback(HMIDIOUT hMO, UINT wMsg, DWORD dwInstance,
        DWORD dwParam1, DWORD dwParam2)
{
    LPMIDIOUT lpMidiOut;        // pointer to MidiOut structure
    LPMIDIHDR lpMidiHdr;        // pointer to MidiHdr structure

    lpMidiOut = (LPMIDIOUT)dwInstance;

    if(wMsg == MOM_DONE)
    {
        // for MOM_DONE dwParam1 is lpMidiHdr
        lpMidiHdr = (LPMIDIHDR)dwParam1;

        // Some drivers send spurious MOM_DONE messages with dwParam1 == NULL
        // don't process these...
        if(lpMidiHdr == NULL || lpMidiOut == NULL)
            return;

        // prepare the header again for reuse
        // The block will remain in the lpHeaderList list to be reused later.
        midiOutPrepareHeader(lpMidiOut->hMidiOut, lpMidiHdr, sizeof(MIDIHDR));

        lpMidiHdr->dwBytesRecorded = 0L;
        lpMidiHdr->dwBufferLength = lpMidiOut->nSysexBufSize;

        // one less buffer is active
        if(lpMidiOut->lpSync != NULL)
            ((LPSYNC)lpMidiOut->lpSync)->nSysexBuffsActive--;

        // one more buffer is available
        lpMidiOut->nAvailableBuffs++;

        // are we done sending this sysex?
        if(lpMidiOut->nAvailableBuffs == lpMidiOut->nSysexBuffsAllowed)
            lpMidiOut->dwFlags &= ~SENDING_SYSEX;

        // if no sync device, post a message to let the client know there is
        // more room for sysex data -- otherwise this is handled by the sync device
        if(lpMidiOut->lpSync == NULL)            
            PostMessage(lpMidiOut->hWnd, OUTBUFFER_READY, 0, (DWORD)lpMidiOut);
    }

    if(wMsg == MOM_CLOSE)
    {
        // send a message to the sysex proc indicating that the
        // device is closed and to free any remaining memory
        PostMessage(lpMidiOut->hMWnd, MOM_CLOSE, 0, (DWORD)MAKELONG(
                HIWORD(lpMidiHdr), HIWORD(lpMidiOut)));
    }
}

//-----------------------------------------------------------------------------
// MidiOutProc
//
// Processes the messages for the Midi Out window.
// If the message is MOM_CLOSE, then cleanup the hidden window and memory
// allocations and alert the app that the driver is closed.
//-----------------------------------------------------------------------------




Page 328

LRESULT CALLBACK _export MidiOutProc(HWND hWnd, UINT iMessage, WPARAM wParam,
    LPARAM lParam)
{
    LPMIDIOUT lpMidiOut;    // pointer to MidiOut structure
    LPMIDIHDR lpMidiHdr;    // pointer to MidiHdr structure

    lpMidiOut = (LPMIDIOUT) MAKELONG(0, HIWORD(lParam));
    lpMidiHdr = (LPMIDIHDR) MAKELONG(0, LOWORD(lParam));

    if(iMessage == MOM_CLOSE)
    {
        // destroy the sysex window
        DestroyWindow(lpMidiOut->hMWnd);

        // free the midi out queue
        FreeGlobalMem16(lpMidiOut->lpMidiOutDataHead);

        // send a close message to the application
        SendMessage(lpMidiOut->hWnd, MOM_CLOSE, 0, 0L);

        // and the rest of the MidiOut structure
        FreeGlobalMem16(lpMidiOut);
    }

    if(iMessage == MOM_LONGDATA)
    {
        // reset data pointer to start of buffer
        lpMidiHdr->lpData -= lpMidiHdr->dwBytesRecorded;
        lpMidiHdr->dwBufferLength = lpMidiHdr->dwBytesRecorded;

        // send the midi header to the driver
        midiOutLongMsg(lpMidiOut->hMidiOut, lpMidiHdr, sizeof(MIDIHDR));
    }

    // all messages that are not completely processed above must be processed here.
    return DefWindowProc(hWnd, iMessage, wParam, lParam);
}
