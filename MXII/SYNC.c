//-----------------------------------------------------------------------------
// Maximum MIDI Programmer's ToolKit - MxMidi16.DLL
// MIDI Event Synchronization Module
//
// Copyright (c) Paul A. Messick, 1994-1996
//
// Written by Paul A. Messick
//
// Provides functions to play and time midi events.
//
// The algorithm used here provides timing of midi events that is
// independent of the available timer interrupt rate.  Resolutions of up
// to 960 ticks/beat at tempos between 10 and 250 beats/min are
// effectively handled.
//
// The maximum error in timing when syncing to tape is approximately
// 1.3 ns per smpte quarter frame, or more than 26 minutes per tick at
// 960 tpb.
//
// The error calculation is as follows:
//   error = 1/3 uS per qF / 256 = 1.3 nS/qF
//   error = 1.3nS/qF * 4qF/fr * 30fr/sec = 156 nS/sec
//   960t/b * 250b/min * 1min/60sec * 1sec/1e9nS = 4e-6 ticks/ns
//
//   error = 156 nS/sec * 4e-6 ticks/nS = 624e-6 ticks/sec
//       = 1602 sec/tick
//   error = 1602 sec/tick * 1min/60sec = 26.7 min/tick
//-----------------------------------------------------------------------------

#include <windows.h>
#include "MxDLL.h"

//-----------------------------------------------------------------------------
// Function Prototypes used in this DLL module
//-----------------------------------------------------------------------------
void InsertInSysexBuffer(LPMIDIOUT lpMidiOut, LPMIDIEVENT thisEvent);
void TrackMidiOut(LPMIDIOUT lpMidiOut, DWORD dwMsg);
WORD TurnNotesOff(LPMIDIOUT lpMidiOut);

//-----------------------------------------------------------------------------




Page 329

// OpenSync
//
// Enables sync for playback and record for the sync mode selected.
// To change the sync mode of a previously opened sync device, open the
// device again, passing the old hSync handle.  Reopening can be used
// to change hWnd, mode, and/or wTimerPeriod.  Any previous sync input
// setting is preserved.
//
// On reopen, the mode and/or wTimerPeriod parameters can be set to
// USE_CURRENT to preserve previous settings.
//
// hSync is the handle to the already opened sync device, if reopening in order
//  to change settings, or zero if initially opening a device.
//
//  hWnd is the handle to the window that will receive sync messages
//       MIDI_BEAT is sent on every beat at the current tempo
//       OUTBUFFER_READY is sent when a particular output is below
//           25% full and is ready for more data
//
// mode can be set to:
//   S_INT      internal sync
//   S_MIDI     midi clock sync
//
// wTimerPeriod is the requested timer period in mS.  The final actual
// period may be greater than the requested period, depending on the
// capabilities of the installed Windows timer driver.
//
// the function returns a handle to the sync device if sync was correctly opened,
// or an error value (less than MXMIDIERR_MAXERR) if there was an error opening.
//-----------------------------------------------------------------------------
HSYNC WINAPI _export OpenSync16(HSYNC hSync, HWND hWnd, WORD mode, WORD wTimerPeriod)
{
    TIMECAPS timeCaps;      // timer capabilities structure
    LPSYNC lpSY;                // pointer to sync structure
    LPSYNC lpSync = (LPSYNC)hSync;

    // check to see if already open
    // open is defined as lpSync having a corresponding valid handle
    // the assumption here is that lpSync might have garbage in it and
    // that that garbage is not accidentally a valid pointer
    if((hSync > MXMIDIERR_MAXERR) && (GlobalHandle(SELECTOROF(lpSync)) != NULL))
    {
        // already open, use the old pointer
        lpSY = lpSync;

        // if timer was active, kill the timer
        if(lpSY->wTimerID != NULL)
        {
            // stop the timer
            timeKillEvent(lpSync->wTimerID);

            // release the timer
            timeEndPeriod(lpSync->wTimerPeriod);
        }
    }
    else
    {
        // attempt to allocate memory for this device
        lpSY = (LPSYNC)GlobalLock(GlobalAlloc(GMEM_MOVEABLE | GMEM_SHARE |       
                                              GMEM_ZEROINIT,
                 sizeof(SyncStruct)));

        // check for an allocation error
        if(lpSY == NULL)
            return MXMIDIERR_NOMEM;

        // allocate memory for start of list of midi out devices
        // that will be serviced by this sync device
        lpSY->lpMidiOutList = (LPMIDIOUT FAR*)GlobalLock(GlobalAlloc(GMEM_MOVEABLE |
             GMEM_SHARE | GMEM_ZEROINIT, sizeof(LPMIDIOUT)));
    }

    // this is the handle to the window that is to receive messages
    lpSY->hWnd = hWnd;

    // set the new sync mode
    if(mode != USE_CURRENT)
        lpSY->wSyncMode = mode;

    // sync starts out disabled




Page 330

    lpSY->wFlags = 0;

    // no sysex buffers are currently active
    lpSY->nSysexBuffsActive = 0;

    // setup timer settings
    if(wTimerPeriod != USE_CURRENT)
        lpSY->wTimerPeriod = wTimerPeriod;

    lpSY->wTimerID = NULL;

    // set the default resolution and tempo for midi playback
    SetResolution16((HSYNC)lpSY, 480);      // 480 ticks per beat
    SetTempo16((HSYNC)lpSY, 500000L);       // 120 bpm

    // enable the timer
    // get the capabilities of the timer driver
    if(timeGetDevCaps((LPTIMECAPS)&timeCaps, sizeof(timeCaps)) != 0)
    {
        FreeGlobalMem16(lpSY);
        return MMSYSERR_ALLOCATED;
    }

    // set the timer to the desired period
    if(timeCaps.wPeriodMin <= lpSY->wTimerPeriod)
        lpSY->wTimerPeriod = lpSY->wTimerPeriod;
    else
        lpSY->wTimerPeriod = timeCaps.wPeriodMin;

    if(timeBeginPeriod(lpSY->wTimerPeriod) != 0)
    {
        FreeGlobalMem16(lpSY);
        return MMSYSERR_ALLOCATED;
    }

    // start the timer
    if((lpSY->wTimerID = timeSetEvent(lpSY->wTimerPeriod, lpSY->wTimerPeriod,
        (LPTIMECALLBACK)syncTimer, (DWORD)lpSY, TIME_PERIODIC)) == NULL)
    {
        FreeGlobalMem16(lpSY);
        return MMSYSERR_ALLOCATED;
    }

    return (HSYNC)lpSY;
}

//-----------------------------------------------------------------------------
// CloseSync
//
// Closes the sync device and frees the Sync structure.
// Returns 0 if successful, non-zero on error.
//-----------------------------------------------------------------------------
WORD WINAPI _export CloseSync16(HSYNC hSync)
{
    LPMIDIOUT FAR* lpMOL;   // pointer to item in midiout list
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return 0;

    // was internal sync enabled?
    if(lpSync->wTimerID != NULL)
    {
        // stop the timer
        timeKillEvent(lpSync->wTimerID);

        // release the timer
        timeEndPeriod(lpSync->wTimerPeriod);
    }

    // disconnect all attached midi out devices from this sync device
    lpMOL = lpSync->lpMidiOutList;
    while(*lpMOL != NULL)
    {
        ((*lpMOL)->lpSync) = NULL;
        lpMOL++;
    }

    // free the lpMidiOutList structure
    FreeGlobalMem16(lpSync->lpMidiOutList);

    // free the Sync structure
    FreeGlobalMem16(lpSync);




Page 331

    return 1;
}

//-----------------------------------------------------------------------------
// StopSync
//
// Disables the sync device, but does not remove the timer, if enabled.
// In contrast to the printed documentation, this function does flush
// the output buffers.
//-----------------------------------------------------------------------------
void WINAPI _export StopSync16(HSYNC hSync)
{
    LPMIDIOUT FAR* lpMOL;       // pointer to item in midiout list
    LPMIDIOUT lpMO;         // pointer to this midi out
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return;

    // disable the timer
    lpSync->wFlags = 0;

    // reset midi out for each output
    lpMOL = lpSync->lpMidiOutList;
    while(*lpMOL != NULL)
    {
        // send midi stop if not in midi clock sync mode
        if(lpSync->wSyncMode != S_MIDI)
        {
            // send start if not midi sync
            lpMO = ((LPMIDIOUT)(*lpMOL));

            if(lpMO->dwFlags & SYNC_OUTPUT)
                midiOutShortMsg(lpMO->hMidiOut, (DWORD)MIDI_STOP);
        }

        // reset the output
        ResetMidiOut16((HMOUT)(*lpMOL));
        lpMOL++;
    }
}

//-----------------------------------------------------------------------------
// PauseSync
//
// Disables the sync device, but does not remove the timer, if enabled.
// This function does not flush the midi output buffers.  If reset is TRUE
// then MidiOutReset is called, turning off any currently playing notes and
// flushing the sysex buffers.  If reset is FALSE then MidiOutReset is
// not called.  Pausing sync without resetting Midi out is intended for
// short duration pauses, since "stuck notes" may result from prolonged
// pauses without resetting the outputs.
//-----------------------------------------------------------------------------
void WINAPI _export PauseSync16(HSYNC hSync, BOOL reset)
{
    LPMIDIOUT FAR* lpMOL;       // pointer to item in midiout list
    LPMIDIOUT lpMO;         // pointer to this midi out
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return;

    // save the state of the SYNC_RUNNING flag for later
    lpSync->wFlags &= ~RUNNING_STATUS;
    lpSync->wFlags |= (lpSync->wFlags & SYNC_RUNNING ? RUNNING_STATUS : 0);

    // disable the timer
    lpSync->wFlags &= ~SYNC_RUNNING;
        lpSync->wFlags &= ~SYNC_ENABLED;

    // if the reset parameter is TRUE, reset midi out for each output
    if(reset)
    {
        lpMOL = lpSync->lpMidiOutList;
        while(*lpMOL != NULL)
        {
            // send midi stop if not in midi clock sync mode
            if(lpSync->wSyncMode != S_MIDI)
            {




Page 332

                // send start if not midi sync
                lpMO = ((LPMIDIOUT)(*lpMOL));

                if(lpMO->dwFlags & SYNC_OUTPUT)
                    midiOutShortMsg(lpMO->hMidiOut, (DWORD)MIDI_STOP);
            }

            // turn off any currently playing notes
            TurnNotesOff((LPMIDIOUT)(*lpMOL));

            lpMOL++;
        }
    }
}

//-----------------------------------------------------------------------------
// StartSync
//
// Enables the sync device and clears the ticks count.
//-----------------------------------------------------------------------------
void WINAPI _export StartSync16(HSYNC hSync)
{
    LPMIDIOUT FAR* lpMOL;
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return;

    // reset sync
    lpSync->dwFticks = 0L;
    lpSync->dwTicks = 0L;
    lpSync->nTicksSinceClock = 0;
    lpSync->nTicksSinceBeat = 0;
    lpSync->wTempoTicks = 0;
    lpSync->dwLastTicks = 0L;
    lpSync->msPosition = 0L;

    // clear the time of the last event
    lpMOL = lpSync->lpMidiOutList;
    while(*lpMOL != NULL)
    {
        ((LPMIDIOUT)(*lpMOL))->dwLastEventTicks = 0L;
        lpMOL++;
    }

    ReStartSync16(hSync);
}

//-----------------------------------------------------------------------------
// ReStartSync
//
// Enables the sync device from the current ticks position.
//-----------------------------------------------------------------------------
void WINAPI _export ReStartSync16(HSYNC hSync)
{
    LPMIDIOUT FAR* lpMOL;       // pointer to item in midiout list
    LPMIDIOUT lpMO;         // pointer to this midi out
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return;

    // enable the timer
    lpSync->wFlags |= SYNC_ENABLED;

    // allow a sync_done to be sent
    lpSync->wFlags &= ~SENT_SYNCDONE;

    // if its not S_MIDI sync...
    if(lpSync->wSyncMode != S_MIDI)
    {
        // start the timer running (if S_MIDI it will be
        // started by reception of MIDI_START message)
        lpSync->wFlags |= SYNC_RUNNING;

        // send start if not midi sync
        lpMOL = lpSync->lpMidiOutList;
        while(*lpMOL != NULL)
        {
            lpMO = ((LPMIDIOUT)(*lpMOL));
            if(lpMO->dwFlags & SYNC_OUTPUT)




Page 333

                midiOutShortMsg(lpMO->hMidiOut, (DWORD)MIDI_START);

            lpMOL++;
        }
    }
    else
    // restore the status of the SYNC_RUNNING flag
    // for the S_MIDI sync mode
    if(lpSync->wFlags & RUNNING_STATUS)
    {
        lpSync->wFlags &= ~RUNNING_STATUS;
        lpSync->wFlags |= SYNC_RUNNING;
    }
}

//-----------------------------------------------------------------------------
// SetTempo
//
// Sets the current tempo.  This function may be called at
// any time to change the tempo.  The tempo is set in micro-
// seconds per midi beat so that tempo may be set fractionally.
//
// Returns 0 if successful, TIMERR_NOCANDO if the uSPerBeat parameter is
// zero.
//-----------------------------------------------------------------------------
WORD WINAPI _export SetTempo16(HSYNC hSync, DWORD uSPerBeat)
{
    LPMIDIIN lpMidiIn;  // local pointer to midi in structure
    DWORD dwTicks;      // num of elaspsed ticks for tempo event
    LPMIDIEVENT Qptr;       // pointer to buffer event
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return MXMIDIERR_BADHANDLE;

    // tempo must be greater than zero
    if(uSPerBeat == 0)
        return MXMIDIERR_BADTEMPO;

    lpSync->dwTempo = uSPerBeat * SCALE;

    // Test for uninitialized lpSyncIn -- without a sync input
    // device there is no valid destination for (sync) tempo changes
    // if sync is running, insert the tempo change in the buffer
    if((lpSync->wFlags & SYNC_RUNNING) && (lpSync->lpSyncIn != NULL))
    {
        lpMidiIn = lpSync->lpSyncIn;

        // calculate the elaspsed ticks
        dwTicks = lpSync->dwTicks - lpMidiIn->dwLastEventTicks;
        lpMidiIn->dwLastEventTicks = lpSync->dwTicks;

        // get a pointer to the buffer
        Qptr = (LPMIDIEVENT)(lpMidiIn->lpMidiInDataHead + lpMidiIn->pMidiInDataIn);

        // store the data in the buffer
        // make sure status is zero.
        Qptr->time = dwTicks;
        Qptr->status = 0;
        Qptr->data1 = LOBYTE(HIWORD(uSPerBeat));
        Qptr->data2 = HIBYTE(LOWORD(uSPerBeat));
        Qptr->data3 = LOBYTE(LOWORD(uSPerBeat));

        // if necessary wrap the pointer
        if(lpMidiIn->pMidiInDataIn++ == lpMidiIn->nMidiInSize)
                lpMidiIn->pMidiInDataIn = 0;

        // send a message to the application
        PostMessage(lpMidiIn->hWnd, MIDI_DATA, 0, (DWORD)lpMidiIn);
    }

    return 0;
}

//-----------------------------------------------------------------------------
// SetResolution
//
// Sets the resolution in ticks per midi beat.  Higher values
// of resolution provide greater timing accuracy.  This function should
// be called after the device is opened and before playback or record
// starts.




Page 334

//-----------------------------------------------------------------------------
void WINAPI _export SetResolution16(HSYNC hSync, WORD resolution)
{
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return;

    if(resolution > MAX_RESOLUTION)
        return;

    lpSync->wResolution = resolution;
    lpSync->nTicksPerClock = resolution/24;
    lpSync->dwTRtime = resolution * lpSync->wTimerPeriod * 256000L;
}

//-----------------------------------------------------------------------------
// GetTempo
//
// Gets the current tempo.  The tempo is set in microseconds
// per midi beat so that tempo may be set fractionally.
//-----------------------------------------------------------------------------
DWORD WINAPI _export GetTempo16(HSYNC hSync)
{
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return 0L;
    else
        return lpSync->dwTempo / SCALE;
}

//-----------------------------------------------------------------------------
// GetResolution
//
// Gets the resolution in ticks per midi beat.  Higher values
// of resolution provide greater timing accuracy.
//-----------------------------------------------------------------------------
WORD WINAPI _export GetResolution16(HSYNC hSync)
{
    LPSYNC lpSync = (LPSYNC)hSync;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return 0;
    else
        return lpSync->wResolution;
}

//-----------------------------------------------------------------------------
//  GetPosition
//
//  Returns the current playback position in either milliseconds since the
//  last time StartSync() was called (by specifying POS_MS for units) or in
//  elapsed ticks since the last time StartSync() was called (by specifying
//  POS_TICKS for units).
//-----------------------------------------------------------------------------
DWORD WINAPI _export GetPosition16(HSYNC hSync, WORD units)
{
    LPSYNC lpSync = (LPSYNC)hSync;
    DWORD rc;

    // if not open, ignore request
    if(hSync < MXMIDIERR_MAXERR)
        return 0L;

    switch(units)
    {
        case POS_MS:
            rc = lpSync->msPosition;
            break;

        case POS_TICKS:
            rc = lpSync->dwTicks;
            break;

        default:
            rc = 0;
            break;




Page 335

    }

    return rc;
}

//-----------------------------------------------------------------------------
// sync handler
//
// This function calculates the number of ticks since last message sent,
// based on the current sync mode.  If it is time, it sends the next midi
// message.
//
//   The algorithm used for timing is outlined below:
//
// given:
//   resolution in ticks/beat, tempo in uS/beat, and thistime in
//      nS/interrupt
// and
//   time in nS, corresponding to accumulated fractions of a tick
//
// then,
//   nticks = ((resolution * time) + (resolution * thistime)) / tempo
// and,
//   fticks = (thistime * resolution) - (nticks * tempo)
//
// since:
//   (resolution * thistime) is known in advance it is calculated
//   outside of this routine and stored in lpSync->dwTRtime.  It need
//   only be changed if the sync mode or resolution are changed.
//
// therefore:
//  nticks = (fticks + trtime)/tempo
//   fticks += trtime - nticks*tempo
//   elasped ticks += nticks
//-----------------------------------------------------------------------------
void sync(LPSYNC lpSync)
{
    LPMIDIEVENT thisEvent;  // pointer to current event to (possibly) send
    DWORD FAR* lpE;         // pointer to event structure as dwords
    DWORD dwMsg;                // outgoing packed midi message
    WORD nticks;                // number of ticks elasped since last call
    LPMIDIOUT FAR* lpMOL;       // pointer to item in midiout list
    LPMIDIOUT lpMO;         // pointer to this midi out
    WORD nclocks;           // number of midi clocks to send
    WORD nc;                    // midi clock sent counter
    DWORD newTempo;         // tempo value for tempo changes
    BOOL fDone;             // true if no messages are left in queue

    // if sync enabled, calculate number of elasped ticks since last call
    if((lpSync->wFlags & SYNC_RUNNING) != SYNC_RUNNING)
        return;

    lpSync->wFlags |= IN_SYNC;

    // add time to the position accumulator for millisecond postion
    lpSync->msPosition += lpSync->wTimerPeriod;

    // if sync is Midi clock, ticks may be handled differently
    // if sync is being re-synchronized to a clock boundary or
    // the tick count is being held at nTicksPerClock-1
    if(lpSync->wSyncMode == S_MIDI)
    {
        // only interpolate up to the tick before the next clock
        // but keep counting new ticks for later tempo calculation
        if(lpSync->wFlags & MC_HOLD)
        {
            nticks = (WORD)((lpSync->dwFticks + lpSync->dwTRtime) / lpSync->dwTempo);
            lpSync->dwFticks += lpSync->dwTRtime - ((DWORD)nticks * lpSync->dwTempo);
            lpSync->wTempoTicks += nticks;
            lpSync->wFlags &= ~IN_SYNC;
            return;
        }

        // re-synchronize midi clock ticks
        if(lpSync->wFlags & MC_RESYNC)
        {
            // clear fractional ticks to re-synchronize tick interpolation
            lpSync->dwFticks = 0L;

            // calculate any extra ticks needed




Page 336

            nticks = (WORD)(lpSync->nTicksPerClock -
                    (lpSync->dwTicks - lpSync->dwLastTicks));

            // save new last ticks count
            lpSync->dwLastTicks = lpSync->dwTicks + nticks;

            // done re-syncing
            lpSync->wFlags &= ~MC_RESYNC;
        }
        else
        {
            // interpolate ticks between midi clocks
            nticks = (WORD)((lpSync->dwFticks + lpSync->dwTRtime) / lpSync->dwTempo);
            lpSync->dwFticks += lpSync->dwTRtime - ((DWORD)nticks * lpSync->dwTempo);

            // add ticks to value used to calculate tempo
            lpSync->wTempoTicks += nticks;

            // the number of elaspsed ticks cannot be greater than the number
            // of ticks per clock - 1 until the next midi clock arrives (and
            // the ticks get re-synced.)  Hold the number of ticks, if necessary.
            if((lpSync->dwTicks - lpSync->dwLastTicks + nticks) >=
                    lpSync->nTicksPerClock)
            {
                nticks = (WORD)(lpSync->nTicksPerClock - 1 -
                         (lpSync->dwTicks - lpSync->dwLastTicks));
                lpSync->wFlags |= MC_HOLD;
            }
        }
    }
    else
    {
        // calculate the number of new ticks
        nticks = (WORD)((lpSync->dwFticks + lpSync->dwTRtime) / lpSync->dwTempo);
        lpSync->dwFticks += lpSync->dwTRtime - ((DWORD)nticks * lpSync->dwTempo);
        lpSync->nTicksSinceClock += nticks;
    }

    // calculate final tick counts
    lpSync->dwTicks += (DWORD)nticks;
    lpSync->nTicksSinceBeat += nticks;

    // time to send a beat message to the app?
    if(lpSync->nTicksSinceBeat >= lpSync->wResolution)
    {
        PostMessage(lpSync->hWnd, MIDI_BEAT, 0, (DWORD)lpSync);
        lpSync->nTicksSinceBeat -= lpSync->wResolution;
    }

    // calculate how many clocks need to be sent, if any
    if(lpSync->wSyncMode != S_MIDI)
    {
        nclocks = 0;
        while(lpSync->nTicksSinceClock >= lpSync->nTicksPerClock)
        {
            nclocks++;
            lpSync->nTicksSinceClock -= lpSync->nTicksPerClock;
        }
    }

    // time to output midi event?
    // search through all attached midi out devices for any that
    // have messages waiting to go out
    lpMOL = lpSync->lpMidiOutList;

    // assume that nothing is waiting to be sent
    fDone = TRUE;

    while(*lpMOL != NULL)
    {
        lpMO = ((LPMIDIOUT)(*lpMOL));

        // time to send midi clock(s)?
        nc = nclocks;
        if((lpMO->dwFlags & SYNC_OUTPUT) && (lpSync->wSyncMode != S_MIDI))
            while(nc > 0)
            {
                midiOutShortMsg(lpMO->hMidiOut, (DWORD)MIDI_CLOCK);
                nc--;
            }

        thisEvent = (LPMIDIEVENT)(lpMO->pMidiOutDataOut + lpMO->lpMidiOutDataHead);




Page 337

        // any messages to send?
        if(lpMO->wSpan != 0)
        {
            // there is at least one message to be sent
            fDone = FALSE;

            // if this output is currently sending a sysex make no
            // attempt to send any short data, to prevent short messages
            // and long message from being sent out of order
            // last event time is on per midi output basis
            while((thisEvent->time <= lpSync->dwTicks - lpMO->dwLastEventTicks) &&
                 (lpMO->wSpan != 0) && (((lpMO->dwFlags & SENDING_SYSEX) == 0) ||
                 (thisEvent->status == SYSEX)))
            {
                // time to send this one
                lpMO->dwLastEventTicks += thisEvent->time;

                // is this a tempo change event?
                if(thisEvent->status == 0)
                {
                    newTempo = MAKELONG((thisEvent->data2 << 8) + thisEvent->data3,
                                 thisEvent->data1);
                    if(newTempo != 0L)
                        lpSync->dwTempo = newTempo * SCALE;
                }
                // no, it's a MIDI event
                else
                {
                    // if this is a sysex message, insert it into a sysex buffer
                    // where it will eventually be sent out.
                    if(thisEvent->status == SYSEX)
                        InsertInSysexBuffer(lpMO, thisEvent);
                    else
                    {
                        // it's a short midi message, build the message to send
                        lpE = (DWORD FAR*)thisEvent;
                        dwMsg = (DWORD)*(lpE + 1);

                        midiOutShortMsg(lpMO->hMidiOut, dwMsg);
                        TrackMidiOut(lpMO, dwMsg);
                    }
                }

                // increment the out pointer, wrapping if necessary
                if(lpMO->pMidiOutDataOut++ == lpMO->nMidiOutSize)
                    lpMO->pMidiOutDataOut = 0;

                // if number of events left in the buffer
                // has fallen below the threshold, send
                // the OUTBUFFER_READY message to the app
                // requesting more data.
                // The threshold is 25% of the buffer size
                if(lpMO->wSpan == (lpMO->nMidiOutSize >> 2))
                    PostMessage(lpMO->hWnd, OUTBUFFER_READY, 0, (DWORD)lpMO);

                // one less message in the buffer
                lpMO->wSpan--;

                // reset thisEvent to the next (possible) event and look again
                thisEvent = (LPMIDIEVENT)(lpMO->pMidiOutDataOut +
                            lpMO->lpMidiOutDataHead);
            }
        }

        // next device in list -- last one will be null
        lpMOL++;
    }

    // were there any messages in the queue or is sysex still busy?
    // send only one message
    if(fDone && (lpSync->nSysexBuffsActive == 0) &&
            ((lpSync->wFlags & SENT_SYNCDONE) == 0))
    {
        // send a SYNC_DONE to the app
        PostMessage(lpSync->hWnd, SYNC_DONE, 0, (DWORD)lpSync);
        lpSync->wFlags |= SENT_SYNCDONE;
    }

    lpSync->wFlags &= ~IN_SYNC;
}





Page 338

//-----------------------------------------------------------------------------
// syncTimer callback function
//
// This callback processes the periodic timer events for internal sync
// and serves as a timebase for midi sync.
//
// Parameter dwUser is lpSync.  Other parameters are unused.
//-----------------------------------------------------------------------------
void CALLBACK _export syncTimer(UINT wTimerID, UINT wMsg, DWORD dwUser,
        DWORD dw1, DWORD dw2)
{
    // don't call sync if already servicing a midi clock event
    if((((LPSYNC)dwUser)->wFlags & IN_SYNC) == 0)
        sync((LPSYNC)dwUser);
}

//-----------------------------------------------------------------------------
// midi sync handler
//
// This function is called when the sync in device receives a midi clock
// message.  It calculates a new tempo based on the number of ticks since
// the last clock.
//-----------------------------------------------------------------------------
void MidiClock(LPSYNC lpSync)
{
    // next sync() will be a re-sync
    lpSync->wFlags |= MC_RESYNC;

    // we are no longer holding the count
    lpSync->wFlags &= ~MC_HOLD;

    // generate these ticks
    sync(lpSync);

    // calculate new tempo
    lpSync->dwTempo -= ((long)lpSync->nTicksPerClock - (long)lpSync->wTempoTicks) *
             (lpSync->dwTempo / (long)lpSync->wResolution);

    // clear tempo tick count
    lpSync->wTempoTicks = 0;