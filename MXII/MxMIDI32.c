//-----------------------------------------------------------------------------
// Maximum MIDI Programmer's ToolKit - MxMidi32.DLL
// MaxMidi Flat Thunk Module
//
// Copyright (c) Paul A. Messick, 1994-1996
//




Page 339

// Written by Paul A. Messick
//
// Thunks between 32-bit applications and the 16-bit MaxMidi DLL.  Also provides
// 32-bit SMF read and write functions.
//-----------------------------------------------------------------------------
#include <windows.h>
#include "MxDLL.h"

__declspec(dllexport) BOOL WINAPI MxMidi_ThunkConnect32(LPSTR pszDll16, LPSTR 
pszDll32, HINSTANCE hInst, DWORD dwReason);

//============================================================================
//  Internal Functions
//============================================================================
//----------------------------------------------------------------------------
//  DllMain
//
//  This is the 32-bit equivalent to "LibMain".  It connects with the 16-bit
//  DLL through the thunk layer.
//----------------------------------------------------------------------------
__declspec(dllexport) BOOL WINAPI DllMain(HINSTANCE hDLL, DWORD dwReason,
        LPVOID lpReserved)
{
    // call the entry point for the thunk layer
    if(!(MxMidi_ThunkConnect32("MXMIDI16.DLL", "MXMIDI32.DLL", hDLL, dwReason)))
        return FALSE;
    
    switch(dwReason)
    {
        // DLL is attaching to the address space of the current process.
        case DLL_PROCESS_ATTACH:
            break;

        // The calling process is detaching the DLL from its address space.
        case DLL_PROCESS_DETACH:
            break;

        // A new thread is being created in the current process.
        case DLL_THREAD_ATTACH:
            break;

        // A thread is exiting cleanly.
        case DLL_THREAD_DETACH:
            break;
    }

    return TRUE;
}

//----------------------------------------------------------------------------
//  GetMaxMidiVersion
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI GetMaxMidiVersion(void)
{
    return GetMaxMidiVersion16();
}

//----------------------------------------------------------------------------
//  GetNumInDevices
//
//  For convenience: use in place of midiInGetNumDevs to avoid dealing with
//  MMSYSTEM directly.
//----------------------------------------------------------------------------
_declspec(dllexport) UINT WINAPI GetNumInDevices(void)
{
    return midiInGetNumDevs();
}

//----------------------------------------------------------------------------
//  GetNumOutDevices
//
//  For convenience: use in place of midiOutGetNumDevs to avoid dealing with
//  MMSYSTEM directly.
//----------------------------------------------------------------------------
_declspec(dllexport) UINT WINAPI GetNumOutDevices(void)
{
    return midiOutGetNumDevs();
}

//----------------------------------------------------------------------------
//  GetMidiOutDescription




Page 340

//----------------------------------------------------------------------------
__declspec(dllexport) BOOL WINAPI GetMidiOutDescription(WORD wDeviceID,
        LPSTR lpzDesc)
{
    return GetMidiOutDescription16(wDeviceID, lpzDesc);
}

//----------------------------------------------------------------------------
//  OpenMidiOut
//----------------------------------------------------------------------------
__declspec(dllexport) HMOUT WINAPI OpenMidiOut(HWND hWnd, WORD wDeviceID,
        HSYNC hSync, DWORD dwFlags)
{
    return OpenMidiOut16(hWnd, wDeviceID, hSync, dwFlags);
}

//----------------------------------------------------------------------------
//  ResetMidiOut
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI ResetMidiOut(HMOUT hMidiOut)
{
    return ResetMidiOut16(hMidiOut);
}

//----------------------------------------------------------------------------
//  FlushMidiout
//----------------------------------------------------------------------------
__declspec(dllexport) void WINAPI FlushMidiOut(HMOUT hMidiOut)
{
    FlushMidiOut16(hMidiOut);
}

//----------------------------------------------------------------------------
//  CloseMidiOut
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI CloseMidiOut(HMOUT hMidiOut)
{
    return CloseMidiOut16(hMidiOut);
}

//----------------------------------------------------------------------------
//  PutMidiOut
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI PutMidiOut(HMOUT hMidiOut, LPMIDIEVENT lpMidiEvent)
{
    return PutMidiOut16(hMidiOut, lpMidiEvent);
}

//----------------------------------------------------------------------------
// GetMidiInDescription
//----------------------------------------------------------------------------
__declspec(dllexport) BOOL WINAPI GetMidiInDescription(WORD wDeviceID,
        LPSTR lpzDesc)
{
    return GetMidiInDescription16(wDeviceID, lpzDesc);
}

//----------------------------------------------------------------------------
//  OpenMidiIn
//----------------------------------------------------------------------------
__declspec(dllexport) HMIN WINAPI OpenMidiIn(HWND hWnd, WORD wDeviceID,
        HSYNC hSync, DWORD dwFlags)
{
    return OpenMidiIn16(hWnd, wDeviceID, hSync, dwFlags);
}

//----------------------------------------------------------------------------
//  StartMidiIn
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI StartMidiIn(HMIN hMidiIn)
{
    return StartMidiIn16(hMidiIn);
}

//----------------------------------------------------------------------------
//  StopMidiIn
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI StopMidiIn(HMIN hMidiIn)
{
    return StopMidiIn16(hMidiIn);
}




Page 341


//----------------------------------------------------------------------------
//  CloseMidiIn
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI CloseMidiIn(HMIN hMidiIn)
{
    return CloseMidiIn16(hMidiIn);
}

//----------------------------------------------------------------------------
//  GetMidiIn
//----------------------------------------------------------------------------
__declspec(dllexport) LPMIDIEVENT WINAPI GetMidiIn(HMIN hMidiIn)
{
    return GetMidiIn16(hMidiIn);
}

//----------------------------------------------------------------------------
//  OpenSync
//----------------------------------------------------------------------------
__declspec(dllexport) HSYNC WINAPI OpenSync(HSYNC hSync, HWND hWnd, WORD mode,
        WORD timerPeriod)
{
    return OpenSync16(hSync, hWnd, mode, timerPeriod);
}

//----------------------------------------------------------------------------
//  CloseSync
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI CloseSync(HSYNC hSync)
{
    return CloseSync16(hSync);
}

//----------------------------------------------------------------------------
//  StopSync
//----------------------------------------------------------------------------
__declspec(dllexport) void WINAPI StopSync(HSYNC hSync)
{
    StopSync16(hSync);
}

//----------------------------------------------------------------------------
//  StartSync
//----------------------------------------------------------------------------
__declspec(dllexport) void WINAPI StartSync(HSYNC hSync)
{
    StartSync16(hSync);
}

//----------------------------------------------------------------------------
//  PauseSync
//----------------------------------------------------------------------------
__declspec(dllexport) void WINAPI PauseSync(HSYNC hSync, BOOL reset)
{
    PauseSync16(hSync, reset);
}

//----------------------------------------------------------------------------
//  ReStartSync
//----------------------------------------------------------------------------
__declspec(dllexport) void WINAPI ReStartSync(HSYNC hSync)
{
    ReStartSync16(hSync);
}

//----------------------------------------------------------------------------
//  SetTempo
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI SetTempo(HSYNC hSync, DWORD uSPerBeat)
{
    return SetTempo16(hSync, uSPerBeat);
}

//----------------------------------------------------------------------------
//  SetResolution
//----------------------------------------------------------------------------
__declspec(dllexport) void WINAPI SetResolution(HSYNC hSync, WORD resolution)
{
    SetResolution16(hSync, resolution);
}




Page 342


//----------------------------------------------------------------------------
//  GetTempo
//----------------------------------------------------------------------------
__declspec(dllexport) DWORD WINAPI GetTempo(HSYNC hSync)
{
    return GetTempo16(hSync);
}

//----------------------------------------------------------------------------
//  GetResolution
//----------------------------------------------------------------------------
__declspec(dllexport) WORD WINAPI GetResolution(HSYNC hSync)
{
    return GetResolution16(hSync);
}

//----------------------------------------------------------------------------
//  GetPosition
//----------------------------------------------------------------------------
__declspec(dllexport) DWORD WINAPI GetPosition(HSYNC hSync, WORD units)
{
    return GetPosition16(hSync, units);
}