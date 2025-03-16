//------------------------------------------------------------------------
// MaxMidi MIDI Programmer's ToolKit
// Standard Midi File Module
//
// Copyright (c) Paul A. Messick, 1994-1996
//
// Written by Paul A. Messick
//
// Provides functions to open, close, read and write Standard Midi Files
// of Format 0 and 1.
//------------------------------------------------------------------------
#include <windows.h>
#include <string.h>
#include "MxDLL.h"

//------------------------------------------------------------------------
// Function Prototypes used in this DLL module
//------------------------------------------------------------------------
#define CurFilePos(lpsmf) SetFilePointer(lpsmf->hsmf, 0, NULL, FILE_CURRENT)
#define SeekTo(lpsmf, ofs) SetFilePointer(lpsmf->hsmf, ofs, NULL, FILE_BEGIN)
#define GetMsgLength(b)  (b < 0xF0 ? CMsgLen[(b >> 4) - 8] : SMsgLen[b & 0x0F]);

int ReadSMFHeader(LPSMF lpSMF);
int OpenChunk(LPSMF lpSMF, LPSTR ChunkName, DWORD *offset, DWORD *len);
int ReadChunk(HFILE hsmf, DWORD len, LPSTR lpBuffer);
void SeekAbsolute(LPSMF lpSMF, DWORD offset);
void SeekRelative(LPSMF lpSMF, DWORD offset);
DWORD ReadVarLength(LPSMF lpSMF); 
BOOL ReadMidiEvent(LPSMF lpSMF, LPMIDIEVENT lpOutBuf);
DWORD ReadNextMeta(LPSMF lpSMF, LPTRACK lpCurTrack, BYTE MetaEvent, DWORD *time);
BYTE ReadByte(LPSMF lpSMF);

void WriteChunkName(LPSMF lpSMF, LPSTR ChunkName, DWORD offset, DWORD len);
void WriteSMFHeader(LPSMF lpSMF);
DWORD MidiToSMF(LPSMF lpSMF, LPMIDIEVENT lpMidiEvent);
DWORD WriteVarLength(LPSMF lpSMF, DWORD value);
void isNewTrack(LPSMF lpSMF, int wTrack);
void WriteReversedData(LPSMF lpSMF, LPVOID lpData, int wSize);
void WriteByte(LPSMF lpSMF, BYTE val);

//------------------------------------------------------------------------
// Local Data
//------------------------------------------------------------------------
// midi message length arrays
static int CMsgLen[] = { 2, 2, 2, 2, 1, 1, 2 };
static int SMsgLen[] = { 0, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

// time in uS/Frame for each frame rate
DWORD usPerFrame[7] = {33333, 33367, 0, 0, 0, 40000, 41667};

//------------------------------------------------------------------------
// OpenSMF




Page 343

//
// This function opens an SMF for read or write.
// hSMF is the handle to the opened SMF, though not an actual file handle.
// 
// filename is an asciiz string, including optional path, for the SMF
// to be opened.
//
// *Format is the address of a UINT that contains the SMF format for the
// opened file.  If the file is opened for read, Format is set to the
// format read from the file.  Format 2 files are read as Format 1 files.
// If the file is opened for write, Format must be set to the desired
// value.
//
// mode is a constant character that determines the mode in which the
// file is opened.  mode can be either 'r' for read, or 'w' for write.
//
// OpenSMF returns the number of tracks in the SMF if opened for read,
// zero if the file is successfully opened for write, or -1 if there is
// an error.
//------------------------------------------------------------------------
PREFIX HSMF WINAPI EXPORT OpenSMF(LPSTR filename, int *Format, const char mode,
        int *nTracks)
{
    LPSMF lpS;          // local pointer to SMF struct
    int track;          // track counter

    // attempt to allocate memory for this SMF structure
    if((lpS = (LPSMF)GlobalLock(GlobalAlloc(GHND, sizeof(SMFStruct)))) == NULL)
        return 0;

    // check for a valid mode
    if((mode != 'r') && (mode != 'w'))
        return 0;

    // setup the mode to open the file
    lpS->mode = mode;

    // Open the file in the requested mode
    lpS->hsmf = CreateFile(filename, 
                            (mode == 'r' ? GENERIC_READ : GENERIC_WRITE),
                            FILE_SHARE_READ,(LPSECURITY_ATTRIBUTES)NULL, 
                            (mode == 'r' ? OPEN_EXISTING : CREATE_ALWAYS),
                            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS, 
                            (HANDLE)NULL);

    // was the open ok?
    if(lpS->hsmf == INVALID_HANDLE_VALUE)
    {
        // on error, unwind the memory allocations
        GlobalFree(lpS);
        return 0;
    }

    if(lpS->mode == 'r')
    {
        // read the smf header
        if(ReadSMFHeader(lpS) == -1)
        {
            // on error, unwind the memory allocations
            GlobalFree(lpS);
            return 0;
        }

        // if format is 2, force it to one
        if(lpS->wFormat == 2)
            lpS->wFormat = 1;

        *Format = lpS->wFormat;

        // allocate an array of structures to manage each track
        if((lpS->lpTrack = (LPTRACK)GlobalLock(GlobalAlloc(GHND, lpS->wTracks *
                sizeof(TrackStruct)))) == NULL)
        {
            // on error, unwind the memory allocations
            GlobalFree(lpS);
            return 0;
        }

        // allocate a buffer for each track that is used to 
        // read track data from the file
        for(track = 0; track < lpS->wTracks; track++)




Page 344

        {
            lpS->lpTrack[track].dwRBsize = 8192;
            if((lpS->lpTrack[track].lpReadBuf = (LPSTR)GlobalAlloc(GPTR,
                    lpS->lpTrack[track].dwRBsize)) == NULL)
            {
                // on error, unwind the memory allocations
                for(track = 0; track < lpS->wTracks; track++)
                    GlobalFree(lpS->lpTrack[track].lpReadBuf);

                GlobalFree(lpS->lpTrack);
                GlobalFree(lpS);
                return 0;
            }

            lpS->lpTrack[track].dwBytesRemaining = 0;
        }

        // allocate buffers for each track to manage meta event
        // offsets and elapsed time in the file (used for ReadMetaEvent())
        for(track = 0; track < lpS->wTracks; track++)
        {
            if((lpS->lpTrack[track].lpMetaOfs = (LPDWORD)GlobalAlloc(GPTR,
                    MAX_META_EVENT * sizeof(DWORD))) == NULL)
            {
                // on error, unwind the memory allocations
                for(track = 0; track < lpS->wTracks; track++)
                {
                    GlobalFree(lpS->lpTrack[track].lpMetaOfs);
                    GlobalFree(lpS->lpTrack[track].lpReadBuf);
                }

                GlobalFree(lpS->lpTrack);
                GlobalFree(lpS);
                return 0;
            }

            // this buffer holds elapsed time in ticks from the start
            // of the track for each meta event
            if((lpS->lpTrack[track].lpMetaTime = (LPDWORD)GlobalAlloc(GPTR,
                    MAX_META_EVENT * sizeof(DWORD))) == NULL)
            {
                // on error, unwind the memory allocations
                for(track = 0; track < lpS->wTracks; track++)
                {
                    GlobalFree(lpS->lpTrack[track].lpMetaOfs);
                    GlobalFree(lpS->lpTrack[track].lpReadBuf);
                }

                GlobalFree(lpS->lpTrack);
                GlobalFree(lpS);
                return 0;
            }
        }

        // allocate a one byte long buffer for reading Meta Events
        // this buffer will be reallocated as needed
        for(track = 0; track < lpS->wTracks; track++)
            lpS->lpMetaBuff = (LPSTR)GlobalAlloc(GPTR, 1);

        // find the beginning offsets for each track by rewinding the file
        RewindSMF((HSMF)lpS);  

        // smf successfully opened, return number of tracks
        *nTracks = lpS->wTracks;
        return (HSMF)lpS;
    }

    // else it is opened for write
    // setup the format and division values
    lpS->wFormat = *Format;
    lpS->resolution = 480;

    // initially, no tracks have been written to
    lpS->wCurTrack = -1;

    // write the header - this will be overwritten later
    WriteSMFHeader(lpS);

    // return zero tracks for write
    *nTracks = lpS->wTracks = 0;
    return (HSMF)lpS;
}





Page 345

//------------------------------------------------------------------------
// CloseSMF
//
// This function closes a previously opened SMF, frees the SMFStruct
// memory and sets the lpSMF pointer to NULL to prevent further attempts
// to accesses the file.
//------------------------------------------------------------------------
PREFIX void WINAPI EXPORT CloseSMF(HSMF hSMF)
{
    int track;              // track counter
    LPSMF lpSMF = (LPSMF)hSMF;

    // don't try to close a null device
    if(lpSMF == NULL)
        return;
                                   
    // free the track structures and buffers
    if(lpSMF->mode == 'r')
    {
        for(track = 0; track < lpSMF->wTracks; track++)
        {
            GlobalFree(lpSMF->lpTrack[track].lpMetaTime);

            GlobalFree(lpSMF->lpTrack[track].lpMetaOfs);
            GlobalFree(lpSMF->lpTrack[track].lpReadBuf);
            GlobalFree(lpSMF->lpMetaBuff);
        }

        GlobalFree(lpSMF->lpTrack);
    }
    else
        if(lpSMF->dwChunkStart != 0) // make sure a track was started
    {
        // write the end of track meta event
        WriteMetaEvent(hSMF, lpSMF->wCurTrack, META_EOT, NULL, 0);

        // seek to the start of the track chunk and
        // update the chunk with the correct length
        WriteChunkName(lpSMF, "MTrk", lpSMF->dwChunkStart, lpSMF->dwChunkLen);

        // update the header with number of tracks, format, etc.
        WriteSMFHeader(lpSMF);
    }

    // close the file
    if(lpSMF->hsmf != 0)
        CloseHandle(lpSMF->hsmf);

    // free the smf structure
    GlobalFree(lpSMF);
}

//------------------------------------------------------------------------
// RewindSMF
//
// If the SMF was opened for read this function rewinds the tracks of
// the SMF to the beginning and returns TRUE.  If the file was opened for
// write, no action is taken and the function returns FALSE.
//------------------------------------------------------------------------
PREFIX BOOL WINAPI EXPORT RewindSMF(HSMF hSMF)
{
    DWORD dwTrkOfs;     // offset into file for this track
    int track;          // track counter
    DWORD dwLen;            // length of track in bytes
    int meta;           // meta event loop counter
    LPSMF lpSMF = (LPSMF)hSMF;

    // if the file is opened for write, nothing to rewind
    if(lpSMF->mode == 'w')
        return FALSE;

    // start at the beginning of the file to rewind
    dwTrkOfs = 0;

    // locate and save file offset and length for each track
    for(track = 0; track < lpSMF->wTracks; track++)
    {
        OpenChunk(lpSMF, "MTrk", &dwTrkOfs, &dwLen);
        lpSMF->lpTrack[track].dwFileOfs = dwTrkOfs;
        lpSMF->lpTrack[track].dwBuffOfs = 0;
        lpSMF->lpTrack[track].dwLength = dwLen;




Page 346

        lpSMF->lpTrack[track].fEndOfTrack = FALSE;

        // all meta event offsets are at start of track and elapsed times are zero
        for(meta = 0; meta < MAX_META_EVENT; meta++)
        {
            lpSMF->lpTrack[track].lpMetaOfs[meta] = dwTrkOfs;
            lpSMF->lpTrack[track].lpMetaTime[meta] = 0;
        }

        // not in a SYSEX
        lpSMF->lpTrack[track].wFlags &= ~IN_SYSEX;

        // clear the meta event delta time for this track
        lpSMF->lpTrack[track].dwMetaDeltaTime = 0;
    }

    // seek to the first track's data
    lpSMF->wCurTrack = 0;
    SeekAbsolute(lpSMF, lpSMF->lpTrack[0].dwFileOfs);

    return TRUE;
}

//------------------------------------------------------------------------
// Standard Midi File Read Functions
//------------------------------------------------------------------------
//------------------------------------------------------------------------
// ReadSMF
//
// This function reads a block of events from the previously opened SMF.
//------------------------------------------------------------------------
PREFIX DWORD WINAPI EXPORT ReadSMF(HSMF hSMF, int wTrack, LPMIDIEVENT 
lpMidiEventBuffer, DWORD dwBufferLen)
{
    DWORD dwNumEvents = 0;      // number of events read
    LPSMF lpSMF = (LPSMF)hSMF;

    // if the file is opened for write, nothing to read
    if(lpSMF->mode == 'w')
        return 0;

    // save which track we are reading
    lpSMF->wCurTrack = wTrack;

    // seek to the proper location for this track
    SeekAbsolute(lpSMF, lpSMF->lpTrack[wTrack].dwFileOfs);

    // read the track, filling the lpMidiEventBuffer with events
    while((dwNumEvents < dwBufferLen) && (ReadMidiEvent(lpSMF, lpMidiEventBuffer)
            != TRUE))
    {
        // another event read
        lpMidiEventBuffer++;
        dwNumEvents++;
    }

    return dwNumEvents;
}

//------------------------------------------------------------------------
// GetSMFResolution
//
// This function returns the timing resolution for the SMF in ticks/beat.
//------------------------------------------------------------------------
PREFIX WORD WINAPI EXPORT GetSMFResolution(HSMF hSMF)
{
    LPSMF lpSMF = (LPSMF)hSMF;

    if(lpSMF->mode == 'r')
        return lpSMF->resolution;

    // not valid for write mode
    return 0;
}

//------------------------------------------------------------------------
// SetSMFResolution
//
// This function sets the timing resolution for the SMF in ticks/beat.
// It returns 0 if the file was opened for read, or the resolution if
// opened for write.
//------------------------------------------------------------------------
PREFIX WORD WINAPI EXPORT SetSMFResolution(HSMF hSMF, WORD resolution)




Page 347

{
    LPSMF lpSMF = (LPSMF)hSMF;

    if(lpSMF->mode == 'w')
    {
        lpSMF->resolution = resolution;
        return resolution;
    }

    return 0;
}

//------------------------------------------------------------------------
// ReadMetaEvent
//
// This function reads a Meta Event from the selected track.
// It returns the time of the event, if found, or -1 if not.  The value
// is in a string of length *EventSize pointed to by *EventValue.
// If the event is not found in the track, further attempts to find the
// same event will also return not found.  *EventSize is set to zero and
// *EventValue is set to NULL if the event is not found.
//------------------------------------------------------------------------
PREFIX DWORD WINAPI EXPORT ReadMetaEvent(HSMF hSMF, int wTrack, BYTE MetaEvent,
        LPSTR* EventValue, DWORD* EventSize)
{
    LPSMF lpSMF = (LPSMF)hSMF;
    DWORD dwOldFilePos;     // starting file position
    LPTRACK lpCurTrack;     // current track to read
    DWORD time;             // event time
    DWORD dwEvents;         // event counter
    LPSTR lpDestBuff;           // destination buffer pointer

    // save the current file offset 
    dwOldFilePos = lpSMF->dwCurFileOfs;

    // which track are we reading?
    lpCurTrack = (LPTRACK)&(lpSMF->lpTrack[wTrack]);

    // if this track contains no more of these meta events, return
    if(lpCurTrack->lpMetaOfs[MetaEvent] == (DWORD)-1)
        return (DWORD)-1;

    // seek to the location of the end of the last one of these
    // meta events
    SeekAbsolute(lpSMF, lpCurTrack->lpMetaOfs[MetaEvent]);

    // look for the next one
    *EventSize = ReadNextMeta(lpSMF, lpCurTrack, MetaEvent, &time);

    // if one was found, return it to app
    if(*EventSize != (DWORD)-1)
    {
        // update the location of next meta event
        lpCurTrack->lpMetaOfs[MetaEvent] = lpSMF->dwCurFileOfs + *EventSize;

        // if the event length is zero, just return pointing to an empty string
        if(*EventSize == 0)
        {
            *EventValue = NULL;

            // restore original file position
            SeekAbsolute(lpSMF, dwOldFilePos); 
            return time;
        }

        // else re-allocate the internal buffer for the new event
        lpSMF->lpMetaBuff = (LPSTR)GlobalReAlloc(lpSMF->lpMetaBuff, *EventSize, GPTR | 
                                                GMEM_MOVEABLE);
        
        if(lpSMF->lpMetaBuff == NULL)
        {
            // on error, return not found
            // restore original file position
            SeekAbsolute(lpSMF, dwOldFilePos); 
            return (DWORD)-1;
        }

        // read the event data into the application buffer
        dwEvents = *EventSize;
        lpDestBuff = lpSMF->lpMetaBuff;

        while(dwEvents--)
            *(lpDestBuff++) = ReadByte(lpSMF);                




Page 348


        // return success, pointing to buffer
        *EventValue = lpSMF->lpMetaBuff;

        // restore original file position
        SeekAbsolute(lpSMF, dwOldFilePos);

        // return the elapsed time since the start of the track
        time += lpCurTrack->lpMetaTime[MetaEvent];
        lpCurTrack->lpMetaTime[MetaEvent] = time;
        return time;
    }

    // else, meta event not found
    // tag this as the last one of these meta events
    lpCurTrack->lpMetaOfs[MetaEvent] = (DWORD)-1;

    // restore original file position
    SeekAbsolute(lpSMF, dwOldFilePos);
    return (DWORD)-1;
}

//------------------------------------------------------------------------
// FindToken
//
// This function searches buffer s1 for the first occurrence of token s2,
// for up to len bytes.  It returns zero if the token is found, non-zero
// if token does not appear in s1.
// If s2 is found in s1, *offset is set to the count of bytes from the
// beginning of s1 to the first byte after the token found in s1.  If the
// token is not found, offset is undefined.
//------------------------------------------------------------------------
int FindToken(LPSTR s1, LPSTR s2, DWORD len, DWORD *offset)
{
    HPSTR sx;           // search string pointer
    HPSTR hpStart;      // start of buffer
    DWORD s2len;

    // setup pointers to the strings
    sx = (HPSTR)s1;
    hpStart = sx;
    s2len = lstrlen(s2);

    do {
        // search for first character in string 
        if((sx = (LPSTR)memchr((LPSTR)sx, s2[0], (size_t)len)) == NULL)
            return -1;

        // calculate remaining buffer length
        len -= (DWORD)(sx - (HPSTR)s1) + 1L;
        s1 = (LPSTR)sx;

        // find rest of token
    } while(strncmp((LPSTR)(sx++), s2, (size_t)s2len) != 0);

    *offset = (DWORD)(sx - hpStart) + s2len - 1L;

    return 0;
}

//------------------------------------------------------------------------
// OpenChunk
//
// This function opens the specified chunk, if found, and sets offset to
// the number of bytes from the start of the file where chunk data begins
// and sets len to the number of bytes of data in the chunk.
// It returns 0 if the chunk was found and read correctly, or -1 on error
//------------------------------------------------------------------------
int OpenChunk(LPSMF lpSMF, LPSTR ChunkName, DWORD *offset, DWORD *len)
{
    DWORD dwBufferSize;     // buffer size in bytes
    DWORD wBytesRead;           // number of bytes read from file
    DWORD ofs;              // offset into this block
    DWORD fptr;             // final file pointer offset
    LPSTR lpRawData;            // raw file data buffer
    DWORD n;

    // allocate a buffer to read file data into
    dwBufferSize = 8192;
    if((lpRawData = (LPSTR)GlobalAlloc(GPTR, dwBufferSize)) == NULL)
        return -1;

    // read a block of data




Page 349

    SeekTo(lpSMF, *offset);
    fptr = *offset;

    ReadFile(lpSMF->hsmf, lpRawData, dwBufferSize, &wBytesRead, NULL);
    while(wBytesRead != 0)
    {
        // look for occurrence of ChunkName in buffer
        if(FindToken(lpRawData, ChunkName, (DWORD)wBytesRead, &ofs) == 0)
        {
            // found it, now set the file pointer to the start of chunk data
            fptr += ofs;
            SeekTo(lpSMF, fptr);

            // read the chunk length
            ReadFile(lpSMF->hsmf, lpRawData, sizeof(DWORD), &n, NULL);
            if(n == 0)
            {
                // on error, unwind the memory allocations
                GlobalFree(lpRawData);
                return -1;
            }

            // offset is the start of chunk data, after len
            *offset = CurFilePos(lpSMF);

            // length is 4 bytes, msb first
            *len = MAKELONG((lpRawData[2] << 8) + lpRawData[3],
                    (lpRawData[0] << 8) + lpRawData[1]);

            // done with the buffer now
            GlobalFree(lpRawData);

            // return success
            return 0;
        }
        else
            fptr += wBytesRead;

        ReadFile(lpSMF->hsmf, lpRawData, dwBufferSize, &wBytesRead, NULL);
    }

    // done with the buffer now
    GlobalFree(lpRawData);

    // chunk never found, return error
    return -1;
}

//------------------------------------------------------------------------
// ReadSMFHeader
//
// This function reads the smf header information and stores the values
// in the lpSMF structure for later use.
// It returns 0 on success and -1 if the file is not a SMF or other error.
//------------------------------------------------------------------------
int ReadSMFHeader(LPSMF lpSMF)
{
    DWORD FileOffset = 0;       // offset into file for header
    DWORD length;           // length of header chunk
    UINT division;          // timing resolution
    LPSTR lpHeader;         // header buffer pointer
    DWORD dwBytesRead;
    DWORD sig = 0;

    // read the first DWORD of the file
    // if it is not "MThd", this is not a valid SMF
    ReadFile(lpSMF->hsmf, &sig, sizeof(DWORD), &dwBytesRead, NULL);
    if(sig != mmioFOURCC('M', 'T', 'h', 'd'))
        return -1;
    else    // rewind the file
        SeekTo(lpSMF, 0);

    // locate and open the chunk
    // offset to the start of chunk data is returned in FileOffset
    // length of chunk data is returned in length
    if(OpenChunk(lpSMF, "MThd", &FileOffset, &length) == -1)
        return -1;

    // allocate the header buffer
    if((lpHeader = (LPSTR)GlobalAlloc(GPTR, length)) == NULL)
        return -1;

    // read the data into the header buffer




Page 350

    ReadFile(lpSMF->hsmf, lpHeader, length, &dwBytesRead, NULL);
    if(dwBytesRead != length)
    {
        // on error, unwind the memory allocations
        GlobalFree(lpHeader);
        return -1;
    }

    // parse the buffer to get the three word parameters
    // format, ntrks, and division
    lpSMF->wFormat = (lpHeader[0] << 8) + (BYTE)lpHeader[1];
    lpSMF->wTracks = (lpHeader[2] << 8) + (BYTE)lpHeader[3];
    division = (lpHeader[4] << 8) + (BYTE)lpHeader[5];
    lpSMF->resolution = division;

    // free the header buffer
    GlobalFree(lpHeader);
    return 0;
}

//------------------------------------------------------------------------
// SeekAbsolute
//
// This function moves the read buffer pointer to the specified offset,
// reading new buffers as necessary.
//------------------------------------------------------------------------
void SeekAbsolute(LPSMF lpSMF, DWORD offset)
{
    LPTRACK lpCurTrack;     // current track structure
    DWORD dwRelOffset;      // relative offset

    // which track are we reading?
    lpCurTrack = (LPTRACK)&(lpSMF->lpTrack[lpSMF->wCurTrack]);

    // if offset is before the start of buffer or after remaining
    // bytes count (end of buffer) seek to new location and read a buffer full
    if((offset < lpCurTrack->dwFileOfs - lpCurTrack->dwBuffOfs) ||
       (offset > lpCurTrack->dwFileOfs + lpCurTrack->dwBytesRemaining) ||
       (lpSMF->dwCurFileOfs != lpCurTrack->dwFileOfs))
    {
        // seek to new location and read a buffer full 
        SeekTo(lpSMF, offset);

        // read the new block
        ReadFile(lpSMF->hsmf, lpCurTrack->lpReadBuf, lpCurTrack->dwRBsize,
                &lpCurTrack->dwBytesRemaining, NULL);

        // update the track offset and remaining length
        lpCurTrack->dwFileOfs = offset;
        lpSMF->dwCurFileOfs = offset;
        lpCurTrack->dwBuffOfs = 0;

        return;
    }

    // else offset is within current buffer, update pointers only
    dwRelOffset = offset - lpCurTrack->dwFileOfs;
    lpCurTrack->dwBytesRemaining -= dwRelOffset;
    lpCurTrack->dwBuffOfs += dwRelOffset;
    lpCurTrack->dwFileOfs += dwRelOffset;
    lpSMF->dwCurFileOfs += dwRelOffset;
}

//------------------------------------------------------------------------
// SeekRelative
//
// This function moves the read buffer pointer by the specified number
// of bytes, reading new buffers as necessary.
//------------------------------------------------------------------------
void SeekRelative(LPSMF lpSMF, DWORD offset)
{
    LPTRACK lpCurTrack;     // current track structure

    // if offset is zero, do nothing
    if(offset == 0)
        return;

    // which track are we reading?
    lpCurTrack = (LPTRACK)&(lpSMF->lpTrack[lpSMF->wCurTrack]);

    // convert to absolute address and seek
    SeekAbsolute(lpSMF, offset + lpCurTrack->dwFileOfs);




Page 351

}

//------------------------------------------------------------------------
// ReadByte
//
// This function returns a single byte of data read from the file.
// The data is read from the file in blocks, but is passed to the caller
// as single bytes.
//------------------------------------------------------------------------
BYTE ReadByte(LPSMF lpSMF)
{
    LPTRACK lpCurTrack;     // current track structure

    // which track are we reading?
    lpCurTrack = (LPTRACK)&(lpSMF->lpTrack[lpSMF->wCurTrack]);

    // anything in the buffer?
    if(lpCurTrack->dwBytesRemaining == 0)
    {
        // file offset is at start of buffer
        lpSMF->dwCurFileOfs = lpCurTrack->dwFileOfs;
        SeekTo(lpSMF, lpCurTrack->dwFileOfs);

        // read the new block
        ReadFile(lpSMF->hsmf, lpCurTrack->lpReadBuf, lpCurTrack->dwRBsize,
                &lpCurTrack->dwBytesRemaining, NULL);

        // update buffer offset
        lpCurTrack->dwBuffOfs = 0;

        if(lpCurTrack->dwBytesRemaining == 0)
            return 0;
    }

    // one less in the buffer
    lpCurTrack->dwBytesRemaining--;

    // get a byte from the buffer and return it to the caller
    lpCurTrack->dwFileOfs++;
    lpSMF->dwCurFileOfs++;
    return *(lpCurTrack->lpReadBuf + lpCurTrack->dwBuffOfs++);
}

//------------------------------------------------------------------------
// ReadVarLength
//
// This function reads the variable length value from the buffer.
// The value is returned as a DWORD and the buffer pointer is updated to
// point to the byte following the value.
//
// A variable-length value is made up of one to four bytes.  Each byte
// contains seven significant bits.  Bit eight serves as a continuation
// bit; if it is set then at least one more byte follows.  The bytes are
// stored most significant first.
//------------------------------------------------------------------------
DWORD ReadVarLength(LPSMF lpSMF)
{
    DWORD v = 0;          // result
    BYTE b;         // current byte from file

    // add up all of the byte values
    do {
        v = (v << 7) | ((b = ReadByte(lpSMF)) & 0x7F);
    } while(b & 0x80);

    return v;
}

//------------------------------------------------------------------------
// ReadMidiEvent
//
// This function reads the next Midi event from the file and places it
// in the next location in the lpOutBuffer.  Meta Events are
// skipped, except for META_TEMPO which is merged into the Midi Events
// as a tempo change event.
// Returns TRUE if end of track, FALSE otherwise.
//------------------------------------------------------------------------
BOOL ReadMidiEvent(LPSMF lpSMF, LPMIDIEVENT lpOutBuf)
{
    BYTE event;     // event byte read from file
    DWORD dwLen;         // length of event data
    UINT nDataBytes;     // count of remaining data bytes




Page 352


    // don't read anything if already at end of track
    if(lpSMF->lpTrack[lpSMF->wCurTrack].fEndOfTrack == TRUE)
        return TRUE;

     while(TRUE)        // read through file until an event is found
     {
        // don't read timestamp if reading sysex data block
        if(lpSMF->lpTrack[lpSMF->wCurTrack].dwSysexLen == 0)
        {
            // read the leading delta time value
            // add any accumulated time from skipped meta events
            lpOutBuf->time = ReadVarLength(lpSMF) +
                    lpSMF->lpTrack[lpSMF->wCurTrack].dwMetaDeltaTime;

            // now clear the delta time, since it has been added to the event time
            lpSMF->lpTrack[lpSMF->wCurTrack].dwMetaDeltaTime = 0;
        }

        // read the first event byte
        // process it according to the event type 
        // if it is a status (other than sysex or eox)
        //   read the following bytes
        //  if it is a data byte for a midi message
        //      read the following bytes - use running status
        //  if it is a 0xFF - its a meta event
        //      skip the entire event
        //  if it is a sysex or eox
        //      read a single byte and return as a sysex
        event = ReadByte(lpSMF);

        // is it a meta event?
        if(event == META)
        {
            // read meta event type
            event = ReadByte(lpSMF);

            // length of the meta event
            dwLen = ReadVarLength(lpSMF);

            // is it end of track?
            if(event == META_EOT)
            {
                // read and discard the dummy length byte
                ReadVarLength(lpSMF);

                // return EOT
                lpSMF->lpTrack[lpSMF->wCurTrack].fEndOfTrack = TRUE;
                return TRUE;
            }

            // is it a tempo change? 
            if(event == META_TEMPO)
            {
                // read the tempo value
                lpOutBuf->data1 = ReadByte(lpSMF);
                lpOutBuf->data2 = ReadByte(lpSMF);
                lpOutBuf->data3 = ReadByte(lpSMF);

                // status is zero on tempo change events
                lpOutBuf->status = 0;

                // we have read three, skip any others
                SeekRelative(lpSMF, dwLen - 3L);

                // found an event
                return FALSE;
            }

            // save the delta time of the meta event so that the
            // timestamp of the _next_ event is correct, since
            // the meta event will be filtered out
            lpSMF->lpTrack[lpSMF->wCurTrack].dwMetaDeltaTime = lpOutBuf->time;

            // skip over any meta data we don't want
            SeekRelative(lpSMF, dwLen);
        }
        else
        {
            // is it a sysex?
            if(event == SYSEX)
            {
                // it's a sysex, leave the status alone




Page 353

                // set the IN_SYSEX flag
                lpSMF->lpTrack[lpSMF->wCurTrack].wFlags |= IN_SYSEX;
                lpOutBuf->status = event;

                // get the length of sysex block
                lpSMF->lpTrack[lpSMF->wCurTrack].dwSysexLen = ReadVarLength(lpSMF);

                // build the first MidiEvent
                lpOutBuf->data1 = event;
                lpOutBuf->data2 = 0;
                lpOutBuf->data3 = 0;

                // found an event
                return FALSE;
            }
            else
            // is it an eox?
            if(event == EOX)
            {
                // is this the termination of a current sysex?
                if(lpSMF->lpTrack[lpSMF->wCurTrack].wFlags & IN_SYSEX)
                {
                    // yes, return the eox to the client
                    lpOutBuf->status = SYSEX;

                    // build the MidiEvent
                    lpOutBuf->data1 = event;
                    lpOutBuf->data2 = 0;
                    lpOutBuf->data3 = 0;

                    // timestamp is zero
                    lpOutBuf->time = 0;

                    // done with this sysex
                    lpSMF->lpTrack[lpSMF->wCurTrack].wFlags &= ~IN_SYSEX;
                    lpSMF->lpTrack[lpSMF->wCurTrack].dwSysexLen = 0;

                    // found an event
                    return FALSE;
                }

                // it's a sysex "escape", set the IN_SYSEX flag
                lpSMF->lpTrack[lpSMF->wCurTrack].wFlags |= IN_SYSEX;
                lpOutBuf->status = SYSEX;

                // get the length of sysex block
                lpSMF->lpTrack[lpSMF->wCurTrack].dwSysexLen = ReadVarLength(lpSMF); 

                // build the first MidiEvent
                lpOutBuf->data1 = ReadByte(lpSMF);
                lpOutBuf->data2 = 0;
                lpOutBuf->data3 = 0;

                // read one of the bytes in block
                lpSMF->lpTrack[lpSMF->wCurTrack].dwSysexLen--;

                // found an event
                return FALSE;
            }
            else
            // is it a data byte for a running status message?
            if((event & 0x80) == 0)
            {
                // if we are in a sysex block, return a single byte from the buffer 
                if(lpSMF->lpTrack[lpSMF->wCurTrack].wFlags & IN_SYSEX)
                {
                    // build the MidiEvent
                    lpOutBuf->status = SYSEX;
                    lpOutBuf->data1 = event;
                    lpOutBuf->data2 = 0;
                    lpOutBuf->data3 = 0;

                    // timestamp for sysex data is always zero
                    lpOutBuf->time = 0;

                    // read one of the bytes in block
                    lpSMF->lpTrack[lpSMF->wCurTrack].dwSysexLen--;

                    // found an event
                    return FALSE;
                }

                // insert running status
                lpOutBuf->status = lpSMF->lpTrack[lpSMF->wCurTrack].bStatus;




Page 354


                // else get the number of remaining bytes (after this one)
                nDataBytes = GetMsgLength(lpOutBuf->status);
                nDataBytes--;

                // there is at least one data byte
                lpOutBuf->data1 = event;
                lpOutBuf->data2 = (nDataBytes != 0 ? ReadByte(lpSMF) : 0);
                lpOutBuf->data3 = 0;

                // found an event
                return FALSE;
            }
            else
            {
                // else it must be a status message
                lpSMF->lpTrack[lpSMF->wCurTrack].bStatus = event;
                lpOutBuf->status = event;

                // retrieve any trailing data bytes
                nDataBytes = GetMsgLength(event);

                lpOutBuf->data1 = (nDataBytes != 0 ? nDataBytes--, ReadByte(lpSMF) 
: 0);
                lpOutBuf->data2 = (nDataBytes != 0 ? ReadByte(lpSMF) : 0);
                lpOutBuf->data3 = 0;

                // found an event
                return FALSE;
            }
        }
    }
}

//------------------------------------------------------------------------
// ReadNextMeta
//
// This function reads then next specified meta event on the current
// track, pointed to by lpCurTrack.  If found, it returns the length
// of the event and sets *time to the elapsed time since the start of the
// track.  The file is located at the start of the data for the event.
// If not found, it returns -1 and *time is undefined.
//------------------------------------------------------------------------
DWORD ReadNextMeta(LPSMF lpSMF, LPTRACK lpCurTrack, BYTE MetaEvent, DWORD *time)
{
    BYTE event;     // event byte read from file
    DWORD dwLen;        // length of event data
    UINT nDataBytes;    // count of remaining data bytes

    // initialize the elapsed time
    *time = 0;

    while(TRUE)     // read through file until an event is found
    {
        // read the leading delta time value
        *time += ReadVarLength(lpSMF);

        // read the first event byte
        // process it according to the event type
        //   if it is a status (other than sysex or eox)
        //      skip midi event
        //  if it is a data byte for a midi message
        //      skip midi event - use running status
        //  if it is a 0xFF - it's a meta event
        //      return with file pointer at start of event data
        //      return length of event data
        //  if it is a sysex or eox
        //      skip entire sysex block
        event = ReadByte(lpSMF);

        // is it a meta event?
        if(event == META)
        {
            // read meta event type
            event = ReadByte(lpSMF);

            // length of the meta event
            dwLen = ReadVarLength(lpSMF);

            // is it an end of track?
            if(event == META_EOT)
                return (DWORD)-1;





Page 355

            // is it the requested value?
            if(event == MetaEvent)
                return dwLen;

            // else skip over it and keep looking
            SeekRelative(lpSMF, dwLen);
        }
        else
        // is it a sysex?
        if((event == SYSEX) || (event == EOX))
        {
            // skip over the whole thing
            SeekRelative(lpSMF, ReadVarLength(lpSMF));
        }
        else
        // is it a data byte for a running status message?
        if((event & 0x80) == 0)
        {
            // get the number of remaining bytes (after this one)
            nDataBytes = GetMsgLength(lpCurTrack->bMetaStatus);
            nDataBytes--;

            // read and discard any more data for this event
            if(nDataBytes != 0)
                ReadByte(lpSMF);
        }
        else
        {
            // else it must be a status message
            lpCurTrack->bMetaStatus = event;

            // get number of remaining bytes (after this one)
            nDataBytes = GetMsgLength(event);

            // read and discard any data bytes for this event
            while(nDataBytes-- != 0)
                ReadByte(lpSMF);
        }
    }   
}

//------------------------------------------------------------------------
// Standard Midi File Write Functions
//------------------------------------------------------------------------
//------------------------------------------------------------------------
// WriteSMF
//
// This function writes a block of events to the previously opened SMF.
// It returns the count of MidiEvents written to the SMF or 0 on error.
//------------------------------------------------------------------------
PREFIX DWORD WINAPI EXPORT WriteSMF(HSMF hSMF, int wTrack, 
LPMIDIEVENT lpMidiEventBuffer, DWORD dwBufferLen)
{
    DWORD dwEventsWritten;  // count of MidiEvents written
    DWORD dwBlockSize;      // size of written block in bytes
    LPSMF lpSMF = (LPSMF)hSMF;

    // was this device opened for write?
    if(lpSMF->mode != 'w')
        return 0;

    // check to see if this is a new track
    isNewTrack(lpSMF, wTrack);

    // convert the Midi events into a smf track data
    dwEventsWritten = 0;
    dwBlockSize = 0;

    while(dwEventsWritten < dwBufferLen)
    {
        dwBlockSize += MidiToSMF(lpSMF, (lpMidiEventBuffer + dwEventsWritten));
        dwEventsWritten++;
    } 
        
    // update the track size count
    lpSMF->dwChunkLen += dwBlockSize;

    return dwEventsWritten;
}

//------------------------------------------------------------------------




Page 356

// WriteMetaEvent
//
// This function writes a Meta Event to the selected track.
//------------------------------------------------------------------------
PREFIX int WINAPI EXPORT WriteMetaEvent(HSMF hSMF, int wTrack, BYTE MetaEvent,
        LPSTR EventValue, DWORD dwTime)
{
    DWORD dwLen;            // length of event data
    DWORD dwEventLength;    // total length of event
    DWORD dwBytesWritten;
    LPSMF lpSMF = (LPSMF)hSMF;

    // check to see if this is a new track
    isNewTrack(lpSMF, wTrack);

    // write the time stamp
    dwEventLength = WriteVarLength(lpSMF, dwTime);

    // write the meta event tag
    WriteByte(lpSMF, META);

    // write the meta event
    WriteByte(lpSMF, MetaEvent);
    dwEventLength += 2;

    // get the length of the event data
    switch(MetaEvent)
    {
        case META_SEQUENCE_NUMBER:
        case META_KEY_SIG:
            dwLen = META_SEQUENCE_NUMBER_LENGTH;
            break;

        case META_CHAN_PREFIX:
            dwLen = META_CHAN_PREFIX_LENGTH;
            break;

        case META_EOT:
            dwLen = META_EOT_LENGTH;
            break;

        case META_TEMPO:
            dwLen = META_TEMPO_LENGTH;
            break;

        case META_SMPTE_OFFSET:
            dwLen = META_SMPTE_OFFSET_LENGTH;
            break;

        case META_TIME_SIG:
            dwLen = META_TIME_SIG_LENGTH;
            break;

        case META_TEXT:
        case META_COPYRIGHT:
        case META_NAME:
        case META_INST_NAME:
        case META_LYRIC:
        case META_MARKER:
        case META_CUE_POINT:
        case META_SEQ_SPECIFIC:
            dwLen = lstrlen(EventValue);
            break;

        default:
            dwLen = 0;
    }

    // write the length, accounting for meta and event
    // number bytes
    dwEventLength += WriteVarLength(lpSMF, dwLen);
    lpSMF->dwChunkLen += dwEventLength;

    // write the data for the event
    if(dwLen != 0)
    {
        WriteFile(lpSMF->hsmf, EventValue, dwLen, &dwBytesWritten, NULL);
        if(dwBytesWritten != dwLen)
            return -1;
    }
       
    return 0;
}




Page 357


//------------------------------------------------------------------------
// isNewTrack
//
// This function checks to see if a new track is being requested.
// If so, it closes the last track (if any) and writes the track chunk.
//------------------------------------------------------------------------
void isNewTrack(LPSMF lpSMF, int wTrack)
{
    DWORD dwCurLoc;         // current file position

    // if this is a new track, close out the old track
    // if this is the first track, just open it
    if(lpSMF->wCurTrack != wTrack)
    {
        if(lpSMF->wCurTrack == -1)
        {
            // save the start of this chunk
            lpSMF->dwChunkStart = CurFilePos(lpSMF);

            // write chunk name and dummy length
            WriteChunkName(lpSMF, "MTrk", CurFilePos(lpSMF), 0);

            // this is the new current track
            lpSMF->wCurTrack = wTrack;
        }
        else
        {
            // this is the new current track.  This must be
            // done here since WriteMetaEvent/isNewTrack 
            // is recursive
            lpSMF->wCurTrack = wTrack;

            // write the end of track meta event by
            // recursively calline WriteMetaEvent
            WriteMetaEvent((HSMF)lpSMF, wTrack, META_EOT, NULL, 0);

            // save the current location
            dwCurLoc = CurFilePos(lpSMF);

            // seek to the start of the track chunk and
            // update the chunk with the correct length
            WriteChunkName(lpSMF, "MTrk", lpSMF->dwChunkStart, lpSMF->dwChunkLen);

            // start of new chunk is end of old one
            lpSMF->dwChunkStart = dwCurLoc;

            // seek back to the original location and
            // write chunk name and dummy length for new track
            WriteChunkName(lpSMF, "MTrk", lpSMF->dwChunkStart, 0);

            // make sure that format is correct now that more
            // than one track has been written
            lpSMF->wFormat = 1;
        }

        // reset running status for new track
        lpSMF->bStatus = 0;

        // new chunk length is zero
        lpSMF->dwChunkLen = 0;
        lpSMF->wTracks++;
    }
}

//------------------------------------------------------------------------
// MidiToSMF
//
// This function reads Midi events from the lpMidiEvent buffer and writes
// running-status-compressed, variable-length-timestamped SMF events to
// the disk file.  If the event is part of a sysex, the data is instead
// written to a buffer.  When the EOX is received, the buffer is written
// to the disk file and the memory is freed.  The buffer will grow in 1K
// increments as needed to hold the entire sysex.  This must be done since
// MidiToSMF does not know the size of the buffer in advance, and the
// length of the sysex must be written to the SMF -- as a variable length
// value -- before the data is written.
// It returns the number of bytes written or buffered.
//------------------------------------------------------------------------
DWORD MidiToSMF(LPSMF lpSMF, LPMIDIEVENT lpMidiEvent)
{
    UINT nDataBytes;            // number of data bytes for event




Page 358

    DWORD dwBytesWritten = 0;   // number of buffer bytes written
    DWORD n = 0;

    // if the message is the sysex status byte, write it
    if(lpMidiEvent->data1 == SYSEX)
    {
        // initialize count of bytes in sysex
        lpSMF->dwSxLen = 0;

        // initialize and allocate a buffer to build the sysex
        // this must be done because we don't know the length
        // of the sysex, and the length must be written first as
        // a variable length value
        lpSMF->dwSxBuffSize = 1024;
        lpSMF->lpSxBuff = (LPSTR)GlobalLock(GlobalAlloc(GHND, lpSMF->dwSxBuffSize));

        // if there is an allocation error, return without writing
        if(lpSMF->lpSxBuff == NULL)
            return 0;

        // convert and write a variable length timestamp
        dwBytesWritten = WriteVarLength(lpSMF, lpMidiEvent->time);
      
        // write the status
        WriteByte(lpSMF, lpMidiEvent->data1);

        return dwBytesWritten + 1;
    }

    // if the message is the eox for a sysex, write out the buffer
    if(lpMidiEvent->data1 == EOX)
    {
        // write the length of the sysex as var length
        // account for the eox that will be written
        dwBytesWritten += WriteVarLength(lpSMF, lpSMF->dwSxLen + 1);

        // write the buffer of data to disk
        WriteFile(lpSMF->hsmf, lpSMF->lpSxBuff, lpSMF->dwSxLen, &n, NULL);

        // write the eox
        WriteByte(lpSMF, EOX);

        // free the sysex buffer
        GlobalFree(lpSMF->lpSxBuff);

        return dwBytesWritten + lpSMF->dwSxLen + 1;
    }

    // if the message is sysex data, just write the data byte
    if((lpMidiEvent->status == SYSEX) && (lpMidiEvent->data1 != SYSEX))
    {
        // check to see if the buffer needs to be grown
        if(lpSMF->dwSxLen == lpSMF->dwSxBuffSize)
        {
            // grow it in 1K chunks 
            lpSMF->dwSxBuffSize += 1024;
            lpSMF->lpSxBuff = (LPSTR)GlobalReAlloc(lpSMF->lpSxBuff, 
                    lpSMF->dwSxBuffSize, GMEM_MOVEABLE);
        }

        // write the data
        *(lpSMF->lpSxBuff + lpSMF->dwSxLen) = lpMidiEvent->data1;

        // count the number of bytes in sysex
        lpSMF->dwSxLen++;

        return 1;
    }

    // else convert and write a variable length timestamp
    dwBytesWritten = (DWORD)WriteVarLength(lpSMF, lpMidiEvent->time);
       
    // if it is a tempo change event, write the meta event
    if(lpMidiEvent->status == 0)
    {
        // write the event
        WriteByte(lpSMF, META);
        WriteByte(lpSMF, META_TEMPO);
        WriteByte(lpSMF, META_TEMPO_LENGTH);
        WriteByte(lpSMF, lpMidiEvent->data1);
        WriteByte(lpSMF, lpMidiEvent->data2);
        WriteByte(lpSMF, lpMidiEvent->data3);

        return dwBytesWritten + 6;




Page 359

    }

    // if this status is the same as the last (running) skip over it
    if(lpMidiEvent->status != lpSMF->bStatus)
    {
        WriteByte(lpSMF, lpMidiEvent->status);
        dwBytesWritten++;
        lpSMF->bStatus = lpMidiEvent->status;
    }

    // write the data for the event, if any
    nDataBytes = GetMsgLength(lpMidiEvent->status);
    dwBytesWritten += nDataBytes;

    if(nDataBytes != 0)
    {
        WriteByte(lpSMF, lpMidiEvent->data1);
        nDataBytes--;
    }

    if(nDataBytes != 0)
        WriteByte(lpSMF, lpMidiEvent->data2);

    return dwBytesWritten;
}

//------------------------------------------------------------------------
// WriteByte
//
// This function writes the byte val to the SMF disk file.
//------------------------------------------------------------------------
void WriteByte(LPSMF lpSMF, BYTE val)
{
    DWORD dwBytesWritten = 0;

    WriteFile(lpSMF->hsmf, &val, 1, &dwBytesWritten, NULL);
}

//------------------------------------------------------------------------
// WriteVarLength
//
// This function writes a timestamp to the buffer as a variable length
// value.  It returns the number of bytes written to the buffer and the
// buffer pointing to the next location.
//------------------------------------------------------------------------
DWORD WriteVarLength(LPSMF lpSMF, DWORD value)
{
    DWORD nBytesWritten = 0;        // count of bytes written
    DWORD dwBuffer;             // dword to build var length in

    // write up to four bytes to the buffer, stop when byte == 0
    dwBuffer = value & 0x7F;

    while((value >>= 7) > 0)
    {
        dwBuffer <<= 8;
        dwBuffer |= 0x80;
        dwBuffer += (value & 0x7F);
    }

    // copy the bytes, in reverse order to the destination
    while(TRUE)
    {
        WriteByte(lpSMF, (BYTE)dwBuffer);
        nBytesWritten++;
        if(dwBuffer & 0x80)
            dwBuffer >>= 8;
        else
            break;
    }

    return nBytesWritten;
}

//------------------------------------------------------------------------
// WriteReversedData
//
// This function writes the data to the file in reverse byte order.
//------------------------------------------------------------------------
void WriteReversedData(LPSMF lpSMF, LPVOID lpData, int wSize)
{
    DWORD dwBytesWritten = 0;





Page 360

    while(--wSize != -1)
        WriteFile(lpSMF->hsmf, &((LPBYTE)lpData)[wSize], 1, &dwBytesWritten, NULL);
}

//------------------------------------------------------------------------
// WriteChunkName
//
// This function writes the name and length of the chunk to the file.
//------------------------------------------------------------------------
void WriteChunkName(LPSMF lpSMF, LPSTR ChunkName, DWORD offset, DWORD len)
{
    DWORD dwBytesWritten = 0;

    // seek to the desired location in the file
    SeekTo(lpSMF, offset);

    // write the name
    WriteFile(lpSMF->hsmf, ChunkName, lstrlen(ChunkName), &dwBytesWritten, NULL);

    // write the chunk size                       
    WriteReversedData(lpSMF, &len, sizeof(DWORD));
}

//------------------------------------------------------------------------
// WriteSMFHeader
//
// This function writes the header for the SMF, based on the information
// in the lpSMF structure.
//------------------------------------------------------------------------
void WriteSMFHeader(LPSMF lpSMF)
{
    // write the 'MThd' chunk type at the start of the file
    WriteChunkName(lpSMF, "MThd", 0, SMF_HEADER_SIZE);

    // write the data to the chunk
    WriteReversedData(lpSMF, &lpSMF->wFormat, sizeof(WORD));
    WriteReversedData(lpSMF, &lpSMF->wTracks, sizeof(WORD));
    WriteReversedData(lpSMF, &lpSMF->resolution, sizeof(WORD));
}
