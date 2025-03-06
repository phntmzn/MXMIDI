//-----------------------------------------------------------------------------
// Maximum MIDI Programmer's ToolKit - MxMidi16.DLL (macOS Port)
// MIDI Event Synchronization Module (macOS using CoreMIDI & CoreAudio)
//
// Original Copyright (c) Paul A. Messick, 1994-1996
// macOS Port by <Your Name>
//
// Provides functions to play and time MIDI events.
//-----------------------------------------------------------------------------

#include <CoreMIDI/CoreMIDI.h>
#include <mach/mach_time.h>
#include <dispatch/dispatch.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_RESOLUTION 960
#ifndef NSEC_PER_MSEC
#define NSEC_PER_MSEC 1000000 // 1 millisecond in nanoseconds
#endif

typedef struct {
    MIDIClientRef client;
    MIDIPortRef outputPort;
    MIDIEndpointRef destination;
    dispatch_source_t timer;
    uint64_t lastTime;
    uint64_t tempo; // microseconds per beat
    uint16_t resolution;
    mach_timebase_info_data_t timebaseInfo;
} MIDISyncHandler;

// Global MIDI sync handler
MIDISyncHandler midiSync;

//-----------------------------------------------------------------------------
// syncHandler
//
// Called every 1ms to generate MIDI ticks.
//-----------------------------------------------------------------------------
void syncHandler(void) {
    uint64_t currentTime = mach_absolute_time();
    uint64_t elapsedNanoSeconds = (currentTime - midiSync.lastTime) *
                                  midiSync.timebaseInfo.numer / midiSync.timebaseInfo.denom;
    uint64_t elapsedMicroseconds = elapsedNanoSeconds / 1000;

    // Calculate elapsed MIDI ticks
    uint64_t nticks = (elapsedMicroseconds * midiSync.resolution) / midiSync.tempo;

    // If enough ticks have elapsed, send MIDI clock
    if (nticks >= (midiSync.resolution / 24)) {
        // Send MIDI clock message
        Byte packetListBuffer[256];
        MIDIPacketList *packetList = (MIDIPacketList *)packetListBuffer;
        MIDIPacket *packet = MIDIPacketListInit(packetList);
        Byte message[1] = {0xF8}; // MIDI Clock Message
        packet = MIDIPacketListAdd(packetList, sizeof(packetListBuffer), packet, 0, 1, message);
        if (packet) {
            OSStatus status = MIDISend(midiSync.outputPort, midiSync.destination, packetList);
            if (status == noErr) {
                printf("Sent MIDI Clock.\n");
            } else {
                fprintf(stderr, "Error: Could not send MIDI clock.\n");
            }
        }
        midiSync.lastTime = currentTime;
    }
}

//-----------------------------------------------------------------------------
// syncHandlerF
//
// A helper function to wrap syncHandler for dispatch_source_set_event_handler_f.
//-----------------------------------------------------------------------------
void syncHandlerF(void *context) {
    (void)context; // unused
    syncHandler();
}

//-----------------------------------------------------------------------------
// OpenSync
//
// Initializes CoreMIDI and starts the sync timer.
//-----------------------------------------------------------------------------
int OpenSync() {
    // Create MIDI client
    if (MIDIClientCreate(CFSTR("MxMidiClient"), NULL, NULL, &midiSync.client) != noErr) {
        fprintf(stderr, "Error: Could not create MIDI client.\n");
        return -1;
    }

    // Create an output port
    if (MIDIOutputPortCreate(midiSync.client, CFSTR("OutputPort"), &midiSync.outputPort) != noErr) {
        fprintf(stderr, "Error: Could not create MIDI output port.\n");
        return -1;
    }

    // Get first available MIDI output device
    midiSync.destination = MIDIGetDestination(0);
    if (midiSync.destination == 0) {
        fprintf(stderr, "Error: No MIDI output destinations found.\n");
        return -1;
    }

    // Set default tempo (120 BPM = 500000 microseconds per beat)
    midiSync.tempo = 500000;
    midiSync.resolution = 480;
    midiSync.lastTime = mach_absolute_time();

    // Get mach timebase info for nanosecond conversion
    if (mach_timebase_info(&midiSync.timebaseInfo) != KERN_SUCCESS) {
        fprintf(stderr, "Error: Failed to get mach timebase info.\n");
        return -1;
    }

    printf("MIDI Sync initialized successfully.\n");

    // Start sync timer (1ms precision)
    dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0);
    midiSync.timer = dispatch_source_create(DISPATCH_SOURCE_TYPE_TIMER, 0, 0, queue);
    if (!midiSync.timer) {
        fprintf(stderr, "Error: Could not create dispatch timer.\n");
        return -1;
    }
    dispatch_source_set_timer(midiSync.timer,
                              dispatch_time(DISPATCH_TIME_NOW, NSEC_PER_MSEC),
                              NSEC_PER_MSEC, 0);
    dispatch_source_set_event_handler_f(midiSync.timer, syncHandlerF);
    dispatch_resume(midiSync.timer);
    return 0;
}

//-----------------------------------------------------------------------------
// SetTempo
//
// Changes the tempo (in microseconds per beat).
//-----------------------------------------------------------------------------
void SetTempo(uint64_t uSPerBeat) {
    if (uSPerBeat == 0) return;
    midiSync.tempo = uSPerBeat;
    printf("Tempo set to %llu uS/beat.\n", uSPerBeat);
}

//-----------------------------------------------------------------------------
// SetResolution
//
// Changes the resolution (ticks per beat).
//-----------------------------------------------------------------------------
void SetResolution(uint16_t resolution) {
    if (resolution > MAX_RESOLUTION) return;
    midiSync.resolution = resolution;
    printf("Resolution set to %d ticks/beat.\n", resolution);
}

//-----------------------------------------------------------------------------
// CloseSync
//
// Stops the MIDI sync timer and releases CoreMIDI resources.
//-----------------------------------------------------------------------------
void CloseSync() {
    if (midiSync.timer) {
        dispatch_source_cancel(midiSync.timer);
        dispatch_release(midiSync.timer);
    }
    MIDIPortDispose(midiSync.outputPort);
    MIDIClientDispose(midiSync.client);
    printf("MIDI Sync closed.\n");
}

//-----------------------------------------------------------------------------
// Main function for testing MIDI sync
//-----------------------------------------------------------------------------
int main() {
    if (OpenSync() != 0) return -1;

    printf("Running MIDI Sync... Press ENTER to stop.\n");
    getchar();

    CloseSync();
    return 0;
}