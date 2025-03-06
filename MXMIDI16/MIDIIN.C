// Maximum MIDI Programmer's ToolKit - MxMidi16.Dylib
// MIDI Input Module for macOS using CoreMIDI
//
// Copyright (c) Paul A. Messick, 1994-1996
// Updated for macOS by <Your Name>
//
// This version uses CoreMIDI instead of Windows MIDI APIs.

#include <CoreMIDI/CoreMIDI.h>
#include <AudioToolbox/AudioToolbox.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef struct {
    MIDIClientRef client;
    MIDIEndpointRef endpoint;
    MIDIPortRef port;
} MIDIHandler;

// Global MIDIHandler instance
MIDIHandler midiHandler;

// MIDI callback function
void MidiInputCallback(const MIDIPacketList *pktlist, void *readProcRefCon, void *srcConnRefCon) {
    MIDIPacket *packet = (MIDIPacket *)pktlist->packet;
    for (int i = 0; i < pktlist->numPackets; i++) {
        printf("MIDI Event: %02X %02X %02X\n", packet->data[0], packet->data[1], packet->data[2]);
        packet = MIDIPacketNext(packet);
    }
}

// Initialize MIDI Input
int OpenMidiIn() {
    if (MIDIClientCreate(CFSTR("MxMidiClient"), NULL, NULL, &midiHandler.client) != noErr) {
        fprintf(stderr, "Error: Could not create MIDI client.\n");
        return -1;
    }

    if (MIDIInputPortCreate(midiHandler.client, CFSTR("InputPort"), MidiInputCallback, NULL, &midiHandler.port) != noErr) {
        fprintf(stderr, "Error: Could not create MIDI input port.\n");
        return -1;
    }

    MIDIEndpointRef source = MIDIGetSource(0);
    if (source == 0) {
        fprintf(stderr, "Error: No MIDI sources found.\n");
        return -1;
    }

    if (MIDIPortConnectSource(midiHandler.port, source, NULL) != noErr) {
        fprintf(stderr, "Error: Could not connect to MIDI source.\n");
        return -1;
    }

    printf("MIDI Input initialized successfully.\n");
    return 0;
}

// Close MIDI Input
void CloseMidiIn() {
    MIDIPortDispose(midiHandler.port);
    MIDIClientDispose(midiHandler.client);
    printf("MIDI Input closed.\n");
}

// Get MIDI Device Name
void GetMidiDeviceName() {
    MIDIEndpointRef source = MIDIGetSource(0);
    if (source == 0) {
        printf("No MIDI devices found.\n");
        return;
    }

    CFStringRef name;
    if (MIDIObjectGetStringProperty(source, kMIDIPropertyName, &name) == noErr) {
        char deviceName[128];
        CFStringGetCString(name, deviceName, sizeof(deviceName), kCFStringEncodingUTF8);
        CFRelease(name);
        printf("MIDI Device: %s\n", deviceName);
    }
}

// Main function to test MIDI input
int main() {
    if (OpenMidiIn() != 0) {
        return -1;
    }

    GetMidiDeviceName();

    printf("Press ENTER to exit...\n");
    getchar();

    CloseMidiIn();
    return 0;
}