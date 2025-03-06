//-----------------------------------------------------------------------------
// Maximum MIDI Programmer's ToolKit - MxMidi16.DLL
// MIDI Output Module (macOS Port using CoreMIDI)
//
// Original Copyright (c) Paul A. Messick, 1994-1996
// macOS Port by <Your Name>
//
// Provides a MIDI output system using CoreMIDI.
//-----------------------------------------------------------------------------

#include <CoreMIDI/CoreMIDI.h>
#include <AudioToolbox/AudioToolbox.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    MIDIClientRef client;
    MIDIPortRef outputPort;
    MIDIEndpointRef destination;
} MIDIHandler;

// Global MIDI handler
MIDIHandler midiHandler;

//-----------------------------------------------------------------------------
// OpenMidiOut
//
// Initializes the CoreMIDI client and output port.
//-----------------------------------------------------------------------------
int OpenMidiOut() {
    // Create a MIDI client
    if (MIDIClientCreate(CFSTR("MxMidiClient"), NULL, NULL, &midiHandler.client) != noErr) {
        fprintf(stderr, "Error: Could not create MIDI client.\n");
        return -1;
    }

    // Create an output port
    if (MIDIOutputPortCreate(midiHandler.client, CFSTR("OutputPort"), &midiHandler.outputPort) != noErr) {
        fprintf(stderr, "Error: Could not create MIDI output port.\n");
        return -1;
    }

    // Get the first available destination (MIDI output device)
    midiHandler.destination = MIDIGetDestination(0);
    if (midiHandler.destination == 0) {
        fprintf(stderr, "Error: No MIDI output destinations found.\n");
        return -1;
    }

    printf("MIDI Output initialized successfully.\n");
    return 0;
}

//-----------------------------------------------------------------------------
// SendMidiMessage
//
// Sends a MIDI short message (note on/off, CC, etc.).
//-----------------------------------------------------------------------------
void SendMidiMessage(Byte status, Byte data1, Byte data2) {
    Byte packetListBuffer[256];
    MIDIPacketList *packetList = (MIDIPacketList *)packetListBuffer;
    MIDIPacket *packet = MIDIPacketListInit(packetList);
    
    Byte message[3] = {status, data1, data2};
    packet = MIDIPacketListAdd(packetList, sizeof(packetListBuffer), packet, 0, 3, message);
    
    if (packet) {
        MIDIReceived(midiHandler.destination, packetList);
        printf("Sent MIDI Message: %02X %02X %02X\n", status, data1, data2);
    } else {
        fprintf(stderr, "Error: Could not add MIDI packet.\n");
    }
}

//-----------------------------------------------------------------------------
// CloseMidiOut
//
// Closes the CoreMIDI client and cleans up resources.
//-----------------------------------------------------------------------------
void CloseMidiOut() {
    MIDIPortDispose(midiHandler.outputPort);
    MIDIClientDispose(midiHandler.client);
    printf("MIDI Output closed.\n");
}

//-----------------------------------------------------------------------------
// GetMidiDeviceName
//
// Retrieves the name of the first available MIDI output device.
//-----------------------------------------------------------------------------
void GetMidiDeviceName() {
    if (midiHandler.destination == 0) {
        printf("No MIDI devices found.\n");
        return;
    }

    CFStringRef name;
    if (MIDIObjectGetStringProperty(midiHandler.destination, kMIDIPropertyName, &name) == noErr) {
        char deviceName[128];
        CFStringGetCString(name, deviceName, sizeof(deviceName), kCFStringEncodingUTF8);
        CFRelease(name);
        printf("MIDI Device: %s\n", deviceName);
    }
}

//-----------------------------------------------------------------------------
// Main function for testing MIDI output
//-----------------------------------------------------------------------------
int main() {
    if (OpenMidiOut() != 0) {
        return -1;
    }

    GetMidiDeviceName();

    // Test sending MIDI messages
    printf("Sending MIDI Note On...\n");
    SendMidiMessage(0x90, 60, 127); // Note On, Middle C, Velocity 127
    sleep(1);  // Wait 1 second
    printf("Sending MIDI Note Off...\n");
    SendMidiMessage(0x80, 60, 0);   // Note Off, Middle C, Velocity 0

    printf("Press ENTER to exit...\n");
    getchar();

    CloseMidiOut();
    return 0;
}