# Makefile for MXMIDI16 on macOS

PROJ = MXMIDI16
CC = clang
CFLAGS = -Wall -g -O2 -I ../include -I ../../include
LFLAGS = -framework CoreMIDI -framework CoreAudio
OBJS = MIDIIN.o MIDIOUT.o SYNC.o
DEFFILE = MXMIDI16.def

all: $(PROJ).dylib

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(PROJ).dylib: $(OBJS)
	$(CC) -dynamiclib -o $(PROJ).dylib $(OBJS) $(LFLAGS)

clean:
	rm -f $(OBJS) $(PROJ).dylib