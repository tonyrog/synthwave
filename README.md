# SynthWave

## Connect VMPK to synthwave

First enable virtual synth devices 

    sudo modprobe snd-virmidi snd_index=1

Then in the VMPK menu "Edit/MIDI connection" select MIDI OUT Driver ALSA
and the select Virtual Raw MIDI 1-0

check input devives

    > midi:devices().
	
run synthwave

    > synthwave:start("VMPK").

if that does not work, try this

    > synthwave:start_device("/dev/snd/midiC1D0").
	
or the whatever device is beeing used.

## Connect LPK25 to synthwave

A small midi keyboard like LPK25 is a midi input device from
scratch, to use that just type:

    > synthwave:start("LPK25").
