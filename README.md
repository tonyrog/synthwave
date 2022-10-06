# SynthWave

## How connect VMPK to synthwave

First enable virtual synth devices 

    sudo modprobe snd-virmidi snd_index=1

Then in the VMPK menu "Edit/MIDI connection" select MIDI OUT Driver ALSA
and the select Virtual Raw MIDI 1-0

check input devives

    midi:devices()
	
run synthwave

    synthwave:start("/dev/snd/midiC1D0")
	
or the whatever device is beeing used.

