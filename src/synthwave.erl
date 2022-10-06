%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Synth wave midi player
%%% @end
%%% Created :  6 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(synthwave).

-export([start/0, start/1]).
-export([start_lpk25/0, start_vmpk/0, start_vpc1/0]).
-export([start_device/1]).

-include_lib("midi/include/midi.hrl").

-define(MAX_VOICES, 10).
-type chan() :: 0..15.
-type note() :: 0..127.
-type voice() :: 1..?MAX_VOICES.
-type pressure() :: 1..127.

-record(state,
	{
	 midi_in,      %% midi input device
	 voices,
	 active = #{} :: #{ {chan(),note()} => voice() },
	 pressure = #{} :: #{ chan() => pressure() }
	}).

wdef(Note, Amp) ->
    Freq1 = midi_play:note_to_frequency(Note),
    Freq2 = midi_play:note_to_frequency(Note+4),
    [{adsr, 0, 0.05, 0.05, 0.7, 0.1},
     {wave, 0, [#{ form=>sine, freq=>Freq1, level=>0.0},
		#{ form=>sine, freq=>Freq1+50, level=>0.9*Amp},
		#{ form=>triangle, freq=>Freq1, level=>0.7*Amp},
		#{ form=>sine, freq=>Freq1, level=>0.7*Amp},
		#{ form=>sine, freq=>Freq1-10, level=>0.0}
	       ]},
     {wave, 1, [#{ form=>sine, freq=>Freq2, level=>0.1*Amp},
		#{ form=>sine, freq=>Freq2, level=>0.1*Amp},
		#{ form=>sine, freq=>Freq2, level=>0.1*Amp},
		#{ form=>sine, freq=>Freq2, level=>0.1*Amp},
		#{ form=>sine, freq=>Freq2, level=>0.1*Amp}
	       ]}
    ].


start() -> start_lpk25().

start_lpk25() ->
    start("LPK25").    

start_vmpk() ->
    start("VMPK").

start_vpc1() ->
    start("VPC1").

start(Name) ->
    Ds = midi:devices(),
    case midi:find_device_by_name(Name, Ds) of
	#{ device := Device } ->
	    start_device(Device);
	#{ output := [Out|_]} ->
	    case midi:find_device_by_port(Out, Ds) of
		#{ device := Device } ->
		    start_device(Device);
		_ ->
		    {error, not_found}
	    end;
	_ ->
	    {error, not_found}
    end.


start_device(InputDevice) ->
    {ok,In}  = midi:open(InputDevice,[event,list,running]),
    Voices = lists:seq(1,?MAX_VOICES),    
    alsa_play:start(#{ rate => 44100 }), %% latency => 50
    Def = wdef(?C, 0),
    alsa_play:new_wave(0, Def),  %% dummy
    lists:foreach(fun(I) ->
			  alsa_play:new_wave(I, Def),
			  alsa_play:stop(I)
		  end, Voices),
    alsa_play:run(),
    alsa_play:resume(),
    iloop(#state { midi_in=In, voices=Voices, active=#{}, pressure=#{}}).

iloop(State=#state{midi_in=In}) ->
    case midi:read(In) of
	select ->
	    receive
		{select,In,undefined,ready_input} ->
		    oloop(State)
	    end;
	{ok,N} when is_integer(N) ->
	    oloop(State);
	Error ->
	    Error
    end.

oloop(State=#state{midi_in=In}) ->
    receive
	{midi,In,Event} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    State1 = midi_event(Event, State),
	    oloop(State1);
	{midi,In,Event,_Delta} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    State1 = midi_event(Event, State),
	    oloop(State1)
    after 0 ->
	    iloop(State)
    end.    

midi_event({note_on,Chan,Note,Velocity}, State) ->
    note_on(Chan, Note, Velocity, State);
midi_event({note_off,Chan,Note,Velocity}, State) ->
    note_off(Chan, Note, Velocity, State);
midi_event({pressure, Chan, Pressure}, State) ->
    Pressure = maps:put(Chan, Pressure, State#state.pressure),
    State#state { pressure = Pressure };
midi_event(_, State) ->
    State.

note_on(Chan,Note,0,State) ->
    note_off(Chan,Note,0,State);
note_on(Chan,Note,_Velocity,State) ->
    case allocate_voice(State#state.voices) of
	{V,Vs} ->
	    %% io:format("allocate ~w to ~w\n", [V, {Chan,Note}]),
	    alsa_play:set_wave(V, wdef(Note, 1)),
	    alsa_play:restart(V),
	    alsa_play:run(V),
	    Active = maps:put({Chan,Note}, V, State#state.active),
	    State#state { voices = Vs, active = Active };
	false ->
	    State
    end.

note_off(Chan,Note,_Velocity,State) ->
    case maps:take({Chan,Note}, State#state.active) of
	{V, Active1} ->
	    %% io:format("release ~w for ~w\n", [V, {Chan,Note}]),
	    alsa_play:stop(V),
	    Voices = release_voice(V, State#state.voices),
	    State#state { voices = Voices, active = Active1 };
	error -> 
	    State
    end.

release_voice(V, Vs) ->
    [V|Vs].

allocate_voice([V|Vs]) ->
    {V, Vs};
allocate_voice([]) ->
    false.
