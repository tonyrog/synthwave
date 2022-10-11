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

wdef(Note, Velocity, Amp) ->
    Freq1 = alsa_util:midi_note_to_frequency(Note),
    Freq2 = alsa_util:midi_note_to_frequency(max(127,Note+4)),
    V = Velocity / 254,
    [{adsr, 0.05, 0.05, 0.4, 0.1},
     {wave, 0, [#{ form=>sine, freq=>Freq1, level=>0.0},
		#{ form=>sine, freq=>Freq1+50, level=>(V+0.5)*Amp},
		#{ form=>triangle, freq=>Freq1, level=>0.5*Amp},
		#{ form=>sine, freq=>Freq1, level=>0.5*Amp},
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
    Def = wdef(?C, 1, 0),
    lists:foreach(fun(I) ->
			  alsa_play:new(I),
			  alsa_play:stop(I),
			  alsa_play:set_wave(I, Def)
		  end, Voices),
    alsa_play:resume(),
    iloop(#state { midi_in=In, voices=Voices, active=#{}, pressure=#{}}).

%% midi input loop
iloop(State=#state{midi_in=In}) ->
    case midi:read(In) of
	select ->
	    sloop(State);
	{ok,N} when is_integer(N) ->
	    oloop(State);
	Error ->
	    Error
    end.

%% wait for select and handle notes that are done
sloop(State=#state{midi_in=In}) ->
    receive
	{select,In,undefined,ready_input} ->
	    oloop(State);
	{_Ref,_Voice,_Pos, {off,Chan,Note}} ->
	    State1 = off(Chan, Note, false, State),
	    sloop(State1)
    end.

%% process midi events and generate waves
oloop(State=#state{midi_in=In}) ->
    receive
	{midi,In,Event} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    State1 = midi_event(Event, State),
	    oloop(State1);
	{midi,In,Event,_Delta} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    State1 = midi_event(Event, State),
	    oloop(State1);
	{_Ref,_Voice,_Pos, {off,Chan,Note}} ->
	    State1 = off(Chan, Note, false, State),
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
note_on(Chan,Note,Velocity,State) ->
    case allocate_voice(State#state.voices) of
	{V,Vs} ->
	    io:format("~w: allocate ~w, vs:~p\n", [{Chan,Note}, V, Vs]),
	    alsa_play:set_wave(V, wdef(Note,Velocity,1)),
	    alsa_play:mark(V, {time,600}, [stop,once,{set,bof},notify],
			   {off,Chan,Note}),
	    alsa_play:restart(V),
	    alsa_play:run(V),
	    Active = maps:put({Chan,Note}, V, State#state.active),
	    State#state { voices = Vs, active = Active };
	false ->
	    State
    end.

note_off(_Chan,_Note,_Velocity,State) ->
    %% off(Chan, Node, true, State),
    State.

off(Chan,Note,Stop,State) ->
    case maps:take({Chan,Note}, State#state.active) of
     	{V, Active1} ->
	    if Stop ->
		    alsa_play:stop(V);
	       true ->
		    ok
	    end,
	    Voices = release_voice(V, State#state.voices),
	    io:format("~w: release ~w, vs: ~p\n", [{Chan,Note},V,Voices]),
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
