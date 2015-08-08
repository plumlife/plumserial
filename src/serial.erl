%% Copyright (c) 2014 Matt Brandt, Plum Inc.
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%    -*- Erlang -*- 
%    File:	serial.erl 
%    Author:	Matt Brandt
%    Created:	Thu Nov 6, 2014

-module(serial).
-author('mbrandt@plumlife.com').

-export([start/1, start/2, init/2, loop/2]).
-export([send/2, stop/1, inject/2]).

priv_dir() ->
    case code:priv_dir(serial) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

start(Name) ->
    start(Name, []).

start(Name, Options) -> 
	Pid = spawn_link(serial, init, [self(), Options]),
	register(Name, Pid),
	Pid.

send(Name, Bytes) -> Name ! {send, Bytes}.

stop(Name) -> Name ! stop.

inject(Name, Bytes) -> Name ! {self(), {data, Bytes}}.

process_options(Acc, []) -> Acc;
process_options(Acc, [Opt|Opts]) ->
    case Opt of
    	{speed, Spd} -> process_options(Acc ++ " -s " ++ integer_to_list(Spd), Opts);
    	{tty, Name} -> process_options(Acc ++ " " ++ Name, Opts);
    	_ -> 
            io:format("bad option for serial: ~p\n", Opt),
            process_options(Acc, Opts)
    end.

init(Pid, Options) ->
    process_flag(trap_exit, true),
    OptString = process_options([], Options),
    Port = open_port({spawn, [priv_dir(), "/serial", OptString]}, [binary, stream]),
    loop(Pid, Port).

loop(Pid, Port) ->
    receive
        {Port, {data, Bytes}} ->
            Pid ! {data, Bytes};
        {send, Bytes} ->
            Port ! {self(), {command, Bytes}};
        stop ->
            stopped;
        {'EXIT', _, Why} ->
            io:format("Exited with reason ~p", [Why]),
            exit(Why);
        OtherError ->
            io:format("Received unknown message ~p", [OtherError])
    end,
    loop(Pid, Port).
