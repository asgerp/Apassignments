%%%-------------------------------------------------------------------
%%% @implementors Asger Pedersen and Kristoffer Cobley
%%% @auther Ken Friis Larsen  
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(mr_skel).

-export([start/1, stop/1, job/5]).

%%%% Notes:
%%% 

%%%% Interface

start(N) ->
    {Reducer, Mappers} = init(N),
    spawn(fun() -> coordinator_loop(Reducer, Mappers) end).


%%% TODO stop the damn thing
stop(Pid) -> ....
    
    
%%% Coordinator id
job(CPid, MapFun, RedFun, RedInit, Data) -> ....


%%%% Internal implementation

%%% TODO Make N mappers and one reducer
init(N) -> ....


%% synchronous communication

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).


%% asynchronous communication

info(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    info(Pid, stop).

data_async(Pid, D) ->
    info(Pid, {data, D}).



%%% Coordinator
%%% also not stop and default
coordinator_loop(Reducer, Mappers) ->
    receive
	{From, stop} ->
	    io:format("~p stopping~n", [self()]),
	    lists:foreach(fun stop_async/1, Mappers),
	    stop_async(Reducer),
	    reply_ok(From);
	....
    end.



send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer
%%% reduce case, and default
reducer_loop() ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
        ....
    end.

%%% get data from mappers
gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	...
    end.


%%% Mapper
%%% continue
mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	....
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.
