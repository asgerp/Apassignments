%%%-------------------------------------------------------------------
%%% @implementors Asger Pedersen and Kristoffer Cobley
%%% @auther Ken Friis Larsen  
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(mr_skel).

-export([start/1, stop/1, job/5]).

%%% Start the MapReducer with N mappers, returns ok, Pid of the coordinator
start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.


%%% Stop the MapReducer with coordinator Pid
stop(Pid) -> 
    reply(Pid,stop).
    
    
%%% Define a MapReduce job
job(CPid, MapFun, RedFun, RedInit, Data) -> 
    rpc(CPid, {init, MapFun, RedFun, RedInit, Data}),
    rpc(CPid, {start, Data}).
    

%%%% Internal implementation

%%% Make N mappers and one reducer
init(N) ->  
    init(N,spawn(fun() -> reducer_loop() end),[]).
init(N, Red, Map) when N > 0 ->
    init(N-1, Red, [spawn(fun() -> mapper_loop(Red, fun() -> im_a_mapper end) end)]++Map); 
init(0, Red, Map) -> {Red, Map}.


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
	{From, {init, MapFun, RedFun, RedInit, Data}} ->
	    Len = length(Data),
	    info(Reducer,{self(),{start,RedFun, RedInit, Len, From}}),
	    init_mappers(Mappers, MapFun),
	    reply_ok(From),
	    coordinator_loop(Reducer, Mappers);
	{_, {start, Data}} ->
	    send_data(Mappers, Data),
	    coordinator_loop(Reducer, Mappers);
	{_, {done, Result, Jid}} ->
	    reply_ok(Jid, Result),
	    coordinator_loop(Reducer, Mappers);
	Unknown ->
	    io:format("[CL] unknown message: ~p~n",[Unknown]), 
	    coordinator_loop(Reducer, Mappers)
    end.

init_mappers([Mid|Mappers], MapFun)->
    info(Mid, {self(), {init, MapFun}}),
   init_mappers(Mappers, MapFun);
init_mappers([],_) ->
    done.


send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
   % io:format("da ~p", [Data]),
   % io:format("d ~p", [D]),
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer
reducer_loop() ->
    receive
	stop -> 
	    io:format("Reducer ~p stopping~n", [self()]),
	    ok;
	{From, {start, RedFun, RedInit, Len, Jid}} ->
	    {stop_gather, OverAllRes} = gather_data_from_mappers(RedFun, RedInit, Len),
	    info(From, {self(),{done, OverAllRes, Jid}}),
	    reducer_loop();
	{stop_gather, Res} ->
	    Res,
	    reducer_loop();
	Unknown ->
	    io:format("[RL] unknown message: ~p~n",[Unknown]), 
	    reducer_loop()
    end.

%%% get data from mappers
gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	{stop_gather, Acc} ->
	    Acc;
	{_, {result, ChunkOfData}} ->
%	    io:format("chunk ~p ~n",[ChunkOfData]),
	    Res = (lists:foldl(Fun, Acc, ChunkOfData)),
	    Miss = Missing - 1,
	    if Miss >= 1 ->
		    gather_data_from_mappers(Fun, Res, Miss);
	       true -> info(self(),{stop_gather,Res})
	    end;
	Unknown ->
	    io:format("[GDFM] unknown message: ~p~n",[Unknown]), 
	    gather_data_from_mappers(Fun, Acc, Missing)
    end.


%%% Mapper
mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	{_, {init, NewFun}} ->
	    %reply_ok(From),
	    mapper_loop(Reducer, NewFun);
	{data, Data} ->
	    Res = lists:map(Fun,Data),
	    info(Reducer,{self(), {result, Res}}),
	    mapper_loop(Reducer, Fun);
	Unknown ->
	    io:format("[ML] unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.

