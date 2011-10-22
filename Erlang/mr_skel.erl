%%%-------------------------------------------------------------------
%%% @implementors Asger Pedersen and Kristoffer Cobley
%%% @auther Ken Friis Larsen  
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(mr_skel).

-export([start/1, stop/1, job/5, test_sum/0, test_start/0, test_job/0, init/1]).

%%%% Notes:
%%% 

%%%% Interface

start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.


%%% TODO stop the damn thing
stop(Pid) -> 
    info(Pid,stop).
    
    
%%% Coordinator id
job(CPid, MapFun, RedFun, RedInit, Data) -> 
    io:format("job~n"),
    rpc(CPid, {init, MapFun, RedFun, RedInit, Data}),
    io:format("kuku~n"),
    Res = rpc(CPid, {start,Data}) .
    

%%%% Internal implementation

%%% TODO Make N mappers and one reducer
init(N) ->  
    init(N,spawn(fun() -> reducer_loop() end),[]).
init(N,Red,Map) when N > 0 ->
    init(N-1,Red,[spawn(fun() -> mapper_loop(Red,fun() -> im_a_mapper end) end)]++Map); 
init(0,Red,Map) -> {Red,Map}.


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
	    io:format("coord init~n"),
	    %reply_ok(From),
	    Len = lists:length(Data),
	    info(Reducer,{start,RedFun, RedInit, Len}),
	    io:format("coord red~n"),
	    loop(Mappers, MapFun),
	    coordinator_loop(Reducer, Mappers);
	{From, {start, Data}} ->
	    io:format("coord start~n"),
	    send_data(Mappers, Data),
	    coordinator_loop(Reducer, Mappers);
	{From, {done, Result}} ->
	    %reply_ok(From),
	    {ok, Result}
	    

    end.

loop([Mid|Mappers], MapFun)->
    info(Mid, {init, MapFun}),
    loop(Mappers, MapFun).
loop([]) ->
    done.


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
	{From, {start, RedFun, Acc, Miss}} ->
	    io:format("reduce plo~n"),
	    reply_ok(From),
	    OverAllRes = gather_data_from_mappers(RedFun, Acc, Miss),
	    info(From, {done, OverAllRes}),
	    reducer_loop()
    end.

%%% get data from mappers
gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	stop ->
	    ok;
	{From, {result, ChunkOfData}} ->
	    Res = lists:map(Fun(ChunkOfData,Acc)),
	    gather_data_from_mappers(Fun, Res, Missing)
    end.


%%% Mapper
mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	{From, {init, NewFun}} ->
	    io:format("mapper init~n"),
	    info(From,ok),
	    mapper_loop(Reducer, NewFun);
	{From, {data, Data}} ->
	    io:format("mapper data~n"),
	    Res = lists:map(Fun, Data),
	    info(Reducer, {result, Res}),
	    mapper_loop(Reducer, Fun);
	Unknown ->
	    io:format("unknown message: ~p~n",[Unknown]), 
	    mapper_loop(Reducer, Fun)
    end.

test_sum() ->
    {ok, MR}  = mr_skel:start(3),
    {ok, Sum} = mr_skel:job(MR,
                       fun(X) -> X end,
                       fun(X,Acc) -> X+Acc end,
                       0,
                       lists:seq(1,10)),
    {ok, Fac} = mr_skel:job(MR,
                       fun(X) -> X end,
		       fun(X,Acc) -> X*Acc end,
		       1,
		       lists:seq(1,10)),
    mr_:stop(MR),
    {Sum, Fac}.

test_start() ->
    {ok, MR} = mr_skel:start(3).

test_job() ->
    {ok, MR} = mr_skel:start(3),
    io:format("~p",[MR]),
    Sum = mr_skel:job(MR,
                       fun(X) -> X end,
                       fun(X,Acc) -> X+Acc end,
                       0,
                       lists:seq(1,10)).
	
