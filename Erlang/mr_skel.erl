%%%-------------------------------------------------------------------
%%% @implementors Asger Pedersen and Kristoffer Cobley
%%% @auther Ken Friis Larsen  
%%% @copyright (C) 2011, Ken Friis Larsen
%%% Created : Oct 2011 by Ken Friis Larsen <kflarsen@diku.dk>
%%%-------------------------------------------------------------------
-module(mr_skel).

-export([start/1, stop/1, job/5, test_sum/0, test_job/0, init/1, test_fac/0,total_words/0, list_of_tracks/2, word_data/0, avg/0]).

%%% Start the MapReducer with N mappers, returns ok, Pid of the coordinator
start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.


%%% Stop the MapReducer with coordinator Pid
stop(Pid) -> 
    info(Pid,stop).
    
    
%%% Define a MapReduce job
job(CPid, MapFun, RedFun, RedInit, Data) -> 
    rpc(CPid, {init, MapFun, RedFun, RedInit, Data}),
    rpc(CPid, {start,Data}).
    

%%%% Internal implementation

%%% Make N mappers and one reducer
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
	    Len = length(Data),
	    info(Reducer,{self(),{start,RedFun, RedInit, Len, From}}),
	    loop(Mappers, MapFun),
	    reply_ok(From),
	    coordinator_loop(Reducer, Mappers);
	{From, {start, Data}} ->
	    send_data(Mappers, Data),
	    coordinator_loop(Reducer, Mappers);
	{From, {done, Result, Jid}} ->
	    reply_ok(Jid, Result),
	    coordinator_loop(Reducer, Mappers)
    end.

loop([Mid|Mappers], MapFun)->
    info(Mid, {self(), {init, MapFun}}),
    loop(Mappers, MapFun);
loop([],_) ->
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
	    reducer_loop()
    end.

%%% get data from mappers
gather_data_from_mappers(Fun, Acc, Missing) ->
    receive
	{stop_gather, Acc} ->
	    Acc;
	{From, {result, ChunkOfData}} ->
%	    io:format("chunk ~p ~n",[ChunkOfData]),
	    Res = (lists:foldl(Fun, Acc, ChunkOfData)),
	    Miss = Missing - 1,
	    if Miss >= 1 ->
		    gather_data_from_mappers(Fun, Res, Miss);
	       true -> info(self(),{stop_gather,Res})
	    end
    end.


%%% Mapper
mapper_loop(Reducer, Fun) ->
    receive
	stop -> 
	    io:format("Mapper ~p stopping~n", [self()]),
	    ok;
	{From, {init, NewFun}} ->
	    reply_ok(From),
	    mapper_loop(Reducer, NewFun);
	{data, Data} ->
	    Res = lists:map(Fun,Data),
	    info(Reducer,{self(), {result, Res}}),
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
                       []++lists:seq(1,10)),
    {ok, Fac} = mr_skel:job(MR,
                       fun(X) -> X end,
		       fun(X,Acc) -> X*Acc end,
		       1,
		       lists:seq(1,10)),
    mr_skel:stop(MR),
    {Sum, Fac}.


    

test_job() ->
    {ok, MR} = mr_skel:start(3),
    {ok, Sum} = mr_skel:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X+Acc end,
			    0,
			    lists:zipwith(fun(X, Y) -> [X*Y/Y] end, lists:seq(1,10), lists:seq(1,10))),
    mr_skel:stop(MR),
    Sum.
	
test_fac() ->
    {ok, MR} = mr_skel:start(3),
    {ok, Fac} = mr_skel:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X*Acc end,
			    1,
			    lists:seq(1,10)),
    mr_skel:stop(MR),
    Fac.


list_of_tracks([],CleanTracks)->
    CleanTracks;
list_of_tracks([H | Tail],CleanTracks) ->
    Track = read_mxm:parse_track(H),
    Words = element(3,Track),
    list_of_tracks(Tail, [Words|CleanTracks]).
    

word_data() ->
    Now = erlang:now(),
    WordData = read_mxm:from_file("mxm_dataset_test.txt"),
    Tracks = element(2,WordData),
    Res = list_of_tracks(Tracks,[]),
    Then = erlang:now(),
    Time = timer:now_diff(Then, Now),
    io:format(" time: ~p~n",[Time/1000000]),
    Res.

%%% 44846399
total_words() ->
    Now = erlang:now(),
    {ok, MR}        = mr_skel:start(25),
    {ok, NoOfWords} = mr_skel:job(MR,
				  fun(H) -> element(2,H) end,
				  fun(X,Acc)   -> X+Acc end,
				  0,
				  word_data()),
    mr_skel:stop(MR),
    Done = erlang:now(),
    Time = timer:now_diff(Done, Now),
    io:format(" time: ~p~n",[Time/1000000]),
    NoOfWords.
			
%%% Preprocessing for averages	
%%% We need the data to be in the form [[[{integer(),integer()}]], [[...]]]  
list_of_tracks2([],CleanTracks)->
    CleanTracks;
list_of_tracks2([H | Tail],CleanTracks) ->
    Track = read_mxm:parse_track(H),
    Words = element(3,Track),
    list_of_tracks2(Tail, [[Words]|CleanTracks]).
    
word_data2() ->
    Now = erlang:now(),
    WordData = read_mxm:from_file("mxm_dataset_test.txt"),
    Tracks = element(2,WordData),
    Res = list_of_tracks2(Tracks,[]),
    Then = erlang:now(),
    Time = timer:now_diff(Then, Now),
    io:format("Preprocessing time: ~p~n",[Time/1000000]),
    Res.					  


%%% To calculate the average number of different words, the mappers calculate the length of the
%%% list containing the word count. The reducer store the lengths in a tuple {Words, Songs}
%%% result 80.0210099800968 for largest dataset
avg() ->
    Data = word_data2(),
    Now = erlang:now(),
    {ok, MR}        = mr_skel:start(1),
    {ok, AvgDiffWords} = mr_skel:job(MR,
				     fun(X)  -> length(X) end,
				     fun(X, Acc) -> {Words, Songs} = Acc,
						    {Words + X, Songs +1}
				     end,
				     {0,0},
				     Data),
    {ok, AvgWords} = mr_skel:job(MR,
				 fun(X)  -> lists:foldl(fun(Y, Acc) -> element(2,Y)+Acc end, 0 ,X) end,
				 fun(X, Acc) -> {Words, Songs} = Acc,
						{Words + X, Songs +1}
				 end,
				 {0,0},
				 Data),
    mr_skel:stop(MR),
    {W, S} = AvgDiffWords,
    {Ws, Ss} = AvgWords,
    Done = erlang:now(),
    Time = timer:now_diff(Done, Now),
    io:format("MapReduce time: ~p~n",[Time/1000000]),
    {W/S, Ws/Ss}.

%% vi skal have [[trackid],[words],[trackid],[words]]
%% mapper = if [trackid] -> [trackid] else [words] -> [1|0]
%% if word is contained in song 1 else 0
%% reducer = if 1 then songid in list else trow it away

grep(Word)->
    Data = word_data2(),
    Now = erlang:now(),
    {ok, MR}        = mr_skel:start(1),
    {ok, NameOfSongs} = mr_skel:job(MR,
				     fun(X)  -> length(X) end,
				     fun(X, Acc) -> {Words, Songs} = Acc,
						    {Words + X, Songs +1}
				     end,
				     {0,0},
				     Data),
    mr_skel:stop(MR),
    Done = erlang:now(),
    Time = timer:now_diff(Done, Now),
    io:format("MapReduce time: ~p~n",[Time/1000000]),
    NameOfSongs.
