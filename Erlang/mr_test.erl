%%% The mister test for the mister map reduce

-module(mr_test).

-export([test_sum/0, test_job/0, test_fac/0, total_words/0, list_of_tracks/2, 
	 word_data/0, avg/0, run_alots/0, grep/1, rev_index/0]).

run_alots() ->
    {Sum, Fac} = test_sum(),
    Sum1 = test_job(),
    Fac1 = test_fac(),
    Res = total_words(),
    {AvgDiff, Avg} = avg(),
    
    {Sum, Fac, Sum1, Fac1, Res, AvgDiff, Avg}.
    
    

%% Test sum and fac. Notice weird data. If we just feed it a list mapper will fail. 
test_sum() ->
    {ok, MR}  = mr_skel:start(3),
    {ok, Sum} = mr_skel:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X+Acc end,
			    0,
			    lists:map(fun(X) -> [X] end, lists:seq(1,10))),
    {ok, Fac} = mr_skel:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X*Acc end,
			    1,
			    lists:map(fun(X) -> [X] end, lists:seq(1,10))),
    mr_skel:stop(MR),
    {round(Sum), round(Fac)}.


    
%% Same as above.
test_job() ->
    {ok, MR} = mr_skel:start(3),
    {ok, Sum} = mr_skel:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X+Acc end,
			    0,
			    lists:map(fun(X) -> [X] end, lists:seq(1,10))),
    mr_skel:stop(MR),
    Sum.

%% Same as above.	
test_fac() ->
    {ok, MR} = mr_skel:start(3),
    {ok, Fac} = mr_skel:job(MR,
			    fun(X) -> X end,
			    fun(X,Acc) -> X*Acc end,
			    1,
			    lists:map(fun(X) -> [X] end, lists:seq(1,10))),
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
    io:format("Preprocessing time: ~p~n",[Time/1000000]),
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
    io:format("MapReduce time: ~p~n",[Time/1000000]),
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
    {ok, MR}        = mr_skel:start(5),
    {ok, AvgDiffWords} = mr_skel:job(MR,
				     fun(X)  -> length(X) end,
				     fun(X, Acc) -> {Words, Songs} = Acc,
						    {Words + X, Songs +1}
				     end,
				     {0,0},
				     Data),
    {ok, AvgWords} = mr_skel:job(MR,
				 fun(X)  -> lists:foldl(fun(Y, Acc) -> element(2,Y)+Acc end, 
							0 ,X) 
				 end,
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


%%% Preprocessing for averages	
%%% We need the data to be in the form [[[{integer(),integer()}]], [[...]]]  
list_of_tracks3([],CleanTracks)->
    CleanTracks;
list_of_tracks3([H | Tail],CleanTracks) ->
    Track = read_mxm:parse_track(H),
    Words = element(3,Track),
    Id = element(1,Track),
    list_of_tracks3(Tail, [[{Id,Words}]|CleanTracks]).
    
word_data3(Word) ->
    Now = erlang:now(),
    WordData = read_mxm:from_file("mxm_dataset_test.txt"),
    {Words, Tracks} = WordData,
    WordsBefore = lists:takewhile(fun(X) -> not string:equal(X, Word) end, Words),
    WordIndex = length(WordsBefore) +1,
    Res = list_of_tracks3(Tracks,[]),
    Then = erlang:now(),
    Time = timer:now_diff(Then, Now),
    io:format("Preprocessing time: ~p~n",[Time/1000000]),
 %   io:format("~p ~n", [Res]),
 %   io:format("~p ~n", [WordIndex]),
    {WordIndex, Res}.					  

%% {track_id, [words]}
%% 
%% if word is contained in song 1 else 0
%% reducer = if 1 then songid in list else trow it away

grep(Word)->
    {WI, Data} = word_data3(Word),
    io:format("index: ~p~n",[WI]),
    Now = erlang:now(),
    {ok, MR}        = mr_skel:start(20),
    {ok, NameOfSongs} = mr_skel:job(MR,
				    fun(X)  -> {TrackId,Wrds} = X,
					       Hits = lists:map(fun(Y) -> 
								 if element(1,Y) == WI ->
									 1;
								    true ->
									 0
								 end	      
							 end, Wrds),
					       {TrackId,Hits}
				    end,
				    fun(X, Acc) -> {TrckId, Wrd} = X,
						   Match = lists:foldl(fun(Y, Sum) -> 
									       Y+Sum 
								       end, 
								       0, Wrd),
						   if Match == 1 ->
							   [TrckId]++Acc;
						      true ->
							   Acc
						   end
				    end,
				    [],
				     Data),
    mr_skel:stop(MR),
    Done = erlang:now(),
    Time = timer:now_diff(Done, Now),
    io:format("MapReduce time: ~p~n",[Time/1000000]),
    {length(NameOfSongs), NameOfSongs}.

rev_index() ->
    {I,Data} = word_data3("i"),
    Now = erlang:now(),
    {ok, MR}        = mr_skel:start(1),
    {ok, RevIndex} = mr_skel:job(MR,
				 fun(X)  -> {TrackId,Wrds} = X,
					    Hits = lists:map(fun(Y) -> 
								     {element(1,Y), TrackId} 
							     end, Wrds),
					    {TrackId,Hits}
				 end,
				 fun(X, Acc) -> 
					 {TrckId, Wrd} = X,
				%	 io:format("Acc ~p~n",[dict:to_list( Acc)]),
				%	 io:format("Y ~p~n",[Wrd]),
					 New = dict:from_list(Wrd),
					 Newer = dict:merge(fun(K, V1, V2) ->
								    [V1] ++ V2
							    end,
								    New,Acc),
					 Newer
				 end,
				 dict:new(),
				 Data),
    mr_skel:stop(MR),
    Done = erlang:now(),
    Time = timer:now_diff(Done, Now),
    io:format("MapReduce time: ~p~n",[Time/1000000]),
    RevIndex.	     
