-module(stream_server).

-export([init/0, init/2, think/1, remember/1, membe/1]).
-export([generate_thoughts/1, generate_thought/1]).

-include("stream_mnesia.hrl").
-include_lib("stdlib/include/qlc.hrl").

init(Req, Opts) ->
    {ok, StreamBody, _} = cowboy_req:body(Req),
    Twilio = parse_body(binary_to_list(StreamBody)),
    %%{_, Thought} = proplists:lookup("Body", Twilio),
    %%{_, FromNumber} = proplists:lookup("From", Twilio),
    io:fwrite("Thought-----\n\t~p\n-----Thought\n", [Twilio]),
    StreamReq = cowboy_req:reply(200,
                                 [{<<"content-type">>, <<"text/plain">>}],
                                 io_lib:format("~p",[Twilio]),
                                 Req),
    {ok, StreamReq, Opts}.

parse_body(Body) ->
    lists:filtermap(
        fun (Prop) -> parse_prop(Prop) end,
        string:tokens(Body, "&")).

parse_prop(Prop) ->
    case string:tokens(Prop, "=") of
        [Name, Value] -> {true, {erlang:list_to_atom(Name), Value}};
        _ -> false
    end.

%% initializes a new node
init() ->
    mnesia:create_table(thought,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, thought)}]),
    mnesia:wait_for_tables([thought], 5000).

generate_thoughts(N) ->
    [generate_thought(Nt) || Nt <- lists:seq(1, N)].

generate_thought(N) ->
    #uthought{text = "I am thought " ++ erlang:integer_to_list(N),
			  time = calendar:now_to_universal_time(erlang:timestamp()),
			  geo = "The Dungeon",
			  insp = "a rather uninspiring thought",
			  tags = ["erlang", "stream"],
			  vsn = 1}.

%% TODO:
%% web server + sms
%% password/auth
%% blob storage
%% better integrated/orthogonal filtering
%% general orthogonality of operations
%% remember({related, ...}).
%% analytics

%% think
%%
%% stores a thought.
think(Thought) ->
    R = sign_thought(Thought),
    finalize(fun() -> mnesia:write(R) end).


%% remember
%%
%% tries to retrieve a thought given the Hint.
%% TODO: increment count
remember(all) ->
	do(qlc:q([Thought || Thought <- mnesia:table(thought)]));

remember({textContains, T}) ->
	do_then(qlc:q([Thought || Thought <- mnesia:table(thought),
				  string:str(Thought#thought.text, T) > 0]),
			fun(Ts) -> increment_score(Ts) end).

%remember({timeRange, R}) -> ;
%remember({geoRange, R}) -> ;
%remember({inspContains, T}) -> ;
%remember({tagsContains, T}) -> ;
%remember({vsn, V}) -> ;
%remember({sig, Sig}) -> ;


%% membe
%%
%% serves up N random thoughts
membe(N) -> [N].


%% performs a DB op defined by Q
do(Q) ->
	F = fun() ->
			qlc:e(Q)
		end,
	finalize(F).

%% performs a DB op defined by Q, followed by a task Fun
do_then(Q, Fun) ->
	F = fun() ->
			Ts = qlc:e(Q),
			Fun(Ts)
		end,
	finalize(F).

%% finalizes a transaction
finalize(F) ->
	{atomic, Val} = mnesia:transaction(F),
	Val.

%% returns a signature for the input thought.
sign_thought({uthought, Text, Time, Geo, Insp, Tags, 1}) ->
    #thought{text = Text,
             time = Time,
             geo = Geo,
             insp = Insp,
             tags = Tags,
             vsn = 1,
             sig = crypto:hash(sha256, term_to_binary({Text, Time, Geo, Insp, Tags, 1})),
             score = 1}.

%% increases the score of each Thought and returns all incremented Thoughts
increment_score([]) -> [];
increment_score([Thought|Rest]) ->
	Inc = Thought#thought{score = Thought#thought.score + 1},
	mnesia:write(Inc),
	[Inc|increment_score(Rest)].
