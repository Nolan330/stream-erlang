-module(stream_server).

-export([init/0, init/2, think/1, remember/1, membe/1]).
-export([generate_thoughts/1, generate_thought/1]).

-include("./stream_mnesia.hrl").
-include_lib("stdlib/include/qlc.hrl").

init(Req, Opts) ->
    {ok, HttpBody, _} = cowboy_req:body_qs(Req),
    io:fwrite("\nData---\n~p\n---Data\n", [HttpBody]),

    {_, SmsBody} = proplists:lookup(<<"Body">>, HttpBody),
    io:fwrite("\nData---\n~s\n---Data\n", [SmsBody]),

    ThoughtData = parse_to_proplist(binary_to_list(SmsBody), ";", ":"),
    io:fwrite("\nData---\n~p\n---Data\n", [ThoughtData]),

    %%Thought = proplist_to_thought(ThoughtData),
    %% Thought = parse_thought(SmsBody)
    %% Think(Thought)

    %% handle(Body)
    %% can never explicitly define time, source, (vsn?)
    %%
    %% minimum case: send only thought body.
    %%  text: body
    %%  time: ingesttime
    %%  geo: {latlong, {Lat, Long}} | {zip, Zip} | {city, City} | {state, State} | {geo, Geo}
    %%  insp: []
    %%  tags: []
    %%  source: twilio -- need to determine type (raw erlang vs. web vs. twilio vs. ...?)
    %%  vsn: ??
    %%
    %% dialog:
    %%  want to add tags? <show relevant existing tags> -> no | tags (list of atoms)
    %%  using <geo> as geo, wanna change that? -> timeout | no | geo
    %%  what inspired you?
    %%
    %% maximum case: send delimited
    %%  text: ...&
    %%  geo: type, Value&
    %%  insp: ...&
    %%  tags: ...&
    %%  vsn: ??

    StreamReq = cowboy_req:reply(200,
                                 [{<<"content-type">>, <<"text/plain">>}],
                                 "K I shat {" ++ io_lib:format("~p", [ThoughtData]) ++ "} into your database.",
                                 Req),
    {ok, StreamReq, Opts}.


%% generic function to handle tokenizing/processing nested delimited strings
parse_to_proplist(Body, OuterToken, InnerToken) ->
    lists:filtermap(
        fun (KV) ->
            case string:tokens(KV, InnerToken) of
                [Key, Value] -> {true, {erlang:list_to_atom(string:strip(Key)), string:strip(Value)}};
                _ -> false
            end
        end,
        string:tokens(Body, OuterToken)).


%% initializes a new node
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(thought,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, thought)}]).

%% debug functions - tmp
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
%% password/auth? check FromNumber?
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
