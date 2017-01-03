-module(stream_server).

-export([start/0]). %% stream
-export([init/2]).  %% cowboy
-export([create_schema/0, create_table/0]). %% mnesia
-export([think/1, remember/1, membe/1]).
-export([generate_thoughts/1, generate_thought/1]).

-include("./stream_mnesia.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% cowboy request handler
init(Req, Opts) ->
    Thought = req_to_thought(Req), io:fwrite("\n~p\n", [Thought]),
    think(Thought),

    %% dialog:
    %%  want to add tags? these look eh: <show relevant existing tags> -> no | tags (list of atoms)
    %%  tags can be added using a tagging process
    %%  tagging process would be responsible for processing previous thoughts:
    %%  1. apply tfidf on all thoughts
    %%  2. select top n thoughts.tags above threshhold
    %%  3. list tags in a response text
    %%  4. allow an symmetrically delimited sms indicating yes or no for each
    %%      a. <tag1>, <tag2>, ..., <tagn>
    %%      b. yes, no, yes, ..., yes
    %%      c. 1,0,1,0,1,0,1, ....,0
    %%      d. 10110101101.......10

    StreamReq = cowboy_req:reply(200,
                                 [{<<"content-type">>, <<"text/plain">>}],
                                 "K I shat your thought into your database.",
                                 Req),
    {ok, StreamReq, Opts}.


%% remember dialog ?

%% parse a Twilio SMS request to a thought
req_to_thought(Req) ->
    {ok, ReqBody, _}    = cowboy_req:body_qs(Req),
    ReqHeaders          = cowboy_req:headers(Req),
    SmsBody             = string:to_lower(binary_to_list(proplists:get_value(<<"Body">>, ReqBody))),
    SmsProps            = sms_to_props(SmsBody),
    #uthought{
        text    = proplists:get_value(thought, SmsProps),
        time    = calendar:now_to_universal_time(erlang:timestamp()),
        geo     = smsprops_to_geo(SmsProps),
        insp    = proplists:get_value(insp, SmsProps),
        tags    = smsprops_to_tags(SmsProps),
        source  = req_to_source(ReqHeaders, ReqBody),
        vsn     = 1}.

sms_to_props(SmsBody) ->
    OuterDelim = ".",
    InnerDelim = ":",
    case string:str(SmsBody, OuterDelim) of
        0 -> [{thought, SmsBody}];
        _ -> [{thought, string:substr(SmsBody, 1, string:cspan(SmsBody, OuterDelim))}] ++
             parse_to_proplist(SmsBody, OuterDelim, InnerDelim)
    end.

%% generic function to handle processing nested delimited strings
parse_to_proplist(Text, OuterDelim, InnerDelim) ->
    lists:filtermap(
        fun (KVText) ->
            case string:tokens(KVText, InnerDelim) of
                [Key, Value] -> {true, {erlang:list_to_atom(string:strip(Key)), string:strip(Value)}};
                _            -> false
            end
        end,
        string:tokens(Text, OuterDelim)).

%% clean up latlong format copied from iPhone compass app
smsprops_to_geo(SmsProps) ->
    case proplists:is_defined(geo, SmsProps) andalso
         string:str(proplists:get_value(geo, SmsProps), "°") > 0 of
        true    -> geoprop_to_term(proplists:get_value(geo, SmsProps));
        false   -> undefined
    end.

geoprop_to_term(GeoProp) ->
    Geo = re:replace(GeoProp, "[^A-Za-z0-9]+", ",", [global, {return, list}]),
    case [string:strip(X) || X <- string:tokens(Geo, ",")] of
        [D1,M1,S1,Dir1,D2,M2,S2,Dir2] -> {latlong, {{D1,M1,S1,Dir1},{D2,M2,S2,Dir2}}}
    end.

%% parse a comma separated list of tags
smsprops_to_tags(SmsProps) ->
    case proplists:is_defined(tags, SmsProps) of
        true    -> [string:strip(Tag) || Tag <- string:tokens(proplists:get_value(tags, SmsProps), ",")];
        false   -> undefined
    end.

%% extract the source from the req
req_to_source(ReqHeaders, ReqBody) ->
    UserAgent = erlang:list_to_atom(string:to_lower(binary_to_list(proplists:get_value(<<"user-agent">>, ReqHeaders)))),
    case UserAgent of
        'twilioproxy/1.1'   -> {twilio, [{binary_to_list(A), binary_to_list(B)} || {A, B} <-
                                    [proplists:lookup(<<"From">>, ReqBody),
                                     proplists:lookup(<<"MessageSid">>, ReqBody),
                                     proplists:lookup(<<"AccountSid">>, ReqBody)]]};
        _                   -> {http}
    end.


%% initializes a new node
start() ->
    mnesia:stop(),
    application:set_env(mnesia, dir, "/Users/Nolan/mnesia/stream/"),
    mnesia:start().

%% common creates for when needed
%% unneeded for boot sequence
create_schema() ->
    mnesia:create_schema([node()]).

create_table() ->
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
              source = {http},
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
sign_thought({uthought, Text, Time, Geo, Insp, Tags, Source, 1}) ->
    #thought{text = Text,
             time = Time,
             geo = Geo,
             insp = Insp,
             tags = Tags,
             source = Source,
             vsn = 1,
             sig = crypto:hash(sha256, term_to_binary({Text, Time, Geo, Insp, Tags, Source, 1})),
             score = 1}.

%% increases the score of each Thought and returns all incremented Thoughts
increment_score([]) -> [];
increment_score([Thought|Rest]) ->
	Inc = Thought#thought{score = Thought#thought.score + 1},
	mnesia:write(Inc),
	[Inc|increment_score(Rest)].
