%% -------------------------------------------------------------------
%%
%% euc_tutorial: 
%%
%% Copyright (c) 2014 Basho Technologies Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License. You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(euc_tutorial).

-export([init/0, init/2, init/3, init/5,
	    insert_post/6, insert_post/7,
	    get_post/2,
        get_thread/2,
        get_threads_by_date/2,
        get_threads_by_date_range/3]).

-define(DEFAULT_BUCKET_TYPE, <<"maps">>).
-define(DEFAULT_POST_BUCKET, <<"posts">>).
-define(DEFAULT_THREAD_INDEX_BUCKET, <<"thread_index">>).

-record(state, {host, port, bucket_type, post_bucket, thread_index_bucket, pid}).

-type portnum()                :: non_neg_integer(). %% The TCP port number of the Riak node's Protocol Buffers interface
-type address()                :: string() | atom() | inet:ip_address(). %% The TCP/IP host name or address of the Riak node
-type state()                  :: #state{}.
-type from()                   :: binary().
-type subject()                :: binary().
-type body()                   :: binary().
-type post_id()                :: binary().
-type thread_id()              :: post_id().
-type response_to()            :: post_id().
-type bin_timestamp()          :: binary(). %% In Solr date format, e.g: "2014-06-06T02:10:35.367Z"
-type msg_timestamp()          :: calendar:datetime() | bin_timestamp().
-type post()                   :: {root, post_id(), thread_id(), from(), subject(), body(), bin_timestamp()} | {response, post_id(), thread_id(), from(), subject(), body(), bin_timestamp(), response_to()}.
-type post_list()              :: [post()].
-type thread_item()            :: {root, post_id(), from(), subject(), body(), bin_timestamp()} | {response, post_id(), from(), subject(), bin_timestamp(), response_to()}.
-type thread_item_list()       :: [thread_item()].
-type thread_index_item()      :: {post_id(), from(), subject(), bin_timestamp()}.
-type thread_index_item_list() :: [thread_index_item()].

%%
%% API Functions
%%

%% @doc Initiate state and create connection to server.
%%      Will return {error, term()} if not able to connect to the server.
-spec init() -> {ok, state()} | {error, term()}.
init() ->
    init("127.0.0.1", 8087).

-spec init(address(), portnum()) -> {ok, state()} | {error, term()}.
init(Host, Port) ->
    init(Host, Port, ?DEFAULT_BUCKET_TYPE, ?DEFAULT_POST_BUCKET, ?DEFAULT_THREAD_INDEX_BUCKET).

-spec init(binary(), binary(), binary()) -> {ok, state()} | {error, term()}.
init(BucketType, PostBucket, ThreadIndexBucket) ->
    init("127.0.0.1", 8087, BucketType, PostBucket, ThreadIndexBucket).

-spec init(address(), portnum(), binary(), binary(), binary()) -> {ok, state()} | {error, term()}.
init(Host, Port, BucketType, PostBucket, ThreadIndexBucket) ->
    case  riakc_pb_socket:start_link("127.0.0.1", 8087) of
        {ok, Pid} ->
            {ok, #state{host = Host, 
                        port = Port, 
                        bucket_type = BucketType, 
                        post_bucket = PostBucket, 
                        thread_index_bucket = ThreadIndexBucket, 
                        pid = Pid}};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Insert initial thread post.
-spec insert_post(state(), post_id(), from(), subject(), body(), msg_timestamp()) -> 
        ok | {error, term()}.
insert_post(State, PostId, From, Subject, Body, Date) when is_tuple(Date) ->
    insert_post(State, PostId, From, Subject, Body, timestamp_to_solr_datetime(Date));

insert_post(State, PostId, From, Subject, Body, Date) when is_binary(Date) ->
    %% Add post
    case update_post(State, PostId, From, Subject, Body, Date) of
        {error, Error} ->
            {error, Error};
        _ ->
            update_thread_index(State, PostId, From, Subject, Date)
    end.

%% @doc Insert thread response post.
-spec insert_post(state(), post_id(), from(), subject(), body(), msg_timestamp(), response_to()) -> 
        ok | {error, term()} | {error, referred_msg_notfound}.
insert_post(State, PostId, From, Subject, Body, Date, InResponseTo) when is_tuple(Date) ->
    insert_post(State, PostId, From, Subject, Body, timestamp_to_solr_datetime(Date), InResponseTo);

insert_post(State, PostId, From, Subject, Body, Date, InResponseTo) when is_binary(Date) ->
    %% Verify that referred post exists and get thread_id from it
    case get_post_thread_id(State, InResponseTo) of
            {error, notfound} ->
                {error, referred_msg_notfound};
            {error, Error} ->
                {error, Error};
            TID ->
                case update_post(State, PostId, From, Subject, Body, Date, InResponseTo, TID) of
                    {error, Error} ->
                        {error, Error};
                    _ ->
                        update_thread_root(State, TID, PostId, From, Subject, Date, InResponseTo)
                end
    end.

%% @doc Get individual post.
-spec get_post(state(), post_id()) -> 
        {ok, post()} | {error, term()} | {error, notfound}.
get_post(#state{pid = Pid, bucket_type = T, post_bucket = B}, PostId) ->
    case riakc_pb_socket:fetch_type(Pid, {T, B}, PostId) of
        {ok, O} ->
            V = riakc_map:value(O),
            case proplists:get_value({<<"thread_root">>, register}, V) of
            	<<"true">> ->
                    {root,
                     PostId,
                     proplists:get_value({<<"thread_id">>, register}, V),
                     proplists:get_value({<<"from">>, register}, V),
                     proplists:get_value({<<"subject">>, register}, V),
                     proplists:get_value({<<"body">>, register}, V),
                     proplists:get_value({<<"date">>, register}, V)
                     };
            	<<"false">> ->
                    {response,
                     PostId,
                     proplists:get_value({<<"thread_id">>, register}, V),
                     proplists:get_value({<<"from">>, register}, V),
                     proplists:get_value({<<"subject">>, register}, V),
                     proplists:get_value({<<"body">>, register}, V),
                     proplists:get_value({<<"date">>, register}, V),
                     proplists:get_value({<<"in_response_to">>, register}, V)
                    };
            	_ ->
            	    {error, incomplete_record}
            end;
        {error, {notfound,map}} ->
            {error, notfound};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Get individual post.
-spec get_thread(state(), post_id()) -> 
        {ok, thread_item_list()} | {error, notfound} | {error, term()}.
get_thread(#state{pid = Pid, bucket_type = T, post_bucket = B}, ThreadId) ->
    case riakc_pb_socket:fetch_type(Pid, {T, B}, ThreadId) of
        {ok, O} ->
            V = riakc_map:value(O),
            case proplists:get_value({<<"thread_root">>, register}, V) of
            	<<"true">> ->
                    R = [{root,
                         ThreadId,
                         proplists:get_value({<<"thread_id">>, register}, V),
                         proplists:get_value({<<"from">>, register}, V),
                         proplists:get_value({<<"subject">>, register}, V),
                         proplists:get_value({<<"body">>, register}, V),
                         proplists:get_value({<<"date">>, register}, V)
                         }],
                    L =  format_responses(V),
                    R ++ L;
            	_ ->
            	    {error, notfound}
            end;
        {error, {notfound,map}} ->
            {error, notfound};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Get all threads initiated a specific date.
-spec get_threads_by_date(state(), calendar:datetime()) -> 
        {ok, thread_index_item_list()} | {error, term()}.
get_threads_by_date(#state{pid = Pid, bucket_type = T, thread_index_bucket = B}, {{Y, M, D}, _Time}) ->
    Date = list_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B",
        [Y, M, D])),
    Month = binary:part(Date, 0, 7),
    case riakc_pb_socket:fetch_type(Pid, {T, B}, Month) of
        {ok, O} ->
            V = format_thread_indexes(riakc_map:value(O)),
            [{P, F, S, X} || {P, F, S, X} <- V, is_match(X, Date)];
        {error, {notfound,map}} ->
            {ok, []};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Get all threads initiated a specific date.
-spec get_threads_by_date_range(state(), calendar:datetime(), calendar:datetime()) -> 
        {ok, thread_index_item_list()} | {error, term()}.
get_threads_by_date_range(_State, _FromDate, _EndDate) ->
    





    {ok, []}.






%% @doc Get all threads initiated a specific date.
-spec search_posts(state(), [term()]) -> 
        {ok, post_list()} | {error, connection_error}.
search_posts(_State, _SearchTerms) ->
    %% Not sure how this works yet...
    {ok, []}.



    

%%
%% Internal Functions
%%
is_match(V, P) ->
    case binary:matches(V, P) of
        [] ->
            false;
        _ ->
            true
    end.

format_responses(V) ->
    case proplists:get_value({<<"responses">>,map}, V) of
        undefined ->
            [];
        L ->
            format_responses([], L)
    end. 

format_responses(List, []) ->
    List;
format_responses(List, [{{PostId, map}, V} | Rest]) ->
    R = [{response,
         PostId,
         proplists:get_value({<<"from">>, register}, V),
         proplists:get_value({<<"subject">>, register}, V),
         proplists:get_value({<<"date">>, register}, V),
         proplists:get_value({<<"in_response_to">>, register}, V)}],
    format_responses(R ++ List, Rest).

format_thread_indexes(V) ->
    format_thread_indexes([], V). 

format_thread_indexes(List, []) ->
    List;
format_thread_indexes(List, [{{PostId, map}, V} | Rest]) ->
    R = [{PostId,
          proplists:get_value({<<"from">>, register}, V),
          proplists:get_value({<<"subject">>, register}, V),
          proplists:get_value({<<"date">>, register}, V)}],
    format_thread_indexes(R ++ List, Rest).

get_post_thread_id(#state{pid = Pid, bucket_type = T, post_bucket = B}, PostId) ->
    case riakc_pb_socket:fetch_type(Pid, {T, B}, PostId) of
        {ok, O} ->
            case proplists:get_value({<<"thread_id">>, register}, riakc_map:value(O), undefined) of
                undefined ->
                    {error, no_thread_id};
                TID ->
                    TID
            end;
        {error, {notfound,map}} ->
            {error, notfound};
        {error, Error} ->
            {error, Error}
    end.

update_post(State, PostId, From, Subject, Body, Date) ->
    update_post(State, PostId, From, Subject, Body, Date, null, null).

update_post(#state{pid = Pid, bucket_type = T, post_bucket = B}, PostId, From, Subject, Body, Date, InResponseTo, ThreadId) ->
    Common = [{<<"from">>, From},
              {<<"date">>, Date},
              {<<"subject">>, Subject},
              {<<"body">>, Body}],
    Special = case {InResponseTo, ThreadId} of
    	{null, null} ->
            [{<<"thread_id">>, PostId},
             {<<"thread_root">>, <<"true">>}];
    	_ ->
    	    [{<<"thread_id">>, ThreadId},
    	     {<<"in_response_to">>, InResponseTo},
             {<<"thread_root">>, <<"false">>}]
    end,
    Map = set_map_registers(riakc_map:new(), Common ++ Special),
    case riakc_pb_socket:update_type(Pid, {T, B}, PostId, riakc_map:to_op(Map)) of
        {error, Error} ->
            {error, Error};
        _ ->
            ok
    end.

set_map_registers(Map, []) ->
    Map;
set_map_registers(Map, [{K, V} | Rest]) ->
    NewMap = riakc_map:update({K, register},
                 fun(R) -> riakc_register:set(V, R) end,
                 Map),
    set_map_registers(NewMap, Rest).

update_thread_root(#state{pid = Pid, bucket_type = T, post_bucket = B}, ThreadId, PostId, From, Subject, Date, InResponseTo) ->
    Map = set_thread_root_response_field(riakc_map:new(), 
    	                                 PostId,
    	                                 [{<<"from">>, From},
    	                                  {<<"subject">>, Subject},
    	                                  {<<"date">>, Date},
    	                                  {<<"in_response_to">>, InResponseTo}]),
    case riakc_pb_socket:update_type(Pid, {T, B}, ThreadId, riakc_map:to_op(Map)) of
        {error, Error} ->
            {error, Error};
        _ ->
            ok
    end.

set_thread_root_response_field(Map, _PostId, []) ->
    Map;

set_thread_root_response_field(Map, PostId, [{Field, Value} | R]) ->
    M = riakc_map:update(
        {<<"responses">>, map},
        fun(A) -> riakc_map:update(
            {PostId, map}, 
            fun(B) -> riakc_map:update(
        	    {Field, register},
        	    fun(C) ->
        	        riakc_register:set(Value, C) end, B) end, A) end,
        Map),
    set_thread_root_response_field(M, PostId, R).


update_thread_index(#state{pid = Pid, bucket_type = T, thread_index_bucket = B}, PostId, From, Subject, Date) ->
    Key = binary:part(Date, 0, 7),
    F = [{<<"from">>, From},{<<"subject">>, Subject},{<<"date">>, Date}],
    Map = set_thread_index_fields(riakc_map:new(), PostId, F),
    case riakc_pb_socket:update_type(Pid, {T, B}, Key, riakc_map:to_op(Map)) of
        {error, Error} ->
            {error, Error};
        _ ->
            ok
    end.

set_thread_index_fields(Map, _PostId, []) ->
    Map;
set_thread_index_fields(Map, PostId, [{Field, Value} | R]) ->
    M = riakc_map:update(
            {PostId, map}, 
            fun(B) -> riakc_map:update(
        	    {Field, register},
        	    fun(C) ->
        	        riakc_register:set(Value, C) end, B) end,
        Map),
    set_thread_index_fields(M, PostId, R).

timestamp_to_solr_datetime(DateTime) ->
    %% Format into <<"2014-06-06T02:10:35.367Z">>
    {{Year,Month,Day},{Hour,Min,Sec}} = DateTime,
    list_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.000Z",
        [Year, Month, Day, Hour, Min, Sec])).
















