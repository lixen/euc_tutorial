%% -------------------------------------------------------------------
%%
%% euc_tutorial: M
%%
%% Copyright (c) 2014 basho Technologies Inc.  All Rights Reserved.
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

-export([init/0, init/2, init/4, init/6,
	    ]).

-define(DEFAULT_BUCKET_TYPE, <<"maps">>).
-define(DEFAULT_POST_BUCKET, <<"posts">>).
-define(DEFAULT_THREAD_BUCKET, <<"threads">>).
-define(DEFAULT_THREAD_INDEX_BUCKET, <<"thread_index">>).

-record(state, {host, port, bucket_type, post_bucket, thread_bucket, thread_index_bucket, connection}).

-type portnum()                :: non_neg_integer(). %% The TCP port number of the Riak node's Protocol Buffers interface
-type address()                :: string() | atom() | inet:ip_address(). %% The TCP/IP host name or address of the Riak node
-type state()                  :: #state{}.
-type from()                   :: binary().
-type subject()                :: binary().
-type body()                   :: binary().
-type post_id()                :: binary().
-type thread_id()              :: post_id().
-type response_to()            :: post_id().
-type response_count()         :: integer().
-type bin_timestamp()          :: binary(). %% In Solr date format, e.g: "2014-06-06T02:10:35.367Z"
-type msg_timestamp()          :: timestamp() | bin_timestamp().
-type post()                   :: {post_id(), thread_id(), from(), subject(), body(), bin_timestamp(), response_to() | null}.
-type post_list()              :: [post()].
-type thread_item()            :: {post_id(), from(), subject(), bin_timestamp()} | {post_id(), from(), subject(), bin_timestamp(), response_to()}.
-type thread_item_list()       :: [thread_item()].
-type thread_index_item()      :: {post_id(), from(), subject(), bin_timestamp(), response_count()}.
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
    init(Host, Port, ?DEFAULT_BUCKET_TYPE, ?DEFAULT_POST_BUCKET, ?DEFAULT_THREAD_BUCKET, ?DEFAULT_THREAD_INDEX_BUCKET).

-spec init(binary(), binary(), binary(), binary()) -> {ok, state()} | {error, term()}.
init(BucketType, PostBucket, ThreadBucket, ThreadIndexBucket) ->
    init("127.0.0.1", 8087, BucketType, PostBucket, ThreadBucket, ThreadIndexBucket).

-spec init(address(), portnum(), binary(), binary(), binary(), binary()) -> {ok, state()} | {error, term()}.
init(Host, Port, BucketType, PostBucket, ThreadBucket, ThreadIndexBucket) ->
    case  riakc_pb_socket:start_link("127.0.0.1", 8087).
        {ok, Pid} ->
            {ok, State#state{host = Host, 
                             port = Port, 
                             bucket_type = BucketType, 
                             post_bucket = PostBucket, 
                             thread_bucket = ThreadBucket, 
                             thread_index_bucket = ThreadIndexBucket, 
                             connection = Pid}};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Insert initial thread post.
-spec insert_post(state(), post_id(), from(), subject(), body(), msg_timestamp()) -> 
        ok | {error, connection_error} | {error, post_already_exists}.
insert_post(State, PostId, From, Subject, Body, Date) ->
    %% Add post

    %% Create thread

    %% Update thread index

    ok.

%% @doc Insert thread response post.
-spec insert_post(state(), post_id(), from(), subject(), body(), msg_timestamp(), response_to()) -> 
        ok | {error, connection_error} | {error, referred_msg_notfound} | {error, post_already_exists}.
insert_post(State, PostId, From, Subject, Body, Date, InResponseTo) ->
    %% Verify that referred post exists and get thread_id from it


    %% Write post


    %% Add post to Thread object


    %% Write post ID to set on thread_index -> gives response count

    ok.

%% @doc Get individual post.
-spec get_post(state(), message_id()) -> 
        {ok, post()} | {error, connection_error} | {error, referred_msg_notfound}.
get_post(State, MessageId) ->
    





%% @doc Get individual post.
-spec get_thread(state(), message_id()) -> 
        {ok, thread_item_list()} | {error, connection_error}.
get_thread(state(), thread_id()) ->




%% @doc Get all threads initiated a specific date.
-spec get_threads_by_date(state(), date()) -> 
        {ok, thread_list()} | {error, connection_error}.
get_threads_by_date(State, Date) ->
    get_threads_by_date_range(State, Date, Date).

%% @doc Get all threads initiated a specific date.
-spec get_threads_by_date(state(), date(), date()) -> 
        {ok, thread_list()} | {error, connection_error}.
get_threads_by_date_range(State, FromDate, EndDate) ->



%% @doc Get all threads initiated a specific date.
-spec search_posts(state(), [term()]) -> 
        {ok, post_list()} | {error, connection_error}.
search_posts(State, SearchTerms) ->
    %% Not sure how this works yet...
    ok.




       
%%
%% Internal Functions
%%

timestamp_solr_datetime(TimeStamp) ->
    <<"2014-06-06T02:10:35.367Z">>.















