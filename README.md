#EUC 2014 Riak Tutorial

##Cluster set-up
The sample code requires a bucket type called 'maps' to be created for holding CRDT maps. This can be done as follows:

    $ rel/riak/bin/riak-admin bucket-type create maps '{"props":{"datatype":"map"}}'
    maps created

    $ rel/riak/bin/riak-admin bucket-type activate maps
    maps has been activated

##Compiling the example
The example requires access to the latest version of the riak-erlang-client. Once this has been compiled, the paths in the `start_shell.sh` script need to be adjusted.

Compile the `euc_tutorial.erl` file.

##Running the examples
Start an Erlang shell using the `start_shell.sh` script.

    Eshell V5.9.1  (abort with ^G)
    1> {ok, State} = euc_tutorial:init().
    {ok,{state,"127.0.0.1",8087,<<"maps">>,<<"posts">>,
               <<"thread_index">>,<<"post_index">>,<0.33.0>}}
    2> euc_tutorial:insert_post(State, <<"P1">>, <<"ciprian@basho.com">>,<<"SUBJECT">>, <<"BODY">>, {{2014,5,15},{0,0,0}}).
    ok
    3> euc_tutorial:insert_post(State, <<"P2">>, <<"bob@basho.com">>, <<"RE: SUBJECT">>, <<"RESPONSE BODY">>, {{2014,5,15},{0,0,0}}, <<"P1">>).
    ok
    4> euc_tutorial:insert_post(State, <<"P3">>, <<"bill@basho.com">>, <<"RE: SUBJECT">>, <<"RESPONSE BODY 2">>, {{2014,5,16},{0,0,0}}, <<"P1">>).
    ok
    5> euc_tutorial:insert_post(State, <<"P4">>, <<"bhunt@basho.com">>, <<"RE: SUBJECT">>, <<"RESPONSE BODY 3">>, {{2014,5,17},{0,0,0}}, <<"P2">>).
    ok
    6> euc_tutorial:get_post(State, <<"P1">>).
    {root,<<"P1">>,<<"P1">>,<<"ciprian@basho.com">>,
      <<"SUBJECT">>,<<"BODY">>,<<"2014-05-15T00:00:00.000Z">>}
    7> euc_tutorial:get_post(State, <<"P2">>).
    {response,<<"P2">>,<<"P1">>,<<"bob@basho.com">>,
          <<"RE: SUBJECT">>,<<"RESPONSE BODY">>,
          <<"2014-05-15T00:00:00.000Z">>,<<"P1">>}
    8> euc_tutorial:get_thread(State, <<"P1">>).
    [{root,<<"P1">>,<<"P1">>,<<"ciprian@basho.com">>,
       <<"SUBJECT">>,<<"BODY">>,<<"2014-05-15T00:00:00.000Z">>},
     {response,<<"P2">>,<<"bob@basho.com">>,<<"RE: SUBJECT">>,
           <<"2014-05-15T00:00:00.000Z">>,<<"P1">>},
     {response,<<"P3">>,<<"bill@basho.com">>,<<"RE: SUBJECT">>,
           <<"2014-05-16T00:00:00.000Z">>,<<"P1">>},
     {response,<<"P4">>,<<"bhunt@basho.com">>,<<"RE: SUBJECT">>,
           <<"2014-05-17T00:00:00.000Z">>,<<"P2">>}]
    9> euc_tutorial:get_threads_by_date(State, {{2014,5,15},{0,0,0}}).
    [{<<"P1">>,<<"ciprian@basho.com">>,<<"SUBJECT">>,
      <<"2014-05-15T00:00:00.000Z">>}]
    10> euc_tutorial:get_threads_by_date(State, {{2014,5,16},{0,0,0}}).
    []


