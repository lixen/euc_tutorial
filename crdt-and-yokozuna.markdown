
noglob curl -XPUT -H 'Content-Type: application/xml'  http://localhost:8098/search/schema/post_schema --data-binary @/common/euc_tutorial/post_schema.xml
curl -i -XPUT http://localhost:8098/search/index/post_index -H 'content-type: application/json' -d '{"schema":"post_schema"}'


dev/dev1/bin/riak-admin bucket-type create maps '{"props":{"datatype":"map", "search_index":"post_index" }}'
dev/dev1/bin/riak-admin bucket-type activate maps
curl -X POST localhost:8098/types/maps/buckets/fooz/datatypes/baz -H"content-type: application/json"  -d '{"update":{"gold_counter":100, "blah_counter": 1, "stone_counter": 50, "foo_set" : { "add_all" : [ "1" ] }}}'



```
./euc_tutorial

1: l(euc_tutorial)
-> {module,euc_tutorial}
2: {ok,State} = euc_tutorial:init()
-> {ok,{state,"127.0.0.1",8087,<<"maps">>,<<"posts">>,
              <<"thread_index">>,<<"post_index">>,<0.35.0>}}
3> euc_tutorial:insert_post(State, <<"11.32">>, <<"11.32@basho.com">>,<<"a subject">>, <<"a body of text adsds">>, {{2014,5,15},{0,0,0}}).
ok
```

Get the id’s of all the items in the ‘posts' bucket

curl -X GET "localhost:8098/types/maps/buckets/posts/keys?keys=true”

Path must be correct

This produces a binary blob
% curl -X GET "localhost:8098/types/maps/buckets/posts/keys/P1"

This gives me the nice JSON representation

% curl -X GET "localhost:8098/types/maps/buckets/posts/datatypes/P1”

Create a default index on posts.

curl -i -XPUT http://localhost:8098/search/index/posts

curl http://localhost:8098/search/schema/post_schema

curl -i -XPUT http://localhost:8098/search/index/post_index -H 'content-type: application/json' -d '{"schema":"post_schema"}'
curl -i http://localhost:8098/search/index/post_index
dev/dev1/bin/riak-admin bucket-type update maps '{"props": { "search_index":"post_index" } }'           
curl 'http://localhost:8098/search/query/post_index?q=*:*'
