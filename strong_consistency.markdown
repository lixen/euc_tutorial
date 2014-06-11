### Setup

``` 
./dev/dev1/bin/riak-admin bucket-type create strongly_consistent '{"props":{"consistent":true}}'
./dev/dev1/bin/riak-admin bucket-type activate strongly_consistent
./dev/dev1/bin/riak-admin bucket-type status strongly_consistent                                
```

### Putting, successfully

```
basho-catah% curl -i -XPUT -H 'content-type: application/json' 'http://localhost:8098/types/strongly_consistent/buckets/people/keys/drew19' -d '{"name": "bryan", "middle":"jude", "surname": "hunt"}'
HTTP/1.1 204 No Content
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Wed, 11 Jun 2014 13:18:23 GMT
Content-Type: application/json
Content-Length: 0
```
### Putting, but it conflicts

```
basho-catah% curl -i -XPUT -H 'content-type: application/json' 'http://localhost:8098/types/strongly_consistent/buckets/people/keys/drew19' -d '{"name": "bryan", "middle":"jude", "surname": "hunt"}'
HTTP/1.1 412 Precondition Failed
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Wed, 11 Jun 2014 13:18:25 GMT
Content-Type: text/html
Content-Length: 180

<html><head><title>412 Precondition Failed</title></head><body><h1>Precondition Failed</h1>Precondition Failed<p><hr><address>mochiweb+webmachine web server</address></body></html>%
```

### Deleting

```
basho-catah% curl -i -XDELETE 'http://localhost:8098/types/strongly_consistent/buckets/people/keys/drew9'
HTTP/1.1 204 No Content
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Wed, 11 Jun 2014 13:18:48 GMT
Content-Type: application/json
Content-Length: 0
```
### Try again, not found

```
basho-catah% curl -i -XDELETE 'http://localhost:8098/types/strongly_consistent/buckets/people/keys/drew9'
HTTP/1.1 404 Object Not Found
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Wed, 11 Jun 2014 13:18:50 GMT
Content-Type: text/plain
Content-Length: 10
```

```
basho-catah% curl -i -XPUT -H 'content-type: application/json' 'http://localhost:8098/types/strongly_consistent/buckets/people/keys/drew19' -d '{"name": "bryan", "middle":"jude", "surname": "hunt"}'
HTTP/1.1 412 Precondition Failed
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Wed, 11 Jun 2014 13:23:47 GMT
Content-Type: text/html
Content-Length: 180

<html><head><title>412 Precondition Failed</title></head><body><h1>Precondition Failed</h1>Precondition Failed<p><hr><address>mochiweb+webmachine web server</address></body></html>%
```

```
basho-catah% curl -i -XPUT -H 'content-type: application/json' 'http://localhost:8098/types/strongly_consistent/buckets/people/keys/drew191' -d '{"name": "bryan", "middle":"jude", "surname": "hunt"}'
HTTP/1.1 204 No Content
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.10.5 (jokes are better explained)
Date: Wed, 11 Jun 2014 13:23:53 GMT
Content-Type: application/json
Content-Length: 0
```
