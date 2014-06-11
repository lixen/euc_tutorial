
### Check all member status

```
for i in dev*/bin/riak-admin ; do $i member-status; done 
```

### Check the effective config for search
```
for i in ./dev/dev[1-5]/bin/riak(:h) ; do $i/riak config effective | grep search; done
```

### Update a bucket-types config

```
for i in dev*/bin/riak-admin ; do $i member-status; done 
```

### Turn on strong consistency 

```
sed -i'' -e '/strong_consistency = /{s/.*/strong_consistency = on/;}' ./**/etc/riak.conf
```

### Turn on search

```
sed -i'' -e '/search = off/{s/.*/search = on/;}' ./**/etc/riak.conf
```
