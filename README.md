# ehbase 

`ehbase` is a HBase driver for Erlang via Erlang Thrift library. It is in the process of developing under branch of develop.
## summary
Below the surface, `ehbase` uses the Erlang Thrift Library to connect to HBase using its Thrift gateway, which is included in the standard HBase 0.9x releases.

In addition, `ehbase` uses the [pooler](https://github.com/seth/pooler) for connection's pool encapsulate. But I used [my forked repo](https://github.com/redink/pooler) which support more feature and do not pull request to the the original branch for some reason for now.
## example
There is one default configure in the file of `demo.config` for `pooler`, and you will also find configure about `Thrift server`'s name, host, ip, and socket params in the file of `demo.config`. Of course, you can move these configure information to your `sys.config` file, that is your right. 

### compile
`ehbase` using `rebar` manager the whole project, you can use all the command of rebar
	
	./rebar compile
and `./rebar com` will work well.
### start 
For now, you can use `erl -pa … ` command to start one Erlang Shell and `application:start/1` only

	$erl -pa ./ebin -pa ./deps/*/ebin -config demo.config
	
	1> application:start(ehbase).
	
### access hbase
First of all, you should get a `Pid` from `pooler`
	
	2> Pid_For_Use = pooler:take_member(hbase_thrift).
	
then, you can use this `Pid` named  Pid_For_Use access HBase
	
	3> gen_server:call(Pid_For_Use, {getTableNames, []}).
	
you will get all tables in the HBase, after using Pid_For_Use you should return it to the pool final
	
	4> pooler:return_member(hbase_thrift, Pid_For_Use).
	

## thanks
* Erlang Thrift Library
* pooler
