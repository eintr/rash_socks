-module(rash_socks).

-export([start/0]).

-define(DEFAULT_CONFIG, [{port, 10800}, {addr, "0.0.0.0"}, {connect_timeout, 2000}, {shortlist_size, 256}]).

start() ->
	start(?DEFAULT_CONFIG).
start(Config) ->
	queuectl:create(Config),
	{port, Port} = lists:keyfind(port, 1, Config),
	{addr, Addr} = lists:keyfind(addr, 1, Config),
	Pid = simple_tcp_server:create({Addr, Port, []}, {socks4, socks4_callback, [Config]}),
	loop(Pid).

loop(Pid) ->
	receive
		_ ->
			loop(Pid)
	end.

