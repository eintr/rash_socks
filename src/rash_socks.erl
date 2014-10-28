-module(rash_socks).

-export([start/0]).

-import(config, [config/2]).

-define(DEFAULT_CONFIG, [{port, 10800}, {addr, "0.0.0.0"}, {admin_port, 8972}, {connect_timeout, 2000}, {shortlist_size, 256}]).

start() ->
	start(?DEFAULT_CONFIG).
start(Config) ->
	queuectl:create(Config),
	simple_tcp_server:create({config(addr, Config), config(port, Config), []}, {socks4, socks4_callback, [Config]}),
	{ok, Socket} = gen_tcp:listen(config(admin_port, Config), [inet, {active, true}, {packet, http}]),
	admin_start(Socket, Config).

admin_start(Socket, Config) ->
	admin_loop(Socket, Config).

admin_loop(Socket, Config) ->
	case gen_tcp:accept(Socket) of
		{ok, Client} ->
			Req = get_request(Client);
		_ ->
			admin_loop(Socket, Config)
	end.

get_request(Socket) -> get_request(Socket, dict:new()).
get_request(Socket, Req) ->
	receive
		{http_request } -> ok
	end.

