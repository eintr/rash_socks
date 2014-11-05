-module(rash_socks).

-export([start/0, start/1]).

-import(config, [config/2]).
-import(log, [log/2, log/3]).

start() ->
	start(config:default()).
start(ConfigFile) ->
	main(config:load_conf(ConfigFile)).

main(Config) ->
	log:create(Config),
	queuectl:create(Config),
	simple_tcp_server:create({config(addr, Config), config(port, Config), []}, {socks4, socks4_callback, [Config]}),
	{ok, Socket} = gen_tcp:listen(config(admin_port, Config), [inet, {active, true}, {packet, http}, {reuseaddr, true}]),
	admin_start(Socket, Config).

admin_start(Socket, Config) ->
	admin_loop(Socket, Config).

admin_loop(Socket, Config) ->
	case gen_tcp:accept(Socket) of
		{ok, Client} ->
			log(log_info, "Admin client = ~p\n", [inet:peername(Client)]),
			case get_request(Client) of
				{ok, Req} ->
					gen_tcp:send(Socket, admin_process(Req)),
					admin_loop(Socket, Config);
				{error, Reason} ->
					log(log_info, "Error: ~s\n", [Reason]),
					admin_loop(Socket, Config)
			end;
		_ ->
			admin_loop(Socket, Config)
	end.

get_request(Socket) -> get_request(Socket, {method, uri, dict:new()}).
get_request(Socket, {Method, Uri, Headers}) ->
	receive
		{http, Socket, {http_request, M, {abs_path, U}, _}} ->
			get_request(Socket, {M, U, Headers});
		{http, Socket, {http_header, _, Key, _, Value}} ->
			get_request(Socket, {Method, Uri, dict:store(Key, Value, Headers)});
		{http, Socket, {http_error, Str}} ->
			{error, Str};
		{http, Socket, http_eoh} ->
			{ok, {Method, Uri, Headers}};
		{inet, tcp_closed} ->
			log(log_error, "Error: Incompleted header."),
			{error, "Error: Incompleted header"};
		Unknown ->
			log(log_error, "Unknown: ~p\n", [Unknown]),
			{error, "Error: http header error"}
	end.

admin_process({'GET', URI, _Headers}) ->
	[PATH, PARAM] = string:tokens(URI, "?"),
	log(log_info, "Path=~p, Param=~p\n", [PATH, PARAM]),
	case PATH of
		"/status" ->
			admin_cmd_status(lists:map(fun(E)->[K, V]=string:tokens(E, "="),{K, V} end, string:tokens(PARAM, "&")));
		_ ->
			log(log_error, "Unsupported method")
	end.

admin_cmd_status(_Param) ->
	qdict ! {enum_all, self()},
	F = fun({Addr, Pid}) ->
		Pid ! {report, self()},
		receive
			{Addr, {Queue, EstDelay, MaxDelay}} ->
				io_lib:format("~p\n", [{Addr, {Queue, EstDelay, MaxDelay}}]);
				%mochijson2:encode([{"addr", Addr}, {"status", [{"queue_len", length(Queue)}, {"measured_delay", EstDelay}, {"max_delay", MaxDelay}]}]);
			_ ->
				io_lib:format("{~p, \"addr_server didnt response\"}", [Addr])
				%mochijson2:encode([{"addr", Addr}, {"status", "unknown"}])
		end
	end,
	receive
		{enum_all, List} ->
			lists:map(F, List);
		_Msg ->
			mochijson2:encode({error, "Cant enum servers"})
	end.

