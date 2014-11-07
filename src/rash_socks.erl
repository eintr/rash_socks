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
			case get_request(Client) of
				{ok, {M,U,_}=Req} ->
					log(log_info, "Admin request from ~p: ~p ~p", [inet:peernames(Client), M, U]),
					Reply = admin_process(Req),
					%io:format("Reply: ~p\n", [Reply]),
					gen_tcp:send(Client, Reply),
					gen_tcp:close(Client),
					admin_loop(Socket, Config);
				{error, Reason} ->
					log(log_info, "Error: ~s", [Reason]),
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
			log(log_error, "Unknown: ~p", [Unknown]),
			{error, "Error: http header error"}
	end.

admin_process({'GET', URI, _Headers}) ->
	%io:format("URI=~p\n", [URI]),
	[PATH, PARAM] = string:tokens(URI, "?"),
	%log(log_info, "Path=~p, Param=~p", [PATH, PARAM]),
	case PATH of
		"/status" ->
			admin_svc_status(param_to_list(PARAM));
		SVCPATH ->
			log(log_error, "Unsupported service path: ~p", [SVCPATH])
	end;
admin_process({Method, _URI, _Headers}) ->
	log(log_error, "Unsupported method: ~p", [Method]).

admin_svc_status(_Param) ->
	Collector = fun({Addr, Pid}) ->
		Pid ! {report, self()},
		receive
			M -> M
		after 1000 ->
			io_lib:format("Server ~p => addr_server didnt response.\n", [Addr])
		end
	end,
	Translater = fun({{{A, B, C, D}, Port}, {Queue, EstDelay, {MaxCDelay, MaxRDelay, MaxSDelay}}}) ->
		io_lib:format("Server ~b.~b.~b.~b:~b => Configured Delay: {~p, ~p, ~p}, Queuelen: ~p, EstDelay: ~pms\n", [A, B, C, D, Port, MaxCDelay, MaxRDelay, MaxSDelay, length(Queue), EstDelay])
	end,
	qdict ! {enum_all, self()},
	receive
		{enum_all, List} ->
			lists:map(Translater,
				serverlist_sort(fun
					({_, {_, EstDelay, _}}) -> -EstDelay end, lists:map(Collector, List)));
		_Msg ->
			"Cant enum servers"
	end.

serverlist_sort(nosort, List) ->
	List;
serverlist_sort(KeySelector, List) ->
	KeyExtract = fun
		(E) ->	{KeySelector(E), E}
	end,
	KeyStrip = fun
		({_, E}) ->
			E
	end,
	lists:map(KeyStrip, lists:keysort(1, lists:map(KeyExtract, List))).

param_to_list(ParamStr) ->
	F1 = fun
		(S, Acc) ->
			case string:tokens(S, "=") of
				[K, V] ->
					Acc ++ [{K, V}];
				_ ->
					Acc
			end
	end,
	lists:foldl(F1, [], string:tokens(ParamStr, "&")).

