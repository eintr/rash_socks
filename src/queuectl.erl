-module(queuectl).

-export([create/1, report_status/1, connect_request/1, connect_ok/1, connect_fail/1, connect_timeout/1]).

-export([addr_server_start/2, start/1]).

create(Config) ->
	register(qdict, spawn_link(?MODULE, start, [Config])).

start(Config) ->
	process_flag(priority, high),
	loop(dict:new(), Config).

loop(Dict, Config) ->
	%io:format("qdict is now: ~p\n", [Dict]),
	receive
		{addr2pid, From, Addr} ->
			case dict:find(Addr, Dict) of
				{ok, Pid} ->
					From ! {ok, Pid},
					%io:format("~p is at ~p\n", [Addr, Pid]),
					loop(Dict, Config);
				error ->
					%io:format("~p is not registered!\n", [Addr]),
					From ! {error, "Not found"},
					loop(Dict, Config)
			end;
		{register, _From, {Addr, Pid}} ->
			%io:format("Got register message, store {~p, ~p}\n", [Addr, Pid]),
			loop(dict:store(Addr, Pid, Dict), Config);
		{unregister, _From, Addr} ->
			loop(dict:erase(Addr, Dict), Config)
	end.

connect_ok(Key) ->
	%io:format("Reporting: Server ~p is connected.\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_ok, self()};
		{error, _Reason} ->
			ignore
	end,
	true.

connect_fail(Key) ->
	%io:format("connect_fail(~p)\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_fail, self()};
		{error, _Reason} ->
			ignore
	end,
	true.

connect_timeout(Key) ->
	%io:format("connect_timeout(~p)\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_timeout, self()};
		{error, _Reason} ->
			ignore
	end,
	true.

connect_request(Key) ->
	%io:format("connect_request(~p)\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_request, self()},
			receive
				Reply -> Reply
			end;
		{error, _Reason} ->
			qdict ! {register, self(), {Key, spawn(?MODULE, addr_server_start, [Key, self()])}},
			true
	end.

report_status(Server) ->
	qdict ! {addr2pid, self(), Server},
	receive
		{ok, Pid} ->
			Pid ! {report, self()},
			receive
				Reply -> Reply
			end;
		{error, _Reason} ->
			qdict ! {register, self(), {Server, spawn(?MODULE, addr_server_start, [Server, self()])}},
			true
	end.

addr_server_start(Server, Pid) ->
	addr_server_loop(Server, {[{Pid, timestamp()}], 1, 3000}).

addr_server_loop(Server, {Queue, EstDelay, MaxDelay}=OldContext) ->
	%io:format("Server ~p: EstDelay=~p, queuelen=~p\n", [Server, EstDelay, length(Queue)]),
	receive
		{report, From} ->
			From ! {Server, {Queue, EstDelay, MaxDelay}},
			addr_server_loop(Server, {Queue, EstDelay, MaxDelay});
		{adjust, _From} ->
			NewQueue = adjust_queue(Queue, timestamp(), MaxDelay),
			addr_server_loop(Server, {NewQueue, EstDelay, MaxDelay});
		{connect_request, From} ->
			if
				length(Queue) == 0 ->
					From ! true,	% Try
					addr_server_loop(Server, {Queue++[{From, timestamp()}], EstDelay, MaxDelay});
				EstDelay < 0 ->		% Server is unavailable.
					From ! false,
					addr_server_loop(Server, OldContext);
				EstDelay*length(Queue) > MaxDelay ->	% Queue is too long.
					From ! false,
					addr_server_loop(Server, OldContext);
				true ->
					From ! true,	% Server is OK
					addr_server_loop(Server, {Queue++[{From, timestamp()}], EstDelay, MaxDelay})
			end;
		{connect_ok, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, Timestamp} ->
					Delay = timestamp() - Timestamp,
					if
						EstDelay == 0 ->
							NextDelay = (EstDelay+Delay)/2;
						true ->
							NextDelay = Delay
					end,
					addr_server_loop(Server, {lists:keydelete(From, 1, Queue), NextDelay, MaxDelay});
				false -> 
					io:format("Process ~p is not found in queue of ~p, ignored\n", [From, Server]),
					ignore
			end;
		{connect_fail, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, _Timestamp} ->
					addr_server_loop(Server, {lists:keydelete(From, 1, Queue), (EstDelay+1)*2, MaxDelay});
				_ -> ignore
			end;
		{connect_timeout, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, _Timestamp} ->
					addr_server_loop(Server, {lists:keydelete(From, 1, Queue), EstDelay*1.6, MaxDelay});
				_ -> ignore
			end;
		{receive_timeout, _From} ->
			io:format("addr_server: receive_timeout not implemented, yet.\n");
		{send_timeout, _From} ->
			io:format("addr_server: send_timeout not implemented, yet.\n");
		{closed, _From} ->
			io:format("addr_server: closed not implemented, yet.\n");
		Msg ->
			io:format("addr_server: Unknown message: ~p\n", [Msg])
	end,
	addr_server_loop(Server, OldContext).

adjust_queue([{SocksPid, Timestamp}|T], Now, MaxDelay) ->
	D = Now - Timestamp,
	if
		D > MaxDelay ->	% Expired.
			exit(SocksPid ,"Connecting was timed out"),
			adjust_queue([T], Now, MaxDelay);
		true ->
			T
	end.

timestamp() ->	% in milliseconds
	{MegaSecs, Secs, MicroSecs} = now(),
	((MegaSecs*1000000 + Secs)*1000000 + MicroSecs)/1000.

