-module(queuectl).

-import(log, [log/2, log/3]).

-export([create/1, report_status/1, connect_request/1, connect_ok/1, connect_fail/1, connect_timeout/1]).

-export([addr_server_start/2, start/1]).

create(Config) ->
	register(qdict, spawn_link(?MODULE, start, [Config])).

start(Config) ->
	process_flag(priority, high),
	loop(dict:new(), Config).

loop(Dict, Config) ->
	receive
		{addr2pid, From, Addr} ->
			case dict:find(Addr, Dict) of
				{ok, Pid} ->
					From ! {ok, Pid},
					loop(Dict, Config);
				error ->
					log(log_warning, "addr_server for ~p is not registered!\n", [Addr]),
					From ! {error, "Not found"},
					loop(Dict, Config)
			end;
		{enum_all, From} ->
			From ! {enum_all, dict:fold(fun (Addr, Pid, Acc) -> Acc++[{Addr, Pid}] end, [], Dict)},
			loop(Dict, Config);
		{create, From, Addr} ->
			case dict:find(Addr, Dict) of
				{ok, Pid} ->
					log(log_warning, "addr_server for ~p is already running: ~p.\n", [Addr, Pid]),
					loop(Dict, Config);
				error ->	% This is the normal case.
					Pid = spawn(?MODULE, addr_server_start, [Addr, Config]),
					From ! ok,
					loop(dict:store(Addr, Pid, Dict), Config)
			end;
		{register, _From, {Addr, Pid}} ->
			log(log_debug, "Got register message, store {~p, ~p}\n", [Addr, Pid]),
			loop(dict:store(Addr, Pid, Dict), Config);
		{unregister, _From, Addr} ->
			loop(dict:erase(Addr, Dict), Config)
	end.

connect_ok(Key) ->
	log(log_debug, "Server ~p is connected.", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_ok, self()};
		{error, _Reason} ->
			ignore
	end,
	true.

connect_fail(Key) ->
	log(log_debug, "Server ~p connect_fail.", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_fail, self()};
		{error, _Reason} ->
			ignore
	end,
	true.

connect_timeout(Key) ->
	log(log_debug, "Server ~p connect_timeout.", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_timeout, self()};
		{error, _Reason} ->
			ignore
	end,
	true.

connect_request(Key) ->
	log(log_debug, "Server ~p is requesting connection.", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_request, self()},
			receive
				Reply -> Reply
			end;
		{error, _Reason} ->
			qdict ! {create, self(), Key},
			receive
				_ -> true
			end,
			connect_request(Key)
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

addr_server_start(Server, Config) ->
	process_flag(priority, high),
	addr_server_loop(Server, Config, {[], 1, config:addr_config(Server, Config)}).

addr_server_loop(Server, Config, {Queue, EstDelay, {MaxCDelay, _MaxRDealy, _MaxSDelay}=MaxDelay}=OldContext) ->
	receive
		{report, From} ->
			From ! {Server, {Queue, EstDelay, MaxDelay}},
			addr_server_loop(Server, Config, {Queue, EstDelay, MaxDelay});
		{adjust, _From} ->
			NewQueue = adjust_queue(Queue, timestamp(), MaxDelay),
			addr_server_loop(Server, Config, {NewQueue, EstDelay, MaxDelay});
		{connect_request, From} ->
			if
				length(Queue) == 0 ->
					From ! true,	% Try
					addr_server_loop(Server, Config, {Queue++[{From, timestamp()}], EstDelay, MaxDelay});
				EstDelay < 0 ->		% Server is unavailable.
					From ! false,
					addr_server_loop(Server, Config, OldContext);
				EstDelay*length(Queue) > MaxCDelay ->	% Queue is too long.
					From ! false,
					addr_server_loop(Server, Config, OldContext);
				true ->
					From ! true,	% Server is OK
					arange_adjust(MaxCDelay),
					addr_server_loop(Server, Config, {Queue++[{From, timestamp()}], EstDelay, MaxDelay})
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
					addr_server_loop(Server, Config, {lists:keydelete(From, 1, Queue), NextDelay, MaxDelay});
				false ->
					log(log_warning, "Process ~p is not found in queue of ~p, ignored\n", [From, Server]),
					ignore
			end;
		{connect_fail, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, _Timestamp} ->
					addr_server_loop(Server, Config, {lists:keydelete(From, 1, Queue), (EstDelay+1)*2, MaxDelay});
				_ -> ignore
			end;
		{connect_timeout, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, _Timestamp} ->
					addr_server_loop(Server, Config, {lists:keydelete(From, 1, Queue), EstDelay*1.6, MaxDelay});
				_ -> ignore
			end;
		{receive_timeout, _From} ->
			log(log_warning, "addr_server: receive_timeout not implemented, yet.");
		{send_timeout, _From} ->
			log(log_warning, "addr_server: send_timeout not implemented, yet.");
		{closed, _From} ->
			log(log_warning, "addr_server: closed not implemented, yet.");
		Msg ->
			log(log_warning, "addr_server: Unknown message: ~p\n", [Msg])
	end,
	addr_server_loop(Server, Config, OldContext).

arange_adjust(0) ->
	true;
arange_adjust(MaxCDelay) ->
	timer:send_after(MaxCDelay, {adjust, ignored}).

adjust_queue([], _, _) ->
	[];
adjust_queue([{SocksPid, Timestamp}|T], Now, {MaxCDelay, _MaxRDealy, _MaxSDelay}=MaxDelay) ->
	D = Now - Timestamp,
	if
		D > MaxCDelay ->	% Expired.
			exit(SocksPid ,"Connecting was timed out"),
			adjust_queue(T, Now, MaxDelay);
		true ->
			T
	end.

timestamp() ->	% in milliseconds
	{MegaSecs, Secs, MicroSecs} = now(),
	((MegaSecs*1000000 + Secs)*1000000 + MicroSecs)/1000.

