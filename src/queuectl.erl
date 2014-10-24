-module(queuectl).

-export([create/0, connect_request/1, connect_ok/1, connect_fail/1, connect_timeout/1]).

-export([addr_server_start/1, start/1]).

create() ->
	create([]).

create(Config) ->
	register(qdict, spawn_link(?MODULE, start, [Config])).

start(_Config) ->
	process_flag(priority, high),
	loop(dict:new()).

loop(Dict) ->
	%io:format("qdict is now: ~p\n", [Dict]),
	receive
		{addr2pid, From, Addr} ->
			case dict:find(Addr, Dict) of
				{ok, Pid} ->
					From ! {ok, Pid},
					io:format("~p is at ~p\n", [Addr, Pid]),
					loop(Dict);
				error ->
					From ! {error, "Not found"},
					loop(Dict)
			end;
		{register, _From, {Addr, Pid}} ->
			loop(dict:store(Addr, Pid, Dict));
		{unregister, _From, Addr} ->
			loop(dict:erase(Addr, Dict))
	end.

connect_ok(Key) ->
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_ok, self()};
		{error, Reason} ->
			{error, Reason}
	end,
	true.

connect_fail(Key) ->
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_fail, self()};
		{error, Reason} ->
			{error, Reason}
	end,
	true.

connect_timeout(Key) ->
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_timeout, self()};
		{error, Reason} ->
			{error, Reason}
	end,
	true.

connect_request(Key) ->
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			io:format("Sending connect_request to process ~p\n", [Pid]),
			Pid ! {connect_request, self()},
			receive
				Reply -> Reply
			end;
		{error, _Reason} ->
			qdict ! {register, self(), {Key, spawn(?MODULE, addr_server_start, [Key])}},
			true
	end.

addr_server_start(Server) ->
	io:format("Server ~p is up.\n", [Server]),
	addr_server_loop(Server, {[], 0, 3000}).

addr_server_loop(Server, {Queue, EstDelay, MaxDelay}=OldCFG) ->
	io:format("Server ~p is now: ~p\n", [Server, OldCFG]),
	receive
		{timer, _From, Qelm} ->
			NewQueue = adjust_queue(Queue -- [Qelm], EstDelay, MaxDelay),
			NewEstDelay = EstDelay*1.6,
			addr_server_loop(Server, {NewQueue, NewEstDelay, MaxDelay});
		{set, _From, {maxdelay, Value}} ->
			NewQueue = adjust_queue(Queue, EstDelay, Value),
			addr_server_loop(Server, {NewQueue, EstDelay, Value});
		{connect_request, From} ->
			if
				length(Queue) == 0 ->
					From ! true,	% Try
					addr_server_loop(Server, {Queue++[{From, timestamp()}], EstDelay, MaxDelay});
				EstDelay < 0 ->		% Server is unavailable.
					From ! false,
					addr_server_loop(Server, OldCFG);
				EstDelay*length(Queue) > MaxDelay ->	% Queue is too long.
					From ! false,
					addr_server_loop(Server, OldCFG);
				true ->
					From ! true,	% Server is OK
					addr_server_loop(Server, {Queue++[{From, timestamp()}], EstDelay, MaxDelay})
			end;
		{connect_ok, From} ->
			case lists:find(From, 1, Queue) of
				{ok, {Pid, Timestamp}} ->
					Delay = timestamp() - Timestamp,
					addr_server_loop(Server -- [{Pid, Timestamp}], {Queue, (EstDelay+Delay)/2, MaxDelay});
				_ -> ignore
			end;
		{connect_fail, From} ->
			case lists:find(From, 1, Queue) of
				{ok, {Pid, Timestamp}} ->
					addr_server_loop(Server -- [{Pid, Timestamp}], {Queue, EstDelay*2, MaxDelay});
				_ -> ignore
			end;
		{connect_timeout, From} ->
			case lists:find(From, 1, Queue) of
				{ok, {Pid, Timestamp}} ->
					addr_server_loop(Server -- [{Pid, Timestamp}], {Queue, EstDelay*1.6, MaxDelay});
				_ -> ignore
			end;
		Msg ->
			io:format("addr_server: Unknown message: ~p\n", [Msg])
	end,
	addr_server_loop(Server, OldCFG).

adjust_queue(Queue, _EstDelay, MaxDelay) ->
	F = fun ({SocksPid, Timestamp}) ->
		D = timestamp() - Timestamp,
		if
			D > MaxDelay ->	% Expired.
				exit(SocksPid ,"Connecting was timed out"),
				false;
			true ->
				true
		end
	end,
	lists:filter(F, Queue).

timestamp() ->	% in milliseconds
	{MegaSecs, Secs, MicroSecs} = now(),
	((MegaSecs*1000000 + Secs)*1000000 + MicroSecs)/1000.

