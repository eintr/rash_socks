-module(queuectl).

-export([create/0, connect_request/1, connect_ok/1, connect_fail/1, connect_timeout/1]).

-export([addr_server_start/2, start/1]).

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
					%io:format("~p is at ~p\n", [Addr, Pid]),
					loop(Dict);
				error ->
					%io:format("~p is not registered!\n", [Addr]),
					From ! {error, "Not found"},
					loop(Dict)
			end;
		{register, _From, {Addr, Pid}} ->
			%io:format("Got register message, store {~p, ~p}\n", [Addr, Pid]),
			loop(dict:store(Addr, Pid, Dict));
		{unregister, _From, Addr} ->
			loop(dict:erase(Addr, Dict))
	end.

connect_ok(Key) ->
	%io:format("Reporting: Server ~p is connected.\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_ok, self()};
		{error, Reason} ->
			{error, Reason}
	end,
	true.

connect_fail(Key) ->
	io:format("connect_fail(~p)\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_fail, self()};
		{error, Reason} ->
			{error, Reason}
	end,
	true.

connect_timeout(Key) ->
	io:format("connect_timeout(~p)\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_timeout, self()};
		{error, Reason} ->
			{error, Reason}
	end,
	true.

connect_request(Key) ->
	io:format("connect_request(~p)\n", [Key]),
	qdict ! {addr2pid, self(), Key},
	receive
		{ok, Pid} ->
			Pid ! {connect_request, self()},
			receive
				Reply -> Reply
			end;
		{error, _Reason} ->
			io:format("Send {register...}\n"),
			qdict ! {register, self(), {Key, spawn(?MODULE, addr_server_start, [Key, self()])}},
			true
	end.

addr_server_start(Server, Pid) ->
	addr_server_loop(Server, {[{Pid, timestamp()}], 1, 3000}).

%addr_server_loop(Server, {[], EstDelay, _MaxDelay}) ->
%	if 
%		EstDelay>0 ->
%			io:format("Server for ~p exit for queue is empty, EstDelay=~pms.\n", [Server, EstDelay]);
%		true ->
%			true
%	end,
%	qdict ! {unregister, self(), Server};
addr_server_loop(Server, {Queue, EstDelay, MaxDelay}=OldCFG) ->
	io:format("Server ~p: EstDelay=~p, queue=~p\n", [Server, EstDelay, Queue]),
	receive
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
			case lists:keyfind(From, 1, Queue) of
				{From, Timestamp} ->
					Delay = timestamp() - Timestamp,
					io:format("Server ~p is OK: Delay = ~p-~p=~p\n", [Server, timestamp(), Timestamp, Delay]),
					addr_server_loop(Server, {lists:keydelete(From, 1, Queue), (EstDelay+Delay)/2, MaxDelay});
				Msg -> 
					io:format("Process ~p is not found in queue of ~p! Reply = ~p\n", [From, Server, Msg]),
					ignore
			end;
		{connect_fail, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, _Timestamp} ->
					addr_server_loop(Server, {lists:keydelete(From, 1, Queue), EstDelay*2, MaxDelay});
				_ -> ignore
			end;
		{connect_timeout, From} ->
			case lists:keyfind(From, 1, Queue) of
				{From, _Timestamp} ->
					addr_server_loop(Server, {lists:keydelete(From, 1, Queue), EstDelay*1.6, MaxDelay});
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

